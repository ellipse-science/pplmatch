#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Construit party_changes_qc.csv depuis la Chronologie parlementaire de l'ANQ.

POURQUOI CE SCRIPT EXISTE
-------------------------
members_historic_qc.csv porte UNE ligne par (personne x election) : le parti y
est fige au moment de l'election. Un changement d'allegeance en cours de mandat
n'y est donc pas exprimable. Resultat mesure le 2026-08-11 : sur 1190 lignes,
`ind` apparait 2 fois, toutes deux dans la 43e legislature. Zero defection
enregistree pour 1994-2022 — ce qui n'est evidemment pas la realite.

La table party_changes_qc.csv comble ce trou, et matcher.py s'en sert deja
(_resolve_party) pour rendre le parti en vigueur A LA DATE de l'intervention.
Seules les 9 defections de la 43e y etaient, saisies a la main. Ce script les
reconstruit — et etend la couverture a toutes les legislatures.

LA SOURCE
---------
La Chronologie parlementaire (Bibliotheque de l'ANQ) consigne l'evenement
lui-meme, date au jour pres :

    12 septembre 2024
    Le depute de Saint-Jerome, Youri Chassin, quitte le caucus de la
    Coalition avenir Quebec. Il siege comme depute independant.

A NOTER : les notices biographiques des deputes ne portent PAS cette date —
elles ne donnent que l'allegeance courante. La chronologie est donc la seule
source publique qui date l'evenement. (Verifie le 2026-08-11 sur la notice de
Youri Chassin : « Independant », sans date de changement.)

CE QUE LE SCRIPT NE FAIT PAS SILENCIEUSEMENT
--------------------------------------------
Un scraper qui rate un evenement sans le dire est pire qu'un scraper absent :
il donne une fausse assurance de completude. Tout paragraphe qui PARLE de
caucus, d'allegeance ou d'independance sans que le script sache le classer est
donc ecrit dans un rapport `unparsed`. Le nombre d'entrees non classees fait
partie de la sortie : c'est la dette visible, pas un trou masque.

USAGE
-----
    python3 inst/python/build_party_changes.py            # ecrit le CSV
    python3 inst/python/build_party_changes.py --dry-run  # n'ecrit rien
"""

import argparse
import csv
import html as html_module
import json
import os
import re
import sys
import time
import unicodedata
import urllib.request
from datetime import date

HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; pplmatch-research-tool)"}
BASE_URL = "https://www.assnat.qc.ca/fr/patrimoine/chronologie/chrono{n}.html"
REQUEST_DELAY = 1.0  # politesse envers le serveur de l'ANQ

# Pages de la chronologie couvrant 1994 -> aujourd'hui. Les pages recentes
# valent une annee chacune ; les anciennes en couvrent plusieurs, d'ou le
# recouvrement volontaire (on deduplique sur la date de toute facon).
# chrono116 (2026) existe bien qu'il ne soit PAS liste dans l'index du site.
FIRST_PAGE, LAST_PAGE = 86, 116

MOIS = {
    "janvier": 1, "fevrier": 2, "mars": 3, "avril": 4, "mai": 5, "juin": 6,
    "juillet": 7, "aout": 8, "septembre": 9, "octobre": 10, "novembre": 11,
    "decembre": 12,
}

# Libelle officiel -> code utilise dans members_historic_qc.csv.
# L'ordre compte : les libelles longs sont testes avant les courts, sinon
# « Parti quebecois » capturerait « Parti quebecois » dans « Parti liberal ».
PARTIS = [
    ("coalition avenir quebec", "CAQ"),
    ("parti liberal du quebec", "PLQ"),
    ("parti conservateur du quebec", "PCQ"),
    ("action democratique du quebec", "ADQ"),
    ("parti quebecois", "PQ"),
    ("quebec solidaire", "QS"),
    ("quebec debout", "QD"),
    ("option nationale", "ON"),
    ("parti egalite", "EQ"),
    ("equality party", "EQ"),
    # Formes ABREGEES, testees en dernier : « reintegre le caucus du Parti
    # liberal » (sans « du Quebec ») est courant. L'ordre compte — place plus
    # haut, « parti liberal » capturerait « Parti liberal du Quebec » avant que
    # la forme longue ne soit testee, ce qui reste correct ici mais fragile.
    ("parti liberal", "PLQ"),
    ("coalition avenir", "CAQ"),
    ("action democratique", "ADQ"),
    ("parti conservateur", "PCQ"),
]

# Un paragraphe qui contient un de ces mots PARLE d'allegeance. S'il n'est pas
# classe ensuite, il ressort dans le rapport `unparsed`.
INDICES = ("caucus", "independant", "independante", "allegeance",
           "joint les rangs", "se joint a", "adhere")


def _strip_accents(s):
    return "".join(c for c in unicodedata.normalize("NFD", s)
                   if unicodedata.category(c) != "Mn")


def _norm(s):
    """Minuscules sans accents — pour comparer du texte, pas pour un identifiant."""
    return _strip_accents(html_module.unescape(s or "")).lower()


def district_id(nom):
    """« Saint-Jerome » -> « saintjerome », comme members_historic_qc.csv."""
    return re.sub(r"[^a-z0-9]", "", _norm(nom))


def fetch_page(n, cache_dir):
    """Recupere une page, avec cache disque : re-executer le script ne
    retape pas sur le serveur de l'ANQ."""
    if cache_dir:
        os.makedirs(cache_dir, exist_ok=True)
        path = os.path.join(cache_dir, f"chrono{n}.html")
        if os.path.exists(path):
            with open(path, encoding="utf-8") as f:
                return f.read()
    req = urllib.request.Request(BASE_URL.format(n=n), headers=HEADERS)
    try:
        html = urllib.request.urlopen(req, timeout=30).read().decode("utf-8", "replace")
    except Exception as e:                                   # noqa: BLE001
        print(f"  chrono{n} : inaccessible ({e})", file=sys.stderr)
        return None
    time.sleep(REQUEST_DELAY)
    if cache_dir:
        with open(os.path.join(cache_dir, f"chrono{n}.html"), "w", encoding="utf-8") as f:
            f.write(html)
    return html


def parse_entries(html):
    """Rend [(date, texte)] — la chronologie alterne <p><strong>DATE</strong></p>
    et <p>evenement</p>."""
    out, courante = [], None
    # DEUX balisages coexistent selon l'epoque, et n'en connaitre qu'un rend
    # aveugle sans rien dire : les pages recentes datent avec
    # <p><strong>12 septembre 2024</strong></p>, celles d'avant 2008 avec
    # <h4>23 fevrier 2003</h4>. Ne lire que la premiere forme donnait ZERO
    # entree sur chrono86 a chrono97 — 14 ans de defections invisibles.
    for m in re.finditer(r"<(p|h[1-6])[^>]*>(.*?)</\1>", html, re.S | re.I):
        balise, brut = m.group(1).lower(), m.group(2)
        texte = html_module.unescape(re.sub(r"<[^>]+>", " ", brut))
        texte = re.sub(r"\s+", " ", texte).strip()
        if not texte:
            continue
        d = _parse_date(texte)
        # Une date SEULE est un en-tete ; une date au fil du texte n'en est pas
        # un. L'en-tete est soit un <hN>, soit un <p> en gras.
        if d and len(texte) < 60 and (balise.startswith("h") or "<strong>" in brut.lower()):
            courante = d
            continue
        if courante:
            out.append((courante, texte))
    return out


def _parse_date(texte):
    """« 12 septembre 2024 » -> date(2024, 9, 12). Rend None sinon.
    Gere « 8 au 10 septembre 2024 » en retenant le premier jour."""
    t = _norm(texte)
    m = re.match(r"^(\d{1,2})(?:\s*(?:au|et)\s*\d{1,2})?\s+([a-z]+)\s+(\d{4})\s*$", t)
    if not m:
        return None
    jour, mois, annee = int(m.group(1)), MOIS.get(m.group(2)), int(m.group(3))
    if not mois:
        return None
    try:
        return date(annee, mois, jour)
    except ValueError:
        return None


def _party_code(texte_norm):
    for libelle, code in PARTIS:
        if libelle in texte_norm:
            return code
    return None


# Verbes de DEPART, releves sur les vrais libelles de la chronologie
# (2026-08-11). La liste vient du rapport `unparsed` d'une premiere passe :
# ecrire les motifs d'apres le texte reel plutot que d'apres ce qu'on imagine.
DEPARTS = (
    "quitte", "quittent", "demissionne du caucus", "demissionnent du caucus",
    "se retire du caucus", "retrait du caucus", "ne fait plus partie du caucus",
    "n'est plus membre du caucus", "nest plus membre du caucus",
    "expulse", "expulsee", "exclu", "exclue",
)
# « reintegre le caucus de X » est LA formulation du retour — elle manquait, et
# c'est elle qui laissait Vaudreuil (Nichols, retour au PLQ le 2025-06-19)
# indefiniment independante dans nos tables.
ARRIVEES = ("rejoint", "reintegre", "reintegrera", "joint les rangs",
            "se joint au caucus", "joint le caucus", "adhere a", "passe a")

# Un retour peut etre annonce au FUTUR, avec sa vraie date dans la phrase :
# « ne fait plus partie du caucus [...] Il le reintegrera le 1er decembre 2011 ».
# Dater ce retour du paragraphe le placerait neuf jours trop tot.
RE_DATE_INLINE = re.compile(
    r"le\s+(\d{1,2})\s*(?:er|re|e)?\s+([a-zA-Zéû]+)\s+(\d{4})")


def date_explicite(texte, mois_map):
    """Rend la date citee DANS la phrase, ou None."""
    m = RE_DATE_INLINE.search(_norm(texte))
    if not m:
        return None
    mois = mois_map.get(m.group(2))
    if not mois:
        return None
    try:
        from datetime import date as _d
        return _d(int(m.group(3)), mois, int(m.group(1)))
    except ValueError:
        return None

# Les circonscriptions se nomment de plusieurs facons ; l'ordre va du plus
# specifique au plus general pour ne pas couper un nom compose trop tot.
# NB : l'elision (« depute d'Abitibi-Est ») et le libelle fautif du site
# (« la deputee DES Rimouski ») sont frequents — les motifs les absorbent.
RE_DISTRICTS = [
    re.compile(r"depute[e]?\s+de\s+la\s+circonscription\s+(?:d[eu]\s+|d[’'])([^,.;]+)", re.I),
    re.compile(r"depute[e]?\s+(?:liberal[e]?\s+|independant[e]?\s+)?(?:d[eu]s?\s+|d[’'])([^,.;]+)", re.I),
]


def _districts(texte_norm):
    """Toutes les circonscriptions nommees, dans l'ordre d'apparition.
    Un paragraphe peut annoncer PLUSIEURS departs d'un coup — c'est frequent
    (trois demissions du caucus du PQ le 6 juin 2011, par exemple)."""
    trouves, vus = [], set()
    for rx in RE_DISTRICTS:
        for m in rx.finditer(texte_norm):
            nom = m.group(1).strip()
            nom = re.sub(r"\s+(et|ainsi que)\s*$", "", nom).strip()
            if not nom or len(nom) > 60:
                continue
            did = district_id(nom)
            if did and did not in vus:
                vus.add(did)
                trouves.append((m.start(), nom))
    return [nom for _, nom in sorted(trouves)]


def classify(texte):
    """Rend une LISTE de (district, parti_avant, parti_apres).

    Un seul paragraphe peut porter plusieurs departs, et la destination n'est
    pas toujours « independant » : « quitte le caucus du Parti quebecois.
    Il rejoint la Coalition avenir Quebec » est un transfert de parti a parti.
    """
    t = _norm(texte)

    part = any(v in t for v in DEPARTS)
    arrive = any(v in t for v in ARRIVEES)
    devient_ind = ("independant" in t or "independante" in t
                   or "independants" in t or "independantes" in t)
    if not (part or arrive):
        return []

    districts = _districts(t)
    if not districts:
        return []

    # Le parti d'ARRIVEE, s'il est nomme apres un verbe d'arrivee.
    apres = None
    for v in ARRIVEES:
        i = t.find(v)
        if i != -1:
            apres = _party_code(t[i:])
            if apres:
                break
    # Sinon, un depart vers les banquettes independantes.
    if not apres and devient_ind and part:
        apres = "IND"
    if not apres:
        return []

    # Le parti de DEPART est celui nomme avant/autour du verbe de depart.
    avant = None
    for v in DEPARTS:
        i = t.find(v)
        if i != -1:
            cand = _party_code(t[i:])
            if cand and cand != apres:
                avant = cand
                break
    if not avant:
        cand = _party_code(t)
        avant = cand if cand and cand != apres else None

    return [(d, avant, apres) for d in districts]


def legislature_for(d, legislatures):
    for leg in legislatures:
        deb = date.fromisoformat(leg["start_date"])
        fin = date.fromisoformat(leg["end_date"])
        if deb <= d <= fin:
            return leg["legislature"]
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true", help="n'ecrit aucun fichier")
    ap.add_argument("--cache", default=None, help="repertoire de cache HTML")
    args = ap.parse_args()

    ici = os.path.dirname(os.path.abspath(__file__))
    extdata = os.path.normpath(os.path.join(ici, "..", "extdata"))
    with open(os.path.join(extdata, "legislatures_qc.json"), encoding="utf-8") as f:
        legislatures = json.load(f)

    changements, non_classes, vus = [], [], set()
    print(f"Chronologie ANQ : pages chrono{FIRST_PAGE} a chrono{LAST_PAGE}")
    for n in range(FIRST_PAGE, LAST_PAGE + 1):
        html = fetch_page(n, args.cache)
        if not html:
            continue
        for d, texte in parse_entries(html):
            t = _norm(texte)
            if not any(i in t for i in INDICES):
                continue
            res = classify(texte)
            leg = legislature_for(d, legislatures)
            if not res or leg is None:
                non_classes.append((d, texte[:200], f"chrono{n}"))
                continue
            for district, avant, apres in res:
                cle = (district_id(district), leg, d.isoformat(), apres)
                if cle in vus:        # les pages anciennes se recouvrent
                    continue
                vus.add(cle)
                changements.append({
                    "district_id": district_id(district),
                    "legislature_id": leg,
                    "party_id_before": avant or "",
                    "party_id_after": apres,
                    "change_date": d.isoformat(),
                })

    changements.sort(key=lambda r: (r["change_date"], r["district_id"]))
    print(f"\n{len(changements)} changement(s) d'allegeance identifie(s)")
    par_leg = {}
    for c in changements:
        par_leg[c["legislature_id"]] = par_leg.get(c["legislature_id"], 0) + 1
    for leg in sorted(par_leg):
        print(f"  legislature {leg} : {par_leg[leg]}")

    print(f"\n{len(non_classes)} paragraphe(s) parlant d'allegeance SANS classement — "
          f"a relire, c'est la dette visible :")
    for d, texte, page in non_classes[:15]:
        print(f"  [{d}] ({page}) {texte}")
    if len(non_classes) > 15:
        print(f"  ... et {len(non_classes) - 15} autre(s)")

    if args.dry_run:
        print("\n--dry-run : rien n'a ete ecrit.")
        return

    sortie = os.path.join(extdata, "party_changes_qc.csv")
    with open(sortie, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["district_id", "legislature_id",
                                          "party_id_before", "party_id_after",
                                          "change_date"])
        w.writeheader()
        w.writerows(changements)
    print(f"\nEcrit : {sortie}")

    rapport = os.path.join(extdata, "party_changes_unparsed.txt")
    with open(rapport, "w", encoding="utf-8") as f:
        for d, texte, page in non_classes:
            f.write(f"[{d}] ({page}) {texte}\n")
    print(f"Rapport des non-classes : {rapport}")


if __name__ == "__main__":
    main()
