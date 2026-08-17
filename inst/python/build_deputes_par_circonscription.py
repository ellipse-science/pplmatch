#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Releve l'historique des elu.es PAR CIRCONSCRIPTION, depuis l'ANQ.

POURQUOI CE SCRIPT EXISTE
-------------------------
`members_historic_qc.csv` s'arrete en cours de 43e legislature. Eric Lefebvre y
figure pour la 41e et la 42e, PAS pour la 43e — alors qu'il a ete reelu dans
Arthabaska en 2022 et y a siege jusqu'a sa demission du 2025-03-18. Consequence
en chaine : aucun mandat n'existait pour ce siege sur cette periode, donc sa
defection du 2024-04-16 tombait sur un siege sans mandat ouvert et etait
ECARTEE, et sa parole sortait `unmatched` — donc jetee par le raffineur.

Le releve du jour (`build_deputes_courants.py`) ne peut pas combler ce trou :
c'est un INSTANTANE, il montre le successeur. `deputes_leg43_qc.csv` non plus —
Arthabaska est justement l'une de ses 9 circonscriptions vides.

CE QUE CETTE SOURCE APPORTE
---------------------------
« Membres de l'Assemblee nationale par circonscription » donne, pour chaque
siege, la suite complete des titulaires depuis 1867 : annee, nom, parti,
remarques, et un lien vers la fiche qui porte l'identifiant ANQ. Les elections
partielles y sont marquees « (election partielle) ». C'est la source
authoritative de l'ANQ sur SA propre composition, et elle couvre le present.

    2022                       LEFEBVRE, Eric        Coalition avenir Quebec
    2025 (election partielle)  BOISSONNEAULT, Alex   Parti quebecois

CE QU'ELLE NE DONNE PAS
-----------------------
Une ANNEE, pas une date. Un mandat ouvert ici est donc date par la generale ou
la partielle correspondante, jamais par cette page. Elle dit QUI, pas QUAND.

Elle n'est pas non plus a jour partout : au moment de l'ecriture, 111 des 125
circonscriptions portent leur ligne 2022, et 14 s'arretent a 2018 (Taillon,
Taschereau, Terrebonne, Verdun...). Ce n'est pas un defaut de lecture, c'est la
page elle-meme. Cette source COMPLETE donc le referentiel, elle ne le remplace
pas — et le script affiche sa couverture par annee d'election pour que la
peremption se voie au lieu de se deviner.

USAGE
    python3 inst/python/build_deputes_par_circonscription.py [--dry-run] [--cache DIR]
"""

import argparse
import csv
import html as html_module
import os
import re
import time
import unicodedata
import urllib.request

BASE = "https://www.assnat.qc.ca/fr/patrimoine/depcir/"
PAGES = ["index.html", "b.html", "c.html", "di.html", "jl.html",
         "mn.html", "op.html", "qr.html", "s.html", "tz.html"]
HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; pplmatch-research-tool)"}

# Garde-fou. Le Quebec a 125 circonscriptions actuelles, et le repertoire couvre
# aussi les abolies : le total est donc bien superieur. En dessous de ce seuil,
# c'est que le balisage a change et qu'on rend un releve partiel EN SILENCE —
# le defaut precis que `deputes_leg43_qc.csv` a introduit en son temps.
CIRCONSCRIPTIONS_MIN = 200
LIGNES_MIN = 4000

PARTIS = [
    ("coalition avenir", "CAQ"), ("parti liberal", "PLQ"), ("liberal", "PLQ"),
    ("parti quebecois", "PQ"), ("quebec solidaire", "QS"),
    ("parti conservateur du quebec", "PCQ"),
    ("action democratique", "ADQ"), ("conservateur", "PC"),
    ("union nationale", "UN"), ("credit social", "CS"),
    ("ralliement creditiste", "RC"), ("bloc populaire", "BP"),
    ("parti national populaire", "PNP"), ("equality", "EQ"),
    ("democratie chretienne", "DC"), ("option nationale", "ON"),
    ("independant", "IND"), ("sans designation", "IND"),
]


def _norm(s):
    return "".join(c for c in unicodedata.normalize("NFD", s or "")
                   if unicodedata.category(c) != "Mn").lower()


def district_id(nom):
    return re.sub(r"[^a-z0-9]", "", _norm(nom))


def party_code(libelle):
    t = _norm(libelle)
    for motif, code in PARTIS:
        if motif in t:
            return code
    return libelle.strip() or ""


def _texte(fragment):
    t = html_module.unescape(re.sub(r"<[^>]+>", " ", fragment))
    return re.sub(r"\s+", " ", t).strip()


def fetch(page, cache=None):
    if cache:
        chemin = os.path.join(cache, page)
        if os.path.exists(chemin):
            return open(chemin, encoding="utf-8").read()
    req = urllib.request.Request(BASE + page, headers=HEADERS)
    corps = urllib.request.urlopen(req, timeout=30).read().decode("utf-8", "replace")
    if cache:
        os.makedirs(cache, exist_ok=True)
        open(os.path.join(cache, page), "w", encoding="utf-8").write(corps)
    time.sleep(1)          # on ne martele pas un site public
    return corps


# Le nom de la circonscription est un titre EN MAJUSCULES qui precede sa table.
# On decoupe donc la page a chaque titre plutot que de se fier a une ancre, dont
# la forme varie d'une page a l'autre.
# Le titre d'une circonscription porte du BALISAGE A L'INTERIEUR : l'ANQ encode
# le tiret cadratin des noms fusionnes par « <sup>__</sup> », comme dans
# « SAINT-HENRI<sup>__</sup>SAINTE-ANNE ». Exiger un contenu sans balise faisait
# echouer le motif sur ces titres — donc la circonscription n'etait pas
# detectee du tout, et ses lignes etaient attribuees a la PRECEDENTE. Effet
# mesure : `sainthenri` heritait des mandats de Saint-Henri-Sainte-Anne, le
# meme siege physique existait sous deux identifiants, et le plafond de 125
# montait a 137.
#
# On capture donc le contenu brut du titre et on le nettoie ensuite, plutot que
# d'exiger qu'il soit deja propre.
RE_TITRE = re.compile(r"<(h[1-6])[^>]*>(.*?)</\1>", re.S | re.I)


def _titres(html):
    """Positions et noms des circonscriptions, dans l'ordre de la page."""
    out = []
    for m in RE_TITRE.finditer(html):
        t = _texte(m.group(2))
        # Un titre de circonscription est en MAJUSCULES et sans chiffre.
        if len(t) > 3 and not re.search(r"\d", t) and t == t.upper() \
                and "ASSEMBL" not in t:
            out.append((m.start(), t))
    return out


RE_ANNEE = re.compile(r"^(\d{4})\b")


def releve_page(html):
    """Rend [{seat, seat_id, annee, partielle, nom, parti, person_id, remarque}]."""
    # Positions des titres de circonscription, puis des lignes de tableau.
    titres = _titres(html)

    lignes = []
    for m in re.finditer(r"<tr[^>]*>(.*?)</tr>", html, re.S | re.I):
        cells = [_texte(c) for c in
                 re.findall(r"<t[dh][^>]*>(.*?)</t[dh]>", m.group(1), re.S | re.I)]
        if len(cells) < 3 or not RE_ANNEE.match(cells[0]):
            continue
        pid = re.search(r"/fr/deputes/[a-z0-9\-]+?-(\d+)/", m.group(1))
        # Le titre le plus proche AVANT cette ligne nomme sa circonscription.
        siege = ""
        for pos, t in titres:
            if pos < m.start():
                siege = t
            else:
                break
        if not siege:
            continue
        lignes.append({
            "seat": siege.title(),
            "seat_id": district_id(siege),
            "annee": int(RE_ANNEE.match(cells[0]).group(1)),
            "partielle": 1 if "partielle" in _norm(cells[0]) else 0,
            "full_name": cells[1],
            "party_id": party_code(cells[2]),
            "person_id": pid.group(1) if pid else "",
            "remarque": cells[3] if len(cells) > 3 else "",
        })
    return lignes


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--cache", default=None)
    args = ap.parse_args()

    toutes = []
    for page in PAGES:
        html = fetch(page, args.cache)
        lues = releve_page(html)
        print(f"  {page:12} {len(lues):5} lignes")
        toutes.extend(lues)

    sieges = {l["seat_id"] for l in toutes}
    avec_pid = sum(1 for l in toutes if l["person_id"])
    print(f"\nlignes           : {len(toutes)}")
    print(f"circonscriptions : {len(sieges)}")
    print(f"avec identifiant : {avec_pid} ({100 * avec_pid // max(1, len(toutes))} %)")
    # Couverture par generale recente : une page figee se voit ici, et nulle
    # part ailleurs. C'est la seule facon de distinguer « ce siege n'a pas
    # change » de « l'ANQ n'a pas mis sa page a jour ».
    print("\ncouverture par annee d'election :")
    for an in (2012, 2014, 2018, 2022):
        n = len({l["seat_id"] for l in toutes if l["annee"] == an})
        print(f"  {an} : {n:3} circonscriptions" + ("  <- incomplet" if n < 120 else ""))

    if len(sieges) < CIRCONSCRIPTIONS_MIN or len(toutes) < LIGNES_MIN:
        raise SystemExit(
            f"\nREFUS : {len(sieges)} circonscriptions et {len(toutes)} lignes "
            f"(seuils {CIRCONSCRIPTIONS_MIN} / {LIGNES_MIN}). Le balisage a "
            f"probablement change. Un releve partiel rendu en silence est pire "
            f"qu'une erreur : il a l'air complet.")

    if args.dry_run:
        print("\n--dry-run : rien n'a ete ecrit.")
        return

    ici = os.path.dirname(os.path.abspath(__file__))
    sortie = os.path.normpath(os.path.join(
        ici, "..", "extdata", "deputes_par_circonscription_qc.csv"))
    champs = ["seat_id", "seat", "annee", "partielle", "full_name",
              "party_id", "person_id", "remarque"]
    with open(sortie, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=champs)
        w.writeheader()
        w.writerows(sorted(toutes, key=lambda l: (l["seat_id"], l["annee"])))
    print(f"\nEcrit : {sortie}")


if __name__ == "__main__":
    main()
