#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Releve les dates de demission depuis les FICHES individuelles de l'ANQ.

POURQUOI UNE TROISIEME SOURCE
-----------------------------
Neuf mandats restaient en `confidence=disputed` : la chronologie dit
« demissionne » sans que sa date puisse etre confirmee ailleurs, et la colonne
Remarques de l'historique par circonscription ne les porte pas.

La notice biographique de chaque depute, elle, les porte :

    Demissionna comme deputee le 20 mars 2006.                    (Marois)
    Demissionna le 26 septembre 2015.                             (Dutil)
    Ministre des Affaires municipales du 20 octobre 2022 au
    4 septembre 2025, date de sa demission comme deputee.         (Laforest)

C'est une source INDEPENDANTE de la chronologie : elle est redigee par la
Bibliotheque de l'ANQ a partir du dossier du membre, pas a partir du Journal
des debats. Deux sources qui concordent ne prouvent rien si elles se recopient
— celles-ci ne se recopient pas.

CE QU'ELLE NE FAIT PAS
----------------------
Elle ne dit rien des changements d'allegeance : la notice ne porte que le parti
COURANT, sans date (verifie sur Youri Chassin). Elle arbitre les DEPARTS, pas
les defections.

USAGE
    python3 inst/python/build_demissions_fiches.py [--dry-run] [--cache DIR]
"""

import argparse
import csv
import html as html_module
import os
import re
import time
import unicodedata
import urllib.request

BASE = "https://www.assnat.qc.ca"
HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; pplmatch-research-tool)"}

MOIS = {"janvier": 1, "fevrier": 2, "mars": 3, "avril": 4, "mai": 5, "juin": 6,
        "juillet": 7, "aout": 8, "septembre": 9, "octobre": 10,
        "novembre": 11, "decembre": 12}
_M = "|".join(MOIS)

# Trois tournures observees, et pas une de plus : on ne devine pas de formes
# qu'on n'a pas vues, sinon le motif attrape ce qu'il ne comprend pas.
# Le role, s'il est nomme, doit etre le SIEGE. Une notice raconte toute une
# carriere, donc toutes ses demissions : celle d'Andre Boisclair « comme chef du
# Parti quebecois le 8 mai 2007 » se lisait comme un depart de l'Assemblee, six
# mois avant qu'il quitte reellement son siege. C'est la meme erreur que le
# « president de l'Assemblee nationale Yvon Vallieres, depute de Richmond ».
RE_DEMISSION = [
    # « Demissionna le 26 septembre 2015 » — aucun role nomme.
    re.compile(r"demissionn\w*\s+le\s+"
               r"(\d{1,2})(?:er)?\s+(" + _M + r")\s+(\d{4})"),
    # « Demissionna comme deputee le 20 mars 2006 » — le role est le siege.
    re.compile(r"demissionn\w*\s+(?:comme|a titre de)\s+deputee?\s+le\s+"
               r"(\d{1,2})(?:er)?\s+(" + _M + r")\s+(\d{4})"),
    # « ... au 4 septembre 2025, date de sa demission comme deputee »
    re.compile(r"\bau\s+(\d{1,2})(?:er)?\s+(" + _M + r")\s+(\d{4})\s*,\s*"
               r"date de sa demission\s+comme\s+deputee?"),
]


def _norm(s):
    return "".join(c for c in unicodedata.normalize("NFD", s or "")
                   if unicodedata.category(c) != "Mn").lower()


def fetch(url, cache=None):
    nom = re.sub(r"[^a-z0-9]", "_", url.lower()) + ".html"
    if cache:
        chemin = os.path.join(cache, nom)
        if os.path.exists(chemin):
            return open(chemin, encoding="utf-8").read()
    req = urllib.request.Request(BASE + url, headers=HEADERS)
    corps = urllib.request.urlopen(req, timeout=30).read().decode("utf-8", "replace")
    if cache:
        os.makedirs(cache, exist_ok=True)
        open(os.path.join(cache, nom), "w", encoding="utf-8").write(corps)
    time.sleep(1)          # on ne martele pas un site public
    return corps


def dates_demission(html):
    """Rend [(iso, phrase)] — une notice raconte une CARRIERE.

    Quelqu'un peut avoir demissionne plus d'une fois, de sieges differents. Ne
    retenir que la premiere date attribuait a un mandat la demission d'un
    autre — d'ou des ecarts de dix-sept ans. On les rend toutes, et l'appelant
    garde celle qui tombe dans le mandat qu'il examine.
    """
    texte = re.sub(r"\s+", " ", html_module.unescape(re.sub(r"<[^>]+>", " ", html)))
    plat = _norm(texte)
    out, vus = [], set()
    for rx in RE_DEMISSION:
        for m in rx.finditer(plat):
            try:
                iso = f"{int(m.group(3)):04d}-{MOIS[m.group(2)]:02d}-{int(m.group(1)):02d}"
            except (KeyError, ValueError):
                continue
            if iso in vus:
                continue
            vus.add(iso)
            # La phrase d'origine, accents compris, pour qu'un humain puisse la
            # contester sans retourner sur le site.
            deb = max(0, m.start() - 60)
            out.append((iso, texte[deb:m.end() + 30].strip()))
    return sorted(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--cache", default=None)
    args = ap.parse_args()

    ici = os.path.dirname(os.path.abspath(__file__))
    extdata = os.path.normpath(os.path.join(ici, "..", "extdata"))

    personnes = {r["person_id"]: r for r in csv.DictReader(
        open(os.path.join(extdata, "persons_qc.csv"), encoding="utf-8"))}
    mandats = list(csv.DictReader(
        open(os.path.join(extdata, "mandates_qc.csv"), encoding="utf-8")))

    # On ne visite que les personnes dont un mandat se ferme sur une demission :
    # inutile d'interroger 1870 fiches pour en exploiter soixante.
    cibles = {}
    for m in mandats:
        if m["end_reason"] == "resignation" and m["person_id"]:
            cibles.setdefault(m["person_id"], []).append(m)

    print(f"personnes a interroger : {len(cibles)}")
    lignes, sans = [], 0
    for pid, ms in sorted(cibles.items()):
        p = personnes.get(pid)
        url = (p or {}).get("assnat_url", "")
        if not url:
            sans += 1
            continue
        try:
            trouvees = dates_demission(fetch(url, args.cache))
        except Exception as e:                                   # noqa: BLE001
            print(f"  ECHEC {pid} ({e})")
            continue
        if not trouvees:
            sans += 1
            continue
        # On rattache chaque date au mandat qu'elle CLOT : celui qui etait
        # ouvert ce jour-la. Une date qui ne tombe dans aucun mandat de cette
        # personne ne concerne pas nos tables, et on ne la retient pas.
        for iso, phrase in trouvees:
            for m in ms:
                if m["date_start"] <= iso <= m["date_end"]:
                    lignes.append({"person_id": pid, "seat_id": m["seat_id"],
                                   "full_name": p.get("full_name", ""),
                                   "date_demission": iso, "phrase": phrase,
                                   "assnat_url": url})
                    break

    print(f"dates trouvees        : {len(lignes)}")
    print(f"fiches sans date      : {sans}")

    # Comparaison avec ce que nos tables disent deja.
    accord = ecart = 0
    par_cle = {(l["person_id"], l["seat_id"]): l["date_demission"] for l in lignes}
    for m in mandats:
        cle = (m["person_id"], m["seat_id"])
        if m["end_reason"] != "resignation" or cle not in par_cle:
            continue
        if m["date_end"] == par_cle[cle]:
            accord += 1
        else:
            ecart += 1
            print(f"  ECART {m['seat_id']:22} nous {m['date_end']} "
                  f"-> fiche {par_cle[cle]}")
    print(f"\naccord {accord} | ecart {ecart}")

    if args.dry_run:
        print("\n--dry-run : rien n'a ete ecrit.")
        return

    sortie = os.path.join(extdata, "demissions_fiches_qc.csv")
    with open(sortie, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["person_id", "seat_id", "full_name",
                                          "date_demission", "phrase", "assnat_url"])
        w.writeheader()
        w.writerows(sorted(lignes, key=lambda l: l["date_demission"]))
    print(f"Ecrit : {sortie}")


if __name__ == "__main__":
    main()
