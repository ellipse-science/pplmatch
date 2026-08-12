#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Releve la composition COURANTE de l'Assemblee depuis l'index de l'ANQ.

POURQUOI CE SCRIPT EXISTE
-------------------------
`members_historic_qc.csv` s'arrete au 2025-03-17 et ne connait que 121 des 125
sieges de la 43e legislature. Les manquants ne sont pas mal attribues : ils
sont `unmatched`, donc JETES par le raffineur qui consomme pplmatch. La parole
disparait au lieu d'etre comptee.

Un fichier `deputes_leg43_qc.csv` existait deja, mais il est defectueux :
125 lignes pour seulement 117 circonscriptions distinctes — huit lignes ont un
champ `circonscription` VIDE, dont celle d'Alex Boissonneault (Arthabaska).
C'est precisement le siege qui manquait. Ce script-ci lit la colonne dans le
tableau plutot que dans la fiche, et verifie son propre resultat.

CE QU'IL GARANTIT
-----------------
Il ECHOUE si le releve n'a pas exactement 125 sieges distincts. Un scraper qui
rend 117 sieges en silence est pire qu'un scraper absent : il donne une fausse
impression de completude, et c'est exactement ce qui s'est produit ici.

USAGE
    python3 inst/python/build_deputes_courants.py [--dry-run]
"""

import argparse
import csv
import html as html_module
import os
import re
import unicodedata
import urllib.request

URL = "https://www.assnat.qc.ca/fr/deputes/index.html"
HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; pplmatch-research-tool)"}
SIEGES_ATTENDUS = 125

PARTIS = {
    "coalition avenir quebec": "CAQ",
    "parti liberal du quebec": "PLQ",
    "parti quebecois": "PQ",
    "quebec solidaire": "QS",
    "parti conservateur du quebec": "PCQ",
    "independant": "IND",
    "independante": "IND",
}


def _norm(s):
    return "".join(c for c in unicodedata.normalize("NFD", s)
                   if unicodedata.category(c) != "Mn").lower()


def district_id(nom):
    return re.sub(r"[^a-z0-9]", "", _norm(nom))


def _cellules(tr):
    out = []
    for c in re.findall(r"<td[^>]*>(.*?)</td>", tr, re.S):
        t = html_module.unescape(re.sub(r"<[^>]+>", " ", c))
        t = re.sub(r"\s+", " ", t).strip()
        if t and t != "Courriel":
            out.append(t)
    return out


def releve(html):
    """Rend [{person_id, full_name, circonscription, seat_id, party_id, url}]."""
    lignes = []
    for tr in re.findall(r"<tr[^>]*>(.*?)</tr>", html, re.S):
        if "deputes/" not in tr:
            continue
        cells = _cellules(tr)
        if len(cells) < 3:
            continue
        nom, circo, parti = cells[0], cells[1], cells[2]
        m = re.search(r"/fr/deputes/([a-z0-9\-]+?)-(\d+)/", tr)
        pid = int(m.group(2)) if m else None
        code = None
        for libelle, cd in PARTIS.items():
            if libelle in _norm(parti):
                code = cd
                break
        lignes.append({
            "person_id": pid,
            "full_name": nom,
            "circonscription": circo,
            "seat_id": district_id(circo),
            "party_id": code or parti,
            "assnat_url": m.group(0) if m else "",
        })
    return lignes


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    html = urllib.request.urlopen(
        urllib.request.Request(URL, headers=HEADERS), timeout=30
    ).read().decode("utf-8", "replace")
    lignes = releve(html)

    sieges = {l["seat_id"] for l in lignes if l["seat_id"]}
    vides = [l for l in lignes if not l["seat_id"]]
    print(f"lignes lues        : {len(lignes)}")
    print(f"sieges distincts   : {len(sieges)}")
    print(f"lignes sans siege  : {len(vides)}")
    par_parti = {}
    for l in lignes:
        par_parti[l["party_id"]] = par_parti.get(l["party_id"], 0) + 1
    print(f"par parti          : {dict(sorted(par_parti.items()))}")

    # Le garde-fou : on refuse de publier un releve incomplet.
    if len(sieges) != SIEGES_ATTENDUS or vides:
        raise SystemExit(
            f"\nREFUS : {len(sieges)} sieges distincts (attendu {SIEGES_ATTENDUS}), "
            f"{len(vides)} ligne(s) sans circonscription. Le releve n'est pas publie — "
            f"c'est exactement le defaut de deputes_leg43_qc.csv, qui rendait 117 sieges "
            f"en silence.")

    if args.dry_run:
        print("\n--dry-run : rien n'a ete ecrit.")
        return

    ici = os.path.dirname(os.path.abspath(__file__))
    sortie = os.path.normpath(os.path.join(ici, "..", "extdata", "deputes_courants_qc.csv"))
    champs = ["person_id", "full_name", "circonscription", "seat_id", "party_id", "assnat_url"]
    with open(sortie, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=champs)
        w.writeheader()
        w.writerows(sorted(lignes, key=lambda l: l["seat_id"]))
    print(f"\nEcrit : {sortie}")


if __name__ == "__main__":
    main()
