#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Resultats par circonscription des scrutins de la 43e legislature.

POURQUOI CE SCRIPT EXISTE
-------------------------
`elections_qc.csv` ne donne qu'un resume par generale (parti au pouvoir,
sieges). Rien ne disait, circonscription par circonscription, qui s'est
presente, avec quel score, ni de combien le gagnant l'a emporte. Le registre
des donnees de reference de la Vitrine notait d'ailleurs les « candidat.e.s
par election » comme INEXISTANTS.

Source : donnees ouvertes d'Elections Quebec (dgeq.org, « Archives des
donnees »), un resultats.json par scrutin. On garde TOUS les candidats.

CE QU'IL GARANTIT
-----------------
Il ECHOUE si un scrutin n'est pas definitif, si une circonscription n'a pas
exactement un elu, si la generale n'a pas 125 circonscriptions, ou si un elu
ne correspond a AUCUN mandat de `mandates_qc.csv` commencant ce jour-la dans
ce siege : deux sources independantes doivent dire la meme chose.

USAGE
    python3 inst/python/build_election_results.py [--dry-run]
"""

import argparse
import csv
import json
import os
import re
import sys
import time
import unicodedata
import urllib.request

ARCHIVES = "https://donnees.electionsquebec.qc.ca/production/provincial/resultats/archives"
HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; pplmatch-research-tool)"}

# Scrutins de la 43e legislature. A completer si une partielle s'ajoute.
SCRUTINS = [
    ("gen2022-10-03", "2022-10-03", "general"),
    ("part2023-03-13", "2023-03-13", "by_election"),
    ("part2023-10-02", "2023-10-02", "by_election"),
    ("part2025-03-17", "2025-03-17", "by_election"),
    ("part2025-08-11", "2025-08-11", "by_election"),
    ("part2026-02-23", "2026-02-23", "by_election"),
]

# Abreviations d'Elections Quebec → party_id de la dimension. Les autres
# partis gardent leur abreviation brute dans `party_abbrev` et un party_id vide.
PARTIS = {"CAQ": "CAQ", "PLQ": "PLQ", "QS": "QS", "PQ": "PQ", "PCQ": "PCQ"}


def _norm(s):
    return "".join(c for c in unicodedata.normalize("NFD", s or "")
                   if unicodedata.category(c) != "Mn").lower()


def seat_id(nom):
    return re.sub(r"[^a-z0-9]", "", _norm(nom))


def party_id(abrev):
    lettres = re.sub(r"[^A-Z]", "", (abrev or "").upper())
    for cle in sorted(PARTIS, key=len, reverse=True):
        if lettres.startswith(cle):
            return PARTIS[cle]
    return ""


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--dry-run", action="store_true")
    a = p.parse_args()
    extdata = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "extdata")

    with open(os.path.join(extdata, "mandates_qc.csv"), encoding="utf-8") as f:
        mandats = list(csv.DictReader(f))

    lignes, fautes = [], []
    for code, date, type_ in SCRUTINS:
        req = urllib.request.Request(f"{ARCHIVES}/{code}/resultats.json", headers=HEADERS)
        with urllib.request.urlopen(req, timeout=60) as r:
            donnees = json.loads(r.read().decode("utf-8"))
        circos = donnees["circonscriptions"]
        if type_ == "general" and len(circos) != 125:
            fautes.append(f"{code} : {len(circos)} circonscriptions, 125 attendues")
        for c in circos:
            if not c.get("isResultatsFinaux"):
                fautes.append(f"{code} {c['nomCirconscription']} : resultats non definitifs")
            sid = seat_id(c["nomCirconscription"])
            cands = sorted(c["candidats"], key=lambda x: -x["nbVoteTotal"])
            if len(cands) > 1 and cands[0]["nbVoteTotal"] == cands[1]["nbVoteTotal"]:
                fautes.append(f"{code} {sid} : egalite au premier rang")
            # L'elu : le mandat commencant ce jour-la dans ce siege. Deux sources
            # independantes (Elections Quebec, mandates_qc) doivent concorder.
            elus = {m["person_id"] for m in mandats if m["seat_id"] == sid and m["date_start"] == date}
            if len(elus) != 1:
                fautes.append(f"{code} {sid} : {len(elus)} mandat(s) commencant le {date} dans mandates_qc")
            for rang, x in enumerate(cands, 1):
                lignes.append({
                    "election_date": date, "election_type": type_, "election_code": code, "seat_id": sid,
                    "candidate_name": f"{x['prenom']} {x['nom']}".strip(),
                    "party_abbrev": x["abreviationPartiPolitique"], "party_id": party_id(x["abreviationPartiPolitique"]),
                    "votes": x["nbVoteTotal"], "vote_share": x["tauxVote"], "rank": rang,
                    "elected": "true" if rang == 1 else "false",
                    "lead_votes": (x.get("nbVoteAvance") or cands[0]["nbVoteTotal"] - (cands[1]["nbVoteTotal"] if len(cands) > 1 else 0)) if rang == 1 else "",
                    "person_id": next(iter(elus)) if rang == 1 and len(elus) == 1 else "",
                    "valid_votes": c["nbVoteValide"], "registered_voters": c["nbElecteurInscrit"],
                    "turnout": c["tauxParticipation"], "source": "dgeq_donnees_ouvertes",
                })
        time.sleep(0.5)

    if fautes:
        sys.exit("ECHEC :\n  " + "\n  ".join(fautes))
    elus = [r for r in lignes if r["elected"] == "true"]
    print(f"  {len(lignes)} candidatures, {len(elus)} elus, {len(SCRUTINS)} scrutins", file=sys.stderr)
    if a.dry_run:
        return
    champs = list(lignes[0].keys())
    with open(os.path.join(extdata, "election_results_qc.csv"), "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=champs)
        w.writeheader()
        w.writerows(lignes)
    print("  -> inst/extdata/election_results_qc.csv", file=sys.stderr)


if __name__ == "__main__":
    main()
