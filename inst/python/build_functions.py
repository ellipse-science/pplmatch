#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Releve les FONCTIONS DATEES des deputes de la 43e legislature.

POURQUOI CE SCRIPT EXISTE
-------------------------
La dimension ne connaissait que les mandats (qui siege, pour quel parti). Or
une grande part de ce qui distingue deux deputes tient a leurs fonctions :
ministre, whip, presidence de commission, porte-parole de l'opposition... et
c'est elles qui fixent la remuneration. La seule trace existante dans
l'entrepot, la colonne `functions` de `dim-qc-parliament-members-staging`, est
un instantane du 2024-06-21 (Francois Legault premier ministre) : inutilisable.

La fiche de chaque depute sur assnat.qc.ca liste TOUTES ses fonctions avec
leurs dates (« ... du 29 novembre 2022 au 27 aout 2026 », « ... depuis le
7 novembre 2024 »). C'est la source.

LES ANCIENS DEPUTES
-------------------
Un depute qui a quitte l'Assemblee n'est plus dans l'index, et sa page ne
donne plus qu'une biographie EN PROSE. Leurs fonctions sont donc transcrites a
la main dans `functions_transcribed_qc.csv` (source et date de lecture dans le
fichier). Ce script les fusionne et verifie que chaque ancien de la 43e y
figure au moins par un commentaire de verification.

CE QU'IL GARANTIT
-----------------
Il ECHOUE si le releve courant n'a pas 125 deputes, si une fiche n'a aucune
fonction datee lisible, ou si une date est illisible. Une ligne perdue en
silence ferait croire a un depute sans fonction.

USAGE
    python3 inst/python/build_functions.py [--cache DOSSIER] [--dry-run]
"""

import argparse
import csv
import html as html_module
import os
import re
import sys
import time
import urllib.request

SITE = "https://www.assnat.qc.ca"
HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; pplmatch-research-tool)"}
DEBUT_LEGISLATURE = "2022-10-03"
FIN_LEGISLATURE = "2026-10-05"  # meme borne que mandates_qc (jour du scrutin suivant)
DEPUTES_ATTENDUS = 125

MOIS = {"janvier": 1, "février": 2, "fevrier": 2, "mars": 3, "avril": 4, "mai": 5, "juin": 6,
        "juillet": 7, "août": 8, "aout": 8, "septembre": 9, "octobre": 10, "novembre": 11,
        "décembre": 12, "decembre": 12}

PERMANENTES = ("de l’administration publique|de l’agriculture, des pêcheries, de l’énergie et des "
               "ressources naturelles|de l’aménagement du territoire|de la culture et de l’éducation|"
               "de l’économie et du travail|des finances publiques|des institutions|des relations "
               "avec les citoyens|de la santé et des services sociaux|des transports et de "
               "l’environnement")

# (motif, code, categorie du bareme, % de l'indemnite de base). Premier motif
# qui correspond l'emporte. Categories : celles du tableau de l'Assemblee
# (indemnity_scale_qc.csv). Les porte-parole n'ont pas d'indemnite ; ils ont
# un code parce qu'ils structurent le travail de l'opposition.
BAREME = [
    (r"^Premi(?:ère|er) ministre$", "PM", "premier_ministre", 105),
    (r"^Ministre\b", "M", "ministre", 75),
    (r"^Leader parlementaire du gouvernement$", "M", "ministre", 75),
    (r"^Président(?:e)? de l’Assemblée nationale$", "PAN", "presidence_assemblee", 75),
    (r"^(?:Première|Deuxième|Troisième) vice-président(?:e)? de l’Assemblée nationale$", "VP", "vice_presidence_assemblee", 35),
    (r"^Chef(?:fe)? de l’opposition officielle$", "CO", "chef_opposition_officielle", 75),
    (r"^Chef(?:fe)? du (?:deuxième|troisième) groupe d’opposition$", "CO", "chef_autre_groupe_opposition", 35),
    (r"^Leader parlementaire de l’opposition officielle$", "LP", "leader_opposition_officielle", 35),
    (r"^Leader parlementaire du deuxième groupe d’opposition$", "LP", "leader_deuxieme_groupe", 25),
    (r"^Whip en chef du gouvernement$", "W", "whip_chef_gouvernement", 35),
    (r"^Whip en chef de l’opposition officielle$", "W", "whip_chef_opposition_officielle", 30),
    (r"^Whip du deuxième groupe d’opposition$", "W", "whip_deuxieme_groupe", 20),
    (r"^Leader parlementaire adjoint(?:e)? du gouvernement$", "LP", "leader_adjoint_gouvernement", 25),
    (r"^Leader parlementaire adjoint(?:e)? de l’opposition officielle$", "LP", "leader_adjoint_opposition_officielle", 20),
    (r"^Whip adjoint(?:e)? du gouvernement$", "W", "whip_adjoint_gouvernement", 20),
    (r"^Président(?:e)? du caucus du gouvernement$", "PCA", "presidence_caucus_gouvernement", 25),
    (r"^Président(?:e)? du caucus de l’opposition officielle$", "PCA", "presidence_caucus_opposition_officielle", 22.5),
    (r"^Adjoint(?:e)? parlementaire\b", "AP", "adjoint_parlementaire", 20),
    (rf"^Président(?:e)? de la Commission (?:{PERMANENTES})$", "PC", "presidence_commission_permanente", 25),
    (rf"^Vice-président(?:e)? de la Commission (?:{PERMANENTES})$", "VC", "vice_presidence_commission_permanente", 20),
    (r"^Président(?:e)? de séance$", "PS", "presidence_seance", 15),
    (r"^Membre du Bureau de l’Assemblée nationale$", "B", "membre_bureau", 15),
    (r"^Porte-parole\b", "PP", "", 0),
]


def iso(texte):
    m = re.search(r"(\d{1,2})(?:er)? (\w+) (\d{4})", texte)
    if not m or m.group(2).lower() not in MOIS:
        return None
    return f"{m.group(3)}-{MOIS[m.group(2).lower()]:02d}-{int(m.group(1)):02d}"


def texte(fragment):
    t = html_module.unescape(re.sub(r"<[^>]+>", "", fragment))
    return re.sub(r"\s+", " ", t).strip()


def classer(titre):
    for motif, code, categorie, pct in BAREME:
        if re.search(motif, titre):
            return code, categorie, pct
    return "", "", 0


def fonctions_datees(page):
    """[(titre, debut, fin)] de la section « Fonctions politiques... »."""
    i = page.find("Fonctions politiques, parlementaires")
    if i < 0:
        return None
    out = []
    for li in re.findall(r"<li>(.*?)</li>", page[i:], re.S):
        t = texte(li)
        m = re.match(r"^(.*) du (.+?\d{4}) au (.+?\d{4})$", t)
        if m:
            out.append((m.group(1), iso(m.group(2)), iso(m.group(3))))
            continue
        m = re.match(r"^(.*) depuis le (.+?\d{4})$", t)
        if m:
            out.append((m.group(1), iso(m.group(2)), ""))
    return out


def telecharger(url, fichier_cache):
    if fichier_cache and os.path.exists(fichier_cache):
        with open(fichier_cache, encoding="utf-8") as f:
            return f.read()
    req = urllib.request.Request(url, headers=HEADERS)
    with urllib.request.urlopen(req, timeout=60) as r:
        page = r.read().decode("utf-8", "replace")
    if fichier_cache:
        with open(fichier_cache, "w", encoding="utf-8") as f:
            f.write(page)
    time.sleep(0.7)  # une page a la fois : c'est le site de l'Assemblee
    return page


def chevauche_legislature(debut, fin):
    return debut <= FIN_LEGISLATURE and (not fin or fin >= DEBUT_LEGISLATURE)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--cache", default=None, help="dossier de cache des fiches")
    p.add_argument("--dry-run", action="store_true")
    a = p.parse_args()
    ici = os.path.dirname(os.path.abspath(__file__))
    extdata = os.path.join(ici, "..", "extdata")
    if a.cache:
        os.makedirs(a.cache, exist_ok=True)

    with open(os.path.join(extdata, "deputes_courants_qc.csv"), encoding="utf-8") as f:
        courants = list(csv.DictReader(f))
    if len(courants) != DEPUTES_ATTENDUS:
        sys.exit(f"ECHEC : {len(courants)} deputes courants, {DEPUTES_ATTENDUS} attendus.")

    lignes, fautes = [], []
    for n, d in enumerate(courants, 1):
        url = SITE + d["assnat_url"].rstrip("/") + "/index.html"
        cache = os.path.join(a.cache, f"{d['person_id']}.html") if a.cache else None
        page = telecharger(url, cache)
        brutes = fonctions_datees(page)
        if not brutes:
            fautes.append(f"{d['person_id']} : aucune fonction datee lisible")
            continue
        for titre, debut, fin in brutes:
            if debut is None or fin is None:
                fautes.append(f"{d['person_id']} : date illisible pour « {titre} »")
                continue
            if not chevauche_legislature(debut, fin):
                continue
            code, categorie, pct = classer(titre)
            lignes.append({"person_id": d["person_id"], "title": titre, "function_code": code,
                           "scale_category": categorie, "scale_pct": pct, "date_start": debut,
                           "date_end": fin, "source": "assnat_fiche", "confidence": "verified"})
        print(f"\r  {n}/{len(courants)} fiches", end="", file=sys.stderr)
    print(file=sys.stderr)

    # Anciens deputes : transcription a la main depuis leur biographie.
    with open(os.path.join(extdata, "functions_transcribed_qc.csv"), encoding="utf-8") as f:
        transcrites = list(csv.DictReader(f))
    # Chaque depute de la 43e qui n'est plus au releve courant doit etre
    # transcrit : sinon il sortirait « sans fonction » alors qu'on ne l'a pas lu.
    with open(os.path.join(extdata, "mandates_qc.csv"), encoding="utf-8") as f:
        du_mandat = {r["person_id"] for r in csv.DictReader(f)
                     if r["person_id"] and chevauche_legislature(r["date_start"], r["date_end"])}
    manquants = du_mandat - {d["person_id"] for d in courants} - {t["person_id"] for t in transcrites}
    if manquants:
        fautes.append("anciens deputes de la 43e sans transcription : " + ", ".join(sorted(manquants)))
    for t in transcrites:
        code, categorie, pct = classer(t["title"])
        lignes.append({"person_id": t["person_id"], "title": t["title"], "function_code": code,
                       "scale_category": categorie, "scale_pct": pct, "date_start": t["date_start"],
                       "date_end": t["date_end"], "source": t["source"], "confidence": "transcribed"})

    if fautes:
        sys.exit("ECHEC :\n  " + "\n  ".join(fautes))

    lignes.sort(key=lambda r: (int(r["person_id"]), r["date_start"], r["title"]))
    payees = sum(1 for r in lignes if r["scale_pct"])
    print(f"  {len(lignes)} fonctions ({payees} remunerees, "
          f"{sum(1 for r in lignes if r['function_code'] == 'PP')} postes de porte-parole, "
          f"{len(transcrites)} transcrites)", file=sys.stderr)
    if a.dry_run:
        return
    champs = ["person_id", "title", "function_code", "scale_category", "scale_pct",
              "date_start", "date_end", "source", "confidence"]
    with open(os.path.join(extdata, "functions_qc.csv"), "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=champs)
        w.writeheader()
        w.writerows(lignes)
    print("  -> inst/extdata/functions_qc.csv", file=sys.stderr)


if __name__ == "__main__":
    main()
