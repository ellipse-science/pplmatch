#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""One-time script to build the assnat_ids_qc.json mapping file.

Fetches all deputies (former and current) from www.assnat.qc.ca and matches
them against the deputies in members_historic_qc.csv using normalized names.

Output: inst/extdata/sources/assnat_ids_qc.json
  [{"full_name": "francois legault", "assnat_id": 4131, "assnat_url": "/fr/deputes/legault-francois-4131/index.html"}, ...]

Run once (or when members_historic_qc.csv is updated):
  python3 inst/python/build_assnat_ids.py
"""

import csv
import html as html_module
import http.cookiejar
import json
import os
import re
import time
import unicodedata
import urllib.request

HEADERS = {"User-Agent": "Mozilla/5.0 (compatible; pplmatch-research-tool)"}
SEARCH_URL = "https://www.assnat.qc.ca/Gabarits/RechercheAvancee.aspx/RechercherAsync"
REQUEST_DELAY = 1.0  # seconds


def _norm(text):
    text = str(text).lower()
    nfkd = unicodedata.normalize("NFD", text)
    text = "".join(c for c in nfkd if unicodedata.category(c) != "Mn")
    text = re.sub(r"[^a-z ]", " ", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text


def _norm_search_result(nom):
    """'Charbonneau, Francine' -> 'francine charbonneau'."""
    if "," in nom:
        parts = nom.split(",", 1)
        nom = parts[1].strip() + " " + parts[0].strip()
    return _norm(nom)


def fetch_former_deputies(opener):
    """Fetch all former deputies in one large request via RechercherAsync."""
    print("  Fetching AncienDeputes...", end=" ", flush=True)
    payload = json.dumps({
        "objRech": {
            "CodeLangue": "fr",
            "EstNouvelleRequete": True,
            "NomRequete": "",
            "MotsCles": "",
            "SectionRecherche": "3",
            "EtatDepute": "AncienDeputes",
            "Circonscription": "",
            "AllegeancePolitique": "",
            "RegionAdministrative": "",
            "FonctionPolitique": "",
            "CommissionParlementaire": "",
            "TypeDocument": "",
            "TypeTravaux": "",
            "NombreParPage": 3000,
            "PageCourante": 0,
        }
    }).encode("utf-8")

    req = urllib.request.Request(
        SEARCH_URL,
        data=payload,
        headers={
            **HEADERS,
            "Content-Type": "application/json; charset=utf-8",
            "Accept": "application/json",
            "X-Requested-With": "XMLHttpRequest",
            "Referer": "https://www.assnat.qc.ca/fr/recherche/recherche-avancee.html",
        },
    )
    resp = opener.open(req, timeout=90)
    d = json.loads(resp.read().decode("utf-8"))
    resultats = d.get("d", {}).get("Resultats", []) or []
    print(f"{len(resultats)} deputies")
    return resultats


def fetch_current_deputies():
    """Fetch current deputies from the HTML index page (rendered server-side).

    The table contains rows like:
      <a href="/fr/deputes/legault-francois-4131/index.html">Legault, François</a>
    We extract both the displayed name ('Legault, François') and the numeric ID.
    """
    print("  Fetching current deputies from HTML...", end=" ", flush=True)
    req = urllib.request.Request(
        "https://www.assnat.qc.ca/fr/deputes/index.html",
        headers=HEADERS,
    )
    html = urllib.request.urlopen(req, timeout=30).read().decode("utf-8", errors="replace")

    resultats = []
    # Match <a href="/fr/deputes/{slug}-{id}/index.html">Displayed Name</a>
    pattern = re.compile(
        r'href="/fr/deputes/[a-z0-9\-]+-(\d+)/index\.html"[^>]*>([^<]+)</a>',
        re.IGNORECASE,
    )
    seen_ids = set()
    for m in pattern.finditer(html):
        dep_id = int(m.group(1))
        display_name = m.group(2).strip()
        display_name = html_module.unescape(display_name).strip()
        if not display_name or dep_id in seen_ids:
            continue
        # Filter out navigation links (they have very short or generic text)
        if len(display_name) < 3 or display_name.lower() in ("index", "liste", "retour"):
            continue
        seen_ids.add(dep_id)
        slug_m = re.search(
            r'/fr/deputes/([a-z0-9\-]+-' + str(dep_id) + r')/index\.html',
            html,
        )
        url_path = f"/fr/deputes/{slug_m.group(1)}/index.html" if slug_m else ""
        resultats.append({"Id": dep_id, "Nom": display_name, "URL": url_path})

    print(f"{len(resultats)} deputies")
    return resultats


def build_mapping(resultats):
    """Build two indexes from search results:
      - by normalized name with spaces  ('jean francois lisee')
      - by normalized name without spaces ('jeanfrancoislee')
    Returns dict: key -> (id, url)
    """
    mapping = {}
    for r in resultats:
        nom = r.get("Nom", "")
        dep_id = r.get("Id")
        url = r.get("URL", "")
        if not nom or not dep_id or not url:
            continue
        norm_name = _norm_search_result(nom)
        nospace = norm_name.replace(" ", "")
        for key in (norm_name, nospace):
            if key not in mapping or dep_id > mapping[key][0]:
                mapping[key] = (dep_id, url)
    return mapping


def _lookup(full_name, assnat_map):
    """Try exact match, then spaceless match."""
    norm = _norm(full_name)
    if norm in assnat_map:
        return assnat_map[norm]
    nospace = norm.replace(" ", "")
    return assnat_map.get(nospace, None)


def main():
    # Paths relative to package root
    script_dir = os.path.dirname(os.path.abspath(__file__))
    pkg_root = os.path.dirname(script_dir)
    members_csv = os.path.join(pkg_root, "extdata", "members_historic_qc.csv")
    output_json = os.path.join(pkg_root, "extdata", "sources", "assnat_ids_qc.json")

    # Load member names from CSV
    print("Loading members from CSV...")
    member_names = set()
    with open(members_csv, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            member_names.add(row["full_name"].strip())
    print(f"  {len(member_names)} unique member names")

    # Set up HTTP session
    cj = http.cookiejar.CookieJar()
    opener = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(cj))
    # Warm up session
    opener.open(urllib.request.Request(
        "https://www.assnat.qc.ca/fr/recherche/recherche-avancee.html?SectionRecherche=3",
        headers=HEADERS,
    ), timeout=15)
    time.sleep(REQUEST_DELAY)

    # Fetch all deputies
    print("Fetching deputies from assnat.qc.ca...")
    ancien_deps = fetch_former_deputies(opener)
    time.sleep(REQUEST_DELAY)
    current_deps = fetch_current_deputies()

    # Build unified mapping (current deputies override former if same name)
    all_deps = ancien_deps + current_deps  # current last = higher priority
    assnat_map = build_mapping(all_deps)
    print(f"  Total unique normalized names in assnat: {len(assnat_map)}")

    # Match members
    results = []
    matched = 0
    not_found = []

    for full_name in sorted(member_names):
        hit = _lookup(full_name, assnat_map)
        if hit is not None:
            dep_id, dep_url = hit
            results.append({
                "full_name": full_name,
                "assnat_id": dep_id,
                "assnat_url": dep_url,
            })
            matched += 1
        else:
            not_found.append(full_name)

    print(f"\nMatched: {matched}/{len(member_names)}")
    if not_found:
        print(f"Not found ({len(not_found)}):")
        for n in not_found:
            print(f"  {n!r}")

    # Save output
    with open(output_json, "w", encoding="utf-8") as f:
        json.dump(results, f, ensure_ascii=False, indent=2)
    print(f"\nSaved {len(results)} entries to {output_json}")


if __name__ == "__main__":
    main()
