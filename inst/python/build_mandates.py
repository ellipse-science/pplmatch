#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Genere les trois tables du modele des mandats (voir design/spec-mandats-qc.md).

    persons_qc.csv   identites stables      <- assnat_ids_qc.json + referentiel
    seats_qc.csv     sieges et leurs noms   <- referentiel + renommages (chronologie)
    mandates_qc.csv  (personne x siege x parti) sur une periode

CE QUE CE SCRIPT NE PRETEND PAS FAIRE
--------------------------------------
Les elections GENERALES viennent du referentiel : ses `election_date` y sont
exactes. Tout le reste — defections, partielles, demissions — vient de la
Chronologie parlementaire, et la qualite d'extraction n'y est pas uniforme :

  · defections : bon rendement, motifs eprouves (8/10 sur la 43e legislature)
  · partielles : motif clair (« est elu a l'election partielle de X »), mais il
    faut exclure « se retire de la course a l'election partielle », qui n'est
    pas une election
  · demissions : AMBIGUES. « demissionne du caucus » est une DEFECTION, pas un
    depart de l'Assemblee ; et « le ministre X demissionne » peut viser le
    cabinet seul. Elles sortent donc en `confidence=disputed` — a trancher a
    la main, pas a deviner.

Toute ligne non certaine est marquee, jamais silencieusement omise : c'est la
regle de la spec (§ 5).

USAGE
    python3 inst/python/build_mandates.py --cache /tmp/chrono --dry-run
"""

import argparse
import csv
import json
import os
import re
import sys
from datetime import date, timedelta

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from build_party_changes import (  # noqa: E402
    FIRST_PAGE, LAST_PAGE, INDICES, _norm, _party_code, district_id,
    fetch_page, parse_entries, classify, legislature_for, PARTIS,
)

FIN_LOINTAINE = date(9999, 12, 31)


# ── Extracteurs supplementaires ──────────────────────────────────────────────

# Le parti apparait souvent en ADJECTIF plutot qu'en raison sociale :
# « la caquiste Joelle Boutin », « les liberaux », « la pequiste ».
ADJECTIFS_PARTI = [
    ("caquiste", "CAQ"), ("pequiste", "PQ"), ("peequiste", "PQ"),
    ("liberaux", "PLQ"), ("liberale", "PLQ"), ("liberal", "PLQ"),
    ("adequiste", "ADQ"), ("solidaire", "QS"), ("conservateur", "PCQ"),
]

# La circonscription se donne avec ou sans le mot « circonscription ».
RE_CIRCO_PARTIELLE = re.compile(
    r"(?:partielles?\s+(?:d[eu]\s+|d[’'])"          # « election partielle DE Rousseau »
    r"|circonscriptions?\s+(?:d[eu]\s+|d[’'])"
    r"|elu[e]?s?\s+(?:respectivement\s+)?dans\s+(?:la\s+circonscription\s+(?:d[eu]\s+|d[’']))?"
    r"|dans\s+)"
    r"([A-Z][\wÀ-ÿ\-–—’'\. ]{2,40})", re.U)

# Ce qui MENTIONNE une partielle sans en etre le resultat.
BRUIT_PARTIELLE = ("decret", "se retire", "retirent", "anniversaire",
                   "tenue d", "sera tenue", "aura lieu", "convoque")


def _partis_positionnes(texte):
    """[(position, code)] pour chaque parti nomme, raison sociale ou adjectif."""
    t = _norm(texte)
    out = []
    for libelle, code in PARTIS:
        for m in re.finditer(re.escape(libelle), t):
            out.append((m.start(), code))
    for adj, code in ADJECTIFS_PARTI:
        for m in re.finditer(r"\b" + adj, t):
            out.append((m.start(), code))
    return sorted(out)


def extract_partielle(texte):
    """Rend [(district_id, parti)] — un paragraphe peut annoncer PLUSIEURS
    partielles, chacune avec son propre parti.

    L'appariement se fait par POSITION : chaque circonscription prend le parti
    nomme le plus proche AVANT elle (« Richer du PQ est elu dans Argenteuil et
    Tanguay du PLQ dans LaFontaine »). Si aucun parti ne precede, on prend le
    plus proche apres — le cas « ... Les deux nouveaux deputes appartiennent au
    Parti liberal », ou le parti est annonce en fin de phrase.
    """
    t = _norm(texte)
    if "partielle" not in t:
        return []
    if any(b in t for b in BRUIT_PARTIELLE):
        return []
    if not re.search(r"\bes[t]? elu[e]?s?\b|\bsont elu[e]?s\b|\belu[e]?s? dans\b", t):
        return []

    partis = _partis_positionnes(texte)
    if not partis:
        return []

    out, vus = [], set()
    for m in RE_CIRCO_PARTIELLE.finditer(texte):
        brut = m.group(1).strip()
        brut = re.sub(r"\s+(et|ou|a l|de la|du)\s*$", "", brut, flags=re.I).strip()
        did = district_id(brut)
        if not did or len(did) < 3 or did in vus:
            continue
        pos = m.start()
        avant = [c for p, c in partis if p < pos]
        parti = avant[-1] if avant else partis[0][1]
        vus.add(did)
        out.append((did, parti))
    return out


# Le siege peut etre nomme AVANT ou APRES le mot « demission », et sous
# plusieurs formes. Motifs releves sur le texte reel (2026-08-12) :
#   « Demission du depute de LaFontaine. »
#   « Demission d'Andree Laforest, a titre de ministre [...] et de deputee de Chicoutimi. »
#   « Demission d'Eric Lefebvre, depute independant d'Arthabaska. »
#   « Jean-Pierre Belisle, depute liberal de Mille-Iles, annonce sa demission. »
RE_DEMISSION_SIEGE = [
    re.compile(r"demission[^.]{0,140}?deputee?\s+(?:liberal[e]?\s+|independant[e]?\s+)?"
               r"(?:d[eu]s?\s+|d[’'])([^,.;]+)", re.I),
    re.compile(r"deputee?\s+(?:liberal[e]?\s+|independant[e]?\s+)?(?:d[eu]s?\s+|d[’'])"
               r"([^,.;]+)[^.]{0,90}demission", re.I),
]


def extract_demission(texte):
    """Depart de l'ASSEMBLEE (pas du caucus, pas du seul cabinet)."""
    t = _norm(texte)
    if "demission" not in t:
        return None
    if "caucus" in t:            # c'est une defection, traitee ailleurs
        return None

    m = None
    for rx in RE_DEMISSION_SIEGE:
        m = rx.search(t)
        if m:
            break
    if not m:
        return None

    # On demissionne aussi d'une FONCTION sans quitter l'Assemblee — « le depute
    # de Chauveau demissionne de son poste de deuxieme vice-president » (il siege
    # toujours). Mais on peut demissionner des DEUX : « a titre de ministre [...]
    # ET DE DEPUTEE DE Chicoutimi ». C'est donc la mention du SIEGE dans ce dont
    # on demissionne qui tranche, pas la mention d'un ministere.
    FONCTIONS = ("de son poste", "de ses fonctions", "de son role",
                 "de la presidence", "vice-president", "vice president",
                 "de son siege au conseil", "de la fonction")
    quitte_le_siege = re.search(r"(?:et\s+)?(?:a titre\s+)?de\s+deputee?\s+d[eu]s?\s+", t) \
        or re.search(r"demission du deputee?\s", t) \
        or re.search(r"deputee?\s+independant[e]?\s+d[’'e]", t)
    if not quitte_le_siege and any(fn in t for fn in FONCTIONS):
        return None
    # Une demission ministerielle seule ne vide pas le siege.
    if not quitte_le_siege and ("a titre de ministre" in t or "comme ministre" in t):
        return None

    return district_id(m.group(1).strip())


RE_RENOMMAGE = re.compile(r"([A-ZÉÈÀ][\w\-–—’' ]+?)\s+est remplac[ée]+e? par\s+([A-ZÉÈÀ][\w\-–—’' ]+)")


def extract_renommages(texte):
    """« Laporte est remplacee par Pierre-Laporte » -> [(ancien, nouveau)]."""
    if "circonscription" not in _norm(texte):
        return []
    return [(a.strip(), b.strip()) for a, b in RE_RENOMMAGE.findall(texte)]


def charger_deputes_courants(extdata):
    """Releve du jour de l'ANQ : {seat_id: {...}}. Vide si le fichier est absent.

    Ce releve sert a DEUX choses, et pas a une troisieme : il comble les sieges
    que le referentiel ignore (il s'arrete au 2025-03-17 et ne connait que 121
    des 125 sieges de la 43e), et il sert d'ancrage de reconciliation pour la
    date du jour. Il ne DATE rien : c'est un instantane, pas un historique.
    """
    chemin = os.path.join(extdata, "deputes_courants_qc.csv")
    if not os.path.exists(chemin):
        return {}
    with open(chemin, encoding="utf-8") as f:
        return {r["seat_id"]: r for r in csv.DictReader(f) if r["seat_id"]}


# ── Construction ─────────────────────────────────────────────────────────────

def build_persons(extdata):
    idx = json.load(open(os.path.join(extdata, "assnat_ids_qc.json"), encoding="utf-8"))
    par_nom = {e["full_name"]: e for e in idx}
    autres = {}
    with open(os.path.join(extdata, "members_historic_qc.csv"), encoding="utf-8") as f:
        for r in csv.DictReader(f):
            autres.setdefault(r["full_name"], r.get("other_names", ""))
    lignes, synth = [], 900000
    for nom in sorted(autres):
        e = par_nom.get(nom)
        if e:
            pid, url = e["assnat_id"], e.get("assnat_url", "")
        else:
            # 21 personnes n'ont pas d'identifiant ANQ (accents, homonymes en
            # « nom2 »). Un identifiant synthetique >= 900000 les rend citables
            # sans pretendre qu'ils viennent de l'ANQ.
            synth += 1
            pid, url = synth, ""
        lignes.append({"person_id": pid, "full_name": nom,
                       "other_names": autres[nom], "assnat_url": url})
    return lignes


def build_seats(extdata, renommages):
    districts = set()
    with open(os.path.join(extdata, "members_historic_qc.csv"), encoding="utf-8") as f:
        for r in csv.DictReader(f):
            if r["district_id"]:
                districts.add(r["district_id"])
    lignes = []
    ren = {district_id(a): (b, d) for d, (a, b) in renommages}
    for sid in sorted(districts):
        if sid in ren:
            nouveau, quand = ren[sid]
            lignes.append({"seat_id": sid, "name": sid, "date_start": "1973-01-01",
                           "date_end": (quand - timedelta(days=1)).isoformat()})
            lignes.append({"seat_id": sid, "name": nouveau,
                           "date_start": quand.isoformat(), "date_end": ""})
        else:
            lignes.append({"seat_id": sid, "name": sid,
                           "date_start": "1973-01-01", "date_end": ""})
    return lignes


def build_mandates(extdata, evenements, legislatures, persons):
    """Ouvre un mandat par election generale, puis applique les evenements dates."""
    pid_par_nom = {p["full_name"]: p["person_id"] for p in persons}
    bornes = {l["legislature"]: (date.fromisoformat(l["start_date"]),
                                 date.fromisoformat(l["end_date"])) for l in legislatures}
    mandats = []
    # Un couple (siege x legislature) ne peut avoir QU'UN vainqueur de generale.
    # Or deux d'entre eux (Roberval 42, Jean-Talon 43) portent DEUX lignes
    # `type=election` : le referentiel etiquette parfois un gagnant de
    # partielle comme une generale. On garde celui qui porte une date de
    # sortie — c'est l'elu de la generale, qui a quitte en cours de mandat —
    # et on laisse la chronologie creer le second via son evenement de
    # partielle, correctement date.
    brutes = []
    with open(os.path.join(extdata, "members_historic_qc.csv"), encoding="utf-8") as f:
        for r in csv.DictReader(f):
            brut = (r["legislature_id"] or "").strip()
            r["_leg"] = int(brut) if brut.isdigit() else None
            if r["_leg"] in bornes and r["type"] == "election":
                brutes.append(r)
    par_cle = {}
    for r in brutes:
        par_cle.setdefault((r["district_id"], r["_leg"]), []).append(r)
    retenues = []
    for cle, rs in par_cle.items():
        if len(rs) == 1:
            retenues.append(rs[0])
        else:
            avec_sortie = [x for x in rs if (x.get("exit_year") or "NA") not in ("", "NA")]
            retenues.append(avec_sortie[0] if avec_sortie else rs[0])

    for r in retenues:
            leg = r["_leg"]
            deb, fin = bornes[leg]
            mandats.append({
                "person_id": pid_par_nom.get(r["full_name"], ""),
                "seat_id": r["district_id"],
                "party_id": (r["party_id"] or "").upper(),
                "date_start": deb, "date_end": fin,
                "start_reason": "election", "end_reason": "dissolution",
                "source": "members_historic_qc", "confidence": "verified",
            })

    def ouvert(seat, quand):
        for m in mandats:
            if m["seat_id"] == seat and m["date_start"] <= quand <= m["date_end"]:
                return m
        return None

    # ── Combler les sieges que le referentiel ignore ────────────────────────
    # Un depute absent du referentiel n'est pas mal attribue : il est
    # `unmatched`, donc JETE par le raffineur. Sa parole disparait. On ouvre
    # donc un mandat depuis le releve de l'ANQ, en le datant du mieux possible :
    # a la date de sa partielle si la chronologie en connait une pour ce siege,
    # sinon au debut de la legislature.
    courants = charger_deputes_courants(extdata)
    if courants:
        leg_courante = max(bornes)
        deb_leg, fin_leg = bornes[leg_courante]
        deja = {m["seat_id"] for m in mandats
                if m["date_start"] <= fin_leg and deb_leg <= m["date_end"]}
        partielles = {e["seat_id"]: e["date"] for e in evenements
                      if e["type"] == "byelection" and deb_leg <= e["date"] <= fin_leg}
        for seat, r in sorted(courants.items()):
            if seat in deja:
                continue
            debut = partielles.get(seat, deb_leg)
            mandats.append({
                "person_id": r.get("person_id", ""), "seat_id": seat,
                "party_id": r.get("party_id", ""), "date_start": debut,
                "date_end": fin_leg,
                "start_reason": "byelection" if seat in partielles else "election",
                "end_reason": "dissolution", "source": "assnat_index",
                "confidence": "single_source",
            })

    for ev in sorted(evenements, key=lambda e: e["date"]):
        d, seat = ev["date"], ev["seat_id"]
        m = ouvert(seat, d)
        if ev["type"] == "defection":
            if not m:
                continue
            fin_orig = m["date_end"]
            m["date_end"], m["end_reason"] = d - timedelta(days=1), "defection"
            mandats.append({**m, "party_id": ev["party_after"], "date_start": d,
                            "date_end": fin_orig, "start_reason": "defection",
                            "end_reason": "dissolution", "source": ev["source"],
                            "confidence": "verified"})
        elif ev["type"] == "byelection":
            if m:
                m["date_end"], m["end_reason"] = d - timedelta(days=1), "resignation"
            _, fin = bornes.get(legislature_for(d, legislatures), (d, FIN_LOINTAINE))
            mandats.append({"person_id": "", "seat_id": seat,
                            "party_id": ev["party_after"], "date_start": d,
                            "date_end": fin, "start_reason": "byelection",
                            "end_reason": "dissolution", "source": ev["source"],
                            "confidence": "single_source"})
        elif ev["type"] == "resignation":
            if m:
                m["date_end"], m["end_reason"] = d, "resignation"
                m["confidence"] = "disputed"   # cabinet ou Assemblee : a trancher
    return mandats


# ── Invariant 3 : reconciliation avec les compositions publiees par l'ANQ ────
#
# C'est l'invariant qui donne un sens a « exhaustif ». Les autres verifient la
# coherence INTERNE de nos tables ; celui-ci les confronte a ce que l'Assemblee
# declare elle-meme. Une table interne parfaitement coherente peut etre
# entierement fausse — pas une table qui reproduit chaque releve publie.

# Les releves abregent : « Parti liberal, 64 » sans « du Quebec ». Ces formes
# courtes sont testees APRES les longues (cf. PARTIS), jamais avant.
PARTIS_COURTS = [("parti liberal", "PLQ"), ("coalition avenir", "CAQ"),
                 ("action democratique", "ADQ"), ("parti conservateur", "PCQ")]


def _party_code2(txt):
    """Comme _party_code, mais accepte aussi les formes abregees."""
    c = _party_code(txt)
    if c:
        return c
    for libelle, code in PARTIS_COURTS:
        if libelle in txt:
            return code
    return None


# La liste NOMINATIVE des independants est le controle le plus fort que l'ANQ
# publie : elle ne donne pas un compte, elle donne les SIEGES.
RE_INDEP_LISTE = re.compile(
    r"deputee?s?\s+(?:et\s+deputes\s+)?independant[e]?s?\s*\(([^)]{10,400})\)", re.U)


def extract_independants_nommes(texte):
    """« 10 deputes independants (Abitibi-Est, Chomedey, ...) » -> {seat_id}."""
    m = RE_INDEP_LISTE.search(_norm(texte))
    if not m:
        return set()
    bruts = re.split(r",| et ", m.group(1))
    return {district_id(b) for b in bruts if district_id(b) and len(district_id(b)) > 2}


NOMBRES_MOTS = {"un": 1, "une": 1, "deux": 2, "trois": 3, "quatre": 4, "cinq": 5,
                "six": 6, "sept": 7, "huit": 8, "neuf": 9, "dix": 10, "onze": 11,
                "douze": 12}

RE_PARTI_COMPTE = re.compile(
    r"([A-Za-zÀ-ÿ' ]{4,40}?)\s*,\s*(\d{1,3}|" + "|".join(NOMBRES_MOTS) + r")\b", re.U)
RE_INDEP_COMPTE = re.compile(r"(\d{1,3}|" + "|".join(NOMBRES_MOTS) +
                             r")\s+deputee?s?\s+(?:et\s+deputes\s+)?independant", re.U)


def _nombre(x):
    return int(x) if x.isdigit() else NOMBRES_MOTS.get(x)


def extract_composition(texte):
    """Rend {code_parti: nombre} pour un instantane COMPLET, sinon {}.

    On n'accepte que les releves qui enumerent plusieurs partis : les phrases
    partielles (« le PQ forme le troisieme groupe avec 7 sieges ») ne
    permettent pas de verifier un total et donneraient de faux ecarts.
    """
    t = _norm(texte)
    if not ("composee" in t or "composition de la chambre" in t
            or "ainsi composee" in t):
        return {}
    out = {}
    for libelle, nombre in RE_PARTI_COMPTE.findall(t):
        code = _party_code2(libelle.strip())
        n = _nombre(nombre)
        if code and n is not None:
            out[code] = n
    m = RE_INDEP_COMPTE.search(t)
    if m:
        n = _nombre(m.group(1))
        if n is not None:
            out["IND"] = n
    return out if len(out) >= 3 else {}


def composition_derivee(mandats, quand):
    """Ce que NOS tables disent de la composition a cette date."""
    out = {}
    for m in mandats:
        if m["date_start"] <= quand <= m["date_end"]:
            p = m["party_id"] or "?"
            out[p] = out.get(p, 0) + 1
    return out


def reconcilier(mandats, releves):
    """Compare chaque releve publie a notre etat derive. Rend la liste des ecarts."""
    ecarts = []
    for quand, publiee, source in releves:
        derivee = composition_derivee(mandats, quand)
        for parti, n in sorted(publiee.items()):
            obtenu = derivee.get(parti, 0)
            if obtenu != n:
                ecarts.append((quand, source, parti, n, obtenu))
    return ecarts


# ── Invariants (spec § 5) ────────────────────────────────────────────────────

def invariants(mandats):
    pbs = []
    par_siege = {}
    for m in mandats:
        par_siege.setdefault(m["seat_id"], []).append(m)
    for seat, ms in par_siege.items():
        ms = sorted(ms, key=lambda x: x["date_start"])
        for i in range(len(ms) - 1):
            for j in range(i + 1, len(ms)):
                if ms[i]["date_start"] <= ms[j]["date_end"] and ms[j]["date_start"] <= ms[i]["date_end"]:
                    pbs.append(f"CHEVAUCHEMENT {seat} : "
                               f"{ms[i]['date_start']}..{ms[i]['date_end']} et "
                               f"{ms[j]['date_start']}..{ms[j]['date_end']}")
    for m in mandats:
        if not m["source"]:
            pbs.append(f"SANS SOURCE : {m['seat_id']} {m['date_start']}")
    return pbs


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--cache", default=None)
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    ici = os.path.dirname(os.path.abspath(__file__))
    extdata = os.path.normpath(os.path.join(ici, "..", "extdata"))
    legislatures = json.load(open(os.path.join(extdata, "legislatures_qc.json"), encoding="utf-8"))

    evenements, renommages = [], []
    releves, releves_indep = [], []
    print(f"Chronologie ANQ : chrono{FIRST_PAGE} a chrono{LAST_PAGE}")
    for n in range(FIRST_PAGE, LAST_PAGE + 1):
        html = fetch_page(n, args.cache)
        if not html:
            continue
        src = f"chrono{n}"
        for d, texte in parse_entries(html):
            for anc, nouv in extract_renommages(texte):
                renommages.append((d, (anc, nouv)))
            t = _norm(texte)
            if any(i in t for i in INDICES):
                for dist, avant, apres in classify(texte):
                    evenements.append({"type": "defection", "date": d,
                                       "seat_id": district_id(dist),
                                       "party_after": apres, "source": src})
            for seat_id, parti in extract_partielle(texte):
                evenements.append({"type": "byelection", "date": d,
                                   "seat_id": seat_id, "party_after": parti,
                                   "source": src})
            comp = extract_composition(texte)
            if comp:
                releves.append((d, comp, src))
            indep = extract_independants_nommes(texte)
            if indep:
                releves_indep.append((d, indep, src))
            dem = extract_demission(texte)
            if dem:
                evenements.append({"type": "resignation", "date": d, "seat_id": dem,
                                   "party_after": None, "source": src})

    par_type = {}
    for e in evenements:
        par_type[e["type"]] = par_type.get(e["type"], 0) + 1
    print(f"\nEvenements extraits : {par_type}")
    print(f"Renommages de circonscription : {len(renommages)}")

    persons = build_persons(extdata)
    seats = build_seats(extdata, renommages)
    mandats = build_mandates(extdata, evenements, legislatures, persons)
    print(f"\npersons  : {len(persons)}")
    print(f"seats    : {len(seats)}")
    print(f"mandates : {len(mandats)}")
    conf = {}
    for m in mandats:
        conf[m["confidence"]] = conf.get(m["confidence"], 0) + 1
    print(f"  par confiance : {conf}")

    pbs = invariants(mandats)
    print(f"\nINVARIANTS : {len(pbs)} violation(s)")
    for p in pbs[:12]:
        print(f"  {p}")
    if len(pbs) > 12:
        print(f"  ... et {len(pbs)-12} autre(s)")

    print(f"\nINVARIANT 3 — reconciliation avec les releves publies par l'ANQ")
    print(f"  releves de comptes : {len(releves)} | listes nominatives : {len(releves_indep)}")
    ecarts = reconcilier(mandats, releves)
    if not ecarts:
        print("  comptes : aucun ecart")
    for quand, source, parti, publie, obtenu in ecarts:
        print(f"  ECART [{quand}] ({source}) {parti} : ANQ dit {publie}, nous {obtenu}")
    for quand, publiee, source in releves_indep:
        nous = {m["seat_id"] for m in mandats
                if m["party_id"] == "IND" and m["date_start"] <= quand <= m["date_end"]}
        manque, trop = sorted(publiee - nous), sorted(nous - publiee)
        etat = "CONFORME" if not manque and not trop else "ECART"
        print(f"  [{quand}] independants — ANQ {len(publiee)}, nous {len(nous)} -> {etat}")
        if manque: print(f"      absents de nos tables : {manque}")
        if trop:   print(f"      en trop chez nous     : {trop}")

    courants = charger_deputes_courants(extdata)
    if courants:
        from datetime import date as _d
        auj = _d.today()
        derive = {m["seat_id"]: m["party_id"] for m in mandats
                  if m["date_start"] <= auj <= m["date_end"]}
        print(f"\nRECONCILIATION AVEC LE RELEVE DU JOUR ({auj}, {len(courants)} sieges)")
        absents = sorted(set(courants) - set(derive))
        divergents = sorted((s, courants[s]["party_id"], derive[s])
                            for s in set(courants) & set(derive)
                            if courants[s]["party_id"] != derive[s])
        print(f"  sieges couverts par nos tables : {len(set(derive) & set(courants))}/{len(courants)}")
        if absents:
            print(f"  ABSENTS de nos tables ({len(absents)}) : {absents[:8]}")
        print(f"  divergences de parti : {len(divergents)}")
        for seat, anq, nous in divergents[:10]:
            print(f"    {seat:22} ANQ dit {anq:4} nous {nous}")

    if args.dry_run:
        print("\n--dry-run : rien n'a ete ecrit.")
        return
    for nom, lignes, champs in (
        ("persons_qc.csv", persons, ["person_id", "full_name", "other_names", "assnat_url"]),
        ("seats_qc.csv", seats, ["seat_id", "name", "date_start", "date_end"]),
        ("mandates_qc.csv", mandats, ["person_id", "seat_id", "party_id", "date_start",
                                      "date_end", "start_reason", "end_reason",
                                      "source", "confidence"]),
    ):
        chemin = os.path.join(extdata, nom)
        with open(chemin, "w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=champs, extrasaction="ignore")
            w.writeheader()
            for l in lignes:
                w.writerow({k: (v.isoformat() if isinstance(v, date) else v)
                            for k, v in l.items()})
        print(f"Ecrit : {chemin}")


if __name__ == "__main__":
    main()
