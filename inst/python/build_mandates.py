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
    # Non gourmand, et borne au premier mot en MINUSCULE. La classe contient
    # l'espace, donc sans borne le motif avalait la suite de la phrase d'un
    # seul tenant : « ... elu dans Bourget ET NICOLE LEGER DANS
    # Pointe-aux-Trembles » rendait la circonscription inexistante
    # « bourgetetnicolelegerdanspointeauxt », de meme que « Beauce-Sud LORS DES
    # elections partielles » ou « Levis A L'ISSUE D'UNE election partielle ».
    #
    # Enumerer les charnieres (et / ou / dans / lors / a l'issue / au cours...)
    # laisse toujours passer la suivante. On s'appuie plutot sur une propriete
    # des noms eux-memes : une circonscription quebecoise est faite de mots
    # capitalises lies par des traits d'union ou des tirets cadratins, jamais
    # par des espaces. Un mot en minuscule marque donc la fin du nom, quelle
    # que soit la tournure — et le motif repart et trouve les circonscriptions
    # SUIVANTES de la phrase au lieu d'une seule, fausse.
    r"([A-Z][\wÀ-ÿ\-–—’'\. ]{2,40}?)(?=\s+[a-zà-ÿ]|[,.;)]|$)", re.U)

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
    # Le « demission » qui suit est en REGARD (lookahead) et non consomme :
    # sinon le premier siege avale la phrase entiere et le second devient
    # introuvable — « la deputee de Kamouraska-Temiscouata [...] et le depute de
    # Bourassa [...] demissionnent » ne rendait que Kamouraska.
    re.compile(r"deputee?\s+(?:liberal[e]?\s+|independant[e]?\s+)?(?:d[eu]s?\s+|d[’'])"
               r"([^,.;]+)(?=[^.]{0,90}demission)", re.I),
]

# Le texte est normalise, donc en MINUSCULES : la borne « premier mot en
# minuscule » qui protege les circonscriptions des partielles est inutilisable
# ici. On coupe donc aux charnieres qui enchainent sur une FONCTION, seule
# forme observee : « depute de Riviere-du-Loup ET DE CHEF de l'Action
# democratique » forgeait « riviereduloupetdechefdelactiondemocratique » — un
# siege inexistant, donc une demission jamais enregistree. En silence, une fois
# de plus : une resignation ne fait que MODIFIER un mandat existant, et un
# siege introuvable n'est simplement pas modifie.
# Enumerer les formes (« et de chef », « et ministre », « et de la
# presidence »...) laisse toujours passer la suivante — la lecon des
# partielles. Aucune circonscription quebecoise ne contient « et » ou « ou », et
# la capture s'arrete deja aux virgules : couper a la conjonction, quelle que
# soit la suite, est donc a la fois plus simple et plus sur.
RE_CHARNIERE_FONCTION = re.compile(
    r"\s+(?:et|ou)\s+|\s+a\s+titre\s+de\s+|\s+en\s+tant\s+que\s+")


# (A) Une demission RECLAMEE n'est pas une demission. La chronologie rapporte
# aussi les petitions et les mises en demeure, dans les memes mots — et les
# lire comme des departs ferme des mandats qui n'ont jamais pris fin. Le cas
# le plus couteux : « Les signataires demandent la demission du depute de
# Sherbrooke, Jean Charest » (2011-02-16). Charest a siege jusqu'en septembre
# 2012 ; on effacait 19 mois de parole d'un premier ministre en exercice. A
# Anjou, la phrase dit meme que le president REFUSE la petition.
RE_DEMISSION_RECLAMEE = re.compile(
    r"(demand|exig|reclam|petition|souhait|reclamation)\w*[^.]{0,60}?demission")


def extract_demission(texte):
    """Rend la LISTE des sieges quittes — l'Assemblee, pas le caucus ni le seul
    cabinet.

    (C) Une seule phrase peut annoncer PLUSIEURS departs : « La deputee de
    Kamouraska-Temiscouata, France Dionne, ET LE DEPUTE DE BOURASSA, Yvon
    Charbonneau, demissionnent comme membres de l'Assemblee nationale ». Ne
    rendre qu'un siege en perdait un, sans rien signaler. D'ou une liste, comme
    `extract_partielle` — un paragraphe est un evenement de plus d'une personne
    bien plus souvent qu'on ne le suppose.
    """
    t = _norm(texte)
    if "demission" not in t:
        return []
    if "caucus" in t:            # c'est une defection, traitee ailleurs
        return []
    if RE_DEMISSION_RECLAMEE.search(t):
        return []

    # On demissionne aussi d'une FONCTION sans quitter l'Assemblee. Ce qui
    # tranche, c'est ce dont on demissionne — donc le texte QUI SUIT le verbe,
    # pas la facon dont la personne est presentee avant lui.
    #
    # La regle precedente (« le siege est nomme quelque part ») se declenchait
    # sur la simple presentation : « Andre Boisclair, DEPUTE DE
    # Pointe-aux-Trembles, quitte la direction du PQ et demissionne A TITRE DE
    # CHEF » fermait son mandat six mois trop tot — alors qu'il a siege
    # jusqu'au 15 novembre 2007. Un mandat clos trop tot fait disparaitre de la
    # parole du corpus, en silence.
    i = t.find("demission")
    portee = t[i:] if i != -1 else t
    ROLES = ("a titre de chef", "a titre de ministre", "comme ministre",
             "de son poste", "de ses fonctions", "de son role",
             "de la presidence", "vice-president", "vice president",
             "a titre de cheffe", "de la fonction", "a titre de president")
    quitte_le_siege = bool(re.search(r"deputee?s?\b|membres?\s+de\s+l[’']assemblee", portee))
    if not quitte_le_siege and any(r in portee for r in ROLES):
        return []

    out = []
    for rx in RE_DEMISSION_SIEGE:
        for m in rx.finditer(t):
            brut = RE_CHARNIERE_FONCTION.split(m.group(1).strip())[0]
            did = district_id(brut)
            if did and len(did) >= 3 and did not in out:
                out.append(did)
        if out:
            break
    return out


# (B) La chronologie date l'ANNONCE ; la phrase, elle, porte parfois la date
# d'EFFET — « annonce sa demission comme depute. Celle-ci sera effective le 15
# avril » (annonce le 14 mars). Fermer au jour de l'annonce retire trois a cinq
# semaines de parole a quelqu'un qui siege encore.
RE_DATE_EFFET = re.compile(
    r"(?:effectives?|en vigueur|a compter d[ue])[^.]{0,30}?\b(\d{1,2})(?:er)?\s+"
    r"(janvier|fevrier|mars|avril|mai|juin|juillet|aout|septembre|octobre|"
    r"novembre|decembre)(?:\s+(\d{4}))?")
MOIS_NUM = {m: i for i, m in enumerate(
    ["janvier", "fevrier", "mars", "avril", "mai", "juin", "juillet", "aout",
     "septembre", "octobre", "novembre", "decembre"], start=1)}


def date_effet(texte, date_annonce):
    """Rend la date d'effet annoncee dans la phrase, ou `date_annonce`."""
    m = RE_DATE_EFFET.search(_norm(texte))
    if not m:
        return date_annonce
    mois = MOIS_NUM[m.group(2)]
    # Sans annee explicite, c'est celle de l'annonce — sauf si le mois est
    # DEJA passe, auquel cas l'effet tombe l'annee suivante (annonce en
    # decembre, effet en janvier).
    annee = int(m.group(3)) if m.group(3) else date_annonce.year + (
        1 if mois < date_annonce.month else 0)
    try:
        d = date(annee, mois, int(m.group(1)))
    except ValueError:
        return date_annonce
    # Une date d'effet ANTERIEURE a l'annonce est une lecture ratee, pas un
    # fait : on garde l'annonce plutot que d'inventer un recul.
    return d if d >= date_annonce else date_annonce


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
                "parliamentary_status": "group",
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

    # ── Fusion ADQ -> CAQ (2012-02-14) ──────────────────────────────────────
    # Le DGE confirme la fusion : « Le nouveau parti, la Coalition avenir
    # Quebec, succede aux droits et obligations des partis fusionnes ». Ce
    # n'est pas une defection individuelle mais une succession de personne
    # morale : les elus adequistes deviennent caquistes sans avoir rien fait.
    #
    # Le meme jour, le president Chagnon tranche leur statut : « Ils siegeront
    # comme independants. CEPENDANT, ils figureront comme deputes independants
    # REPRESENTANT LA CAQ dans le Journal des debats [...] ». D'ou les deux
    # attributs : party_id = CAQ (affiliation, ce que le Journal affiche) et
    # parliamentary_status = independent (statut de siege). L'ANQ compte
    # d'ailleurs « Coalition avenir Quebec, 9 » dans sa propre composition a la
    # dissolution — elle ne les compte PAS comme independants.
    FUSION_ADQ_CAQ = date(2012, 2, 14)
    for m in list(mandats):
        if m["party_id"] != "ADQ":
            continue
        if not (m["date_start"] <= FUSION_ADQ_CAQ <= m["date_end"]):
            continue
        fin_orig = m["date_end"]
        m["date_end"], m["end_reason"] = FUSION_ADQ_CAQ - timedelta(days=1), "merger"
        mandats.append({**m, "party_id": "CAQ",
                        "parliamentary_status": "independent",
                        "date_start": FUSION_ADQ_CAQ, "date_end": fin_orig,
                        "start_reason": "merger", "end_reason": "dissolution",
                        "source": "chrono102:fusion+decision-chagnon",
                        "confidence": "verified"})

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
                            # La chronologie de l'ANQ est faisante foi ; le repli
                            # Wikipedia, non. L'evenement porte donc sa propre
                            # confiance plutot que de l'heriter de son type.
                            "confidence": ev.get("confidence", "verified")})
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


# ── Repli Wikipedia pour l'annee courante ────────────────────────────────────

def evenements_de_repli(html_derniere_page, annees_chrono, extdata, legislatures):
    """Bouche le trou de l'annee en cours, et RIEN d'autre.

    La Chronologie parlementaire est compilee retrospectivement : la page de
    l'annee courante existe mais reste un gabarit vide. Sans repli, une
    defection de janvier n'entre dans nos tables qu'un an plus tard — et d'ici
    la, pplmatch attribue le mauvais parti a un depute en toute confiance.

    Ce repli ne se declenche QUE si la page de l'ANQ est encore vide : des
    qu'elle publie, elle reprend la main sans qu'on ait a toucher au code. Ses
    evenements sortent en `confidence=single_source` et sont marques
    `wikipedia:` dans `source`, pour rester repérables et re-generables.
    Wikipedia est un candidat, jamais une verite (spec § 6).
    """
    from wikipedia_fallback import (chronologie_vide, evenements_wikipedia,
                                    resoudre_siege, classer_transition)
    if not chronologie_vide(html_derniere_page):
        return []

    # NE PAS deriver l'annee du numero de page. La tentation est forte
    # (chrono86 = 1994, donc chronoN = N + 1908), mais elle est fausse des que
    # la numerotation de l'ANQ saute un cran — et elle a effectivement produit
    # 2024 pour chrono116. Consequence : le repli rejouait deux annees DEJA
    # publiees par la chronologie, et chaque evenement comptait double
    # (Arthabaska se retrouvait avec trois mandats ouverts le meme jour).
    #
    # On se fie donc a ce que la chronologie a REELLEMENT rendu : toute annee
    # dont elle a date au moins une entree est a elle, et Wikipedia n'y touche
    # pas. Le repli ne couvre que ce qui manque, quelle que soit la
    # numerotation.
    leg = legislature_for(date.today(), legislatures)
    if not leg:
        return []

    # On resout le siege par PERSONNE, contre le releve du jour de l'ANQ : une
    # phrase Wikipedia peut nommer la circonscription qu'une deputee BRIGUE
    # plutot que celle qu'elle occupe (cf. wikipedia_fallback, le cas Rimouski
    # / La Peltrie). Sans ce releve, on n'infere rien.
    deputes = charger_deputes_courants(extdata)
    if not deputes:
        print(f"\nRepli Wikipedia : IMPOSSIBLE — deputes_courants_qc.csv absent, "
              f"aucun siege ne peut etre resolu par personne.")
        return []

    print(f"\nRepli Wikipedia : chrono{LAST_PAGE} est un gabarit vide, "
          f"lecture de « {leg}e legislature du Quebec »")
    print(f"  annees deja couvertes par la chronologie : "
          f"{min(annees_chrono, default='-')}..{max(annees_chrono, default='-')}")
    try:
        puces = evenements_wikipedia(leg)
    except Exception as e:                                  # noqa: BLE001
        print(f"  ECHEC de la lecture ({e}) — on continue sans le repli.")
        return []

    src = f"wikipedia:{leg}e-legislature"
    out, non_resolus = [], []
    puces = [(d, t) for d, t in puces if d.year not in annees_chrono]
    for d, texte in puces:
        classe = classer_transition(texte)
        if not classe:
            continue
        typ, _, apres = classe
        seat = resoudre_siege(texte, deputes)
        if not seat:
            non_resolus.append((d, texte[:70]))
            continue
        # « rejoint le PCQ » suit la meme mecanique qu'une defection : le
        # mandat courant se ferme, un autre s'ouvre sous le nouveau parti.
        out.append({"type": "byelection" if typ == "byelection" else "defection",
                    "date": d, "seat_id": seat,
                    "party_after": "IND" if typ == "defection" else apres,
                    "source": src, "confidence": "single_source"})

    print(f"  {len(puces)} puce(s) sur des annees non couvertes, "
          f"{len(out)} evenement(s) retenu(s)")
    for e in out:
        print(f"    [{e['date']}] {e['type']:10} {e['seat_id']:20} -> {e['party_after']}")
    for d, t in non_resolus:
        print(f"    NON RESOLU [{d}] {t}...")
    return out


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
    # Le repli est actif PAR DEFAUT : sans lui, l'annee en cours est un trou
    # silencieux. L'option sert aux regenerations reproductibles, ou Wikipedia
    # — qui bouge au fil de l'eau — introduirait une source non figee.
    ap.add_argument("--sans-wikipedia", action="store_true",
                    help="n'utilise pas le repli Wikipedia pour l'annee courante")
    args = ap.parse_args()

    ici = os.path.dirname(os.path.abspath(__file__))
    extdata = os.path.normpath(os.path.join(ici, "..", "extdata"))
    legislatures = json.load(open(os.path.join(extdata, "legislatures_qc.json"), encoding="utf-8"))

    evenements, renommages = [], []
    releves, releves_indep = [], []
    print(f"Chronologie ANQ : chrono{FIRST_PAGE} a chrono{LAST_PAGE}")
    html_derniere, annees_chrono = "", set()
    for n in range(FIRST_PAGE, LAST_PAGE + 1):
        html = fetch_page(n, args.cache)
        if n == LAST_PAGE:
            html_derniere = html or ""
        if not html:
            continue
        src = f"chrono{n}"
        for d, texte in parse_entries(html):
            annees_chrono.add(d.year)
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
            for dem in extract_demission(texte):
                evenements.append({"type": "resignation",
                                   "date": date_effet(texte, d),
                                   "seat_id": dem,
                                   "party_after": None, "source": src})

    if not args.sans_wikipedia:
        evenements += evenements_de_repli(html_derniere, annees_chrono,
                                          extdata, legislatures)

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
        ("mandates_qc.csv", mandats, ["person_id", "seat_id", "party_id",
                                      "parliamentary_status", "date_start",
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
