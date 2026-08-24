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
PLAFOND_SIEGES = 125          # l'Assemblee nationale en compte 125 depuis 1989


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

    # Une phrase peut annoncer une LISTE de circonscriptions dont une seule
    # suit « dans » : « elus respectivement dans Beauce-Sud, Fabre et
    # Saint-Henri-Sainte-Anne ». Les suivantes sont separees par des virgules,
    # donc invisibles au motif — deux partielles sur trois etaient perdues, et
    # le siege de Saint-Henri-Sainte-Anne restait vide trois ans.
    trouves = []
    for m in RE_CIRCO_PARTIELLE.finditer(texte):
        trouves.append((m.start(), m.group(1)))
        reste = texte[m.end():]
        # Chaque element doit etre SUIVI d'une virgule, d'un « et », ou de la
        # fin de phrase. Sans cette borne, l'enumeration avalait les PRENOMS :
        # « elu dans Bourget, Sylvain Simard dans Vimont » rendait la
        # circonscription « sylvain ». Un nom de personne est suivi de son
        # patronyme, donc d'un mot capitalise — jamais d'une charniere de liste.
        element = r"[A-ZÀ-Þ][\wÀ-ÿ\-–—’'\.]*(?=\s*,|\s+et\s|\s*[.;]|\s*$)"
        suite = re.match(r"((?:\s*,\s*" + element + r")+"
                         r"(?:\s+et\s+" + element + r")?)", reste)
        if not suite:
            continue
        pos = m.end()
        for element in re.split(r"\s*,\s*|\s+et\s+", suite.group(1)):
            element = element.strip()
            if element:
                trouves.append((pos, element))

    out, vus = [], set()
    for depart, capture in trouves:
        brut = capture.strip()
        brut = re.sub(r"\s+(et|ou|a l|de la|du)\s*$", "", brut, flags=re.I).strip()
        did = district_id(brut)
        if not did or len(did) < 3 or did in vus:
            continue
        pos = depart
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
    # Le siege doit etre l'OBJET de la demission, pas l'appositif qui presente
    # la personne. « Demission DU PRESIDENT DE L'ASSEMBLEE NATIONALE Yvon
    # Vallieres, depute de Richmond » : il quitte la presidence et garde son
    # siege — dix-sept mois de plus. La simple presence du mot « depute » apres
    # le verbe suffisait a fermer le mandat.
    #
    # On demande donc que « depute » soit introduit comme objet (« du depute »,
    # « a titre de depute », « comme depute ») des lors qu'une FONCTION est
    # nommee. Sans fonction concurrente, l'appositif suffit : « Jean-Pierre
    # Belisle, depute liberal de Mille-Iles, annonce sa demission en Chambre »
    # ne nomme rien d'autre, et reste un depart de l'Assemblee.
    objet_siege = bool(re.search(
        r"(?:d[eu]s?|a titre de|comme|en tant que)\s+(?:liberal[e]?s?\s+|independant[e]?s?\s+)?"
        r"deputee?s?\b", portee))
    mention_siege = bool(re.search(r"deputee?s?\b|membres?\s+de\s+l[’']assemblee", portee))
    role_nomme = any(r in portee for r in ROLES) or re.search(
        r"demission\s+d[eu]s?\s+(?:president|presidente|ministre|chef|cheffe|leader|"
        r"vice-president|whip)", portee)
    if role_nomme and not objet_siege:
        return []
    if not mention_siege and role_nomme:
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


# « demissionne le 08-03-2001 » dans la colonne Remarques de l'ANQ.
# L'ANQ ecrit tantot « 08-03-2001 », tantot « 29-01-96 ». Exiger quatre
# chiffres ecartait les deux seules remarques a annee courte — celles de
# Jacques Parizeau et de Liza Frulla, precisement deux des cas qu'on n'arrivait
# pas a arbitrer.
RE_DEM_DEPCIR = re.compile(r"demission\w*\s*(?:le\s+)?(\d{1,2})-(\d{1,2})-(\d{2,4})")


def demissions_depcir(extdata):
    """Rend {seat_id: [date, ...]} — les demissions DATEES par l'ANQ elle-meme.

    C'est l'arbitre des mandats classes `disputed`. La question qui les rendait
    litigieux — a-t-on quitte le SIEGE ou seulement une fonction ? — est
    exactement celle a laquelle un historique des titulaires par circonscription
    repond : s'il inscrit « demissionne le ... », le siege est devenu vacant ce
    jour-la.

    Et la ou la chronologie date l'ANNONCE, l'ANQ date la VACANCE. Lucien
    Bouchard annonce son depart le 2001-01-11 et quitte le siege le 2001-03-08 :
    ce sont deux faits distincts, et c'est le second qui borne un mandat.
    """
    out = {}
    for r in charger_depcir(extdata):
        m = RE_DEM_DEPCIR.search(_norm(r.get("remarque", "")))
        if not m:
            continue
        an = int(m.group(3))
        if an < 100:                       # « 96 » -> 1996, « 05 » -> 2005
            an += 1900 if an >= 50 else 2000
        try:
            d = date(an, int(m.group(2)), int(m.group(1)))
        except ValueError:
            continue
        out.setdefault(r["seat_id"], []).append(d)
    return out


def charger_depcir(extdata):
    """Historique par circonscription (ANQ). Liste vide si le fichier manque.

    Complete le referentiel la ou il s'arrete. Il ne le remplace pas : la page
    de l'ANQ est elle-meme figee pour 14 des 125 circonscriptions, qui en
    restent a 2018.
    """
    chemin = os.path.join(extdata, "deputes_par_circonscription_qc.csv")
    if not os.path.exists(chemin):
        return []
    with open(chemin, encoding="utf-8") as f:
        return [r for r in csv.DictReader(f) if r.get("seat_id")]


def pid_par_nom_et_siege(extdata):
    """{(clef_nom, seat_id): person_id} depuis l'historique de l'ANQ.

    Deux deputes peuvent porter le meme nom. `assnat_ids_qc.json` etant indexe
    par NOM, il ne peut pas les separer : les deux Eric Girard — Groulx et
    Lac-Saint-Jean — recevaient le meme identifiant 17957, et une seule personne
    semblait donc occuper deux sieges a la fois. L'historique par
    circonscription, lui, est indexe par SIEGE : il donne 17929 pour Groulx et
    17957 pour Lac-Saint-Jean. Le siege desambigue ce que le nom ne peut pas.
    """
    out = {}
    for r in charger_depcir(extdata):
        pid = (r.get("person_id") or "").strip()
        if pid:
            out.setdefault((cle_nom(_nom_depcir(r.get("full_name"))), r["seat_id"]), pid)
    return out


def index_noms(persons):
    """{« prenom nom » normalise: person_id} — pour lire un nom dans une phrase.

    On ne fait pas d'analyse grammaticale : on cherche dans le texte les
    personnes qu'on CONNAIT DEJA. L'appariement se valide ainsi lui-meme, et un
    nom qu'on ne connait pas ne peut pas produire de fausse identite. Mesure
    sur la chronologie : 60 phrases de demission sur 64 nomment une personne de
    la table.
    """
    idx = {}
    for p in persons:
        n = " ".join(_norm(p["full_name"]).split())
        if len(n.split()) >= 2:
            idx.setdefault(n, str(p["person_id"]))
    return idx


def personne_nommee(texte, idx):
    """Rend le person_id nomme dans la phrase, ou None si ce n'est pas net.

    On ne rend un nom que s'il n'y en a qu'UN. Deux noms, c'est un paragraphe
    a plusieurs departs, et les apparier au bon siege demanderait de deviner —
    on s'abstient plutot que de risquer d'attribuer un mandat a la mauvaise
    personne, qui est exactement l'erreur qu'on repare ici.
    """
    plat = " ".join(_norm(texte).split())
    trouves = {v for k, v in idx.items() if k in plat}
    return next(iter(trouves)) if len(trouves) == 1 else None


def meme_personne(a, b, noms):
    """Deux identifiants designent-ils la meme personne ecrite autrement ?

    « regent beaudet » et « regent l. beaudet » sont un doublon de graphie, pas
    une contradiction : les mots de l'un sont inclus dans ceux de l'autre.
    """
    if a == b:
        return True
    ta = set(_norm(noms.get(a, "")).replace(".", " ").split())
    tb = set(_norm(noms.get(b, "")).replace(".", " ").split())
    return bool(ta) and bool(tb) and (ta <= tb or tb <= ta)


def cle_nom(nom):
    """Clef d'identite d'une personne, insensible a la ponctuation.

    Le referentiel ecrit « christine stpierre », l'ANQ « ST-PIERRE, Christine ».
    Comparees telles quelles, ce sont deux personnes : `persons_qc` en portait
    deux lignes, avec deux identifiants — l'un entier, l'autre chaine — et le
    controle « cette personne a-t-elle deja un mandat ? » echouait donc DEUX
    fois. Christine St-Pierre siegeait dans L'Acadie ET dans Acadie, et le
    plafond de 125 sautait.
    """
    # Le referentiel desambigue ses homonymes par un SUFFIXE numerique
    # (« eric girard2 ») ; l'ANQ ne le fait pas. On retire donc le suffixe pour
    # que les deux graphies se rencontrent, et c'est le siege qui tranche
    # ensuite laquelle des deux personnes est visee.
    return re.sub(r"\d+$", "", re.sub(r"[^a-z0-9]", "", _norm(nom or "")))


def _nom_depcir(brut):
    """« LEFEBVRE, Eric » -> « eric lefebvre », la graphie du referentiel."""
    brut = (brut or "").strip()
    if not brut:
        return ""
    nom = " ".join(p.strip() for p in reversed(brut.split(","))) if "," in brut else brut
    return " ".join(_norm(nom).split())


# ── Construction ─────────────────────────────────────────────────────────────

# Le référentiel historique contient deux graphies fautives qui ne rencontrent
# pas les fiches ANQ stables. Sans cette réconciliation, build_persons leur
# invente un identifiant 9000xx et build_mandates propage cet identifiant dans
# toute la 43e législature. Garder la graphie historique comme clé permet de
# conserver ses alias tout en publiant le vrai nom et le vrai person_id.
PERSON_RECONCILIATIONS = {
    "karianabourassa": {"person_id": "19291", "full_name": "kariane bourassa"},
    "valeriesetlakwe": {"person_id": "19285", "full_name": "michelle setlakwe"},
}

def build_persons(extdata):
    idx = json.load(open(os.path.join(extdata, "assnat_ids_qc.json"), encoding="utf-8"))
    par_nom = {e["full_name"]: e for e in idx}
    autres = {}
    with open(os.path.join(extdata, "members_historic_qc.csv"), encoding="utf-8") as f:
        for r in csv.DictReader(f):
            autres.setdefault(r["full_name"], r.get("other_names", ""))
    lignes, synth = [], 900000
    for nom in sorted(autres):
        reconciliation = PERSON_RECONCILIATIONS.get(cle_nom(nom))
        e = par_nom.get(nom)
        if reconciliation:
            # Réserver le rang synthétique historique garde les 19 autres
            # identifiants 9000xx stables à travers une régénération.
            synth += 1
            pid, url = reconciliation["person_id"], ""
            full_name = reconciliation["full_name"]
        elif e:
            pid, url = e["assnat_id"], e.get("assnat_url", "")
            full_name = nom
        else:
            # 21 personnes n'ont pas d'identifiant ANQ (accents, homonymes en
            # « nom2 »). Un identifiant synthetique >= 900000 les rend citables
            # sans pretendre qu'ils viennent de l'ANQ.
            synth += 1
            pid, url = synth, ""
            full_name = nom
        lignes.append({"person_id": pid, "full_name": full_name,
                       "other_names": autres[nom], "assnat_url": url})

    # ── Les personnes que SEUL le releve du jour connait ────────────────────
    # `members_historic_qc.csv` s'arrete au 2025-03-17. Les elu.es arrive.es
    # depuis — vainqueurs de partielles surtout — n'existaient donc dans aucune
    # table de personnes, et un mandat sans personne est inutilisable : pplmatch
    # apparie des NOMS. Arthabaska et Joliette restaient `unmatched`, donc
    # jetes, alors meme que leur mandat etait correctement date.
    #
    # L'ANQ ecrit « Boissonneault, Alex » la ou le referentiel ecrit « alex
    # boissonneault » : on retourne le nom pour parler la meme langue que le
    # reste de la table, sinon la personne est ajoutee EN DOUBLE sous une
    # graphie que l'appariement ne retrouvera jamais.
    connus = {cle_nom(l["full_name"]) for l in lignes}
    # Dedoublonner aussi par IDENTIFIANT, pas seulement par nom : deux personnes
    # peuvent porter le meme. Ecarter le second Eric Girard parce que le nom
    # etait « deja connu » laissait l'identifiant 17929 present dans les
    # mandats et absent de la table des personnes — une clef etrangere qui ne
    # pointe nulle part, donc un depute sans nom en aval.
    ids_connus = {str(l["person_id"]) for l in lignes if l["person_id"]}
    # L'historique par circonscription nomme aussi les elu.es que le
    # referentiel a manques — dont ceux encore en poste.
    for r in charger_depcir(extdata):
        nom = _nom_depcir(r.get("full_name"))
        pid = str(r.get("person_id") or "")
        if not nom:
            continue
        if cle_nom(nom) in connus and (not pid or pid in ids_connus):
            continue
        if pid and pid in ids_connus:
            continue
        connus.add(cle_nom(nom))
        if pid:
            ids_connus.add(pid)
        lignes.append({"person_id": pid, "full_name": nom,
                       "other_names": nom.split()[-1], "assnat_url": ""})

    for r in charger_deputes_courants(extdata).values():
        brut = (r.get("full_name") or "").strip()
        if not brut:
            continue
        nom = " ".join(p.strip() for p in reversed(brut.split(","))).strip() \
            if "," in brut else brut
        nom = _norm(nom)
        nom = " ".join(nom.split())
        if not nom or cle_nom(nom) in connus:
            continue
        connus.add(cle_nom(nom))
        lignes.append({"person_id": r.get("person_id", ""), "full_name": nom,
                       "other_names": nom.split()[-1] if nom.split() else "",
                       "assnat_url": r.get("assnat_url", "")})
    return lignes


def build_seats(extdata, renommages):
    districts = set()
    with open(os.path.join(extdata, "members_historic_qc.csv"), encoding="utf-8") as f:
        for r in csv.DictReader(f):
            if r["district_id"]:
                districts.add(district_id(r["district_id"]))
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
    # Indexe par CLEF et non par graphie, et en chaine : deux types
    # differents pour le meme identifiant ne se comparent jamais egaux.
    pid_par_nom = {cle_nom(p["full_name"]): str(p["person_id"]) for p in persons}
    # Les lignes historiques gardent les deux mauvaises graphies. Elles doivent
    # pointer vers la même fiche stable que build_persons, faute de quoi les
    # mandats recréeraient les identifiants temporaires à la régénération.
    pid_par_nom.update({
        cle_nom(legacy_name): correction["person_id"]
        for legacy_name, correction in PERSON_RECONCILIATIONS.items()
    })
    pid_siege = pid_par_nom_et_siege(extdata)
    noms_par_id = {str(p["person_id"]): p["full_name"] for p in persons}
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
                "person_id": pid_siege.get((cle_nom(r["full_name"]),
                                            district_id(r["district_id"]))) \
                or pid_par_nom.get(cle_nom(r["full_name"]), ""),
                # Le referentiel porte deux identifiants mal formes, avec un
                # ESPACE : « bourassa sauve » et « la piniere ». Ils ne
                # s'apparient a rien — ni au releve de l'ANQ, ni a l'historique
                # par circonscription — et creaient donc un siege fantome en
                # doublon du vrai, invisible aux invariants puisque ceux-ci
                # comparent des identifiants et que les deux different.
                "seat_id": district_id(r["district_id"]),
                "party_id": (r["party_id"] or "").upper(),
                "date_start": deb, "date_end": fin,
                "start_reason": "election", "end_reason": "dissolution",
                "parliamentary_status": "group",
                "source": "members_historic_qc", "confidence": "verified",
            })

    # ── Combler les (siege x legislature) que le referentiel a manques ──────
    # `members_historic_qc.csv` s'arrete en cours de 43e : Eric Lefebvre y
    # figure pour la 41e et la 42e, pas pour la 43e, alors qu'il a ete reelu
    # dans Arthabaska en 2022. Sans mandat, sa defection du 2024-04-16 tombait
    # dans le vide et sa parole sortait `unmatched`, donc jetee.
    #
    # L'historique par circonscription de l'ANQ donne l'ANNEE, pas la date : on
    # borne donc le mandat a la legislature dont la generale porte cette annee,
    # et on ne touche QUE les couples absents. Les partielles sont laissees a
    # la chronologie, qui les date au jour pres.
    deja_cle = {(m["seat_id"], leg) for m in mandats for leg, (d0, d1) in bornes.items()
                if m["date_start"] <= d1 and d0 <= m["date_end"]}
    an_vers_leg = {d0.year: leg for leg, (d0, _) in bornes.items()}
    # Le referentiel et l'ANQ n'orthographient pas toujours le siege pareil —
    # « L'Acadie » contre « Acadie », « Laurier » contre « Laurier-Dorion ». La
    # cle (siege x legislature) ne suffit donc pas a voir qu'il s'agit du meme
    # mandat, et Yvan Bordeleau se retrouvait elu dans deux circonscriptions a
    # la fois. Une PERSONNE n'occupe qu'un siege par legislature : c'est la
    # cle qui resiste aux variantes de graphie.
    deja_personne = {(m["person_id"], leg) for m in mandats if m["person_id"]
                     for leg, (d0, d1) in bornes.items()
                     if m["date_start"] <= d1 and d0 <= m["date_end"]}
    comblees = 0
    for r in charger_depcir(extdata):
        if r.get("partielle") == "1":
            continue
        leg = an_vers_leg.get(int(r["annee"]) if r["annee"].isdigit() else 0)
        if leg is None or (r["seat_id"], leg) in deja_cle:
            continue
        nom = _nom_depcir(r.get("full_name"))
        pid = str(r.get("person_id", "") or "") or pid_par_nom.get(cle_nom(nom), "")
        if pid and (pid, leg) in deja_personne:
            continue
        deb, fin = bornes[leg]
        mandats.append({
            "person_id": pid,
            "seat_id": r["seat_id"], "party_id": (r.get("party_id") or "").upper(),
            "date_start": deb, "date_end": fin,
            "start_reason": "election", "end_reason": "dissolution",
            "parliamentary_status": "group",
            "source": "assnat_depcir", "confidence": "single_source",
        })
        deja_cle.add((r["seat_id"], leg))
        if pid:
            deja_personne.add((pid, leg))
        comblees += 1
    if comblees:
        print(f"Mandats combles depuis l'historique par circonscription : {comblees}")

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
            # Si la chronologie connait une partielle pour ce siege, elle
            # ouvrira elle-meme le mandat, a la bonne date et avec sa source.
            # En ouvrir un ici EN PLUS produisait deux mandats au meme jour,
            # dont l'un se faisait refermer la veille de sa propre ouverture
            # (« 2025-08-11..2025-08-10 »). Le nom du titulaire, lui, est
            # rattache plus bas depuis le releve du jour.
            if seat in partielles:
                continue
            debut = deb_leg
            mandats.append({
                "person_id": r.get("person_id", ""), "seat_id": seat,
                "party_id": r.get("party_id", ""), "date_start": debut,
                "date_end": fin_leg,
                "start_reason": "byelection" if seat in partielles else "election",
                "end_reason": "dissolution", "source": "assnat_index",
                "confidence": "single_source",
            })

    # Un meme evenement peut etre annonce DEUX fois — la chronologie le repete
    # d'une page a l'autre, et le repli Wikipedia peut le redire. Applique deux
    # fois, une partielle ouvre un mandat puis le referme le lendemain de sa
    # propre ouverture : Arthabaska portait « 2025-08-11..2025-08-10 », un
    # intervalle a duree negative que les invariants comptaient comme un
    # chevauchement sans dire pourquoi.
    hors_bornes = []
    vus, uniques = set(), []
    for ev in sorted(evenements, key=lambda e: e["date"]):
        cle = (ev["type"], ev["date"], ev["seat_id"], ev.get("party_after"))
        if cle in vus:
            continue
        vus.add(cle)
        uniques.append(ev)

    for ev in uniques:
        d, seat = ev["date"], ev["seat_id"]
        m = ouvert(seat, d)
        if ev["type"] == "defection":
            if not m:
                continue
            # Rejoindre le parti qu'on a DEJA n'est pas un changement. Le cas
            # reel : Monique Simard se retire du caucus du PQ en avril 1996 —
            # que la chronologie n'annonce que comme une INTENTION, donc non
            # retenue — puis « est acquittee » et le reintegre le 1996-09-25.
            # Seule l'arrivee etait vue, et elle scindait le mandat en deux
            # moities identiques dont la seconde heritait de l'identite du
            # titulaire PRECEDENT. Un evenement sans effet ne doit rien couper.
            if ev["party_after"] == m["party_id"]:
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
            # Hors de toute legislature connue, on n'ouvre RIEN. Le referentiel
            # commence a la 35e (1994-09-12) ; deux partielles de fevrier 1994
            # la precedent. Faute de legislature, leur mandat prenait pour fin
            # FIN_LOINTAINE et courait jusqu'en 9999 — il chevauchait donc TOUS
            # les mandats suivants de son siege, et produisait a lui seul la
            # majorite des violations d'invariant. Modeliser la 34e legislature
            # serait la vraie reponse ; inventer une borne n'en est pas une.
            leg_ev = legislature_for(d, legislatures)
            if leg_ev is None:
                hors_bornes.append((d, ev["seat_id"]))
                continue
            if m:
                m["date_end"], m["end_reason"] = d - timedelta(days=1), "resignation"
            _, fin = bornes[leg_ev]
            mandats.append({"person_id": "", "seat_id": seat,
                            "party_id": ev["party_after"], "date_start": d,
                            "date_end": fin, "start_reason": "byelection",
                            "end_reason": "dissolution", "source": ev["source"],
                            "confidence": "single_source"})
        elif ev["type"] == "resignation":
            if m:
                # La phrase de demission NOMME souvent la personne qui part. Si
                # ce n'est pas celle que porte le mandat, c'est le mandat qui a
                # tort : le referentiel inscrit parfois le gagnant d'une
                # PARTIELLE comme elu de la generale. Terrebonne portait ainsi
                # Catherine Gentilcore, elue en 2025, pour un mandat ouvert en
                # 2022 qui etait celui de Pierre Fitzgibbon — mauvaise personne
                # ET mauvais parti pour deux ans de parole.
                nomme = ev.get("person_id")
                if nomme and m["person_id"] and not meme_personne(
                        nomme, m["person_id"], noms_par_id):
                    ancien = [x for x in mandats
                              if x["seat_id"] == m["seat_id"] and x["person_id"] == nomme
                              and x["date_end"] < m["date_start"]]
                    m["person_id"] = nomme
                    if ancien:
                        # Le parti n'est pas atteste pour CETTE legislature : on
                        # reprend celui du mandat precedent de la meme personne
                        # sur le meme siege, et on le dit en `single_source`.
                        m["party_id"] = max(
                            ancien, key=lambda x: x["date_end"])["party_id"]
                    m["source"] += "+chrono-titulaire"
                    m["confidence"] = "single_source"
                    print(f"  TITULAIRE CORRIGE {m['seat_id']:18} {m['date_start']} "
                          f"-> {noms_par_id.get(nomme, nomme)}")
                m["date_end"], m["end_reason"] = d, "resignation"
                if m["confidence"] != "single_source":
                    m["confidence"] = "disputed"   # cabinet ou Assemblee : a trancher
    # La fusion s'applique EN DERNIER, une fois tous les mandats ouverts.
    # Placee avant la boucle d'evenements, elle ne voyait pas les mandats que
    # les partielles allaient creer : celui de Riviere-du-Loup (partielle du
    # 2009-06-22) courait jusqu'a la dissolution et chevauchait le mandat CAQ
    # ouvert par la fusion. C'etait la derniere violation d'invariant.
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


    # ── Arbitrage des demissions par l'ANQ ─────────────────────────────────
    # Deux sources independantes valent mieux qu'un jugement a la main : la ou
    # la chronologie et l'historique par circonscription concordent, le mandat
    # cesse d'etre `disputed` sans que personne n'ait a trancher ; la ou ils
    # divergent, l'ANQ l'emporte, parce que la question porte sur l'occupation
    # d'un siege et que c'est precisement ce dont cette page tient le registre.
    arbitrees = {"accord": 0, "corrigees": 0, "trouvees": 0}
    for seat, dates in demissions_depcir(extdata).items():
        for d in dates:
            m = ouvert(seat, d)
            if not m:
                # Notre date peut etre TROP TOT — c'est meme le cas le plus
                # frequent, puisque la chronologie date l'annonce. La date de
                # l'ANQ tombe alors dans le vide laisse entre la fin qu'on a
                # posee et la partielle suivante : Lucien Bouchard annonce le
                # 2001-01-11 et quitte le 2001-03-08. Chercher seulement « le
                # mandat qui couvre cette date » ne savait donc que RACCOURCIR
                # un mandat, jamais corriger vers l'avant.
                candidats = [x for x in mandats if x["seat_id"] == seat
                             and x["end_reason"] == "resignation"
                             and 0 < (d - x["date_end"]).days <= 400]
                if not candidats:
                    continue
                m = max(candidats, key=lambda x: x["date_end"])
            elif m["date_end"] < d:
                continue
            # Ne pas empieter sur le mandat suivant : si quelqu'un occupe deja
            # le siege apres cette date, notre modele en sait plus que la
            # remarque, et on ne touche a rien.
            # On ne marche jamais sur le mandat suivant : si quelqu'un occupe
            # deja le siege a cette date, notre modele en sait plus que la
            # remarque, et on ne touche a rien.
            if any(x["seat_id"] == seat and x is not m and x["date_start"] <= d
                   and x["date_start"] > m["date_start"] for x in mandats):
                continue
            if m["date_end"] == d and m["end_reason"] == "resignation":
                arbitrees["accord"] += 1
            elif m["end_reason"] == "resignation":
                arbitrees["corrigees"] += 1
            else:
                arbitrees["trouvees"] += 1
            m["date_end"], m["end_reason"] = d, "resignation"
            m["confidence"] = "verified"
            m["source"] = (m["source"] + "+assnat_depcir") if "depcir" not in m["source"] \
                else m["source"]
    if any(arbitrees.values()):
        print(f"Demissions arbitrees par l'ANQ : {arbitrees['accord']} confirmees, "
              f"{arbitrees['corrigees']} redatees, {arbitrees['trouvees']} trouvees")

    # ── Arbitrage par les NOTICES individuelles de l'ANQ ────────────────────
    # Troisieme source, et reellement independante : la notice est redigee par
    # la Bibliotheque a partir du dossier du membre, pas du Journal des debats.
    # Deux sources qui concordent ne prouvent rien si elles se recopient ;
    # celles-ci ne se recopient pas.
    #
    # Elle tranche aussi un cas que les deux autres manquaient : quand nous
    # ignorons la date d'un depart, le mandat se ferme la veille de la
    # partielle, donc des MOIS trop tard. Catherine Fournier quitte
    # Marie-Victorin le 2021-11-13 pour la mairie de Longueuil ; nous la
    # faisions sieger jusqu'au 2022-04-10.
    chemin_fiches = os.path.join(extdata, "demissions_fiches_qc.csv")
    if os.path.exists(chemin_fiches):
        stat = {"accord": 0, "corrigees": 0, "conflit": 0}
        with open(chemin_fiches, encoding="utf-8") as f:
            for r in csv.DictReader(f):
                try:
                    d = date.fromisoformat(r["date_demission"])
                except ValueError:
                    continue
                for m in mandats:
                    if m["person_id"] != r["person_id"] or m["seat_id"] != r["seat_id"]:
                        continue
                    if not (m["date_start"] <= d <= m["date_end"]):
                        continue
                    if m["date_end"] == d:
                        stat["accord"] += 1
                        m["confidence"] = "verified"
                    elif "depcir" in m["source"]:
                        # Deux sources de l'ANQ qui se contredisent : on ne
                        # choisit pas en silence, on le signale et on laisse la
                        # ligne en litige.
                        stat["conflit"] += 1
                        m["confidence"] = "disputed"
                        print(f"  CONFLIT {m['seat_id']:20} depcir {m['date_end']} "
                              f"vs fiche {d}")
                    else:
                        stat["corrigees"] += 1
                        m["date_end"], m["end_reason"] = d, "resignation"
                        m["confidence"] = "verified"
                    if "fiche" not in m["source"]:
                        m["source"] += "+assnat_fiche"
                    break
        if any(stat.values()):
            print(f"Demissions arbitrees par les notices : {stat['accord']} confirmees, "
                  f"{stat['corrigees']} redatees, {stat['conflit']} en conflit")

    # ── Qui occupe le siege ? ───────────────────────────────────────────────
    # Une partielle ouvre un mandat sans savoir QUI l'a gagnee : la chronologie
    # donne le parti, pas toujours un nom exploitable. Un mandat sans personne
    # est pourtant inutilisable en aval — pplmatch apparie des NOMS, et un siege
    # sans nom reste `unmatched`, donc jete.
    #
    # Le releve du jour de l'ANQ nomme les 125 titulaires ACTUELS. On ne s'en
    # sert que pour les mandats qui couvrent aujourd'hui : c'est un instantane,
    # il ne dit rien du passe et on ne lui fait rien dire de plus.
    courants = charger_deputes_courants(extdata)
    if courants:
        aujourdhui = date.today()
        for m in mandats:
            if m["person_id"] or m["date_start"] > aujourdhui or m["date_end"] < aujourdhui:
                continue
            r = courants.get(m["seat_id"])
            if r and r.get("person_id"):
                m["person_id"] = r["person_id"]

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
    `wp+ref:` dans `source`, pour rester repérables et re-generables.
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

    src = f"wp+ref:{leg}e-legislature"
    out, non_resolus = [], []
    puces = [(d, t, ref) for d, t, ref in puces
             if d.year not in annees_chrono]
    puces_avec_ref = [(d, t) for d, t, ref in puces if ref]
    sans_ref = len(puces) - len(puces_avec_ref)
    for d, texte in puces_avec_ref:
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
          f"{sans_ref} ecartee(s) sans <ref>, {len(out)} evenement(s) retenu(s)")
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

def invariants(mandats, persons=None):
    pbs = []

    # ── Clefs etrangeres ────────────────────────────────────────────────────
    # Un mandat qui pointe vers une personne inexistante est un depute SANS
    # NOM : pplmatch apparie des noms, donc la ligne est inutilisable et sa
    # parole se perd. C'est arrive en desambiguisant les deux Eric Girard —
    # l'identifiant 17929 est entre dans les mandats avant d'exister dans la
    # table des personnes.
    if persons is not None:
        connus = {str(p["person_id"]) for p in persons if p.get("person_id")}
        manquants = {m["person_id"] for m in mandats
                     if m["person_id"] and str(m["person_id"]) not in connus}
        for pid in sorted(manquants):
            pbs.append(f"PERSONNE INCONNUE : person_id={pid}")

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

    # ── Invariant 2 : le plafond ────────────────────────────────────────────
    # Il etait ECRIT dans la spec et jamais implemente. « 0 violation » ne
    # portait donc que sur deux regles de cinq — un chiffre rassurant qui ne
    # regardait pas la ou etait l'erreur. Un titre de circonscription non
    # detecte faisait attribuer ses lignes a la precedente, le meme siege
    # existait sous deux identifiants, et 137 mandats coexistaient en 2022. Le
    # non-chevauchement ne pouvait pas le voir : deux identifiants differents
    # ne se chevauchent jamais.
    dates = sorted({m["date_start"] for m in mandats})
    for d in dates:
        n = sum(1 for m in mandats if m["date_start"] <= d <= m["date_end"])
        if n > PLAFOND_SIEGES:
            pbs.append(f"PLAFOND depasse au {d} : {n} mandats ouverts "
                       f"(maximum {PLAFOND_SIEGES})")
            break          # un seul suffit a signaler : ils se ressemblent tous

    # ── Invariant 4 : la continuite ─────────────────────────────────────────
    # Une defection ferme un mandat ET en ouvre un autre, pour la MEME personne
    # et le meme siege, le lendemain. Sans ce controle, une defection peut
    # fermer un mandat sans rien rouvrir : le depute disparait du corpus au
    # lieu de changer de banniere.
    for seat, ms in par_siege.items():
        ouverts = {m["date_start"] for m in ms}
        for m in ms:
            if m["end_reason"] != "defection":
                continue
            if m["date_end"] + timedelta(days=1) not in ouverts:
                pbs.append(f"DEFECTION SANS SUITE : {seat} {m['date_end']}")
    # ── Invariant 6 : on ne gagne pas sa propre partielle ───────────────────
    # Si une personne ouvre un mandat par PARTIELLE sur un siege, elle ne peut
    # pas etre aussi la gagnante de la GENERALE du meme siege dans la meme
    # legislature : la partielle n'aurait pas eu lieu.
    #
    # Cas reel, et couteux : le referentiel inscrit Catherine Gentilcore comme
    # elue de la generale de 2022 dans Terrebonne (`type=election`), alors
    # qu'elle a gagne la partielle du 2025-03-17. Le siege appartenait a Pierre
    # Fitzgibbon, CAQ. La parole de 2022 a 2024 partait donc a la mauvaise
    # personne ET au mauvais parti — sans qu'aucun autre controle bronche.
    for seat, ms in par_siege.items():
        partielles = {m["person_id"] for m in ms
                      if m["start_reason"] == "byelection" and m["person_id"]}
        for m in ms:
            if m["start_reason"] == "election" and m["person_id"] in partielles:
                pbs.append(f"GAGNANT DE SA PROPRE PARTIELLE : {seat} "
                           f"{m['date_start']} person_id={m['person_id']}")
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
    idx_noms = index_noms(build_persons(extdata))
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
            sieges_dem = extract_demission(texte)
            # Un seul siege ET un seul nom : sinon on ne saurait pas les
            # apparier, et on prefere ne rien dire.
            qui = personne_nommee(texte, idx_noms) if len(sieges_dem) == 1 else None
            for dem in sieges_dem:
                evenements.append({"type": "resignation",
                                   "date": date_effet(texte, d),
                                   "seat_id": dem, "person_id": qui,
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

    pbs = invariants(mandats, persons)
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
