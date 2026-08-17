#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Repli sur Wikipedia quand l'ANQ n'a pas encore publie sa chronologie.

POURQUOI
--------
La Chronologie parlementaire est compilee RETROSPECTIVEMENT : au 2026-08-13,
`chrono116.html` (2026) existe mais c'est un gabarit vide (« Date »,
« Texte ici. »). Aucun evenement de l'annee en cours n'est donc disponible
dans la source qui les date — voir design/spec-mandats-qc.md § 6 bis.

Wikipedia, elle, tient la page « 43e legislature du Quebec » a jour au fil de
l'eau, avec des evenements dates :

    9 janvier : Francois Tremblay se retire du caucus de la CAQ [...]
    23 fevrier : Marie-Karlynn Laflamme, candidate du Parti quebecois,
                 remporte l'election partielle dans la circonscription de
                 Chicoutimi [...]

CE QUE CE MODULE NE FAIT PAS
-----------------------------
Il ne remplace pas la chronologie, il BOUCHE le trou de l'annee courante. Ses
evenements sortent en `confidence=single_source` et doivent etre re-generes
depuis l'ANQ des qu'elle publie. La regle de la spec tient : Wikipedia est un
candidat, jamais une verite (§ 6).

LE PIEGE DE LA CIRCONSCRIPTION
------------------------------
Wikipedia nomme des PERSONNES, pas des sieges — et quand elle nomme un siege,
ce n'est pas forcement le bon. Cas reel du 24 mars 2026 :

    « Maite Blanchette Vezina, deputee independante anciennement membre de la
      CAQ, rejoint officiellement le Parti conservateur du Quebec [...] et
      annonce sa candidature aux prochaines elections pour ce meme parti dans
      la circonscription de La Peltrie »

Son siege est RIMOUSKI ; La Peltrie est celui qu'elle brigue. Extraire la
circonscription du texte donnerait donc un evenement sur le mauvais siege. On
resout donc par PERSONNE, via le releve des deputes courants, et on ignore
toute circonscription citee dans la phrase.
"""

import re
import unicodedata
import urllib.parse
import urllib.request
import json
from datetime import date

HEADERS = {"User-Agent": "pplmatch-research-tool/1.0"}
API = "https://fr.wikipedia.org/w/api.php"

MOIS = {"janvier": 1, "fevrier": 2, "mars": 3, "avril": 4, "mai": 5, "juin": 6,
        "juillet": 7, "aout": 8, "septembre": 9, "octobre": 10,
        "novembre": 11, "decembre": 12}


def _norm(s):
    return "".join(c for c in unicodedata.normalize("NFD", s or "")
                   if unicodedata.category(c) != "Mn").lower()


def chronologie_vide(html):
    """L'ANQ a-t-elle publie cette annee, ou est-ce le gabarit ?

    Le gabarit ne contient que quelques paragraphes de chrome (« Ouvrir »,
    « Date », « Texte ici. »). Un seuil sur le nombre de paragraphes suffit et
    ne depend pas d'une chaine exacte que l'ANQ pourrait changer.
    """
    if not html:
        return True
    paras = re.findall(r"<p[^>]*>(.*?)</p>", html, re.S | re.I)
    utiles = [p for p in paras if len(re.sub(r"<[^>]+>", "", p).strip()) > 40]
    return len(utiles) < 5


def _api(params):
    url = API + "?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers=HEADERS)
    return json.loads(urllib.request.urlopen(req, timeout=30).read().decode())


def evenements_wikipedia(legislature, annee_min=None):
    """Rend [(date, texte, ref_verifiable)] depuis « <N>e legislature du Quebec ».

    La page organise les evenements en listes a puces sous un titre d'annee :
    le jour et le mois sont dans la puce, l'annee dans le titre.
    """
    page = f"{legislature}e législature du Québec"
    d = _api({"action": "parse", "page": page, "prop": "wikitext",
              "format": "json", "formatversion": "2", "redirects": "1"})
    wt = d["parse"]["wikitext"]

    out, annee = [], None
    for ligne in wt.split("\n"):
        t = ligne.strip()
        m_an = re.match(r"^=+\s*(\d{4})\s*=+$", t)
        if m_an:
            annee = int(m_an.group(1))
            continue
        if not annee or not t.startswith("*"):
            continue
        if annee_min and annee < annee_min:
            continue
        brut = t.lstrip("* ").strip()
        # Une puce Wikipedia ne devient jamais une donnee de reference sans
        # source exploitable. On garde cette information AVANT de nettoyer les
        # balises, puisque le texte nettoye sert seulement a classifier.
        ref_verifiable = _a_une_reference_verifiable(brut)
        texte = _nettoyer_wiki(brut)
        m = re.match(r"^(\d{1,2})\s*(?:er|re)?\s+([a-zA-Zéûî]+)\s*(?:\d{4})?\s*:\s*(.+)$", texte)
        if not m:
            continue
        mois = MOIS.get(_norm(m.group(2)))
        if not mois:
            continue
        try:
            quand = date(annee, mois, int(m.group(1)))
        except ValueError:
            continue
        out.append((quand, m.group(3).strip(), ref_verifiable))
    return out


def _a_une_reference_verifiable(wikitexte):
    """Vrai si une puce contient au moins une reference non vide.

    Un simple ``<ref name=\"x\"/>`` est accepte : MediaWiki resolvra ce nom
    vers sa definition ailleurs dans la page. Une balise vide ne l'est pas.
    """
    if re.search(r"<ref\b[^>]*/\s*>", wikitexte, re.I):
        return True
    return bool(re.search(r"<ref\b[^>]*>\s*[^<\s].*?</ref>",
                          wikitexte, re.I | re.S))


def _nettoyer_wiki(s):
    """Retire le balisage wiki : liens, refs, modeles."""
    s = re.sub(r"<ref[^>]*>.*?</ref>", "", s, flags=re.S)
    s = re.sub(r"<ref[^>]*/>", "", s)
    s = re.sub(r"\[\[([^\]|]+)\|([^\]]+)\]\]", r"\2", s)   # [[cible|libellé]]
    s = re.sub(r"\[\[([^\]]+)\]\]", r"\1", s)              # [[libellé]]
    # ATTENTION : la date est un MODELE, pas du texte — « {{date|9 janvier
    # 2026-}} : ... ». Supprimer les modeles en bloc effacait donc la date
    # elle-meme, et la puce devenait inclassable. On developpe d'abord
    # {{date|...}}, puis on retire le reste.
    s = re.sub(r"\{\{date\s*\|\s*([^}|]+?)-?\s*(?:\|[^}]*)?\}\}", r"\1", s, flags=re.I)
    s = re.sub(r"\{\{[^}]*\}\}", "", s)
    s = re.sub(r"'{2,}", "", s)
    return re.sub(r"\s+", " ", s).strip()


def resoudre_siege(texte, deputes):
    """Rend le seat_id de la PERSONNE nommee dans le texte, ou None.

    `deputes` = {seat_id: {"full_name": "Nom, Prenom", ...}} — le releve de
    l'ANQ. On resout par personne et JAMAIS par la circonscription citee dans
    la phrase : Wikipedia mentionne parfois le siege qu'une personne BRIGUE,
    pas celui qu'elle occupe (cf. l'en-tete du module).
    """
    t = _norm(texte)
    for seat, d in deputes.items():
        nom = d.get("full_name", "")
        # « Poulet, Isabelle » -> parties comparables independamment de l'ordre
        parties = [p.strip() for p in _norm(nom).replace(",", " ").split() if len(p) > 2]
        if len(parties) >= 2 and all(p in t for p in parties):
            return seat
    return None


def classer_transition(texte):
    """Rend ("defection"|"byelection"|"rejoin", parti_avant, parti_apres) ou None.

    Variante de `classify` de build_party_changes : celle-ci prend le SIEGE
    comme deja resolu (par personne) et ne s'occupe que de la transition. Les
    listes de verbes et de partis sont importees de la, pour qu'un motif ajoute
    a l'une profite a l'autre.
    """
    from build_party_changes import DEPARTS, ARRIVEES, _party_code

    # Wikipedia ecrit en SIGLES la ou la chronologie ecrit la raison sociale :
    # « reintegre le caucus de la CAQ ». Bornes de mots obligatoires, sinon
    # « pq » attraperait n'importe quelle sous-chaine.
    def _code(txt):
        c = _party_code(txt)
        if c:
            return c
        for sigle, code in (("caq", "CAQ"), ("plq", "PLQ"), ("pcq", "PCQ"),
                            ("adq", "ADQ"), ("pq", "PQ"), ("qs", "QS")):
            if re.search(r"\b" + sigle + r"\b", txt):
                return code
        return None

    t = _norm(texte)
    devient_ind = "independant" in t or "independante" in t

    # Une partielle gagnee : le siege change d'occupant.
    if "partielle" in t and re.search(r"\bremporte\b|\bes[t]? elu[e]?\b|\bgagne\b", t):
        parti = _code(t)
        return ("byelection", None, parti) if parti else None

    part = any(v in t for v in DEPARTS)
    arrive = any(v in t for v in ARRIVEES)

    # « rejoint officiellement le PCQ », « reintegre le caucus de la CAQ » :
    # le parti nomme APRES le verbe d'arrivee est la destination.
    if arrive:
        for v in ARRIVEES:
            i = t.find(v)
            if i != -1:
                apres = _code(t[i:])
                if apres:
                    return ("rejoin", None, apres)

    # « quitte le caucus de la CAQ pour sieger comme depute independant »
    if part and devient_ind:
        return ("defection", _code(t), "IND")
    # Quitter un caucus SANS en rejoindre un autre, c'est sieger comme
    # independant — meme si la phrase ne le dit pas. Cas reel : « Francois
    # Tremblay se retire du caucus de la CAQ apres avoir eu un accident de
    # voiture [...] », ou le motif de l'evenement occupe toute la phrase.
    if part and "caucus" in t and not arrive:
        return ("defection", _code(t), "IND")
    return None
