"""Multi-level matching engine for Quebec National Assembly speakers.

Level 1: Deterministic (exact match after normalization)
Level 2: Fuzzy (rapidfuzz with configurable threshold)
Level 3: Contextual (inference based on session roster)
"""

import csv as _csv
import os as _os
import warnings as _warnings
from datetime import date as _date, timedelta as _timedelta

from rapidfuzz import fuzz
from normalizer import (
    normalize_speaker,
    normalize_member_name,
    extract_last_name,
)
from legislature import load_legislatures, date_to_legislature


def _load_party_changes(path):
    """Load party_changes_qc.csv into {(district_id, legislature_id): [change, ...]}."""
    if not path or not _os.path.exists(path):
        return {}
    changes = {}
    with open(path, newline="", encoding="utf-8") as f:
        for row in _csv.DictReader(f):
            key = (row["district_id"], str(row["legislature_id"]))
            changes.setdefault(key, []).append({
                "party_before": row["party_id_before"],
                "party_after": row["party_id_after"],
                "change_date": _date.fromisoformat(row["change_date"]),
            })
    return changes


_FIN_OUVERTE = _date(9999, 12, 31)


def _load_mandates(path):
    """Charge mandates_qc.csv en {seat_id: [mandat, ...]} tries par date.

    C'est le REMPLACANT de party_changes_qc.csv, et la difference tient en un
    mot : une BORNE DE FIN. Un changement de parti sans date de fin s'applique
    indefiniment, donc au SUCCESSEUR du transfuge — la defection d'Eric Lefebvre
    (Arthabaska, 2024-04-16) etait heritee par Alex Boissonneault, elu peequiste
    a la partielle de 2025. Un mandat, lui, est un intervalle ferme : il ne peut
    pas deborder sur celui d'apres.
    """
    if not path or not _os.path.exists(path):
        return {}
    par_siege = {}
    with open(path, newline="", encoding="utf-8") as f:
        for numero, row in enumerate(_csv.DictReader(f), start=2):
            try:
                debut = _date.fromisoformat(row["date_start"])
            except (ValueError, TypeError, KeyError):
                raise ValueError(
                    f"mandates_qc.csv:{numero}: date_start illisible "
                    f"({row.get('date_start')!r})"
                ) from None
            fin = row.get("date_end") or ""
            try:
                fin = _date.fromisoformat(fin) if fin else _FIN_OUVERTE
            except (ValueError, TypeError):
                raise ValueError(
                    f"mandates_qc.csv:{numero}: date_end illisible "
                    f"({row.get('date_end')!r})"
                ) from None
            if fin < debut:
                raise ValueError(
                    f"mandates_qc.csv:{numero}: date_end precede date_start"
                )
            par_siege.setdefault(row["seat_id"], []).append({
                "person_id": row.get("person_id", ""),
                "party_id": row.get("party_id", ""),
                "date_start": debut, "date_end": fin,
            })
    for v in par_siege.values():
        v.sort(key=lambda m: m["date_start"])
    return par_siege


def _borne_fin_referentiel(mandates):
    """Derniere date couverte par le referentiel, hors bornes ouvertes."""
    fins = [m["date_end"] for ms in mandates.values() for m in ms
            if m["date_end"] != _FIN_OUVERTE]
    return max(fins) if fins else None


def _statut_referentiel_mandat(seat_id, event_date, mandates):
    """Distingue un siege inconnu, une vacance et un referentiel perime."""
    if isinstance(event_date, str):
        try:
            event_date = _date.fromisoformat(event_date[:10])
        except (ValueError, TypeError):
            return "date_invalide"
    if not isinstance(event_date, _date):
        return "date_invalide"
    if not seat_id or seat_id not in mandates:
        return "siege_inconnu"
    if _mandat_en_vigueur(seat_id, event_date, mandates):
        return "couvert"
    borne_fin = _borne_fin_referentiel(mandates)
    if borne_fin and event_date > borne_fin:
        return "referentiel_perime"
    return "hors_mandat"


def _mandats_depuis_party_changes(party_changes):
    """Adapte l'ancien format en intervalles, pour n'avoir qu'UN resolveur.

    Garder deux chemins de resolution, c'est garantir qu'ils divergent. On
    convertit donc l'ancien fichier plutot que de le resoudre autrement : chaque
    changement ferme le precedent la veille, et le dernier reste ouvert.

    La conversion ne repare rien — un changement sans date de fin devient un
    intervalle qui court jusqu'a la fin des temps, et deborde donc toujours sur
    le successeur. C'est le defaut meme du format, rendu ici visible.
    """
    par_siege = {}
    for (seat, _leg), changements in party_changes.items():
        ch = sorted(changements, key=lambda x: x["change_date"])
        bornes = [{"person_id": "", "party_id": ch[0]["party_before"],
                   "date_start": _date(1867, 7, 1),
                   "date_end": ch[0]["change_date"] - _timedelta(days=1)}]
        for i, c in enumerate(ch):
            suivant = ch[i + 1]["change_date"] - _timedelta(days=1) if i + 1 < len(ch) else _FIN_OUVERTE
            bornes.append({"person_id": "", "party_id": c["party_after"],
                           "date_start": c["change_date"], "date_end": suivant})
        par_siege.setdefault(seat, []).extend(bornes)
    for v in par_siege.values():
        v.sort(key=lambda m: m["date_start"])
    return par_siege


def _mandat_en_vigueur(seat_id, event_date, mandates):
    """Rend le mandat qui couvre `event_date` pour ce siege, ou None."""
    if not mandates or not seat_id:
        return None
    if isinstance(event_date, str):
        try:
            event_date = _date.fromisoformat(event_date[:10])
        except (ValueError, TypeError):
            return None
    if not isinstance(event_date, _date):
        return None
    for m in mandates.get(seat_id, ()):
        if m["date_start"] <= event_date <= m["date_end"]:
            return m
    return None


def _resolve_party(district_id, event_date, mandates):
    """Rend le parti en vigueur pour ce siege a cette date, ou None.

    On ne rend RIEN plutot qu'un parti approximatif : une date hors de tout
    mandat connu (siege vacant, lacune du referentiel) doit laisser le parti
    issu de l'appariement, pas en inventer un.
    """
    m = _mandat_en_vigueur(district_id, event_date, mandates)
    return m["party_id"] if m and m["party_id"] else None


def _build_lookup(members, legislature):
    """Build lookup indexes for a given legislature."""
    full_name_index = {}
    other_names_index = {}
    last_name_index = {}
    district_index = {} # {normalized_district: [member_info, ...]}
    all_members = []

    leg_str = str(legislature)

    for m in members:
        m_leg = str(m.get("legislature_id", ""))
        if m_leg != leg_str:
            continue

        full_norm = normalize_member_name(m["full_name"])
        last = extract_last_name(full_norm)
        dist_norm = normalize_member_name(m.get("district_id", ""))

        info = {
            "full_name": m["full_name"],
            "full_name_norm": full_norm,
            "last_name_norm": last,
            "party_id": m.get("party_id", ""),
            "gender": m.get("gender", ""),
            "district_id": m.get("district_id", ""),
            "district_norm": dist_norm
        }
        all_members.append(info)

        full_name_index[full_norm] = info

        # Index other_names (semicolon-separated)
        other_names_raw = m.get("other_names", None)
        if other_names_raw and str(other_names_raw).strip():
            for alt in str(other_names_raw).split(";"):
                alt = alt.strip()
                if alt:
                    alt_norm = normalize_member_name(alt)
                    other_names_index[alt_norm] = info

        # Last name index
        if last not in last_name_index:
            last_name_index[last] = []
        last_name_index[last].append(info)
        
        # District index
        if dist_norm:
            if dist_norm not in district_index:
                district_index[dist_norm] = []
            district_index[dist_norm].append(info)

    return {
        "full_name_index": full_name_index,
        "other_names_index": other_names_index,
        "last_name_index": last_name_index,
        "district_index": district_index,
        "all_members": all_members,
    }


def _fuzzy_score_full(speaker_norm, candidate_norm):
    s1 = fuzz.token_sort_ratio(speaker_norm, candidate_norm)
    s2 = fuzz.ratio(speaker_norm, candidate_norm)
    return 0.6 * s1 + 0.4 * s2


def _fuzzy_score_last(speaker_norm, candidate_last):
    s1 = fuzz.partial_ratio(speaker_norm, candidate_last)
    s2 = fuzz.token_sort_ratio(speaker_norm, candidate_last)
    s3 = fuzz.ratio(speaker_norm, candidate_last)
    return 0.5 * s1 + 0.3 * s2 + 0.2 * s3


def _make_result(info, match_level, match_score):
    return {
        "matched_name": info["full_name"],
        "party_id": info["party_id"],
        "gender": info["gender"],
        "district_id": info["district_id"],
        "match_level": match_level,
        "match_score": match_score,
    }


def _make_ambiguous_result(candidates, match_score):
    parties = set(c.get("party_id") for c in candidates if c.get("party_id"))
    genders = set(c.get("gender") for c in candidates if c.get("gender"))
    
    consensus_party = parties.pop() if len(parties) == 1 else None
    consensus_gender = genders.pop() if len(genders) == 1 else None
    
    names = sorted([c["full_name"] for c in candidates])
    composite_name = "; ".join(names)

    return {
        "matched_name": composite_name,
        "party_id": consensus_party,
        "gender": consensus_gender,
        "district_id": None,
        "match_level": "ambiguous",
        "match_score": match_score,
    }


def match_speaker_atomic(speaker_norm, lookup, fuzzy_threshold=85, speaker_district=None):
    """Core matching logic for a single speaker."""
    no_match = {
        "matched_name": None, "party_id": None, "gender": None,
        "district_id": None, "match_level": "unmatched", "match_score": None,
    }

    if not speaker_norm:
        return no_match, None

    # 1. District override: If we have a district, filter members by it
    if speaker_district and speaker_district in lookup["district_index"]:
        district_members = lookup["district_index"][speaker_district]
        # If only one member in district, high chance it's them
        if len(district_members) == 1:
            return _make_result(district_members[0], "deterministic", 100.0), None
        # Otherwise, match name within that district
        for m in district_members:
            if speaker_norm in m["full_name_norm"] or m["last_name_norm"] == speaker_norm:
                return _make_result(m, "deterministic", 100.0), None

    # 2. Exact matches
    if speaker_norm in lookup["full_name_index"]:
        return _make_result(lookup["full_name_index"][speaker_norm], "deterministic", 100.0), None

    if speaker_norm in lookup["other_names_index"]:
        return _make_result(lookup["other_names_index"][speaker_norm], "deterministic", 100.0), None

    # 3. Last name matches
    speaker_tokens = speaker_norm.split()
    if len(speaker_tokens) == 1:
        last = speaker_tokens[0]
        if last in lookup["last_name_index"]:
            candidates = lookup["last_name_index"][last]
            if len(candidates) == 1:
                return _make_result(candidates[0], "deterministic", 100.0), None
            else:
                return _make_ambiguous_result(candidates, 100.0), candidates

    # 4. Fuzzy matches
    best_score = 0.0
    best_info = None
    is_single_token = len(speaker_tokens) == 1

    for member in lookup["all_members"]:
        if is_single_token:
            score = _fuzzy_score_last(speaker_norm, member["last_name_norm"])
        else:
            # Try matching speaker_norm as a substring of full_name (handles "Zaga Mendez")
            if speaker_norm in member["full_name_norm"]:
                score = 95.0
            else:
                score = _fuzzy_score_full(speaker_norm, member["full_name_norm"])

        if score > best_score:
            best_score = score
            best_info = member

    if best_score >= fuzzy_threshold and best_info is not None:
        if is_single_token:
            close_matches = [m for m in lookup["all_members"] if _fuzzy_score_last(speaker_norm, m["last_name_norm"]) >= fuzzy_threshold]
            unique_matches = list({m["full_name"]: m for m in close_matches}.values())
            if len(unique_matches) > 1:
                return _make_ambiguous_result(unique_matches, best_score), unique_matches

        return _make_result(best_info, "fuzzy", best_score), None

    return no_match, None


def match_corpus(corpus_rows, members, fuzzy_threshold=85,
                 legislatures_path=None, sessions_path=None,
                 party_changes_path=None, mandates_path=None,
                 web_lookup=False, verbose=False):
    legislatures = load_legislatures(legislatures_path)
    # Le modele DATE remplace party_changes_qc.csv des qu'il est fourni ; on
    # garde l'ancien chemin en repli pour ne pas casser un appelant qui ne le
    # passe pas encore.
    mandates = _load_mandates(mandates_path)
    party_changes = {} if mandates else _load_party_changes(party_changes_path)
    if party_changes:
        mandates = _mandats_depuis_party_changes(party_changes)
    dates_hors_couverture = []
    lookup_cache = {}
    grouped_results = {}
    n = len(corpus_rows)

    if verbose:
        print(f"  Pre-processing {n} rows...")

    for i, row in enumerate(corpus_rows):
        speaker_raw = row.get("speaker", "")
        event_date = row.get("event_date", "")
        date_str = str(event_date) if event_date else "unknown"
        leg = date_to_legislature(event_date, legislatures)

        category, speaker_norm, speaker_dist = normalize_speaker(speaker_raw)

        result = dict(row)
        result.update({"speaker_category": category, "speaker_normalized": speaker_norm, "legislature": leg,
                       "matched_name": None, "party_id": None, "gender": None, "district_id": None,
                       "match_level": "unmatched", "match_score": None})

        candidates = None
        if category == "person" and leg is not None:
            if leg not in lookup_cache:
                lookup_cache[leg] = _build_lookup(members, leg)
            match_res, candidates = match_speaker_atomic(speaker_norm, lookup_cache[leg], fuzzy_threshold, speaker_dist)
            result.update(match_res)
            if mandates and result.get("district_id"):
                resolved = _resolve_party(result["district_id"], event_date, mandates)
                if resolved is not None:
                    result["party_id"] = resolved
                elif _statut_referentiel_mandat(
                        result["district_id"], event_date, mandates) == "referentiel_perime":
                    dates_hors_couverture.append(str(event_date)[:10])
        else:
            result["match_level"] = category if category != "person" else "unmatched"

        if date_str not in grouped_results:
            grouped_results[date_str] = []
        grouped_results[date_str].append({"index": i, "result": result, "candidates": candidates})

    # --- Level 3: Contextual resolution (daily roster) ---
    final_results_sorted = [None] * n
    for date_str, items in grouped_results.items():
        daily_roster = set(item["result"]["matched_name"] for item in items if item["result"]["match_level"] in ("deterministic", "fuzzy") and item["result"]["matched_name"])
        for item in items:
            res, candidates = item["result"], item["candidates"]
            if res["match_level"] == "ambiguous" and candidates:
                matches_in_roster = [c for c in candidates if c["full_name"] in daily_roster]
                if len(matches_in_roster) == 1:
                    best = matches_in_roster[0]
                    res.update({"matched_name": best["full_name"], "party_id": best["party_id"], "gender": best["gender"],
                                "district_id": best["district_id"], "match_level": "contextual", "match_score": 99.0})
                    if mandates and res.get("district_id"):
                        resolved = _resolve_party(res["district_id"], date_str, mandates)
                        if resolved is not None:
                            res["party_id"] = resolved
                        elif _statut_referentiel_mandat(
                                res["district_id"], date_str, mandates) == "referentiel_perime":
                            dates_hors_couverture.append(str(date_str)[:10])
            final_results_sorted[item["index"]] = res

    # --- Level 4: Web-based disambiguation (optional, requires network) ---
    if web_lookup:
        from web_lookup import web_disambiguate
        n_web = 0
        for item in (i for date_items in grouped_results.values() for i in date_items):
            res = item["result"]
            candidates = item["candidates"]
            if res["match_level"] != "ambiguous" or not candidates:
                continue
            event_date = res.get("event_date", "")
            if not event_date or str(event_date) == "unknown":
                continue
            web_result, web_level = web_disambiguate(
                candidates, str(event_date), sessions_path=sessions_path
            )
            if web_level == "web_contextual" and web_result is not None:
                res.update(web_result)
                n_web += 1
        if verbose and n_web:
            print(f"  Web lookup resolved {n_web} additional ambiguous case(s).")

    if verbose:
        stats = {lvl: sum(1 for r in final_results_sorted if r["match_level"] == lvl) for lvl in
                 ["deterministic", "fuzzy", "contextual", "web_contextual", "ambiguous", "role", "crowd", "unmatched"]}
        print(f"  Done. Det: {stats['deterministic']}, Fuzzy: {stats['fuzzy']}, Ctx: {stats['contextual']}, "
              f"Web: {stats['web_contextual']}, Amb: {stats['ambiguous']}, Roles: {stats['role']}, Unm: {stats['unmatched']}")

    if dates_hors_couverture:
        debut, fin = min(dates_hors_couverture), max(dates_hors_couverture)
        _warnings.warn(
            f"Referentiel de mandats perime : {len(dates_hors_couverture)} "
            f"appariement(s) date(s) de {debut} a {fin} depassent sa derniere "
            f"borne connue ({_borne_fin_referentiel(mandates).isoformat()}). Regenerer mandates_qc.csv "
            f"avant de publier ces resultats.",
            RuntimeWarning,
            stacklevel=2,
        )

    return final_results_sorted
