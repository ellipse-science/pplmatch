"""Web-based disambiguation using assnat.qc.ca intervention pages.

For ambiguous matches that survive contextual resolution, this module
queries the Assemblée nationale website to check whether each candidate
deputy has any recorded interventions on the target date. If exactly
one candidate spoke that day, the ambiguity is resolved.

Prerequisites:
    Run inst/python/build_assnat_ids.py once to create inst/extdata/sources/assnat_ids_qc.json.
    Without this file, web_lookup will skip all ambiguous cases silently.

Usage flow (called from matcher.py when web_lookup=True):
    1. date_to_session_id(event_date)  -> session_id (or None)
    2. lookup_deputy_id(full_name)     -> (assnat_id, url_slug) (or None, from pre-built map)
    3. get_intervention_dates(assnat_id, url_slug, session_id) -> set of dates (cached)
    4. web_disambiguate(candidates, event_date) -> (winner, "web_contextual") or (None, "ambiguous")
"""

import json
import os
import re
import time
import unicodedata
import urllib.error
import urllib.parse
import urllib.request
from datetime import date

# ---------------------------------------------------------------------------
# Session data (loaded once at module import)
# ---------------------------------------------------------------------------

_SESSIONS = None


def _load_sessions(sessions_path=None):
    global _SESSIONS
    if _SESSIONS is not None:
        return _SESSIONS
    if sessions_path is None:
        sessions_path = os.path.join(
            os.path.dirname(__file__), "..", "extdata", "sessions_qc.json"
        )
    with open(sessions_path, "r") as f:
        raw = json.load(f)
    _SESSIONS = [
        {
            "session_id": entry["session_id"],
            "legislature": entry["legislature"],
            "start_date": date.fromisoformat(entry["start_date"]),
            "end_date": date.fromisoformat(entry["end_date"]),
        }
        for entry in raw
    ]
    return _SESSIONS


def date_to_session_id(event_date, sessions_path=None):
    """Return the session_id for a given event date, or None if not covered."""
    sessions = _load_sessions(sessions_path)
    if isinstance(event_date, str):
        try:
            event_date = date.fromisoformat(event_date)
        except ValueError:
            return None
    for s in sessions:
        if s["start_date"] <= event_date <= s["end_date"]:
            return s["session_id"]
    return None


# ---------------------------------------------------------------------------
# Name normalisation (mirrors normalizer.py logic)
# ---------------------------------------------------------------------------

def _norm(text):
    """Lowercase, strip accents, keep only letters and spaces."""
    if not text:
        return ""
    text = str(text).lower()
    nfkd = unicodedata.normalize("NFD", text)
    text = "".join(c for c in nfkd if unicodedata.category(c) != "Mn")
    text = re.sub(r"[^a-z ]", " ", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text


# ---------------------------------------------------------------------------
# HTTP helpers
# ---------------------------------------------------------------------------

_BASE_URL = "https://www.assnat.qc.ca"
_HEADERS = {
    "User-Agent": "Mozilla/5.0 (compatible; pplmatch-research-tool)",
    "Accept": "text/html,application/xhtml+xml,*/*",
}
_REQUEST_DELAY = 0.4  # seconds between requests (be polite)
_TIMEOUT = 15


def _get(url):
    req = urllib.request.Request(url, headers=_HEADERS)
    with urllib.request.urlopen(req, timeout=_TIMEOUT) as r:
        return r.read().decode("utf-8", errors="replace")


def _post(url, data_dict):
    headers = dict(_HEADERS)
    headers["Content-Type"] = "application/x-www-form-urlencoded"
    data = urllib.parse.urlencode(data_dict).encode("utf-8")
    req = urllib.request.Request(url, data=data, headers=headers)
    with urllib.request.urlopen(req, timeout=_TIMEOUT) as r:
        return r.read().decode("utf-8", errors="replace")


# ---------------------------------------------------------------------------
# Deputy ID lookup — loaded from pre-built mapping file
# ---------------------------------------------------------------------------

_id_map = None    # full_name_norm -> (assnat_id, url_path)
_id_map_path_used = None


def _load_id_map(assnat_ids_path=None):
    """Load the assnat_ids_qc.json mapping built by build_assnat_ids.py.

    Indexes by both the normalized name with spaces ('jean francois lisee')
    and without spaces ('jeanfrancoislee') to handle compound first names
    that appear differently in the members CSV vs assnat.
    """
    global _id_map, _id_map_path_used
    if _id_map is not None:
        return _id_map

    if assnat_ids_path is None:
        assnat_ids_path = os.path.join(
            os.path.dirname(__file__), "..", "extdata", "sources", "assnat_ids_qc.json"
        )

    if not os.path.exists(assnat_ids_path):
        _id_map = {}
        return _id_map

    with open(assnat_ids_path, "r", encoding="utf-8") as f:
        entries = json.load(f)

    _id_map = {}
    for entry in entries:
        norm_name = _norm(entry["full_name"])
        val = (entry["assnat_id"], entry["assnat_url"])
        _id_map[norm_name] = val
        # Also index without spaces (handles "jeanfrancois" == "jean francois")
        nospace = norm_name.replace(" ", "")
        if nospace not in _id_map:
            _id_map[nospace] = val

    _id_map_path_used = assnat_ids_path
    return _id_map


def lookup_deputy_id(full_name, full_name_norm=None, assnat_ids_path=None):
    """Find a deputy's assnat numeric ID and URL slug by name.

    Looks up in the pre-built mapping created by build_assnat_ids.py.
    Returns None silently if the mapping file doesn't exist or if the
    deputy is not found (so web_lookup degrades gracefully).

    Args:
        full_name: Name as stored in members dataset.
        full_name_norm: Pre-normalised form; computed from full_name if omitted.
        assnat_ids_path: Optional path to assnat_ids_qc.json.

    Returns:
        (assnat_id, url_path) tuple, or None if not found.
        url_path looks like '/fr/deputes/legault-francois-4131/index.html'.
    """
    norm = full_name_norm if full_name_norm is not None else _norm(full_name)
    id_map = _load_id_map(assnat_ids_path)
    result = id_map.get(norm)
    if result is None:
        result = id_map.get(norm.replace(" ", ""))
    return result


# ---------------------------------------------------------------------------
# Intervention date fetching  (cached per assnat_id + session_id)
# ---------------------------------------------------------------------------

_intervention_cache = {}   # (assnat_id, session_id) -> frozenset of date strings

_SESSION_SELECT = "ctl00$ColCentre$ContenuColonneGauche$OngletInterventions$ddlChoisirSession"
_SEARCH_BUTTON  = "ctl00$ColCentre$ContenuColonneGauche$OngletInterventions$btnRechercher"
_DATE_RE = re.compile(r'<td class="colonneDate">\s*(\d{4}-\d{2}-\d{2})\s*</td>')
_VS_RE   = re.compile(r'id="__VIEWSTATE"[^>]+value="([^"]+)"')
_EV_RE   = re.compile(r'id="__EVENTVALIDATION"[^>]+value="([^"]+)"')


def get_intervention_dates(assnat_id, url_path, session_id):
    """Return the set of date strings (YYYY-MM-DD) when a deputy intervened in a session.

    Args:
        assnat_id:  Numeric ID (int or str).
        url_path:   URL path like '/fr/deputes/legault-francois-4131/index.html'.
        session_id: Integer session ID from sessions_qc.json.

    Returns:
        frozenset of 'YYYY-MM-DD' strings, empty on error.
    """
    cache_key = (int(assnat_id), int(session_id))
    if cache_key in _intervention_cache:
        return _intervention_cache[cache_key]

    # Build the interventions page URL from the index URL
    interventions_url = _BASE_URL + url_path.replace("/index.html", "/interventions.html")

    try:
        # Step 1: GET the page to retrieve the ASP.NET ViewState token
        time.sleep(_REQUEST_DELAY)
        html_get = _get(interventions_url)

        vs_match = _VS_RE.search(html_get)
        ev_match = _EV_RE.search(html_get)
        if not vs_match:
            # Page has no ViewState — probably a redirect or error page
            _intervention_cache[cache_key] = frozenset()
            return frozenset()

        # Step 2: POST back with the chosen session
        time.sleep(_REQUEST_DELAY)
        post_data = {
            "__VIEWSTATE": vs_match.group(1),
            "__EVENTVALIDATION": ev_match.group(1) if ev_match else "",
            _SESSION_SELECT: str(session_id),
            _SEARCH_BUTTON: "Rechercher",
        }
        html_post = _post(interventions_url, post_data)

        dates = frozenset(_DATE_RE.findall(html_post))

    except (urllib.error.URLError, OSError):
        dates = frozenset()

    _intervention_cache[cache_key] = dates
    return dates


# ---------------------------------------------------------------------------
# Main disambiguation function
# ---------------------------------------------------------------------------

def web_disambiguate(candidates, event_date, sessions_path=None):
    """Attempt to resolve an ambiguous match using assnat.qc.ca intervention data.

    For each candidate deputy, looks up their assnat ID then checks whether
    they have any recorded intervention on *event_date* in the relevant session.

    Args:
        candidates: List of member info dicts (from matcher._build_lookup).
                    Each must have keys: 'full_name', 'full_name_norm',
                    'party_id', 'gender', 'district_id'.
        event_date: ISO date string 'YYYY-MM-DD' or date object.
        sessions_path: Optional path to sessions_qc.json (uses bundled file if None).

    Returns:
        (result_dict, match_level) where:
          - result_dict is a _make_result-compatible dict, or None on failure.
          - match_level is 'web_contextual' (resolved) or 'ambiguous' (not resolved).
    """
    event_date_str = str(event_date) if not isinstance(event_date, str) else event_date

    session_id = date_to_session_id(event_date_str, sessions_path)
    if session_id is None:
        return None, "ambiguous"

    candidates_with_match = []

    for candidate in candidates:
        full_name = candidate.get("full_name", "")
        full_name_norm = candidate.get("full_name_norm", _norm(full_name))

        id_result = lookup_deputy_id(full_name, full_name_norm)
        if id_result is None:
            # Could not find this deputy on the site — skip (don't disqualify)
            continue

        assnat_id, url_path = id_result
        dates = get_intervention_dates(assnat_id, url_path, session_id)

        if event_date_str in dates:
            candidates_with_match.append(candidate)

    if len(candidates_with_match) == 1:
        winner = candidates_with_match[0]
        result = {
            "matched_name": winner["full_name"],
            "party_id": winner["party_id"],
            "gender": winner["gender"],
            "district_id": winner["district_id"],
            "match_level": "web_contextual",
            "match_score": 97.0,
        }
        return result, "web_contextual"

    # Could not resolve (0 or 2+ candidates spoke that day)
    return None, "ambiguous"


def clear_caches():
    """Clear in-process caches (useful for testing)."""
    global _id_cache, _intervention_cache, _SESSIONS
    _id_cache.clear()
    _intervention_cache.clear()
    _SESSIONS = None
