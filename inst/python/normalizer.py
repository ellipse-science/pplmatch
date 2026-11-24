"""Normalization of Quebec National Assembly speaker names."""

import re
import unicodedata

# Roles that identify institutional speakers (not individual persons)
ROLES = {
    "le president", "la presidente",
    "le vice-president", "la vice-presidente",
    "le president suppleant", "la presidente suppleante",
    "une voix", "des voix",
    "le secretaire", "la secretaire", "le secretaire adjoint", "la secretaire adjointe",
    "le greffier", "la greffiere",
    "mise aux voix", "motion", "ordre du jour",
}

# Patterns that match role-like speakers
ROLE_PREFIXES = (
    "le president",
    "la presidente",
    "le vice-president",
    "la vice-presidente",
    "le secretaire",
    "la secretaire",
    "le greffier",
    "la greffiere",
)

# Crowds / collective voices
CROWDS = {"des voix"}

# Honorifics to strip
HONORIFICS_RE = re.compile(
    r"^(M\.\s*|Mme\s+|Mme\.\s*|Mr\.\s*|Mr\s+)", re.IGNORECASE
)

# Honorific glued to name (e.g. "M.Caire" -> "Caire")
HONORIFIC_GLUED_RE = re.compile(
    r"^(M\.|Mme\.?)([A-ZÀ-ÖØ-Ý])", re.IGNORECASE
)

# Leading numeric garbage (e.g. "15 725 M. Marissal")
LEADING_NUMS_RE = re.compile(r"^[\d\s]+")

# Trailing district after comma (e.g. ", Chauveau", ", Côte-du-Sud").
# Traits d'union et tirets ADMIS : la 44e départage vingt homonymes par leur
# circonscription, et « Poulin, Côte-du-Sud » n'était pas lue (pplmatch#11).
TRAILING_DISTRICT_RE = re.compile(r",\s*([A-ZÀ-ÖØ-Ý][^(]*)$", re.IGNORECASE)


def district_key(name):
    """Clé compacte d'une circonscription : sans accent, espace ni ponctuation.

    « La Pinière » et le siège `lapiniere` doivent se reconnaître : la
    comparaison sur le nom normalisé (« la piniere ») échouait dès que la
    circonscription avait plus d'un mot (pplmatch#11).
    """
    if not name:
        return ""
    return re.sub(r"[^a-z0-9]", "", strip_accents(str(name)).lower())

# Trailing action keywords (e.g. "Legault (réplique)" or "Legault réplique")
TRAILING_ACTION_RE = re.compile(
    r"\s*([\(\-–—]\s*)?"
    r"(réplique|suite|en remplacement|par intérim|suppléant|suppléante)"
    r"[\)\s]*$",
    re.IGNORECASE,
)


def strip_accents(text):
    """Remove diacritics from text using NFD decomposition."""
    if not text:
        return ""
    nfkd = unicodedata.normalize("NFD", text)
    return "".join(c for c in nfkd if unicodedata.category(c) != "Mn")


def classify_speaker(raw_speaker):
    """Classify a raw speaker string into a category.

    Returns one of: 'person', 'role', 'crowd', 'empty'.
    """
    if not raw_speaker or not str(raw_speaker).strip():
        return "empty"

    cleaned = str(raw_speaker).strip()
    # Remove leading numbers first for classification (e.g. "12 187 La Présidente")
    cleaned = LEADING_NUMS_RE.sub("", cleaned).strip()
    
    lower = cleaned.lower()
    lower_no_accent = strip_accents(lower)

    # Check exact crowd matches
    if lower_no_accent in CROWDS:
        return "crowd"

    # Check role patterns
    if lower_no_accent in ROLES:
        return "role"
    
    for prefix in ROLE_PREFIXES:
        if lower_no_accent.startswith(prefix):
            return "role"

    # Keywords that indicate procedure rather than a person speaking
    if any(keyword in lower_no_accent for keyword in ["mise aux voix", "motion", "grief"]):
        return "role"

    return "person"


def normalize_speaker(raw_speaker):
    """Normalize a speaker name for matching.

    Returns a tuple: (category, normalized_name, extracted_district)
    - category: 'person', 'role', 'crowd', or 'empty'
    - normalized_name: cleaned name string
    - extracted_district: cleaned district name (or None)
    """
    category = classify_speaker(raw_speaker)
    if category != "person":
        return category, "", None

    name = str(raw_speaker).strip()

    # 1. Extract district before stripping leading numbers (sometimes numbers are page refs)
    district = None
    district_match = TRAILING_DISTRICT_RE.search(name)
    if district_match:
        district_raw = district_match.group(1)
        district = district_key(district_raw)
        name = TRAILING_DISTRICT_RE.sub("", name).strip()

    # 2. Remove leading numeric garbage
    name = LEADING_NUMS_RE.sub("", name).strip()

    # 3. Remove trailing action keywords
    name = TRAILING_ACTION_RE.sub("", name).strip()

    # 4. Handle glued honorific (M.Caire -> Caire)
    name = HONORIFIC_GLUED_RE.sub(r"\2", name)

    # 5. Remove honorific prefix
    name = HONORIFICS_RE.sub("", name).strip()

    # 6. Final cleanup
    name = name.lower()
    name = strip_accents(name)
    name = re.sub(r"[^a-z ]", "", name)
    name = re.sub(r"\s+", " ", name).strip()

    return "person", name, district


def normalize_member_name(name):
    """Normalize a member name from the reference dataset for matching."""
    if not name:
        return ""
    name = str(name)
    # Handle "Lastname, Firstname" format (common in CLESSN data)
    if "," in name:
        parts = name.split(",", 1)
        if len(parts) == 2:
            # Flip to "Firstname Lastname"
            name = parts[1].strip() + " " + parts[0].strip()

    name = name.lower()
    name = strip_accents(name)
    name = re.sub(r"[^a-z ]", "", name)
    name = re.sub(r"\s+", " ", name).strip()
    return name


def extract_last_name(normalized_name):
    """Extract the last name from a normalized full name.

    Convention: the last token is the family name.
    """
    parts = normalized_name.split()
    if not parts:
        return ""
    return parts[-1]
