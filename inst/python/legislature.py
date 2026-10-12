"""Legislature date-range mapping for Quebec National Assembly."""

import json
import os
from datetime import date


def load_legislatures(json_path=None):
    """Load legislature date ranges from JSON file.

    Args:
        json_path: Path to legislatures_qc.json. If None, uses the bundled file.

    Returns:
        List of dicts with keys: legislature (int), start_date (date), end_date (date).
    """
    if json_path is None:
        json_path = os.path.join(
            os.path.dirname(__file__), "..", "extdata", "legislatures_qc.json"
        )

    with open(json_path, "r") as f:
        raw = json.load(f)

    legislatures = []
    for entry in raw:
        legislatures.append(
            {
                "legislature": entry["legislature"],
                "start_date": date.fromisoformat(entry["start_date"]),
                # Une fin absente = legislature en cours.
                "end_date": date.fromisoformat(entry["end_date"]) if entry.get("end_date") else date.max,
            }
        )
    return legislatures


def date_to_legislature(event_date, legislatures):
    """Map an event date to a legislature number.

    Args:
        event_date: A date object or ISO date string (YYYY-MM-DD).
        legislatures: List of legislature dicts from load_legislatures().

    Returns:
        Legislature number (int) or None if no match.
    """
    if isinstance(event_date, str):
        event_date = date.fromisoformat(event_date)

    for leg in legislatures:
        if leg["start_date"] <= event_date <= leg["end_date"]:
            return leg["legislature"]
    return None
