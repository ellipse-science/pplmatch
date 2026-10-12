# inst/extdata — ce que chaque fichier est

Trois familles de fichiers cohabitent ici. Le dossier le dit par sa structure ;
ce fichier le dit en toutes lettres.

## 1. La dimension publiée (racine, CSV)

Les tables que `inst/scripts/publier_dimension.R` publie telles quelles dans
le datamart `dim_qc_parliament`, et que le paquet lit à l'exécution
(`qc_mandates()`, `qc_persons()`, `pplmatchQC()`).

| fichier | grain |
|---|---|
| `mandates_qc.csv` | une personne × un siège × un parti × un intervalle fermé |
| `persons_qc.csv` | une personne (identifiant ANQ) et ses variantes de graphie |
| `seats_qc.csv` | un siège et un nom, daté |
| `functions_qc.csv` | une personne × une fonction × un intervalle |
| `election_results_qc.csv` | un candidat × un scrutin × une circonscription |
| `indemnities_qc.csv` | une indemnité de base, datée |
| `indemnity_scale_qc.csv` | une catégorie du barème |

Elles ne s'éditent pas à la main : on corrige la source ou le script, et on
régénère. Voir `design/spec-mandats-qc.md` et `design/spec-fonctions-resultats.md`.

## 2. Le calendrier et le référentiel historique (racine)

Lus à l'exécution par le matcher, mais pas publiés comme dimension.

| fichier | rôle | lecteurs |
|---|---|---|
| `legislatures_qc.json` | bornes des législatures ; une date hors borne donne `legislature = NA` | `legislature.py`, `pplmatchQC()`, `build_mandates.py` |
| `sessions_qc.json` | sessions parlementaires avec leur `session_id` ANQ (roster de séance, `web_lookup`) | `pplmatchQC()`, `web_lookup.py` |
| `elections_qc.csv` | parti au pouvoir par générale (ministre non identifié, statut gouvernement/opposition) | `pplmatchQC()` |
| `members_historic_qc.csv` | référentiel historique 1994–2025, une ligne par personne × élection ; complété depuis `mandates` par `.completer_membres()` | `qc_members()` |

Dette connue : la date d'une générale est écrite trois fois (`elections_qc.csv`,
`start_date` de `legislatures_qc.json`, `election_code` des résultats). À
unifier en une seule table de périodes, au moment d'ajouter la 44e législature.

## 3. `sources/` — les sorties de scraping

Matière première des scripts `inst/python/build_*.py`. Chaque fichier est
écrit par un script et lu par un autre ; aucun n'est une table du modèle.

| fichier | écrit par | lu par |
|---|---|---|
| `assnat_ids_qc.json` | `build_assnat_ids.py` | `build_mandates.py`, `web_lookup.py` |
| `deputes_courants_qc.csv` | `build_deputes_courants.py` (index ANQ du jour, 125 sièges exigés) | `build_mandates.py`, `build_functions.py` |
| `deputes_par_circonscription_qc.csv` | `build_deputes_par_circonscription.py` | `build_mandates.py` |
| `demissions_fiches_qc.csv` | `build_demissions_fiches.py` | `build_mandates.py` |
| `functions_transcribed_qc.csv` | à la main (biographies des anciens députés) | `build_functions.py` |
| `party_changes_qc.csv` | `build_party_changes.py` | plus personne : remplacé par `mandates_qc.csv` (spec § 1) |
| `deputes_leg43_qc.csv` | ancien relevé, défectueux (8 circonscriptions vides) | plus personne ; gardé comme pièce à conviction |

`web_lookup.py` lit `assnat_ids_qc.json` à l'exécution quand `web_lookup = TRUE`,
c'est pourquoi `sources/` reste dans le paquet installé.

Les fixtures de test (`reference_*_arthabaska.csv`) vivent sous
`tests/testthat/fixtures/`, pas ici.
