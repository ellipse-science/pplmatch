#' Appariement des intervenants de l'Assemblee nationale du Quebec
#'
#' Fait correspondre les noms des intervenants dans les debats de l'Assemblee
#' nationale du Quebec avec la table dimensionnelle des deputes. Utilise un
#' matching multi-niveaux : deterministe (correspondance exacte apres
#' normalisation) puis fuzzy (similarite via rapidfuzz en Python).
#'
#' @param corpus Un data frame des debats parlementaires. Doit contenir au
#'   minimum les colonnes :
#'   \describe{
#'     \item{speaker}{Nom de l'intervenant tel qu'il apparait dans les debats
#'       (ex: "Legault", "M. Berube", "Le President", "Des Voix").}
#'     \item{event_date}{Date du debat, en format Date ou caractere YYYY-MM-DD.}
#'   }
#'   Toutes les autres colonnes sont conservees dans le resultat.
#'
#' @param members Un data frame de la table des deputes, ou \code{NULL}
#'   (defaut) pour le construire depuis \code{annuaire} (mandats et personnes).
#'   Colonnes requises :
#'   \describe{
#'     \item{full_name}{Nom complet du depute (ex: "Francois Legault").}
#'     \item{party_id}{Identifiant du parti (ex: "CAQ", "PLQ", "PQ", "QS").}
#'     \item{gender}{Genre du depute ("m" ou "f").}
#'     \item{legislature_id}{Numero de la legislature en caractere (ex: "42").}
#'   }
#'   Colonnes optionnelles :
#'   \describe{
#'     \item{other_names}{Noms alternatifs separes par des points-virgules
#'       (ex: "Legault; F. Legault"). Utile pour les noms qui different
#'       entre les debats et la table officielle.}
#'     \item{district_id}{Identifiant de la circonscription.}
#'   }
#'
#' @param annuaire L'annuaire des deputes : une liste de tables \code{mandates},
#'   \code{persons}, \code{functions} et \code{periods} (les tables
#'   \code{dim-qc-parliament-*} de l'entrepot, lues par tube, ou un dossier de
#'   CSV lu par \code{\link{qc_annuaire_csv}}). pplmatch ne contient aucune
#'   donnee : sans annuaire, pas d'appariement.
#'
#' @param fuzzy_threshold Score minimum (0 a 100) pour accepter un match fuzzy.
#'   Defaut : 85. Un seuil plus eleve = plus precis mais moins de matchs.
#'   Un seuil plus bas = plus de matchs mais plus de faux positifs.
#'
#' @param legislatures Vecteur d'entiers des numeros de legislatures a traiter.
#'   Defaut : toutes celles de \code{annuaire$periods}. Les lignes du corpus
#'   dont la date tombe hors de ces legislatures recoivent un
#'   \code{match_level} de \code{"unmatched"} et un \code{legislature} de
#'   \code{NA}.
#'
#' @param web_lookup Logique. Si \code{TRUE}, tente de resoudre les cas
#'   \code{"ambiguous"} restants apres la resolution contextuelle en
#'   consultant les pages d'interventions des deputes sur le site de
#'   l'Assemblee nationale (\url{https://www.assnat.qc.ca}). Pour chaque cas
#'   ambigu, interroge le site pour verifier si exactement un des candidats
#'   a des interventions enregistrees a cette date. Necessite une connexion
#'   internet. Defaut : \code{FALSE}.
#'
#' @param verbose Logique. Si \code{TRUE}, affiche la progression et un resume
#'   des resultats a la fin. Defaut : \code{FALSE}.
#'
#' @return Un tibble contenant toutes les colonnes du corpus original, plus
#'   les colonnes suivantes :
#'   \describe{
#'     \item{legislature}{Numero de la legislature (entier) determine
#'       automatiquement a partir de \code{event_date}. \code{NA} si la date
#'       est hors des legislatures couvertes.}
#'     \item{speaker_category}{Classification de l'intervenant :
#'       \code{"person"} (depute), \code{"role"} (fonction institutionnelle
#'       comme "Le President"), \code{"crowd"} (voix collectives comme
#'       "Des Voix"), ou \code{"empty"} (champ vide).}
#'     \item{speaker_normalized}{Version normalisee du nom (minuscules, sans
#'       accents, sans honorifiques). Vide pour les non-personnes.}
#'     \item{matched_name}{Nom complet du depute apparie, ou \code{NA} si
#'       aucun match.}
#'     \item{party_id}{Parti politique du depute apparie, ou \code{NA}.}
#'     \item{gender}{Genre du depute apparie, ou \code{NA}.}
#'     \item{district_id}{Circonscription du depute apparie, ou \code{NA}.}
#'     \item{match_level}{Niveau de l'appariement :
#'       \code{"deterministic"} (correspondance exacte),
#'       \code{"fuzzy"} (similarite au-dessus du seuil),
#'       \code{"contextual"} (resolu par le roster journalier),
#'       \code{"web_contextual"} (resolu via assnat.qc.ca, uniquement si
#'         \code{web_lookup = TRUE}),
#'       \code{"presiding_officer"} (etiquette nue « La Presidente » /
#'         « Le President » attribuee a la personne qui detient la
#'         presidence de l'Assemblee ce jour-la, d'apres functions_qc.csv),
#'       \code{"ambiguous"} (plusieurs deputes possibles),
#'       \code{"unmatched"} (aucune correspondance),
#'       \code{"role"}, \code{"crowd"}, \code{"empty"}.}
#'     \item{match_score}{Score de similarite entre 0 et 100. \code{100} pour
#'       les matchs deterministes, la valeur calculee pour les fuzzy, \code{NA}
#'       pour les non-matchs.}
#'   }
#'
#' @section Prerequis:
#' Avant la premiere utilisation, executer \code{\link{ensure_python_deps}()}
#' pour installer les dependances Python (rapidfuzz).
#'
#' @section Pipeline de matching:
#' \enumerate{
#'   \item **Preprocessing** : classification de l'intervenant, normalisation
#'     du nom (retrait des honorifiques, accents, chiffres, districts).
#'   \item **Niveau 1 (deterministe)** : correspondance exacte sur le nom
#'     complet normalise, puis sur les noms alternatifs, puis sur le nom
#'     de famille seul (si non-ambigu dans la legislature).
#'   \item **Niveau 2 (fuzzy)** : si le niveau 1 echoue, comparaison par
#'     similarite avec rapidfuzz. Score pondere different pour les noms
#'     complets vs noms de famille seuls.
#'   \item **Niveau 3 (contextuel)** : pour les cas ambigus, verifie si
#'     exactement un des candidats figure dans le roster journalier (c.-a-d.
#'     a deja ete apparie deterministe ou fuzzy ce jour-la).
#'   \item **Niveau 4 (web, optionnel)** : si \code{web_lookup = TRUE}, pour
#'     les cas encore ambigus, consulte les pages d'interventions de
#'     l'Assemblee nationale pour chaque candidat et verifie si un seul
#'     a des interventions enregistrees a cette date precise.
#' }
#'
#' @examples
#' \dontrun{
#' # Premiere utilisation : installer les dependances Python
#' ensure_python_deps()
#'
#' # Preparer les donnees
#' corpus <- data.frame(
#'   speaker = c("Legault", "M. Bérubé", "Le Président", "Des Voix"),
#'   event_date = as.Date(c("2023-03-15", "2023-03-15", "2023-03-15", "2023-03-15"))
#' )
#'
#' members <- data.frame(
#'   full_name = c("François Legault", "Pascal Bérubé"),
#'   party_id = c("CAQ", "PQ"),
#'   gender = c("m", "m"),
#'   legislature_id = c("43", "43")
#' )
#'
#' # Lancer l'appariement (le repertoire est construit depuis l'annuaire)
#' result <- pplmatchQC(corpus, verbose = TRUE)
#'
#' # Ou avec une table de deputes custom
#' result <- pplmatchQC(corpus, members = members, fuzzy_threshold = 85)
#'
#' # Verifier le taux d'appariement (personnes seulement)
#' persons <- result[result$speaker_category == "person", ]
#' rate <- sum(persons$match_level %in% c("deterministic", "fuzzy")) / nrow(persons)
#' message(sprintf("Taux d'appariement : %.1f%%", rate * 100))
#'
#' # Explorer les cas non-apparies
#' unmatched <- result[result$match_level == "unmatched", ]
#' table(unmatched$speaker)
#'
#' # Explorer les cas ambigus
#' ambiguous <- result[result$match_level == "ambiguous", ]
#' table(ambiguous$speaker)
#' }
#'
#' @seealso \code{\link{ensure_python_deps}} pour l'installation des dependances,
#'   \code{\link{qc_annuaire_csv}} pour lire l'annuaire depuis un dossier,
#'   \code{\link{generate_gold_sample}} et \code{\link{evaluate_matches_qc}} pour
#'   l'evaluation de la qualite.
#'
#' @import dplyr
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr map_chr map_dbl
#' @importFrom reticulate import_from_path
#'
#' @export
pplmatchQC <- function(corpus, annuaire, members = NULL,
                       fuzzy_threshold = 85,
                       legislatures = NULL,
                       web_lookup = FALSE,
                       verbose = FALSE) {

  annuaire <- .verifier_annuaire(annuaire)
  bornes <- .bornes_legislatures(annuaire$periods)
  if (is.null(legislatures)) legislatures <- bornes$legislature

  # Sans `members`, le repertoire est construit depuis les mandats et les
  # personnes de l'annuaire : une ligne par (personne x legislature). Un
  # appelant qui fournit son propre `members` decrit exactement la population
  # qu'il veut apparier ; on n'y injecte rien.
  if (is.null(members)) {
    members <- .membres_depuis_annuaire(annuaire, bornes)
  }

  # Load Python matcher
  matcher <- .load_matcher()

  # Prepare corpus rows as list of dicts for Python
  corpus_list <- purrr::map(seq_len(nrow(corpus)), function(i) {
    list(
      speaker = as.character(corpus$speaker[i]),
      event_date = as.character(corpus$event_date[i])
    )
  })

  # Prepare member records as list of dicts for Python
  # Filter to requested legislatures
  members_filtered <- members[members$legislature_id %in% as.character(legislatures), ]
  members_list <- purrr::map(seq_len(nrow(members_filtered)), function(i) {
    row <- members_filtered[i, ]
    m <- list(
      full_name = as.character(row$full_name),
      party_id = as.character(row$party_id),
      gender = as.character(row$gender),
      legislature_id = as.character(row$legislature_id)
    )
    if ("other_names" %in% names(row) && !is.na(row$other_names)) {
      m$other_names <- as.character(row$other_names)
    }
    if ("district_id" %in% names(row) && !is.na(row$district_id)) {
      m$district_id <- as.character(row$district_id)
    }
    m
  })

  # Le matcher lit des fichiers : on lui ecrit l'annuaire dans un dossier
  # temporaire, efface a la sortie.
  fichiers <- .fichiers_temporaires_annuaire(annuaire)
  on.exit(unlink(fichiers$dossier, recursive = TRUE), add = TRUE)

  # La desambiguation par le web cherche la fiche ANQ de chaque candidat :
  # son identifiant et son URL sont dans `persons`.
  persons_list <- NULL
  if (isTRUE(web_lookup)) {
    pp <- annuaire$persons
    persons_list <- purrr::map(seq_len(nrow(pp)), function(i) {
      list(person_id = pp$person_id[i], full_name = pp$full_name[i],
           assnat_url = if ("assnat_url" %in% names(pp)) pp$assnat_url[i] else "")
    })
  }

  # Call Python matcher
  results <- matcher$match_corpus(
    corpus_rows         = corpus_list,
    members             = members_list,
    fuzzy_threshold     = as.integer(fuzzy_threshold),
    legislatures_path   = fichiers$legislatures,
    sessions_path       = fichiers$sessions,
    mandates_path       = fichiers$mandates,
    functions_path      = fichiers$functions,
    persons             = persons_list,
    web_lookup          = isTRUE(web_lookup),
    verbose             = verbose
  )

  # Convert Python results back to tibble
  n <- length(results)
  out <- tibble::tibble(
    speaker_category = purrr::map_chr(results, ~ .x$speaker_category %||% NA_character_),
    speaker_normalized = purrr::map_chr(results, ~ .x$speaker_normalized %||% NA_character_),
    legislature = purrr::map_int(results, ~ as.integer(.x$legislature %||% NA_integer_)),
    matched_name = purrr::map_chr(results, ~ .x$matched_name %||% NA_character_),
    party_id = purrr::map_chr(results, ~ .x$party_id %||% NA_character_),
    gender = purrr::map_chr(results, ~ .x$gender %||% NA_character_),
    district_id = purrr::map_chr(results, ~ .x$district_id %||% NA_character_),
    match_level = purrr::map_chr(results, ~ .x$match_level %||% NA_character_),
    match_score = purrr::map_dbl(results, ~ as.double(.x$match_score %||% NA_real_))
  )

  # --- Add Government/Opposition Status & Minister Inference ---
  # Le parti au pouvoir a chaque generale vient des legislatures de `periods`.
  elections <- .elections_depuis_periods(bornes)
  if (nrow(elections)) {
    # 1. Determine Government Party for each event
    evt_dates <- as.Date(corpus$event_date)
    idx <- findInterval(evt_dates, elections$election_date)
    gov_parties <- rep(NA_character_, n)
    valid_idx <- idx > 0
    gov_parties[valid_idx] <- elections$gov_party_id[idx[valid_idx]]

    # 2. Infer Party for Unidentified Ministers
    # If we didn't match a name, but the speaker title is clearly a Minister,
    # they belong to the government party.
    minister_regex <- "^(M\\.|Mme|Le|La)?\\s*(le|la)?\\s*(premier|première)?\\s*ministre\\b"
    is_unmatched <- out$match_level %in% c("unmatched", "role", "empty")
    is_minister_title <- grepl(minister_regex, corpus$speaker, ignore.case = TRUE)
    to_infer <- is_unmatched & is_minister_title & !is.na(gov_parties)
    if (any(to_infer)) {
      out$party_id[to_infer] <- gov_parties[to_infer]
      out$match_level[to_infer] <- "role_inferred"
      out$speaker_category[to_infer] <- "role"
    }

    # 3. Determine Role Status (Government vs Opposition)
    out$role_status <- dplyr::case_when(
      !out$speaker_category %in% c("person", "role") ~ NA_character_,
      is.na(out$party_id) ~ NA_character_,
      is.na(gov_parties) ~ NA_character_,
      out$party_id == gov_parties ~ "Government",
      TRUE ~ "Opposition"
    )
  } else {
    warning("Aucun parti au pouvoir dans `annuaire$periods` : role_status et l'inference des ministres sont ignores.")
    out$role_status <- NA_character_
  }

  # Bind with original corpus
  result_df <- dplyr::bind_cols(corpus, out)
  return(result_df)
}

#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
