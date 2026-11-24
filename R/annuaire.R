#' Lire l'annuaire des deputes depuis un dossier de CSV
#'
#' pplmatch ne contient plus de donnees : l'annuaire des deputes (la dimension
#' \code{dim-qc-parliament-*} de l'entrepot) lui est fourni par l'appelant.
#' Cette fonction le lit depuis un dossier qui contient les six tables en CSV,
#' tel que le depot \code{dim-qc-parliament} les produit, pour le travail hors
#' AWS et pour les tests. En production, le raffineur lit les memes tables
#' dans l'entrepot par tube et les passe telles quelles.
#'
#' @param dossier Dossier contenant \code{mandates_qc.csv},
#'   \code{persons_qc.csv}, \code{seats_qc.csv}, \code{functions_qc.csv},
#'   \code{election_results_qc.csv} et \code{periods_qc.csv}.
#' @return Une liste nommee de tibbles : \code{mandates}, \code{persons},
#'   \code{seats}, \code{functions}, \code{election_results}, \code{periods}.
#'   Toutes les colonnes sont des chaines ; les dates sont au format
#'   \code{YYYY-MM-DD}, une fin vide signifie « en cours ».
#' @seealso \code{\link{pplmatchQC}}
#' @export
qc_annuaire_csv <- function(dossier) {
  lire <- function(f) {
    chemin <- file.path(dossier, f)
    if (!file.exists(chemin)) stop("Fichier introuvable : ", chemin, call. = FALSE)
    tibble::as_tibble(utils::read.csv(chemin, stringsAsFactors = FALSE, colClasses = "character",
                                      encoding = "UTF-8"))
  }
  list(
    mandates = lire("mandates_qc.csv"),
    persons = lire("persons_qc.csv"),
    seats = lire("seats_qc.csv"),
    functions = lire("functions_qc.csv"),
    election_results = lire("election_results_qc.csv"),
    periods = lire("periods_qc.csv")
  )
}

# Ce que pplmatchQC() exige de l'annuaire. Les tables absentes ou aux colonnes
# manquantes arretent tout de suite, avec le nom de ce qui manque : un
# annuaire incomplet donnerait des `unmatched` silencieux, pas une erreur.
.verifier_annuaire <- function(annuaire) {
  if (!is.list(annuaire)) stop("`annuaire` doit etre une liste de tables (voir qc_annuaire_csv()).", call. = FALSE)
  requis <- list(
    mandates = c("person_id", "seat_id", "party_id", "date_start", "date_end"),
    persons = c("person_id", "full_name", "other_names"),
    functions = c("person_id", "function_code", "date_start", "date_end"),
    periods = c("period_type", "legislature", "date_start", "date_end")
  )
  for (nom in names(requis)) {
    t <- annuaire[[nom]]
    if (is.null(t)) stop("`annuaire$", nom, "` manque.", call. = FALSE)
    manque <- setdiff(requis[[nom]], names(t))
    if (length(manque)) stop("`annuaire$", nom, "` : colonne(s) manquante(s) : ",
                             paste(manque, collapse = ", "), call. = FALSE)
    annuaire[[nom]] <- .dates_en_texte(as.data.frame(t, stringsAsFactors = FALSE))
  }
  annuaire
}

# Les dates circulent en texte YYYY-MM-DD, vide = en cours : c'est ce que le
# matcher Python lit. Un appelant qui a deja converti en Date est accepte.
.dates_en_texte <- function(df) {
  for (col in intersect(c("date_start", "date_end"), names(df))) {
    x <- df[[col]]
    if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
    x <- as.character(x)
    x[is.na(x) | x == "NA"] <- ""
    df[[col]] <- x
  }
  for (col in names(df)) if (!is.character(df[[col]])) df[[col]] <- as.character(df[[col]])
  df
}

# Les bornes des legislatures, depuis la table periods. Une fin vide (la
# legislature en cours) est bornee au futur lointain pour que les comparaisons
# restent des comparaisons de dates.
.bornes_legislatures <- function(periods) {
  p <- periods[periods$period_type == "legislature", ]
  fin <- ifelse(is.na(p$date_end) | p$date_end == "", "9999-12-31", p$date_end)
  data.frame(legislature = as.integer(p$legislature),
             start_date = as.Date(p$date_start), end_date = as.Date(fin),
             gov_party_id = if ("gov_party_id" %in% names(p)) p$gov_party_id else NA_character_,
             stringsAsFactors = FALSE)
}

# Le matcher Python lit des fichiers. On lui ecrit les tables de l'annuaire
# dans un dossier temporaire, dans les formats qu'il connait, et on les efface
# apres l'appel. Ca evite de dupliquer la logique de lecture en Python.
.fichiers_temporaires_annuaire <- function(annuaire) {
  dossier <- tempfile("pplmatch-annuaire-")
  dir.create(dossier)
  bornes <- .bornes_legislatures(annuaire$periods)
  legs <- data.frame(legislature = bornes$legislature,
                     start_date = format(bornes$start_date, "%Y-%m-%d"),
                     end_date = ifelse(bornes$end_date >= as.Date("9999-01-01"), NA_character_,
                                       format(bornes$end_date, "%Y-%m-%d")),
                     stringsAsFactors = FALSE)
  s <- annuaire$periods[annuaire$periods$period_type == "session", ]
  sessions <- data.frame(session_id = as.integer(s$anq_session_id),
                         legislature = as.integer(s$legislature), session = as.integer(s$session),
                         start_date = s$date_start,
                         end_date = ifelse(s$date_end == "", NA_character_, s$date_end),
                         stringsAsFactors = FALSE)
  chemins <- list(
    legislatures = file.path(dossier, "legislatures_qc.json"),
    sessions = file.path(dossier, "sessions_qc.json"),
    mandates = file.path(dossier, "mandates_qc.csv"),
    functions = file.path(dossier, "functions_qc.csv")
  )
  jsonlite::write_json(legs, chemins$legislatures, na = "null", auto_unbox = TRUE)
  jsonlite::write_json(sessions, chemins$sessions, na = "null", auto_unbox = TRUE)
  utils::write.csv(annuaire$mandates, chemins$mandates, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  utils::write.csv(annuaire$functions, chemins$functions, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  chemins$dossier <- dossier
  chemins
}

# Le repertoire des membres, construit entierement depuis les mandats et les
# personnes : une ligne par (personne x legislature), avec le parti du mandat
# et le siege comme circonscription. `gender` reste NA : on ne devine pas.
.membres_depuis_annuaire <- function(annuaire, bornes) {
  vide <- data.frame(full_name = character(0), party_id = character(0), gender = character(0),
                     legislature_id = character(0), other_names = character(0),
                     district_id = character(0), stringsAsFactors = FALSE)
  .completer_membres(vide, bornes, annuaire$mandates, annuaire$persons)
}

# Le parti au pouvoir a chaque generale, depuis les legislatures de periods :
# sert a attribuer un parti aux « ministres » non identifies et a classer
# gouvernement / opposition.
.elections_depuis_periods <- function(bornes) {
  e <- bornes[!is.na(bornes$gov_party_id) & nzchar(bornes$gov_party_id), ]
  e <- e[order(e$start_date), ]
  data.frame(election_date = e$start_date, gov_party_id = e$gov_party_id, stringsAsFactors = FALSE)
}
