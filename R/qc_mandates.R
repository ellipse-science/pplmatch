#' Charger la table des mandats dates du Quebec
#'
#' Lit \code{mandates_qc.csv} : un mandat par personne, par siege et par
#' intervalle de dates. C'est la table qui remplace \code{party_changes_qc.csv}
#' pour resoudre l'appartenance politique.
#'
#' La difference tient a la borne de fin. Un changement de parti sans date de
#' fin s'applique indefiniment, donc au SUCCESSEUR du transfuge : la defection
#' d'Eric Lefebvre (Arthabaska, 2024-04-16) etait heritee par Alex
#' Boissonneault, elu peequiste a la partielle du 2025-08-11. Un intervalle
#' ferme ne peut pas deborder sur le suivant.
#'
#' @return Un tibble : \code{person_id}, \code{seat_id}, \code{party_id},
#'   \code{parliamentary_status}, \code{date_start}, \code{date_end},
#'   \code{start_reason}, \code{end_reason}, \code{source}, \code{confidence}.
#'
#' @seealso \code{\link{qc_members}}, \code{\link{pplmatchQC}}
#' @export
qc_mandates <- function() {
  chemin <- .fichier_extdata("mandates_qc.csv")
  d <- utils::read.csv(chemin, stringsAsFactors = FALSE, colClasses = "character")
  d$date_start <- as.Date(d$date_start)
  # Une fin vide = mandat en cours. On la borne au futur lointain pour que les
  # comparaisons restent des comparaisons de dates, jamais des NA silencieux.
  d$date_end <- ifelse(is.na(d$date_end) | d$date_end == "", "9999-12-31", d$date_end)
  d$date_end <- as.Date(d$date_end)
  tibble::as_tibble(d)
}


#' Charger la table des personnes
#'
#' @return Un tibble : \code{person_id}, \code{full_name}, \code{other_names},
#'   \code{assnat_url}.
#' @export
qc_persons <- function() {
  tibble::as_tibble(utils::read.csv(
    .fichier_extdata("persons_qc.csv"),
    stringsAsFactors = FALSE, colClasses = "character"))
}


# Trouve un fichier de donnees, que le package soit installe ou charge par
# devtools::load_all() depuis la racine du depot.
.fichier_extdata <- function(nom) {
  chemin <- system.file("extdata", nom, package = "pplmatch")
  if (!nzchar(chemin)) chemin <- file.path("inst", "extdata", nom)
  if (!file.exists(chemin)) {
    stop(sprintf("Fichier de donnees introuvable : %s. Le package est-il installe ?", nom),
         call. = FALSE)
  }
  chemin
}


# Complete le repertoire des deputes avec les personnes que SEULS les mandats
# connaissent.
#
# POURQUOI. `members_historic_qc.csv` s'arrete au 2025-03-17 et ne couvre que
# 121 des 125 sieges de la 43e legislature. Un depute absent du repertoire n'est
# pas mal apparie : il est `unmatched`, donc JETE par le raffineur en aval. Sa
# parole disparait au lieu d'etre comptee — Arthabaska rendait zero ligne.
#
# CE QU'ON N'INVENTE PAS. `gender` n'existe pas dans les tables de mandats : il
# reste NA plutot que d'etre devine. On n'ajoute que ce qui manque, et jamais
# on ne remplace une ligne existante du repertoire.
.completer_membres <- function(members, legislatures_bornes) {
  mandats <- tryCatch(qc_mandates(), error = function(e) NULL)
  personnes <- tryCatch(qc_persons(), error = function(e) NULL)
  if (is.null(mandats) || is.null(personnes) || !nrow(mandats)) return(members)

  # `match()` plutot que `[[` : un identifiant absent rend NA, la ou `[[` leve
  # « subscript out of bounds ». Les mandats en portent — les partielles
  # anciennes n'ont pas de titulaire connu.
  idx <- match(mandats$person_id, personnes$person_id)
  mandats$full_name <- personnes$full_name[idx]
  mandats$other_names <- personnes$other_names[idx]
  mandats <- mandats[!is.na(mandats$full_name) & nzchar(mandats$full_name), ]
  if (!nrow(mandats)) return(members)

  # A quelle legislature appartient un mandat ? Celle dont les bornes
  # contiennent son debut. Un mandat qui n'en recoupe aucune est ignore.
  leg_de <- function(d) {
    i <- vapply(d, function(x) {
      k <- which(legislatures_bornes$start_date <= x & x <= legislatures_bornes$end_date)
      if (length(k)) k[1] else NA_integer_
    }, integer(1))
    as.character(legislatures_bornes$legislature[i])
  }
  mandats$legislature_id <- leg_de(mandats$date_start)
  mandats <- mandats[!is.na(mandats$legislature_id), ]
  if (!nrow(mandats)) return(members)

  ajouts <- data.frame(
    full_name = mandats$full_name, party_id = mandats$party_id,
    gender = NA_character_, legislature_id = mandats$legislature_id,
    other_names = mandats$other_names, district_id = mandats$seat_id,
    stringsAsFactors = FALSE)

  # On n'ajoute QUE ce qui manque : une personne deja connue pour cette
  # legislature garde sa ligne d'origine, avec son `gender` et ses variantes.
  connu <- paste(members$legislature_id, members$full_name)
  ajouts <- unique(ajouts[!(paste(ajouts$legislature_id, ajouts$full_name) %in% connu), ])
  if (!nrow(ajouts)) return(members)

  # Un appelant peut fournir son PROPRE `members`, avec moins de colonnes que
  # le repertoire livre — `members[, names(ajouts)]` levait alors « undefined
  # columns selected ». On aligne les deux cotes sur l'union des colonnes,
  # en NA pour ce qui manque, plutot que d'imposer un schema a l'appelant.
  for (col in setdiff(names(ajouts), names(members))) members[[col]] <- NA_character_
  for (col in setdiff(names(members), names(ajouts))) ajouts[[col]] <- NA_character_
  rbind(members, ajouts[, names(members), drop = FALSE])
}
