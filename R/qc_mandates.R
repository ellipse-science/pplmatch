# Complete le repertoire des deputes avec les personnes que SEULS les mandats
# connaissent.
#
# POURQUOI. Le repertoire historique d'origine s'arretait au 2025-03-17 et ne
# couvrait que 121 des 125 sieges de la 43e legislature. Un depute absent du repertoire n'est
# pas mal apparie : il est `unmatched`, donc JETE par le raffineur en aval. Sa
# parole disparait au lieu d'etre comptee — Arthabaska rendait zero ligne.
#
# CE QU'ON N'INVENTE PAS. `gender` n'existe pas dans les tables de mandats : il
# reste NA plutot que d'etre devine. On n'ajoute que ce qui manque, et jamais
# on ne remplace une ligne existante du repertoire.
.completer_membres <- function(members, legislatures_bornes, mandats, personnes) {
  if (is.null(mandats) || is.null(personnes) || !nrow(mandats)) return(members)
  mandats <- as.data.frame(mandats, stringsAsFactors = FALSE)
  personnes <- as.data.frame(personnes, stringsAsFactors = FALSE)
  mandats$date_start <- as.Date(mandats$date_start)

  # Deux noms fautifs sont inscrits dans le repertoire historique de la 43e
  # legislature. Les corriger avant de completer le repertoire evite de garder
  # a la fois l'ancienne graphie et la fiche ANQ stable comme candidates.
  # Les alias historiques restent portes par la ligne corrigee.
  reconciliations <- c(
    "43\rkariana bourassa" = "kariane bourassa",
    "43\rvalerie setlakwe" = "michelle setlakwe"
  )
  cles_membres <- paste(members$legislature_id, tolower(members$full_name), sep = "\r")
  a_corriger <- match(cles_membres, names(reconciliations))
  trouve <- !is.na(a_corriger)
  members$full_name[trouve] <- unname(reconciliations[a_corriger[trouve]])

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
