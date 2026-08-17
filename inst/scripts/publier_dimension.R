#!/usr/bin/env Rscript
# Publie les tables de mandats comme DIMENSION dans l'infrastructure Ellipse.
#
# POURQUOI UNE DIMENSION, ET PAS UN FICHIER DE PLUS
# -------------------------------------------------
# `dim-qc-parliament-members` existe deja et n'est pas une dimension : `version`
# vaut 43 pour toutes ses lignes, `start_date` vaut la generale pour toutes, et
# `end_date` est vide. C'est un INSTANTANE deguise. Nos tables sont ce que
# celle-la pretend etre : une SCD-2, avec un intervalle ferme par mandat.
#
# POURQUOI `addto_or_replace_table = 2`
# --------------------------------------
# L'append (1) est structural pour les tables d'EVENEMENTS : il donne une
# memoire anti-bruit. Une dimension, elle, est regeneree en entier a chaque
# passe — l'append y empilerait des versions successives du meme mandat et
# rendrait toute lecture ambigue. On ecrase.
#
# CE QUE CE SCRIPT REFUSE DE FAIRE
# ---------------------------------
# Publier une dimension partagee, c'est diffuser ses erreurs a tous les
# consommateurs d'un coup, et une donnee fausse y coute bien plus cher a
# rattraper qu'une donnee fausse dans un paquet que personne ne lit encore. Le
# script re-verifie donc les invariants AVANT d'ecrire, et s'arrete si l'un
# d'eux echoue. Il refuse aussi PROD tant qu'on ne l'a pas nomme explicitement.
#
# USAGE
#   Rscript inst/scripts/publier_dimension.R                 # verifie, n'ecrit rien
#   Rscript inst/scripts/publier_dimension.R --env DEV --go  # publie en DEV
#   Rscript inst/scripts/publier_dimension.R --env PROD --go  # apres revue seulement
#
# Prealable : `aws sso login --profile ellipse-dev` (session interactive).

args <- commandArgs(trailingOnly = TRUE)
env <- if ("--env" %in% args) args[which(args == "--env") + 1L] else "DEV"
go <- "--go" %in% args

# Le nom calque la convention des dimensions de l'entrepot —
# `dim-qc-parliament-members`, `dim-ca-parliament-members`, `dim-medias` — en
# gardant les tirets bas que la couche datamart emploie. Le nom complet d'une
# table est `<datamart>-<table>`, donc on lit `dim_qc_parliament-mandates` :
# une dimension, du Quebec, du parlement, des mandats. « pplmatch » ne disait
# que le nom de l'outil qui l'a produite, pas ce que la table contient.
DATAMART <- "dim_qc_parliament"
TABLES <- c(mandates = "Mandats dates : une personne x un siege x un intervalle ferme (SCD-2).",
            persons  = "Identites des parlementaires quebecois et variantes de graphie.",
            seats    = "Circonscriptions et leurs noms successifs dans le temps.")

msg <- function(...) cat(..., "\n", sep = "")

# ── 1. Charger ──────────────────────────────────────────────────────────────
# On remonte depuis le repertoire courant jusqu'a trouver inst/extdata, pour
# que le script marche depuis la racine du depot comme depuis inst/scripts.
racine <- getwd()
for (i in 1:4) {
  if (dir.exists(file.path(racine, "inst", "extdata"))) break
  racine <- dirname(racine)
}
if (!dir.exists(file.path(racine, "inst", "extdata"))) {
  stop("inst/extdata introuvable — lancer depuis le depot pplmatch.", call. = FALSE)
}
lire <- function(f) utils::read.csv(file.path(racine, "inst", "extdata", f),
                                    stringsAsFactors = FALSE, colClasses = "character")

mandats <- lire("mandates_qc.csv")
personnes <- lire("persons_qc.csv")
sieges <- lire("seats_qc.csv")
msg("mandats ", nrow(mandats), " | personnes ", nrow(personnes),
    " | sieges ", nrow(sieges))

# ── 2. Les gardes ───────────────────────────────────────────────────────────
# On ne fait pas confiance a une verification faite ailleurs, il y a peut-etre
# des jours : on refait celles qui comptent, ici, sur ce qu'on s'apprete a
# ecrire.
fautes <- character(0)
d0 <- as.Date(mandats$date_start)
d1 <- as.Date(ifelse(is.na(mandats$date_end) | mandats$date_end == "",
                     "9999-12-31", mandats$date_end))

if (any(d1 < d0)) fautes <- c(fautes, "des intervalles finissent avant de commencer")
if (any(grepl("[^a-z0-9]", mandats$seat_id))) fautes <- c(fautes, "des seat_id mal formes")
if (any(!nzchar(mandats$source))) fautes <- c(fautes, "des mandats sans source")

orphelins <- setdiff(mandats$person_id[nzchar(mandats$person_id)], personnes$person_id)
if (length(orphelins)) fautes <- c(fautes, sprintf(
  "%d mandat(s) pointent vers une personne inexistante", length(orphelins)))

# Le plafond : l'Assemblee compte 125 sieges. Un depassement signale deux
# identifiants pour un meme siege physique, et c'est invisible au controle de
# non-chevauchement, qui compare des identifiants.
pic <- max(vapply(unique(d0), function(j) sum(d0 <= j & j <= d1), integer(1)))
if (pic > 125) fautes <- c(fautes, sprintf("plafond depasse : %d mandats simultanes", pic))
msg("pic de mandats simultanes : ", pic, " / 125")

# Chevauchements par siege.
ch <- 0L
for (s in unique(mandats$seat_id)) {
  i <- which(mandats$seat_id == s)
  o <- i[order(d0[i])]
  if (length(o) > 1) ch <- ch + sum(d1[o[-length(o)]] >= d0[o[-1]])
}
if (ch > 0) fautes <- c(fautes, sprintf("%d chevauchement(s) de mandats", ch))

conf <- table(mandats$confidence)
msg("confiance : ", paste(sprintf("%s=%d", names(conf), conf), collapse = " | "))

if (length(fautes)) {
  msg("\nREFUS DE PUBLIER :")
  for (f in fautes) msg("  - ", f)
  msg("\nUne dimension partagee diffuse ses erreurs a tous ses consommateurs ",
      "d'un coup. On corrige avant, pas apres.")
  quit(status = 1)
}
msg("gardes : toutes passees")

# ── 3. Publier ──────────────────────────────────────────────────────────────
if (!go) {
  msg("\n--go absent : rien n'a ete publie.")
  msg("Pour publier en DEV : Rscript inst/scripts/publier_dimension.R --env DEV --go")
  quit(status = 0)
}
if (!env %in% c("DEV", "PROD")) stop("--env doit valoir DEV ou PROD", call. = FALSE)
if (env == "PROD") {
  msg("\nPROD demande. Rappel : la PR doit etre relue et fusionnee avant.")
}

con <- tube::ellipse_connect(env, "datamarts")
on.exit(try(tube::ellipse_disconnect(con), silent = TRUE), add = TRUE)

for (nom in names(TABLES)) {
  df <- get(switch(nom, mandates = "mandats", persons = "personnes",
                   seats = "sieges"))
  msg("publication de ", nom, " (", nrow(df), " lignes) vers ", env, "...")
  tube::ellipse_publish(
    con = con,
    dataframe = df,
    datamart = DATAMART,
    table = nom,
    table_description = TABLES[[nom]],
    table_tags = list(app = "radar+", pole = "vitrine", type = "dimension",
                      dimensions = "person,seat,party,date", measures = "aucune",
                      data = "qc-assemblee-nationale"),
    data_tag = format(Sys.time(), "%Y-%m-%d %H:%M"),
    unattended_options = list(
      create_datamart = "oui",
      # 2 = ECRASER. Une dimension est regeneree en entier ; l'append y
      # empilerait des versions successives du meme mandat.
      addto_or_replace_table = 2,
      are_you_sure = "oui", create_table = "oui", process_data = "oui"))
}
msg("\npublie dans ", env, " / datamart ", DATAMART)
msg("Pense a enregistrer le schema Glue si les colonnes ont change — ",
    "un schema non enregistre rend COLUMN_NOT_FOUND en PROD.")
