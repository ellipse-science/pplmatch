# Test de reference du modele des mandats — cas Arthabaska.
#
# POURQUOI ARTHABASKA. Ce seul siege contient trois des quatre cas que le
# modele doit tenir : defection (Lefebvre quitte la CAQ le 2024-04-16),
# demission (il quitte l'Assemblee le 2025-03-18), et partielle (Boissonneault,
# PQ, elu le 2025-08-11). Laporte fournit le quatrieme : le renommage en
# Pierre-Laporte le 2025-05-29, qui ne doit RIEN changer a l'appartenance.
#
# CE QUE CE TEST VERROUILLE. Le modele precedent etait indexe par
# CIRCONSCRIPTION : la defection de Lefebvre etait donc heritee par son
# successeur, et pplmatch attribuait « IND » a un depute peequiste. Le test
# « le successeur n'herite pas » echoue sur cette conception, et passe sur
# celle-ci. C'est le test qui justifie le changement de schema.
#
# Spec : docs/spec-mandats-qc.md

lire_ref <- function(f) {
  chemin <- system.file("extdata", f, package = "pplmatch")
  if (!nzchar(chemin)) chemin <- file.path("..", "..", "inst", "extdata", f)
  d <- utils::read.csv(chemin, stringsAsFactors = FALSE, colClasses = "character")
  d$date_start <- as.Date(d$date_start)
  # une fin vide = mandat toujours en cours ; on la borne au futur lointain
  # pour que les comparaisons restent des comparaisons de dates.
  d$date_end <- ifelse(is.na(d$date_end) | d$date_end == "", "9999-12-31", d$date_end)
  d$date_end <- as.Date(d$date_end)
  d
}

MANDATS <- lire_ref("reference_mandats_arthabaska.csv")
SIEGES  <- lire_ref("reference_sieges_arthabaska.csv")

# Resolution : qui siege, sous quelle banniere, a cette date.
parti_a <- function(seat, quand, mandats = MANDATS) {
  quand <- as.Date(quand)
  m <- mandats[mandats$seat_id == seat &
                 mandats$date_start <= quand &
                 quand <= mandats$date_end, ]
  if (nrow(m) == 0) return(list(person = NA_character_, party = "VACANT"))
  list(person = m$person_id[1], party = m$party_id[1])
}

nom_siege_a <- function(seat, quand, sieges = SIEGES) {
  quand <- as.Date(quand)
  s <- sieges[sieges$seat_id == seat &
                sieges$date_start <= quand & quand <= sieges$date_end, ]
  if (nrow(s) == 0) NA_character_ else s$name[1]
}


test_that("Arthabaska : les trois cas se resolvent correctement", {
  expect_equal(parti_a("arthabaska", "2023-05-01")$party, "CAQ")   # elu
  expect_equal(parti_a("arthabaska", "2024-09-01")$party, "IND")   # defection
  expect_equal(parti_a("arthabaska", "2025-05-01")$party, "VACANT") # demission
  expect_equal(parti_a("arthabaska", "2025-10-01")$party, "PQ")    # partielle
})

test_that("le successeur n'herite PAS de la defection de son predecesseur", {
  # Le bug que ce schema existe pour rendre impossible : avec une cle par
  # circonscription, Arthabaska restait « IND » apres l'arrivee de
  # Boissonneault, elu sous la banniere du Parti quebecois.
  apres <- parti_a("arthabaska", "2025-10-01")
  expect_equal(apres$party, "PQ")
  expect_equal(apres$person, "99001")
  expect_false(identical(apres$person, "17845"))
})

test_that("un renommage de circonscription ne touche pas a l'appartenance", {
  expect_equal(nom_siege_a("laporte", "2025-01-15"), "Laporte")
  expect_equal(nom_siege_a("laporte", "2025-06-15"), "Pierre-Laporte")
  # meme personne, meme parti, de part et d'autre du renommage
  expect_equal(parti_a("laporte", "2025-01-15")$party, "CAQ")
  expect_equal(parti_a("laporte", "2025-06-15")$party, "CAQ")
  expect_equal(parti_a("laporte", "2025-01-15")$person,
               parti_a("laporte", "2025-06-15")$person)
})


# ── Invariants (spec § 5) ────────────────────────────────────────────────────

chevauchements <- function(mandats) {
  pbs <- character(0)
  for (s in unique(mandats$seat_id)) {
    m <- mandats[mandats$seat_id == s, ]
    if (nrow(m) < 2) next
    for (i in seq_len(nrow(m) - 1)) {
      for (j in (i + 1):nrow(m)) {
        if (m$date_start[i] <= m$date_end[j] && m$date_start[j] <= m$date_end[i]) {
          pbs <- c(pbs, sprintf("%s : %s..%s et %s..%s", s,
                                m$date_start[i], m$date_end[i],
                                m$date_start[j], m$date_end[j]))
        }
      }
    }
  }
  pbs
}

test_that("invariant 1 — aucun chevauchement de mandats sur un meme siege", {
  expect_length(chevauchements(MANDATS), 0)
})

test_that("invariant 1 — le chevauchement est bel et bien DETECTE", {
  # Un invariant qu'on n'a jamais vu echouer ne prouve rien. On injecte ici
  # l'erreur reelle de la table deployee : Arthabaska laisse « IND » ouvert
  # alors que Boissonneault occupe le siege depuis aout 2025.
  faux <- MANDATS
  faux$date_end[faux$seat_id == "arthabaska" & faux$party_id == "IND"] <-
    as.Date("9999-12-31")
  expect_gt(length(chevauchements(faux)), 0)
})

test_that("invariant 4 — toute defection est suivie d'une ligne pour la meme personne", {
  d <- MANDATS[MANDATS$end_reason == "defection", ]
  for (i in seq_len(nrow(d))) {
    suite <- MANDATS[MANDATS$person_id == d$person_id[i] &
                       MANDATS$seat_id == d$seat_id[i] &
                       MANDATS$date_start > d$date_end[i], ]
    expect_gt(nrow(suite), 0)
  }
})

test_that("invariant 5 — toute ligne porte une source non vide", {
  expect_true(all(nzchar(MANDATS$source)))
  expect_true(all(nzchar(SIEGES$seat_id)))
})

test_that("le niveau de confiance est declare et connu", {
  expect_true(all(MANDATS$confidence %in%
                    c("verified", "single_source", "disputed")))
})
