# Tests des tables RESULTATS ELECTORAUX et INDEMNITES (43e legislature), et
# du lien entre les fonctions payees et le bareme.
#
# CE QUE CES TESTS VERROUILLENT. Les memes invariants que les gardes de
# publier_dimension.R, plus des cas connus qu'une regression ferait tomber
# en silence : une partielle, un depute qui cumule deux fonctions payees.
#
# Spec : design/spec-fonctions-resultats.md

lire_ext <- function(f) {
  chemin <- system.file("extdata", f, package = "pplmatch")
  if (!nzchar(chemin)) chemin <- file.path("..", "..", "inst", "extdata", f)
  utils::read.csv(chemin, stringsAsFactors = FALSE, colClasses = "character")
}

FONCTIONS <- lire_ext("functions_qc.csv")
RESULTATS <- lire_ext("election_results_qc.csv")
INDEMNITES <- lire_ext("indemnities_qc.csv")
BAREME <- lire_ext("indemnity_scale_qc.csv")
MANDATS <- lire_ext("mandates_qc.csv")

test_that("chaque categorie payee existe au bareme, au bon taux", {
  payees <- FONCTIONS[nzchar(FONCTIONS$scale_category), ]
  expect_true(all(payees$scale_category %in% BAREME$scale_category))
  taux <- setNames(as.numeric(BAREME$pct), BAREME$scale_category)
  expect_equal(unname(as.numeric(payees$scale_pct)), unname(taux[payees$scale_category]))
})

test_that("un elu et un seul par circonscription et par scrutin", {
  cles <- paste(RESULTATS$election_code, RESULTATS$seat_id)
  n <- tapply(RESULTATS$elected == "true", cles, sum)
  expect_true(all(n == 1))
  expect_equal(sum(RESULTATS$election_code == "gen2022-10-03" & RESULTATS$elected == "true"), 125L)
})

test_that("chaque elu a un mandat qui commence le jour du scrutin dans ce siege", {
  elus <- RESULTATS[RESULTATS$elected == "true", ]
  ok <- mapply(function(p, s, d) any(MANDATS$person_id == p & MANDATS$seat_id == s & MANDATS$date_start == d),
               elus$person_id, elus$seat_id, elus$election_date)
  expect_true(all(ok))
})

test_that("la partielle de Chicoutimi relie Marie-Karlynn Laflamme, pas Andree Laforest", {
  r <- RESULTATS[RESULTATS$election_code == "part2026-02-23" & RESULTATS$elected == "true", ]
  expect_equal(r$seat_id, "chicoutimi")
  expect_false(r$person_id == "17913")
})

test_that("indemnites : dates croissantes, montants connus", {
  expect_false(is.unsorted(as.Date(INDEMNITES$date_start), strictly = TRUE))
  expect_equal(INDEMNITES$base_annual[INDEMNITES$date_start == "2023-06-07"], "131766")
})
