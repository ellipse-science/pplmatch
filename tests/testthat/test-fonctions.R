# Tests de la table FONCTIONS (43e legislature).
#
# CE QUE CES TESTS VERROUILLENT. Les invariants de la table, plus des cas
# connus qu'une regression ferait tomber en silence : les deux Eric Girard,
# un ancien depute transcrit a la main, et la presidence de l'Assemblee (PAN),
# dont le matcher se sert pour attribuer « La Presidente » (test-presidence.R).

lire_ext <- function(f) {
  chemin <- system.file("extdata", f, package = "pplmatch")
  if (!nzchar(chemin)) chemin <- file.path("..", "..", "inst", "extdata", f)
  utils::read.csv(chemin, stringsAsFactors = FALSE, colClasses = "character")
}

FONCTIONS <- lire_ext("functions_qc.csv")
PERSONNES <- lire_ext("persons_qc.csv")

test_that("chaque fonction a un intervalle valide et une personne connue", {
  debut <- as.Date(FONCTIONS$date_start)
  fin <- as.Date(ifelse(FONCTIONS$date_end == "", "9999-12-31", FONCTIONS$date_end))
  expect_false(any(is.na(debut)))
  expect_true(all(fin >= debut))
  expect_true(all(FONCTIONS$person_id %in% PERSONNES$person_id))
})

test_that("une seule presidence de l'Assemblee a la fois", {
  pan <- FONCTIONS[FONCTIONS$function_code == "PAN", ]
  expect_gte(nrow(pan), 1L)
  expect_equal(sum(pan$date_end == ""), 1L)
  expect_true("12187" %in% pan$person_id)
})

test_that("les deux Eric Girard ne sont pas confondus", {
  # Groulx (17929) : ministre des Finances ; Lac-Saint-Jean (17957) : ministre delegue.
  groulx <- FONCTIONS$title[FONCTIONS$person_id == "17929"]
  lac <- FONCTIONS$title[FONCTIONS$person_id == "17957"]
  expect_true("Ministre des Finances" %in% groulx)
  expect_false("Ministre des Finances" %in% lac)
})

test_that("un ancien depute absent des fiches est transcrit", {
  # Andree Laforest (17913), demission le 2025-09-04 : sa page n'est plus
  # qu'une biographie. Sa fonction vient de la transcription.
  laforest <- FONCTIONS[FONCTIONS$person_id == "17913", ]
  expect_true(any(laforest$confidence == "transcribed"))
  expect_true("Ministre des Affaires municipales" %in% laforest$title)
})
