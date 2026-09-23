# Anomalies de la 43e legislature corrigees le 2026-09-23 (aws-refiners#547,
# cartes de depute). Chaque cas est source : chronologie ANQ ou fiche du membre.

lire <- function(f) {
  chemin <- system.file("extdata", f, package = "pplmatch")
  if (!nzchar(chemin)) chemin <- file.path("..", "..", "inst", "extdata", f)
  utils::read.csv(chemin, stringsAsFactors = FALSE, colClasses = "character")
}
M <- lire("mandates_qc.csv")
parti_le <- function(pid, jour) {
  m <- M[M$person_id == pid & M$date_start <= jour & M$date_end >= jour, ]
  unique(m$party_id)
}

test_that("Youri Chassin : elu CAQ, independant a partir du 12 septembre 2024 (chrono114)", {
  expect_equal(parti_le("17881", "2023-03-01"), "CAQ")
  expect_equal(parti_le("17881", "2024-09-11"), "CAQ")
  expect_equal(parti_le("17881", "2024-09-12"), "IND")
})

test_that("Marie-Claude Nichols : exclue du caucus le 27 octobre 2022, reintegree le 19 juin 2025", {
  expect_equal(parti_le("15439", "2022-10-26"), "PLQ")
  expect_equal(parti_le("15439", "2022-10-27"), "IND")
  expect_equal(parti_le("15439", "2025-06-19"), "PLQ")
})

test_that("Dominique Anglade : mandat clos a sa demission du 1er decembre 2022 (fiche)", {
  a <- M[M$person_id == "16499" & M$date_start == "2022-10-03", ]
  expect_equal(a$date_end, "2022-12-01")
  expect_equal(parti_le("16499", "2023-01-15"), character(0))
})

test_that("Eric Girard (Groulx) a sa fiche", {
  P <- lire("persons_qc.csv")
  expect_equal(P$assnat_url[P$person_id == "17929"], "/fr/deputes/girard-eric-17929/index.html")
})
