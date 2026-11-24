test_that("Python legislature module maps dates correctly", {
  skip_if_not(reticulate::py_available(), "Python not available")
  leg_mod <- .load_legislature()

  leg_path <- chemins_annuaire_test()$legislatures
  legislatures <- leg_mod$load_legislatures(leg_path)

  # Legislature 42: 2018-10-01 to 2022-08-28
  expect_equal(leg_mod$date_to_legislature("2020-06-15", legislatures), 42L)

  # Legislature 43: 2022-10-03 onwards
  expect_equal(leg_mod$date_to_legislature("2023-01-15", legislatures), 43L)

  # Legislature 35: 1994-09-12 to 1998-11-25
  expect_equal(leg_mod$date_to_legislature("1995-03-01", legislatures), 35L)

  # Legislature 34: 1989-09-25 to 1994-07-24. Ajoutee pour que les partielles
  # de fevrier 1994 (Bonaventure, Shefford) cessent d'etre ecartees faute de
  # bornes : sans legislature, leur mandat prenait une fin lointaine et
  # chevauchait tous les mandats suivants du meme siege.
  expect_equal(leg_mod$date_to_legislature("1990-01-01", legislatures), 34L)

  # Before any legislature in scope
  res <- leg_mod$date_to_legislature("1985-01-01", legislatures)
  expect_null(res)

  # Between legislatures (gap)
  res <- leg_mod$date_to_legislature("1998-11-28", legislatures)
  expect_null(res)
})

# pplmatch#11 : la liste des législatures était écrite en dur (35:43), et la
# 44e (élue le 2026-10-05) en serait restée exclue. Elle vient maintenant de
# `periods` : une législature déclarée dans la dimension est prise par défaut,
# sans toucher au code.
avec_44e <- function() {
  a <- annuaire_test()
  ligne <- a$periods[a$periods$period_type == "legislature", ][1, ]
  ligne[] <- NA
  ligne$period_type <- "legislature"; ligne$legislature <- "44"
  ligne$date_start <- "2026-10-05"; ligne$date_end <- ""
  ligne$gov_party_id <- "PQ"; ligne$total_seats <- "127"
  a$periods <- rbind(a$periods, ligne)
  a
}

test_that("une législature déclarée dans periods est retenue par défaut (pplmatch#11)", {
  bornes <- pplmatch:::.bornes_legislatures(avec_44e()$periods)
  expect_true(44L %in% bornes$legislature)
  expect_equal(bornes$start_date[bornes$legislature == 44L], as.Date("2026-10-05"))
})

test_that("une intervention de la 44e est rattachée à la 44e, sans liste écrite en dur", {
  skip_if_not(reticulate::py_available(), "Python not available")
  corpus <- data.frame(speaker = "Legault", event_date = as.Date("2026-11-04"),
                       stringsAsFactors = FALSE)
  members <- data.frame(full_name = "François Legault", party_id = "CAQ", gender = "m",
                        legislature_id = "44", stringsAsFactors = FALSE)
  result <- pplmatchQC(corpus, avec_44e(), members)
  expect_equal(result$legislature[1], 44L)
})
