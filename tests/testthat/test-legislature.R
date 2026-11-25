test_that("Python legislature module maps dates correctly", {
  skip_if_not(reticulate::py_available(), "Python not available")
  leg_mod <- .load_legislature()

  leg_path <- file.path(.find_extdata_dir(), "legislatures_qc.json")
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

# La 44e (élue le 2026-10-05, première séance le 2026-12-01). Sans elle, toute
# intervention de la nouvelle Assemblée tombait hors des législatures
# couvertes : `unmatched`, donc jetée en aval par le raffineur (pplmatch#11).
test_that("une intervention de la 44e est rattachée à la 44e (pplmatch#11)", {
  skip_if_not(reticulate::py_available(), "Python not available")
  corpus <- data.frame(speaker = "St-Pierre Plamondon", event_date = as.Date("2026-12-01"),
                       stringsAsFactors = FALSE)
  result <- pplmatchQC(corpus)
  expect_equal(result$legislature[1], 44L)
  expect_false(result$match_level[1] == "unmatched")
  expect_equal(result$party_id[1], "PQ")
})

test_that("les législatures viennent de legislatures_qc.json, plus d'une liste écrite en dur", {
  legs <- jsonlite::fromJSON(file.path(pplmatch:::.find_extdata_dir(), "legislatures_qc.json"))
  expect_true(44L %in% legs$legislature)
  expect_null(formals(pplmatchQC)$legislatures)
})

test_that("deux homonymes de la 44e sont départagés par leur circonscription (pplmatch#11)", {
  skip_if_not(reticulate::py_available(), "Python not available")
  corpus <- data.frame(
    speaker = c("Caron, La Pinière", "Caron, Ungava", "Poulin, Côte-du-Sud", "Poulin, Beauce-Sud",
                "Simard, Dubuc", "Simard, Charlevoix–Côte-de-Beaupré"),
    event_date = as.Date("2026-12-01"), stringsAsFactors = FALSE)
  result <- pplmatchQC(corpus)
  expect_equal(result$party_id, c("PLQ", "PQ", "PCQ", "PCQ", "PQ", "PCQ"))
  expect_false(any(result$match_level == "unmatched"))
})
