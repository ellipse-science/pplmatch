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
