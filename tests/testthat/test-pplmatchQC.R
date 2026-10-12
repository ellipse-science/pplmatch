test_that("pplmatchQC returns expected columns", {
  skip_if_not(reticulate::py_available(), "Python not available")

  corpus <- data.frame(
    speaker = c("François Legault", "Le Président", "Des Voix", "Massé"),
    event_date = as.Date(c("2020-06-15", "2020-06-15", "2020-06-15", "2020-06-15")),
    text = c("Bonjour", "Oui", "Oui!", "Merci"),
    stringsAsFactors = FALSE
  )

  members <- data.frame(
    full_name = c("François Legault", "Manon Massé"),
    party_id = c("CAQ", "QS"),
    gender = c("m", "f"),
    legislature_id = c("42", "42"),
    other_names = c(NA, NA),
    stringsAsFactors = FALSE
  )

  result <- pplmatchQC(corpus, annuaire_test(), members, fuzzy_threshold = 85)

  # Same number of rows

  expect_equal(nrow(result), 4)

  # Expected columns exist
  expected_cols <- c("speaker", "event_date", "text",
                     "speaker_category", "speaker_normalized",
                     "legislature", "matched_name", "party_id",
                     "gender", "district_id", "match_level", "match_score")
  expect_true(all(expected_cols %in% names(result)))

  # Person matched
  legault_row <- result[result$speaker == "François Legault", ]
  expect_equal(legault_row$speaker_category, "person")
  expect_equal(legault_row$matched_name, "François Legault")
  expect_equal(legault_row$party_id, "CAQ")

  # Role tagged
  pres_row <- result[result$speaker == "Le Président", ]
  expect_equal(pres_row$speaker_category, "role")
  expect_true(is.na(pres_row$matched_name))

  # Crowd tagged
  voix_row <- result[result$speaker == "Des Voix", ]
  expect_equal(voix_row$speaker_category, "crowd")
  expect_true(is.na(voix_row$matched_name))

  # Last name matched
  masse_row <- result[result$speaker == "Massé", ]
  expect_equal(masse_row$matched_name, "Manon Massé")
})

test_that("pplmatchQC filters by legislature", {
  skip_if_not(reticulate::py_available(), "Python not available")

  corpus <- data.frame(
    speaker = c("Legault", "Legault"),
    event_date = as.Date(c("2020-06-15", "2023-06-15")),
    stringsAsFactors = FALSE
  )

  members <- data.frame(
    full_name = c("François Legault", "François Legault"),
    party_id = c("CAQ", "CAQ"),
    gender = c("m", "m"),
    legislature_id = c("42", "43"),
    stringsAsFactors = FALSE
  )

  result <- pplmatchQC(corpus, annuaire_test(), members)

  # Both should match
  expect_equal(result$legislature[1], 42L)
  expect_equal(result$legislature[2], 43L)
  expect_true(all(!is.na(result$matched_name)))
})
