# L'etiquette nue du fauteuil (« La Presidente ») est attribuee a la personne
# qui detient la presidence de l'Assemblee ce jour-la (aws-refiners#547).

test_that("La Presidente est attribuee a Nathalie Roy pendant sa presidence", {
  skip_if_not(reticulate::py_available(), "Python not available")
  corpus <- data.frame(
    speaker = c("La Présidente", "12 187 La Présidente", "La Présidente: 485",
                "Le Président", "La Présidente", "La Présidente : j jj",
                "La Vice-Présidente (Mme Soucy)"),
    event_date = as.Date(c("2024-03-12", "2023-02-22", "2024-05-30",
                           "2024-03-12", "2022-11-01", "2023-03-30",
                           "2025-02-27")),
    stringsAsFactors = FALSE
  )
  r <- pplmatchQC(corpus)

  # Les trois graphies de l'etiquette, pendant la presidence
  expect_equal(r$match_level[1:3], rep("presiding_officer", 3))
  expect_equal(tolower(r$matched_name[1:3]), rep("nathalie roy", 3))
  expect_equal(r$district_id[1:3], rep("montarville", 3))
  expect_equal(r$party_id[1:3], rep("CAQ", 3))

  # « Le President » un jour ou la presidente est une femme : non attribue
  expect_true(is.na(r$matched_name[4]))
  # Avant l'ouverture de la legislature (29 novembre 2022) : non attribue
  expect_true(is.na(r$matched_name[5]))
  # Etiquette bruitee par du texte : non attribuee plutot que devinee
  expect_true(is.na(r$matched_name[6]))
  # La vice-presidence n'est pas la presidence
  expect_false(identical(r$match_level[7], "presiding_officer"))
})
