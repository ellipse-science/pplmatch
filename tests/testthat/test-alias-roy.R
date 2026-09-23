# « Mme Roy » dans la 43e legislature : Nathalie Roy (Montarville) et Suzanne
# Roy (Vercheres) siegent toutes deux. L'alias « roy ; mme roy » donne a
# Suzanne Roy lui attribuait d'office les 168 interventions de Nathalie Roy
# (aws-refiners#547, point 3). Sans alias, l'appel au seul patronyme est
# ambigu, et c'est l'en-tete (« Mme Roy (Montarville) : ») qui tranche en aval.

test_that("« Mme Roy » n'est plus attribuee d'office a Suzanne Roy", {
  skip_if_not(reticulate::py_available(), "Python not available")
  corpus <- data.frame(
    speaker = c("Mme Roy", "Suzanne Roy", "Mme Boivin Roy"),
    event_date = as.Date(rep("2024-11-28", 3)),
    stringsAsFactors = FALSE
  )
  r <- pplmatchQC(corpus)
  expect_false(identical(r$match_level[1], "deterministic") && identical(r$matched_name[1], "suzanne roy"))
  expect_equal(r$matched_name[2], "suzanne roy")
  expect_equal(r$matched_name[3], "karine boivin roy")
})
