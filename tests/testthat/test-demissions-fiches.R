# L'ordinal de « 1er » est en exposant sur les fiches : balises retirees, il
# devient « 1 er ». Le motif doit l'accepter (cas de Dominique Anglade).
test_that("une demission datee d'un premier du mois est lue", {
  skip_if_not(reticulate::py_available(), "Python not available")
  mod <- reticulate::import_from_path("build_demissions_fiches",
                                      path = file.path("..", "..", "inst", "python"))
  html <- "<p>Démissionna comme députée le 1<sup>er</sup> décembre 2022. Devint professeure.</p>"
  d <- mod$dates_demission(html)
  expect_equal(d[[1]][[1]], "2022-12-01")
})
