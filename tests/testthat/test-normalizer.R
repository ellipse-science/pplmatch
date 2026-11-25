test_that("Python normalizer classifies speakers correctly", {
  skip_if_not(reticulate::py_available(), "Python not available")
  norm <- .load_normalizer()

  # Persons
  res <- norm$classify_speaker("Legault")
  expect_equal(res, "person")

  res <- norm$classify_speaker("M. Picard")
  expect_equal(res, "person")

  res <- norm$classify_speaker("Mme Soucy")
  expect_equal(res, "person")

  # Roles
  res <- norm$classify_speaker("Le Président")
  expect_equal(res, "role")

  res <- norm$classify_speaker("La Vice-Présidente")
  expect_equal(res, "role")

  # Crowds
  res <- norm$classify_speaker("Des Voix")
  expect_equal(res, "crowd")

  # Empty
  res <- norm$classify_speaker("")
  expect_equal(res, "empty")

  res <- norm$classify_speaker("   ")
  expect_equal(res, "empty")
})

test_that("Python normalizer cleans person names", {
  skip_if_not(reticulate::py_available(), "Python not available")
  norm <- .load_normalizer()

  # Simple last name
  res <- norm$normalize_speaker("Legault")
  expect_equal(res[[1]], "person")
  expect_equal(res[[2]], "legault")

  # With honorific
  res <- norm$normalize_speaker("M. Picard")
  expect_equal(res[[2]], "picard")

  res <- norm$normalize_speaker("Mme Soucy")
  expect_equal(res[[2]], "soucy")

  # Glued honorific (M.Caire)
  res <- norm$normalize_speaker("M.Caire")
  expect_equal(res[[2]], "caire")

  # Accented name
  res <- norm$normalize_speaker("Bérubé")
  expect_equal(res[[2]], "berube")

  # Full name with accents
  res <- norm$normalize_speaker("François Legault")
  expect_equal(res[[2]], "francois legault")

  # Leading numeric garbage
  res <- norm$normalize_speaker("15 725 M. Marissal")
  expect_equal(res[[2]], "marissal")

  # Trailing district
  res <- norm$normalize_speaker("Picard, Chauveau")
  expect_equal(res[[2]], "picard")

  # Trailing action
  res <- norm$normalize_speaker("Legault (réplique)")
  expect_equal(res[[2]], "legault")
})

test_that("Python normalizer handles member names", {
  skip_if_not(reticulate::py_available(), "Python not available")
  norm <- .load_normalizer()

  res <- norm$normalize_member_name("François Legault")
  expect_equal(res, "francois legault")

  res <- norm$normalize_member_name("Marie-Victorin Hébert")
  expect_equal(res, "marievictorin hebert")
})

test_that("Python normalizer extracts last names", {
  skip_if_not(reticulate::py_available(), "Python not available")
  norm <- .load_normalizer()

  expect_equal(norm$extract_last_name("francois legault"), "legault")
  expect_equal(norm$extract_last_name("legault"), "legault")
  expect_equal(norm$extract_last_name(""), "")
})

# La 44e compte vingt homonymes, contre deux dans la 43e. Le Journal des débats
# les départage par la circonscription (« Simard, Dubuc »). Celles à trait
# d'union ou à tiret n'étaient pas lues : l'intervention sortait unmatched,
# donc jetée en aval.
test_that("une circonscription à trait d'union ou à tiret est lue", {
  skip_if_not(reticulate::py_available(), "Python not available")
  norm <- reticulate::import_from_path("normalizer", path = file.path("..", "..", "inst", "python"))
  res <- norm$normalize_speaker("Poulin, Côte-du-Sud")
  expect_equal(res[[2]], "poulin")
  expect_equal(res[[3]], "cotedusud")
  res <- norm$normalize_speaker("Simard, Charlevoix–Côte-de-Beaupré")
  expect_equal(res[[2]], "simard")
  expect_equal(res[[3]], "charlevoixcotedebeaupre")
})
