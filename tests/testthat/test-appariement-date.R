# Test de bout en bout du modele DATE, par l'API publique.
#
# POURQUOI CE FICHIER EXISTE. Les autres tests verifient des morceaux : ce
# fichier-ci appelle `pplmatchQC()` comme un appelant reel, et verifie ce qui
# sort. C'est le seul niveau ou l'on peut affirmer que pplmatch est correct —
# le generateur peut produire des tables parfaites sans que rien ne les
# consomme, et c'etait exactement la situation avant ce changement.
#
# CE QU'IL VERROUILLE, MESURE. Sur les six cas ci-dessous, QUATRE etaient faux
# avant le rebranchement :
#
#   Sklavounos 2017-05-01   PLQ  au lieu de IND
#   LeBel      2021-06-01   PQ   au lieu de IND
#   Boissonneault           unmatched
#   St-Louis                unmatched
#
# La cause des deux premiers n'est pas celle qu'on croit : `party_changes_qc.csv`
# est VIDE (en-tete seul). L'ancien resolveur ne resolvait donc rien, et le
# parti sortait tel quel du repertoire — un parti par LEGISLATURE, incapable de
# changer en cours de mandat. Le mecanisme de defection existait dans le code
# et ne s'appliquait a personne.
#
# Le cas « le successeur n'herite pas », lui, passait DEJA : la clef
# (circonscription, legislature) de l'ancien format faisait garde-fou par
# accident, un changement de la 42e ne pouvant pas atteindre la 43e. Il reste
# teste ici parce que le modele par intervalle doit le garantir par
# CONSTRUCTION plutot que par effet de bord — mais il ne prouve pas la
# regression a lui seul, et on ne le fait pas dire plus que ca.

test_that("le parti depend de la DATE, pas seulement de la legislature", {
  skip_if_not(reticulate::py_available(), "Python not available")

  # Deux defections reelles, chacune verifiee des deux cotes de sa date.
  # Meme personne, meme siege : seule la date change.
  corpus <- data.frame(
    speaker = c("Gerry Sklavounos", "Gerry Sklavounos",
                "Harold LeBel", "Harold LeBel"),
    event_date = c("2016-05-01", "2017-05-01",     # PLQ -> IND le 2016-10-20
                   "2020-06-01", "2021-06-01"),    # PQ  -> IND le 2020-12-15
    stringsAsFactors = FALSE)

  r <- pplmatchQC(corpus)
  expect_equal(r$party_id, c("PLQ", "IND", "PQ", "IND"))
  expect_equal(r$district_id, c("laurierdorion", "laurierdorion",
                                "rimouski", "rimouski"))
  expect_true(all(r$match_level == "deterministic"))
})

test_that("le successeur n'herite PAS de la defection de son predecesseur", {
  skip_if_not(reticulate::py_available(), "Python not available")

  # LE TEST QUI JUSTIFIE LE SCHEMA. Harold LeBel siege independant a Rimouski
  # a partir du 2020-12-15. Sans borne de fin, ce changement vaut pour
  # toujours : Maite Blanchette Vezina, elue caquiste au meme siege en 2022,
  # sortait « IND ». Un depute correctement apparie, avec le mauvais parti.
  r <- pplmatchQC(data.frame(
    speaker = c("Harold LeBel", "Maite Blanchette Vezina"),
    event_date = c("2021-06-01", "2023-05-01"), stringsAsFactors = FALSE))

  expect_equal(r$district_id, c("rimouski", "rimouski"))
  expect_equal(r$party_id, c("IND", "CAQ"))
})

test_that("les sieges absents du referentiel sont desormais apparies", {
  skip_if_not(reticulate::py_available(), "Python not available")

  # `members_historic_qc.csv` s'arrete au 2025-03-17 et ignore 4 des 125
  # sieges de la 43e. Un siege absent n'est pas mal attribue : il est
  # `unmatched`, donc JETE en aval — sa parole disparait au lieu d'etre
  # comptee. Arthabaska rendait litteralement zero ligne.
  r <- pplmatchQC(data.frame(
    speaker = c("Alex Boissonneault", "Francois St-Louis"),
    event_date = c("2025-09-15", "2024-02-01"), stringsAsFactors = FALSE))

  expect_equal(r$district_id, c("arthabaska", "joliette"))
  expect_equal(r$party_id, c("PQ", "CAQ"))
  expect_false(any(r$match_level == "unmatched"))
})

test_that("le resolveur ne rend rien plutot qu'un parti approximatif", {
  skip_if_not(reticulate::py_available(), "Python not available")
  chemin <- system.file("extdata", "mandates_qc.csv", package = "pplmatch")
  if (!nzchar(chemin)) chemin <- file.path("..", "..", "inst", "extdata", "mandates_qc.csv")
  py <- file.path("..", "..", "inst", "python")
  if (!dir.exists(py)) py <- system.file("python", package = "pplmatch")
  m <- reticulate::import_from_path("matcher", path = py)
  mand <- m$`_load_mandates`(chemin)

  # Une date couverte rend le parti ; une date hors de tout mandat connu ne
  # doit RIEN rendre, pour laisser en place le parti issu de l'appariement.
  expect_equal(m$`_resolve_party`("rimouski", "2021-06-01", mand), "IND")
  expect_null(m$`_resolve_party`("rimouski", "1850-01-01", mand))
  expect_null(m$`_resolve_party`("siege-inexistant", "2021-06-01", mand))
  expect_null(m$`_resolve_party`("rimouski", "pas-une-date", mand))
})
