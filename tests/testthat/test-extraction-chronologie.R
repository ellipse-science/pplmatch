# Tests de regression sur l'extraction depuis la Chronologie parlementaire.
#
# POURQUOI CE FICHIER EXISTE. Les deux bugs verrouilles ici echouaient EN
# SILENCE : ils ne levaient rien, ne cassaient aucun invariant, et produisaient
# des tables d'apparence saine. Ils n'ont ete trouves qu'en verifiant a la main
# la 38e legislature. Sans test, ils reviennent au premier ajustement du motif.
#
# Les phrases ci-dessous sont recopiees telles quelles de la chronologie de
# l'ANQ (chrono97, chrono98) : c'est le texte reel qui doit rester classe
# correctement, pas une paraphrase commode.

charger_mandats <- function() {
  chemin <- file.path("..", "..", "inst", "python")
  if (!dir.exists(chemin)) chemin <- system.file("python", package = "pplmatch")
  reticulate::import_from_path("build_mandates", path = chemin)
}

test_that("une demission de FONCTION ne ferme pas le mandat (cas Boisclair)", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  # Le 2007-05-08, Boisclair quitte la direction du PQ — il reste depute
  # jusqu'au 2007-11-15. La regle precedente cherchait la mention d'un siege
  # n'importe ou dans la phrase, et se declenchait donc sur l'appositif qui
  # PRESENTE la personne. Resultat : mandat clos six mois trop tot, et la
  # parole de Boisclair disparaissait du corpus sans que rien ne le signale.
  expect_null(bm$extract_demission(paste(
    "André Boisclair , député de Pointe-aux-Trembles, quitte la direction",
    "du Parti québécois et démissionne à titre de chef du Parti québécois.")))

  # La vraie demission de siege, elle, doit toujours etre vue.
  expect_equal(
    bm$extract_demission("André Boisclair démissionne à titre de député de Pointe-aux-Trembles."),
    "pointeauxtrembles")

  # On peut demissionner des DEUX : le siege l'emporte sur la fonction.
  expect_equal(
    bm$extract_demission(paste(
      "Démission d’Andrée Laforest, à titre de ministre des Affaires",
      "municipales et de députée de Chicoutimi.")),
    "chicoutimi")

  # Fonction seule, sans siege nomme apres le verbe : le depute siege toujours.
  expect_null(bm$extract_demission(paste(
    "Le député de Chauveau, Sylvain Lévesque, démissionne de son poste",
    "de deuxième vice-président.")))
})

test_that("une phrase annoncant DEUX partielles rend DEUX circonscriptions", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  # La classe de caracteres du motif contient l'espace : sans borne, elle
  # avalait la conjonction et le nom suivant, et forgeait la circonscription
  # inexistante « bourgetetnicolelegerdanspointeauxt » — un siege fantome qui
  # n'apparie jamais personne.
  res <- bm$extract_partielle(paste(
    "À l'issue de deux élections partielles , Maka Kotto du Parti québécois",
    "est élu dans Bourget et Nicole Léger dans Pointe-aux-Trembles."))
  expect_equal(vapply(res, `[[`, character(1), 1L),
               c("bourget", "pointeauxtrembles"))

  # Chaque circonscription prend le parti nomme le plus proche AVANT elle.
  res2 <- bm$extract_partielle(paste(
    "À l'issue d' élections partielles , Roland Richer du Parti québécois est",
    "élu dans Argenteuil et Marc Tanguay du Parti libéral dans LaFontaine."))
  expect_equal(vapply(res2, `[[`, character(1), 2L), c("PQ", "PLQ"))
})

test_that("le nom de circonscription s'arrete au premier mot en minuscule", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  # Enumerer les charnieres laisse toujours passer la suivante : « et » reglé,
  # « lors de » et « à l'issue de » passaient encore. La borne porte donc sur
  # la forme du nom lui-meme — mots capitalises lies par des traits d'union.
  for (cas in list(
    list(txt = paste("Robert Dutil du Parti libéral est élu dans Beauce-Sud",
                     "lors des élections partielles du 21 septembre."),
         attendu = "beaucesud"),
    list(txt = paste("Christian Dubé de la Coalition avenir Québec est élu",
                     "dans Lévis à l'issue d'une élection partielle."),
         attendu = "levis"))) {
    res <- bm$extract_partielle(cas$txt)
    expect_equal(length(res), 1L)
    expect_equal(res[[1]][[1]], cas$attendu)
  }
})

test_that("le repli Wikipedia se tait des que l'ANQ a publie", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  # Le repli n'existe que pour combler l'annee que l'ANQ n'a pas encore
  # compilee. Des que sa page porte de vrais paragraphes, elle reprend la main
  # sans qu'on touche au code — et la fonction rend tout de suite, sans meme
  # aller lire Wikipedia (donc sans reseau ici).
  page_publiee <- paste0(
    paste0("<p>", strrep("Texte reel de la chronologie parlementaire. ", 3),
           "</p>", collapse = ""),
    strrep(paste0("<p>", strrep("Un autre paragraphe bien rempli. ", 3), "</p>"), 6))

  expect_length(
    bm$evenements_de_repli(page_publiee, c(2026), "/inexistant", list()), 0L)
})

test_that("le repli ne rejoue jamais une annee deja couverte", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  # LA REGRESSION. Une premiere version derivait l'annee du numero de page
  # (chrono86 = 1994, donc chronoN = N + 1908). La numerotation de l'ANQ ne
  # suit pas, et chrono116 devenait 2024 : le repli rejouait deux annees deja
  # publiees, et chaque evenement comptait double — Arthabaska se retrouvait
  # avec trois mandats ouverts le meme jour. La borne porte donc sur les
  # annees que la chronologie a REELLEMENT datees, pas sur un calcul de page.
  #
  # Ici on declare TOUTE la legislature couverte : le repli doit se taire,
  # meme s'il lit Wikipedia et y trouve des evenements.
  res <- tryCatch(
    bm$evenements_de_repli("", as.integer(2022:2030), "inst/extdata", list()),
    error = function(e) NULL)
  skip_if(is.null(res), "Wikipedia inaccessible")
  expect_length(res, 0L)
})

test_that("un decret annoncant la TENUE de partielles n'elit personne", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  # Convoquer une partielle n'est pas la gagner : aucun mandat ne doit naitre.
  expect_length(bm$extract_partielle(paste(
    "Le gouvernement prend un décret pour la tenue d' élections partielles",
    "le 5 décembre dans les circonscriptions d'Arthabaska.")), 0L)
})
