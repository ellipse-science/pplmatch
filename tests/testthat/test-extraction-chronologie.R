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

# `extract_demission` rend une LISTE de sieges ; reticulate en fait une liste R.
sieges <- function(bm, txt) as.character(unlist(bm$extract_demission(txt)))

test_that("une demission de FONCTION ne ferme pas le mandat (cas Boisclair)", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  # Le 2007-05-08, Boisclair quitte la direction du PQ — il reste depute
  # jusqu'au 2007-11-15. La regle precedente cherchait la mention d'un siege
  # n'importe ou dans la phrase, et se declenchait donc sur l'appositif qui
  # PRESENTE la personne. Resultat : mandat clos six mois trop tot, et la
  # parole de Boisclair disparaissait du corpus sans que rien ne le signale.
  expect_length(sieges(bm, paste(
    "André Boisclair , député de Pointe-aux-Trembles, quitte la direction",
    "du Parti québécois et démissionne à titre de chef du Parti québécois.")), 0L)

  # La vraie demission de siege, elle, doit toujours etre vue.
  expect_equal(
    sieges(bm, "André Boisclair démissionne à titre de député de Pointe-aux-Trembles."),
    "pointeauxtrembles")

  # On peut demissionner des DEUX : le siege l'emporte sur la fonction.
  expect_equal(
    sieges(bm, paste(
      "Démission d’Andrée Laforest, à titre de ministre des Affaires",
      "municipales et de députée de Chicoutimi.")),
    "chicoutimi")

  # Fonction seule, sans siege nomme apres le verbe : le depute siege toujours.
  expect_length(sieges(bm, paste(
    "Le député de Chauveau, Sylvain Lévesque, démissionne de son poste",
    "de deuxième vice-président.")), 0L)
})

test_that("une demission RECLAMEE n'est pas une demission (cas Charest)", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  # Le cas le plus couteux trouve a ce jour. La chronologie rapporte les
  # PETITIONS dans les memes mots que les departs ; les lire comme des
  # demissions fermait le siege de Sherbrooke le 2011-02-16, alors que Jean
  # Charest y a siege jusqu'en septembre 2012 — 19 mois de parole d'un premier
  # ministre EN EXERCICE effaces du corpus, sans un signe.
  expect_length(sieges(bm, paste(
    "Le député de Mercier, Amir Khadir, dépose l'extrait d'une pétition signée",
    "par 247 379 citoyens. Les signataires demandent la démission du député de",
    "Sherbrooke, Jean Charest, en tant que chef du gouvernement.")), 0L)

  # A Anjou, la phrase dit meme que la petition est REFUSEE.
  expect_length(sieges(bm, paste(
    "Le président de l'Assemblée nationale refuse une pétition de plus de",
    "3 000 signatures exigeant la démission du député libéral d'Anjou,",
    "Jean-Sébastien Lamoureux.")), 0L)
})

test_that("une phrase peut annoncer PLUSIEURS departs", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  # Bourassa etait perdu : seul le premier siege sortait. Meme classe que les
  # partielles multiples — un paragraphe parle de plus d'une personne bien plus
  # souvent qu'on ne le suppose.
  expect_equal(
    sieges(bm, paste(
      "La députée de Kamouraska-Témiscouata, France Dionne, et le député de",
      "Bourassa, Yvon Charbonneau, démissionnent comme membres de l'Assemblée",
      "nationale.")),
    c("kamouraskatemiscouata", "bourassa"))

  # Mais « depute de X ET DE chef de parti », c'est UN siege et une fonction.
  # La capture avalait la fonction et forgeait
  # « riviereduloupetdechefdelactiondemocratique » : un siege introuvable, donc
  # une demission jamais enregistree — silencieusement, puisqu'une demission ne
  # fait que MODIFIER un mandat existant.
  expect_equal(
    sieges(bm, paste(
      "Démission de Mario Dumont à titre de député de Rivière-du-Loup et de",
      "chef de l'Action démocratique.")),
    "riviereduloup")
  expect_equal(
    sieges(bm, paste(
      "Guy Chevrette, député de Joliette et ministre des Transports, Jacques",
      "Brassard, député de Lac-Saint-Jean, démissionnent.")),
    c("joliette", "lacsaintjean"))
})

test_that("la date d'EFFET l'emporte sur la date d'annonce", {
  skip_if_not(reticulate::py_available(), "Python not available")
  bm <- charger_mandats()

  d <- function(x) as.Date(reticulate::py_to_r(x))
  # « annonce sa demission [...] Celle-ci sera effective le 15 avril » : fermer
  # au jour de l'annonce retire cinq semaines a quelqu'un qui siege encore.
  expect_equal(d(bm$date_effet(
    "Gérald Tremblay annonce sa démission comme député. Celle-ci sera effective le 15 avril.",
    reticulate::r_to_py(as.Date("1996-03-14")))), as.Date("1996-04-15"))

  # Sans date d'effet, on garde l'annonce.
  expect_equal(d(bm$date_effet(
    "Démission du député libéral d'Argenteuil, Régent L. Beaudet.",
    reticulate::r_to_py(as.Date("1997-12-18")))), as.Date("1997-12-18"))

  # Annonce en decembre, effet en janvier : l'annee bascule.
  expect_equal(d(bm$date_effet(
    "X annonce sa démission, effective le 5 janvier.",
    reticulate::r_to_py(as.Date("2010-12-20")))), as.Date("2011-01-05"))
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
