# L'annuaire des tests : une copie des six tables de dim-qc-parliament, figee
# sous fixtures/annuaire/. pplmatch ne contient aucune donnee ; ces fichiers
# n'existent que pour tester l'appariement sur des cas reels et connus.
annuaire_test <- function() qc_annuaire_csv(testthat::test_path("fixtures", "annuaire"))

lire_fixture <- function(f) {
  utils::read.csv(testthat::test_path("fixtures", "annuaire", f),
                  stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")
}

# Les chemins des fichiers temporaires que pplmatchQC() ecrit pour le matcher
# Python (legislatures, sessions, mandats, fonctions), pour les tests qui
# appellent le module Python directement.
chemins_annuaire_test <- function() pplmatch:::.fichiers_temporaires_annuaire(annuaire_test())
