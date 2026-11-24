#' Installer les dependances Python pour pplmatch
#'
#' Cree un environnement virtuel Python nomme "pplmatch" et y installe le
#' package \code{rapidfuzz}, necessaire pour l'appariement fuzzy dans
#' \code{\link{pplmatchQC}}.
#'
#' Cette fonction ne doit etre appelee qu'**une seule fois** par machine.
#' Apres l'installation, le package active automatiquement le virtualenv au
#' chargement.
#'
#' @param envname Nom de l'environnement virtuel (defaut : "pplmatch").
#'   Le virtualenv est cree dans \code{~/.virtualenvs/}.
#'
#' @return \code{TRUE} (invisible) si l'installation a reussi.
#'
#' @details
#' Prerequis : Python 3 (>= 3.8) doit etre installe sur le systeme.
#' Le virtualenv est gere par \code{reticulate} et se trouve dans
#' \code{~/.virtualenvs/pplmatch/}.
#'
#' @examples
#' \dontrun{
#' # Premiere utilisation du package :
#' ensure_python_deps()
#'
#' # Ensuite, pplmatchQC() fonctionne directement :
#' result <- pplmatchQC(corpus, members)
#' }
#'
#' @seealso \code{\link{pplmatchQC}} pour la fonction de matching principale.
#'
#' @export
ensure_python_deps <- function(envname = "pplmatch") {
  if (!reticulate::virtualenv_exists(envname)) {
    message("Creating virtualenv '", envname, "'...")
    reticulate::virtualenv_create(envname)
  }
  message("Installing rapidfuzz into '", envname, "'...")
  reticulate::virtualenv_install(envname, packages = "rapidfuzz")
  message("Done. Python dependencies are ready.")
  invisible(TRUE)
}

#' Trouver le repertoire des modules Python
#'
#' Cherche le repertoire \code{inst/python} dans le package installe ou dans
#' l'arborescence source (mode developpement).
#'
#' @return Chemin vers le repertoire contenant les modules Python.
#' @keywords internal
.find_python_dir <- function() {
  # First try installed package location
  d <- system.file("python", package = "pplmatch")
  if (d != "") return(d)

  # During development: search up from working directory for DESCRIPTION
  dir <- getwd()
  for (i in seq_len(10)) {
    if (file.exists(file.path(dir, "DESCRIPTION"))) {
      candidate <- file.path(dir, "inst", "python")
      if (dir.exists(candidate)) return(candidate)
    }
    parent <- dirname(dir)
    if (parent == dir) break
    dir <- parent
  }

  stop("Cannot find pplmatch Python modules. Is the package installed or ",
       "are you in the package source directory?", call. = FALSE)
}

#' @keywords internal
.load_matcher <- function() {
  py_dir <- .find_python_dir()
  reticulate::py_run_string(sprintf("import sys; sys.path.insert(0, '%s')", py_dir))
  reticulate::import_from_path("matcher", path = py_dir)
}

#' @keywords internal
.load_normalizer <- function() {
  reticulate::import_from_path("normalizer", path = .find_python_dir())
}

#' @keywords internal
.load_legislature <- function() {
  reticulate::import_from_path("legislature", path = .find_python_dir())
}
