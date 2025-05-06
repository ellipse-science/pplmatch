#' Match Canadian Parliamentary Debate Speakers to Member Records
#'
#' This function performs fuzzy matching between speakers in parliamentary debates
#' and official records of parliament members. It uses the Jaro-Winkler distance
#' method to match names and filters matches by legislative period.
#'
#' @param Corpus A data frame containing parliamentary debates. Must include the
#'   columns: `speaker` (speaker's name), `event_number` (a string that includes
#'   a legislature number at the beginning).
#' @param Names A data frame containing parliament member records. Must include the
#'   columns: `id`, `full_name`, `other_names` (optional), `party_id`, `gender`, and
#'   `legislature_id`.
#' @param max_dist Maximum string distance allowed for a match (default: 0.15).
#'   Lower values are more strict.
#'
#' @return A data frame of the original Corpus with additional columns:
#'   `legislature_num`, `legislature_id`, `speaker_clean`, `matched_name`, `party_id`,
#'   `gender`, and `name_source`.
#'
#' @import dplyr
#' @import fuzzyjoin
#' @import stringr
#' @importFrom tidyr separate_rows
#' 
#' @examples
#' \dontrun{
#' # Assuming 'debates' and 'MPs' data frames are loaded:
#' matched_debates <- pplmatchCA(debates, MPs, max_dist = 0.15)
#' 
#' # Check match rate
#' match_rate <- sum(!is.na(matched_debates$matched_name)) / nrow(matched_debates)
#' print(paste0("Match rate: ", round(match_rate * 100, 2), "%"))
#' }
#'
#' @export
pplmatchCA <- function(Corpus, Names, max_dist = 0.15) {
  # Étape 1 : ajouter legislature_id et speaker_clean au corpus
  Corpus <- Corpus %>%
    mutate(
      legislature_num = str_extract(event_number, "^\\d{2}"),
      legislature_id = paste0("CACOMMONS", legislature_num),
      speaker_clean = str_to_lower(str_replace_all(speaker, "[^[:alpha:] ]", ""))
    )
  
  # Étape 2 : Préparer le full_name et les other_names
  Names_full <- Names %>%
    mutate(
      name_clean = str_to_lower(str_replace_all(full_name, "[^[:alpha:] ]", "")),
      name_source = "full_name"
    ) %>%
    select(id, full_name, name_clean, party_id, gender, legislature_id, name_source)
  
  Names_other <- Names %>%
    filter(!is.na(other_names)) %>%
    tidyr::separate_rows(other_names, sep = ";\\s*") %>%
    mutate(
      name_clean = str_to_lower(str_replace_all(other_names, "[^[:alpha:] ]", "")),
      name_source = "other_names"
    ) %>%
    select(id, full_name, name_clean, party_id, gender, legislature_id, name_source)
  
  Names_all <- bind_rows(Names_full, Names_other)
  
  # Étape 3 : intervenants uniques avec leur législature
  Speakers <- Corpus %>%
    distinct(speaker, speaker_clean, legislature_id)
  
  # Étape 4 : jointure filtrée par législature
  Speaker_matches <- stringdist_inner_join(
    Speakers,
    Names_all,
    by = c("speaker_clean" = "name_clean", "legislature_id" = "legislature_id"),
    method = "jw",
    max_dist = max_dist,
    distance_col = ".dist"
  ) %>%
    rename(
      legislature_id = legislature_id.x
    ) %>%
    group_by(speaker, legislature_id) %>%
    slice_min(.dist, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(speaker, legislature_id, matched_name = full_name, party_id, gender, name_source)
  
  # Étape 5 : merge avec le corpus original
  Corpus_matched <- Corpus %>%
    left_join(Speaker_matches, by = c("speaker", "legislature_id")) |> 
    select(-c("legislature_num", "speaker_clean", "name_source"))
  
  return(Corpus_matched)
}