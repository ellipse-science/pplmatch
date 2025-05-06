#' Match Quebec National Assembly Debate Speakers to Member Records
#'
#' This function performs fuzzy matching between speakers in Quebec National Assembly 
#' debates and official records of assembly members. It uses the Jaro-Winkler distance
#' method to match names and filters matches by legislative period.
#'
#' @param Corpus A data frame containing assembly debates. Must include the
#' columns: `speaker` (speaker's name) and `event_date` (date of the debate).
#' @param Names A data frame containing assembly member records. Must include the
#' columns: `full_name`, `other_names` (optional), `party_id`, `gender`, and
#' `legislature_id`.
#' @param max_dist Maximum string distance allowed for a match (default: 0.15).
#' Lower values are more strict.
#'
#' @return A data frame of the original Corpus with additional columns:
#' `legislature`, `legislature_id`, `speaker_clean`, `matched_name`, `party_id`,
#' `gender`, and `name_source`.
#'
#' @import dplyr
#' @import fuzzyjoin
#' @import stringr
#' @importFrom tidyr separate_rows
#'
#' @examples
#' \dontrun{
#' # Assuming 'debates' and 'MPs' data frames are loaded:
#' matched_debates <- pplmatchQC(debates, MPs, max_dist = 0.15)
#'
#' # Check match rate
#' match_rate <- sum(!is.na(matched_debates$matched_name)) / nrow(matched_debates)
#' print(paste0("Match rate: ", round(match_rate * 100, 2), "%"))
#' }
#'
#' @export
pplmatchQC <- function(Corpus, Names, max_dist = 0.15) {
  # Create internal legislatures data frame
  legislatures <- tibble(
    legislature = 31:43,
    leg_start_date = as.Date(c(
      "1976-11-15", "1981-04-13", "1985-12-02", "1989-09-25", "1994-09-12",
      "1998-11-30", "2003-04-14", "2007-03-26", "2008-12-08", "2012-09-04",
      "2014-04-07", "2018-10-01", "2022-10-03"
    )),
    leg_end_date = as.Date(c(
      "1981-03-12", "1985-11-20", "1989-08-09", "1994-07-29", "1998-11-25",
      "2003-03-12", "2007-02-21", "2008-11-05", "2012-08-01", "2014-03-05",
      "2018-08-23", "2022-08-28", "2026-10-05"
    ))
  )
  
  # Step 1: Add legislature information to the corpus based on event_date
  Corpus_with_leg <- Corpus %>%
    fuzzy_left_join(
      legislatures,
      by = c("event_date" = "leg_start_date", "event_date" = "leg_end_date"),
      match_fun = list(`>=`, `<=`)
    ) %>%
    mutate(
      legislature_id = as.character(legislature),
      speaker_clean = str_to_lower(str_replace_all(speaker, "[^[:alpha:] ]", ""))
    )
  
  # Step 2: Prepare the full_name and other_names
  Names_full <- Names %>%
    mutate(
      name_clean = str_to_lower(str_replace_all(full_name, "[^[:alpha:] ]", "")),
      name_source = "full_name"
    ) %>%
    select(full_name, name_clean, party_id, gender, legislature_id, name_source)
  
  Names_other <- Names %>%
    filter(!is.na(other_names)) %>%
    tidyr::separate_rows(other_names, sep = ";\\s*") %>%
    mutate(
      name_clean = str_to_lower(str_replace_all(other_names, "[^[:alpha:] ]", "")),
      name_source = "other_names"
    ) %>%
    select(full_name, name_clean, party_id, gender, legislature_id, name_source)
  
  Names_all <- bind_rows(Names_full, Names_other)
  
  # Step 3: Unique speakers with their legislature
  Speakers <- Corpus_with_leg %>%
    distinct(speaker, speaker_clean, legislature_id)
  
  # Step 4: Join filtered by legislature
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
  
  # Step 5: Merge with the original corpus
  Corpus_matched <- Corpus_with_leg %>%
    left_join(Speaker_matches, by = c("speaker", "legislature_id"))
  
  return(Corpus_matched)
}