library(fuzzyjoin)
library(dplyr)
library(stringr)

# Objets de connexion
condwd <- tube::ellipse_connect("DEV", "datawarehouse")
condwp <- tube::ellipse_connect("PROD", "datawarehouse")

# charger la table des parlementaires
MPs <- tube::ellipse_query(condwd, "dim-ca-parliament-members") |> 
  dplyr::collect()

# Charger les débats parlementaires
debates <- tube::ellipse_query(condwp, "a-ca-parliament-debates") |> 
  dplyr::collect()

# Fonction 
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
  
  Names_all <- bind_rows(Names_full, Names_alt)
  
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
    left_join(Speaker_matches, by = c("speaker", "legislature_id"))
  
  return(Corpus_matched)
}


Matched_data <- pplmatchCA(debates, MPs)




