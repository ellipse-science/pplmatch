################################################################################
######## Script gabarit pour appareillement avec l'Assemblée Nationale #########
################################################################################

##### Paquetages #####
library(dplyr)
library(readxl)
library(fuzzyjoin)
library(stringr)

  # Veuillez réutiliser à votre guise les différentes lignes de ce scripts pour
  # les vôtres, en s'assurant de le modifier selon vos chemins d'accès

  # Cette fonction et base de donnée est encore en développement.
  # À noter que certains noms ne matchent pas en raison de doublons.
################################################################################
##### Chargement du dataset #####

  # Pour cet exemple, un échantillon aléatoire de 10000 interventions du journal
  # des débats de l'assemblée nationale datant de 1995-2024.
data_agora <- read.csv("C:/Users/mrben/Dropbox/CLESSN/Ellipse/ellipse-science/pplmatch/pplmatchQC/data_agora_95-24_RANDOM_SAMPLE.csv")

##### Chargement de la fonction pour l'appariement #####
source("C:/Users/mrben/Dropbox/CLESSN/Ellipse/ellipse-science/pplmatch/pplmatchQC/Script_pplmatchQC_beta.R")

##### Chargement de la base de données pour l'appariement #####
df_appQC <- read_excel("C:/Users/mrben/Dropbox/CLESSN/Ellipse/ellipse-science/pplmatch/pplmatchQC/Dataset_appariement_QC.xlsx")


################################################################################
##### Utilisation de la fonction pplmatchQC #####

  # C'est super simple ! il ne suffit que d'appeler le dataset sur lequel
  # appliquer la base de données pour l'appariement.

  # Par contre, il est nécessaire de nommer certaines colonnes
  # Celle contenant le nom des députés : "speaker_full_name"
  # Celle contenant la date de l'intervention : "event_date"
  # Celle contenant le contenu de l'intervention : "text"
data_agora_matched <- pplmatchQC(data_agora, df_appQC)


################################################################################
##### Visionnement des résultats #####

# par parti politique
table(data_agora_matched$party_id)

# proportion de match/non-match (FALSE/TRUE)
table(is.na(data_agora_matched$party_id))

# Count par speaker en ordre décroissant
sort(table(data_agora_matched$speaker_full_name), decreasing = TRUE)

# Count par speaker par parti en ordre décroissant (à ajuster selon le parti)
sort(table(data_agora_matched$speaker_full_name[data_agora_matched$party_id == "CAQ"]), decreasing = TRUE)

################################################################################
##### Ajout de noms la base de données pour l'appariement #####

  # Pour ajouter des noms, il ne suffit que d'ajouter ceux qui apparaissent dans
  # les non-matchs de votre utilisation de la fonction. Il faut les ajouter à la
  # colonne "other_names" séparé par des ";"

# Création d'un df contenant les non-matchs
na_party <- data_agora_matched %>%
  filter(is.na(party_id))

# Même chose mais cette fois sans le parti et avec un filtre ajustable pour le
# nombre d'occurence d'un speaker. De cette façon, c'est possible d'enlever les
# n les plus faibles et se concentrer sur ceux qui apparaissent plus souvent.
top_speakers <- data_agora_matched %>%
  filter(is.na(party_id)) %>%
  count(speaker_full_name, sort = TRUE) %>%
  slice_head(n = 50)
