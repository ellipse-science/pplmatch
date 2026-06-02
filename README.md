# pplmatch

**Relier les paroles aux politiciens.**

Ce package R permet d'identifier automatiquement qui parle dans les debats de l'Assemblee nationale du Quebec (et du Canada). Il transforme des noms incomplets comme *"M. le Ministre"* ou *"M. Legault"* en donnees precises : **Francois Legault**, **CAQ**, **Premier Ministre**.

Il est concu pour etre utilise facilement par les etudiants et chercheurs du CAPP.

---

## Installation

Copiez-collez ces commandes dans votre console RStudio.

### 1. Installer le package
```r
remotes::install_github("ellipse-science/pplmatch")
```

### 2. Configurer Python (Important !)
Cet outil utilise une petite partie en Python pour faire la reconnaissance "floue" (quand il y a des fautes de frappe). Lancez cette commande **une seule fois** apres l'installation :

```r
pplmatch::ensure_python_deps()
```

---

## Utilisation rapide

```r
library(pplmatch)

# Chargez vos donnees de debats (CSV, base de donnees, API, etc.)
# Le data frame doit contenir les colonnes 'speaker' et 'event_date'
corpus <- read.csv("mes_debats_qc.csv")

# Lancer l'identification — c'est tout !
resultats <- pplmatchQC(corpus, verbose = TRUE)
```

Le package inclut deja la table des deputes du Quebec (legislatures 35 a 43, 1994-present). Vous n'avez rien d'autre a fournir.

### Analyser les resultats

```r
library(dplyr)

resultats %>%
  select(event_date, speaker, matched_name, party_id, role_status) %>%
  head(10)
```

### Utilisation avancee

```r
# Activer le web lookup (niveau 4) pour resoudre les cas ambigus via assnat.qc.ca
resultats <- pplmatchQC(corpus, web_lookup = TRUE, verbose = TRUE)

# Ajuster le seuil de correspondance floue (defaut : 85, entre 0 et 100)
resultats <- pplmatchQC(corpus, fuzzy_threshold = 90, verbose = TRUE)

# Fournir votre propre table de deputes (colonnes: full_name, party_id, gender, legislature_id)
members <- qc_members()  # table integree, modifiable
resultats <- pplmatchQC(corpus, members = mes_deputes, verbose = TRUE)
```

---

## Comprendre les resultats

Voici ce que signifient les nouvelles colonnes ajoutees a vos donnees :

| Colonne | Description | Exemple |
| :--- | :--- | :--- |
| **`speaker`** | Le nom tel qu'ecrit dans le transcript (brut). | *"M. Charest"* |
| **`matched_name`** | Le nom complet officiel du depute identifie. | *"Jean Charest"* |
| **`party_id`** | Le parti politique du depute a ce moment-la. | *"PLQ"* |
| **`role_status`** | **Gouvernement** ou **Opposition** ? Calcule selon la date de l'election. | *"Government"* |
| **`match_level`** | La fiabilite de l'identification (voir ci-dessous). | *"deterministic"* |

### Les niveaux de confiance (`match_level`)

*   **deterministic** : On est sur a 100%. Le nom est exact.
*   **contextual** : On a utilise le contexte de la journee pour deviner (ex: distinguer deux "Tremblay").
*   **web_contextual** : Resolu via assnat.qc.ca (uniquement si `web_lookup = TRUE`).
*   **role_inferred** : On n'a pas le nom, mais le titre (ex: "Le Ministre") confirme que c'est le **Gouvernement**.
*   **fuzzy** : Il y avait peut-etre une faute de frappe, mais on a trouve un nom tres proche.
*   **ambiguous** : Il y a plusieurs deputes avec ce nom et on n'a pas pu trancher.
*   **unmatched** : Impossible d'identifier la personne.

### Changements de parti en cours de mandat

Le package gere automatiquement les deputes qui ont quitte leur parti en cours de legislature (ex : passage a independant). La colonne `party_id` reflete le parti **au moment de l'intervention**, pas seulement le parti d'election.

---

## FAQ

**Q: Ca prend du temps a charger.**
R: C'est normal si vous avez un corpus tres long (plusieurs annees de debats). Essayez de travailler par periodes plus courtes.

**Q: C'est quoi "unmatched" ?**
R: Ce sont souvent des gens qui ne sont pas deputes (ex: un invite), ou des erreurs dans les vieux documents numerises (annees 90). Pour une analyse scientifique, vous pouvez souvent ignorer ces lignes.

**Q: Puis-je utiliser mes propres donnees de deputes ?**
R: Oui. `qc_members()` est un raccourci pour la table integree, mais vous pouvez passer n'importe quel data frame avec les colonnes `full_name`, `party_id`, `gender` et `legislature_id`.

---

*Developpe par le CLESSN / CAPP — ellipse-science/pplmatch*
