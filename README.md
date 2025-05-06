# pplmatch

## Description

Un package R qui fournit des outils pour faire l'appariement des noms des intervenants dans les débats parlementaires avec les tables dimensionnelles, permettant d'obtenir de l'information sur le parti politique et le genre des intervenants. Il utilise des techniques de correspondance pour maximiser le taux d'identification des intervenants.

## Fonctionnalités

- Appariement entre les noms d'intervenants dans les débats et la table dimensionnelle des parlementaires
- Fonctions spécifiques pour différentes législatures :
  - `pplmatchCA` : Chambre des Communes du Canada
  - `pplmatchQC` : Assemblée nationale du Québec
- Filtrage des correspondances par période législative
- Gestion des noms alternatifs des parlementaires
- Standardisation et nettoyage des noms

## Installation

```r
# Installer depuis GitHub
# devtools::install_github("username/pplmatch")
```

# Utilisation

## Appariement pour la Chambre des Communes

```r
library(pplmatch)

Matched_debates <- pplmatchCA(debates, MPs, max_dist = 0.15)
```

## Appariement pour l'Assemblée Nationale

```r
library(pplmatch)

Matched_debates <- pplmatchQC(debates, MPs, max_dist = 0.15)
```

# Fonctions principales

## pplmatchCA

Fait correspondre les intervenants des débats de la Chambre des communes du Canada avec la table dimensionnelle de ses parlementaires.

### Paramètres

- Corpus : Un jeu de données contenant les débats parlementaires (a-ca-parliament-debates)
- Names : La table dimensionnelle des parlementaires (dim-ca-parliament-members)
- max_dist : Distance maximale autorisée pour une correspondance (par défaut : 0.15)

## pplmatchQC

Fait correspondre les intervenants des débats de l'Assemblée Nationale du Québec avec la table dimensionnelle de ses parlementaires.

### Paramètres

- Corpus : Un jeu de données contenant les débats parlementaires (a-qc-parliament-debates)
- Names : La table dimensionnelle des parlementaires (dim-qc-parliament-members)
- max_dist : Distance maximale autorisée pour une correspondance (par défaut : 0.15)
