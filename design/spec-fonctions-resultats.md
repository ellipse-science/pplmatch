# Fonctions, résultats électoraux et indemnités (43e législature)

Quatre tables ajoutées à la dimension `dim_qc_parliament`, à côté de
`mandates`, `persons` et `seats`. Rédigé le 2026-09-23.

**Pourquoi.** Les cartes de député de la Vitrine démocratique ont eu besoin
des fonctions de chaque élu, de sa rémunération et de son résultat électoral.
Collectées d'abord dans le dépôt de la Vitrine, ces données sont des faits de
référence sur les parlementaires : elles appartiennent à la dimension, pas à
un module. Le registre des données de référence de la Vitrine
(`docs/reference/donnees-de-reference.md`) le demande, et y notait les
« candidat·e·s par élection » comme inexistants.

Ce qui est **calculé** à partir de ces tables (rémunération totale, vis-à-vis
ministre et porte-parole, rareté des cartes) reste dans le code qui le
calcule : ce ne sont pas des faits de référence.

## Tables

### `functions` (`functions_qc.csv`)

Une personne × une fonction × un intervalle. Toutes les fonctions datées qui
touchent la 43e législature (du 2022-10-03 au 2026-10-05, mêmes bornes que
`mandates`), rémunérées ou non.

| colonne | contenu |
|---|---|
| `person_id` | identifiant de l'Assemblée (clé de `persons`) |
| `title` | intitulé officiel, tel que la fiche l'écrit |
| `function_code` | PM, PAN, M, CO, VP, LP, W, PCA, PC, AP, VC, PS, B, PP (porte-parole), vide sinon |
| `scale_category` | catégorie du barème (clé de `indemnity_scale`), vide si non rémunérée |
| `scale_pct` | pourcentage de l'indemnité de base, 0 si non rémunérée |
| `date_start`, `date_end` | intervalle ; `date_end` vide = en cours à la lecture |
| `source` | `assnat_fiche` ou `assnat_biographie` |
| `confidence` | `verified` (fiche datée) ou `transcribed` (biographie transcrite à la main) |

**Source** : la fiche de chaque député (assnat.qc.ca, section « Fonctions
politiques, parlementaires et ministérielles »), lue le 2026-09-22.
**Anciens députés** : l'index ne les liste plus et leur page n'offre qu'une
biographie en prose ; leurs fonctions sont transcrites à la main dans
`functions_transcribed_qc.csv`, avec la date de lecture. Le générateur échoue
si un député de la 43e absent du relevé courant n'y figure pas.

Les dates sont celles des fiches, **non bornées** : une fonction commencée en
2018 garde sa date de 2018. Les titres régionaux (« Ministre responsable de la
région de… ») sont conservés ; c'est au consommateur de les écarter s'il
cherche des portefeuilles. Le code et le taux se déduisent de l'intitulé
(`BAREME` dans le générateur) : ministres délégués comptés comme ministres,
membres suppléants du Bureau non rémunérés, seules les dix commissions
permanentes sectorielles donnent une indemnité.

### `election_results` (`election_results_qc.csv`)

Une candidature par ligne, pour les six scrutins de la 43e législature
(générale du 2022-10-03, partielles des 2023-03-13, 2023-10-02, 2025-03-17,
2025-08-11 et 2026-02-23).

| colonne | contenu |
|---|---|
| `election_date`, `election_type`, `election_code` | scrutin (`general` ou `by_election`, code d'Élections Québec) |
| `seat_id` | clé de `seats` |
| `candidate_name`, `party_abbrev`, `party_id` | candidat, abréviation d'Élections Québec, parti de la dimension (vide hors CAQ, PLQ, QS, PQ, PCQ) |
| `votes`, `vote_share`, `rank`, `elected` | résultat |
| `lead_votes` | avance de l'élu sur le deuxième (`nbVoteAvance`), élu seulement |
| `person_id` | élu seulement : la personne dont le mandat commence ce jour-là dans ce siège |
| `valid_votes`, `registered_voters`, `turnout` | circonscription |

**Source** : données ouvertes d'Élections Québec (dgeq.org, archives des
résultats). **Contrôle croisé** : chaque élu doit correspondre à un mandat de
`mandates` commençant le jour du scrutin dans ce siège ; les 130 élus
concordent, et leurs noms aussi.

### `indemnities` (`indemnities_qc.csv`)

L'indemnité annuelle de base par date d'entrée en vigueur : 101 561 $ (2022-04-01,
inchangée en 2023), 131 766 $ (2023-06-07, L.Q. 2023, c. 14, sans
rétroactivité), 141 625 $ (2025-04-01), 146 375 $ (2026-04-01). Sources :
versions archivées de la page « Indemnités et allocations » de l'Assemblée.

### `indemnity_scale` (`indemnity_scale_qc.csv`)

Le barème des indemnités additionnelles (pourcentage de la base par
fonction), inchangé de 2022 à 2026 d'après les tableaux archivés. Règle de
cumul, citée de l'Assemblée : un député qui cumule plusieurs fonctions « n'a
droit qu'à l'indemnité la plus élevée ».

## Gardes (`publier_dimension.R`)

Fonctions : intervalle valide, personne connue, catégorie présente au barème.
Résultats : un élu et un seul par circonscription et par scrutin, relié à une
personne connue, dans un siège connu. Indemnités : dates strictement
croissantes, montants positifs, barème sans doublon. Tests :
`tests/testthat/test-fonctions-resultats.R` (16 vérifications, dont les deux
Eric Girard et la partielle de Chicoutimi).

## Régénérer

```sh
python3 inst/python/build_functions.py --cache /tmp/fiches     # ~2 min (0,7 s par fiche)
python3 inst/python/build_election_results.py
Rscript inst/scripts/publier_dimension.R                        # vérifie, n'écrit rien
```

Sur un Python installé depuis python.org, macOS peut refuser le certificat
d'Élections Québec : préfixer par `SSL_CERT_FILE=/etc/ssl/cert.pem`.

## Points à vérifier dans les tables existantes

Relevés en construisant celles-ci, **non corrigés** ici :

1. **Dominique Anglade (16499)** : `mandates` fait finir son mandat le
   2023-03-12 ; sa biographie dit qu'elle a démissionné comme députée le
   1er décembre 2022.
2. **Youri Chassin (17881)** : `mandates` le dit `IND` dès le 2022-10-03 ; il a
   été élu sous la bannière de la CAQ et a quitté le caucus en septembre 2024
   (ses fonctions de député caquiste s'arrêtent les 6 et 12 septembre 2024).
3. **`persons` : deux lignes pour 17957** (« eric girard », « eric girard2 »,
   même lien), et la ligne de 17929 n'a pas de lien vers sa fiche.
