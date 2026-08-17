# Passation — modèle de mandats daté pour `pplmatch`

**Pour la personne (ou l'agent) qui reprend.** Rédigé le 2026-08-17, à la fin
d'une session qui a produit la branche `feat/modele-mandats-dates` et la
[PR #2](https://github.com/ellipse-science/pplmatch/pull/2), 18 commits.

Lis d'abord [`spec-mandats-qc.md`](./spec-mandats-qc.md) — c'est la conception et
le raisonnement. Ce fichier-ci dit **où on en est, ce qui reste, et les pièges
qui ont coûté cher**.

---

## 1. Le problème d'origine

`pplmatch` attribuait le mauvais parti à des député·es, **en silence**.

Le modèle était indexé par **circonscription**. Une défection était donc héritée
par le successeur du transfuge : Éric Lefebvre quitte la CAQ dans Arthabaska le
2024-04-16, et Alex Boissonneault — élu **péquiste** au même siège en 2025 —
sortait `IND`.

Deuxième défaut, plus grave parce qu'invisible : le référentiel ne couvrait que
**121 des 125 sièges** de la 43e législature. Les manquants n'étaient pas mal
attribués, ils étaient `unmatched`, donc **jetés par le raffineur en aval**. De
la parole disparaissait au lieu d'être comptée.

---

## 2. Ce qui a été construit

Un modèle **daté**, indexé par personne et par intervalle fermé. Trois tables
générées :

| table | contenu |
|---|---|
| `mandates_qc.csv` | une personne × un siège × un intervalle, avec `party_id`, `parliamentary_status`, `source`, `confidence` |
| `persons_qc.csv` | identités et variantes de graphie |
| `seats_qc.csv` | sièges et leurs noms successifs |

Les quatre cas (défection, démission, partielle, renommage) sont exprimés **par
construction** : une défection ferme un mandat et en ouvre un autre pour la même
personne ; un renommage ne touche que `seats_qc`.

### Les sources, et leur hiérarchie

1. **Chronologie parlementaire** (`chrono86`–`chrono116`, 1994→2026) — date les
   événements, fait autorité.
2. **Historique par circonscription** (`depcir`) — 4 497 lignes, 229
   circonscriptions depuis 1867. Arbitre les démissions (colonne *Remarques*,
   219 dates) et comble les trous du référentiel.
3. **Fiches individuelles** — troisième source, **réellement indépendante** :
   rédigée par la Bibliothèque à partir du dossier du membre, pas du Journal des
   débats. Arbitre 36 démissions.
4. **Wikipédia** — repli pour l'année en cours seulement, sort en
   `single_source`.

### Générateurs (`inst/python/`)

```
build_party_changes.py             chronologie : analyse et défections
build_mandates.py                  LE générateur — construit les 3 tables
build_deputes_courants.py          relevé du jour de l'ANQ (125 sièges)
build_deputes_par_circonscription.py  historique par circonscription
build_demissions_fiches.py         dates de démission depuis les fiches
wikipedia_fallback.py              repli année courante
```

Régénérer : `python3 inst/python/build_mandates.py --cache /tmp/chrono`

---

## 3. État mesuré au 2026-08-17

```
1348 mandats : 1166 verified | 181 single_source | 1 disputed
125/125 sièges couverts | 0 divergence de parti contre le relevé du jour
0 violation d'invariant sur 6 règles
8 suites de tests, 137 assertions, 0 échec
```

**Preuve par l'API publique** — 6 cas de contrôle, **4 étaient faux avant** :

| appel | avant | après |
|---|---|---|
| Sklavounos 2017-05-01 | `PLQ` | `IND` |
| LeBel 2021-06-01 | `PQ` | `IND` |
| Boissonneault 2025-09-15 | `unmatched` | `PQ` / arthabaska |
| St-Louis 2024-02-01 | `unmatched` | `CAQ` / joliette |

### Publié en DEV

```
dim_qc_parliament-mandates   1348 lignes
dim_qc_parliament-persons    1870 lignes
dim_qc_parliament-seats       157 lignes
```

Base Glue `gluestackdatamartdbd046f685`, préfixe S3 `dim_qc_parliament/`.
Le nom calque la convention de l'entrepôt (`dim-qc-parliament-members`).
Publier : `Rscript inst/scripts/publier_dimension.R --env DEV --go`.

Une première publication sous le datamart `pplmatch` a été **supprimée** (Glue +
S3) après renommage — ce nom disait l'outil, pas le contenu.

---

## 4. Ce qui reste à faire

1. **Faire relire la PR par Adrien.** Un brouillon Slack lui est adressé, non
   envoyé. Les deux points qui méritent son avis : *affiliation ≠ statut
   parlementaire* (décision Chagnon 2012) et la hiérarchie des sources.
2. **Publier en PROD** — après revue et merge seulement.
   `Rscript inst/scripts/publier_dimension.R --env PROD --go`.
3. **Enregistrer le schéma Glue en PROD.** C'est le défaut exact qui fait
   renvoyer `COLUMN_NOT_FOUND` à `dim-qc-parliament-members`.
4. **Décider si ces tables remplacent `dim-qc-parliament-members`**, qui est un
   **instantané déguisé en dimension** (`version` = 43 partout, `end_date`
   vide). Nos tables sont ce que celle-là prétend être. La convention `-staging`
   existe dans l'entrepôt pour ce genre de candidature.

---

## 5. Limites connues — à ne pas « corriger » à l'aveugle

- **1 litige** : Jean-Talon 2019 (Sébastien Proulx). Les trois sources sont
  muettes. C'est le bon usage de l'étiquette, pas une dette.
- **181 `single_source`** : surtout des partielles datées par la chronologie
  seule. Les passer en `verified` sans seconde source, ce serait **fabriquer de
  la confiance**.
- **`depcir` est périmée pour 14 circonscriptions** (t→w, arrêtées à 2018).
  Vérifié par retéléchargement : c'est la page, pas l'analyse. **Conséquence
  réelle : nulle** — une seule de ces 14 n'a pas de mandat en 43e, Bourget, et
  c'est parce qu'elle a été renommée Camille-Laurin.
- **Homonymes** : « Eric Girard » seul reste ambigu par construction (deux
  personnes). Le matcher en choisit une et sort `deterministic` — une certitude
  affichée qui n'en est pas une. **Prochain vrai chantier**, et il touche le
  matcher, pas les données.
- **Colonne `tag`** ajoutée par la plateforme à la publication (11 colonnes au
  lieu de 10). Sans danger en accès **par nom** ; un consommateur qui lit **par
  index** se décalerait.

---

## 6. Les pièges qui ont coûté cher

À lire avant de toucher aux extracteurs. Chacun a été trouvé **en lisant les
données à la main**, jamais par une exception.

**Tout échoue en silence.** Aucun de ces bugs ne levait d'erreur, ne violait un
invariant, ni ne rendait une table suspecte. C'est la nature du domaine : une
donnée politique fausse ressemble à une donnée politique vraie.

**Une démission réclamée n'est pas une démission.** « Les signataires demandent
la démission du député de Sherbrooke, Jean Charest » fermait son siège —
Charest y a siégé jusqu'en septembre 2012. **19 mois de parole d'un premier
ministre en exercice, effacés.**

**Le siège doit être l'OBJET de la démission, pas l'appositif.** « Démission
**du président de l'Assemblée nationale** Yvon Vallières, député de Richmond » :
il quitte la présidence et garde son siège dix-sept mois. Trouver le mot
« député » après le verbe ne suffit pas.

**La chronologie date l'annonce, l'ANQ date la vacance.** Bouchard annonce le
2001-01-11, quitte le 2001-03-08. Deux faits, et c'est le second qui borne un
mandat.

**Énumérer les charnières ne marche jamais.** Corriger « et » laisse passer
« lors de », puis « à l'issue de ». Il faut une **propriété du nom lui-même** :
une circonscription québécoise est faite de mots capitalisés liés par des traits
d'union, et ne contient jamais « et ».

**Ne jamais dériver une année d'un numéro de page.** `chronoN = N + 1908` semble
sûr et donne 2024 pour `chrono116`. Le repli Wikipédia rejouait alors deux
années déjà publiées, et chaque événement comptait double.

**Un invariant écrit n'est pas un invariant implémenté.** Le plafond de 125 et
la continuité figuraient dans la spec sans exister dans le code : « 0 violation »
ne portait que sur deux règles de cinq, et 137 mandats coexistaient en 2022.
Les six règles sont maintenant dans `invariants()`.

**Vérifier, pas lire un log.** `tube` écrit `ERROR: la table n'existe pas` avant
de la créer — ce n'est pas un échec. Et le traitement est **asynchrone** : j'ai
conclu à tort qu'une table manquait alors qu'elle est arrivée 20 secondes après
ma requête. Interroger Athena, et retenter.

---

## 7. Discipline de travail à conserver

- **Un test de régression doit échouer sur le code d'avant.** Vérifié à chaque
  fois en restaurant l'ancienne version. Un test qui n'a jamais mordu ne prouve
  rien — et une garde non plus : celle de `publier_dimension.R` a été testée sur
  des données volontairement cassées.
- **Diff complet avant/après sur les 1 348 mandats**, et chaque écart doit
  s'expliquer. C'est ce qui a permis d'affirmer « 24 écarts, tous des
  corrections » plutôt que « ça a l'air correct ».
- **Ne jamais inférer un parti par soustraction.** Les membres CAQ manquants de
  2012 seraient déductibles d'un total ; on ne le fait pas — c'est l'erreur que
  ce modèle existe pour empêcher.
- **Commits en anglais, trailer de provenance en français** :
  `Assisté par : Claude Code (Sonnet 5)`. Jamais de `Co-Authored-By`.
