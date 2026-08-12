# Spec — modèle des mandats et des allégeances (Québec)

**Statut** : proposition, 2026-08-12. Test de référence : Arthabaska (voir
`tests/testthat/test-mandats-reference.R`).

---

## 1. Le problème

Le modèle actuel demande **« quel parti est cette circonscription ? »**.
La bonne question est **« qui occupait ce siège à cette date, et sous quelle
bannière ? »**. Tout ce qui suit découle de ce déplacement.

### Ce qui est cassé, mesuré le 2026-08-11

`members_historic_qc.csv` porte une ligne par (personne × élection), parti figé
au moment de l'élection.

| constat | mesure |
|---|---|
| défections enregistrées | `ind` apparaît **2 fois sur 1190 lignes**, toutes deux en 43ᵉ législature — zéro pour 1994-2022 |
| sièges à occupants multiples | **70** couples (siège × législature) |
| … dont les dates ne permettent PAS de dire qui siège quand | **68 sur 70** |
| partielles portant la date de l'élection **générale** | **67 sur 69** |
| granularité de fin de mandat | `exit_year` est une **année**, pas une date |

Conclusion : le référentiel n'est pas incomplet, il est **structurellement
incapable** de répondre à la question. L'information n'y est sous aucune forme.

`party_changes_qc.csv` (9 lignes, saisie à la main) comble partiellement le
trou, mais il est indexé par **circonscription**. Vérifié contre le relevé
officiel de l'ANQ du 31 décembre 2025, il contient au moins quatre défauts :
date erronée pour La Prairie (2025-12-18 alors que la chronologie dit
2025-09-18), et absences de Rimouski, Taillon et Arthabaska.

### Le défaut de conception, en une ligne

Une défection appartient à une **personne**, pas à un **siège**. Avec une clé
par circonscription, la défection d'Eric Lefebvre (Arthabaska, CAQ → IND,
2024-04-16) est héritée par Alex Boissonneault, élu **PQ** à la partielle du
11 août 2025 dans le même siège.

---

## 2. Le schéma

Trois tables. Une seule porte les faits ; les deux autres portent les
identités.

### `mandates_qc.csv` — le cœur

Une ligne = une période continue où **une personne** occupe **un siège** sous
**un parti**.

```csv
person_id,seat_id,party_id,date_start,date_end,start_reason,end_reason,source,confidence
17845,arthabaska,CAQ,2022-10-03,2024-04-15,election,defection,chrono114,verified
17845,arthabaska,IND,2024-04-16,2025-03-18,defection,resignation,chrono114;chrono115,verified
99001,arthabaska,PQ,2025-08-11,,byelection,,chrono115,verified
```

| colonne | rôle |
|---|---|
| `person_id` | identifiant ANQ stable — déjà dans `assnat_ids_qc.json` |
| `seat_id` | identifiant de siège stable, **jamais** son nom d'affichage |
| `date_end` | vide = toujours en cours |
| `start_reason` | `election`, `byelection`, `defection`, `reinstatement` |
| `end_reason` | `defection`, `resignation`, `death`, `dissolution`, vide si en cours |
| `source` | page d'où vient l'affirmation — toute ligne est auditable |
| `confidence` | `verified`, `single_source`, `disputed` (§ 5) |

### `seats_qc.csv` — les sièges et leurs noms dans le temps

```csv
seat_id,name,date_start,date_end
laporte,Laporte,1973-01-01,2025-05-28
laporte,Pierre-Laporte,2025-05-29,
```

Un renommage change le **nom**, pas l'identité. `seat_id` ne bouge jamais.
(Chronologie du 29 mai 2025 : Laporte → Pierre-Laporte, Matane-Matapédia →
Matane-Matapédia-Mitis, et trois autres.)

### `persons_qc.csv` — les identités et leurs variantes

```csv
person_id,full_name,other_names,assnat_url
```

Sépare l'identité du libellé. C'est ce qui absorbe les graphies fautives de
la source elle-même — l'ANQ écrit « Markwah » pour Marwah Rizqy, et
« la députée **des** Rimouski ».

---

## 3. Les quatre cas, exprimés par construction

| cas | expression |
|---|---|
| **défection** | la ligne se ferme (`end_reason=defection`), une nouvelle s'ouvre — **même personne, même siège**, parti différent |
| **démission** | la ligne se ferme (`end_reason=resignation`), **aucune** ne s'ouvre → siège vacant, visible comme tel |
| **partielle** | une ligne s'ouvre pour une **autre** personne, même siège (`start_reason=byelection`) |
| **renommage** | rien ne bouge dans `mandates` — c'est `seats_qc.csv` qui porte le changement |

---

## 4. La résolution

```
étant donné (locuteur, date) :
  1. candidats ← mandats où date_start ≤ date ≤ (date_end ou +∞)
  2. appariement flou du nom sur CES candidats seulement
  3. parti ← celui de la ligne retenue
```

Remplace `date → législature → membres`. La différence n'est pas cosmétique :
l'ensemble de candidats est déjà correct, donc un successeur **ne peut pas**
hériter du parti de son prédécesseur.

---

## 5. Ce que le schéma élimine, et ce qu'il rend seulement détectable

Aucun schéma n'abolit les erreurs. Un bon schéma rend des classes entières
**inexprimables**, et le reste **détectable**. Les deux listes, séparées.

### Rendues impossibles à écrire

- une défection héritée par un successeur — les lignes sont par personne ;
- un renommage qui casse la clé — `seat_id` est stable ;
- « le parti d'un siège » sans date — la date est dans la clé.

### Rendues détectables — invariants à faire tourner en CI

1. **Non-chevauchement** — pour un siège donné, jamais deux mandats simultanés.
2. **Plafond** — à toute date, mandats ouverts ≤ 125.
3. **Réconciliation ANQ** — à chaque date où la chronologie publie une
   composition, les comptes dérivés doivent correspondre **exactement**.
   C'est l'invariant qui donne un sens à « 100 % » : il a déjà attrapé quatre
   erreurs dans la table actuelle.
4. **Continuité** — tout `end_reason=defection` est suivi d'une ligne pour la
   même personne ; tout `resignation` est suivi soit d'une partielle, soit de
   la fin de la législature.
5. **Traçabilité** — toute ligne porte une `source` non vide.

---

## 6. Les sources, et leur hiérarchie

| source | apporte | limite |
|---|---|---|
| **Chronologie ANQ** (`chrono{N}.html`, 1994 → 2026 ; **chrono116 existe sans être listé dans l'index**) | dates au jour près, fait autorité | compositions détaillées fréquentes depuis 2010, **rares avant** |
| **Wikipédia** | comble 1994-2007 (Gobé 2003-02-20, Bouchard 2007-03-16), donne des dates de **fin** (« Simard, 12 avril **au** 25 septembre 1996 ») | **9 `<ref>` sur toute la page** — l'essentiel non sourcé |
| **Journal des débats** (`a-qc-parliament-debates`, que nous possédons) | primaire et réellement indépendant | ne **date** pas la défection, il la **borne** |

**Ce qui ne marche pas comme source** : les notices biographiques des
député·es ne donnent que l'allégeance **courante**, sans date de changement
(vérifié sur Youri Chassin). L'index des députés est un état du jour.

### La règle de confiance

1. **Chronologie ANQ** = source de vérité (`source=chrono###`).
2. **Wikipédia** = candidat quand l'ANQ est muette, **jamais retenu sans
   `<ref>` vérifiable** (`source=wp+ref`).
3. **Journal des débats** = falsificateur, **jamais** source. Il ne crée pas
   de ligne ; il en invalide.

Le point qui décide : **Wikipédia n'est pas indépendant de l'ANQ** — ses
sources sont largement les mêmes. Deux sources d'accord ne prouvent donc rien ;
ça peut être la même erreur recopiée. Un accord n'est pas une preuve, c'est
une absence de désaccord.

### La falsification par nos propres données

Pour chaque défection datée, chercher la première intervention où la personne
parle « à titre de député·e indépendant·e ». Si elle **précède** la date de la
table, la table est fausse.

Exemple réel : la table donne Laporte (Isabelle Poulet) au 2025-11-04 ; le
Journal des débats la fait parler « pour la première fois à titre de députée
indépendante » le 2025-11-12. Cohérent. Une date au 1ᵉʳ décembre aurait été
réfutée mécaniquement.

Ça ne fixe pas la date — ça la borne. Mais ça tourne en CI et ne dépend de
personne.

### Ce que le croisement achète vraiment

Pas la certitude : il transforme des **inconnues inconnues** en **désaccords
énumérables**. On passe de « on espère que c'est juste » à « voici les six cas
où les sources divergent, un humain tranche ». L'incertitude ne disparaît pas,
elle devient finie et adressable.

D'où la colonne `confidence`. **Le module public n'affiche que
`verified`** — même convention que le champ `verified` du manifest de
`sonar-pipeline` : la dette reste visible au lieu d'être maquillée.

C'est la seule forme honnête de « 100 % » : non pas « tout est juste », mais
**« tout ce qu'on affiche est doublement attesté, et ce qui ne l'est pas est
nommé »**.

---

## 7. Réserve

« Exhaustif » n'est atteignable que pour ce que les sources **consignent**.
Les compositions détaillées de l'ANQ sont fréquentes depuis 2010, rares avant.
Pour les législatures 35-38 (1994-2007), l'invariant n° 3 aura donc moins de
points d'ancrage et la vérification y sera plus faible. **Il faut le dire**
plutôt que de laisser croire à une couverture uniforme — c'est précisément à
ça que sert `confidence`.
