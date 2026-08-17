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

## 5 bis. Affiliation ≠ statut parlementaire (le cas CAQ 2011-2012)

Un siège peut porter **deux vérités simultanées**, et le modèle doit les
séparer plutôt que d'en choisir une.

Le 14 février 2012, deux décisions tombent le même jour. Le DGE confirme la
fusion ADQ–CAQ : « Le nouveau parti, la Coalition avenir Québec, **succède aux
droits et obligations** des partis fusionnés. » Et le président Chagnon tranche
le statut des élu·es concerné·es :

> « Ils siégeront comme indépendants. **Cependant, ils figureront comme députés
> indépendants représentant la CAQ dans le *Journal des débats*, au Canal de
> l'Assemblée et dans le site Internet de l'Assemblée nationale.** »

Le *Journal des débats* **est notre corpus**. L'ANQ y étiquette donc elle-même
ces personnes « représentant la CAQ ». Et sa composition à la dissolution
compte « Coalition avenir Québec, **9** » — elle ne les range pas parmi les
indépendants.

Leur attribuer la CAQ n'est donc pas une interprétation de notre part : c'est
reproduire ce que fait la source. D'où deux colonnes :

| colonne | ce qu'elle porte | pour ce cas |
|---|---|---|
| `party_id` | l'**affiliation politique** — ce que le Journal affiche | `CAQ` |
| `parliamentary_status` | le **statut de siège** — la décision du président | `independent` |

La fusion se distingue d'une défection : personne n'a changé de camp, c'est la
personne morale qui a été remplacée. D'où `start_reason=merger`.

### Les autres gisements Agora de l'infra (relevé du 2026-08-12)

Le corpus des débats n'est pas seul. Inventaire fait :

| table | couverture | utile ici ? |
|---|---|---|
| `a-qc-parliament-debates` (PROD) | 2016-09-20 → 2026-06-12, 462 618 lignes | ne remonte pas à 2012 |
| idem (DEV) | même départ, 219 641 lignes | non |
| `a-qc-parliamentary-commissions` | **2009**, puis **rien jusqu'en 2022** | le trou couvre 2012 |
| `dim-qc-parliament-members` | 125 personnes, 43ᵉ législature | **oui** — 125 sièges contre 121 au référentiel |
| `a-ca-parliament-debates`, `a-eu-parliament-debates` | autres corpus | hors sujet |

`dim-qc-parliament-members` (DEV) est un **instantané**, pas une dimension à
versions : `version` vaut 43 pour les 125 lignes, `start_date` = 2022-10-03
partout, `end_date` vide. Elle porte des colonnes `wikidata_qid` et
`wikipedia_url` — **vides (0/125)**. Le même objet côté PROD renvoie
`COLUMN_NOT_FOUND` : schéma Glue non enregistré.

### Wikidata : mesuré, pas supposé

Test du 2026-08-12 sur deux cas de la formation de la CAQ, via la propriété
P102 (« membre d'un parti politique ») et ses qualificatifs de dates :

| personne | Wikidata | chronologie ANQ |
|---|---|---|
| Éric Caire | CAQ **depuis 2011-12-19** (date précise) | quitte l'ADQ en 2009, rien sur la CAQ |
| François Rebello | PQ, Bloc québécois — **aucune CAQ** | « quitte le PQ, rejoint la CAQ » (2012-01-10) |

Chaque source sait ce que l'autre ignore, et Wikidata laisse Caire « ADQ, en
cours » — faux depuis 2009. C'est la démonstration concrète de la règle du § 6 :
Wikidata est un **candidat**, jamais une vérité. Elle ajoute des points
d'ancrage là où elle concorde, et signale un **désaccord à trancher** là où
elle diverge. Aucune des deux ne domine.

### Ce qui reste non prouvé, et pourquoi on n'invente pas

L'ANQ compte 9 caquistes à la dissolution ; nous en établissons 8 — les
adéquistes par la fusion, plus François Rebello, nommé explicitement
(2012-01-10). Les autres sont d'ancien·nes péquistes passé·es à la CAQ après
avoir siégé comme indépendant·es, **et la chronologie ne les nomme pas
individuellement**.

Les compter comme caquistes ferait tomber l'écart à zéro. On ne le fait pas :
un compte agrégé n'identifie personne, et attribuer un parti à quelqu'un sur la
foi d'une soustraction est exactement ce que ce modèle existe pour empêcher.
Le corpus pourrait trancher — le Journal des débats les étiquette, par décision
du président — mais `a-qc-parliament-debates` ne remonte qu'à 2016-09-20.

## 6 bis. La chronologie est une source RETARDÉE

Constaté le 2026-08-12 : `chrono116.html` (2026) existe mais c'est un **gabarit
vide** — « Date », « Texte ici. ». La Bibliothèque de l'ANQ compile la
chronologie **rétrospectivement**, pas au fil de l'eau.

Conséquence structurelle, et non accidentelle : **aucun événement de l'année en
cours n'est disponible dans la source qui les date**. Au 2026-08-12, les quatre
divergences restantes du générateur sont toutes de 2026 —

| siège | ce que l'ANQ affiche aujourd'hui | événement |
|---|---|---|
| Chicoutimi | PQ | partielle après la démission d'Andrée Laforest (2025-09-04) |
| Dubuc | IND | défection |
| Orford | IND | défection |
| Rimouski | PCQ | adhésion, après CAQ → IND le 2025-09-18 |

Aucune n'est un défaut d'extraction. Elles se fermeront d'elles-mêmes quand
l'ANQ publiera la chronologie 2026.

**Ce que ça impose au modèle :** deux sources, deux rôles.

- La **chronologie** date les événements — jusqu'à la fin de l'année précédente.
- L'**index des députés** donne l'état du jour — sans aucune date.

L'année en cours n'a donc que le second : on connaît le *quoi*, jamais le
*quand*. Une transition déduite de l'index seul doit rester
`confidence=single_source` et **ne peut pas** porter de date de changement
crédible. La rattraper consiste à re-générer une fois la chronologie publiée,
pas à deviner une date entre-temps.

C'est aussi pourquoi le générateur ne doit jamais être lancé « une fois pour
toutes » : il se relit chaque année.

### Le repli Wikipedia, branché le 2026-08-13

Wikipedia tient la page de la législature **au fil de l'eau**, avec des
événements datés. Elle donne donc ce qui manque à l'index : le *quand*.
`evenements_de_repli()` la lit quand la page de l'ANQ est encore un gabarit, et
ferme les quatre divergences du tableau ci-dessus — 124/125 sièges couverts →
**125/125**, 4 divergences de parti → **1** (Bonaventure, antérieure et sans
rapport).

Trois garde-fous, parce que Wikipedia reste un candidat et jamais une vérité
(§ 6) :

1. **Le repli ne se déclenche que si l'ANQ n'a pas publié.** Le jour où elle
   publie, elle reprend la main sans qu'on touche au code.
2. **Il ne couvre que les années que la chronologie n'a pas datées.** La borne
   porte sur les années réellement rendues par la chronologie, **jamais sur un
   calcul à partir du numéro de page** — la tentation `chronoN = N + 1908` est
   fausse (elle donne 2024 pour `chrono116`) et faisait rejouer deux années
   déjà publiées, chaque événement comptant double.
3. **Une puce sans `<ref>` vérifiable est écartée.** Les mandats retenus
   sortent en `confidence=single_source`, avec `source` préfixée `wp+ref:`.
   Ils sont donc repérables et destinés à être re-générés depuis l'ANQ.

Le siège est résolu **par personne**, contre l'index du jour : une phrase
Wikipedia peut nommer la circonscription qu'une députée *brigue* plutôt que
celle qu'elle occupe. Sans l'index, on n'infère rien.

`--sans-wikipedia` désactive le repli, pour les régénérations reproductibles.

## 6 ter. Quatre pièges du texte, tous silencieux

Trouvés en relisant à la main les 31 mandats classés `disputed`. Aucun ne levait
d'exception, ne violait un invariant, ni ne rendait une table suspecte. Ils sont
consignés ici parce que **la classe se reforme** dès qu'on ajoute un motif.

**A. Une démission réclamée n'est pas une démission.** La chronologie rapporte
les pétitions dans les mêmes mots que les départs. « *Les signataires demandent
la démission du député de Sherbrooke, Jean Charest* » (2011-02-16) fermait son
siège — Charest y a siégé jusqu'en septembre 2012. **19 mois de parole d'un
premier ministre en exercice, effacés.** À Anjou, la phrase dit même que le
président *refuse* la pétition.

**B. La chronologie date l'annonce, la phrase porte l'effet.** « *annonce sa
démission comme député. Celle-ci sera effective le 15 avril* », entrée datée du
14 mars. Trois à cinq semaines retirées à quelqu'un qui siège encore.

**C. Un paragraphe parle souvent de plus d'une personne.** « *La députée de
Kamouraska-Témiscouata, France Dionne, **et le député de Bourassa**, Yvon
Charbonneau, démissionnent* » — Bourassa était perdu. Même classe que les
partielles multiples ; on ne l'avait pas cherchée du côté des démissions.

**D. Une date en tête de paragraphe fait foi.** L'ANQ fusionne parfois la date
et l'événement dans un même `<p>` : l'entrée héritait alors du dernier en-tête
vu, **et tout ce qui suivait sur la page aussi**. Sur 1 377 paragraphes, 16
commencent par une date complète et **14 contredisent l'en-tête** — dont une
partielle à trois sièges datée du 14 octobre au lieu du 9 novembre 2015.

### La règle qui se dégage des quatre

Deux d'entre eux venaient d'une capture trop large qui avalait la suite de la
phrase (`député de Rivière-du-Loup **et de chef de l'ADQ**` → un siège
inexistant, donc une démission **jamais enregistrée** — une démission ne fait
que *modifier* un mandat, et un siège introuvable n'est simplement pas modifié).

Énumérer les charnières ne marche pas : corriger « et » laisse passer « lors
de », puis « à l'issue de ». **Il faut une propriété du nom lui-même**, pas une
liste de tournures — une circonscription est faite de mots capitalisés liés par
des traits d'union, et ne contient jamais « et ».

Effet mesuré sur les 1 220 mandats : **24 écarts, tous des corrections**,
invariants et réconciliation inchangés. La file `disputed` monte de 31 à 42 —
elle grossit parce qu'on détecte enfin des démissions qu'on manquait.

## 6 quater. Arbitrer par recoupement, pas à la main

43 mandats sortaient en `confidence=disputed` : la chronologie dit
« démissionne », sans qu'on sache toujours si c'est le **siège** ou une
**fonction** qu'on quitte. Les faire trancher un par un ne compose pas — la
question se reposerait identique à la prochaine régénération.

L'historique par circonscription de l'ANQ répond exactement à cette
question. Sa colonne *Remarques* porte **219 démissions datées**
(« démissionne le 08-03-2001 »), et un registre des titulaires par siège dit
précisément quand un siège devient vacant.

| | |
|---|---|
| **25** confirmées | les deux sources donnent la même date → `verified` |
| **16** redatées | l'ANQ l'emporte |
| **14** trouvées | démissions qu'on ne voyait pas du tout |
| **14** restantes | l'ANQ ne les date pas |

**La chronologie date l'annonce, l'ANQ date la vacance.** Lucien Bouchard
annonce son départ le 2001-01-11 et quitte son siège le 2001-03-08. Ce sont
deux faits distincts, et c'est le second qui borne un mandat. Le premier
arbitrage ne savait que **raccourcir** un mandat — il cherchait le mandat
couvrant la date de l'ANQ, et quand notre date était trop tôt, celle de l'ANQ
tombait dans le vide laissé avant la partielle. Il corrige désormais dans les
deux sens.

Sur les 14 restantes, **13 sont corroborées indirectement** : une partielle
suit dans un délai plausible. La seule qui ne l'est pas — Jacques Parizeau,
L'Assomption, 1995-10-31 — porte la date de l'annonce, et aucune source dont
nous disposons ne date son départ réel.

### Trois défauts que l'arbitrage a révélés

**Le siège doit être l'OBJET de la démission.** « Démission **du président de
l'Assemblée nationale** Yvon Vallières, député de Richmond » : il quitte la
présidence et garde son siège dix-sept mois. Trouver le mot « député » après le
verbe ne suffit pas — encore faut-il qu'il soit introduit comme objet
(« du député », « à titre de député »). Sans fonction concurrente, l'appositif
suffit : « Jean-Pierre Bélisle, député libéral de Mille-Îles, annonce sa
démission en Chambre » ne nomme rien d'autre.

**Une énumération n'est lue qu'en entier.** « élus respectivement dans
Beauce-Sud, Fabre et Saint-Henri-Sainte-Anne » : une seule circonscription suit
« dans », les autres sont séparées par des virgules. Deux partielles sur trois
étaient perdues, et Saint-Henri-Sainte-Anne restait vide **trois ans**.

**Rejoindre le parti qu'on a déjà n'est pas un changement.** Monique Simard se
retire du caucus du PQ puis le réintègre ; seule l'arrivée était vue, et elle
scindait le mandat en deux moitiés identiques dont la seconde héritait de
l'identité du **titulaire précédent**.

## 7. Réserve

« Exhaustif » n'est atteignable que pour ce que les sources **consignent**.
Les compositions détaillées de l'ANQ sont fréquentes depuis 2010, rares avant.
Pour les législatures 35-38 (1994-2007), l'invariant n° 3 aura donc moins de
points d'ancrage et la vérification y sera plus faible. **Il faut le dire**
plutôt que de laisser croire à une couverture uniforme — c'est précisément à
ça que sert `confidence`.
