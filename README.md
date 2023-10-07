
# Introduction
Ce projet consiste en l'élaboration d'un tableau de bord afin de visualiser les éléments pertinents d'un jeu de données que l'on a choisi.

Il a été réalisé dans le cadre de l'unité d'enseignement IF36 - Conception de visualisations de données, enseigné à l'Université de Technologie de Troyes.
Il a pour objectif d'appliquer les notions vu en cours sur un cas pratique. Nous avons alors eu l'occasion d'utiliser les techniques et méthodes abordées durant notre formation. 

Notre équipe est formée de :
- Antoine-Valentin Charpentier
- Chloé Martin
- Marc-Arthur Kadangha
- Mathieu Le Treust

## Sommaire :
1. [Données](#titre1)  
    A. [Provenance](#titre2)  
    B. [Présentation des tables](#titre3)  
    C. [Description des tables](#titre5)
2. [Plan d'analyse](#titre4)

<span id="titre1"></span>
## I-Données
<span id="titre2"></span>
### A-Provenance

Le dataset que nous avons choisi provient d'Ergast Developer API. L’API Ergast Developer est un service Web expérimental qui fournit un enregistrement historique des données de course automobile à des fins non commerciales. L’API fournit des données pour la série de Formule 1, depuis le début des championnats du monde en 1950. 

Vous pouvez consulter le dataset directement à partir de ce lien:
[Lien vers la dataset](https://ergast.com/mrd/db/)

Le dataset est un ensemble de 14 fichiers CSV issu d'une base de données MySQL pour un total de 19,2 Mo. Il est téléchargeable sous plusieurs formats: 
- [Tables de base de données CSV](http://ergast.com/downloads/f1db_csv.zip)
- [Images de base de données MySQL](http://ergast.com/downloads/f1db.sql.gz)

Les images de la base de données sont  mises à jour très régulièrement. Par soucis de simplicité, nous figerons le dataset à la date de début du projet (15/03/2023). 

Nous avons choisi d'analyser ce dataset sur la Formule 1 car c'est un sujet qui présente de nombreux avantages. Tout d'abord, la F1 est un sport très populaire, ce qui en fait un sujet intéressant. De plus, il s'agit d'un sport bien documenté, avec de nombreuses données collectées en permanence, ce qui facilite l'analyse et la visualisation des données. Enfin, les compétitions de F1 offrent de nombreuses possibilités d'analyse, comme l'étude des performances des équipes et des pilotes, l'impact des différentes stratégies de course et bien d'autres encore.

<span id="titre3"></span>
### B-Présentation des tables

Pour commencer, voici une brève liste des tables de la base de données. 

Elle contient la liste des saisons de F1, le planning des courses, leurs résultats, les résultats des qualifications et ceux des sprint. On retrouve également le classement général des conducteurs ou des constructeurs par saisons ou par course. Des tables détaillent les informations sur les conducteurs, les informations sur les constructeurs et les informations sur les circuits. L'API fournit pour ce dataset une liste de tous les codes d’état de finition afin de lister par exemple le statut d’arrivée d’un pilote spécifique dans une saison particulière. Ensuite, on retrouve une table concernant les temps au tour pour chaque conducteur et pour chaque course (données disponibles à partir de la saison 1996). Enfin, les données sur les arrêts aux stands sont disponibles pour chaque conducteur et pour chaque course à partir de la saison 2012.

|Liste des Tables      |
|---------------------:|
| circuits             |
| constructorResults   |
| constructorStandings |
| constructors         |
| driverStandings      |
| drivers              |
| lapTimes             |
| pitStops             |
| qualifying           |
| races                |
| results              | 
| seasons              |  
| status               | 

| General Notes                                                    |
|-----------------------------------------------------------------:|
| Dates, times and durations are in ISO 8601 format                |
| Dates and times are UTC                                          |
| Strings use UTF-8 encoding                                       |
| Primary keys are for internal use only                           |
| Fields ending with "Ref" are unique identifiers for external use |
| A grid position of '0' is used for starting from the pitlane     |
| Labels used in the positionText fields:                          |
|   "D" - disqualified                                             |
|   "E" - excluded                                                 |
|   "F" - failed to qualify                                        |
|   "N" - not classified                                           |
|   "R" - retired                                                  |
|   "W" - withdrew                                                 |


Les relations entre les différentes tables sont illustrées à l'aide d'un diagramme de relation d’entité et sont expliquées plus en détails dans le Guide de l’utilisateur.
![Lien vers la relation d'entité](http://ergast.com/images/ergast_db.png)

Le guide de l'utilisateur peut être consulté ici :
[Lien vers le guide de l'utilisateur](https://ergast.com/docs/f1db_user_guide.txt)

Nous n'avons pas un nombre fixé d'individus dans chaque fichier csv. Voila donc une description pour chaque table. Pour chacune d'entre elles, nous nous sommes tenus d'expliciter le nombre d'individus, le nombre de variables,  leur type, et une description de chacune d'elle.

NB: Dans le cadre du projet nous serons amenés à effectuer des jointures sur les différentes tables afin d'apporter des visualitions pertinentes qui nous l'espérons permettront de répondre aux questions que nous allons détailler plus bas.

<span id="titre5"></span>
### Description des tables

Table d'origine  : **seasons** , 74 individus , 2 variables.  
Description de la table  : Cette table nous donne une liste des saisons actuellement prises en charge par l’API (1950 à 2023). L'URL réfère sur l'année et renvoie une page Wikipédia pour fournir plus d'informations.

| Variable |   Type | Description                 |
| -------: | -----: | --------------------------: |
|     year |    int | Année de la saison          |
|      url | string | Page Wikipédia de la saison |


Table d'origine  : **races** , 1102 individus , 18 variables.  
Description de la table  : Cette table nous donne les informations sur le calendrier des courses. On peut voir par exemple le calendrier des courses pour une certaine saison, ou encore le calendrier des courses effectuées par certains conducteurs.

|    Variable |   Type | Description                                   |
| ----------: | -----: | --------------------------------------------: |
|      raceId |    int | ID de la course                               |
|        year |    int | ID de la saison, relié à la table **seasons** |
|       round |    int | Numéro de course dans la saison               |
|   circuitId |    int | ID du circuit, relié à la table **circuit**   |
|        name | string | Nom de la course                              |
|        date | string | Date de la course                             |
|        time | string | Heure de départ de la course                  |
|         url | string | Page Wikipédia sur la course                  |
|    fp1_date | string | Date d'entrainement n°1                       |
|    fp1_time | string | Horaire de départ de l'entrainement n°1       |
|    fp2_date | string | Date d'entrainement n°2                       |
|    fp2_time | string | Horaire de départ de l'entrainement n°2       |
|    fp3_date | string | Date d'entrainement n°3                       |
|    fp3_time | string | Horaire  de départ de l'entrainement n°3      |
|  quali_date | string | Date des qualifications                       |
|  quali_time | string | Horaire de départ des qualifications          |
| sprint_date | string | Date du sprint                                |
| sprint_time | string | Horaire de départ du sprint                   |


Table d'origine  : **results** , 25860 individus , 18 variables. 
Description de la table  : Cette table répertorie les résultats d’une course spécifique. Si les résultats de la course spécifiée ne sont pas encore disponibles, l’élément RaceTable de la réponse sera vide.
La valeur de l’attribut position dans l’élément Result est toujours un entier, donnant l’ordre de finition de tous les pilotes. La valeur de l’attribut positionText est un entier (position de fin), « R » (retiré), « D » (disqualifié), « E » (exclu), « W » (retiré), « F » (non qualifié) ou « N » (non classé). De plus amples informations sont fournies par l’élément status. Une valeur de position sur la grille de '0' indique que le pilote a démarré de la voie des stands.

|        Variable |   Type | Description                                                |
| --------------: | -----: | ---------------------------------------------------------: |
|        resultId |    int | ID du résultat                                             |
|          raceId |    int | ID de la course, relié à la table **races**                |
|        driverId |    int | ID du pilote, relié à la table **drivers**                 |
|   constructorId |    int | ID du constructeur, relié à la table **constructors**      |
|          number |    int | Numéro du pilote                                           |
|            grid |    int | Position sur la grille de départ                           |
|        position |    int | Classement de la course                                    |
|    positionText | string | Classement de la course (format texte)                     |
|   positionOrder |    int | Classement à des fins de commande                          |
|          points |  float | Point du pilote pour la course                             |
|            laps |    int | Nombre de tour complet                                     |
|            time | string | Temps pour le 1er et l'écart avec le 1er pour les suivants |
|    milliseconds |    int | Temps du circuit en miliisecondes                          |
|      fastestLap |    int | Numéro du tour le plus rapide                              |
|            rank |    int | Classement du tour le plus rapide avec les autres pilotes  |
|  fastestLapTime | string | Durée du tour le plus rapide                               |
| fastestLapSpeed | string | Vitesse du tour le plus rapide                             |
|        statusId |    int | ID du statut du pilote, relié à la table **statuts**       |



Table d'origine  : **constructor_results** , 12180 individus , 5 variables  
Description de la table  : Cette table nous donne le résultat des constructeurs par course spécifiée.

|             Variable |  Type | Description                                          |
| -------------------: | ----: | ---------------------------------------------------: |
| constructorResultsId |   int | Id sur les résultats des constructeurs               |
|               raceId |   int | Id de la course, relié à la table **races**          |
|        constructorId |   int | Id du constructeur, relié à la table **constructor** |
|               points | float | Points obtenus par le constructeur sur la course     |
|               status |   int | "D" pour un disqualification sinon NULL              |


Table d'origine  : **sprint_results** , 120 individus , 16 variables . 
Cette table-ci nous donne des résultats des courses pour chaque conducteur avec ses performances.
|        Variable |   Type | Description                                                |
| --------------: | -----: | ---------------------------------------------------------: |
|        resultId |    int | ID du résultat du sprint                                   |
|          raceId |    int | ID de la course, relié à la table **races**                |
|        driverId |    int | ID du pilote, relié à la table **drivers**                 |
|   constructorId |    int | ID du constructeur, relié à la table **constructors**      |
|          number |    int | Numéro du pilote                                           |
|            grid |    int | Position sur la grille de départ                           |
|        position |    int | Classement officiel                                        |
|    positionText | string | Classement officiel (format texte)                         |
|   positionOrder |    int | Classement à des fins de commande                          |
|          points |  float | Point du pilote pour la course                             |
|            laps |    int | Nombre de tour complet                                     |
|            time | string | Temps pour le 1er et l'écart avec le 1er pour les suivants |
|    milliseconds |    int | Temps du circuit en miliisecondes                          |
|      fastestLap |    int | Numéro du tour le plus rapide                              |
|  fastestLapTime | string | Durée du tour le plus rapide                               |
|        statusId |    int | ID du statut du pilote, relié à la table **statuts**       |


Table d'origine  : **circuit** , 77 individus , 9 variables
C'est une table qui permet de connaître diverses informations sur les circuits de course.
|   Variable |   Type | Description                                        |
| ---------: | -----: | -------------------------------------------------: |
|  circuitId |    int | ID du circuit                                      |
| circuitRef | string | Identifiant unique du circuit (ex : "albert_park") |
|       name | string | Nom du circuit                                     |
|   location | string | Localisation du circuit                            |
|    country | string | Pays du circuit                                    |
|        lat |  float | Latitude                                           |
|        lng |  float | Longitude                                          |
|        alt | string | Altitude du circuit en mètres                      |
|        url | string | Page wikipédia du circuit                          |


Table d'origine  : **constructors** , 211 individus , 5 variables
Cette table présente les constructeurs automobiles
|       Variable |   Type | Description                                       |
| -------------: | -----: | ------------------------------------------------: |
|  constructorId |    int | ID du constructeur                                |
| constructorRef | string | Identifiant unique du constructeur (ex : mclaren) |
|           name | string | Nom du constructeur                               |
|    nationality | string | Nationalité du constructeur                       |
|            url | string | Page Wikipédia du constructeur                    |



Table d'origine  : **constructor_standings** , 12941 individus , 7 variables  
Cette table-ci nous donne des informations sur pour chaque constructeur sur les courses qu'il a effectué.
|               Variable |               Type | Description                                             |
| ---------------------: | -----------------: | ------------------------------------------------------: |
| constructorStandingsId |                int | ID du classement constructeur                           |
|                 raceId |                int | ID de la course, relié à la table **races**             |
|          constructorId |                int | ID du constructeur, relié à la table **constructors**   |
|                 points |              float | Nombre de point du pilote sur la saison                 |
|               position |                int | Classement du constructeur sur la course                |
|           positionText |             String | Classement du constructeur sur la course (format texte) |
|                    win | int (like boolean) | Gagnant ou Perdant de la course                         |


Table d'origine  : **drivers** , 857 individus , 9 variables  
Cette table-ci nous donne des informations sur les conducteurs.
|            Variable |   Type | Description                                            |
| ------------------: | -----: | -----------------------------------------------------: |
|            driverId |    int | ID du pilote                                           |
|           driverRef | string | Identification unique du pilote (ex : "hamilton")      |
|              number | string | Numero du pilote courante (ce n'est pas le classement) |
|                code | String | Code du pilote (ex : "HAM")                            |
|            forename | String | Prénom du pilote                                       |
|             surname | String | Nom du pilote                                          |
| dob (date of birth) | string | Date de naissance du pilote                            |
|         nationality | String | Nationalité du pilote                                  |
|                 url | string | Page Wikipédia du pilote                               |


Table d'origine  : **drivers_standings** , 33902 individus , 7 variables .
Cette table-ci nous donne des informations pour chaque conducteur sur les courses qu'il a effectué.
|          Variable |          Type | Description                                 |
| ----------------: | ------------: | ------------------------------------------: |
| driverStandingsId |           int | ID du classement pilote                     |
|            raceId |           int | ID de la course, relié à la table **races** |
|          driverId |           int | ID du pilote, relié à la table **drivers**  |
|            points |         float | Nombre de points du pilote sur la saison    |
|          position |           int | Classement du pilote sur la course          |
|      positionText |        String | Classement du pilote (format texte)         |
|              wins | int (boolean) | Gagnant ou perdant de la course             |


Table d'origine  : **laps_time** , 539171 individus , 6 variables  
Cette table-ci nous donne des informations à chaque tour de course sur chaque conducteur.
|     Variable |   Type | Description                                 |
| -----------: | -----: | ------------------------------------------: |
|       raceId |    int | ID de la course, relié à la table **races** |
|     driverId |    int | ID du pilote, relié à la table **drivers**  |
|          lap |    int | Numéro du tour                              |
|     position |    int | Position du pilote                          |
|         time | string | Durée du tour                               |
| milliseconds |    int | Durée du tour en millisecondes              |



Table d'origine  : **pit_stop** , 9684 individus , 7 variables  
Cette table-ci nous donne des informations sur l'arrêt effectué par un conducteur auprès de son équipe (repos , rafraîchissement) pour chaque tour de course 
|     Variable |   Type | Description                                 |
| -----------: | -----: | ------------------------------------------: |
|       raceId |    int | ID de la course, relié à la table **races** |
|     driverId |    int | ID du pilote, relié à la table **drivers**  |
|         stop |    int | Numéro de l'arrêt                           |
|          lap |    int | Numéro de tour                              |
|         time | string | Temps auquel le driver s'arrête             |
|     duration | string | Durée de l'arrêt                            |
| milliseconds |    int | Durée de l'arrêt en millisecondes           |



Table d'origine  : **qualifying** , 9595 individus , 9 variables . 
Cette table-ci nous donne des informations pour chaque conducteur ayant participé avec leur rang . (q1,q2 et q3 sont des sessions de la course , les 15 plus rapides vont à q2 et les 10 plus rapides à q3)
|      Variable |   Type | Description                                           |
| ------------: | -----: | ----------------------------------------------------: |
|     qualifyId |    int | ID de qualification                                   |
|        raceId |    int | ID de la course, relié à la table **races**           |
|      driverId |    int | ID du pilote, relié à la table **drivers**            |
| constructorId |    int | ID du constructeur, relié à la table **constructors** |
|        number |    int | Numero du pilote                                      |
|      position |    int | Position lors des phases de qualifications            |
|            q1 | string | Meilleur temps sur un tour en q1                      |
|            q2 | string | Meilleur temps sur un tour en q2                      |
|            q3 | string | Meilleur temps sur un tour en q3                      |


Table d'origine  : **status** , 139 individus , 2 variables . 
Cette table-ci nous donne la liste des états possibles pour les conducteurs. (qualifié, blessé etc.)
| Variable |   Type | Description   |
| -------: | -----: | ------------: |
| statusId |    int | ID par statut |
|   status | string | Statut texte  |

<span id="titre4"></span>
## II-Plan d'analyse

Nous avons décidé d'aborder ce dataset à l'aide de la problématique générale suivante : Comment les performances des constructeurs et des pilotes de Formule 1 ont-elles évoluées au fil des années ?



Afin de répondre de manière pertinente, nous souhaitons commencer l'analyse par l'étude du circuit. Par exemple, il sera possible d'évoquer le localisation des circuit et ainsi voir si les choix de circuit ont été modifiés pour obtenir des performances plus impressionnantes. Voici un ensemble de questions concernant l'**étude du circuit** : 

- Quels sont les circuits les plus pratiqués ?
- Comment sont répartis les circuits dans le monde ?
- Y a-t-il des tendances ou des schémas dans les résultats des courses ?
- La vitesse de pointe des circuits, est-elle impactée par la localisation géographique du circuit et / ou par la période ou la course à lieu ?
- Le nombre d'accident est-il en lien avec la vitesse de pointe sur un circuit ? Quels sont les circuits avec le moins d'accident ?
- Y a-t-il des circuits avec plus d'accidents que d'autres ? 
- Quelle est la proportion de chaque type d'accidents parmi l'ensemble des accidents, les plus récurrents ?
- Y a-t-il des corrélations entre les performances des équipes et des pilotes, et des facteurs tels que le budget ou les caractéristiques du circuit ?


Par la suite, nous souhaitons réduire le champ d'étude en nous renseignant sur les constructeurs. Voici un ensemble de questions concernant l'**étude des constructeurs** :

- Comment les règles et les réglementations ont-elles influencées les performances des constructeurs et des pilotes ?
- Quels constructeurs dominent le classement depuis 1950 ?
- Quelle est l'évolution de la vitesse de pointe des conducteurs en fonction du circuit et en fonction de la période ?
- Quel est le constructeur avec les pilotes les plus rapides ?
- Un constructeur, est-il plus à l'aise sur un circuit qu'un autre ? Il y a une corrélation entre un constructeur et un circuit ?



Enfin, nous terminerons par l'étude des pilotes et leurs performances. Voici un ensemble de questions concernant l'**étude des pilotes** :

- Comment ont évolué les temps de qualification et de course au fil du temps ?
- Nous essayerons de déterminer quels sont les circuits qu'un conducteur préfère.
- Quel est le pilote / constructeur avec le plus de récompense sur un circuit ?
- Quels sont les pilotes avec les plus de victoire durant leur période ?
- Quels sont les pilotes les plus qualifiés dans leur carrière ?
- Il y a-t-il une corrélation entre le nombre d'arrêts au Pit et le résultat à une compétition ?
- La durée des arrêts, dépend-elle du circuit ou du pilote ? 
- La phase de qualication a-t-elle un impact sur le résultat de la course ?
- Quel pays sort le plus de pilote par année en moyenne ?
