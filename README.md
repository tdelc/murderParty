
# murderParty

<!-- badges: start -->
<!-- badges: end -->

Bienvenue dans l'interface informatique permettant de gérer les indices durant
la murder party Habemu Papam. Comme vous avez pu le lire dans les règles de jeu, 
les personnages ont la possibilité d’obtenir des indices via des enquêtes. 

Vous pouvez gérer ces actions vous mêmes (auquel cas vous devez imprimer les 
indices en plusieurs exemplaires ainsi que des points d’indices, et réaliser 
les actions) mais cela prend beaucoup de temps (nous vous conseillons dans ce 
cas d’être deux). 

Vous pouvez également vous munir d’ordinateurs gérant cette tâche à votre place. 
Deux ou trois ordinateurs sont nécessaires dans ce cas, répartis sur l’ensemble 
de l’espace de jeu. Vous n’avez rien à installer pour les ordinateurs devant 
gérer les indices, toutes les actions sont réalisées avec cette interface web.

Le package murderParty propose une suite de fonctions et d’interfaces permettant de gérer informatiquement une murder party scénarisée, ici dans le cadre du scénario "Habemu Papam" ou "Dieu est mort". Il gère :

- la création du serveur d’indices et des bases de données,
- la gestion des indices et des enquêtes par les joueurs,
- la copie/interception d’indices,
- la messagerie in-game,
- la gestion des points d’action,
- un système de classement évolutif,
- une interface web Shiny pour l’animation en live.

Ce package vise à faciliter l’animation, à alléger la charge des orgas, et à rendre le jeu plus fluide pour les joueur·euses.

## Installation

Vous pouvez installer la version de développement à partir de [GitHub](https://github.com/) avec :

``` r
# install.packages("devtools")
devtools::install_github("tdelc/murderParty")
```

## Example

Voici comment lancer l'interface graphique pour la murder party Dieu est mort :

``` r
library(murderParty)
creer_serveur_dieu('server_dieu_test')
lancer_serveur_dieu("server_dieu_test")
```

Attention, cela va créer un fichier 'server_dieu_test.csv' dans votre répertoire par défaut.
Vous pouvez le modifier pour pouvoir changer la configuration initale de la murder party.

Voici comment lancer l'interface graphique pour la murder party Dieu est mort :

``` r
library(murderParty)
creer_serveur_habemus('server_habemus_test')
lancer_serveur_habemus("server_habemus_test")
```

Il est possible de créer une murder party personalisée en créant un autre jeu de fichiers "ui.r" et "server.r" provenant du répertoire "/inst/habemus". Il faut pour cela maîtriser R et le package shiny, mais l'ensemble des fonctions actuelles peut être rendu générique pour d'autres usage.

## Auteur
Développé par Thomas Delclite
