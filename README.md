
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

Cette url donne accès aux joueurs à une interface complète permettant d’envoyer 
des enquêteurs sur les indices disponibles dans leur feuille de personnage 
(et d’ajouter des points de pouvoir pour améliorer leur chance de réussite 
parfaite), de copier ou d’intercepter des enquêtes réalisées par les autres ou 
de s’envoyer des messages entre eux ou elles. Toutes ces actions sont expliquées 
directement dans l’interface. Pensez quand même à faire un tutoriel aux 
joueur.euse.s avant le début du jeu et/ou à les accompagner lors de leur 
première action.


Le package murderParty propose une suite de fonctions et d’interfaces permettant de gérer informatiquement une murder party scénarisée, ici dans le cadre du scénario "Habemu Papam". Il gère :

- la création du serveur d’indices et des bases de données,
- la gestion des indices et des enquêtes par les joueurs,
- la copie/interception d’indices,
- la messagerie in-game,
- la gestion des points d’action,
- un système de classement évolutif,
- une interface web Shiny pour l’animation en live.

Ce package vise à faciliter l’animation (en physique ou à distance), à alléger la charge des organisateurs, et à rendre le jeu plus fluide pour les joueurs.





## Installation

You can install the development version of murderParty from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tdelc/murderParty")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(murderParty)
## basic example code
```

