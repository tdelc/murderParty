#' Fonction de création BDD dés
#'
#' @param actions
#' @param bonus_recherche
#'
#' @returns
#' @export
#'
#' @examples
creation_bdd_des <- function(actions,bonus_recherche=0){

  # Extraire la liste des résultats
  rate <- actions[actions$cible == 1,]$PA
  ok <- actions[actions$cible == 2,]$PA
  parfait <- actions[actions$cible == 3,]$PA

  rate <- rate[!is.na(rate)]
  ok <- max(ok[!is.na(ok)]-bonus_recherche,0)
  parfait <- parfait[!is.na(parfait)]+bonus_recherche

  # Prendre le dernier r?sultat
  valeur_alea <- tibble(resultat=numeric(),valeur=numeric()) %>%
    add_row(resultat=1,valeur=rate[length(rate)]) %>%
    add_row(resultat=2,valeur=ok[length(ok)]) %>%
    add_row(resultat=3,valeur=parfait[length(parfait)]) %>%
    mutate(pc=valeur/sum(valeur),
           label = case_when(resultat == 1 ~ "Ratée",
                             resultat == 2 ~ "Réussie",
                             resultat == 3 ~ "Parfaite",
                             TRUE ~ "Erreur"))

  return(valeur_alea)
}

#' Fonction de graphique pour le dé
#'
#' @param valeur_alea
#'
#' @returns
#' @export
#'
#' @examples
plot_bdd_des <- function(valeur_alea){
  ggplot2::ggplot(valeur_alea) +
    ggplot2::aes(x = pc, y = reorder(label, pc), fill = label) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(
      label, " : ",round(pc*100,1), "%")),
      position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::scale_fill_manual(values = c("Parfaite" = "palegreen",
                                          "Ratée" = "orangered",
                                          "Réussie" = "skyblue")) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_void()+
    ggplot2::theme(legend.position = "none")+
    ggplot2::labs(title = "Probabilité de succès \nde l'enquête")
}
