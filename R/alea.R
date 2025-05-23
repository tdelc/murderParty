#' Calculate Dice Roll Outcome Probabilities
#'
#' Calculates probabilities for dice roll outcomes (failure, success, perfect success)
#' based on game actions and a potential search bonus.
#'
#' @param actions A data frame of game actions. Expected to contain columns:
#'   `cible` (numeric: 1 for failure, 2 for success, 3 for perfect success) and
#'   `PA` (numeric: action points or value associated with the outcome).
#' @param bonus_recherche A numeric value representing a bonus that increases
#'   the likelihood of 'success' and 'perfect success' outcomes, and decreases
#'   'failure'. Defaults to 0.
#'
#' @returns A tibble with columns:
#'   \describe{
#'     \item{resultat}{Numeric outcome category (1, 2, 3)}
#'     \item{valeur}{Adjusted value for the outcome after considering bonus}
#'     \item{pc}{Probability of the outcome (0 to 1)}
#'     \item{label}{Character label for the outcome ('Ratée', 'Réussie', 'Parfaite')}
#'   }
#' @export
#'
#' @examples
#' sample_actions <- data.frame(
#'   cible = c(1, 1, 2, 2, 3, 3),
#'   PA = c(10, 12, 5, 6, 2, 3),
#'   stringsAsFactors = FALSE
#' )
#' bdd_results <- creation_bdd_des(sample_actions, bonus_recherche = 1)
#' print(bdd_results)
#'
#' # Example with a different bonus
#' bdd_results_no_bonus <- creation_bdd_des(sample_actions)
#' print(bdd_results_no_bonus)
creation_bdd_des <- function(actions, bonus_recherche = 0) {

  # Extraire la liste des résultats
  rate <- actions[actions$cible == 1, ]$PA
  ok <- actions[actions$cible == 2, ]$PA
  parfait <- actions[actions$cible == 3, ]$PA

  rate <- rate[!is.na(rate)]
  ok <- max(ok[!is.na(ok)] - bonus_recherche, 0)
  parfait <- parfait[!is.na(parfait)] + bonus_recherche

  # Prendre le dernier résultat
  valeur_alea <- tibble::tibble(resultat = numeric(), valeur = numeric()) %>%
    dplyr::add_row(resultat = 1, valeur = rate[length(rate)]) %>%
    dplyr::add_row(resultat = 2, valeur = ok[length(ok)]) %>%
    dplyr::add_row(resultat = 3, valeur = parfait[length(parfait)]) %>%
    dplyr::mutate(
      pc = valeur / sum(valeur, na.rm = TRUE), # Added na.rm = TRUE for sum
      label = dplyr::case_when(
        resultat == 1 ~ "Ratée",
        resultat == 2 ~ "Réussie",
        resultat == 3 ~ "Parfaite",
        TRUE ~ "Erreur"
      )
    )

  return(valeur_alea)
}

#' Plot Dice Roll Outcome Probabilities
#'
#' Generates a ggplot bar chart visualizing the probabilities of dice roll
#' outcomes, typically calculated by `creation_bdd_des()`.
#'
#' @param valeur_alea A tibble, typically the output of `creation_bdd_des()`,
#'   containing outcome probabilities (column `pc`) and labels (column `label`).
#'   It should also have a column `label` for fill aesthetics.
#'
#' @returns A ggplot object representing the bar chart.
#' @export
#'
#' @examples
#' sample_actions_for_plot <- data.frame(
#'   cible = c(1, 1, 2, 2, 3, 3),
#'   PA = c(10, 12, 5, 6, 2, 3),
#'   stringsAsFactors = FALSE
#' )
#' bdd_plot_data <- creation_bdd_des(sample_actions_for_plot, bonus_recherche = 1)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_bdd_des(bdd_plot_data)
#' }
plot_bdd_des <- function(valeur_alea) {
  ggplot2::ggplot(valeur_alea) +
    ggplot2::aes(x = .data$pc, y = stats::reorder(.data$label, .data$pc), fill = .data$label) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(
      .data$label, " : ", round(.data$pc * 100, 1), "%")),
      position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::scale_fill_manual(values = c("Parfaite" = "palegreen",
                                          "Ratée" = "orangered",
                                          "Réussie" = "skyblue")) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = "Probabilité de succès \nde l'enquête")
}
