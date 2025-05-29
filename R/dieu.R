#' Créer le serveur Dieu est mort
#'
#' @description
#' `creer_serveur_dieu()` permet de créer un fichier csv avec les données de serveur pour Dieu est mort.
#'
#' @param server_name nom du serveur
#' @param df_init_dieu lien vers la base de données initiale (joueurs)
#' @param df_init_dieu_indices lien vers la base de données initiale (indices)
#' @param df_init_dieu_indices_dispo lien vers la base de données initiale (indices dispo)
#'
#' @returns data.frame avec toutes les infos de la murder party
#' @export
#'
#' @examples
#'
#' # Exemple avec les fichiers par défaut
#' creer_server_dieu('server_dieu_test')
creer_server_dieu <- function(
    server_name,
    path = "./"){

  df <- init_dieu %>%
    add_row(init_dieu_indices) %>%
    add_row(init_dieu_indices_dispo)

  server_name <- paste0(path,server_name,".csv")
  write_csv(df,file=server_name)
}
