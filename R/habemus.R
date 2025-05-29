#' Créer le serveur Habemus Papam
#'
#' @description
#' `creer_serveur_habemus()` permet de créer un fichier csv avec les données de serveur pour Habemus Papam.
#'
#' @param server_name nom du serveur
#' @param df_init_habemus lien vers la base de données initiale (joueurs)
#' @param df_init_habemus_indices lien vers la base de données initiale (indices)
#' @param df_init_habemus_indices_dispo lien vers la base de données initiale (indices dispo)
#'
#' @returns data.frame avec toutes les infos de la murder party
#' @export
#'
#' @examples
#'
#' # Exemple avec les fichiers par défaut
#' creation_server_habemus('server_habemus_test')
creer_serveur_habemus <- function(
    server_name,
    df_init_habemus="data-raw/habemus/init_habemus.csv",
    df_init_habemus_indices="data-raw/habemus/init_habemus_indices.csv",
    df_init_habemus_indices_dispo="data-raw/habemus/init_habemus_indices_dispo.csv"){

  df <- read_csv(df_init_habemus) %>%
    add_row(read_csv(df_init_habemus_indices)) %>%
    add_row(read_csv(df_init_habemus_indices_dispo))

  server_name <- paste0("inst/dieu-habemus/",server_name,".csv")
  write_csv(df,file=server_name)
}
