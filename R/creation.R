#' Création d'un fichier de serveur pour Habemus Papam
#'
#' @param df_init_habemus data.frame avec les infos initiales
#' @param df_init_habemus_indices data.frame avec les textes des indices
#' @param df_init_habemus_indices_dispo data.frame avec les indices de départ
#'
#' @importFrom magrittr %>%
#' @returns data.frame
#' @export
#'
#' @examples
creation_server_habemus <- function(
    df_init_habemus=init_habemus,
    df_init_habemus_indices=init_habemus_indices,
    df_init_habemus_indices_dispo=init_habemus_indices_dispo
    ){

  df_init_habemus %>%
    add_row(df_init_habemus_indices) %>%
    add_row(df_init_habemus_indices_dispo)
}
