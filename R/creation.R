#' Creates the initial server data frame for a 'Habemus Papam' game.
#'
#' This function combines initial game settings, clue texts, and initially
#' available clues to form the starting data frame for a game instance.
#' This data frame is typically then saved as a CSV file to be used by
#' `creer_serveur_habemus`.
#'
#' @param df_init_habemus A data frame containing initial game parameters
#'   (e.g., player roles, initial PA). Defaults to `murderParty::init_habemus`.
#' @param df_init_habemus_indices A data frame containing the text and details
#'   of all possible clues. Defaults to `murderParty::init_habemus_indices`.
#' @param df_init_habemus_indices_dispo A data frame specifying which clues are
#'   available at the start of the game. Defaults to
#'   `murderParty::init_habemus_indices_dispo`.
#'
#' @importFrom magrittr %>%
#' @returns A data frame representing the initial state of the game server,
#'   ready to be written to a CSV file.
#' @export
#'
#' @examples
#' # Create server data using default internal datasets
#' server_data_default <- creation_server_habemus()
#'
#' # Example with custom initial data (conceptual)
#' # custom_init <- data.frame(...)
#' # custom_indices <- data.frame(...)
#' # custom_dispo <- data.frame(...)
#' # server_data_custom <- creation_server_habemus(
#' #   df_init_habemus = custom_init,
#' #   df_init_habemus_indices = custom_indices,
#' #   df_init_habemus_indices_dispo = custom_dispo
#' # )
#'
#' # Typically used internally by creer_serveur_habemus()
creation_server_habemus <- function(
    df_init_habemus=murderParty::init_habemus,
    df_init_habemus_indices=murderParty::init_habemus_indices,
    df_init_habemus_indices_dispo=murderParty::init_habemus_indices_dispo
    ){

  df_init_habemus %>%
    dplyr::add_row(df_init_habemus_indices) %>%
    dplyr::add_row(df_init_habemus_indices_dispo)
}
