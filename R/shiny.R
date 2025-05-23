#' Launches the Shiny application for the 'Habemus Papam' murder mystery game.
#'
#' This function sources the UI and server logic from the 'inst/habemus'
#' directory, reads initial game data, and launches the Shiny web application.
#'
#' @param game_data_path The full path to the CSV file containing the game
#'   actions and state (e.g., "my_game_server.csv").
#'
#' @returns No return value; called for the side effect of launching a Shiny
#'   application.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Define a path for a temporary game data CSV file
#'   # temp_game_file <- tempfile(fileext = ".csv")
#'   #
#'   # Define player list for the game (example structure)
#'   # example_joueurs <- list(
#'   #   list(nom = "Alice", role = "Cardinal A", password = "passwordAlice", PA = 10),
#'   #   list(nom = "Bob", role = "Cardinal B", password = "passwordBob", PA = 10)
#'   # )
#'   #
#'   # First, create a server file (if it doesn't exist)
#'   # creer_serveur_habemus(temp_game_file, list_joueurs = example_joueurs)
#'   #
#'   # Then, launch the server using the path to the created file
#'   # lancer_serveur_habemus(temp_game_file)
#'   #
#'   # Clean up the temporary file after closing the app
#'   # if (file.exists(temp_game_file)) file.remove(temp_game_file)
#' }
lancer_serveur_habemus <- function(game_data_path) {

  if (!file.exists(game_data_path)) {
    stop("Specified game_data_path does not exist: ", game_data_path)
  }

  # Read initial actions from the CSV file.
  # Using readr::read_csv for consistency with potential tidyverse use elsewhere.
  # Ensure readr is listed in Imports in DESCRIPTION.
  initial_actions <- readr::read_csv(game_data_path, show_col_types = FALSE)
  if (is.null(initial_actions) || nrow(initial_actions) == 0) {
    stop("No data found in the game data file or file is empty: ", game_data_path)
  }

  # Source the UI and Server factory functions from the inst/habemus directory
  # local = TRUE ensures they are sourced into a new environment, not polluting global.
  # The $value is used because source() returns the result of the last expression evaluated,
  # which should be the function definition if that's the last thing in the file.
  app_ui_function <- source(system.file('habemus', 'ui.R', package = 'murderParty'), local = TRUE)$value
  app_server_factory <- source(system.file('habemus', 'server.R', package = 'murderParty'), local = TRUE)$value

  if (!is.function(app_ui_function)) {
    stop("Failed to load UI function from inst/habemus/ui.R")
  }
  if (!is.function(app_server_factory)) {
    stop("Failed to load server factory function from inst/habemus/server.R")
  }
  
  # Instantiate the UI
  actual_ui <- app_ui_function()

  # Instantiate the server function with the loaded data and game path
  actual_server_function <- app_server_factory(initial_actions, game_data_path)

  # Launch the Shiny app
  shiny::shinyApp(ui = actual_ui, server = actual_server_function)
}

#' Creates the initial server CSV file for a 'Habemus Papam' game.
#'
#' This function generates the necessary data structure for a new game
#' and saves it to a CSV file.
#'
#' @param game_data_path The full path where the server's CSV file will be
#'   created (e.g., "my_game_server.csv").
#' @param ... Additional arguments passed to `creation_server_habemus`
#'   (e.g., `list_joueurs` as defined in `creation_server_habemus`).
#'
#' @returns No return value; called for the side effect of writing a CSV file.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Define a path for a temporary game data CSV file
#'   # temp_game_file_create <- tempfile(fileext = ".csv")
#'   #
#'   # Define player list for the game (example structure)
#'   # example_joueurs_create <- list(
#'   #   list(nom = "Alice", role = "Cardinal A", password = "passwordAlice", PA = 10),
#'   #   list(nom = "Bob", role = "Cardinal B", password = "passwordBob", PA = 10)
#'   # )
#'   # creer_serveur_habemus(temp_game_file_create, list_joueurs = example_joueurs_create)
#'   #
#'   # Check if file was created
#'   # print(paste("File created:", file.exists(temp_game_file_create)))
#'   #
#'   # Clean up
#'   # if (file.exists(temp_game_file_create)) file.remove(temp_game_file_create)
#' }
creer_serveur_habemus <- function(game_data_path, ...){
  # The function now expects the full path including .csv
  # server_name_path <- paste0(server_name,".csv") # Original logic
  
  # Using utils::write.csv2 for consistency with server logic if it writes back.
  # creation_server_habemus(...) should return a data.frame.
  game_df <- creation_server_habemus(...)
  utils::write.csv2(game_df, file = game_data_path, row.names = FALSE)
}
