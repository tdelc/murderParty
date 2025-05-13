#' Lancer le serveur pour Habemus Papam
#'
#' @param server_name
#' @param actions
#'
#' @returns
#' @export
#'
#' @examples
lancer_serveur_habemus <- function(server_name){

  server_name <- paste0(server_name,".csv")
  options(habemus.server_name = server_name)

  actions <- read_csv(server_name)
  if (is.null(actions) | nrow(actions) == 0) {
    stop("Aucune db")
  }

  options(habemus.actions = actions)
  shiny::runApp(system.file('habemus', package='murderParty'))

}

creer_serveur_habemus <- function(server_name, ...){

  server_name <- paste0(server_name,".csv")
  write.csv(creation_server_habemus(...),file=server_name)

}
