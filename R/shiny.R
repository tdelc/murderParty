#' Lancer le serveur Habemus Papam
#'
#' @description
#' `lancer_serveur_habemus()` permet de lancer, à partir d'un fichier de données de serveur, l'interface web pour Habemus Papam.
#'
#' @param server_name nom du serveur
#' @param id_chatgpt (facultatif) credential de chatGPT
#'
#' @returns exécute l'interface shiny
#' @export
#'
#' @examples
#' # Exemple avec les fichiers par défaut
#' creation_server_habemus('server_habemus_test')
#' creer_serveur_habemus("server_habemus_test")
lancer_serveur_habemus <- function(server_name,id_chatgpt=NULL){

  server_name <- paste0(server_name,".csv")
  actions <- read_csv(paste0("inst/dieu-habemus/",server_name))
  if (is.null(actions) | nrow(actions) == 0) {
    stop("Aucune db")
  }

  options(habemus.server_name = server_name)
  options(habemus.id_chatgpt = id_chatgpt)
  options(habemus.actions = actions)
  shiny::runApp(system.file('dieu-habemus', package='murderParty'))
}


