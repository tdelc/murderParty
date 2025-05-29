#' Lancer le serveur Habemus Papam (ou Dieu est mort)
#'
#' @description
#' `lancer_serveur_habemus()` permet de lancer, à partir d'un fichier de données de serveur, l'interface web pour Habemus Papam.
#'
#' @param path_server lien vers le serveur
#' @param id_chatgpt (facultatif) credential de chatGPT
#'
#' @returns exécute l'interface shiny
#' @export
#'
#' @examples
#' # Exemple avec les fichiers par défaut
#' creer_serveur_habemus('server_habemus_test')
#' lancer_serveur_habemus("server_habemus_test")
lancer_serveur_habemus <- function(path_server,id_chatgpt=NULL){

  if (stringr::str_sub(path_server,-3,-1) != "csv")
    path_server <- paste0(path_server,".csv")

  path_server <- normalizePath(path_server)
  actions <- read_csv(path_server)
  if (is.null(actions) | nrow(actions) == 0) {
    stop("Aucune db")
  }

  options(path_server = path_server)
  options(id_chatgpt = id_chatgpt)
  options(actions = actions)
  shiny::runApp(system.file('dieu-habemus', package='murderParty'))
}

#' @rdname lancer_serveur_habemus
#' @export
lancer_serveur_dieu <- function(...) {
  lancer_serveur_habemus(...)
}
