#' Afficher un message dans l'interface
#'
#' @param title titre du message
#' @param message texte du message
#'
#' @returns modalBox
show_message <- function(title, message) {
  showModal(modalDialog(
    h3(title),
    span(message),
    footer = tagList(modalButton("OK"))
  ))
}

#' Charger les indices
#'
#' @param actions objet réactif contenant la base de données du serveur
#'
#' @returns data.frame avec les indices disponibles
info_indices <- function(actions){
  actions %>%
    filter(user == "admin" & action ==  "init") %>%
    group_by(cible,PA,resultat) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    select(titre = resultat,texte,variation=PA,indice=cible) %>%
    arrange(indice,variation)
}

#' Charger les user/password
#'
#' @param actions objet réactif contenant la base de données du serveur
#'
#' @returns data.frame avec les users et passwords
info_user <- function(actions){
  actions %>%
    filter(user == "admin" & action ==  "password") %>%
    group_by(cible) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    select(user=cible,password=resultat,PA)
}

#' Afficher la durée de manière propre
#'
#' @param diff_seconds durée en secondes
#'
#' @returns texte avec la durée
#' @export
#'
#' @examples
#' display_duration(1234)
display_duration <- function(diff_seconds) {
  diff_seconds <- as.numeric(diff_seconds)
  hours <- floor(diff_seconds / 3600)  # Calculer le nombre d'heures entières
  minutes <- floor((diff_seconds %% 3600) / 60)  # Calculer le nombre de minutes restantes
  label_hours <- ifelse(hours > 1,"heures","heure")
  label_minutes <- ifelse(minutes > 1,"minutes","minute")
  if (hours > 0 && minutes > 0) {
    paste(hours, label_hours, minutes, label_minutes)
  } else if (hours > 0) {
    paste(hours, label_hours)
  } else {
    paste(minutes, label_minutes)
  }
}

#' Préparer les timers
#'
#' @param timer timer à modifier
#'
#' @returns timer en meilleur format
#' @export
#'
#' @examples
#' prepare_timer(now())
prepare_timer <- function(timer){
  as.POSIXct(timer, origin = "1970-01-01",tz = "Europe/Paris")
}
