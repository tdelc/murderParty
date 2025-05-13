#' Envoyer un message à un ou des PJ
#'
#' @param actions
#' @param origine
#' @param destinataire
#' @param message
#' @param PA
#' @param delai
#'
#' @returns
#' @export
#'
#' @examples
envoi_message <- function(actions,origine,destinataire,message,PA,delai){

  timer_now <- Sys.time()

  if (destinataire == "Tout le conseil"){
    info_user <- info_user(actions)

    new_rows <- tibble(
      user=origine,action="chat",cible=info_user$user[1],PA=PA,
      timer=timer_now,resultat=message,timer_ok=timer_now+delai,texte="")

    for (pj in info_user$user[-1]){
      new_rows <- new_rows %>%
        add_row(user=origine,action="chat",cible=pj,PA=PA,
                timer=timer_now,resultat=message,timer_ok=timer_now+delai,texte="")
    }

    if (PA > 0){
      # Attention, trop de PA utilisé, création d'une ligne pour rajouter PA
      new_rows <- new_rows %>%
        add_row(user=origine,action="admin_PA",cible="",PA=1-nrow(new_rows),
                timer=timer_now,resultat="",timer_ok=timer_now+delai,texte="")
    }
    destinataire <- "Chacun"
  }else{
    new_rows <- tibble(
      user=origine,action="chat",cible=destinataire,PA=PA,
      timer=timer_now,resultat=message,timer_ok=timer_now+delai,texte="")
  }

  message_output <- paste0("Le message est envoyé. ",destinataire,
                           " le recevra dans ",round(delai/60)," minute(s)")

  list(new_rows=new_rows,message=message_output)
}
