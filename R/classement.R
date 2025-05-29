#' Obtenir le classement actuel des PJ
#'
#' @param actions objet réactif contenant la base de données du serveur
#'
#' @returns data.frame avec le classement
bdd_classement  <- function(actions){

  classement <- actions %>%
    filter(action == "classement" & cible != "HACK")

  classement$resultat <- as.numeric(classement$resultat)

  classement %>%
    mutate(resultat = as.numeric(resultat)) %>%
    group_by(cible) %>%
    summarise(resultat = sum(resultat,na.rm = TRUE)) %>%
    arrange(-resultat)
}

#' Copie le classement des PJ
#'
#' @param actions objet réactif contenant la base de données du serveur
#' @param user_name PJ a l'origine de la copie
#' @param PA nombre de PA dépensé
#' @param duree (pour admin) détermine manuellement la durée de la recherche
#'
#' @returns liste avec les nouvelles lignes et le message à afficher
copie_classement <- function(actions,user_name,PA,duree=NA){
  timer_now <- Sys.time()
  timer_ok <- Sys.time()+ifelse(is.na(duree),sample(c(2:10),1)*60,duree*60)

  # Accès au classement
  bdd <- bdd_classement(actions)

  bdd$phrase <- paste(bdd$cible,bdd$resultat,sep = " : ")

  indice <- paste("Classement actuel des 10 premières entités :",paste(bdd$phrase,collapse = "\n"),sep = "\n")

  message <- "Monseigneur Andromalius, nous vous informons que votre système de classement des entités vient de subir une tentative de piratage. Malheureusement, nous n'avons pas su bloquer cette tentative, et une copie du classement a été effectuée."

  new_rows <- tibble(
    user=user_name,action="enquete",cible="classement",PA=PA,
    timer=timer_now,resultat=indice,timer_ok=timer_ok,texte="") %>%
  add_row(user="Andromalius",action="enquete",cible="classement",PA=0,
          timer=timer_now,resultat=message,timer_ok=timer_now,texte="")

  list(new_rows=new_rows,message="Nous avons lancé la copie du classement. Vous devriez l'obtenir dans les 10 minutes avec vos indices.")
}

#' Altérer le classement des entités
#'
#' @param actions objet réactif contenant la base de données du serveur
#' @param user_name PJ a l'origine de la copie
#' @param cible PJ que l'on veut placer en haut du classement
#' @param PA nombre de PA dépensé
#' @param duree (pour admin) détermine manuellement la durée de la recherche
#'
#' @returns liste avec les nouvelles lignes et le message à afficher
modification_classement <- function(actions,user_name,cible,PA,duree=NA){
  timer_now <- Sys.time()

  if (!is.null(cible)){
    if (sample(1:10,1) == 1){
      # Détection de l'intrusion
      message <- paste("Monseigneur Andromalius, nous vous informons que votre système de classement des entités vient de subir une tentative de piratage. Nos services ont été efficaces et ont bloqué l'intrusion. Nous savons que cette tentative provient de ",user_name,".",sep="")

      new_rows <- tibble(
        user=user_name,action="modification rate",cible=cible,PA=PA,
        timer=timer_now,resultat="",timer_ok=timer_now,texte="") %>%
        add_row(user="Andromalius",action="enquete",cible="classement",PA=0,
                timer=timer_now,resultat=message,timer_ok=timer_now,texte="")

      message_output <- "Malheureusement, notre tentative d'intrusion a été détectée par les services d'Andromalius. Malheureusement, iel va savoir qu'il s'agit de vous."

    }else{
      # Modification du classement
      bdd <- bdd_classement(actions)

      max_points <- bdd %>% summarise(max(resultat)) %>% pull
      point_cible <- pull(bdd[bdd$cible == cible,"resultat"])

      ecart <- max_points - point_cible
      ecart <- as.character(round(ecart + runif(1,100,500),0))

      message <- "Monseigneur Andromalius, nous vous informons que votre système de classement des entités vient de subir une tentative de piratage. Malheureusement, nous n'avons pas su bloquer cette tentative, et le classement a été modifié."

      new_rows <- tibble(
        user=user_name,action="classement",cible=cible,PA=PA,
        timer=timer_now,resultat=ecart,timer_ok=timer_now,texte="") %>%
        add_row(user="Andromalius",action="enquete",cible="classement",PA=0,
                timer=timer_now,resultat=message,timer_ok=timer_now,texte="")

      message_output <- "Nous avons modifié le classement comme vous l'avez souhaité."
    }

    list(new_rows=new_rows,message=message_output)
  }
}
