#' Rechercher un indice
#'
#' @description
#' `recherche_indice()` permet de faire une recherche d'indices parmi les indices dispo.
#'
#' @param actions objet réactif contenant la base de données du serveur
#' @param user_name PJ a l'origine de la recherche
#' @param enquete nom de l'enquête
#' @param PA nombre de PA dépensé
#' @param resultat (pour admin) détermine manuellement le résultat
#' @param duree (pour admin) détermine manuellement la durée de la recherche
#' @param id_chatgpt (facultatif) credential de chatGPT
#'
#' @returns liste avec les nouvelles lignes et le message à afficher
recherche_indice <- function(actions,user_name,
                             enquete,PA,resultat=NA,duree=NA,
                             id_chatgpt=NULL){
  n_new <- 0
  print(actions)
  print(user_name)
  print(enquete)
  print(PA)

  if (is.na(duree)){
    # Durée aléatoire de recherche
    alea_duree <- sample(c(2:10),1)
    if (nchar(enquete) == 1){
      # Enquête normale
      message_output <- paste("L'équipe de recherche est partie se renseigner. Vous devriez avoir une réponse dans ",alea_duree," minutes environ",sep="")
    }else{
      # Enquête spéciale
      message_output <- paste("L'équipe de recherche est partie se renseigner. Cette enquête sera particulièrement longue. Nous espérons avoir des réponses 1 heure avant la fin de votre soirée.",sep="")
    }
  }else{
    alea_duree <- duree
    # Enquête admin
    message_output <- paste("Enquête lancée",sep="")
  }
  timer_ok <- Sys.time()+alea_duree*60

  if (is.na(resultat)){
    # Création de la BDD dés
    valeur_alea <- creation_bdd_des(actions)

    # Lancement du dé
    liste_resultats <- rep(valeur_alea$resultat,valeur_alea$valeur)
    id_resultat <- sample(1:length(liste_resultats),1)
    resultat <- liste_resultats[id_resultat]
    # Echec critique
    if (resultat != 1){
      id_variation <- id_resultat + PA - 1
      resultat <- liste_resultats[min(id_resultat,length(liste_resultats))]
    }
  }

  # Savoir s'il y a une interception
  recup_indice <- actions %>%
    filter(cible == user_name, action %in% c("interception","interception ok"),
           timer < Sys.time())

  if (nrow(recup_indice) > 0){
    if (pull(recup_indice[nrow(recup_indice),"action"]) != "interception ok"){

      recup_indice <- recup_indice[nrow(recup_indice),]

      recup_indice$timer <- prepare_timer(recup_indice$timer)
      recup_indice$timer_ok <- prepare_timer(recup_indice$timer)

      new_rows <- tibble(
        user=user_name,action="Enquete interceptee",cible=enquete,PA=PA,
        timer=timer_now,resultat=as.character(resultat),timer_ok=timer_now,texte="") %>%
        add_row(user=recup_indice$user,action="interception ok",
                cible=user_name,PA=0,timer=recup_indice$timer,
                resultat=as.character(resultat),timer_ok=recup_indice$timer_ok,texte="")

      user_effectif <- pull(recup_indice[,'user'])
      PA_effectif <- 0
    }else{
      # Normal
      user_effectif <- user_name
      PA_effectif <- PA
    }
  }else{
    # Normal
    user_effectif <- user_name
    PA_effectif <- PA
  }

  texte_indice <- info_indices(actions) %>%
    filter(indice == enquete & variation == resultat) %>%
    pull(texte)

  # Chat GPT
  fl_chat_gpt <- actions[actions$action == "chat_gpt",]$resultat

  if (fl_chat_gpt[length(fl_chat_gpt)] == "Oui"){
    generic <- "Tu es une aide de jeu pour une soirée enquête. Il s'agit d'un scénario dans l'univers de INS/MV. Le joueur vient de demander une enquête à son équipe d'ange ou de démons, je vais te donner l'indice qu'il récupère, tu dois reformuler un peu le message, tout en gardant exactement les mêmes informations de scénario. S'il y a une prophétie dans l'indice, copie là exactement. Voici l'indice à reformuler : "

    try({
      #https://platform.openai.com/settings/proj_0cGdt4GJlVJwbTdYKtjE1WVW/limits
      TheOpenAIR::openai_api_key(id_chatgpt)
      TheOpenAIR::set_chatlog(chatlog_id = ".__CURRENTCHAT__",initial_content = generic)
      answer <- TheOpenAIR::chat(texte_indice,model = "gpt-4o-mini",output = "response_object")
      texte_indice <- answer$choices$message$content
    },silent = TRUE)
  }

  # Enregistrement du résultat
  new_rows <- tibble(
    user=user_effectif,action="enquete",cible=enquete,PA=PA_effectif,
    timer=Sys.time(),resultat=as.character(resultat),timer_ok=timer_ok,texte=texte_indice)

  list(new_rows=new_rows,message=message_output)
}

#' Copier l'indice d'un autre PJ
#'
#' @description
#' `copie_indice()` permet de faire une copie d'une enquête réalisée par un autre PJ.
#'
#' @param actions objet réactif contenant la base de données du serveur
#' @param user_name PJ a l'origine de la copie
#' @param cible_name PJ ciblé par la copie
#' @param PA nombre de PA dépensé
#'
#' @returns liste avec les nouvelles lignes et le message à afficher
copie_indice <- function(actions,user_name,cible_name,PA){

  timer_now <- Sys.time()
  new_rows <- tibble(
    user=user_name,action="copie",cible=cible_name,PA=PA,
    timer=timer_now,resultat="",timer_ok=timer_now,texte="")

  recup_indice <- actions %>%
    filter(user == cible_name, action == "enquete",timer_now <= timer_ok+10*60)

  if (nrow(recup_indice) > 0){
    recup_indice <- recup_indice[nrow(recup_indice),]
    new_rows <- new_rows %>% add_row(
      user=user_name,action="enquete",cible=recup_indice$cible,PA=0,
      timer=timer_now,resultat=recup_indice$resultat,timer_ok=timer_now,
      texte=recup_indice$texte
    )

    titre_indice <- info_indices(actions) %>%
      filter(indice == recup_indice$cible & variation == 3) %>%
      pull(titre)

    message_output <- paste("Nous avons obtenu une copie de l'enquête suivante : ",titre_indice,sep='')

  }else{
    message_output <- "Nous n'avons rien pu copier comme enquête"
  }

  list(new_rows = new_rows,message=message_output)
}

#' Intercepter l'enquête future d'un autre PJ
#'
#' @description
#' `interception_indice()` permet de préparer une interception d'une prochaine recherche d'indice d'un autre PJ.
#'
#' @param actions objet réactif contenant la base de données du serveur
#' @param user_name PJ a l'origine de l'interception
#' @param cible_name PJ ciblé par l'interception
#' @param PA nombre de PA dépensé
#'
#' @returns liste avec les nouvelles lignes et le message à afficher
interception_indice <- function(actions,user_name,cible_name,PA){
  timer_now <- Sys.time()

  if (sample(1:10,1) == 1){

    new_rows <- tibble(
      user=user_name,action="interception ratee",cible=cible_name,PA=PA,
      timer=timer_now,resultat="",timer_ok=timer_now,texte="")

    message_output <- "Malheureusement, nos équipes se sont faites directement captées par l'équipe concurrente. L'interception est un échec. Heureusement, votre identité est restée dissimulée."
  }else{

    new_rows <- tibble(
      user=user_name,action="interception",cible=cible_name,PA=PA,
      timer=timer_now,resultat="",timer_ok=timer_now+20*60,texte="")

    message_output <- "Nous avons lancé l'interception du message, si votre cible enquête d'ici 15 minutes, vous recevrez directement le rapport d'enquête à sa place."
  }

  list(new_rows = new_rows,message=message_output)
}


#' Lister les indices actuels d'un PJ
#'
#' @description
#' `liste_indices()` liste les indices actuels d'un PJ
#'
#' @param actions objet réactif contenant la base de données du serveur
#' @param user_name PJ dont on veut la liste des indices actuels
#'
#' @returns liste avec les indices
liste_indices <- function(actions,user_name){

  # Liste des indices
  info_indices <- info_indices(actions) %>%
    select(titre,texte,variation,indice)

  # Indice du User
  temp_indices <- actions %>%
    filter(action %in% c("init","bonus") & user == user_name) %>%
    pull(cible) %>% unique()

  liste_indices <- temp_indices

  option_indices <- list()
  for (i in liste_indices){
    titre <- info_indices %>%
      filter(indice==i & variation == 3) %>%
      pull(titre)
    option_indices[[titre]] <- i
  }

  return(option_indices)
}
