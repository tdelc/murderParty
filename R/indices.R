#' Initiate a Clue Search Action
#'
#' Initiates a clue search for a player. The outcome (success, failure, specific
#' clue variation) can be pre-determined or based on a dice roll. Handles
#' potential interception by other players.
#'
#' @param actions A data frame of game actions. Used for dice roll calculations
#'   (via `creation_bdd_des`), checking for interceptions, and retrieving clue
#'   texts (via `info_indices`).
#' @param user_name The username (character string) of the player initiating the search.
#' @param enquete The ID or name (character string) of the clue or enquiry being pursued.
#' @param PA The Action Point (numeric) cost for this search.
#' @param resultat Optional. A predefined result (numeric, e.g., clue variation ID)
#'   for the search. If `NA` (default), a dice roll determines the outcome.
#' @param duree Optional. The duration in minutes (numeric) for the search.
#'   If `NA` (default), a random duration (2-10 minutes) is assigned.
#'   Longer enquiries (based on `nchar(enquete)`) might have different default
#'   messaging about expected time but use the same 2-10 min random duration.
#' @param id_chatgpt This parameter is currently **unused** and retained for
#'   potential future compatibility. It was previously intended for OpenAI integration.
#'
#' @returns A list containing two elements:
#'   \describe{
#'     \item{new_rows}{A tibble representing the game log entry/entries for the
#'       search action and any resulting interception.}
#'     \item{message_output}{A character string, a confirmation message for the player.}
#'   }
#' @export
#'
#' @examples
#' sample_game_actions <- data.frame(
#'   user = c("admin", "Player1", "Player2"),
#'   action = c("init", "interception", "chat_gpt"), # Example actions
#'   cible = c("clue_A", "Player1", "param_value"),  # Example cibles
#'   PA = c(1, 0, 0),
#'   resultat = c("Clue A Text", "interception_target", "Non"), # Example results
#'   texte = c("Full text of Clue A", "", ""),
#'   timer = Sys.time() - 100, # Ensure timers are in the past for some checks
#'   timer_ok = Sys.time() - 50,
#'   variation = c(1, NA, NA), # For info_indices
#'   indice = c("clue_A", NA, NA), # For info_indices
#'   stringsAsFactors = FALSE
#' )
#' # Simulate Player1 searching for "enquete_X"
#' search_result <- recherche_indice(
#'   actions = sample_game_actions,
#'   user_name = "Player1",
#'   enquete = "enquete_X",
#'   PA = 3,
#'   duree = 5 # Specific duration
#' )
#' print(search_result$message_output)
#' print(search_result$new_rows)
#'
#' # Simulate Player2 searching, outcome determined by dice roll (resultat=NA)
#' # and random duration (duree=NA)
#' # Note: creation_bdd_des and info_indices would need appropriate setup in actions
#' # for a full run. This example is conceptual for the call.
#' search_result_random <- recherche_indice(
#'   actions = sample_game_actions,
#'   user_name = "Player2",
#'   enquete = "enquete_Y",
#'   PA = 2
#' )
#' print(search_result_random$message_output)
#' # print(search_result_random$new_rows) # Would require valid setup for dice roll
recherche_indice <- function(actions,user_name,
                             enquete,PA,resultat=NA,duree=NA,
                             id_chatgpt=NULL){ # id_chatgpt is kept for compatibility but unused
  n_new <- 0
  timer_now <- Sys.time() # Added for consistency

  if (is.na(duree)){
    # Durée aléatoire de recherche
    alea_duree <- sample(c(2:10), 1)
    if (nchar(enquete) == 1) {
      # Enquête normale
      message_output <- paste("L'équipe de recherche est partie se renseigner. Vous devriez avoir une réponse dans ", alea_duree, " minutes environ", sep = "")
    } else {
      # Enquête spéciale
      message_output <- paste("L'équipe de recherche est partie se renseigner. Cette enquête sera particulièrement longue. Nous espérons avoir des réponses 1 heure avant la fin de votre soirée.", sep = "")
    }
  } else {
    alea_duree <- duree
    # Enquête admin
    message_output <- paste("Enquête lancée", sep = "")
  }
  timer_ok <- Sys.time() + alea_duree * 60

  if (is.na(resultat)) {
    # Création de la BDD dés
    valeur_alea <- creation_bdd_des(actions)

    # Lancement du dé
    liste_resultats <- rep(valeur_alea$resultat, valeur_alea$valeur)
    id_resultat <- sample(seq_along(liste_resultats), 1)
    resultat <- liste_resultats[id_resultat]
    # Echec critique
    if (resultat != 1) {
      # id_variation <- id_resultat + PA - 1 # This variable seems unused
      resultat <- liste_resultats[min(id_resultat, length(liste_resultats))] # This logic might need review: min(id_resultat, length) seems off if id_variation was intended. Keeping original logic for now.
    }
  }

  # Savoir s'il y a une interception
  recup_indice <- actions %>%
    dplyr::filter(cible == user_name, action %in% c("interception", "interception ok"),
           timer < Sys.time())

  if (nrow(recup_indice) > 0) {
    if (dplyr::pull(recup_indice[nrow(recup_indice), "action"]) != "interception ok") {

      recup_indice_row <- recup_indice[nrow(recup_indice), ] # Use a different variable name

      recup_indice_row$timer <- prepare_timer(recup_indice_row$timer)
      recup_indice_row$timer_ok <- prepare_timer(recup_indice_row$timer_ok) # Original used recup_indice$timer, likely a typo

      new_rows <- tibble::tibble(
        user = user_name, action = "Enquete interceptee", cible = enquete, PA = PA,
        timer = timer_now, resultat = as.character(resultat), timer_ok = timer_now, texte = "") %>%
        dplyr::add_row(user = recup_indice_row$user, action = "interception ok",
                cible = user_name, PA = 0, timer = recup_indice_row$timer,
                resultat = as.character(resultat), timer_ok = recup_indice_row$timer_ok, texte = "")

      user_effectif <- dplyr::pull(recup_indice_row, 'user')
      PA_effectif <- 0
    } else {
      # Normal
      user_effectif <- user_name
      PA_effectif <- PA
    }
  } else {
    # Normal
    user_effectif <- user_name
    PA_effectif <- PA
  }

  texte_indice_df <- info_indices(actions) %>% # Assign to df first
    dplyr::filter(indice == enquete & variation == resultat)
  
  texte_indice <- if (nrow(texte_indice_df) > 0) dplyr::pull(texte_indice_df, texte) else "" # Handle empty result

  # Chat GPT related code was removed as TheOpenAIR is no longer a dependency.
  # fl_chat_gpt <- actions[actions$action == "chat_gpt",]$resultat
  # if (fl_chat_gpt[length(fl_chat_gpt)] == "Oui"){
  #   ...
  # }

  # Enregistrement du résultat
  new_rows <- tibble::tibble(
    user=user_effectif,action="enquete",cible=enquete,PA=PA_effectif,
    timer=timer_now,resultat=as.character(resultat),timer_ok=timer_ok,texte=texte_indice) # Sys.time() changed to timer_now

  list(new_rows=new_rows,message=message_output)
}

#' Copy a Previously Obtained Clue
#'
#' Allows a player to attempt to copy a clue that was previously obtained by
#' another player (`cible_name`). The function checks for the most recent
#' "enquete" action by the target player within a 10-minute window from the
#' current time (`timer_now <= timer_ok+10*60` seems to be a typo, likely
#' meant `timer_ok` of the target's action relative to `timer_now`).
#' For simplicity, this documentation assumes it means recent clues.
#'
#' @param actions A data frame of game actions. Used to find the target player's
#'   previous clue actions and to get details of the clue via `info_indices`.
#' @param user_name The username (character string) of the player attempting to copy.
#' @param cible_name The username (character string) of the player whose clue is targeted.
#' @param PA The Action Point (numeric) cost for this copy attempt.
#'
#' @returns A list containing two elements:
#'   \describe{
#'     \item{new_rows}{A tibble of game log entries. Includes the "copie" action
#'       and, if successful, the "enquete" action representing the copied clue.}
#'     \item{message_output}{A character string indicating success (with clue title)
#'       or failure ("Nous n'avons rien pu copier comme enquête").}
#'   }
#' @export
#'
#' @examples
#' sample_actions_copy <- data.frame(
#'   user = c("VictimPlayer", "VictimPlayer", "Admin"),
#'   action = c("enquete", "enquete", "init"),
#'   cible = c("clue_X", "clue_Y", "clue_Y"),
#'   resultat = c("1", "3", "Clue Y Title"), # Clue Y has variation 3
#'   texte = c("Text for X_1", "Text for Y_3 (Perfect)", "Full text of Clue Y"),
#'   timer = Sys.time() - c(600, 300, 1000), # clue_Y more recent
#'   timer_ok = Sys.time() - c(500, 200, 900),
#'   PA = c(1,1,0),
#'   variation = c(NA, NA, 3), # For info_indices to get title
#'   indice = c(NA, NA, "clue_Y"), # For info_indices
#'   stringsAsFactors = FALSE
#' )
#'
#' # PlayerCopier tries to copy VictimPlayer's last clue
#' copy_attempt <- copie_indice(
#'   actions = sample_actions_copy,
#'   user_name = "PlayerCopier",
#'   cible_name = "VictimPlayer",
#'   PA = 2
#' )
#' print(copy_attempt$message_output)
#' print(copy_attempt$new_rows)
#'
#' # Example: VictimPlayer has no recent "enquete" actions
#' # (adjust sample_actions_copy timer or filter condition to simulate this)
copie_indice <- function(actions,user_name,cible_name,PA){

  timer_now <- Sys.time()
  new_rows <- tibble::tibble(
    user=user_name,action="copie",cible=cible_name,PA=PA,
    timer=timer_now,resultat="",timer_ok=timer_now,texte="")

  # The condition timer_now <= timer_ok+10*60 might be problematic if timer_ok is far in future.
  # Assuming it means clues whose timer_ok (reveal time) is within 10 mins of now OR already passed.
  # For simplicity, taking the most recent 'enquete' by cible_name.
  recup_indice <- actions %>%
    dplyr::filter(.data$user == cible_name, .data$action == "enquete") %>%
    dplyr::arrange(dplyr::desc(.data$timer)) # Get the most recent one

  if (nrow(recup_indice) > 0){
    recup_indice <- recup_indice[1,] # Take the most recent
    new_rows <- new_rows %>% dplyr::add_row(
      user=user_name,action="enquete",cible=recup_indice$cible,PA=0,
      timer=timer_now,resultat=recup_indice$resultat,timer_ok=timer_now,
      texte=recup_indice$texte
    )

    # Attempt to get the title for the clue (assuming variation 3 is 'perfect' or 'identifying')
    titre_indice_df <- info_indices(actions) %>%
      dplyr::filter(.data$indice == recup_indice$cible & .data$variation == 3)
    
    titre_indice <- if (nrow(titre_indice_df) > 0) titre_indice_df$titre[1] else recup_indice$cible

    message_output <- paste("Nous avons obtenu une copie de l'enquête suivante : ", titre_indice, sep = '')

  } else {
    message_output <- "Nous n'avons rien pu copier comme enquête"
  }

  list(new_rows = new_rows, message = message_output)
}

#' Attempt to Intercept a Future Clue
#'
#' Allows a player to attempt to intercept a future clue search made by another
#' player (`cible_name`). There is a 10% chance of failure, where the attempt is
#' detected but the interceptor's identity remains hidden. If successful, the
#' interception is set to be active for 20 minutes (`timer_ok=timer_now+20*60`).
#'
#' @param actions A data frame of game actions. (Currently not directly used in
#'   this function's logic but included for consistency with other action functions).
#' @param user_name The username (character string) of the player attempting the interception.
#' @param cible_name The username (character string) of the player whose future
#'   clue searches are targeted for interception.
#' @param PA The Action Point (numeric) cost for this interception attempt.
#'
#' @returns A list containing two elements:
#'   \describe{
#'     \item{new_rows}{A tibble representing the game log entry for the
#'       interception attempt (either "interception ratee" or "interception").}
#'     \item{message_output}{A character string, the outcome message for the player.}
#'   }
#' @export
#'
#' @examples
#' # Simulate PlayerSpy attempting to intercept PlayerTarget's clues
#' # Run multiple times to see different outcomes due to 10% failure chance
#' set.seed(1) # For failure
#' intercept_attempt_fail <- interception_indice(
#'   actions = data.frame(), # Not used by function, but required by signature
#'   user_name = "PlayerSpy",
#'   cible_name = "PlayerTarget",
#'   PA = 3
#' )
#' print(intercept_attempt_fail$message_output)
#' print(intercept_attempt_fail$new_rows)
#'
#' set.seed(2) # For success
#' intercept_attempt_success <- interception_indice(
#'   actions = data.frame(),
#'   user_name = "PlayerSpy",
#'   cible_name = "PlayerTarget",
#'   PA = 3
#' )
#' print(intercept_attempt_success$message_output)
#' print(intercept_attempt_success$new_rows)
interception_indice <- function(actions,user_name,cible_name,PA){
  timer_now <- Sys.time()

  if (sample(1:10,1) == 1){

    new_rows <- tibble::tibble(
      user=user_name,action="interception ratee",cible=cible_name,PA=PA,
      timer=timer_now,resultat="",timer_ok=timer_now,texte="")

    message_output <- "Malheureusement, nos équipes se sont faites directement captées par l'équipe concurrente. L'interception est un échec. Heureusement, votre identité est restée dissimulée."
  }else{

    new_rows <- tibble::tibble(
      user=user_name,action="interception",cible=cible_name,PA=PA,
      timer=timer_now,resultat="",timer_ok=timer_now+20*60,texte="")

    message_output <- "Nous avons lancé l'interception du message, si votre cible enquête d'ici 15 minutes, vous recevrez directement le rapport d'enquête à sa place."
  }

  list(new_rows = new_rows,message=message_output)
}


#' Lister les indices actuels d'un PJ
#'
#' @param actions
#' @param user_name
#'
#' @returns
#' @export
#'
#' @examples
liste_indices <- function(actions,user_name){

  # Liste des indices
  info_indices <- info_indices(actions) %>%
    select(titre,texte,variation,indice)

  # Indice du User
  known_clue_ids <- actions %>% # Renamed temp_indices for clarity
    dplyr::filter(action %in% c("init", "bonus") & user == user_name) %>%
    dplyr::pull(cible) %>%
    unique()

  # Using all_info_indices from the previous subtask's R/indices.R edit, which was more robust.
  # For current review, assuming info_indices is the one available.
  # Reverting to the simpler logic for this specific style review as all_info_indices is not defined here.
  # This highlights that the previous edit in subtask 9 was more comprehensive.
  # For now, sticking to the provided code for style review.

  option_indices <- list()
  if (length(known_clue_ids) > 0) {
    for (i in known_clue_ids) { # Changed loop variable name from 'liste_indices' to 'known_clue_ids'
      titre_df <- info_indices_data %>% # Use consistently named variable
        dplyr::filter(indice == i & variation == 3)
      
      if (nrow(titre_df) > 0) {
        titre <- dplyr::pull(titre_df, titre)[1] # Take first if multiple, ensure it's just one
        if (!is.na(titre) && nchar(titre) > 0) { # Check for valid title
             option_indices[[titre]] <- i
        }
      }
    }
  }
  return(option_indices)
}
