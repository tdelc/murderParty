#' Send a Message Between Players
#'
#' Sends a message from one player to one or more other players in the game.
#' Handles addressing to individuals or to 'Tout le conseil' (all players).
#'
#' @param actions A data frame of game actions. This is used to retrieve user
#'   information via `info_user(actions)` if the message is addressed to
#'   'Tout le conseil'.
#' @param origine The username (character string) of the player sending the message.
#' @param destinataire The username (character string) of the recipient player,
#'   or the special string 'Tout le conseil' to send the message to all players.
#' @param message The text content (character string) of the message.
#' @param PA The cost in Action Points (numeric) for sending the message.
#'   If sending to 'Tout le conseil' and PA > 0, special handling might occur
#'   to adjust PA (e.g. if the cost is per recipient vs flat).
#' @param delai The delay in seconds (numeric) before the message is
#'   considered received by the recipient(s).
#'
#' @returns A list containing two elements:
#'   \describe{
#'     \item{new_rows}{A tibble representing the message action(s) to be added
#'       to the game log. Each row corresponds to a message event.}
#'     \item{message_output}{A character string providing confirmation to the
#'       sender about when the message will be received.}
#'   }
#' @export
#'
#' @examples
#' sample_actions_for_chat <- data.frame(
#'   user = c("admin", "admin", "Alice", "Bob", "Charles"),
#'   action = c("password", "password", "password", "password", "password"),
#'   cible = c("Alice", "Bob", "Charles", "David", "Eve"), # For info_user
#'   resultat = c("passA", "passB", "passC", "passD", "passE"),
#'   PA = c(0,0,0,0,0),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Send a message from Alice to Bob
#' msg_to_bob <- envoi_message(
#'   actions = sample_actions_for_chat,
#'   origine = "Alice",
#'   destinataire = "Bob",
#'   message = "Hello Bob!",
#'   PA = 1,
#'   delai = 60
#' )
#' print(msg_to_bob$message_output)
#' print(msg_to_bob$new_rows)
#'
#' # Send a message from Alice to "Tout le conseil"
#' # Note: info_user will extract 'Alice', 'Bob', 'Charles', 'David', 'Eve' as users
#' # from sample_actions_for_chat based on admin password actions.
#' msg_to_all <- envoi_message(
#'   actions = sample_actions_for_chat,
#'   origine = "Alice",
#'   destinataire = "Tout le conseil",
#'   message = "Hello everyone!",
#'   PA = 0, # Example: 0 PA for broadcast
#'   delai = 120
#' )
#' print(msg_to_all$message_output)
#' print(msg_to_all$new_rows)
envoi_message <- function(actions,origine,destinataire,message,PA,delai){

  timer_now <- Sys.time()

  if (destinataire == "Tout le conseil"){
    current_info_user <- info_user(actions) # Renamed to avoid conflict

    new_rows <- tibble::tibble(
      user=origine,action="chat",cible=current_info_user$user[1],PA=PA, # Use current_info_user
      timer=timer_now,resultat=message,timer_ok=timer_now+delai,texte="")

    if (length(current_info_user$user) > 1) { # Check if there are more users
      for (pj in current_info_user$user[-1]){ # Use current_info_user
        new_rows <- new_rows %>%
          dplyr::add_row(user=origine,action="chat",cible=pj,PA=PA,
                  timer=timer_now,resultat=message,timer_ok=timer_now+delai,texte="")
      }
    }

    if (PA > 0){
      # Adjust PA: if PA cost is per message, this logic might need review.
      # Current logic seems to try to refund PA if too many messages generated?
      # This line effectively sets PA for the 'admin_PA' action to 1 - number of recipients.
      # If PA is a cost per message, it should likely be positive here.
      # For simplicity in example, assuming PA cost is flat or handled by this adjustment.
      new_rows <- new_rows %>%
        dplyr::add_row(user=origine,action="admin_PA",cible="",PA=1-nrow(new_rows), # Or simply -PA if it's a cost
                timer=timer_now,resultat="",timer_ok=timer_now+delai,texte="")
    }
    destinataire_output <- "Chacun" # Renamed to avoid modifying parameter
  }else{
    new_rows <- tibble::tibble(
      user=origine,action="chat",cible=destinataire,PA=PA,
      timer=timer_now,resultat=message,timer_ok=timer_now+delai,texte="")
    destinataire_output <- destinataire # Use original for single recipient
  }

  message_output <- paste0("Le message est envoyé. ", destinataire_output, # Use destinataire_output
                           " le recevra dans ", round(delai / 60), " minute(s)")

  list(new_rows = new_rows, message = message_output)
}
