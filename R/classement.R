#' Get Current Player Rankings
#'
#' Retrieves and summarizes the current player rankings based on 'classement'
#' (ranking) actions recorded in the game log. Excludes actions targeted at "HACK".
#'
#' @param actions A data frame of game actions. Expected to contain columns:
#'   `action` (character, e.g., "classement"),
#'   `cible` (character, player/entity name), and
#'   `resultat` (character or numeric, score to be summed).
#'
#' @returns A data frame (specifically, a tibble) with columns `cible` (player/entity
#'   name) and `resultat` (total summed score). The tibble is arranged in
#'   descending order of `resultat`.
#' @export
#'
#' @examples
#' sample_actions_ranking <- data.frame(
#'   user = c("PlayerA", "PlayerB", "PlayerA", "PlayerC", "PlayerB"),
#'   action = c("classement", "classement", "classement", "other", "classement"),
#'   cible = c("EntityX", "EntityY", "EntityX", "Any", "EntityZ"),
#'   resultat = c("10", "5", "20", "100", "15"),
#'   stringsAsFactors = FALSE
#' )
#' rankings <- bdd_classement(sample_actions_ranking)
#' print(rankings)
#'
#' sample_actions_hack <- data.frame(
#'   user = "PlayerD", action = "classement", cible = "HACK", resultat = "1000",
#'   stringsAsFactors = FALSE
#' )
#' rankings_no_hack <- bdd_classement(sample_actions_hack)
#' print(rankings_no_hack) # Should be empty or not include HACK
bdd_classement <- function(actions) {

  classement <- actions %>%
    dplyr::filter(.data$action == "classement" & .data$cible != "HACK")

  classement$resultat <- as.numeric(classement$resultat)

  classement %>%
    dplyr::mutate(resultat = as.numeric(.data$resultat)) %>%
    dplyr::group_by(.data$cible) %>%
    dplyr::summarise(resultat = sum(.data$resultat, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(.data$resultat))
}

#' Copy Player Rankings Action
#'
#' Simulates a player action to copy the current game rankings.
#' This generates a game log entry for the action itself (an "enquete" or investigation)
#' and a notification message for the character "Andromalius".
#'
#' @param actions A data frame of game actions, passed to `bdd_classement` to
#'   get the current rankings.
#' @param user_name The username (character string) of the player performing the
#'   action to copy the rankings.
#' @param PA The Action Point (numeric) cost for this action.
#' @param variation Optional. This parameter is currently not used in the function body.
#'   It is kept for potential future use or compatibility.
#' @param duree Optional. The delay in minutes (numeric) before the copied
#'   classement is considered 'received' by the player. If NA (default),
#'   a random delay between 2 and 10 minutes (inclusive) is used.
#'
#' @returns A list containing two elements:
#'   \describe{
#'     \item{new_rows}{A tibble of game log entries to be added. This includes
#'       the player's action and the notification to Andromalius.}
#'     \item{message}{A character string, a confirmation message for the player
#'       indicating the action has been initiated.}
#'   }
#' @export
#'
#' @examples
#' game_actions <- data.frame(
#'   user = "PlayerA", action = "classement", cible = "EntityX", resultat = "100",
#'   stringsAsFactors = FALSE
#' ) # Simplified for example
#' result_copy <- copie_classement(
#'   actions = game_actions,
#'   user_name = "PlayerSpy",
#'   PA = 5,
#'   duree = 3 # Specific duration of 3 minutes
#' )
#' print(result_copy$message)
#' print(result_copy$new_rows)
#'
#' result_copy_random_delay <- copie_classement(
#'   actions = game_actions,
#'   user_name = "PlayerShadow",
#'   PA = 5
#'   # duree = NA, so random delay
#' )
#' print(result_copy_random_delay$message)
#' print(result_copy_random_delay$new_rows)
copie_classement <- function(actions,user_name,PA,variation=NA,duree=NA){
  timer_now <- Sys.time()
  timer_ok <- Sys.time()+ifelse(is.na(duree),sample(c(2:10),1)*60,duree*60)

  # Accès au classement
  bdd <- bdd_classement(actions)

  bdd$phrase <- paste(bdd$cible, bdd$resultat, sep = " : ")

  indice <- paste("Classement actuel des 10 premières entités :", paste(bdd$phrase, collapse = "\n"), sep = "\n")

  message_andromalius <- "Monseigneur Andromalius, nous vous informons que votre système de classement des entités vient de subir une tentative de piratage. Malheureusement, nous n'avons pas su bloquer cette tentative, et une copie du classement a été effectuée."

  new_rows <- tibble::tibble(
    user = user_name, action = "enquete", cible = "classement", PA = PA,
    timer = timer_now, resultat = indice, timer_ok = timer_ok, texte = "") %>%
  dplyr::add_row(user = "Andromalius", action = "enquete", cible = "classement", PA = 0,
          timer = timer_now, resultat = message_andromalius, timer_ok = timer_now, texte = "")

  list(new_rows = new_rows, message = "Nous avons lancé la copie du classement. Vous devriez l'obtenir dans les 10 minutes avec vos indices.")
}

#' Modify Entity Ranking Action
#'
#' Simulates a player action to modify another entity's ranking in the game.
#' There is a 10% chance that this attempt is detected, leading to a notification
#' for "Andromalius" and failure of the modification. Otherwise, the modification
#' proceeds by significantly increasing the target's score relative to the current maximum.
#'
#' @param actions A data frame of game actions, passed to `bdd_classement` to
#'   determine current ranking scores if modification is successful.
#' @param user_name The username (character string) of the player attempting the modification.
#' @param cible The username or entity name (character string) whose ranking is targeted
#'   for modification. If `NULL`, the function will not proceed and implicitly returns `NULL`.
#' @param PA The Action Point (numeric) cost for this action.
#' @param duree Optional. This parameter is currently not used in the function body.
#'   It is kept for potential future use or compatibility.
#'
#' @returns A list containing two elements: `new_rows` (a tibble of game log entries)
#'   and `message` (an outcome message for the player). Returns `NULL` implicitly if
#'   `cible` is `NULL`, as the main logic block is skipped.
#' @export
#'
#' @examples
#' current_actions <- data.frame(
#'   user = c("PlayerA", "PlayerB"),
#'   action = c("classement", "classement"),
#'   cible = c("TargetEntity", "OtherEntity"),
#'   resultat = c("50", "200"), # TargetEntity is lower than OtherEntity (max)
#'   stringsAsFactors = FALSE
#' )
#'
#' # Simulate PlayerX modifying TargetEntity's rank
#' # (run multiple times to see different outcomes due to randomness)
#' set.seed(123) # For reproducible "failure" example if sample(1:10,1) == 1
#' result_modif_fail <- modification_classement(
#'   actions = current_actions,
#'   user_name = "PlayerX",
#'   cible = "TargetEntity",
#'   PA = 10
#' )
#' print(result_modif_fail$message)
#' print(result_modif_fail$new_rows)
#'
#' set.seed(456) # For reproducible "success" example
#' result_modif_success <- modification_classement(
#'   actions = current_actions,
#'   user_name = "PlayerY",
#'   cible = "TargetEntity",
#'   PA = 15
#' )
#' print(result_modif_success$message)
#' print(result_modif_success$new_rows)
#'
#' # Example where cible is NULL
#' result_null_cible <- modification_classement(
#'  actions = current_actions, user_name = "PlayerZ", cible = NULL, PA = 5
#' )
#' print(result_null_cible) # Should be NULL
modification_classement <- function(actions,user_name,cible,PA,duree=NA){
  timer_now <- Sys.time()

  if (!is.null(cible)) {
    if (sample(1:10, 1) == 1) {
      # Détection de l'intrusion
      message_andromalius_detected <- paste("Monseigneur Andromalius, nous vous informons que votre système de classement des entités vient de subir une tentative de piratage. Nos services ont été efficaces et ont bloqué l'intrusion. Nous savons que cette tentative provient de ", user_name, ".", sep = "")

      new_rows <- tibble::tibble(
        user = user_name, action = "modification rate", cible = cible, PA = PA,
        timer = timer_now, resultat = "", timer_ok = timer_now, texte = "") %>%
        dplyr::add_row(user = "Andromalius", action = "enquete", cible = "classement", PA = 0,
                timer = timer_now, resultat = message_andromalius_detected, timer_ok = timer_now, texte = "")

      message_output <- "Malheureusement, notre tentative d'intrusion a été détectée par les services d'Andromalius. Malheureusement, iel va savoir qu'il s'agit de vous."

    } else {
      # Modification du classement
      bdd <- bdd_classement(actions)

      max_points <- bdd %>% dplyr::summarise(max_val = max(.data$resultat, na.rm = TRUE)) %>% dplyr::pull(.data$max_val)
      # Ensure max_points is not -Inf if bdd is empty or all results are NA
      if (is.infinite(max_points)) max_points <- 0
      
      point_cible_df <- bdd[bdd$cible == cible, ]
      point_cible <- if (nrow(point_cible_df) > 0) point_cible_df$resultat else 0
      if (is.na(point_cible) || length(point_cible) == 0) point_cible <- 0

      ecart <- max_points - point_cible
      ecart <- as.character(round(ecart + stats::runif(1, 100, 500), 0))

      message_andromalius_modified <- "Monseigneur Andromalius, nous vous informons que votre système de classement des entités vient de subir une tentative de piratage. Malheureusement, nous n'avons pas su bloquer cette tentative, et le classement a été modifié."

      new_rows <- tibble::tibble(
        user = user_name, action = "classement", cible = cible, PA = PA,
        timer = timer_now, resultat = ecart, timer_ok = timer_now, texte = "") %>%
        dplyr::add_row(user = "Andromalius", action = "enquete", cible = "classement", PA = 0,
                timer = timer_now, resultat = message_andromalius_modified, timer_ok = timer_now, texte = "")

      message_output <- "Nous avons modifié le classement comme vous l'avez souhaité."
    }

    list(new_rows = new_rows, message = message_output)
  } else {
    # Implicitly return NULL if cible is NULL
    return(NULL)
  }
}
