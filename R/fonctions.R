#' Displays a modal message in the Shiny UI.
#'
#' @param title The title of the message dialog.
#' @param message The content of the message.
#'
#' @returns No return value; called for side effects (displaying a modal).
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # This function is intended to be used within a Shiny app.
#'   # Example: show_message("Welcome", "This is a message.")
#' }
show_message <- function(title, message) {
  showModal(modalDialog(
    h3(title),
    span(message),
    footer = tagList(modalButton("OK"))
  ))
}

#' Loads and processes clue information from a data frame.
#'
#' @param df A data frame containing game log data. It should include columns
#'   like 'user', 'action', 'cible', 'PA', and 'resultat'.
#'
#' @returns A data frame with formatted clue information, including title, text,
#'   cost (PA), and clue ID.
#' @export
#'
#' @examples
#' sample_data_indices <- data.frame(
#'   user = c("admin", "admin", "player1"),
#'   action = c("init", "init", "search"),
#'   cible = c("clue1", "clue2", "clue1"),
#'   PA = c(1, 2, 1),
#'   resultat = c("Title for Clue 1", "Title for Clue 2", "Found Clue 1"),
#'   texte = c("Text for Clue 1", "Text for Clue 2", NA),
#'   stringsAsFactors = FALSE
#' )
#' info_indices(sample_data_indices)
info_indices <- function(df) {
  df %>%
    dplyr::filter(user == "admin" & action == "init") %>%
    dplyr::group_by(cible, PA, resultat) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(titre = resultat, texte, variation = PA, indice = cible) %>%
    dplyr::arrange(indice, variation)
}

#' Loads and processes user credential information from a data frame.
#'
#' @param df A data frame containing game log data. It should include columns
#'   like 'user', 'action', 'cible', and 'resultat'.
#'
#' @returns A data frame with user credentials (username, password, PA).
#' @export
#'
#' @examples
#' sample_data_users <- data.frame(
#'   user = c("admin", "admin", "admin"),
#'   action = c("password", "password", "other_action"),
#'   cible = c("player1", "player2", "some_target"),
#'   resultat = c("pass1", "pass2", "res"),
#'   PA = c(10, 10, 0),
#'   stringsAsFactors = FALSE
#' )
#' info_user(sample_data_users)
info_user <- function(df) {
  df %>%
    dplyr::filter(user == "admin" & action == "password") %>%
    dplyr::group_by(cible) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(user = cible, password = resultat, PA)
}

#' Formats a time duration (in seconds) into a human-readable string (hours and minutes).
#'
#' @param diff_seconds The duration in seconds (numeric).
#'
#' @returns A string representing the duration in hours and/or minutes.
#' @export
#'
#' @examples
#' display_duration(3660) # "1 heure 1 minute"
#' display_duration(7200) # "2 heures"
#' display_duration(150)  # "2 minutes"
#' display_duration(59)   # "0 minutes"
display_duration <- function(diff_seconds) {
  diff_seconds <- as.numeric(diff_seconds)
  hours <- floor(diff_seconds / 3600)
  minutes <- floor((diff_seconds %% 3600) / 60)
  label_hours <- ifelse(hours > 1, "heures", "heure")
  label_minutes <- ifelse(minutes > 1, "minutes", "minute")

  if (hours > 0 && minutes > 0) {
    paste(hours, label_hours, minutes, label_minutes)
  } else if (hours > 0) {
    paste(hours, label_hours)
  } else {
    paste(minutes, label_minutes)
  }
}

#' Converts a POSIXct timestamp to the 'Europe/Paris' timezone.
#'
#' @param timer A POSIXct object representing a timestamp.
#'
#' @returns A POSIXct object adjusted to 'Europe/Paris' timezone.
#' @export
#'
#' @examples
#' my_time <- as.POSIXct("2023-10-27 10:00:00", tz = "UTC")
#' prepare_timer(my_time)
prepare_timer <- function(timer) {
  as.POSIXct(timer, origin = "1970-01-01", tz = "Europe/Paris")
}
