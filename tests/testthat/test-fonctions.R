# Tests for functions in R/fonctions.R
library(testthat)
library(dplyr) # For sample data creation and manipulation

# Since murderParty is not a fully installed package in this environment,
# we might need to source the functions directly if library(murderParty) fails.
# For now, assume functions are available if tests are run in an environment
# where murderParty has been loaded (e.g. via devtools::load_all())

test_that("show_message runs without error", {
  # Mock shiny functions to avoid errors in non-Shiny context
  mock_showModal <- function(ui) { TRUE }
  mock_modalDialog <- function(...) { TRUE }
  mock_tagList <- function(...) { TRUE }
  mock_h3 <- function(...) { TRUE }
  mock_span <- function(...) { TRUE }
  mock_modalButton <- function(...) { TRUE }

  # Temporarily replace shiny functions with mocks
  original_showModal <- if (exists("showModal", where = "package:shiny", inherits = FALSE)) get("showModal", pos = "package:shiny") else NULL
  original_modalDialog <- if (exists("modalDialog", where = "package:shiny", inherits = FALSE)) get("modalDialog", pos = "package:shiny") else NULL
  original_tagList <- if (exists("tagList", where = "package:shiny", inherits = FALSE)) get("tagList", pos = "package:shiny") else NULL
  original_h3 <- if (exists("h3", where = "package:shiny", inherits = FALSE)) get("h3", pos = "package:shiny") else NULL
  original_span <- if (exists("span", where = "package:shiny", inherits = FALSE)) get("span", pos = "package:shiny") else NULL
  original_modalButton <- if (exists("modalButton", where = "package:shiny", inherits = FALSE)) get("modalButton", pos = "package:shiny") else NULL

  if (!is.null(original_showModal)) assign("showModal", mock_showModal, envir = as.environment("package:shiny"))
  if (!is.null(original_modalDialog)) assign("modalDialog", mock_modalDialog, envir = as.environment("package:shiny"))
  if (!is.null(original_tagList)) assign("tagList", mock_tagList, envir = as.environment("package:shiny"))
  if (!is.null(original_h3)) assign("h3", mock_h3, envir = as.environment("package:shiny"))
  if (!is.null(original_span)) assign("span", mock_span, envir = as.environment("package:shiny"))
  if (!is.null(original_modalButton)) assign("modalButton", mock_modalButton, envir = as.environment("package:shiny"))
  
  on.exit({
    # Restore original shiny functions
    if (!is.null(original_showModal)) assign("showModal", original_showModal, envir = as.environment("package:shiny"))
    if (!is.null(original_modalDialog)) assign("modalDialog", original_modalDialog, envir = as.environment("package:shiny"))
    if (!is.null(original_tagList)) assign("tagList", original_tagList, envir = as.environment("package:shiny"))
    if (!is.null(original_h3)) assign("h3", original_h3, envir = as.environment("package:shiny"))
    if (!is.null(original_span)) assign("span", original_span, envir = as.environment("package:shiny"))
    if (!is.null(original_modalButton)) assign("modalButton", original_modalButton, envir = as.environment("package:shiny"))
  }, add = TRUE)

  # If shiny is not installed/loaded, skip this test gracefully or test parts that don't need shiny
  # For now, we assume direct sourcing or loading via devtools, so show_message should exist.
  # The test is that it *can be called*. The mocks ensure it doesn't try to render UI.
  expect_true(show_message("Test Title", "Test message content."))
})

test_that("info_indices processes data correctly", {
  sample_data_1 <- tibble::tribble(
    ~user,   ~action, ~cible, ~PA, ~resultat,            ~texte,
    "admin", "init",  "idx1", 1,   "Titre Idx 1 Var 1", "Texte Idx 1 Var 1",
    "admin", "init",  "idx1", 2,   "Titre Idx 1 Var 2", "Texte Idx 1 Var 2", # last for idx1
    "player1","search","idx1", 1,   "Joueur trouve idx1",NA,
    "admin", "init",  "idx2", 1,   "Titre Idx 2 Var 1", "Texte Idx 2 Var 1", # last for idx2
    "admin", "other", "idx3", 1,   "Autre action admin", "Autre texte"
  )

  expected_output_1 <- tibble::tribble(
    ~titre,              ~texte,              ~variation, ~indice,
    "Titre Idx 1 Var 2", "Texte Idx 1 Var 2", 2,          "idx1",
    "Titre Idx 2 Var 1", "Texte Idx 2 Var 1", 1,          "idx2"
  ) %>% dplyr::arrange(indice, variation) # ensure same order as function output

  result_1 <- info_indices(sample_data_1)
  expect_equal(colnames(result_1), c("titre", "texte", "variation", "indice"))
  expect_equal(nrow(result_1), 2)
  expect_equal(result_1, expected_output_1)

  # Test with specific selection of last entry per group
  sample_data_2 <- tibble::tribble(
    ~user,   ~action, ~cible, ~PA, ~resultat,       ~texte,
    "admin", "init",  "idxA", 1, "Old Title A V1", "Old Text A V1",
    "admin", "init",  "idxA", 1, "New Title A V1", "New Text A V1"  # This one should be picked
  )
  expected_output_2 <- tibble::tribble(
    ~titre,           ~texte,           ~variation, ~indice,
    "New Title A V1", "New Text A V1",  1,          "idxA"
  )
  expect_equal(info_indices(sample_data_2), expected_output_2)
})

test_that("info_indices handles empty or non-matching data", {
  empty_df <- tibble::tibble(user=character(), action=character(), cible=character(), PA=numeric(), resultat=character(), texte=character())
  expect_equal(nrow(info_indices(empty_df)), 0)
  expect_equal(colnames(info_indices(empty_df)), c("titre", "texte", "variation", "indice"))

  non_matching_df <- tibble::tribble(
    ~user,   ~action, ~cible, ~PA, ~resultat, ~texte,
    "player1","search","idx1", 1,   "Found",   NA,
    "admin", "delete","idx2", 0,   "Deleted", NA
  )
  expect_equal(nrow(info_indices(non_matching_df)), 0)
})

test_that("info_user processes data correctly", {
  sample_data_users <- tibble::tribble(
    ~user,    ~action,   ~cible,    ~resultat, ~PA,
    "admin",  "password","playerA", "pass123", 10,
    "admin",  "password","playerB", "pass456", 10, # last for playerB
    "playerA","login",   "playerA", "success", 0,
    "admin",  "password","playerB", "newpass", 10, # this one should be picked for PlayerB
    "admin",  "other",   "playerC", "other",   0
  )

  expected_output_users <- tibble::tribble(
    ~user,     ~password, ~PA,
    "playerA", "pass123", 10,
    "playerB", "newpass", 10
  ) %>% dplyr::arrange(user)

  result_users <- info_user(sample_data_users) %>% dplyr::arrange(user)
  expect_equal(colnames(result_users), c("user", "password", "PA"))
  expect_equal(nrow(result_users), 2)
  expect_equal(result_users, expected_output_users)
})

test_that("info_user handles empty or non-matching data", {
  empty_df_users <- tibble::tibble(user=character(), action=character(), cible=character(), resultat=character(), PA=numeric())
  expect_equal(nrow(info_user(empty_df_users)), 0)
  expect_equal(colnames(info_user(empty_df_users)), c("user", "password", "PA"))

  non_matching_df_users <- tibble::tribble(
    ~user,   ~action, ~cible,    ~resultat, ~PA,
    "player1","login", "player1", "success", 0,
    "admin",  "init",  "game",    "done",    0
  )
  expect_equal(nrow(info_user(non_matching_df_users)), 0)
})

test_that("display_duration formats time correctly", {
  expect_equal(display_duration(0), "0 minutes")
  expect_equal(display_duration(59), "0 minutes") # Behavior for < 1 min
  expect_equal(display_duration(60), "1 minute")
  expect_equal(display_duration(61), "1 minute") # Behavior for > 1 min but < 2 min
  expect_equal(display_duration(119), "1 minute")
  expect_equal(display_duration(120), "2 minutes")
  expect_equal(display_duration(3599), "59 minutes")
  expect_equal(display_duration(3600), "1 heure")
  expect_equal(display_duration(3601), "1 heure 0 minutes") # Behavior for > 1 hour
  expect_equal(display_duration(3660), "1 heure 1 minute")
  expect_equal(display_duration(3720), "1 heure 2 minutes")
  expect_equal(display_duration(7200), "2 heures")
  expect_equal(display_duration(7260), "2 heures 1 minute")
  expect_equal(display_duration(7320), "2 heures 2 minutes")
})

test_that("prepare_timer converts timestamp correctly", {
  # Test with a numeric input (seconds since epoch)
  numeric_ts <- 1678886400 # Represents a specific date
  posix_ct_ts <- as.POSIXct(numeric_ts, origin = "1970-01-01", tz = "UTC")

  # Expected result should be in "Europe/Paris"
  # Note: The exact representation of the expected time in "Europe/Paris"
  # depends on DST rules for that specific date.
  # For this test, we primarily check the class, and that it's not identical to UTC if conversion happened.
  # A more robust test would involve specific dates and known DST behavior or using a library for timezone math.

  result_timer <- prepare_timer(posix_ct_ts) # function expects POSIXct
  expect_s3_class(result_timer, "POSIXct")
  expect_equal(attr(result_timer, "tzone"), "Europe/Paris")

  # If input was UTC and output is Europe/Paris, they shouldn't be identical bit-wise
  # unless the specific timestamp has no offset between UTC and Europe/Paris (unlikely for most times)
  # This is a basic check; true timezone testing is complex.
  if (format(posix_ct_ts, "%Z") != format(result_timer, "%Z")) {
    expect_false(identical(as.numeric(posix_ct_ts), as.numeric(result_timer)))
  } else {
    # If timezones happen to be the same (e.g. during winter for some definitions),
    # then numeric values would be same. This is a less common case.
    expect_true(identical(as.numeric(posix_ct_ts), as.numeric(result_timer)))
  }
  
  # Test with another POSIXct object already in a different timezone
  posix_ct_ny <- as.POSIXct("2023-03-15 10:00:00", tz = "America/New_York")
  result_timer_ny <- prepare_timer(posix_ct_ny)
  expect_s3_class(result_timer_ny, "POSIXct")
  expect_equal(attr(result_timer_ny, "tzone"), "Europe/Paris")
  
  # Numeric representation should differ if timezones are effectively different
  expect_false(identical(as.numeric(posix_ct_ny), as.numeric(result_timer_ny)))
})
