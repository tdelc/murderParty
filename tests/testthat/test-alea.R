# Tests for functions in R/alea.R
library(testthat)
library(dplyr) # For sample data creation
library(tibble) # For tribble

# Assume functions are available (sourced or loaded via devtools::load_all())

test_that("creation_bdd_des processes data correctly and returns expected structure", {
  sample_actions_alea <- tibble::tribble(
    ~user,   ~action, ~cible, ~PA, ~resultat, ~texte, # cible is outcome (1,2,3), PA is weight
    "admin", "alea",  1,      10,  NA,        NA,     # 1 = Ratée
    "admin", "alea",  2,      5,   NA,        NA,     # 2 = Réussie
    "admin", "alea",  3,      2,   NA,        NA,     # 3 = Parfaite
    "player1","other", 1,      1,   NA,        NA
  )

  # Test with no bonus
  result_no_bonus <- creation_bdd_des(sample_actions_alea, bonus_recherche = 0)
  expect_s3_class(result_no_bonus, "tbl_df")
  expect_equal(colnames(result_no_bonus), c("resultat", "valeur", "pc", "label"))
  expect_equal(nrow(result_no_bonus), 3)
  expect_true(all(result_no_bonus$resultat %in% c(1, 2, 3)))
  expect_true(all(is.numeric(result_no_bonus$valeur)))
  expect_true(all(is.numeric(result_no_bonus$pc)))
  expect_true(all(is.character(result_no_bonus$label)))
  expect_equal(sum(result_no_bonus$pc), 1)
  expect_equal(result_no_bonus$valeur[result_no_bonus$resultat == 1], 10)
  expect_equal(result_no_bonus$valeur[result_no_bonus$resultat == 2], 5)
  expect_equal(result_no_bonus$valeur[result_no_bonus$resultat == 3], 2)

  # Test with bonus_recherche
  # bonus_recherche increases 'parfait' (cible 3) and decreases 'ok' (cible 2)
  # 'rate' (cible 1) is unaffected by this specific bonus application logic
  result_with_bonus <- creation_bdd_des(sample_actions_alea, bonus_recherche = 1)
  expect_equal(result_with_bonus$valeur[result_with_bonus$resultat == 1], 10) # Ratée, not affected by bonus
  expect_equal(result_with_bonus$valeur[result_with_bonus$resultat == 2], 4)  # Réussie, PA decreased by bonus
  expect_equal(result_with_bonus$valeur[result_with_bonus$resultat == 3], 3)  # Parfaite, PA increased by bonus
  expect_equal(sum(result_with_bonus$pc), 1)

  # Test bonus making 'ok' value 0 if bonus is large
  result_bonus_max_ok <- creation_bdd_des(sample_actions_alea, bonus_recherche = 6)
  expect_equal(result_bonus_max_ok$valeur[result_bonus_max_ok$resultat == 2], 0) # Réussie, PA becomes 0
  expect_equal(result_bonus_max_ok$valeur[result_bonus_max_ok$resultat == 3], 8) # Parfaite, PA increased

  # Test with multiple entries for the same outcome, last one should be used
  sample_actions_multiple <- tibble::tribble(
    ~user,   ~action, ~cible, ~PA, ~resultat, ~texte,
    "admin", "alea",  1,      10,  NA,        NA,
    "admin", "alea",  1,      12,  NA,        NA, # This one for Ratée
    "admin", "alea",  2,      5,   NA,        NA,
    "admin", "alea",  3,      2,   NA,        NA
  )
  result_multiple <- creation_bdd_des(sample_actions_multiple, bonus_recherche = 0)
  expect_equal(result_multiple$valeur[result_multiple$resultat == 1], 12)
  expect_equal(result_multiple$valeur[result_multiple$resultat == 2], 5)
  expect_equal(result_multiple$valeur[result_multiple$resultat == 3], 2)
})

test_that("creation_bdd_des handles missing alea actions or NA values in PA", {
  # No "alea" actions, or more accurately, no actions matching cible 1, 2, or 3
  # The function filters for `actions$cible == 1/2/3`, not action type "alea"
  # So, if these cibles are missing, PA values will be NA.
  sample_actions_no_alea <- tibble::tribble(
    ~user,   ~action, ~cible, ~PA, ~resultat, ~texte,
    "player1","other", 4,      1,   NA,        NA
  )
  # This will lead to NA for rate, ok, parfait values.
  # The function currently doesn't handle NA inputs gracefully for PAs,
  # it will try `NA[length(NA)]` which is `NA_real_`.
  # Then pc will be NA/sum(NA) = NA.
  # This test highlights a potential area for improvement in the source function
  # (e.g. returning 0 probabilities or erroring if no valid PA values found).
  # For now, testing current behavior.
  result_no_alea <- creation_bdd_des(sample_actions_no_alea)
  expect_true(all(is.na(result_no_alea$valeur)))
  expect_true(all(is.na(result_no_alea$pc))) # Since sum(valeur) will be NA

  # Actions with NA in PA for relevant cibles
  sample_actions_na_pa <- tibble::tribble(
    ~user,   ~action, ~cible, ~PA, ~resultat, ~texte,
    "admin", "alea",  1,      NA_real_,  NA,        NA,
    "admin", "alea",  2,      5,         NA,        NA,
    "admin", "alea",  3,      2,         NA,        NA
  )
  result_na_pa <- creation_bdd_des(sample_actions_na_pa)
  expect_true(is.na(result_na_pa$valeur[result_na_pa$resultat == 1]))
  expect_equal(result_na_pa$valeur[result_na_pa$resultat == 2], 5)
  # If one value is NA, sum(valeur) is NA, so all pc are NA
  expect_true(all(is.na(result_na_pa$pc)))
})


# Tests for plot_bdd_des
# Testing ggplot output is complex. We can check if it returns a ggplot object.
# We'd need ggplot2 to be available.

test_that("plot_bdd_des returns a ggplot object", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")

  sample_plot_data <- tibble::tribble(
    ~resultat, ~valeur, ~pc,    ~label,
    1,         10,      0.58,  "Ratée",
    2,         5,       0.29,  "Réussie",
    3,         2,       0.13,  "Parfaite"
  )
  # Ensure factors for correct ordering in plot if that's intended by reorder()
  sample_plot_data$label <- factor(sample_plot_data$label, levels = c("Ratée", "Réussie", "Parfaite"))


  plot_object <- plot_bdd_des(sample_plot_data)
  expect_s3_class(plot_object, "ggplot")

  # Further checks could involve inspecting layers, but that's more detailed.
  # For now, ensuring it doesn't error and returns a ggplot object is a good start.
  # expect_length(plot_object$layers, 2) # Expecting geom_bar and geom_text
  # expect_equal(plot_object$labels$title, "Probabilité de succès \nde l'enquête")
})
