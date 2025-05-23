# library(shiny)
# library(shinydashboard)
# library(DT)
# library(tidyverse) # Ensure specific functions are imported by the main package

habemus_server <- function(initial_actions_df, current_game_data_path) {

  # Parameter validation
  if (is.null(initial_actions_df) || is.null(current_game_data_path)) {
    stop("Game data path or initial actions are missing.")
  }
  
  # Assign parameters to the names expected by the original server logic
  server_name_from_param <- current_game_data_path # Renaming for clarity inside server function
  actions_from_param <- initial_actions_df       # Renaming for clarity

  # Infos scénario (can remain as is, or be passed as parameters if they vary)
  message_PA_insuffisant <- "Points de pouvoir insuffisants"

  # Return the actual shiny server function
  return(function(input, output, session) {

    USER <- shiny::reactiveValues(logged = FALSE)
    # Initialize reactive values with the passed-in data
    values <- shiny::reactiveValues(
      enquete_speciale = "Non", # Default or could be derived from initial_actions_df
      classement = "non",       # Default or could be derived
      actions = actions_from_param, # Use the passed data frame
      start = TRUE
    )

    # Persist changes to the CSV file
    shiny::observeEvent(values$actions, {
      # Ensure server_name_from_param is used for the filepath
      # Using write.csv2 as in original, consider consistency with read.csv in R/shiny.R
      utils::write.csv2(values$actions, server_name_from_param) 
    })

  # Message d'informations

  info <- shiny::reactiveVal("")
  info_log <- shiny::reactiveVal("")
  info_server <- shiny::reactiveVal("")
  info_admin <- shiny::reactiveVal("")

  output$info <- shiny::renderText(info())
  output$info_log <- shiny::renderText(info_log())
  output$info_server <- shiny::renderText(info_server())
  output$info_admin <- shiny::renderText(info_admin())

  #### Module log début de session ####

  output$logged <- shiny::renderText({
    if (!values$start) {
      'not_connected'
    } else if (USER$logged == TRUE) {
      if (USER$nom_user == 'admin') {
        'admin'
      } else {
        "ok"
      }
    } else {
      "not_ok"
    }
  })
  shiny::outputOptions(output, "logged", suspendWhenHidden = FALSE)

  shiny::observe({
    if (values$start) {
      info_log("Pour accéder à nos services, veuillez vous loguer")

      if (USER$logged == TRUE) {
        if (USER$nom_user == "admin") shiny::updateTabItems(session, "sidebar", "admin")
        else shiny::updateTabItems(session, "sidebar", "enquete")
      }
    }
  })

  shiny::observeEvent(input$boutton_log, {
    if (!is.null(input$username)) {
      Username <- shiny::isolate(input$username)
      Password <- shiny::isolate(input$password)

      current_info_user <- info_user(values$actions) # Use a different name to avoid conflict with the function
      Id.username <- which(current_info_user$user == Username)
      Id.password <- which(current_info_user$password == Password)
      if (length(Id.username) > 0 && length(Id.password) > 0) {
        if (Id.username %in% Id.password) {
          USER$logged <- TRUE
          USER$ligne_user <- Id.username
          USER$nom_user <- Username
          USER$PA_user_ini <- current_info_user %>% dplyr::filter(user == Username) %>% dplyr::pull(PA)

          # Existence des pouvoirs de classement (Habemus Papam)
          recup <- values$actions %>% dplyr::filter(action == "classement")
          if (nrow(recup) > 0) values$classement <- "oui"

        } else {
          info_log("Login ou mot de passe incorrect")
        }
      } else {
        info_log("Login ou mot de passe incorrect")
      }
    }
  })

  # Mise à jour des variables réactives lors du log
  shiny::observe({
    if (values$start && USER$logged == TRUE) {

      # Panneau latéral du personnage
      PA_actu <- values$actions %>%
        dplyr::filter(user == USER$nom_user) %>% dplyr::summarise(total_PA = sum(PA, na.rm = TRUE)) %>% dplyr::pull(total_PA)

      USER$PA_user <- as.numeric(USER$PA_user_ini) - PA_actu

      output$user_name <- shiny::renderText({ USER$nom_user })
      output$user_PA <- shiny::renderText({
        paste(as.character(USER$PA_user), " Point",
              ifelse(USER$PA_user > 1, "s", ""),
              " de pouvoir", sep = "")
      })

      # Mise à jour des listes de perso
      liste_perso <- values$actions %>%
        dplyr::filter(!user %in% c("HACK", "admin", "TEST", "Régis")) %>%
        dplyr::arrange(user) %>% 
        dplyr::pull(user) %>% 
        unique()

      shiny::updateSelectInput(session, "choix_user_copie", choices = liste_perso)
      shiny::updateSelectInput(session, "choix_user_chat", choices = c(liste_perso, "Tout le conseil", "Régis"))
      shiny::updateSelectInput(session, "choix_user_chat_admin", choices = c(liste_perso, "Tout le conseil"))
      shiny::updateSelectInput(session, "choix_user_classement", choices = liste_perso)
      shiny::updateSelectInput(session, "choix_user_indice", choices = liste_perso)
      shiny::updateSelectInput(session, "choix_user_password", choices = liste_perso)
      shiny::updateSelectInput(session, "choix_user_PA", choices = c("All", liste_perso))
      shiny::updateSelectInput(session, "user_classement_admin", choices = liste_perso)

      # Mise à jour des pouvoirs
      output$pv_classement <- shiny::renderText({ values$classement })
      shiny::outputOptions(output, "pv_classement", suspendWhenHidden = FALSE)

      output$pv_copie_classement <- shiny::renderText({
        if (values$classement == "oui") {
          recup_ini <- values$actions %>%
            dplyr::filter(user == USER$nom_user, action == "pouvoir",
                   cible == "copie_classement",
                   timer_ok < Sys.time() | is.na(timer_ok))

          recup_enq <- values$actions %>%
            dplyr::filter(user == USER$nom_user, action == "enquete", cible == "Q",
                   resultat == 3, timer_ok < Sys.time() | is.na(timer_ok))

          if (nrow(recup_enq) > 0) "oui" 
          else if (nrow(recup_ini) == 0) "non"
          else dplyr::pull(recup_ini[nrow(recup_ini), "resultat"])
        } else "non"
      })
      shiny::outputOptions(output, "pv_copie_classement", suspendWhenHidden = FALSE)

      output$pv_modif_classement <- shiny::renderText({
        if (values$classement == "oui") {
          recup_ini <- values$actions %>%
            dplyr::filter(user == USER$nom_user, action == "pouvoir",
                   cible == "modif_classement",
                   timer_ok < Sys.time() | is.na(timer_ok))

          if (nrow(recup_ini) == 0) "non"
          else dplyr::pull(recup_ini[nrow(recup_ini), "resultat"])
        } else "non"
      })
      shiny::outputOptions(output, "pv_modif_classement", suspendWhenHidden = FALSE)
    }
  })

  #### Informations ####

  # Info timer

  output$currentTime <- shiny::renderText({
    shiny::invalidateLater(1000, session)
    Sys.setenv(TZ = "Europe/Paris") # This should ideally be set globally for the app if needed
    format(Sys.time(), "%H:%M:%S")
  })


  #### Module Enquête basique ####

  # Choix de l'enquête

  shiny::observe({
    if (USER$logged == TRUE) {
      option_indices <- liste_indices(values$actions, USER$nom_user)
      shiny::updateSelectInput(session, "choix_enquete", choices = option_indices)
    }
  })

  # PA et graphique de réussite
  shiny::observe({
    if (USER$logged == TRUE) {
      # Nombre de PA
      shiny::updateNumericInput(session, "PA", max = USER$PA_user)

      # Tableau de probabilité
      bonus_recherche <- input$PA - 1 # input$PA should be numeric
      if (!is.numeric(input$PA) || is.na(input$PA)) bonus_recherche <- 0 # More robust check

      # Création de la BDD dés
      valeur_alea <- creation_bdd_des(values$actions, bonus_recherche)

      output$table_proba <- shiny::renderTable({
        valeur_alea %>% 
          dplyr::select(label, pc) %>%
          dplyr::arrange(dplyr::desc(pc)) %>% 
          dplyr::mutate(pc = paste0(round(pc * 100, 1), "%"))
      }, colnames = FALSE)

      output$plot_proba <- shiny::renderPlot({ plot_bdd_des(valeur_alea) })
    }
  })

  # Lancement de l'enquête
  shiny::observeEvent(input$boutton_enquete, {
    if (!is.null(input$choix_enquete)) {
      if (input$choix_enquete != "") {
        if (!is.numeric(input$PA) || is.na(input$PA) || input$PA <= 0) { # More robust check
          message_output <- "Petit futé.... Mais c'est râté !"
        } else if (USER$PA_user < input$PA) {
          message_output <- message_PA_insuffisant
        } else {
          out <- recherche_indice(values$actions, USER$nom_user, input$choix_enquete, input$PA)
          values$actions <- values$actions %>% dplyr::add_row(out$new_rows)
          show_message("Actions", out$message) # Assumes show_message is available (e.g. from murderParty package)
          message_output <- ""
        }
      } else { # Added for clarity: if choix_enquete is ""
        message_output <- "Veuillez sélectionner une enquête."
      }
    } else {
      message_output <- "Veuillez sélectionner une enquête."
    }
    info(message_output)
  })

  shiny::observeEvent(input$boutton_enquete_help, {
    shiny::updateControlbar(id = "controlbar", session = session)
    # updateControlbarMenu is not a standard shiny function, likely from shinydashboardPlus or custom
    # Assuming it's available in the environment where this app runs.
    # For robustness, it would be shinydashboardPlus::updateControlbarMenu if that's its origin.
    updateControlbarMenu("controlbarMenu", selected = "Enquête")
  })
  shiny::observeEvent(input$boutton_enquete_advanced_help, {
    shiny::updateControlbar(id = "controlbar", session = session)
    updateControlbarMenu("controlbarMenu", selected = "Enquête")
  })
  shiny::observeEvent(input$boutton_enquete_copie_help, {
    shiny::updateControlbar(id = "controlbar", session = session)
    updateControlbarMenu("controlbarMenu", selected = "Copie")
  })
  shiny::observeEvent(input$boutton_enquete_interception_help, {
    shiny::updateControlbar(id = "controlbar", session = session)
    updateControlbarMenu("controlbarMenu", selected = "Interception")
  })


  #### Module de recherche d'indices avancés ####

  # Fonction pour pondérer les résultats
  alea_pond <- function(mot, base_text_vector) { # Renamed 'base' to 'base_text_vector'
    # Ensure mot is not empty or NA
    if (is.null(mot) || is.na(mot) || nchar(mot) == 0) return(NULL)
    
    nb_vec <- unlist(lapply(gregexpr(mot, base_text_vector, ignore.case = TRUE),
                            function(x) if (x[1] != -1) length(x) else 0))
    result_indices <- NULL # Renamed 'result' to 'result_indices'
    if (length(nb_vec) > 0) {
      for (i in seq_along(nb_vec)) {
        if (nb_vec[i] > 0) { # Check if nb_vec[i] is not 0
          for (k in seq_len(nb_vec[i])) { # Use seq_len for robustness
            result_indices <- c(result_indices, i)
          }
        }
      }
    }
    return(result_indices)
  }

  # Recherche d'indice avancée
  shiny::observeEvent(input$boutton_enquete_advanced, {
    if (!is.numeric(input$PA) || is.na(input$PA) || input$PA <= 0) { # More robust check
      info("Petit futé.... Mais c'est râté !")
    } else if (USER$PA_user < input$PA) {
      info(message_PA_insuffisant)
    } else {
      recup1 <- NULL; recup1b <- NULL # Initialize on one line
      recup2 <- NULL; recup2b <- NULL
      recup3 <- NULL; recup3b <- NULL

      # indices non spéciaux
      indices_basiques <- info_indices(values$actions) %>%
        dplyr::filter(nchar(indice) == 1 & variation == 2)

      if (input$advanced_1 != "") {
        recup1 <- alea_pond(input$advanced_1, indices_basiques$texte)
        recup1b <- alea_pond(input$advanced_1, indices_basiques$titre)
      }
      if (input$advanced_2 != "") {
        recup2 <- alea_pond(input$advanced_2, indices_basiques$texte)
        recup2b <- alea_pond(input$advanced_2, indices_basiques$titre)
      }
      if (input$advanced_3 != "") {
        recup3 <- alea_pond(input$advanced_3, indices_basiques$texte)
        recup3b <- alea_pond(input$advanced_3, indices_basiques$titre)
      }

      # Combine and ensure unique indices are pulled
      all_recup_indices <- unique(c(recup1, recup1b, recup2, recup2b, recup3, recup3b))
      
      # Check if all_recup_indices is not NULL and has length > 0
      if (length(all_recup_indices) > 0 && !all(is.na(all_recup_indices))) {
          valid_indices <- indices_basiques$indice[all_recup_indices] # Get actual indice values
          valid_indices <- valid_indices[!is.na(valid_indices)] # Remove any NAs from subsetting
          
          if(length(valid_indices) > 0) {
            out <- recherche_indice(values$actions, USER$nom_user, sample(valid_indices, 1), input$PA)
          } else { # Fallback if no valid indices found from keywords
            enquete_alea <- sample(dplyr::pull(indices_basiques, 'indice'), 1)
            out <- recherche_indice(values$actions, USER$nom_user, enquete_alea, input$PA, resultat = 1) # Default to resultat = 1 (Ratée)
          }
      } else {
        # Résultat aléatoire if no keywords match or no keywords provided
        enquete_alea <- sample(dplyr::pull(indices_basiques, 'indice'), 1)
        out <- recherche_indice(values$actions, USER$nom_user, enquete_alea, input$PA, resultat = 1) # Default to resultat = 1 (Ratée)
      }
      values$actions <- values$actions %>% dplyr::add_row(out$new_rows)
      show_message("Actions", out$message)
    }
  })

  #### Module de Copie d'un indice ####

  shiny::observeEvent(input$boutton_enquete_copie, {
    if (USER$PA_user < 2) {
      info(message_PA_insuffisant)
    } else {
      out <- copie_indice(values$actions, USER$nom_user, input$choix_user_copie, 2)
      values$actions <- values$actions %>% dplyr::add_row(out$new_rows)
      show_message("Actions", out$message)
    }
  })

  #### Intercepter une enquête ####

  shiny::observeEvent(input$boutton_enquete_interception, {
    if (USER$PA_user < 2) {
      info(message_PA_insuffisant)
    } else {
      out <- interception_indice(values$actions, USER$nom_user,
                                 input$choix_user_copie, 2)
      values$actions <- values$actions %>% dplyr::add_row(out$new_rows)
      show_message("Actions", out$message)
    }
  })

  #### Module lecture des indices ####

  shiny::observe({
    if (USER$logged == TRUE) {
      # Vérification de l'activation des enquêtes spéciales
      nb_car_enq <- 1
      enq_spe <- values$actions %>% dplyr::filter(action == "enq_spe") %>% dplyr::pull(resultat)

      if (length(enq_spe) == 0) enq_spe <- "Non" # Default if no "enq_spe" action found
      if (enq_spe[length(enq_spe)] == "Oui") nb_car_enq <- 2

      # Création du tableau
      tableau_indices_raw <- values$actions %>%  # Renamed to avoid conflict
        dplyr::filter(user == USER$nom_user,
               action %in% c("enquete", "Enquete interceptee"), # Use %in% for multiple conditions
               timer_ok < Sys.time(), 
               nchar(cible) <= nb_car_enq)

      tableau_indices_formatted <- tableau_indices_raw %>% # Renamed
        dplyr::rename(indice_col = cible) %>% # Renamed 'indice' to avoid conflict with info_indices columns
        dplyr::left_join(info_indices(values$actions) %>%
                    dplyr::filter(variation == 3) %>%
                    dplyr::select(titre, indice), by = c("indice_col" = "indice")) %>% # Specify join columns
        dplyr::mutate(
          texte = dplyr::if_else(action == "Enquete interceptee",
            "Malheureusement, une équipe surnaturelle (nous ne savons dire s'il s'agissait de démons ou d'anges) nous est tombée dessus, et nous a intercepté le rapport de l'enquête avant que nous puissons vous le transmettre. Nous vous promettons que cela n'arrivera plus jamais.",
            texte),
          resultat = dplyr::case_when(
            resultat == "1" ~ "Ratée", # Ensure resultat is character for case_when
            resultat == "2" ~ "Réussie",
            resultat == "3" ~ "Parfaite",
            TRUE ~ as.character(resultat) # Ensure it's character
          )
        )

      tableau_classement <- values$actions %>%
        dplyr::filter(user == USER$nom_user,
               action == "enquete",
               timer_ok < Sys.time(),
               cible == "classement") %>%
        dplyr::rename(indice_col = cible) %>%
        dplyr::mutate(titre = "Classement actuel des entités",
               texte = as.character(resultat), resultat = "")

      # Combine using bind_rows for safety with differing columns initially
      tableau_indices_final <- dplyr::bind_rows(tableau_indices_formatted, tableau_classement)

      if (nrow(tableau_indices_final) > 0) {
        tableau_indices_display <- tableau_indices_final %>%
          dplyr::arrange(dplyr::desc(timer_ok)) %>%
          dplyr::select(titre, timer_ok, resultat, texte) %>%
          dplyr::mutate(timer_ok = format(prepare_timer(timer_ok), format = "%H:%M:%S"),
                 texte = stringr::str_replace_all(texte, "\n", "<br/>"))

        colnames(tableau_indices_display) <- c("Enquête", "Heure d'arrivée", "Résultat", "Rapport d'enquête")

        output$tableau_indices <- DT::renderDataTable(
          tableau_indices_display, rownames = FALSE, escape = FALSE,
          options = list(ordering = FALSE, info = FALSE)
        )
      } else {
        # Ensure output is cleared if no data
        output$tableau_indices <- DT::renderDataTable(NULL)
      }
    }
  })

  #### Module de classement des entités ####

  # Classement des entités
  shiny::observeEvent(input$boutton_copie_classement, {
    if (USER$PA_user < 2) {
      info(message_PA_insuffisant)
    } else {
      out <- copie_classement(values$actions, USER$nom_user, "", 2) # cible is "" for copie_classement
      values$actions <- values$actions %>% dplyr::add_row(out$new_rows)
      show_message("Classement", out$message)
    }
  })

  # Modification du classement des entités
  shiny::observeEvent(input$boutton_modif_classement, {
    if (USER$PA_user < 3) {
      info(message_PA_insuffisant)
    } else {
      out <- modification_classement(values$actions, USER$nom_user,
                                     input$choix_user_classement, 3)
      values$actions <- values$actions %>% dplyr::add_row(out$new_rows)
      show_message("Classement", out$message)
    }
  })

  #### Module Chat ####

  shiny::observe({
    if (USER$logged == TRUE) {
      tableau_chat_raw <- values$actions %>%  # Renamed
        dplyr::filter(cible == USER$nom_user, action == "chat", timer_ok < Sys.time()) %>%
        dplyr::select(user, cible, PA, resultat, timer_ok) # Removed duplicate resultat

      if (nrow(tableau_chat_raw) > 0) {
        tableau_chat_raw[tableau_chat_raw$PA == 1, 'user'] <- "Anonyme"
      }
      
      tableau_chat_display <- tableau_chat_raw %>%
        dplyr::arrange(dplyr::desc(timer_ok)) %>%
        dplyr::select(user, timer_ok, resultat) %>%
        dplyr::mutate(timer_ok = format(prepare_timer(timer_ok), format = "%H:%M:%S"),
               resultat = stringr::str_replace_all(resultat, "\n", "<br/>"))

      colnames(tableau_chat_display) <- c("Expéditeur", "Heure d'envoi", "Message")

      if (nrow(tableau_chat_display) > 0) {
        output$tableau_chat <- DT::renderDataTable(
          tableau_chat_display, rownames = FALSE, escape = FALSE, options = list(
            paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE)
        )
      } else {
        output$tableau_chat <- DT::renderDataTable(NULL)
      }
    }
  })

  shiny::observeEvent(input$boutton_chat, {
    ano <- ifelse(input$ano_chat, 1, 0)

    if (ano == 1 && USER$PA_user < 1) { # Added && for clarity
      info(message_PA_insuffisant)
    } else {
      out <- envoi_message(values$actions, USER$nom_user, input$choix_user_chat,
                           input$message_chat, ano, input$timer_chat * 60)
      values$actions <- values$actions %>% dplyr::add_row(out$new_rows)
      show_message("Chat", out$message)
    }
    shiny::updateCheckboxInput(session, "ano_chat", value = FALSE)
  })

  #### Module d'administration de la murder ####

  ##### Suivi de la murder ####

  ###### Chat Admin ######

  output$tableau_chat_admin <- DT::renderDataTable({
    tableau_chat_admin_data <- values$actions %>% # Renamed
      dplyr::filter(cible == "Régis",
             action == "chat",
             timer_ok < Sys.time()) %>%
      dplyr::select(user, PA, resultat, timer_ok) %>% # Removed cible, PA was used for anon check before
      dplyr::arrange(dplyr::desc(timer_ok)) %>%
      # dplyr::select(user, timer_ok, resultat) %>% # No, select before mutate for timer_ok
      dplyr::mutate(timer_ok = format(as.POSIXct(timer_ok, origin = "1970-01-01", # No need for prepare_timer if already POSIXct
                                          tz = "Europe/Paris"), format = "%H:%M:%S"),
             resultat = stringr::str_replace_all(resultat, "\n", "<br/>")) %>%
      dplyr::select(Expéditeur = user, `Heure d'envoi` = timer_ok, Message = resultat)


    tableau_chat_admin_data # Use the renamed variable
  }, rownames = FALSE, escape = FALSE,
  options = list(paging = FALSE, searching = FALSE,
                 ordering = FALSE, info = FALSE))

  shiny::observeEvent(input$boutton_chat_admin, {
    pj <- input$choix_user_chat_admin

    if (pj == "Tout le conseil") pj <- info_user(values$actions)$user
    
    new_rows_list <- lapply(pj, function(single_pj) { # Handle multiple pjs if "Tout le conseil"
        tibble::tibble(
          user = "Régis", action = "chat", cible = single_pj, PA = 0,
          timer = Sys.time(), resultat = input$message_chat_admin,
          timer_ok = Sys.time(), texte = "")
    })
    new_rows_df <- dplyr::bind_rows(new_rows_list)


    values$actions <- values$actions %>% dplyr::add_row(new_rows_df)
    info_admin("Message envoyé")
  })


  ###### Timer #####

  output$text_timer_admin <- shiny::renderText({
    info_timer_data <- values$actions %>% # Renamed
      dplyr::filter(user == "admin", action == "timer") %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% # Explicit dplyr::
      dplyr::pull(timer)

    if (length(info_timer_data) == 1) {
      info_timer_val <- difftime(Sys.time(), info_timer_data, units = "secs") # Renamed
      paste0(display_duration(info_timer_val))
    } else {
      "Aucun timer activé"
    }
  })

  output$text_timer_PA_admin <- shiny::renderText({
    info_timer_pa_data <- values$actions %>% # Renamed
      dplyr::filter(user == "admin", action == "admin_PA") %>%
      dplyr::filter(dplyr::row_number() == dplyr::n()) %>% # Explicit dplyr::
      dplyr::pull(timer)

    if (length(info_timer_pa_data) == 1) {
      info_timer_pa_val <- difftime(Sys.time(), info_timer_pa_data, units = "secs") # Renamed
      paste0(display_duration(info_timer_pa_val))
    } else {
      "Aucun PA distribué"
    }
  })

  shiny::observeEvent(input$bouton_timer_admin, {
    new_rows <- tibble::tibble(
      user = "admin", action = "timer", cible = "", PA = NA,
      timer = Sys.time(), resultat = "", timer_ok = Sys.time(), texte = "") # Corrected empty resultat field

    values$actions <- values$actions %>% dplyr::add_row(new_rows)
    info_admin("Timer mis à jour")
  })

  ###### Action rapide : + 4 PA ######
  shiny::observeEvent(input$bouton_PA_all, {
    current_info_user <- info_user(values$actions) # Store result
    new_rows <- tibble::tibble(
      user = current_info_user$user, action = "admin_PA", cible = "", PA = -4,
      timer = Sys.time(), resultat = "", timer_ok = Sys.time(), texte = "")

    values$actions <- values$actions %>% dplyr::add_row(new_rows)
    info_admin("4 PA envoyé à toustes")
  })

  ##### Module de vérification des actions ####

  output$table_admin <- DT::renderDataTable({
    values$actions %>%
      dplyr::filter(action != "init") %>% 
      dplyr::arrange(dplyr::desc(timer_ok)) %>%
      dplyr::mutate(timer_ok = format(prepare_timer(timer_ok), format = "%H:%M:%S"),
             timer = format(prepare_timer(timer), format = "%H:%M:%S"))
  }, rownames = FALSE)

  ##### Module de vérification des indices obtenus ####

  output$table_indices_admin <- shiny::renderTable({
    if (!is.null(input$choix_enquete_admin)) {
      if (input$choix_enquete_admin != "dynamique") {
        # on prend la liste des indices possibles
        liste_indices_possible <- info_indices(values$actions) %>% # Renamed
          dplyr::filter(indice == input$choix_enquete_admin) %>%
          dplyr::select(indice, titre, variation) %>%
          dplyr::mutate(variation = as.character(variation))

        liste_enquete_actual <- values$actions %>% # Renamed
          dplyr::filter(cible == input$choix_enquete_admin, action == "enquete") %>%
          dplyr::group_by(indice = cible, variation = resultat) %>%
          dplyr::summarise(user = paste0(unique(user), collapse = ", "), .groups = 'drop') # Added .groups

        liste_indices_display <- liste_indices_possible %>% # Renamed
          dplyr::left_join(liste_enquete_actual, by = c("indice", "variation")) %>% # Specify by
          dplyr::mutate(Résultat = dplyr::case_when(
            variation == "1" ~ "Ratée",
            variation == "2" ~ "Réussie",
            variation == "3" ~ "Parfaite",
            TRUE ~ "erreur")) %>%
          dplyr::select(Résultat, Personnage = user)

        liste_indices_display
      } else { NULL } # Return NULL if "dynamique"
    } else { NULL } # Return NULL if input is NULL
  })

  ##### Module de vérification des infos PJ ####

  output$table_admin_PJ <- DT::renderDataTable({
    # Nombre de PA de chaque joueur
    info_user_update <- info_user(values$actions) %>%
      dplyr::filter(!user %in% c("HACK", "admin", "TEST")) %>%
      dplyr::rename(PA_ini = PA) %>%
      dplyr::left_join(values$actions %>%
                  dplyr::group_by(user) %>%
                  dplyr::summarise(PA_actu = sum(PA, na.rm = TRUE), .groups = 'drop'), by = "user") %>% # Added .groups and by
      dplyr::mutate(PA_actu = PA_ini - PA_actu) %>%
      dplyr::arrange(user) %>%
      dplyr::select(Personnage = user, `Mot de passe` = password, `PA initiaux` = PA_ini,
             `PA actuels` = PA_actu)

    liste_reussite <- values$actions %>%
      dplyr::filter(action == "enquete") %>%
      dplyr::group_by(user, resultat) %>%
      dplyr::summarise(nb_enquete = dplyr::n(), .groups = 'drop') %>% # Added .groups
      dplyr::group_by(user) %>%
      dplyr::mutate(pc_enquete = round(100 * nb_enquete / sum(nb_enquete, na.rm = TRUE)),
             nb_enquete = paste0(nb_enquete, " (", pc_enquete, "%)"),
             resultat = dplyr::case_when(
               resultat == "1" ~ "Ratée",
               resultat == "2" ~ "Réussie",
               resultat == "3" ~ "Parfaite",
               TRUE ~ "erreur")) %>%
      dplyr::ungroup() %>%
      dplyr::select(Personnage = user, resultat, nb_enquete) %>%
      tidyr::pivot_wider(names_from = resultat, values_from = nb_enquete) # Explicit tidyr::

    info_user_update <- info_user_update %>%
      dplyr::left_join(liste_reussite, by = "Personnage")

    if (values$classement == "oui") {
      liste_classement <- bdd_classement(values$actions) %>%
        dplyr::rename(Personnage = cible, Classement = resultat)
      info_user_update <- info_user_update %>% dplyr::left_join(liste_classement, by = "Personnage")
    }

    DT::datatable(info_user_update, # Explicit DT::
              options = list(pageLength = 100),
              rownames = FALSE)
  })

  shiny::observeEvent(input$boutton_admin_PA, {
    current_info_user <- info_user(values$actions) # Store result
    # timer_now <- Sys.time() # Unused

    pj <- input$choix_user_PA
    if (pj == "All") pj <- current_info_user$user

    new_rows <- tibble::tibble(
      user = pj, action = "admin_PA", cible = "", PA = -input$PA_admin,
      timer = Sys.time(), resultat = "", timer_ok = Sys.time(), texte = "")

    values$actions <- values$actions %>% dplyr::add_row(new_rows)

    message_output <- paste(input$PA_admin, " PA attribué à ",
                            input$choix_user_PA, sep = "")
    info_admin(message_output)
  })

  shiny::observeEvent(input$boutton_admin_password, {
    current_pa <- info_user(values$actions) %>% # Renamed PA to current_pa
      dplyr::filter(user == input$choix_user_password) %>% dplyr::pull(PA)

    # prepare_row_drive seems to be a helper not defined in this file or package
    # For now, commenting it out as it would cause an error.
    # new_row <-  prepare_row_drive(values$id_session,"admin","password",
    #                               input$choix_user_password,PA,Sys.time(),
    #                               input$password_admin,Sys.time())

    new_rows <- tibble::tibble(
      user = "admin", action = "password", cible = input$choix_user_password, PA = current_pa,
      timer = Sys.time(), resultat = input$password_admin, timer_ok = Sys.time(), texte = "")

    values$actions <- values$actions %>% dplyr::add_row(new_rows)

    info_admin(paste0("Nouveau mot de passe pour ", input$choix_user_password,
                      " : ", input$password_admin))
  })

  shiny::observeEvent(input$boutton_admin_classement, {
    new_rows <- tibble::tibble(
      user = "admin", action = "classement", cible = input$user_classement_admin, PA = 0,
      timer = Sys.time(), resultat = input$points_classement_admin,
      timer_ok = Sys.time(), texte = "")

    values$actions <- values$actions %>% dplyr::add_row(new_rows)
    info_admin(paste("Modification du classement pour",
                     input$user_classement_admin))
  })

  ##### Module de modification des indices ####

  output$table_admin_indices <- DT::renderDT({ 
    DT::datatable(info_indices(values$actions) %>% 
                dplyr::select(indice, titre, variation, texte),
              editable = TRUE)
  })

  shiny::observeEvent(input$table_admin_indices_cell_edit, {
    temp_indices <- info_indices(values$actions) %>%
      dplyr::select(indice, titre, variation, texte) 

    row  <- input$table_admin_indices_cell_edit$row
    col  <- input$table_admin_indices_cell_edit$col 

    if (col == 4) { # Assuming column 4 is 'texte'
      indice_val <- dplyr::pull(temp_indices[row, 1]) 
      titre_val <- dplyr::pull(temp_indices[row, 2])   
      variation_val <- dplyr::pull(temp_indices[row, 3]) 
      texte_val <- input$table_admin_indices_cell_edit$value

      new_rows <- tibble::tibble( 
        user = "admin", action = "init", cible = indice_val, PA = variation_val,
        timer = Sys.time(), resultat = titre_val,
        timer_ok = Sys.time(), texte = texte_val)

      values$actions <- values$actions %>% dplyr::add_row(new_rows) 
      info_admin("Modification des indices")
    }
  })

  ##### Module d'ajout des indices ####

  option_variations <- c("Enquête ratée" = 1,
                         "Enquête réussie" = 2,
                         "Enquête parfaite" = 3)
  shiny::observe({ # Added observe for this updateSelectInput as it depends on session
      shiny::updateSelectInput(session, "choix_variation", choices = option_variations)
  })


  shiny::observe({
    if (USER$logged == TRUE) {
      if (USER$nom_user == "admin") {
        current_liste_indices <- info_indices(values$actions) %>% # Renamed
          dplyr::filter(variation == 3) %>%
          dplyr::select(titre, texte, variation, indice)

        option_indices <- list()
        if (nrow(current_liste_indices) > 0) { # Check if there are indices
            for (i in current_liste_indices$indice) { # Loop through actual indice values
              titre <- current_liste_indices %>% 
                dplyr::filter(indice == i) %>%
                # dplyr::mutate(lettre_titre = paste0(i, " : ", titre)) %>% # Not needed if titre is unique
                dplyr::pull(titre)
              if (length(titre) > 0) option_indices[[titre[1]]] <- i # Use first title if multiple
            }
        }
        shiny::updateSelectInput(session, "choix_enquete_admin",
                          choices = option_indices, selected = NULL)
      }
    }
  })

  shiny::observe({
    if (USER$logged == TRUE) {
      if (!is.null(input$choix_enquete_admin)) {
        ligne_indice <- info_indices(values$actions) %>%
          dplyr::filter(indice == input$choix_enquete_admin)

        lettre <- ligne_indice %>% dplyr::filter(variation == 1) %>% dplyr::pull(indice)
        if (nrow(ligne_indice) == 0) lettre <- "" # Default if no rows
        
        current_info <- "" # Renamed to avoid conflict
        if (stringr::str_length(lettre) == 2) { # Explicit stringr::
          current_info <- paste0("Il s'agit d'une enquête spéciale, que les joueur·euse·s peuvent avoir s'iels obtiennent une enquête partfaite sur l'enquête ", substr(lettre, 1, 1), ". Vous pouvez envoyer cette indice à n'importe qui, mais sachez que ces enquêtes spéciales ne sont accessibles qu'à partir du moment où l'action spéciale 'enquêtes spéciales' est activée.")
        } else {
          current_info <- ""
        }
        output$admin_info_lettre <- shiny::renderText({ current_info })

        output$admin_indice_1 <- shiny::renderText({ ligne_indice %>% dplyr::filter(variation == 1) %>% dplyr::pull(texte) })
        output$admin_indice_2 <- shiny::renderText({ ligne_indice %>% dplyr::filter(variation == 2) %>% dplyr::pull(texte) })
        output$admin_indice_3 <- shiny::renderText({ ligne_indice %>% dplyr::filter(variation == 3) %>% dplyr::pull(texte) })

      } else {
        output$admin_info_lettre <- shiny::renderText(NULL)
        output$admin_indice_1 <-  shiny::renderText(NULL)
        output$admin_indice_2 <-  shiny::renderText(NULL)
        output$admin_indice_3 <-  shiny::renderText(NULL)
      }
    }
  })

  shiny::observeEvent(input$boutton_admin_indice, {
    if (!is.null(input$choix_enquete_admin)) {
      out <- recherche_indice(values$actions,
                              input$choix_user_indice,
                              input$choix_enquete_admin,
                              0, input$choix_variation, 0) # PA is 0, duree is 0
      values$actions <- values$actions %>% dplyr::add_row(out$new_rows)
      info_admin(paste0(input$choix_enquete_admin, " attribué à ",
                        input$choix_user_indice, sep = ""))
    }
  })

  ##### Module de modification du dé ####

  shiny::observe({
    if (USER$logged == TRUE) {
      if (USER$nom_user == "admin") {
        # Création de la BDD dés
        valeur_alea <- creation_bdd_des(values$actions)

        nb_1 <- valeur_alea %>% dplyr::filter(resultat == 1) %>% dplyr::pull(valeur)
        nb_2 <- valeur_alea %>% dplyr::filter(resultat == 2) %>% dplyr::pull(valeur)
        nb_3 <- valeur_alea %>% dplyr::filter(resultat == 3) %>% dplyr::pull(valeur)

        shiny::updateNumericInput(session, "Nb_echec", value = nb_1)
        shiny::updateNumericInput(session, "Nb_reussite", value = nb_2)
        shiny::updateNumericInput(session, "Nb_reussite_parfaite", value = nb_3)
      }
    }
  })

  output$plot_proba_admin <- shiny::renderPlot({
    valeur_alea <- creation_bdd_des(values$actions) %>%
      dplyr::mutate(valeur = dplyr::case_when(resultat == 1 ~ input$Nb_echec,
                                resultat == 2 ~ input$Nb_reussite,
                                resultat == 3 ~ input$Nb_reussite_parfaite,
                                TRUE ~ 99)) %>% # Ensure this default makes sense
      dplyr::mutate(pc = valeur / sum(valeur, na.rm = TRUE)) # Added na.rm

    plot_bdd_des(valeur_alea)
  })

  shiny::observeEvent(input$boutton_admin_des, {
    # timer_ok <- Sys.time() # Unused variable

    new_rows <- tibble::tibble(
      user = "admin", action = "alea", cible = 1, PA = input$Nb_echec,
      timer = Sys.time(), resultat = "", timer_ok = Sys.time(), texte = "") %>%
      dplyr::add_row(
        user = "admin", action = "alea", cible = 2, PA = input$Nb_reussite,
        timer = Sys.time(), resultat = "", timer_ok = Sys.time(), texte = "") %>%
      dplyr::add_row(
        user = "admin", action = "alea", cible = 3, PA = input$Nb_reussite_parfaite,
        timer = Sys.time(), resultat = "", timer_ok = Sys.time(), texte = "")

    values$actions <- values$actions %>% dplyr::add_row(new_rows)
    info_admin("Modification du dé")
  })

  ##### Module des actions spéciales ####

  shiny::observeEvent(input$enquete_speciale_on, {
    new_rows <- tibble::tibble(
      user = "admin", action = "enq_spe", cible = NA, PA = 0, # cible should be character if it can be string
      timer = Sys.time(), resultat = "Oui", timer_ok = Sys.time(), texte = "")

    values$actions <- values$actions %>% dplyr::add_row(new_rows)
    info_admin("Enquête spéciale activée")
  })

  shiny::observeEvent(input$enquete_speciale_off, {
    new_rows <- tibble::tibble(
      user = "admin", action = "enq_spe", cible = NA, PA = 0, # cible should be character
      timer = Sys.time(), resultat = "Non", timer_ok = Sys.time(), texte = "")

    values$actions <- values$actions %>% dplyr::add_row(new_rows)
    info_admin("Enquête spéciale désactivée")
  })

  shiny::observeEvent(input$enquete_chat_gpt_on, {
    new_rows <- tibble::tibble(
      user = "admin", action = "chat_gpt", cible = NA, PA = 0, # cible should be character
      timer = Sys.time(), resultat = "Oui", timer_ok = Sys.time(), texte = "")

    values$actions <- values$actions %>% dplyr::add_row(new_rows)
    info_admin("Modification par Chat GPT activée")
  })

  shiny::observeEvent(input$enquete_chat_gpt_off, {
    new_rows <- tibble::tibble(
      user = "admin", action = "chat_gpt", cible = NA, PA = 0, # cible should be character
      timer = Sys.time(), resultat = "Non", timer_ok = Sys.time(), texte = "")

    values$actions <- values$actions %>% dplyr::add_row(new_rows)
    info_admin("Modification par Chat GPT désactivée")
  })

  output$admin_table_autre_actions <- shiny::renderTable({
    fl_chat_gpt_actions <- values$actions %>% dplyr::filter(action == "chat_gpt") %>% dplyr::pull(resultat) 
    fl_enq_spe_actions <- values$actions %>% dplyr::filter(action == "enq_spe") %>% dplyr::pull(resultat)   

    # Use last entry, default to "Non" if no entries
    fl_chat_gpt_status <- if (length(fl_chat_gpt_actions) > 0) fl_chat_gpt_actions[length(fl_chat_gpt_actions)] else "Non"
    fl_enq_spe_status <- if (length(fl_enq_spe_actions) > 0) fl_enq_spe_actions[length(fl_enq_spe_actions)] else "Non"
    
    # Ensure status is either "Oui" or "Non"
    fl_chat_gpt_status <- ifelse(fl_chat_gpt_status == "Oui", "Oui", "Non")
    fl_enq_spe_status <-  ifelse(fl_enq_spe_status == "Oui", "Oui", "Non")

    table_autre_actions <- tibble::tibble(Actions = character(), Statut = character()) %>% 
      dplyr::add_row(Actions = "Enquête spéciale", Statut = fl_enq_spe_status) %>%          
      dplyr::add_row(Actions = "Réécriture par ChatGPT", Statut = fl_chat_gpt_status)       

    table_autre_actions
  })

  }) # End of the returned shinyServer function
} # End of habemus_server factory function
