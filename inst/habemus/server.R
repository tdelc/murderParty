library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

server_name <- getOption("habemus.server_name", default = NULL)
actions <- getOption("habemus.actions", default = NULL)
if (is.null(actions) | is.null(server_name)) {
  stop("Aucun nom de serveur ou server_name défini.
       Utilisez lancer_serveur_habemus(server_name)
       pour lancer l'application.")
}

# Infos scénario

message_PA_insuffisant <- "Points de pouvoir insuffisants"

shinyServer(function(input, output,session) {

  USER <- reactiveValues(logged = FALSE)
  values <- reactiveValues(enquete_speciale="Non",classement="non",
                           actions=actions,start=TRUE)

  observeEvent(values$actions,{
    write.csv2(values$actions,server_name)
  })

  # Message d'informations

  info <- reactiveVal("")
  info_log <- reactiveVal("")
  info_server <- reactiveVal("")
  info_admin <- reactiveVal("")

  output$info <- renderText(info())
  output$info_log <- renderText(info_log())
  output$info_server <- renderText(info_server())
  output$info_admin <- renderText(info_admin())

  #### Module log début de session ####

  output$logged <- renderText({
    if (!values$start){
      'not_connected'
    } else if(USER$logged == TRUE){
      if (USER$nom_user == 'admin'){
        'admin'
      }else{
        "ok"
      }
    }else{
      "not_ok"
    }
  })
  outputOptions(output, "logged", suspendWhenHidden = FALSE)

  observe({

    if (values$start){

      info_log("Pour accéder à nos services, veuillez vous loguer")

      if (USER$logged == TRUE) {
        if (USER$nom_user == "admin") updateTabItems(session,"sidebar","admin")
        else updateTabItems(session,"sidebar","enquete")
      }
    }
  })

  observeEvent(input$boutton_log, {
    if (!is.null(input$username)) {
      Username <- isolate(input$username)
      Password <- isolate(input$password)

      info_user <- info_user(values$actions)
      Id.username <- which(info_user$user == Username)
      Id.password <- which(info_user$password == Password)
      if (length(Id.username) > 0 & length(Id.password) > 0) {
        if (Id.username %in% Id.password) {
          USER$logged <- TRUE
          USER$ligne_user <- Id.username
          USER$nom_user <- Username
          USER$PA_user_ini <- info_user %>% filter(user == Username) %>% pull(PA)

          # Existence des pouvoirs de classement (Habemus Papam)
          recup <- values$actions %>% filter(action == "classement")
          if (nrow(recup) > 0) values$classement <- "oui"

        }else{
          info_log("Login ou mot de passe incorrect")
        }
      }else{
        info_log("Login ou mot de passe incorrect")
      }
    }
  })

  # Mise à jour des variables réactives lors du log
  observe({
    if (values$start & USER$logged == TRUE) {

      # Panneau latéral du personnage
      PA_actu <- values$actions %>%
        filter(user == USER$nom_user) %>% summarise(sum(PA)) %>% pull()

      USER$PA_user <- as.numeric(USER$PA_user_ini) - PA_actu

      output$user_name <- renderText({USER$nom_user})
      output$user_PA <- renderText({paste(as.character(USER$PA_user)," Point",
                                          ifelse(USER$PA_user >1,"s",""),
                                          " de pouvoir",sep="")})

      # Mise à jour des listes de perso
      liste_perso <- values$actions %>%
        filter(!user %in% c("HACK","admin","TEST","Régis")) %>%
        arrange(user) %>% pull(user) %>% unique()

      updateSelectInput(session,"choix_user_copie",choices = liste_perso)
      updateSelectInput(session,"choix_user_chat",choices = c(liste_perso,"Tout le conseil","Régis"))
      updateSelectInput(session,"choix_user_chat_admin",choices = c(liste_perso,"Tout le conseil"))
      updateSelectInput(session,"choix_user_classement",choices = liste_perso)
      updateSelectInput(session,"choix_user_indice",choices = liste_perso)
      updateSelectInput(session,"choix_user_password",choices = liste_perso)
      updateSelectInput(session,"choix_user_PA",choices = c("All",liste_perso))
      updateSelectInput(session,"user_classement_admin",choices = liste_perso)

      # Mise à jour des pouvoirs
      output$pv_classement <- renderText({values$classement})
      outputOptions(output, "pv_classement", suspendWhenHidden = FALSE)

      output$pv_copie_classement <- renderText({
        if (values$classement == "oui"){
          recup_ini <- values$actions %>%
            filter(user == USER$nom_user, action == "pouvoir",
                   cible == "copie_classement",
                   timer_ok < Sys.time() | is.na(timer_ok))

          recup_enq <- values$actions %>%
            filter(user == USER$nom_user, action == "enquete", cible == "Q",
                   resultat == 3, timer_ok < Sys.time() | is.na(timer_ok))

          if (nrow(recup_enq) > 0) "oui" else if (nrow(recup_ini) == 0) "non"
          else pull(recup_ini[nrow(recup_ini),"resultat"])
        } else "non"
      })
      outputOptions(output, "pv_copie_classement", suspendWhenHidden = FALSE)

      output$pv_modif_classement <- renderText({

        if (values$classement == "oui"){
          recup_ini <- values$actions %>%
            filter(user == USER$nom_user,action == "pouvoir",
                   cible == "modif_classement",
                   timer_ok < Sys.time() | is.na(timer_ok))

          if (nrow(recup_ini) == 0) "non"
          else pull(recup_ini[nrow(recup_ini),"resultat"])
        } else "non"
      })
      outputOptions(output, "pv_modif_classement", suspendWhenHidden = FALSE)

    }
  })

  #### Informations ####

  # Info timer

  output$currentTime <- renderText({
    invalidateLater(1000, session)
    Sys.setenv(TZ = "Europe/Paris")
    format(Sys.time(), "%H:%M:%S")
  })


  #### Module Enquête basique ####

  # Choix de l'enquête

  observe({
    if (USER$logged == TRUE) {
      option_indices <- liste_indices(values$actions,USER$nom_user)
      updateSelectInput(session, "choix_enquete",choices = option_indices)
    }
  })

  # PA et graphique de réussite
  observe({
    if (USER$logged == TRUE) {

      # Nombre de PA
      updateNumericInput(session, "PA",max = USER$PA_user)

      # Tableau de probabilité
      bonus_recherche <- input$PA-1
      if (!is.numeric(input$PA)) bonus_recherche <- 0

      # Création de la BDD dés
      valeur_alea <- creation_bdd_des(values$actions,bonus_recherche)

      output$table_proba <- renderTable({
        valeur_alea %>% select(label,pc) %>%
          arrange(-pc) %>% mutate(pc = paste0(round(pc*100,1), "%"))
      },colnames = FALSE)

      output$plot_proba <- renderPlot({plot_bdd_des(valeur_alea)})
    }
  })

  # Lancement de l'enquête
  observeEvent(input$boutton_enquete, {
    if (!is.null(input$choix_enquete)){
      if (input$choix_enquete != ""){
        if (!is.numeric(input$PA) | input$PA <= 0){
          message_output <- "Petit futé.... Mais c'est râté !"
        }else if (USER$PA_user < input$PA){
          message_output <- message_PA_insuffisant
        }else{
          out <- recherche_indice(values$actions,USER$nom_user,input$choix_enquete,input$PA)
          values$actions <- values$actions %>% add_row(out$new_rows)
          show_message("Actions",out$message)

          message_output <- ""
        }
      }
    }else{
      message_output <- "Veuillez sélectionner une enquête."
    }
    info(message_output)
  })

  observeEvent(input$boutton_enquete_help, {
    updateControlbar(id = "controlbar", session = session)
    updateControlbarMenu("controlbarMenu", selected = "Enquête")
  })
  observeEvent(input$boutton_enquete_advanced_help, {
    updateControlbar(id = "controlbar", session = session)
    updateControlbarMenu("controlbarMenu", selected = "Enquête")
  })
  observeEvent(input$boutton_enquete_copie_help, {
    updateControlbar(id = "controlbar", session = session)
    updateControlbarMenu("controlbarMenu", selected = "Copie")
  })
  observeEvent(input$boutton_enquete_interception_help, {
    updateControlbar(id = "controlbar", session = session)
    updateControlbarMenu("controlbarMenu", selected = "Interception")
  })


  #### Module de recherche d'indices avancés ####

  # Fonction pour pondérer les résultats
  alea_pond <- function(mot,base){
    nb_vec <- unlist(lapply(gregexpr(mot,base,ignore.case = TRUE),
                            function(x) if (x[1] != -1) length(x) else 0))
    result <- NULL
    for(i in 1:length(nb_vec)){
      if (nb_vec[i] != 0){
        for (k in 1:nb_vec[i]){
          result <- c(result,i)
        }
      }
    }
    result
  }

  # Recherche d'indice avancée
  observeEvent(input$boutton_enquete_advanced, {

    if (input$PA == 0){
      info("Petit futé.... Mais c'est râté !")
    }else if (USER$PA_user < input$PA){
      info(message_PA_insuffisant)
    }else{

      recup1 <- NULL
      recup1b <- NULL
      recup2 <- NULL
      recup2b <- NULL
      recup3 <- NULL
      recup3b <- NULL

      # indices non spéciaux
      indices_basiques <- info_indices(values$actions) %>%
        filter(nchar(indice) == 1 & variation == 2)

      if (input$advanced_1 != ""){
        recup1 <- alea_pond(input$advanced_1,indices_basiques$texte)
        recup1b <- alea_pond(input$advanced_1,indices_basiques$titre)
      }
      if (input$advanced_2 != ""){
        recup2 <- alea_pond(input$advanced_2,indices_basiques$texte)
        recup2b <- alea_pond(input$advanced_2,indices_basiques$titre)
      }
      if (input$advanced_3 != ""){
        recup3 <- alea_pond(input$advanced_3,indices_basiques$texte)
        recup3b <- alea_pond(input$advanced_3,indices_basiques$titre)
      }

      recup <- pull(indices_basiques[c(recup1,recup2,recup3,recup1b,recup2b,recup3b),'indice'])

      if (length(recup)>0){
        out <- recherche_indice(values$actions,USER$nom_user,sample(recup,1),input$PA)
      }else{
        # Résultat aléatoire
        enquete_alea <- sample(pull(indices_basiques[,'indice']),1)
        out <- recherche_indice(values$actions,USER$nom_user,enquete_alea,input$PA,1)
      }
      values$actions <- values$actions %>% add_row(out$new_rows)
      show_message("Actions",out$message)
    }
  })

  #### Module de Copie d'un indice ####

  observeEvent(input$boutton_enquete_copie, {
    if (USER$PA_user < 2){
      info(message_PA_insuffisant)
    }else{
      out <- copie_indice(values$actions,USER$nom_user,input$choix_user_copie,2)
      values$actions <- values$actions %>% add_row(out$new_rows)
      show_message("Actions",out$message)
    }
  })

  #### Intercepter une enquête ####

  observeEvent(input$boutton_enquete_interception, {
    if (USER$PA_user < 2){
      info(message_PA_insuffisant)
    }else{
      out <- interception_indice(values$actions,USER$nom_user,
                                 input$choix_user_copie,2)
      values$actions <- values$actions %>% add_row(out$new_rows)
      show_message("Actions",out$message)
    }
  })

  #### Module lecture des indices ####

  observe({
    if (USER$logged == TRUE) {

      # Vérification de l'activation des enquêtes spéciales
      nb_car_enq <- 1
      enq_spe <- values$actions %>% filter(action == "enq_spe") %>% pull(resultat)

      if (length(enq_spe) == 0) enq_spe <- "Non"
      if (enq_spe[length(enq_spe)] == "Oui") nb_car_enq <- 2

      # Création du tableau
      tableau_indices <- values$actions %>%
        filter(user == USER$nom_user,
               action == "enquete" | action=="Enquete interceptee",
               timer_ok < Sys.time(),nchar(cible) <= nb_car_enq)

      tableau_indices <- tableau_indices %>%
        rename(indice=cible) %>%
        left_join(info_indices(values$actions) %>%
                    filter(variation == 3) %>%
                    select(titre,indice)) %>%
        mutate(texte = if_else(action == "Enquete interceptee","Malheureusement, une équipe surnaturelle (nous ne savons dire s'il s'agissait de démons ou d'anges) nous est tombée dessus, et nous a intercepté le rapport de l'enquête avant que nous puissons vous le transmettre. Nous vous promettons que cela n'arrivera plus jamais.",texte),
               resultat = case_when(
                 resultat == 1 ~ "Ratée",
                 resultat == 2 ~ "Réussie",
                 resultat == 3 ~ "Parfaite",
                 TRUE ~ resultat
               )
        )

      tableau_classement <- values$actions %>%
        filter(user == USER$nom_user,
               action == "enquete",
               timer_ok < Sys.time(),
               cible == "classement") %>%
        rename(indice=cible) %>%
        mutate(titre = "Classement actuel des entités",
               texte=as.character(resultat),resultat="")

      tableau_indices <- tableau_indices %>% add_row(tableau_classement)

      if (nrow(tableau_indices) > 0 ){

        tableau_indices <- tableau_indices %>%
          arrange(desc(timer_ok)) %>%
          select(titre,timer_ok,resultat,texte) %>%
          mutate(timer_ok = format(prepare_timer(timer_ok),format="%H:%M:%S"),
                 texte = str_replace_all(texte,"\n","<br/>"))

        colnames(tableau_indices) <- c("Enquête","Heure d'arrivée","Résultat","Rapport d'enquête")

        output$tableau_indices = DT::renderDataTable(
          tableau_indices, rownames = FALSE,escape=FALSE,
          options = list(ordering = FALSE,info = FALSE))
      }
    }
  })

  #### Module de classement des entités ####

  # Classement des entités
  observeEvent(input$boutton_copie_classement, {
    if (USER$PA_user < 2){
      info(message_PA_insuffisant)
    }else{
      out <- copie_classement(values$actions,USER$nom_user,"",2)
      values$actions <- values$actions %>% add_row(out$new_rows)
      show_message("Classement",out$message)
    }
  })

  # Modification du classement des entités
  observeEvent(input$boutton_modif_classement, {
    if (USER$PA_user < 3){
      info(message_PA_insuffisant)
    }else{
      out <- modification_classement(values$actions,USER$nom_user,
                                     input$choix_user_classement,3)
      values$actions <- values$actions %>% add_row(out$new_rows)
      show_message("Classement",out$message)
    }
  })

  #### Module Chat ####

  observe({
    if (USER$logged == TRUE) {
      tableau_chat <- values$actions %>%
        filter(cible == USER$nom_user,action == "chat",timer_ok < Sys.time()) %>%
        select(user,cible,PA,resultat,timer_ok,resultat)

      if (nrow(tableau_chat) > 0)
        tableau_chat[tableau_chat$PA == 1,'user'] <- "Anonyme"

      tableau_chat <- tableau_chat %>%
        arrange(desc(timer_ok)) %>%
        select(user,timer_ok,resultat) %>%
        mutate(timer_ok = format(prepare_timer(timer_ok),format="%H:%M:%S"),
               resultat = str_replace_all(resultat,"\n", "<br/>"))

      colnames(tableau_chat) <- c("Expéditeur","Heure d'envoi","Message")

      if (nrow(tableau_chat)>0){
        output$tableau_chat = DT::renderDataTable(
          tableau_chat, rownames = FALSE,escape=FALSE,options = list(
            paging = FALSE,searching = FALSE,ordering = FALSE,info = FALSE)
        )
      }
    }
  })

  observeEvent(input$boutton_chat, {

    ano <- ifelse(input$ano_chat,1,0)

    if (ano == 1 & USER$PA_user < 1)
      info(message_PA_insuffisant)
    else{
      out <- envoi_message(values$actions,USER$nom_user,input$choix_user_chat,
                           input$message_chat,ano,input$timer_chat*60)
      values$actions <- values$actions %>% add_row(out$new_rows)
      show_message("Chat",out$message)
    }

    updateCheckboxInput(session,"ano_chat",value = FALSE)
  })

  #### Module d'administration de la murder ####

  ##### Suivi de la murder ####

  ###### Chat Admin ######

  output$tableau_chat_admin = DT::renderDataTable({
    tableau_chat <- values$actions %>%
      filter(cible == "Régis",
             action == "chat",
             timer_ok < Sys.time()) %>%
      select(user,cible,PA,resultat,timer_ok,resultat) %>%
      arrange(desc(timer_ok)) %>%
      select(user,timer_ok,resultat) %>%
      mutate(timer_ok = format(as.POSIXct(timer_ok, origin = "1970-01-01",
                                          tz = "Europe/Paris"), format="%H:%M:%S"),
             resultat = str_replace_all(resultat,"\n", "<br/>"))

    colnames(tableau_chat) <- c("Expéditeur","Heure d'envoi","Message")

    tableau_chat
  },rownames = FALSE,escape=FALSE,
  options = list(paging = FALSE,searching = FALSE,
                 ordering = FALSE,info = FALSE))

  observeEvent(input$boutton_chat_admin, {

    pj <- input$choix_user_chat_admin

    if (pj == "Tout le conseil") pj <- info_user(values$actions)$user
    new_rows <- tibble(
      user="Régis",action="chat",cible=pj,PA=0,
      timer=Sys.time(),resultat=input$message_chat_admin,
      timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin("Message envoyé")
  })


  ###### Timer #####

  output$text_timer_admin <- renderText({
    info_timer <- values$actions %>%
      filter(user == "admin",action == "timer") %>%
      filter(row_number() == n()) %>%
      pull(timer)

    if (length(info_timer) == 1){
      info_timer <- difftime(Sys.time(), info_timer,units = "secs")
      paste0(display_duration(info_timer))
    }else{
      "Aucun timer activé"
    }
  })

  output$text_timer_PA_admin <- renderText({
    info_timer <- values$actions %>%
      filter(user == "admin",action == "admin_PA") %>%
      filter(row_number() == n()) %>%
      pull(timer)

    if (length(info_timer) == 1){
      info_timer <- difftime(Sys.time(), info_timer,units = "secs")
      paste0(display_duration(info_timer))
    }else{
      "Aucun PA distribué"
    }
  })

  observeEvent(input$bouton_timer_admin,{

    new_rows <- tibble(
      user="admin",action="timer",cible="",PA=NA,
      timer=Sys.time(),"",timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin("Timer mis à jour")
  })

  ###### Action rapide : + 4 PA ######
  observeEvent(input$bouton_PA_all, {

    new_rows <- tibble(
      user=info_user(values$actions)$user,action="admin_PA",cible="",PA=-4,
      timer=Sys.time(),resultat="",timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin("4 PA envoyé à toustes")
  })

  ##### Module de vérification des actions ####

  output$table_admin <- DT::renderDataTable({

    values$actions %>%
      filter(action != "init") %>% arrange(desc(timer_ok)) %>%
      mutate(timer_ok = format(prepare_timer(timer_ok),format="%H:%M:%S"),
             timer = format(prepare_timer(timer),format="%H:%M:%S"))

  }, rownames = FALSE)

  ##### Module de vérification des indices obtenus ####

  output$table_indices_admin = renderTable({
    if (!is.null(input$choix_enquete_admin)){
      if (input$choix_enquete_admin != "dynamique"){

        # on prend la liste des indices possibles
        liste_indices <- info_indices(values$actions) %>%
          filter(indice==input$choix_enquete_admin) %>%
          select(indice,titre,variation) %>%
          mutate(variation = as.character(variation))

        liste_enquete <- values$actions %>%
          filter(cible == input$choix_enquete_admin,action == "enquete") %>%
          group_by(indice=cible,variation=resultat) %>%
          summarise(user = paste0(unique(user),collapse = ", "))

        liste_indices <- liste_indices %>%
          left_join(liste_enquete) %>%
          mutate(Résultat = case_when(
            variation == "1" ~ "Ratée",
            variation == "2" ~ "Réussie",
            variation == "3" ~ "Parfaite",
            TRUE ~ "erreur")) %>%
          select(Résultat,Personnage = user)

        liste_indices
      }
    }
  })

  ##### Module de vérification des infos PJ ####

  output$table_admin_PJ <- DT::renderDataTable({

    # Nombre de PA de chaque joueur
    info_user_update <- info_user(values$actions) %>%
      filter(!user %in% c("HACK","admin","TEST")) %>%
      rename(PA_ini = PA) %>%
      left_join(values$actions %>%
                  group_by(user) %>%
                  summarise(PA_actu = sum(PA,na.rm=TRUE))) %>%
      mutate(PA_actu = PA_ini - PA_actu) %>%
      arrange(user) %>%
      select(Personnage = user,`Mot de passe` = password,`PA initiaux` = PA_ini,
             `PA actuels` = PA_actu)

    liste_reussite <- values$actions %>%
      filter(action == "enquete") %>%
      group_by(user,resultat) %>%
      summarise(nb_enquete = n()) %>%
      group_by(user) %>%
      mutate(pc_enquete = round(100*nb_enquete/sum(nb_enquete,na.rm=TRUE)),
             nb_enquete = paste0(nb_enquete," (",pc_enquete,"%)"),
             resultat = case_when(
               resultat == "1" ~ "Ratée",
               resultat == "2" ~ "Réussie",
               resultat == "3" ~ "Parfaite",
               TRUE ~ "erreur")) %>%
      ungroup() %>%
      select(Personnage=user,resultat,nb_enquete) %>%
      pivot_wider(names_from = resultat,values_from = nb_enquete)

    info_user_update <- info_user_update %>%
      left_join(liste_reussite)

    if (values$classement == "oui"){
      liste_classement <- bdd_classement(values$actions) %>%
        rename(Personnage = cible,Classement=resultat)
      info_user_update <- info_user_update %>% left_join(liste_classement)
    }

    datatable(info_user_update,
              options = list(pageLength = 100),
              rownames = FALSE)

  })

  observeEvent(input$boutton_admin_PA, {

    info_user <- info_user(values$actions)
    timer_now <- Sys.time()

    pj <- input$choix_user_PA
    if (pj == "All") pj <- info_user(values$actions)$user

    new_rows <- tibble(
      user=pj,action="admin_PA",cible="",PA=-input$PA_admin,
      timer=Sys.time(),resultat="",timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    message_output <- paste(input$PA_admin," PA attribué à ",
                            input$choix_user_PA,sep="")

    info_admin(message_output)
  })

  observeEvent(input$boutton_admin_password, {

    PA <- info_user(values$actions) %>%
      filter(user == input$choix_user_password) %>% pull(PA)

    new_row <-  prepare_row_drive(values$id_session,"admin","password",
                                  input$choix_user_password,PA,Sys.time(),
                                  input$password_admin,Sys.time())

    new_rows <- tibble(
      user="admin",action="password",cible=input$choix_user_password,PA=PA,
      timer=Sys.time(),resultat=input$password_admin,timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin(paste0("Nouveau mot de passe pour ",input$choix_user_password,
                      " : ",input$password_admin))
  })

  observeEvent(input$boutton_admin_classement, {

    new_rows <- tibble(
      user="admin",action="classement",cible=input$user_classement_admin,PA=0,
      timer=Sys.time(),resultat=input$points_classement_admin,
      timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin(paste("Modification du classement pour",
                     input$user_classement_admin))
  })

  ##### Module de modification des indices ####

  output$table_admin_indices <- renderDT({

    datatable(info_indices(values$actions) %>%
                select(indice,titre,variation,texte)
              , editable = TRUE)
  })

  observeEvent(input$table_admin_indices_cell_edit, {

    temp_indices <- info_indices(values$actions) %>%
      select(indice,titre,variation,texte)

    row  <- input$table_admin_indices_cell_edit$row
    col  <- input$table_admin_PJ_cell_edit$col

    if (col == 4){
      indice <- pull(temp_indices[row,1])
      titre <- pull(temp_indices[row,2])
      variation <- pull(temp_indices[row,3])
      texte <- input$table_admin_indices_cell_edit$value

      new_rows <- tibble(
        user="admin",action="init",cible=indice,PA=variation,
        timer=Sys.time(),resultat=titre,
        timer_ok=Sys.time(),texte=texte)

      values$actions <- values$actions %>% add_row(new_rows)

      info_admin("Modification des indices")
    }
  })

  ##### Module d'ajout des indices ####

  option_variations <- c("Enquête ratée" = 1,
                         "Enquête réussie" = 2,
                         "Enquête parfaite" = 3)
  updateSelectInput(session,"choix_variation",choices = option_variations)

  observe({
    if (USER$logged == TRUE) {
      if (USER$nom_user == "admin") {

        liste_indices <- info_indices(values$actions) %>%
          filter(variation == 3) %>%
          select(titre,texte,variation,indice)

        option_indices <- list()
        for (i in liste_indices$indice){
          titre <- liste_indices %>% filter(indice==i) %>%
            mutate(lettre_titre = paste0(i," : ",titre)) %>%
            pull(lettre_titre)
          option_indices[[titre]] <- i
        }

        updateSelectInput(session, "choix_enquete_admin",
                          choices = option_indices,selected = NULL
        )

      }}
  })

  observe({
    if (USER$logged == TRUE) {
      if (!is.null(input$choix_enquete_admin)){

        ligne_indice <- info_indices(values$actions) %>%
          filter(indice == input$choix_enquete_admin)

        lettre <- ligne_indice %>% filter(variation == 1) %>% pull(indice)
        if (nrow(ligne_indice) == 0) lettre = ""
        if (str_length(lettre) == 2){
          info <- paste0("Il s'agit d'une enquête spéciale, que les joueur·euse·s peuvent avoir s'iels obtiennent une enquête partfaite sur l'enquête ",substr(lettre,1,1),". Vous pouvez envoyer cette indice à n'importe qui, mais sachez que ces enquêtes spéciales ne sont accessibles qu'à partir du moment où l'action spéciale 'enquêtes spéciales' est activée.")
        }else{
          info <- ""
        }
        output$admin_info_lettre <- renderText({info})

        output$admin_indice_1 <- renderText({ligne_indice %>%
            filter(variation == 1) %>% pull(texte)})
        output$admin_indice_2 <- renderText({ligne_indice %>%
            filter(variation == 2) %>% pull(texte)})
        output$admin_indice_3 <- renderText({ligne_indice %>%
            filter(variation == 3) %>% pull(texte)})

      }else{
        output$admin_indice_lettre <- renderText(NULL)
        output$admin_indice_1 <-  renderText(NULL)
        output$admin_indice_2 <-  renderText(NULL)
        output$admin_indice_3 <-  renderText(NULL)
      }
    }
  })

  observeEvent(input$boutton_admin_indice, {

    if (!is.null(input$choix_enquete_admin)){
      out <- recherche_indice(values$actions,
                              input$choix_user_indice,
                              input$choix_enquete_admin,
                              0,input$choix_variation,0)
      values$actions <- values$actions %>% add_row(out$new_rows)

      info_admin(paste0(input$choix_enquete_admin," attribué à ",
                        input$choix_user_indice,sep=""))
    }
  })

  ##### Module de modification du dé ####

  observe({
    if (USER$logged == TRUE) {
      if (USER$nom_user == "admin") {

        # Création de la BDD dés
        valeur_alea <- creation_bdd_des(values$actions)

        nb_1 <- valeur_alea %>% filter(resultat == 1) %>% pull(valeur)
        nb_2 <- valeur_alea %>% filter(resultat == 2) %>% pull(valeur)
        nb_3 <- valeur_alea %>% filter(resultat == 3) %>% pull(valeur)

        updateNumericInput(session, "Nb_echec",value = nb_1)
        updateNumericInput(session, "Nb_reussite",value = nb_2)
        updateNumericInput(session, "Nb_reussite_parfaite",value = nb_3)
      }}
  })

  output$plot_proba_admin <- renderPlot({

    valeur_alea <- creation_bdd_des(values$actions) %>%
      mutate(valeur = case_when(resultat == 1 ~ input$Nb_echec,
                                resultat == 2 ~ input$Nb_reussite,
                                resultat == 3 ~ input$Nb_reussite_parfaite,
                                TRUE ~ 99)) %>%
      mutate(pc = valeur / sum(valeur))

    plot_bdd_des(valeur_alea)
  })

  observeEvent(input$boutton_admin_des, {

    timer_ok <- Sys.time()

    new_rows <- tibble(
      user="admin",action="alea",cible=1,PA=input$Nb_echec,
      timer=Sys.time(),resultat="",timer_ok=Sys.time(),texte="") %>%
      add_row(
        user="admin",action="alea",cible=2,PA=input$Nb_reussite,
        timer=Sys.time(),resultat="",timer_ok=Sys.time(),texte="") %>%
      add_row(
        user="admin",action="alea",cible=3,PA=input$Nb_reussite_parfaite,
        timer=Sys.time(),resultat="",timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin("Modification du dé")
  })

  ##### Module des actions spéciales ####

  observeEvent(input$enquete_speciale_on, {

    new_rows <- tibble(
      user="admin",action="enq_spe",cible=NA,PA=0,
      timer=Sys.time(),resultat="Oui",timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin("Enquête spéciale activée")
  })
  observeEvent(input$enquete_speciale_off, {

    new_rows <- tibble(
      user="admin",action="enq_spe",cible=NA,PA=0,
      timer=Sys.time(),resultat="Non",timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin("Enquête spéciale désactivée")
  })

  observeEvent(input$enquete_chat_gpt_on, {

    new_rows <- tibble(
      user="admin",action="chat_gpt",cible=NA,PA=0,
      timer=Sys.time(),resultat="Oui",timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin("Modification par Chat GPT activée")
  })
  observeEvent(input$enquete_chat_gpt_off, {
    new_rows <- tibble(
      user="admin",action="chat_gpt",cible=NA,PA=0,
      timer=Sys.time(),resultat="Non",timer_ok=Sys.time(),texte="")

    values$actions <- values$actions %>% add_row(new_rows)

    info_admin("Modification par Chat GPT désactivée")
  })

  output$admin_table_autre_actions <- renderTable({

    fl_chat_gpt <- values$actions %>% filter(action == "chat_gpt") %>% pull(resultat)
    fl_enq_spe <- values$actions %>% filter(action == "enq_spe") %>% pull(resultat)

    fl_chat_gpt <- ifelse(fl_chat_gpt[length(fl_chat_gpt)] == "Oui","Oui","Non")
    fl_enq_spe <-  ifelse(fl_enq_spe[length(fl_enq_spe)] == "Oui","Oui","Non")

    table_autre_actions <- tibble(Actions = character(),Statut = character()) %>%
      add_row(Actions="Enquête spéciale",Statut=fl_enq_spe) %>%
      add_row(Actions="Réécriture par ChatGPT",Statut=fl_chat_gpt)

    table_autre_actions
  })

})
