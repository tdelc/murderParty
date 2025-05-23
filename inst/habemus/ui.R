habemus_ui <- function() {
  # Ensure necessary libraries are available. Ideally, these are package imports.
  # library(shiny)
  # library(DT)
  # library(shinydashboard)
  # library(shinydashboardPlus)

  # It's better if options are set by the main package or globally,
  # but for now, keeping it here for encapsulation during sourcing.
  # options(DT.options = list(
  #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
  # ))

  # Titre de Mazette
  header <- function(){
    shinydashboard::dashboardHeader(title = "") }

  sidebar <- function(){
    shinydashboard::dashboardSidebar(
      collapsed=TRUE,
      shinydashboard::sidebarMenu(
        id = "sidebar",
        shinydashboard::menuItem("Login", tabName = "connexion", icon = shiny::icon("arrow-right-to-bracket")),
        shinydashboard::menuItem("User", tabName = "enquete", icon = shiny::icon("magnifying-glass")),
        shinydashboard::menuItem("Admin", tabName = "admin", icon = shiny::icon("lock"))
      )
    )
  }

  body <- function(){
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        tabItem_connexion,
        tabItem_enquete,
        tabItem_admin
      )
    )
  }

  tabItem_connexion <- shinydashboard::tabItem(tabName = "connexion",
                               shiny::conditionalPanel(
                                 condition = "output.logged == 'not_ok'",
                               shiny::fluidRow(
                                 shinydashboard::box(width = 6, title = "Connexion",
                                     status = "primary", solidHeader = TRUE,
                                     shiny::div(shiny::h5(shiny::textOutput("info_log")), style = "color:red"),
                                     shiny::br(),
                                     shiny::textInput("username", "Identifiant", ""),
                                     shiny::passwordInput("password", "Mot de passe", ""),
                                     shiny::actionButton("boutton_log", "Go!")
                                 )
                               )
                             ),
                             shiny::conditionalPanel(
                               condition = "output.logged == 'not_connected'",
                               shiny::fluidRow(
                                 shinydashboard::box(width = 6, title = "Interface informatique pour Habemus Papam",
                                     status = "primary", solidHeader = TRUE,
                                     shiny::p("Bienvenue dans l'interface informatique permettant de gérer les indices durant la murder party Habemu Papam"),
                                     shiny::p("Comme vous avez pu le lire dans les règles de jeu, les personnages ont la possibilité d’obtenir des indices via des enquêtes. Vous pouvez gérer ces actions vous mêmes (auquel cas vous devez imprimer les indices en plusieurs exemplaires ainsi que des points d’indices, et réaliser les actions) mais cela prend beaucoup de temps (nous vous conseillons dans ce cas d’être deux)."),
                                     shiny::p("Vous pouvez également vous munir d’ordinateurs gérant cette tâche à votre place. Deux ou trois ordinateurs sont nécessaires dans ce cas, répartis sur l’ensemble de l’espace de jeu. Vous n’avez rien à installer pour les ordinateurs devant gérer les indices, toutes les actions sont réalisées avec cette interface web."),
                                     shiny::p("En vous rendant sur l'url https://tdelc.shinyapps.io/habemus_new/?new_server , Vous allez pouvoir créer le serveur qui servira à stocker les actions des joueurs durant le jeu. Choisissez un nom sans espace ni caractères spéciaux. Vous cliquez ensuite sur “Créer Serveur” et l’interface vous indiquera si le serveur a été créé, ou si vous avez choisi un nom qui existe déjà."),
                                     shiny::p("Une fois le serveur créé, vous pouvez lancer cette url sur tous les ordinateurs de la session : https://tdelc.shinyapps.io/habemus_new/?server=XXX en remplaçant le terme XXX par le nom que vous aurez choisi pour votre serveur."),
                                     shiny::p("Cette url donne accès aux joueurs à une interface complète permettant d’envoyer des enquêteurs sur les indices disponibles dans leur feuille de personnage (et d’ajouter des points de pouvoir pour améliorer leur chance de réussite parfaite), de copier ou d’intercepter des enquêtes réalisées par les autres ou de s’envoyer des messages entre eux ou elles. Toutes ces actions sont expliquées directement dans l’interface. Pensez quand même à faire un tutoriel aux joueur.euse.s avant le début du jeu et/ou à les accompagner lors de leur première action.")

                                 )
                               )
                             )
)

tabItem_enquete <- shinydashboard::tabItem(tabName = "enquete",
                           shiny::conditionalPanel(
                             condition = "output.logged == 'ok'",
                             shiny::fluidRow(
                               shinydashboard::box(width = 3, title = "Informations",
                                   status = "info", solidHeader = TRUE,
                                   shiny::div(shiny::h4(shiny::textOutput("info")), style = "color:red"),
                                   shiny::h3(shiny::textOutput("currentTime")),
                                   shiny::h3(shiny::textOutput("user_name")),
                                   shiny::h4(shiny::textOutput("user_PA")),
                                   shiny::helpText("(Vous regagnerez quatre points de pouvoir par heure)"),
                                   shiny::h4(shiny::HTML("<A HREF=\"javascript:history.go(0)\">Quitter l'interface</A>"))
                               ),
                               shinydashboard::box(width = 3, title = "Lancer une enquête",
                                   status = "primary", solidHeader = TRUE,
                                   shiny::tabsetPanel(
                                     shiny::tabPanel("Recherche classique",
                                              shiny::selectInput("choix_enquete", "Choisir l'enquête:",c("dynamique"),selectize=FALSE,size=10,width='100%'),
                                              shiny::actionButton("boutton_enquete", "Lancer la recherche"),
                                              shiny::actionButton("boutton_enquete_help",label="",icon = shiny::icon("circle-question"),style="color: #000; background-color: #fff; border-color: #fff")
                                     ),
                                     shiny::tabPanel("Recherche avancée",
                                              shiny::helpText("(n'oubliez pas les accents)"),
                                              shiny::textInput("advanced_1", "Premier mot-clef"),
                                              shiny::textInput("advanced_2", "Deuxième mot-clef"),
                                              shiny::textInput("advanced_3", "Troisième mot-clef"),
                                              shiny::actionButton("boutton_enquete_advanced", "Lancer la recherche"),
                                              shiny::actionButton("boutton_enquete_advanced_help",label="",icon = shiny::icon("circle-question"),style="color: #000; background-color: #fff; border-color: #fff")
                                     )
                                   ),
                                   shiny::br(),
                                   shiny::column(width=5,
                                          shiny::numericInput("PA", "Points de pouvoir :", 1, min = 1, max = 10,step=1)),
                                   shiny::column(width=7,
                                          shiny::h5("Probabilité de réussite"),
                                          shiny::tags$head(shiny::tags$style(type="text/css", "#table_proba table td {line-height:50%;}")),
                                          shiny::tableOutput("table_proba"))
                                   # shiny::plotOutput("plot_proba",width="100%")
                               ),
                               shinydashboard::box(width = 3, title = "Copier ou Intercepter une enquête",
                                   status = "primary", solidHeader = TRUE,
                                   shiny::selectInput("choix_user_copie", "Choisir la cible",
                                               c("dynamique"),selectize=FALSE,size=10),
                                   shiny::actionButton("boutton_enquete_copie", "Récupérer une copie de l'enquête"),
                                   shiny::actionButton("boutton_enquete_copie_help",label="",icon = shiny::icon("circle-question"),style="color: #000; background-color: #fff; border-color: #fff"),shiny::br(),
                                   shiny::actionButton("boutton_enquete_interception", "Intercepter l'enquête"),
                                   shiny::actionButton("boutton_enquete_interception_help",label="",icon = shiny::icon("circle-question"),style="color: #000; background-color: #fff; border-color: #fff"),
                                   shiny::helpText("(coûte 2 points de pouvoir)")
                               ),
                               shinydashboard::box(width = 3, title = "Envoyer un message privé",
                                   status = "success", solidHeader = TRUE,

                                   shiny::selectInput("choix_user_chat", "Choisir la cible",
                                               c("dynamique"),selectize=FALSE,size=12),
                                   shiny::textAreaInput("message_chat","Message :",width='100%'),
                                   shiny::numericInput("timer_chat", "Délai d'attente (en minute) pour envoyer le message", 1, min = 0, max = 15),
                                   shiny::column(7,shiny::checkboxInput("ano_chat", "Message anonyme ? (1 PA)")),
                                   shiny::column(5,shiny::actionButton("boutton_chat", "Envoyer le message"))
                               )),
                             shiny::fluidRow(
                               shinydashboard::box(width = 9, title = "Liste des indices obtenus",
                                   status = "primary", solidHeader = TRUE,
                                   DT::dataTableOutput("tableau_indices")),
                               shinydashboard::box(width = 3, title = "Liste des messages obtenus",
                                   status = "success", solidHeader = TRUE,
                                   DT::dataTableOutput("tableau_chat"))

                             ),
                             shiny::fluidRow(
                               shiny::conditionalPanel(
                                 condition = "output.pv_copie_classement == 'oui'",
                                 shinydashboard::box(width = 6, title = "Obtenir une copie du classement",
                                     status = "warning", solidHeader = TRUE,
                                     shiny::p("Vous pouvez obtenir une copie du classement actuel des entités. Attention, il s'agit d'une copie à cet instant et le classement peut évoluer ensuite."),
                                     shiny::p("La copie sera disponible dans les 10 minutes et vous coûtera 2 points de pouvoir."),
                                     shiny::p("Attention, nous vous prévenons que les services de Andromalius pourraient détecter cette intrusion dans leur système."),
                                     shiny::br(),
                                     shiny::actionButton("boutton_copie_classement", "Obtenir le classement"),
                                     shiny::h4("(coûte 2 points de pouvoir)")
                                 )
                               ),
                               shiny::conditionalPanel(
                                 condition = "output.pv_modif_classement == 'oui'",
                                 shinydashboard::box(width = 6, title = "Modifier le classement",
                                     status = "warning", solidHeader = TRUE,
                                     shiny::p("Vous pouvez modifier le classement actuel des entités pour choisir la personne à mettre en tête. Attention, il s'agit d'une modification à cet instant et le classement peut évoluer ensuite."),
                                     shiny::p("La modification sera immédiate et vous coûtera 3 points de pouvoir."),
                                     shiny::p("Attention, nous vous prévenons que les services de Andromalius détecteront à coup sûr cette intrusion dans leur système. De plus, il y a encore une chance sur 6 qu'ils puissent contrer cette intrusion avant qu'elle puisse être réalisée."),
                                     shiny::br(),
                                     shiny::selectInput("choix_user_classement", "Choisir la cible",
                                                 c("dynamique"),selectize=FALSE,size=10),
                                     shiny::actionButton("boutton_modif_classement", "Modifier le classement"),
                                     shiny::h4("(coûte 3 points de pouvoir)")
                                 )
                               )
                             )
                           )
)

tabItem_admin <- shinydashboard::tabItem(tabName = "admin",
                         shiny::conditionalPanel(
                           condition = "output.logged == 'server'",
                           shiny::textInput("server_name", "Nom du serveur", ""),
                           shiny::actionButton("create_server", "Créer le serveur"),
                           shiny::br(),
                           shiny::textOutput("info_server")
                         ),
                         shiny::conditionalPanel(
                           condition = "output.logged == 'admin'",
                           shiny::fluidRow(
                             shiny::column(width = 2,shiny::h4(shiny::HTML("<A HREF=\"javascript:history.go(0)\">Quitter l'interface</A>"))),
                             shiny::column(width = 7,""),
                             shiny::column(width = 3,shiny::div(shiny::h4(shiny::textOutput("info_admin")), style = "color:red"))
                           ),
                           shiny::tabsetPanel(
                             shiny::tabPanel("Accueil",
                                      shiny::fluidRow(
                                        shinydashboard::box(width = 4, title = "Messages privés avec les PJ",
                                            status = "primary", solidHeader = TRUE,

                                            shiny::selectInput("choix_user_chat_admin", "Choisir la cible",c("dynamique"),selectize=FALSE,size=12),
                                            shiny::textAreaInput("message_chat_admin","Message :",width='100%'),
                                            shiny::actionButton("boutton_chat_admin", "Envoyer le message"),
                                            DT::dataTableOutput("tableau_chat_admin")

                                        ),
                                        shinydashboard::box(width = 4, title = "Votre rôle de MJ",
                                            status = "primary", solidHeader = TRUE,
                                            shiny::p("Bienvenue dans l'interface de gestion de la murder. Vous avez ici de quoi influencer toute la murder concernant les indices, les classements, les points d'indices. Normalement, tout peut être consulté et modifié à l'aide de cette interface. Vous devez lire le document d'organisatin pour comprendre tout ce que vous devrez faire."),
                                            shiny::p("Globalement, durant la murder, vous devez ajouter des points d'action toutes les heures afin de permettre à vos PJ d'enquêter. Vous pouvez tout à fait accélérer ou ralentir ce rythme selon votre envie, pensez juste à ne pas trop laisser attendre vos PJ sans rien avoir à faire."),
                                            shiny::p("Vous avez accès à plusieurs onglets, chacun permettant un impact sur la murder :"),
                                            shiny::tags$ol(
                                              shiny::tags$li("Infos PJ : Cet onglet permet de vérifier et/ou modifier les mots de passe des PJ. C'est également ici que vous devrez ajouter les points de pouvoirs. Il est possible d'ajouter des points de pouvoir à l'ensemble des PJ ou à un.e joueur.euse en particulier. Il est enfin possible d'observer et modifier le classement des entités ;"),
                                              shiny::tags$li("Vérifier et ajouter indices : Cet onglet permet de lire durant la murder les différents indices, de savoir si les PJ les ont obtenu, et éventuellement d'en ajouter un à l'un·e de vos PJ si cela est pertinent ;"),
                                              shiny::tags$li("Actions spéciales : Cet onglet permet de modifier les probabilités de réussites des enquêtes. Cet onglet permet également d'activer les enquêtes spéciales (les enquêtes les plus complexes, dépendantes d’une autre enquête, et qui vont sans aucun doute provoquer de grandes révélations en fin de partie) et la transformation des enquêtes par chatGPT (avant de personnaliser un peu les réponses de chaque équipe d'enquête)"),
                                              shiny::tags$li("Modifier Indices : Cet onglet permet de modifier tous les indices de la murder. Soit en le modifiant manuellement dans le tableau, soit en téléchargeant le fichier complet, en le modifiant, puis en l'uploadant. Attention, ces modifications doivent être réalisées avant la murder (du moins avant qu'une enquête soit réalisée) ;"),
                                              shiny::tags$li("Vérifier actions : L'onglet le plus 'expert', il permet d'observer toutes les actions réalisées par les PJ, en ordre décroissant avoir d'avoir toujours une vue sur les derniers ;")
                                            )
                                        ),
                                        shinydashboard::box(width = 4, title = "Déroulement de votre murder",
                                            status = "primary", solidHeader = TRUE,
                                            shiny::h4("Durée de la murder"),
                                            shiny::h5(shiny::textOutput("text_timer_admin")),
                                            shiny::actionButton("bouton_timer_admin","Lancer le timer de la murder"),
                                            shiny::br(),shiny::br(),
                                            shiny::h4("Durée depuis les derniers points d'indices"),
                                            shiny::h5(shiny::textOutput("text_timer_PA_admin")),
                                            shiny::actionButton("bouton_PA_all","Envoyer tout de suite 4 PA à tous les PJ")
                                        )
                                      )
                             ),
                             shiny::tabPanel("Infos PJ",
                                      shiny::fluidRow(
                                        shinydashboard::box(width = 8, title = "Information sur les personnages, les statistiques d'enquête et le classement des entités",
                                            status = "primary", solidHeader = TRUE,
                                            DT::dataTableOutput("table_admin_PJ")
                                        ),
                                        shiny::column(4,
                                               shinydashboard::box(width = 12, title = "Modification du mot de passe",
                                                   status = "primary", solidHeader = TRUE,
                                                   shiny::column(6,
                                                          shiny::selectInput("choix_user_password", "Choisir le personnage",c("dynamique"))),
                                                   shiny::column(6,
                                                          shiny::textInput("password_admin", "Nouveau mot de passe")),
                                                   shiny::actionButton("boutton_admin_password", "Changer mot de passe")
                                               ),
                                               shinydashboard::box(width = 12, title = "Ajouter des points d'action",
                                                   status = "primary", solidHeader = TRUE,
                                                   shiny::column(6,
                                                          shiny::selectInput("choix_user_PA", "Choisir le personnage",
                                                                      c("dynamique")
                                                          )),
                                                   shiny::column(6,
                                                          shiny::numericInput("PA_admin", "Nombre de PA à ajouter", 1, min = -5, max = 5)),
                                                   shiny::actionButton("boutton_admin_PA", "Ajouter/Retirer les PA")),
                                               shiny::conditionalPanel(
                                                 condition = "output.pv_classement == 'oui'",
                                                 shinydashboard::box(width = 12, title = "Modifier le classement des entités",
                                                     status = "primary", solidHeader = TRUE,
                                                     shiny::column(6,shiny::selectInput("user_classement_admin", "Choisir le personnage",c("dynamique"))),
                                                     shiny::column(6,shiny::numericInput("points_classement_admin", "Nombre de points à ajouter", 1, min = -10000, max = 10000)),
                                                     shiny::actionButton("boutton_admin_classement", "Ajouter/Retirer les points")
                                                 )
                                               )
                                        )
                                      )
                             ),
                             shiny::tabPanel("Vérifier et Ajouter Indices",
                                      shiny::fluidRow(
                                        shinydashboard::box(width = 9, title = "Infos sur les enquêtes",
                                            status = "primary", solidHeader = TRUE,
                                            shiny::column(3,
                                                   shiny::selectInput("choix_enquete_admin", "Choisir l'enquête:",
                                                               c("dynamique"),selectize=FALSE,size=28,width='100%')),
                                            shiny::column(9,
                                                   shiny::div(shiny::h5(shiny::textOutput("admin_info_lettre")), style = "color:red"),
                                                   shiny::h4("Enquête râtée"),
                                                   shiny::textOutput("admin_indice_1"),
                                                   shiny::h4("Enquête réussie"),
                                                   shiny::textOutput("admin_indice_2"),
                                                   shiny::h4("Enquête parfaite"),
                                                   shiny::textOutput("admin_indice_3")
                                            )
                                        ),
                                        shiny::column(3,
                                               shinydashboard::box(width = 12, title = "Qui a eu cette enquête ?",
                                                   status = "primary", solidHeader = TRUE,
                                                   shiny::tableOutput("table_indices_admin")
                                               ),
                                               shinydashboard::box(width = 12, title = "Ajouter cette enquête",
                                                   status = "primary", solidHeader = TRUE,
                                                   shiny::selectInput("choix_user_indice", "Choisir le personnage",
                                                               c("dynamique")),
                                                   shiny::selectInput("choix_variation", "Choisir le résultat:",
                                                               c("dynamique")),
                                                   shiny::actionButton("boutton_admin_indice", "Ajouter l'indice")
                                               )
                                        )
                                      )
                             ),
                             shiny::tabPanel("Actions spéciales",
                                      shiny::fluidRow(
                                        shinydashboard::box(width = 6, title = "Modifier le dé pour les enquêtes",
                                            status = "primary", solidHeader = TRUE,
                                            shiny::column(3,
                                                   shiny::numericInput("Nb_echec", "Nombre d'échec:", 1, min = 0, max = 10)),
                                            shiny::column(3,
                                                   shiny::numericInput("Nb_reussite", "Nombre de réussite:", 4, min = 0, max = 10)),
                                            shiny::column(3,
                                                   shiny::numericInput("Nb_reussite_parfaite", "Nombre de réussite parfaite:", 1, min = 0, max = 10)),
                                            shiny::column(3,
                                                   shiny::actionButton("boutton_admin_des", "Changer les dés")),
                                            shiny::plotOutput("plot_proba_admin",width="100%")
                                        ),
                                        shinydashboard::box(width = 6, title = "Autres actions",
                                            status = "primary", solidHeader = TRUE,
                                            shiny::h3("Enquêtes spéciales"),
                                            shiny::p("Les enquêtes spéciales sont à celles que les joueur·euse·s peuvent avoir dans leur liste s'iels obtiennent un résultat parfait sur une autre enquête. Cependant ces enquêtes contiennent beaucoup de révélations, et il est préférable qu'elles ne soient disponibles qu'en fin de soirée. Vous devez donc cliquer pour activer les enquêtes spéciales au moment où vous pensez que cela est nécessaire. A partir de là, les résultats de ces enquêtes sont accessibles."),
                                            shiny::actionButton("enquete_speciale_on", "Activer les enquêtes spéciales ?"),
                                            shiny::actionButton("enquete_speciale_off", "Desactiver les enquêtes spéciales ?"),
                                            shiny::br(),
                                            shiny::h3("Modification des indices par ChatGPT"),
                                            shiny::p("Afin que toustes n'aient pas les mêmes indices, vous pouvez demander une reformulation des indices avec l'aide automatique de ChatGPT. Une fois activée, tous les indices sont envoyés à ChatGPT avec comme instruction de modifier légèrement l'écriture tout en conservant les informations principales. Cela semble efficace, mais une modification sensible d'un indice reste possible. A vos risques et périls."),

                                            shiny::actionButton("enquete_chat_gpt_on", "Activer les enquêtes par Chat GPT ?"),
                                            shiny::actionButton("enquete_chat_gpt_off", "Desactiver les enquêtes par Chat GPT ?"),
                                            shiny::textOutput("admin_enquete_speciale"),
                                            shiny::br(),
                                            shiny::tableOutput("admin_table_autre_actions")
                                        )
                                      )
                                      # shiny::br(),
                                      # shiny::actionButton("boutton_admin_reset", "Remettre a réro la BDD"),
                                      # shiny::actionButton("boutton_admin_reset_chat", "Remettre a réro le chat"),
                                      # shiny::actionButton("boutton_admin_reset_normale", "Remettre a réro les enquêtes normales"),
                                      # shiny::actionButton("boutton_admin_reset_speciale", "Remettre a réro les actions spéciales")
                             ),
                             shiny::tabPanel("Modifier Indices",
                                      shiny::h2("Changer tous les indices via Excel"),
                                      shiny::p("Vous pouvez télécharger la liste des indices, la modifier, puis l'uploader pour remplacer la liste actuelle. Attention à garder les mêmes lettres d'indices, 3 résultats par lettre (enquête ratée, réussie ou parfaite)."),
                                      shiny::downloadButton('downloadIndices', 'Télécharger les indices actuels'),
                                      shiny::fileInput('loadIndices', 'Charger de nouveaux indices',accept='.csv'),
                                      shiny::br(),
                                      shiny::helpText("(Vous pouvez changer les textes en direct)"),
                                      DT::dataTableOutput("table_admin_indices")
                             ),
                             shiny::tabPanel("Vérifier actions",
                                      shiny::fluidRow(
                                        shinydashboard::box(width = 12, title = "Liste des toutes les actions",
                                            status = "primary", solidHeader = TRUE,
                                            DT::dataTableOutput("table_admin")
                                        ))
                             )
                           )
                         )
)


controlbar <- function(){
  shinydashboardPlus::dashboardControlbar( # Assuming this comes from shinydashboardPlus
    id = "controlbar",
    skin = "dark",width = 250,
    shinydashboardPlus::controlbarMenu( # Assuming this comes from shinydashboardPlus
      id = "controlbarMenu",
      shinydashboardPlus::controlbarItem( # Assuming this comes from shinydashboardPlus
        "Enquête",
        shiny::p("Vous pouvez nous envoyer enquêter pour obtenir plus d'informations sur un sujet précis. Cela vous coûtera de base 1 point de pouvoir. Pour chaque point supplémentaire, vous augmenterez nos chances de faire une enquête parfaite."),
        shiny::p("Vous pouvez enquêter de deux manières :"),
        shiny::p("1- Recherche Classique : Vous utilisez les ragots que vous avez."),
        shiny::p("2- Recherche Avancée : Vous nous donnez un ou plusieurs mots-clefs concernant des sujets que vous avez entendu durant la soirée. Attention, si vous nous envoyez vers une piste illogique, le résultat le sera également."),
        shiny::helpText("(L'enquête prendra entre 2 et 10 minutes)")
      ),
      shinydashboardPlus::controlbarItem( # Assuming this comes from shinydashboardPlus
        "Copie",
        shiny::p("Selon vos ordres, nous pouvons aller récupérer une copie du rapport d'enquête obtenu par un Archange ou Prince Démon."),
        shiny::p("Vous devez nous demander cela dans les 3 minutes après que votre cible ait obtenu son rapport d'enquête. Nous serons discrets, il n'en saura rien."),
        shiny::p("Cela vous coûtera 2 points de pouvoir.")
      ),
      shinydashboardPlus::controlbarItem( # Assuming this comes from shinydashboardPlus
        "Interception",
        shiny::p("Si vous remarquez, via une conversation ou une indiscrétion, qu'un Archange ou Prince Démon se prépare à enquêter sur un sujet qui ne vous arrange pas (bref, qu'on cherche à vous nuire...), vous pouvez nous demander d'intercepter le rapport."),
        shiny::p("Vous devez nous envoyer en interception avant qu'il ne lance son enquête. S'il fait ensuite une enquête dans les 15 minutes, nous serons en mesure de l'intercepter et de vous la transmettre."),
        shiny::p("Cela vous coûtera 2 points de pouvoir. Mais nous préférons vous prévenir, nous avons environ 1 chance sur 6 de nous faire capter par nos concurrents et de ne pas pouvoir intercepter convenablement l'indice.")
      )
    )
  )
}


  # The main UI object to be returned by the function
  ui_dashboard_page <- shinydashboard::dashboardPage(
    header = header(),
    sidebar = sidebar(),
    controlbar = controlbar(),
    body = body()
  )

  return(ui_dashboard_page)
}
