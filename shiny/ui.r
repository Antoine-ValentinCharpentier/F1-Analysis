## ui.R ##
library(shinydashboard)
library(shiny.i18n)
library(plotly)
library(shinyWidgets)

i18n <- Translator$new(translation_json_path = "./www/translations.json")
i18n$set_translation_language("fr")

dashboardPage(
  skin='red',
  dashboardHeader(
    title = div(
      tags$img(src = "./logo.png", height = "40px")
    )
  ),
  dashboardSidebar(
    usei18n(i18n),
    includeCSS("./www/style.css"),
    includeCSS("./www/f1.css"),
    includeScript("./www/skin.js"),
    sidebarMenu(
      menuItem(i18n$t("Statistiques générales"), tabName = "stats", icon = icon("car-side")),
      menuItem(i18n$t("Circuit"), tabName = "circuit", icon = icon("road")),
      menuItem(i18n$t("Constructeur"), tabName = "constructeur", icon = icon("screwdriver-wrench")),
      menuItem(i18n$t("Pilote"), tabName = "pilote", icon = icon("user-astronaut")),
      tags$div(
        class = "sidebar-footer",
        selectInput("theme", i18n$t("Choisissez votre thème en fonction d'une écurie"), 
                    choices = c("Ferrari", "Mercedes", "Brawn GP", "Red Bull"),
                    selected = "Ferrari"),
        selectInput(
          inputId = "selected_language",
          label = i18n$t("Changer la langue"),
          choices = setNames(
            i18n$get_languages(),
            c("Français", "English", "Polski")
          ),
          selected = i18n$get_key_translation()
        )
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "stats",
        h2(i18n$t("Statistiques générales")),
        fluidRow(
          infoBox(i18n$t("Nombre de pilotes"), textOutput('stats_nb_drivers'), icon = icon("user-astronaut"), color="red"),
          infoBox(i18n$t("Nombre de constructeurs"), textOutput('stats_nb_constructors'), icon = icon("screwdriver-wrench"), color='red'),
          infoBox(i18n$t("Nombre de circuits"), textOutput('stats_nb_circuits'), icon = icon("road"), color='red'),
          infoBox(i18n$t("Nombre de courses"), textOutput('stats_nb_races'), icon = icon("car-side"), color='red')
        ),
        tags$div(
          class = "f1",
          tags$div(class = "back-wing f1-color1"),
          tags$div(class = "f1-body f1-color-mix"),
          tags$div(
            class = "wheel wheel-left",
            tags$div(class = "wheel-rayons")
          ),
          tags$div(
            class = "wheel wheel-right",
            tags$div(class = "wheel-rayons")
          ),
          tags$div(class = "front-wing f1-color1"),
          tags$div(class = "support-front-wing f1-color3")
        )
      ),
      tabItem(
        tabName = "circuit",
        h2(i18n$t("Étude des circuits")),
        h3(i18n$t("Quelques statistiques")),
        fluidRow(
          infoBox(i18n$t("Vitesse moyenne (km/h)"), textOutput('stats_mean_speed'), icon = icon("gauge"), color="red"),
          infoBox(i18n$t("Temps moyen d'un tour (s)"), textOutput('stats_mean_laps_time'), icon = icon("clock"), color='red'),
          infoBox(i18n$t("Pays avec le plus de circuit"), textOutput('stats_country_most_circuits'), icon = icon("globe"), color='red')
        ),
        h3(i18n$t("Graphiques")),
        fluidRow(
          column(12,
            box(title=textOutput("plot2_title"),status='info',sliderInput("year_range", i18n$t("Année"),min =1950, max =2023,value = c(1950,2023),width=350,sep=""),
              plotlyOutput("plot2"),width="100%")
          ),
          column(12,
            sliderInput("nb_circuits",i18n$t("Nombre de circuits"), 1, 77, 10,width=300)
          ),
          box(title=textOutput("plot1_title"),status='info',
            plotlyOutput("plot1")),
          box(title=textOutput("plot7_title"),status='info',
            plotlyOutput("plot7")),
          box(title=textOutput("plot7bis_title"),status='info',
            plotlyOutput("plot7bis"))
         ),
        fluidRow(
          column(12,
            box(title=textOutput("plot19_5_16_title"),status='info',width = "100%",
              selectInput("moustache_choice", i18n$t("Choisissez ce que voulez observer :"), choices = NULL, width = "300px"),
              plotlyOutput("plot19_5_16",width = "100%",height =1000))
          )
        ),
        fluidRow(
          box(title=textOutput("plot6_title"),status='info',
                      plotOutput("plot6"))
          ),
        fluidRow(
          column(12,
            box(title=textOutput("plot20_title"),status='info',width = "100%",sliderInput("plot_20_year_range", i18n$t("Année"),min =1950, max =2023,value = c(1950,2023),width=350,sep=""),
                plotlyOutput("plot20",width = "100%",height =800))
          )
        )
      ),
      tabItem(
        tabName = "constructeur",
        h2(i18n$t("Étude des constructeurs")),
        h3(i18n$t("Quelques statistiques")),
        fluidRow(
          infoBox(i18n$t("Nombre de nationalités"), textOutput('stats_nb_nationality_constructors'), icon = icon("globe"), color="red"),
          infoBox(i18n$t("Constructeur étant arrivé 1er le plus de fois"), textOutput('stats_constructor_first'), icon = icon("trophy"), color='red')
        ),
        h3(i18n$t("Graphiques")),
        column(12,
          box(title=textOutput("plot9_title"),status='info',
            pickerInput("choose_nationality", i18n$t("Choisissez une nationalité"), choices = NULL, width=300),
            sliderInput("plot9_year_range", i18n$t("Année"),min =1950, max =2023,value = c(1950,2023),width=350,sep=""),
            uiOutput("plot9_errors"),
            plotOutput('plot9')   
          )
        ),
        column(12,
          pickerInput("choose_constructor", i18n$t("Choisissez un constructeur"), choices = NULL)
        ),
        fluidRow(
          box(title=textOutput("plot10_title"),status='info',
            sliderInput("nb_position_plot10",i18n$t("Nombre de position"), 1, 12, 12, width=300),
            uiOutput("plot10_errors"),
            plotlyOutput("plot10")    
          ),
          box(title=textOutput("plot11_title"),status='info', 
            uiOutput("plot11_errors"),
            plotlyOutput("plot11")
          )
        ),
        fluidRow(
          column(12,
            box(title=i18n$t("Flux de pilotes entre les différentes écuries (un pilote vient d’une écurie pour aller dans une autre)"),status='info',
              selectInput("choose_graph_type", i18n$t("Choisissez un type de graphique"), choices = NULL, width = "300px"), 
              uiOutput("plot14_errors"),
              div(
                style = "display: flex; gap: 10px;",
                pickerInput("country_in", i18n$t("Pays entrant"), choices = NULL, width = "300px"),
                pickerInput("country_out", i18n$t("Pays sortant"), choices = NULL, width = "300px")
              ),
              uiOutput("plot14")
            )
          )
        )
      ),
      tabItem(
        tabName = "pilote",
        h2(i18n$t("Étude des pilotes")),
        h3(i18n$t("Quelques statistiques")),
        fluidRow(
          infoBox(i18n$t("Nombre de nationalités"), textOutput('stats_nb_nationality_drivers'), icon = icon("globe"), color="red"),
          infoBox(i18n$t("Age moyen"), textOutput('stats_avg_age'), icon = icon("cake-candles"), color='red'),
          infoBox(i18n$t("Pilote étant arrivé 1er le plus de fois"), textOutput('stats_driver_first'), icon = icon("trophy"), color='red')
        ),
        h3(i18n$t("Graphiques")),
        fluidRow(
          column(12,
                 pickerInput("driver", i18n$t("Choisissez votre pilote :"), choices = NULL,  options = list(searchable = TRUE), width = 300),
          ),
          column(12,
                 box(title=textOutput("plot18_title"),status='info',
                     plotlyOutput("plot18"),width = 250)
          ),
          column(12,
            box(title=textOutput("plot8_title"),status='info', sliderInput("nb_status",i18n$t("Nombre de status"), 1, 139, 7,width=200),
                      plotlyOutput("plot8") 
            )
          ),
          column(12,
            box(title=textOutput("plot17_title"),status='info',
                plotlyOutput("plot17"),width = "100%")
          ),
          column(12,
            box(title=textOutput("plot15_title"),status='info',
                div(
                  style = "display: flex; gap: 10px;",
                sliderInput("nb_driver",i18n$t("Nombre de pilotes"), 1, 857, 15,width = "300px"),
                uiOutput("list_drivers2")
                ),
                uiOutput("plot15_errors"),
                plotlyOutput("plot15"),width = 250)
          ),
          column(12,
            box(title=textOutput("plot13_title"),status='info',
              div(
                class = "slider-color-remover",
                sliderInput("year_range2", i18n$t("Année"),1950, 2023,2023,width=350,sep=""),
              ),
              plotlyOutput("plot13")
            )
          )
        )        
      )
    )
  )
)