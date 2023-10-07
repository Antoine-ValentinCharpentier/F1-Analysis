## server.R ##
library("shiny")
library("shiny.i18n")
library("plotly")
library("readr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("forcats")
library("RColorBrewer")
library("treemap")
library("sp")
library("mapdeck")
library("mapdata")
library("maps")
library("sf")
library("rnaturalearth")
library("countrycode")
library("shinyWidgets")

i18n <- Translator$new(translation_json_path = "./www/translations.json")
i18n$set_translation_language("fr")

function(input, output, session) {
  
  column_types_results <- cols(
    position = col_character(),
    number = col_character(),
    fastestLapSpeed = col_character()
  )
  column_types_driver_standings <- cols(
    positionText = col_character()
  )
  column_types_constructor_standings <- cols(
    positionText = col_character()
  )
  
  #Import data
  circuits <- read_csv("../data/circuits.csv",show_col_types = FALSE)
  constructors <- read_csv("../data/constructors.csv",show_col_types = FALSE)
  constructor_standings <- read_csv("../data/constructor_standings.csv", col_types = column_types_constructor_standings)
  drivers <- read_csv("../data/drivers.csv",show_col_types = FALSE)
  driver_standings <- read_csv("../data/driver_standings.csv", col_types = column_types_driver_standings)
  pit_stops <- read_csv("../data/pit_stops.csv",show_col_types = FALSE)
  qualifying <- read_csv("../data/qualifying.csv",show_col_types = FALSE)
  races <- read_csv("../data/races.csv",show_col_types = FALSE)
  results <- read_csv("../data/results.csv", col_types = column_types_results)
  status <- read_csv("../data/status.csv",show_col_types = FALSE)
  
  # modification des types
  results$position <- na_if(results$position, "\\N")
  results$position <- as.numeric(results$position)
  
  results$number <- na_if(results$number, "\\N")
  results$number <- as.numeric(results$number)
  
  results$fastestLapSpeed <- na_if(results$fastestLapSpeed, "\\N")
  results$fastestLapSpeed <- as.numeric(results$fastestLapSpeed)
  
  # modification du format de l'attribut min:sec => sec
  results$fastestLapTime <- sapply(results$fastestLapTime, function(x) {
    temps_split <- strsplit(x, ":", fixed = TRUE)[[1]]
    if (length(temps_split) != 2) {
      NA
    } else {
      minutes <- as.numeric(temps_split[1])
      secondes <- as.numeric(temps_split[2])
      minutes * 60 + secondes
    }
  })
  
  # modification du format de l'attribut min:sec ou sec => sec
  pit_stops$duration <- sapply(pit_stops$duration, function(x) {
    temps_split <- strsplit(x, ":", fixed = TRUE)[[1]]
    if (length(temps_split) != 2) {
      as.numeric(temps_split[1])
    } else if (length(temps_split) == 2) {
      minutes <- as.numeric(temps_split[1])
      secondes <- as.numeric(temps_split[2])
      minutes * 60 + secondes
    } else {
      NA
    }
  })
  
  #Ajout d'attributs supplémentaires
  races$month <- format(races$date, "%B")
  
  # Nettoyage du dataset
  circuits <- circuits %>% select(-circuitRef, -url, -alt)
  constructors <- constructors %>% select(-constructorRef,-url)
  constructor_standings <- constructor_standings %>% select(-constructorStandingsId, -positionText)
  drivers <- drivers %>% select(-driverRef, -number, -code,-url)
  driver_standings <- driver_standings %>% select(-driverStandingsId, -positionText)
  pit_stops <- pit_stops %>% select(-time)
  qualifying <- qualifying %>% select(-qualifyId, -number)
  races <- races %>% select(-sprint_date, -sprint_time, -quali_date, - quali_time, -url, -time, -fp1_date, -fp2_date, -fp3_date)
  results <- results %>% select(-resultId, -number, -positionText, -time, -fastestLap)
  
  
  
  
  
  # Changer la langue en fonction du menu de sélection
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language)
  })
  # Changer le thème (couleurs) :
  colors <- reactiveVal(list(
    skin = "red",
    first = "#DC0000",
    second = "#FFF12C",
    third = "#33A357",
    fourth = "#000000"
  ))
  observeEvent(input$theme, {
    if (input$theme == "Ferrari") {
      colors(list(
        skin = "red",
        first = "#DC0000",
        second = "#FFF12C",
        third = "#33A357",
        fourth = "#000000"
      ))
    } else if (input$theme == "Mercedes") {
      colors(list(
        skin = "black",
        first = "#C0C0C0",
        second = "#04E4E4",
        third = "#A40104",
        fourth = "#010101"
      ))
    } else if (input$theme == "Brawn GP") {
      colors(list(
        skin = "yellow",
        first = "#E5FE00",
        second = "#FBFBFD",
        third = "#CF1A1E",
        fourth = "#1E1A17"
      ))
    } else if (input$theme == "Red Bull") {
      colors(list(
        skin = "blue",
        first = "#0000A2",
        second = "#FE0018",
        third = "#FFCA00",
        fourth = "#EAEAEA"
      ))
    }
    # changer le skin
    session$sendCustomMessage("change_skin", colors())
  })

  # < ============ STATS ============ >
  output$stats_nb_drivers <- renderText({
    nrow(drivers)
  })
  output$stats_nb_constructors <- renderText({
    nrow(constructors)
  })
  output$stats_nb_circuits <- renderText({
    nrow(circuits)
  })
  output$stats_nb_races <- renderText({
    nrow(races)
  })

  # < ============ CIRCUITS ============ >

  # Stats
  output$stats_mean_speed <- renderText({
    mean(results$fastestLapSpeed, na.rm = TRUE) %>%
      round(2)
  })

  output$stats_mean_laps_time <- renderText({
    mean(results$fastestLapTime, na.rm = TRUE) %>%
      round(2)
  })

  output$stats_country_most_circuits <- renderText({
    occurrences <- table(circuits$country)
    indice_max <- which.max(occurrences)
    pays_max <- names(occurrences)[indice_max]
    return(pays_max)
  })

  # plot 1
  output$plot1<-renderPlotly({
    
      visu1<-inner_join(races, circuits, by = "circuitId") %>%
              select(circuit_name = name.y) %>%
                count(circuit_name) %>%
                  slice_max(n, n = input$nb_circuits) %>%
                    mutate(circuit_name = reorder(circuit_name, n)) %>%
                    ggplot(aes(x=n, y=circuit_name)) + 
                    geom_col(fill = colors()$first) +
                    labs(x = i18n$t("Nombre de courses"), y =i18n$t("Circuits"))+
                          geom_text(aes(label = n), hjust = -0.5, size = 3,position = position_dodge(width = 1),inherit.aes = TRUE)
    ggplotly(visu1)
    
   
  })
  output$plot1_title<-renderText({
    paste( "Top",as.character(input$nb_circuits),i18n$t("des circuits les plus pratiqués"),sep=" ")
  })
  
  
  #plot 2
  
  output$plot2<-renderPlotly({
    circuit_year <- races %>%
      inner_join(circuits, by = "circuitId") %>%
      select(circuitId, name.x, location, country, lat, lng, year) %>%
      group_by(circuitId) %>%
      mutate(first_timer=min(year)) %>%
      filter(first_timer >= input$year_range[1], first_timer <= input$year_range[2])%>%
      select(-year) %>%
      distinct(.keep_all = TRUE)

    map <- plot_geo(circuit_year) %>%
      add_markers(
        x = ~lng,
        y = ~lat,
        color = colors()$first,
        text = ~name.x,
        marker = list(color = colors()$first)
      )
    
    map <- map %>%
      layout(
        xaxis = list(title = i18n$t("Longitude")),
        yaxis = list(title = i18n$t("Latitude")),
        legend = list(title = "Variable", font = list(color = colors()$first)),
        geo = list(
          showland = TRUE,
          landcolor = "lightgrey",
          showocean = TRUE,
          oceancolor = "#2F4F4F"
        )
      )
    ggplotly(map)
    
  })
  
  output$plot2_title<-renderText({
    i18n$t("Carte des circuits dans le monde")
  })

  observe({
    box_choice <- c(i18n$t("Distribution des temps de tour"),i18n$t("Statistiques de vitesse moyenne"),i18n$t("Distribution de la durée des arrêts aux stands"))
    updateSelectInput(session, "moustache_choice", choices = box_choice)
  })
  
  
  # Plot 19 

  q19 <- results %>% 
        left_join(races,by = join_by(raceId == raceId)) %>% 
          left_join(circuits,by = join_by(circuitId == circuitId)) %>% 
            rename(circuitName = name.y) %>%
              drop_na(fastestLapTime) %>%
                filter(fastestLapTime < 150)

  mean_time_19 <- reactive({
    res <- q19 %>%
      group_by(circuitName) %>%
        summarize(mean_time = mean(fastestLapTime, na.srm = TRUE)) %>%
          arrange(desc(mean_time))

    nbCircuits <- input$nb_circuits
    if (!is.null(nbCircuits)) {
      res <- slice_max(res, n = nbCircuits, with_ties = FALSE, order_by = mean_time)
    }
    return(res)
  })

  q5 <- results %>% 
        left_join(races,by = join_by(raceId == raceId)) %>%
          left_join(circuits,by = join_by(circuitId == circuitId)) %>% 
            rename(circuitName = name.y) %>%
              drop_na(fastestLapSpeed) %>%
                  filter(fastestLapSpeed > 140) 

  mean_speed_5 <- reactive({
    res <- q5 %>%
      group_by(circuitName) %>%
        summarize(mean_speed = mean(fastestLapSpeed, na.srm = TRUE))%>%
          arrange(desc(mean_speed)) 
    nbCircuits <- input$nb_circuits
    if (!is.null(nbCircuits)) {
      res <- slice_max(res, n = nbCircuits, with_ties = FALSE, order_by = mean_speed)
    }
    return(res)
  })

  q16 <- pit_stops %>%
        left_join(races,by = join_by(raceId == raceId)) %>%
          left_join(circuits,by = join_by(circuitId == circuitId)) %>%
            rename(circuitName = name.y) %>%
              drop_na(duration) %>%
                filter(duration < 45)

  mean_time_16 <- reactive({
    res <- q16 %>%
        group_by(circuitName) %>%
          summarize(mean_time = mean(duration, na.srm = TRUE)) %>%
            arrange(desc(mean_time)) 
    nbCircuits <- input$nb_circuits
    if (!is.null(nbCircuits)) {
      res <- slice_max(res, n = nbCircuits, with_ties = FALSE, order_by = mean_time)
    }
    return(res)
  })

  output$plot19_5_16<-renderPlotly({
    if(input$moustache_choice==i18n$t("Distribution des temps de tour")){
      mean_time_data <- mean_time_19()
      visu<-ggplot(q19, 
                     aes(x = circuitName, y = fastestLapTime, fill = circuitName)) + 
        geom_boxplot() +
        labs(x =  i18n$t("Circuits"), 
             y =  i18n$t("Temps d'un tour (secondes)")) +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))+
        scale_x_discrete(limits = mean_time_data$circuitName)
      
    }else if(input$moustache_choice==i18n$t("Statistiques de vitesse moyenne")){
      #5
      mean_speed_data <- mean_speed_5()
      visu<-ggplot(q5, aes(x = circuitName, y = fastestLapSpeed, fill = circuitName)) + 
        geom_boxplot() +
        labs(x = i18n$t("Circuits"), y = i18n$t("Vitesse moyenne (km/h)")) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), plot.title = element_text(hjust = 0.5,face = "bold", size = 13))+
        scale_x_discrete(limits = mean_speed_data$circuitName)
    }else{
      
      #16
      mean_time_data <- mean_time_16()
      visu<-ggplot(q16, 
                     aes(x=circuitName, y=duration, fill=circuitName)) +
        geom_boxplot() +
        labs(x = i18n$t("Circuits"), 
             y = i18n$t("Temps d'arrêt (secondes)")) +
        theme(legend.position = "none", 
              plot.title = element_text(hjust = 0.5,face = "bold", size = 13),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
        scale_x_discrete(limits = mean_time_data$circuitName)
    }
    
    
    ggplotly(visu)
    
  })
  
  output$plot19_5_16_title<-renderText({
    if(input$moustache_choice==i18n$t("Distribution des temps de tour")){
      i18n$t("Distribution des temps de tour pour chaque circuit")
    }else if(input$moustache_choice==i18n$t("Statistiques de vitesse moyenne")){
     i18n$t("Statistiques de vitesse moyenne par circuit")
    }else{
      i18n$t("Distribution de la durée des arrêts aux stands par circuit en F1")
    }
  })
  
  
  # Plot 6
  output$plot6<-renderPlot({
   
    #jointure des tables status, results, races, et circuits + data cleaning + on filtre que les courses où il y a eu des accidents
    circuit_accidents<- results %>%
      inner_join(status,by = join_by(statusId == statusId)) %>% 
      inner_join(races,by = join_by(raceId == raceId)) %>% 
      inner_join(circuits,by = join_by(circuitId == circuitId)) %>%  
      rename("circuit_name" = "name.y") %>% filter(statusId == 3)
    
    #on ajoute une colonne qui donne la moyenne de la plus grande vitesse atteinte sur les circuits accidentés
    circuit_vitesse <- circuit_accidents %>%
      group_by(circuit_name) %>%
      summarise(average_fastestLapSpeed = mean(fastestLapSpeed, na.rm = TRUE))
    
    #on compte le nombre d'accident moyen par course sur chaque circuit  
    circuit_moyenne_accident <- circuit_accidents %>% 
      count(circuit_name, name = "n_accidents")
    
    #on joint les deux tables précédentes pour obtenir la table suivant: CIRCUIT_ACCIDENTS(nom_circuit, nombre_accidents, plus_gde_vitesse_moyenne)
    circuit_accidents <- circuit_vitesse %>% 
      inner_join(circuit_moyenne_accident, by = join_by(circuit_name == circuit_name)) %>% 
      select(circuit_name, n_accidents, average_fastestLapSpeed)
    
    #Nombre de courses par circuit
    circuit_courses <- inner_join(races, circuits, by = "circuitId") %>%
      select(circuit_name = name.y) %>%
      count(circuit_name,name="n_courses")
    
    #Tous les circuits accidentés 
    circuit_stats <- right_join(circuit_accidents, circuit_courses, by = "circuit_name") %>%
      replace(is.na(.), 0)%>%
      mutate(n_avg_accidents = n_accidents / n_courses)%>%   #calcul du nombre moyen d'accidents sur un circuit (nbr_accidents/nbr_course)
      mutate(across(c("n_avg_accidents"), ~round(., 2))) %>%   #arrondi à 2 chiffres apres la virgule n_avg_accidents
      filter(average_fastestLapSpeed != 0)   #on retire les circuits où il y a aucune valeur pour la vitesse maximale atteinte
    
    #scatter plot sans plotly
    visu6<-circuit_stats %>%
      ggplot(mapping = aes(y = n_avg_accidents, x = average_fastestLapSpeed)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, color = colors()$first) +
      scale_y_continuous(limits = c(0,4)) +
      labs(y = i18n$t("Moyenne des nombres d'accidents"), x =i18n$t("Moyenne des vitesses maximales atteintes")) +
      theme(plot.title = element_text(hjust = 0.5,face = "bold", size = 13))
    
    #ggplotly(visu6)
    visu6
    
  })
  
  output$plot6_title<-renderText({
    i18n$t("Corrélation entre le nombre d'accidents et la vitesse de pointe des circuits")
  })
  
  
  # Plot 7
  output$plot7<-renderPlotly({
    
    #Nombre d'accidents par circuit
    circuit_accidents <- results %>%
      inner_join(status,by = join_by(statusId == statusId)) %>%
      inner_join(races,by = join_by(raceId == raceId)) %>%
      inner_join(circuits,by = join_by(circuitId == circuitId)) %>% 
      rename("circuit_name" = "name.y") %>%
      filter(statusId == 3) %>%
      count(circuit_name, name = "n_accidents")
    
    circuit_courses <- inner_join(races, circuits, by = "circuitId") %>%
      select(circuit_name = name.y) %>%
      count(circuit_name,name="n_courses")
    
    #Top 15 des plus accidenté
    circuit_stats <- right_join(circuit_accidents, circuit_courses, by = "circuit_name") %>%
      replace(is.na(.), 0)%>%
      mutate(n_avg_accidents = n_accidents / n_courses)%>%
      mutate(across(c('n_avg_accidents'), ~ round(., 2)))%>%
      slice_max(n_avg_accidents, n = input$nb_circuits)%>%
      mutate(circuit_name = reorder(circuit_name, n_avg_accidents))
    
    # Visualisation des données
    visu7 <- circuit_stats %>%
      ggplot(aes(y =circuit_name, x = n_avg_accidents)) +
      geom_col(fill= colors()$first) +
      labs(x = i18n$t("Nombre moyen d'accidents par course"), 
           y = i18n$t("Circuits")) +
      geom_text(aes(label = n_avg_accidents), 
                hjust = -0.5, 
                size = 3, 
                position = position_dodge(width = 1), 
                inherit.aes = TRUE) +
      theme(plot.title = element_text(hjust = 0.5,face = "bold", size = 13))

    
    ggplotly(visu7)
    
  })
  
  output$plot7_title<-renderText({
    paste( "Top",as.character(input$nb_circuits),i18n$t("des circuits les plus accidentés en moyenne"),sep=" ")
  })
  
  # Plot 7 bis
  output$plot7bis<-renderPlotly({
    
    circuit_accidents <- results %>%
      inner_join(status,by = join_by(statusId == statusId)) %>%
      inner_join(races,by = join_by(raceId == raceId)) %>%
      inner_join(circuits,by = join_by(circuitId == circuitId)) %>% 
      rename("circuit_name" = "name.y") %>%
      filter(statusId == 3) %>%
      count(circuit_name, name = "n_accidents")
    
    circuit_courses <- inner_join(races, circuits, by = "circuitId") %>%
      select(circuit_name = name.y) %>%
      count(circuit_name,name="n_courses")
    
    #Top 15 des plus accidenté
    circuit_stats <- right_join(circuit_accidents, circuit_courses, by = "circuit_name") %>%
      replace(is.na(.), 0)%>%
      mutate(n_avg_accidents = n_accidents / n_courses)%>%
      mutate(across(c('n_avg_accidents'), ~ round(., 2)))%>%
      slice_max(n_avg_accidents, n = input$nb_circuits)%>%
      mutate(circuit_name = reorder(circuit_name, n_avg_accidents))
    
    name_acc <- circuit_stats$circuit_name
    
    visu7bis <- inner_join(races, circuits, by = "circuitId") %>%
      select(circuit_name = name.y, year) %>%
      filter(circuit_name %in% name_acc) %>%
      arrange(match(circuit_name, name_acc)) %>%
      mutate(circuit_name = reorder(circuit_name, match(circuit_name, name_acc))) %>%
      mutate(circuit_name = fct_rev(circuit_name)) %>%
      ggplot(aes(y = circuit_name, x = year)) +
      geom_point(color = colors()$first ) +
      labs(x = i18n$t("Année"), 
           y = i18n$t("Circuits")) +
      theme(plot.title = element_text(hjust = 0.5,face = "bold", size = 13))
    
    ggplotly(visu7bis)
    
  })
  
  output$plot7bis_title<-renderText({
    i18n$t("Utilisation des circuits au fil des années")
  })
  
  
  # Plot 20
  output$plot20<-renderPlotly({
    race_circuit <- inner_join(races, circuits, by = "circuitId")%>%
      filter(year >= input$plot_20_year_range[1], year <= input$plot_20_year_range[2])%>%
      count(location ,month,name="nb_course")
    
    
    ordre_mois <- c("janvier", "février", "mars", "avril", "mai", "juin",
                    "juillet", "août", "septembre", "octobre", "novembre", "décembre")
    
    race_circuit$month <- factor(race_circuit$month, levels = ordre_mois)
    
    visu20<-ggplot(race_circuit, 
                   aes(x=month,y=location,fill=nb_course)) + 
      geom_tile()+
      labs(x = i18n$t("Mois"), 
           y = i18n$t("Ville"),
           fill=i18n$t("Nombre de courses")) +
      scale_fill_gradient(low=colors()$second,high=colors()$first)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(visu20)
    
  })
  
  output$plot20_title<-renderText({
    i18n$t("Concentration des courses par mois et par ville")
  })
  
  # < ============ CONSTRUCTORS ============ >

  # Stats
  output$stats_nb_nationality_constructors <- renderText({
    constructors %>% 
      distinct(nationality) %>%
        nrow()
  })

  output$stats_constructor_first <- renderText({
    count_wins <- results %>%
      filter(position == 1) %>%
        group_by(constructorId) %>%
          summarize(nombre_wins = n())

    res <- count_wins %>%
      filter(nombre_wins == max(nombre_wins)) %>%
        inner_join(constructors)

    return(paste0(res$name, " (", res$nombre_wins, ")"))
  })

  races$date <- as.Date(races$date, format = "%Y-%m-%d")

  # Regrouper les dates par année et extraire la date la plus récente pour chaque année
  last_dates <- aggregate(date ~ year, data = races, FUN = max) %>%
    merge(races, by = c("year", "date"))

  # Sélectionner les colonnes pertinentes
  last_dates <- last_dates[, c("year", "date", "raceId")]

  # Renommer la colonne "raceId" en "last_race_id"
  colnames(last_dates)[3] <- "last_race_id"

  # Fusionner last_dates avec constructor_standings
  standing_by_year <- merge(last_dates, constructor_standings, by.x = "last_race_id", by.y = "raceId") %>%
    merge(constructors, by = "constructorId") %>%
    arrange(last_race_id, desc(points)) %>%
    group_by(last_race_id) %>%
    mutate(classement = rank(desc(points))) %>%
    filter(classement == 1)
  
  treemap_df_filtered <- reactive({
    data <- standing_by_year %>%
              filter(year >= input$plot9_year_range[1], year <= input$plot9_year_range[2])

    if (nrow(data) == 0) {
      # Aucune donnée correspondant à la plage de dates sélectionnée
      return(data.frame())
    }

    natio <- input$choose_nationality
    if (!is.null(natio) && i18n$t("Tous") != natio) {
      if (natio %in% unique(data$nationality)) {
        data <- data %>%
          filter(nationality == natio)
      } else {
        return(data.frame())
      }
    }

    # Création du dataframe pour le treemap
    treemap_df <- data %>%
                    group_by(nationality, name) %>%
                      tally()

    return(treemap_df)
  })

  # Choose a nationality
  observe({
    nationalities <- sort(unique(standing_by_year$nationality))
    all_nationalities <- c(i18n$t("Tous"), nationalities)
    updatePickerInput(session, "choose_nationality", choices = all_nationalities)
  })

  # Plot 9
  output$plot9 <- renderPlot({
    treemap_filtered <- treemap_df_filtered()
    if (nrow(treemap_filtered) == 0) {
      # Aucune donnée pour générer le treemap
      return(NULL)
    }

    # Calculer la plage de valeurs pour la colonne "n"
    min_n <- min(treemap_filtered$n, na.rm = TRUE)
    max_n <- max(treemap_filtered$n, na.rm = TRUE)

    # Calculer les tailles de police correspondantes en utilisant une transformation linéaire
    min_fontsize <- 10  # Taille de police minimale
    max_fontsize <- 20  # Taille de police maximale

    # Calculer les tailles de police en fonction des valeurs de la colonne "n"
    fontsize <- min_fontsize + (max_fontsize - min_fontsize) * (treemap_filtered$n - min_n) / (max_n - min_n)
    # Graphique Treemap
    if (nrow(treemap_filtered) == 1) {
      # Cas spécial : une seule case
      font_label <- max_fontsize
    } else {
      # Cas général : plusieurs cases
      font_label <- c(13, fontsize)
    }

    treemap(dtf = treemap_filtered,
            index = c("nationality", "name"),
            vSize = "n",
            fontsize.labels = font_label,
            fontcolor.labels = c("black", colors()$first),
            fontface.labels = c(2, 1),
            align.labels = list(c("center", "top"), c("center", "center")),
            border.col = c(colors()$first, colors()$first),
            fontsize.title = 13)
  })

  output$plot9_errors <- renderUI({
    if (nrow(treemap_df_filtered()) == 0) {
      div(i18n$t("Aucune donnée disponible"), 
          style = paste("color:", colors()$first, ";", sep = ""))
    } 
  })

  output$plot9_title <- renderText({
    data <- i18n$t("Répartition des écuries et pays champions entre {start_date} et {end_date}")
    data <- gsub('\\{start_date\\}', input$plot9_year_range[1], data)
    data <- gsub('\\{end_date\\}', input$plot9_year_range[2], data)
    return(data)
  })
  
  
  # Choose a constructor
  observe({
    names <- sort(constructors$name)
    all_names <- c(i18n$t("Tous"), names)
    updatePickerInput(session, "choose_constructor", choices = all_names)
  })

  # Fonction de filtrage des données en fonction du constructeur sélectionné
  filter_by_constructor <- function(data, constructorName) {
    if (!is.null(constructorName) && i18n$t("Tous") != constructorName) {
        if (constructorName %in% unique(data$name.y)) {
          data <- data %>%
            filter(name.y == constructorName)
        } else {
          return(data.frame())
        }
      }
    return(data)
  }

  # Plot 10
  total_duration <- pit_stops %>%
                      left_join(qualifying, by = c("raceId", "driverId")) %>%
                        select(duration, raceId, constructorId) %>%
                          group_by(raceId, constructorId) %>%
                            summarize(total_duration = sum(duration), .groups = 'drop')
  
  q10 <- reactive({
      data <- constructor_standings %>% 
        left_join(races, by = "raceId") %>%
          left_join(total_duration, by = c("raceId", "constructorId")) %>%
            left_join(constructors, by = c("constructorId")) %>%
              drop_na(total_duration) %>%
                arrange(position) %>%
                  filter(total_duration < 750) %>%
                    filter(position <= input$nb_position_plot10) %>%
                      mutate(position = factor(position, levels = unique(position)))

      data <- filter_by_constructor(data, input$choose_constructor)
      return(data)
    })

  output$plot10_title<-renderText({
    paste0(
      i18n$t("Distribution du temps d'arrêt au stop en fonction de la position d'arrivée"),
      " (",
      input$choose_constructor,
      ")"
    )
  })

  output$plot10_errors <- renderUI({
    q10_data <- q10()
    if (is.null(q10_data) || nrow(q10_data) == 0) {
      div(i18n$t("Aucune donnée disponible"), 
          style = paste("color:", colors()$first, ";", sep = ""))
    } 
  })

  output$plot10 <- renderPlotly({
    q10_data <- q10()

    req(!is.null(q10_data) && nrow(q10_data) > 0)

    gg <- ggplot(q10_data, aes(x = position, y = total_duration, fill = position)) +
          geom_violin() +
          labs(
              x = i18n$t("Position d'arrivée"),
              y = i18n$t("Durée d'arrêt (secondes)")
          ) +
          theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5,face = "bold", size = 13))
    ggplotly(gg)
  })

  # Plot 11

  evolution_pit_stop <- reactive({
    data <- left_join(pit_stops, races, by = "raceId") %>%
      left_join(qualifying, by = c("raceId", "driverId")) %>%
      left_join(constructors, by = c("constructorId")) %>%
      select(raceId, year, milliseconds, name.y) %>%
      group_by(year) %>%
      filter_by_constructor(input$choose_constructor)

    if(nrow(data) == 0) {
      return(data)
    }

    data$duration <- as.numeric(data$milliseconds)

    data <- data %>%
      summarize(medianne_pitstop = median(milliseconds, na.rm = TRUE))     

    return(data)
  })

  output$plot11_title <- renderText({
    paste0(
      i18n$t("Evolution du temps médian d'arrêt depuis 2011"),
      " (",
      input$choose_constructor,
      ")"
    )
  })

  output$plot11_errors <- renderUI({
    q11_data <- evolution_pit_stop()
    if (is.null(q11_data) || nrow(q11_data) == 0) {
      div(i18n$t("Aucune donnée disponible"), 
          style = paste("color:", colors()$first, ";", sep = ""))
    } 
  }) 

  output$plot11 <- renderPlotly({
    q11_data <- evolution_pit_stop()
    req(!is.null(q11_data) && nrow(q11_data) > 0)

    gg <- ggplot(q11_data, aes(x = year, y = medianne_pitstop)) +
          geom_line(color = colors()$first) +
          labs(x = i18n$t("Année"), y = i18n$t("Temps d'arrêt médian (en millisecondes)")) + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 13))

    ggplotly(gg)
  })

  # Plot 14 => Flux

  #DataFrame qui contient les valeurs pour afficher la carte du monde grâce à la library "maps"
  world_map <- map_data("world")


  #DataFrame qui associe les nationalités et les pays
  nationality <- c("American", "Austrian", "Belgian", "Brazilian", "British", "Canadian", "Dutch", "French", "German", "Hong Kong", "Indian", "Irish", "Italian", "Japanese", "Malaysian", "Mexican", "New Zealander", "Russian", "South African", "Spanish", "Swiss")

  countrie <-c("United States of America", "Austria", "Belgium", "Brazil", "United Kingdom", "Canada", "Netherlands", "France", "Germany", "Hong Kong", "India", "Ireland", "Italy", "Japan", "Malaysia", "Mexico", "New Zealand", "Russia", "South Africa", "Spain", "Switzerland")

  countrie_nationality <- data.frame(nationality = nationality, countrie = countrie)


  # Récupération de la latitude et de la longitude des pays concernés grâce à la library "rnaturalearth"
  countries <- ne_countries()
  countries$longitude <- coordinates(countries)[,1]
  countries$latitude <- coordinates(countries)[,2]
  countrie_nationality <- countries@data %>%
    select(admin, longitude, latitude)%>%
      merge(countrie_nationality, by.x = "admin", by.y = "countrie")%>%
        rbind(c("Hong Kong", 114.177216, 22.302711, "Hong Kong"))


  #DataFrame qui recense le volume de transfert entre les combinaisons de pays
  transfert <- inner_join(results, races, by = "raceId")%>%
    select(driverId, constructorId, year) %>%
      distinct()%>%
        group_by(driverId, constructorId)%>%
          slice_max(year)%>%
            arrange(driverId, constructorId, year)%>%
              ungroup()%>%
                mutate(constructorNext = ifelse(driverId == lead(driverId), lead(constructorId), NA))%>%
                  na.omit()%>%
                    inner_join(constructors, by ="constructorId")%>%
                      select(driverId, nationality, constructorNext, year)%>%
                        merge(constructors, by.x= "constructorNext", by.y="constructorId")%>%
                          select(nationality.x, nationality.y, year)%>%
                            group_by(nationality.x, nationality.y) %>%
                              tally()%>%
                                filter(nationality.x != nationality.y)

  colnames(transfert) <- c("pays_sortant", "pays_entrant", "volume")
  transfert[transfert$pays_sortant == "East German","pays_sortant"] <- "German"
  transfert[transfert$pays_entrant == "East German","pays_entrant"] <- "German"
  doublon = data.frame(pays_sortant = c("German"), pays_entrant = c("British"), volume = c(17))
  transfert <- rbind(transfert, doublon)
  transfert <- transfert[-c(36,47,48),]


  #Aggrégat du volume de transfert sortant et entrant par pays
  volume_par_pays_entrant <- aggregate(volume ~ pays_entrant, data = transfert, sum)
  volume_par_pays_sortant <- aggregate(volume ~ pays_sortant, data = transfert, sum)


  #DataFrame qui recense la balance (volume entrant - volume sortant) de tranfert par pays
  balance_par_pays <- merge(volume_par_pays_entrant, volume_par_pays_sortant, by.x = "pays_entrant", by.y = "pays_sortant")%>%
                        mutate(balance = volume.x - volume.y)

  balance_par_pays$color <- ifelse(balance_par_pays$balance > 0, "Positive", "Negative")


  #Ajout de la latitude et de la longitude du pays sortant
  transfert <- countrie_nationality %>%
    merge(transfert, by.x ="nationality", by.y = "pays_sortant")%>%
      select(admin, longitude, latitude, pays_entrant, volume)

  colnames(transfert) <- c("pays_sortant", "start_long", "start_lat", "pays_entrant", "volume")


  #Ajout de la latitude et de la longitude du pays entrant
  transfert <- countrie_nationality %>%
    merge(transfert, by.x = "nationality", by.y = "pays_entrant")%>%
      select(admin, longitude, latitude, pays_sortant, start_long, start_lat, volume)

  colnames(transfert)<- c("pays_entrant", "end_long", "end_lat", "pays_sortant", "start_long", "start_lat", "volume")


  #Transformation des colonnes suivantes en numerique
  transfert$end_long <- as.numeric(transfert$end_long)
  transfert$end_lat <- as.numeric(transfert$end_lat)
  transfert$start_long <- as.numeric(transfert$start_long)
  transfert$start_lat <- as.numeric(transfert$start_lat)


  # ID a chaque transfert
  transfert <- transfert %>%
    mutate(ID = row_number())


  #Join le dataframe transfert avec lui même pour calculer le différence de volume entre une combinaison de pays et la combinaison opposée
  merge_transfert <- merge(transfert, transfert, by.x = c("pays_entrant", "pays_sortant"), by.y = c("pays_sortant", "pays_entrant"))

  merge_transfert$diff_volume <- merge_transfert$volume.x - merge_transfert$volume.y
  merge_transfert <- merge_transfert%>%
    select(pays_entrant, end_long.x, end_lat.x, pays_sortant, start_long.x, start_lat.x, ID.x, diff_volume)

  colnames(merge_transfert) <- c("pays_entrant", "end_long", "end_lat", "pays_sortant", "start_long", "start_lat", "ID", "diff_volume")


  #ID des transferts qui ne possèdent pas de combinaison opposée
  transfert_not_in_merge <- transfert[transfert[!(transfert$ID %in% merge_transfert$ID), "ID"],]

  colnames(transfert_not_in_merge) <- c("pays_entrant", "end_long", "end_lat", "pays_sortant", "start_long", "start_lat", "diff_volume", "ID")


  #Réecriture du dataframe transfert avec uniquement des transferts unidirectionnels (>2 transfert pour plus de lisibilté)
  transfert <- rbind(merge_transfert, transfert_not_in_merge)%>%
    filter(diff_volume >2)


  #Carte interactive avec MAPBOX
  key <- 'pk.eyJ1IjoicmllbmRlbm91dmVhdSIsImEiOiJjbGg2cGljcmkwODcyM2dub3ZkYTViZWhwIn0.3EQEnEKg_nPkCXaAbh3vww'

  filtered_transfert <- reactive({
    data <- transfert

    country_in <- input$country_in
    country_out <- input$country_out
    if (!is.null(country_in) && i18n$t("Tous") != country_in) {
      data <- data %>%
        filter(pays_entrant == country_in)
    }

    if (!is.null(country_out) && i18n$t("Tous") != country_out) {
      data <- data %>%
        filter(pays_sortant == country_out)
    }

    return(data)
  })

  balance_par_pays <- reactive({
    filtered_transfert_df <- filtered_transfert()
    
    if (nrow(filtered_transfert_df) == 0) {
      return(data.frame())
    }
    
    subset_df <- subset(filtered_transfert_df, select = c("pays_entrant", "pays_sortant", "diff_volume"))
    balance_diff_volume_entrant <- aggregate(diff_volume ~ pays_entrant, data = subset_df, sum)
    balance_diff_volume_sortant <- aggregate(diff_volume ~ pays_sortant, data = subset_df, sum)
    balance_merge <- merge(balance_diff_volume_entrant, balance_diff_volume_sortant, by.x = "pays_entrant", by.y = "pays_sortant", all = TRUE)
    balance_merge[is.na(balance_merge)] <- 0
    balance_merge <- balance_merge %>%
                        mutate(balance = diff_volume.x - diff_volume.y)

    balance_merge$color <- ifelse(balance_merge$balance > 0, "Positive", "Negative")
    return(balance_merge)
  })

  output$ggplot_graph_transfert <- renderPlotly({
    balance_df <- balance_par_pays()
    req(!is.null(balance_df) && nrow(balance_df) > 0)
    graph_transfert <- balance_df %>%
                          filter(balance != 0)%>%
                            ggplot(mapping = aes(x = pays_entrant, y = balance, fill = color)) +
                            geom_bar(stat = "identity") +
                            labs(title = "Balance de transferts par pays", 
                                y = "Nombre de pilotes", 
                                x = "Nationalité") +
                            scale_fill_manual(values = c("Positive" = "green3", "Negative" = "red"))+
                            theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                                  plot.title = element_text(hjust = 0.5,face = "bold", size = 13)) + 
                            guides(fill = guide_legend(title = "Balance", title.position = "top", title.theme = element_text(size = 12)))
    return(graph_transfert)
  })

  output$plot14 <- renderUI({

    if(input$choose_graph_type == i18n$t("Graphique à barres")){
      plotlyOutput("ggplot_graph_transfert")
    } else {
      map <- mapdeck(token = key, style = 'mapbox://styles/mapbox/streets-v12', pitch = 45)
      data <- filtered_transfert()

      if(nrow(data) > 0) {
        map <- map %>%
                  add_animated_arc(
                    data = data,
                    layer_id = "ID",
                    origin = c("start_long", "start_lat"),
                    destination = c("end_long", "end_lat"),
                    stroke_width = "diff_volume",
                    frequency = 5,
                    animation_speed = 1,
                    trail_length = 3) %>%
                  add_arc(
                    data = data,
                      layer_id = "ID",
                      origin = c("start_long", "start_lat"),
                      destination = c("end_long", "end_lat"),
                      stroke_width = 2
                  )
      }
      
      return(map)
    }
  })

  observe({
    graph_type_14 <- c(i18n$t("Carte"), i18n$t("Graphique à barres"))
    updateSelectInput(session, "choose_graph_type", choices = graph_type_14)
  })

  observe({
    names <- sort(unique(transfert$pays_entrant))
    all_names <- c(i18n$t("Tous"), names)
    updatePickerInput(session, "country_in", choices = all_names)
  })

  observe({
    names <- sort(unique(transfert$pays_entrant))
    all_names <- c(i18n$t("Tous"), names)
    updatePickerInput(session, "country_out", choices = all_names)
  })

  output$plot14_errors <- renderUI({
    q14_data <- balance_par_pays()
    if ((is.null(q14_data) || nrow(q14_data) == 0) && input$choose_graph_type == i18n$t("Graphique à barres")) {
      div(i18n$t("Aucune donnée disponible"), 
          style = paste("color:", colors()$first, ";", sep = ""))
    } 
  }) 

  # < ============ PILOTES ============ >

  # Plot 8
  #Label et nombre des résultats
  q8 <- results %>%
    inner_join(status,by = join_by(statusId == statusId)) %>%
    inner_join(drivers, by = join_by(driverId == driverId)) %>%
    unite(driver,c("forename","surname"),sep = " ")

  res_ac_pretty <- reactive({
    result_accident <- q8

    driverName <- input$driver
    if (!is.null(driverName)) {
      result_accident <- filter(result_accident, driver==driverName)
    }
    
    result_accident <- result_accident %>% count(status,sort=TRUE)

    #Regrouper les autres hors  top en other
    top_status <- result_accident %>%
      slice_max(n, n = input$nb_status)
    
    other <- result_accident %>% 
      anti_join(top_status, by = "n")
    
    other_row <- tibble(status = "other", n = sum(other$n))
    
    res_ac_pretty <- bind_rows(top_status, other_row)%>%
                    mutate(status = reorder(status, n))
    return(res_ac_pretty)
  })

  output$plot8<-renderPlotly({
    visu8 <- ggplot(res_ac_pretty(), 
                    aes(x = 1, y = n, fill =status)) +
      geom_bar(position = "fill", 
               stat = "identity") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x ="",
           y = i18n$t("Pourcentage"),
           fill=i18n$t('Status')) +
      geom_text(aes(label = paste0(round(n/sum(n) * 100, 1), "%")), 
                position = position_fill(vjust = 0.5), 
                color = "white", 
                size = 3) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            plot.title = element_text(hjust = 0.5,face = "bold", size = 13))
    
    ggplotly(visu8)
    
  })

  # Stats
  output$stats_nb_nationality_drivers <- renderText({
    drivers %>% 
      distinct(nationality) %>%
        nrow()
  })
  
  output$stats_avg_age <- renderText({
    res <- qualifying %>% 
      inner_join(drivers) %>%
        inner_join(races) %>%
          select(raceId, driverId, dob, date) %>%
            mutate(age = as.numeric(difftime(date, dob, units = "days")) / 365)

    moyenne_age <- mean(res$age) %>% round(0)

    return(moyenne_age)
  })

  output$stats_driver_first <- renderText({
    count_wins <- results %>%
      filter(position == 1) %>%
        group_by(driverId) %>%
          summarize(nombre_wins = n())

    res <- count_wins %>%
      filter(nombre_wins == max(nombre_wins)) %>%
        inner_join(drivers)

    return(paste(res$forename, " ", res$surname, " (", res$nombre_wins, ")"))
  })

  output$plot8_title<-renderText({
    paste(i18n$t("Repartition des status en fin de course pour "),input$driver)
  })
  
  
  observe({
    names_list <- paste(drivers$forename, drivers$surname, sep = " ")
    sorted_names <- sort(names_list)
    updatePickerInput(session, "driver", choices = sorted_names,selected = "Michael Schumacher")
  })
  
  # Plot 18
  q18 <- results %>%
      inner_join(drivers,by = join_by(driverId == driverId)) %>%
      inner_join(races,by = join_by(raceId == raceId)) %>%
      inner_join(constructors,by = join_by(constructorId == constructorId)) %>%
      rename("constructor_name"="name.y") %>%
      unite(driver,c("forename","surname"),sep = ' ') %>%
      select(driver,year,position,constructor_name) %>%
      mutate_at('position', as.numeric) %>%
      na.omit()

  result_dri_const <- reactive({
    res <- q18
    driverName <- input$driver
    if (!is.null(driverName)) {
      res <- filter(res, driver==driverName)
    }
    return(res)
  })

  output$plot18<-renderPlotly({
    visu18 <- ggplot(result_dri_const(), aes(x=year, y=position,color=constructor_name)) + 
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x) +
      labs(x = i18n$t("Année"), 
           y = i18n$t("Classement"),
           color=i18n$t("Constructeur"))
    
    ggplotly(visu18)
  })
  
  output$plot18_title<-renderText({
    paste(i18n$t("Performances de"),as.character(input$driver),i18n$t("au cours des années en fonction des constructeurs"))
  })
  
  # Plot 17
  output$plot17<-renderPlotly({
    
    world <- ne_countries(scale = "small", returnclass = "sf")
    
    #Data preparation
    data <- drivers %>%
      mutate(countries = case_when(
        nationality == "British" ~ "United Kingdom",
        nationality == "German" ~ "Germany",
        nationality == "Spanish" ~ "Spain",
        nationality == "Finnish" ~ "Finland",
        nationality == "Japanese" ~ "Japan",
        nationality == "French" ~ "France",
        nationality == "Polish" ~ "Poland",
        nationality == "Brazilian" ~ "Brazil",
        nationality == "Portuguese" ~ "Portugal",
        nationality == "Italian" ~ "Italy",
        nationality == "Australian" ~ "Australia",
        nationality == "Austrian" ~ "Austria",
        nationality == "American" ~ "United States",
        nationality == "Dutch" ~ "Netherlands",
        nationality == "Colombian" ~ "Colombia",
        nationality == "Canadian" ~ "Canada",
        nationality == "Indian" ~ "India",
        nationality == "Hungarian" ~ "Hungary",
        nationality == "Irish" ~ "Ireland",
        nationality == "Danish" ~ "Denmark",
        nationality == "Argentine" ~ "Argentina",
        nationality == "Czech" ~ "Czech Republic",
        nationality == "Malaysian" ~ "Malaysia",
        nationality == "Swiss" ~ "Switzerland",
        nationality == "Belgian" ~ "Belgium",
        nationality == "Monegasque" ~ "Monaco",
        nationality == "Swedish" ~ "Sweden",
        nationality == "Venezuelan" ~ "Venezuela",
        nationality == "New Zealander" ~ "New Zealand",
        nationality == "Chilean" ~ "Chile",
        nationality == "Mexican" ~ "Mexico",
        nationality == "South African" ~ "South Africa",
        nationality == "Liechtensteiner" ~ "Liechtenstein",
        nationality == "Rhodesian" ~ "Rhodes",
        nationality == "American-Italian" ~ "United States,Italy",
        nationality == "Uruguayan" ~ "Uruguay",
        nationality == "Argentine-Italian" ~ "Argentina,Italy",
        nationality == "Thai" ~ "Thailand",
        nationality == "East German" ~ "Germany",
        nationality == "Russian" ~ "Russia",
        nationality == "Indonesian" ~ "Indonesia",
        nationality == "Chinese" ~ "China"
      )) %>% 
      separate_rows(countries, sep = ",") %>%
      filter(countries != "Rhodes") %>%
      group_by(countries) %>% 
      summarise(n_drivers = n()) %>% 
      arrange(desc(n_drivers)) %>% 
      mutate(countries_where_drivers = TRUE)
    
    #add iso3 country code
    #No ISO code for Rhodes
    ISO_data <- data %>%
      mutate(Iso3 = countrycode::countrycode(
        sourcevar = countries,
        origin = "country.name",
        destination = "iso3c"
      ))
    mid <- mean(ISO_data$n_drivers)
    
    #Join datasets
    drivers_countries <- world %>%
      select(geometry, name, adm0_a3) %>%
      left_join(ISO_data, by = c("adm0_a3" = "Iso3")) %>%
      filter(countries_where_drivers == TRUE)
    
    #Countries in which drivers were born
    
    map <- world %>%
      filter(admin != "Antartica") %>%
      st_transform(crs = "+proj=robin") %>%
      ggplot() +
      geom_sf(color= "#EAEAEA") +
      geom_sf(data = drivers_countries, aes(fill = n_drivers, group = countries_where_drivers)) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      labs(x = NULL, y = NULL, fill=i18n$t("Nombre de pilotes"))
    
    visu17<-map + scale_fill_gradient(low ="white", high =colors()$skin)
    
    ggplotly(visu17)
    
  })
  
  output$plot17_title<-renderText({
    paste(i18n$t("Diversité de la nationalité des pilotes de Formule 1"))
  })
  
  output$list_drivers2<-renderUI({ 
    #paste(drivers$forename, drivers$surname, sep = " ")%>% paste(collapse = '-')
    names_list <- paste(drivers$forename, drivers$surname, sep = " ")
    sorted_names <- sort(names_list)
    sorted_names<-c(i18n$t("Tous"),sorted_names)
    pickerInput("driver2", i18n$t("Choisissez votre pilote :"), choices = sorted_names, selected = i18n$t("Tous"), options = list(searchable = TRUE), width = 300)
  })
  
  filter_by_driver <- function(data, driverName) {
    if (!is.null(driverName) && i18n$t("Tous") != driverName) {
      if (driverName %in% unique(data$driver)) {
        data <- data %>%
          filter(driver == driverName)
      } else {
        return(data.frame())
      }
    }
    return(data)
  }
  
  # Plot 15
  create_data_plot15<-reactive({
    
    #Nom , et nombre de course , victoire , défaite et pourcentage de victoire
    drivers_full <- driver_standings%>%
      inner_join(drivers,by = join_by(driverId == driverId))%>%
      inner_join(races,by = join_by(raceId == raceId))%>%
      unite(driver,c("forename","surname"),sep = " ")%>%
      group_by(driver,year) %>%
      summarise(nb_course = n(),nb_victoire = max(wins),.groups = 'drop')%>%
      group_by(driver) %>%
      summarise(nb_course = sum(nb_course),nb_victoire = sum(nb_victoire),.groups = 'drop')%>%
      mutate(pourcentage_victoire = nb_victoire / nb_course) %>%
      mutate(nb_lost = nb_course-nb_victoire) %>%
      slice_max(pourcentage_victoire, n = input$nb_driver) %>%
      pivot_longer(cols=c('nb_lost', 'nb_victoire'),names_to='type',values_to='value')%>%
      mutate(driver = reorder(driver,desc(pourcentage_victoire)))
    
    data<-filter_by_driver(drivers_full,input$driver2)
    return(data)
    
    
  })
  
  
  output$plot15_errors <- renderUI({
    drivers_full2 <- create_data_plot15()
    if (is.null(drivers_full2) || nrow(drivers_full2) == 0) {
      div(i18n$t("Aucune donnée disponible ; Essayez d'augmenter le nombre de pilotes ! Celui que vous avez choisi se ne se trouve pas dans ce top ."), 
          style = paste("color:", colors()$first, ";", sep = ""))
    } 
  }) 
  
  output$plot15 <- renderPlotly({
    drivers_full2 <- create_data_plot15()
    req(!is.null(drivers_full2) && nrow(drivers_full2) > 0)
    
    visu15<-ggplot(drivers_full2, 
           aes(x =driver,
               y=value , 
               fill=type)) +
      geom_bar(position = "stack",
               stat="identity") +
      labs(x = i18n$t("Pilote"), 
           y = i18n$t("Nombre de courses")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5,face = "bold", size = 13), 
            plot.subtitle = element_text(hjust = 0.5)) +
      geom_text(aes(label = paste0(round(100*value/nb_course,2),'%')), 
                position = position_stack(vjust = 0.75), 
                size = 2, color=colors()$fourth) +
      scale_fill_manual(values = c(colors()$first, colors()$second),
                        guide = guide_legend(title = i18n$t('Résultat'), 
                                             title.position = "top", 
                                             title.theme = element_text(size = 12)))
    
    
    visu15_plotly <- ggplotly(visu15) 
    visu15_plotly$x$data[[1]]$name <- i18n$t('Défaites')
    visu15_plotly$x$data[[2]]$name <- i18n$t('Victoires')
    visu15_plotly
  })
  
  
  
 
  output$plot15_title<-renderText({
    paste("Top",as.character(input$nb_driver),i18n$t("des pilotes les plus performants classé par ratio entre le nombre de victoires/défaites"))
  })
  
  # Plot 13
  output$plot13<-renderPlotly({
    data_point <- driver_standings %>%
      inner_join(races, by = join_by(raceId == raceId)) %>%
      filter(year ==input$year_range2)
      
    
    max_points <- aggregate(points ~ driverId, data = data_point, FUN = max)
    
    
    visu13<-ggplot(max_points, aes(x = points)) +
      geom_density(fill = colors()$first, alpha = 0.5) +
      labs(x = i18n$t("Nombre de points"), y = i18n$t("Densité de pilotes")) +
      theme(plot.title = element_text(hjust = 0.5,face = "bold", size = 13)) +
      scale_y_continuous(trans = "sqrt")
    
    
    ggplotly(visu13)
    
  })
  
  output$plot13_title<-renderText({
    paste(i18n$t("Distribution des meilleurs scores en fin de saison des pilotes de Formule 1 en"), input$year_range2)
  })
  
  
  
  
  
}
