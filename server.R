function(input, output, session) {
  # what about number of people killed per year, breakdown of how many of 
  # them were motorists, cyclists, PEDESTRIANS!!!!
  
  ##### FILTERED DF FOR VEHICLE TYPES #####
  # REMOVE ALL UNNECESSARY COLUMNS AND IT SPEEDS UP
  
  filtered_df <- reactive({
    if (is.null(input$vehicle_category)) {
      return(df[,c("year","borough","month", "wday", "hour", "population", "zip.code", 
                   "number.of.persons.injured", "number.of.persons.killed",
                   "vehicle.type.code.1","vehicle.type.code.2","vehicle.type.code.3",
                   "vehicle.type.code.4","vehicle.type.code.5","latitude","longitude")])
    } else {
      return(df[,c("year","borough","month", "wday", "hour", "population", "zip.code", 
                   "number.of.persons.injured", "number.of.persons.killed",
                   "vehicle.type.code.1","vehicle.type.code.2","vehicle.type.code.3",
                   "vehicle.type.code.4","vehicle.type.code.5","latitude","longitude")] %>%
               filter_all(any_vars(. %in% input$vehicle_category)))
    }
  })
  
  ##### FILTERED DF FOR MAP #####
  
  filtered_df_map <- reactive({
    if (is.null(input$mapboro)) {
      if (is.null(input$mapvehicle)) {
        return(df[,c("year","borough","month", "wday", "hour", "population", "zip.code",  
                     "number.of.persons.injured", "number.of.persons.killed",
                     "number.of.pedestrians.injured", "number.of.pedestrians.killed", 
                     "number.of.cyclist.injured", "number.of.cyclist.killed",
                     "number.of.motorist.injured","number.of.motorist.killed",
                     "vehicle.type.code.1","vehicle.type.code.2","vehicle.type.code.3",
                     "vehicle.type.code.4","vehicle.type.code.5","latitude","longitude")])
      } else {
        return(df[,c("year","borough","month", "wday", "hour", "population", "zip.code",  
                     "number.of.persons.injured", "number.of.persons.killed",
                     "number.of.pedestrians.injured", "number.of.pedestrians.killed", 
                     "number.of.cyclist.injured", "number.of.cyclist.killed",
                     "number.of.motorist.injured","number.of.motorist.killed",
                     "vehicle.type.code.1","vehicle.type.code.2","vehicle.type.code.3",
                     "vehicle.type.code.4","vehicle.type.code.5","latitude","longitude")] %>%
                 filter_all(any_vars(. %in% input$mapvehicle)))
      }
    } else {
      if (is.null(input$mapvehicle)) {
        return(df[,c("year","borough","month", "wday", "hour", "population", "zip.code",  
                     "number.of.persons.injured", "number.of.persons.killed",
                     "number.of.pedestrians.injured", "number.of.pedestrians.killed", 
                     "number.of.cyclist.injured", "number.of.cyclist.killed",
                     "number.of.motorist.injured","number.of.motorist.killed",
                     "vehicle.type.code.1","vehicle.type.code.2","vehicle.type.code.3",
                     "vehicle.type.code.4","vehicle.type.code.5","latitude","longitude")] %>%
                 filter(borough %in% input$mapboro))
      } else {
        return(df[,c("year","borough","month", "wday", "hour", "population", "zip.code",  
                     "number.of.persons.injured", "number.of.persons.killed",
                     "number.of.pedestrians.injured", "number.of.pedestrians.killed", 
                     "number.of.cyclist.injured", "number.of.cyclist.killed",
                     "number.of.motorist.injured","number.of.motorist.killed",
                     "vehicle.type.code.1","vehicle.type.code.2","vehicle.type.code.3",
                     "vehicle.type.code.4","vehicle.type.code.5","latitude","longitude")] %>%
                 filter(borough %in% input$mapboro) %>%
                 filter_all(any_vars(. %in% input$mapvehicle)))
      }
    }
  })
  
  filtered_df_map2 <- reactive({
    if (is.null(input$mapzipcode)) {
      return(filtered_df_map() %>%
               filter(zip.code != 0))
    } else {
      return(filtered_df_map() %>%
               filter(zip.code != 0) %>%
               filter(zip.code %in% input$mapzipcode))
    }
  })
  
  
  ##### TAB ICONS #####
  # This is the observe section designed specifically to update the tab icons to moving icons when the tab is selected
  # IT DOESNT WORK IF EVERY TAB HAS THIS FEATURE
  
  # observe({
  #   print(input$tabs)
  #    if (is.null(input$tabs)) {
  #      output$aboutme <- renderMenu({
  #        menuItem(tabName="aboutme",
  #                 HTML('<i class="fa-solid fa-address-card fa-beat"></i> About Me'))
  #       }) 
  #      } else {
  #     if (input$tabs %in% c("aboutme")) {
  #       output$aboutme <- renderMenu({
  #         menuItem(tabName="aboutme",
  #                  HTML('<i class="fa-solid fa-address-card fa-beat"></i> About Me'))
  #       })
  #     } else {
  #       output$aboutme <- renderMenu({
  #         menuItem(tabName="aboutme",
  #                  HTML('<i class="fa-solid fa-address-card"></i> About Me'))
  #       })
  #     }
  #    }
  # })
  
  observe({
    if (input$tabs %in% c("accidents")) {
      output$accidents <- renderMenu({
        menuItem(tabName="accidents",
                 HTML('<i class="fa-solid fa-car-burst fa-beat"></i> Accidents'))
      })
    } else {
      output$accidents <- renderMenu({
        menuItem(tabName="accidents",
                 HTML('<i class="fa-solid fa-car-burst"></i> Accidents'))
      })
    }
    
  })
  
  observe({
    if (!is.null(input$tabs) & input$tabs %in% c("injuries")) {
      output$injuries <- renderMenu({
        menuItem(tabName="injuries",
                 HTML('<i class="fa-solid fa-user-injured fa-beat"></i> Injuries'))
      })
    } else {
      output$injuries <- renderMenu({
        menuItem(tabName="injuries",
                 HTML('<i class="fa-solid fa-user-injured"></i> Injuries'))
      })
    }
  })
  
  observe({
    if (!is.null(input$tabs) & input$tabs %in% c("deaths")) {
      output$deaths <- renderMenu({
        menuItem(tabName="deaths",
                 HTML('<i class="fa-solid fa-skull-crossbones fa-beat"></i> Deaths'))
      })
    } else {
      output$deaths <- renderMenu({
        menuItem(tabName="deaths",
                 HTML('<i class="fa-solid fa-skull-crossbones"></i> Deaths'))
      })
    }
  })
  
  observe({
    if (!is.null(input$tabs) & input$tabs %in% c("data")) {
      output$data <- renderMenu({
        menuItem(tabName="data",
                 HTML('<i class="fa-solid fa-database fa-beat"></i> Selected Data'))
      })
    } else {
      output$data <- renderMenu({
        menuItem(tabName="data",
                 HTML('<i class="fa-solid fa-database"></i> Selected Data:'))
      })
    }
  })
  
  observe({
    if (!is.null(input$tabs) & input$tabs %in% c("maps")) {
      output$maps <- renderMenu({
        menuItem(tabName="maps",
                 HTML('<i class="fa-solid fa-map fa-beat"></i> Maps'))
      })
    } else {
      output$maps <- renderMenu({
        menuItem(tabName="maps",
                 HTML('<i class="fa-solid fa-map"></i> Maps'))
      })
    }
  })
  
  ##### PLOT YEARS #####
  # Plot years for plot titles
  plot_title_years <- reactive({
    selected_values <- input$range
    return(paste('(', selected_values[1], '-', selected_values[2], ')' ))
  })
  
  ##### ACCIDENTS #####
  output$count_plot <- renderPlot(
    if (input$selected_option == "Annually") {
      filtered_df() %>%
        filter(year %inrange% input$range) %>%
        filter(borough %in% input$bor) %>%
        group_by(borough,year) %>%
        summarise(accident_count = n()) %>%
        ggplot(aes(x=year,y=accident_count)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") + 
        scale_x_continuous(breaks = c(2012:2023)) +
        labs(y = "Number of Accidents", x = "Year", 
             title = paste("Number of Accidents per Year by Borough",plot_title_years())) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              legend.position = "bottom")
    } else if (input$selected_option == "Monthly") {
      filtered_df() %>%
        filter(year %inrange% input$range) %>%
        filter(borough %in% input$bor) %>%
        group_by(borough,month) %>%
        summarise(accident_count = n()) %>%
        ggplot(aes(x=month,y=accident_count)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") + 
        scale_x_continuous(breaks = c(1:12), labels=month_list) +
        labs(y = "Number of Accidents", x = "Month", 
             title = "Number of Accidents per Month by Borough") +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom")
    } else if (input$selected_option == "Weekly") {
      filtered_df() %>%
        filter(year %inrange% input$range) %>%
        filter(borough %in% input$bor) %>%
        group_by(borough,wday) %>%
        summarise(accident_count = n()) %>%
        ggplot(aes(x=wday,y=accident_count)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") + 
        scale_x_continuous(breaks = c(1:7),labels=day_list) + 
        labs(y = "Number of Accidents", x = "Day of Week", 
             title = "Number of Accidents per Day of Week by Borough") +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom")
    } else if (input$selected_option == "Hourly") {
      filtered_df() %>%
        filter(year %inrange% input$range) %>%
        filter(borough %in% input$bor) %>%
        group_by(borough,hour) %>%
        summarise(accident_count = n()) %>%
        ggplot(aes(x=hour,y=accident_count)) +
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") +
        scale_x_continuous(breaks = c(0:23),labels = c(0:23)) +
        labs(y = "Number of Accidents", x = "Hour",
             title = "Number of Accidents per Hour by Borough",
             subtitle = "(Events between 0:00 - 0:59 grouped at 0)") +
        theme(plot.title = element_text(size = 16, face = "bold"),
              legend.position = "bottom")
    }
  )
  
  output$percent_plot <- renderPlot(
    if (input$selected_option == "Annually") {
      merge(
        filtered_df() %>% 
          filter(year %inrange% input$range) %>%
          group_by(borough,year) %>%
          summarise(total_year=n()),
        filtered_df() %>% 
          filter(year %inrange% input$range) %>%
          group_by(borough,year) %>%
          summarise(total_year=n()) %>%
          summarise(total_accs=sum(total_year)),
        by="borough") %>%
        mutate(pct=100*total_year/total_accs) %>%
        filter(borough %in% input$bor) %>%
        ggplot(aes(x=year,y=pct,color=borough)) + 
        geom_line(stat="identity") + 
        scale_x_continuous(breaks = c(2012:2023)) + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              legend.position = "none") + 
        labs(title = "Percentage of Accidents per Year by Borough",x="Year",y="Percentage")
    } else if (input$selected_option == "Monthly") {
      merge(
        filtered_df() %>% 
          filter(year %inrange% input$range) %>%
          group_by(borough,month) %>%
          summarise(total_month=n()),
        filtered_df() %>% 
          filter(year %inrange% input$range) %>%
          group_by(borough,month) %>%
          summarise(total_month=n()) %>%
          summarise(total_accs=sum(total_month)),
        by="borough") %>%
        mutate(pct=100*total_month/total_accs) %>%
        filter(borough %in% input$bor) %>%
        ggplot(aes(x=month,y=pct,color=borough)) + 
        geom_line(stat="identity") + 
        scale_x_continuous(breaks = c(1:12),labels=month_list) + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom") + 
        labs(title = "Percentages of Accidents by Month",x="Month",y="Percentage")
    } else if (input$selected_option == "Weekly") {
      merge(
        filtered_df() %>% 
          group_by(borough,wday) %>%
          summarise(total_wday=n()),
        filtered_df() %>% 
          group_by(borough,wday) %>%
          summarise(total_wday=n()) %>%
          summarise(total_accs=sum(total_wday)),
        by="borough") %>%
        mutate(pct=100*total_wday/total_accs) %>%
        filter(borough %in% input$bor) %>%
        ggplot(aes(x=wday,y=pct,color=borough)) + geom_line(stat="identity") + 
        scale_x_continuous(breaks = c(1:7),labels=day_list) + 
        labs(title = "Percentages of Accidents by Day of Week",
             x="Day of Week",y="Percentage") +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "none") 
    } else if (input$selected_option == "Hourly") {
      merge(
        filtered_df() %>% 
          group_by(borough,hour) %>%
          summarise(total_hour=n()),
        filtered_df() %>% 
          group_by(borough,hour) %>%
          summarise(total_hour=n()) %>%
          summarise(total_accs=sum(total_hour)),
        by="borough") %>%
        mutate(pct=100*total_hour/total_accs) %>%
        filter(borough %in% input$bor) %>%
        ggplot(aes(x=hour,y=pct,color=borough)) + 
        #geom_smooth(se=FALSE) +
        geom_line() +
        labs(title = "Percentages of Accidents per Hour by Borough",
             subtitle = "(Events between 0:00 - 0:59 grouped at 0)",
             x="Hour", y="Percentage") + 
        scale_x_continuous(breaks = c(0:23),labels = c(0:23)) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              plot.subtitle = element_text(size = 10,face = "italic"),
              legend.position = "none")
    }
  )
  
  ##### INJURIES #####
  
  output$injury_count <- renderPlot(
    if (input$selected_option2 == "Annually") {
      filtered_df() %>% 
        group_by(borough,year) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(injuries = sum(number.of.persons.injured)) %>%
        ggplot(aes(x=year,y=injuries,color=borough)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") +
        labs(x = "Year", y = "Number of Injuries",
             title = paste("Number of Persons Injured Annually by Borough",plot_title_years())) +
        scale_x_continuous(breaks = c(2012:2023)) + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              legend.position = "bottom")
    } else if (input$selected_option2 == "Monthly") {
      filtered_df() %>% 
        group_by(borough,month) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(injuries = sum(number.of.persons.injured)) %>%
        ggplot(aes(x=month,y=injuries,color=borough)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") +
        labs(x = "Month", y = "Number of Injuries",
             title = paste("Number of Persons Injured Monthly by Borough",plot_title_years())) +
        scale_x_continuous(breaks = c(1:12),labels=month_list) + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom")   
    } else if (input$selected_option2 == "Weekly") {
      filtered_df() %>% 
        group_by(borough,wday) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(injuries = sum(number.of.persons.injured)) %>%
        ggplot(aes(x=wday,y=injuries,color=borough)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") +
        labs(x = "Day of Week", y = "Number of Injuries",
             title = paste("Number of Persons Injured per Day of Week by Borough",plot_title_years())) +
        scale_x_continuous(breaks = c(1:7),labels=day_list) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom")
    } else if (input$selected_option2 == "Hourly") {
      filtered_df() %>% 
        group_by(borough,hour) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(injuries = sum(number.of.persons.injured)) %>%
        ggplot(aes(x=hour,y=injuries,color=borough)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") +
        labs(y = "Number of Injuries", x = "Hour",
             title = paste("Number of Persons Injured per Hour by Borough",plot_title_years()),
             subtitle = "(Events between 0:00 - 0:59 grouped at 0)") +
        scale_x_continuous(breaks = c(0:23),labels = c(0:23)) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              plot.subtitle = element_text(size = 10,face = "italic"),
              legend.position = "bottom")
    }
  )
  
  output$injury_pct <- renderPlot(
    if (input$selected_option2 == "Annually") {
      filtered_df() %>% 
        group_by(borough,year) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(injuries = sum(number.of.persons.injured),
                  pop=mean(population),
                  pct_ppl_inj=100*injuries/pop) %>%
        arrange(desc(pct_ppl_inj)) %>%
        ggplot(aes(x=year,y=pct_ppl_inj,color=borough)) + 
        geom_line(stat="identity") + 
        labs(x = "Year", y = "Percent of Population Injured",
             title = paste("Percentage of Borough Population Injured Annually",plot_title_years())) +
        scale_x_continuous(breaks = c(2012:2023)) + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              legend.position = "none")
    } else if (input$selected_option2 == "Monthly") {
      filtered_df() %>% 
        group_by(borough,month) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(injuries = sum(number.of.persons.injured),
                  pop=mean(population),
                  pct_ppl_inj=100*injuries/pop) %>%
        arrange(desc(pct_ppl_inj)) %>%
        ggplot(aes(x=month,y=pct_ppl_inj,color=borough)) + 
        geom_line(stat="identity") + 
        labs(x = "Month", y = "Percent of Population Injured",
             title = paste("Percentage of Borough Population Injured Monthly",plot_title_years())) +
        scale_x_continuous(breaks = c(1:12), labels=month_list) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "none")
    } else if (input$selected_option2 == "Weekly") {
      filtered_df() %>% 
        group_by(borough,wday) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(injuries = sum(number.of.persons.injured),
                  pop=mean(population),
                  pct_ppl_inj=100*injuries/pop) %>%
        arrange(desc(pct_ppl_inj)) %>%
        ggplot(aes(x=wday,y=pct_ppl_inj,color=borough)) + 
        geom_line(stat="identity") + 
        labs(x = "Day of Week", y = "Percent of Population Injured",
             title = paste("Percentage of Borough Population Injured by Day of Week",plot_title_years())) +
        scale_x_continuous(breaks = c(1:7),labels=day_list) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "none")      
    } else if (input$selected_option2 == "Hourly") {
      filtered_df() %>% 
        group_by(borough,hour) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(injuries = sum(number.of.persons.injured),
                  pop=mean(population),
                  pct_ppl_inj=100*injuries/pop) %>%
        arrange(desc(pct_ppl_inj)) %>%
        ggplot(aes(x=hour,y=pct_ppl_inj,color=borough)) + 
        geom_line(stat="identity") + 
        labs(x = "Hour", y = "Percent of Population Injured",
             title = paste("Percentage of Borough Population Injured by Hour of Day",plot_title_years()),
             subtitle = "(Events between 0:00 - 0:59 grouped at 0)") +
        scale_x_continuous(breaks = c(0:23),labels = c(0:23)) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              plot.subtitle = element_text(size = 10,face = "italic"),
              legend.position = "none")      
    }
  )
  ##### DEATHS #####
  
  output$death_count <- renderPlot(
    if (input$selected_option3 == "Annually") {
      filtered_df() %>% 
        group_by(borough,year) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(deaths = sum(number.of.persons.killed)) %>%
        ggplot(aes(x=year,y=deaths,color=borough)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") +
        labs(x = "Year", y = "Number of Deaths",
             title = paste("Number of Persons Killed Annually by Borough",plot_title_years())) +
        scale_x_continuous(breaks = c(2012:2023)) + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              legend.position = "bottom")
    } else if (input$selected_option3 == "Monthly") {
      filtered_df() %>% 
        group_by(borough,month) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(deaths = sum(number.of.persons.killed)) %>%
        ggplot(aes(x=month,y=deaths,color=borough)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") +
        labs(x = "Month", y = "Number of Deaths",
             title = paste("Number of Persons Killed Monthly by Borough",plot_title_years())) +
        scale_x_continuous(breaks = c(1:12),labels=month_list) + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom")   
    } else if (input$selected_option3 == "Weekly") {
      filtered_df() %>% 
        group_by(borough,wday) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(deaths = sum(number.of.persons.killed)) %>%
        ggplot(aes(x=wday,y=deaths,color=borough)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") +
        labs(x = "Day of Week", y = "Number of Deaths",
             title = paste("Number of Persons Killed per Day of Week by Borough",plot_title_years())) +
        scale_x_continuous(breaks = c(1:7),labels=day_list) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom")
    } else if (input$selected_option3 == "Hourly") {
      filtered_df() %>% 
        group_by(borough,hour) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(deaths = sum(number.of.persons.killed)) %>%
        ggplot(aes(x=hour,y=deaths,color=borough)) + 
        geom_bar(aes(fill=borough), stat="identity", position = "dodge") +
        labs(y = "Number of Deaths", x = "Hour",
             title = paste("Number of Persons Killed per Hour by Borough",plot_title_years()),
             subtitle = "(Events between 0:00 - 0:59 grouped at 0)") +
        scale_x_continuous(breaks = c(0:23),labels = c(0:23)) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              plot.subtitle = element_text(size = 10,face = "italic"),
              legend.position = "bottom")
    }
  )
  
  output$death_pct <- renderPlot(
    if (input$selected_option3 == "Annually") {
      filtered_df() %>% 
        group_by(borough,year) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(deaths = sum(number.of.persons.killed),
                  pop=mean(population),
                  pct_ppl_dead=100*deaths/pop) %>%
        arrange(desc(pct_ppl_dead)) %>%
        ggplot(aes(x=year,y=pct_ppl_dead,color=borough)) + 
        geom_line(stat="identity") + 
        labs(x = "Year", y = "Percent of Population Killed",
             title = paste("Percentage of Borough Population Killed Annually",plot_title_years())) +
        scale_x_continuous(breaks = c(2012:2023)) + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              legend.position = "none")
    } else if (input$selected_option3 == "Monthly") {
      filtered_df() %>% 
        group_by(borough,month) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(deaths = sum(number.of.persons.killed),
                  pop=mean(population),
                  pct_ppl_dead=100*deaths/pop) %>%
        arrange(desc(pct_ppl_dead)) %>%
        ggplot(aes(x=month,y=pct_ppl_dead,color=borough)) + 
        geom_line(stat="identity") + 
        labs(x = "Month", y = "Percent of Population Killed",
             title = paste("Percentage of Borough Population Killed Monthly",plot_title_years())) +
        scale_x_continuous(breaks = c(1:12), labels=month_list) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "none")
    } else if (input$selected_option3 == "Weekly") {
      filtered_df() %>% 
        group_by(borough,wday) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(deaths = sum(number.of.persons.killed),
                  pop=mean(population),
                  pct_ppl_dead=100*deaths/pop) %>%
        arrange(desc(pct_ppl_dead)) %>%
        ggplot(aes(x=wday,y=pct_ppl_dead,color=borough)) + 
        geom_line(stat="identity") + 
        labs(x = "Day of Week", y = "Percent of Population Killed",
             title = paste("Percentage of Borough Population Killed by Day of Week",plot_title_years())) +
        scale_x_continuous(breaks = c(1:7),labels=day_list) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "none")      
    } else if (input$selected_option3 == "Hourly") {
      filtered_df() %>% 
        group_by(borough,hour) %>%
        filter(borough %in% input$bor) %>%
        filter(year %inrange% input$range) %>%
        summarise(deaths = sum(number.of.persons.killed),
                  pop=mean(population),
                  pct_ppl_dead=100*deaths/pop) %>%
        arrange(desc(pct_ppl_dead)) %>%
        ggplot(aes(x=hour,y=pct_ppl_dead,color=borough)) + 
        geom_line(stat="identity") + 
        labs(x = "Hour", y = "Percent of Population Killed",
             title = paste("Percentage of Borough Population Killed by Hour of Day",plot_title_years()),
             subtitle = "(Events between 0:00 - 0:59 grouped at 0)") +
        scale_x_continuous(breaks = c(0:23),labels = c(0:23)) +
        theme(plot.title = element_text(size = 16, face = "bold"),
              plot.subtitle = element_text(size = 10,face = "italic"),
              legend.position = "none")      
    }
  )
  
  
  ##### DATA TABLE #####
  output$table <- DT::renderDataTable({
    
    if (is.null(input$vehicle_category)) {
      return(datatable(df%>%
                         filter(borough %in% input$bor) %>%
                         filter(year %inrange% input$range)))
    } else {
      return(datatable(df%>%
                         filter(borough %in% input$bor) %>%
                         filter(year %inrange% input$range) %>%
                         filter(vehicle.type.code.1 %in% input$vehicle_category |
                                  vehicle.type.code.2 %in% input$vehicle_category |
                                  vehicle.type.code.3 %in% input$vehicle_category |               
                                  vehicle.type.code.4 %in% input$vehicle_category |
                                  vehicle.type.code.5 %in% input$vehicle_category)))
    }
  })
  
  ##### SHOW/HIDE + SELECT ALL (SIDEBAR #####
  observe({
    if (input$tabs %in% c("aboutme","maps")) {
      shinyjs::hide("range")
      shinyjs::hide("vehicle_category")
      shinyjs::hide("bor")
      shinyjs::hide("selectallboros")
    } else {
      shinyjs::show("range")
      shinyjs::show("vehicle_category")
      shinyjs::show("bor")
      shinyjs::show("selectallboros")
    }
  })
  
  
  observe({
    if(input$selectallboros == 0) return(NULL)
    else if (input$selectallboros%%2 == 0)
    {
      updateCheckboxGroupInput(session,"bor",choices=sort(unique(df$borough)))
    }
    else
    {
      updateCheckboxGroupInput(session,"bor",choices=sort(unique(df$borough)),
                               selected=unique(df$borough))
    }
  })
  
  ##### SELECT ALL (MAPS #####
  
  observe({
    if(input$selectallborosmap == 0) return(NULL)
    else if (input$selectallborosmap%%2 == 0)
    {
      updateCheckboxGroupInput(session,"mapboro",
                               choices=list("Bronx"="bronx", "Brooklyn"="brooklyn", 
                                            "Manhattan"="manhattan", "Queens"="queens", 
                                            "Staten Island"="staten island"))
    }
    else
    {
      updateCheckboxGroupInput(session,"mapboro",
                               choices=list("Bronx"="bronx", "Brooklyn"="brooklyn", 
                                            "Manhattan"="manhattan", "Queens"="queens", 
                                            "Staten Island"="staten island"),
                               selected=c("bronx", "brooklyn", "manhattan", 
                                          "queens", "staten island"))
    }
  })
  
  observe({
    if(input$selectallvehiclesmap == 0) return(NULL)
    else if (input$selectallvehiclesmap%%2 == 0)
    {
      updateCheckboxGroupInput(session,"mapvehicle",
                               choiceValues = c("Bicycle/Scooter", "E-Bicycle/Scooter",
                                                "Motor Bike", "Passenger Car", "Truck"),
                               choiceNames = list(
                                 HTML('<i class="fa-solid fa-person-biking" style="color: black;"></i> Bicycle/Scooter'),
                                 HTML('<i class="fa-solid fa-bicycle" style="color: red;"></i> E-Bicycle/Scooter'),
                                 HTML('<i class="fa-solid fa-motorcycle" style="color: blue;"></i> Motor Bike'),
                                 HTML('<i class="fa-solid fa-car-side" style="color: green;"></i> Passenger Car'),
                                 HTML('<i class="fa-solid fa-truck" style="color: purple;"></i> Truck')))
    }
    else
    {
      updateCheckboxGroupInput(session,"mapvehicle",
                               choiceValues = c("Bicycle/Scooter", "E-Bicycle/Scooter",
                                                "Motor Bike", "Passenger Car", "Truck"),
                               choiceNames = list(
                                 HTML('<i class="fa-solid fa-person-biking" style="color: black;"></i> Bicycle/Scooter'),
                                 HTML('<i class="fa-solid fa-bicycle" style="color: red;"></i> E-Bicycle/Scooter'),
                                 HTML('<i class="fa-solid fa-motorcycle" style="color: blue;"></i> Motor Bike'),
                                 HTML('<i class="fa-solid fa-car-side" style="color: green;"></i> Passenger Car'),
                                 HTML('<i class="fa-solid fa-truck" style="color: purple;"></i> Truck')),
                               selected=c("Bicycle/Scooter", "E-Bicycle/Scooter",
                                          "Motor Bike", "Passenger Car", "Truck"))
    }
  })
  
  
  
  
  ##### MAP DATAFRAME #####
  
  accident_map <- reactive({
    if(input$mapchoice=="Accidents") {
      return(filtered_df_map2()[,c("zip.code", "borough","year", "latitude","longitude")] %>%
               filter(between(year, input$maprange[1], input$maprange[2])) %>%
               group_by(latitude,longitude) %>%
               summarise(total_inj=n()) %>%
               arrange(desc(total_inj)) %>%
               head(input$mappointcount))
    } else if (input$mapchoice=="Injuries") {
        if (input$inj_dead_choice=="All"){
          return(filtered_df_map2()[,c("zip.code", "borough","year", "latitude","longitude","number.of.persons.injured")] %>%
                   filter(between(year, input$maprange[1], input$maprange[2])) %>%
                   group_by(latitude,longitude) %>%
                   summarise(total_inj=sum(number.of.persons.injured)) %>%
                   arrange(desc(total_inj)) %>%
                   head(input$mappointcount))
        } else if (input$inj_dead_choice=="Pedestrians"){
          return(filtered_df_map2()[,c("zip.code", "borough","year", "latitude","longitude","number.of.pedestrians.injured")] %>%
                   filter(between(year, input$maprange[1], input$maprange[2])) %>%
                   group_by(latitude,longitude) %>%
                   summarise(total_inj=sum(number.of.pedestrians.injured)) %>%
                   arrange(desc(total_inj)) %>%
                   head(input$mappointcount))
        } else if (input$inj_dead_choice=="Cyclist"){
          return(filtered_df_map2()[,c("zip.code", "borough","year", "latitude","longitude","number.of.cyclist.injured")] %>%
                   filter(between(year, input$maprange[1], input$maprange[2])) %>%
                   group_by(latitude,longitude) %>%
                   summarise(total_inj=sum(number.of.cyclist.injured)) %>%
                   arrange(desc(total_inj)) %>%
                   head(input$mappointcount))
        } else {
          return(filtered_df_map2()[,c("zip.code", "borough","year", "latitude","longitude","number.of.motorist.injured")] %>%
                   filter(between(year, input$maprange[1], input$maprange[2])) %>%
                   group_by(latitude,longitude) %>%
                   summarise(total_inj=sum(number.of.motorist.injured)) %>%
                   arrange(desc(total_inj)) %>%
                   head(input$mappointcount))
        }
    } else {
      if (input$inj_dead_choice=="All"){
        return(filtered_df_map2()[,c("zip.code", "borough","year", "latitude","longitude","number.of.persons.killed")] %>%
                 filter(between(year, input$maprange[1], input$maprange[2])) %>%
                 group_by(latitude,longitude) %>%
                 summarise(total_inj=sum(number.of.persons.killed)) %>%
                 arrange(desc(total_inj)) %>%
                 head(input$mappointcount))
      } else if (input$inj_dead_choice=="Pedestrians"){
        return(filtered_df_map2()[,c("zip.code", "borough","year", "latitude","longitude","number.of.pedestrians.killed")] %>%
                 filter(between(year, input$maprange[1], input$maprange[2])) %>%
                 group_by(latitude,longitude) %>%
                 summarise(total_inj=sum(number.of.pedestrians.killed)) %>%
                 arrange(desc(total_inj)) %>%
                 head(input$mappointcount))
      } else if (input$inj_dead_choice=="Cyclist"){
        return(filtered_df_map2()[,c("zip.code", "borough","year", "latitude","longitude","number.of.cyclist.killed")] %>%
                 filter(between(year, input$maprange[1], input$maprange[2])) %>%
                 group_by(latitude,longitude) %>%
                 summarise(total_inj=sum(number.of.cyclist.killed)) %>%
                 arrange(desc(total_inj)) %>%
                 head(input$mappointcount))
      } else {
        return(filtered_df_map2()[,c("zip.code", "borough","year", "latitude","longitude","number.of.motorist.killed")] %>%
                 filter(between(year, input$maprange[1], input$maprange[2])) %>%
                 group_by(latitude,longitude) %>%
                 summarise(total_inj=sum(number.of.motorist.killed)) %>%
                 arrange(desc(total_inj)) %>%
                 head(input$mappointcount))
      }
    }
  })
  
  ##### MAP FORMATTING #####
  
  color_palette <- reactive({
    if (is.null(input$mapvehicle)) {
      colorNumeric(palette = c("navy", "navy"), domain = accident_map()$total_inj)
    } else if (length(input$mapvehicle)==1){
      if (input$mapvehicle=="Bicycle/Scooter") {
        colorNumeric(palette = c("gray30", "black"), domain = accident_map()$total_inj)
      } else if (input$mapvehicle=="E-Bicycle/Scooter") {
        colorNumeric(palette = c("coral", "firebrick1"), domain = accident_map()$total_inj)
      } else if (input$mapvehicle=="Motor Bike") {
        colorNumeric(palette = c("dodgerblue", "blue"), domain = accident_map()$total_inj)
      } else if (input$mapvehicle=="Passenger Car") {
        colorNumeric(palette = c("green2", "green4"), domain = accident_map()$total_inj)
      } else if (input$mapvehicle=="Truck") {
        colorNumeric(palette = c("mediumpurple1", "darkorchid4"), domain = accident_map()$total_inj)
      } 
    } else {
      colorNumeric(palette = c("navy", "navy"), domain = accident_map()$total_inj)
    }
  })
  
  scaled_radius <- reactive({
    scaled_value <- (accident_map()$total_inj - min(accident_map()$total_inj)) / (max(accident_map()$total_inj) - min(accident_map()$total_inj))
    5 + scaled_value * (15 - 5)
  })
  
  output$mymap2 <- renderLeaflet({
    leaflet(data=accident_map()) %>%
      addTiles() %>%
      # addProviderTiles("Stamen.Toner") %>%
      addCircleMarkers(~longitude, ~latitude, popup= ~total_inj, label = ~total_inj,
                       color = ~color_palette()(total_inj),
                       radius = ~scaled_radius(),
                       stroke = FALSE, fillOpacity = 0.8)
  })
  
  output$map_count_title <- renderText({
    paste("Top", as.character(input$mappointcount),
          "Locations Where the Most", as.character(input$mapchoice), "Occur:")
  })
  
  output$map_subtitle <- renderText({
    paste("Hover to see number of", tolower(as.character(input$mapchoice)))
  })
  
  ##### OBSERVE FOR CHECKBOX ICONS #####
  observe({
    selected_items <- input$mapvehicle
    selected_names <- names(input$mapvehicle)
    cat("Selected items:", paste(selected_names, collapse = ", "), "\n")
  })
  
  ##### ZIPCODE DROPDOWN REACTIVE #####
  properzipsdf <- reactive({
    if (is.null(input$mapboro)) {
      return(df[df$zip.code!=0,])
    } else {
      return(df[df$borough %in% input$mapboro & df$zip.code!=0,])
    }
  })
  
  output$mapzipcode <- renderUI({
    selectInput(
      inputId = "mapzipcode",
      label = "Specify Zip Codes to Include:",
      choices = sort(unique(properzipsdf()$zip.code)),
      multiple = TRUE
    )
  })
  
}



