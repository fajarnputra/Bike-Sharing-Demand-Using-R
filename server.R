# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
#  cities_max_bike <- aggregate(city_weather_bike_df$BIKE_PREDICTION, by = list(city_weather_bike_df$CITY_ASCII), max) 
  cities_max_bike <- city_weather_bike_df %>% group_by(CITY_ASCII) %>% filter(BIKE_PREDICTION == max(BIKE_PREDICTION))
  
  

  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    
    bike_filter <- reactive({
      cities_max_bike %>% filter(CITY_ASCII == input$city_dropdown)
    })
    bike_filter <- bike_filter()
    
    weather_filter <- reactive({
      city_weather_bike_df %>% filter(CITY_ASCII == input$city_dropdown) %>% mutate(row_num = row_number())
    })
    weather_filter <- weather_filter()
    
    
    # If All was selected from dropdown, then render a leaflet map with circle markers
    # and popup weather LABEL for all five cities
    
    if(input$city_dropdown == 'All') {
      #Render the city overview map
      
      # Then render output plots with an id defined in ui.R
      output$city_bike_map <- renderLeaflet({
        # Complete this function to render a leaflet map
        leaflet(data = cities_max_bike) %>% 
          addTiles() %>%
          #addMarkers(popup = ~htmlEscape(LABEL)) %>%
          addPopups(lng = ~LNG, lat = ~LAT, cities_max_bike$LABEL,
                    options = popupOptions(closeButton = FALSE)) %>%
          addCircleMarkers(lng = ~LNG, lat = ~LAT,
                           color = color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL),
                           radius = ifelse(cities_max_bike$BIKE_PREDICTION_LEVEL == 'small',6,
                                           ifelse(cities_max_bike$BIKE_PREDICTION_LEVEL == 'medium', 10, 12) ),
                           stroke = FALSE, fillOpacity = 0.5)
      })
      
      
    }
    else {
      #Render the specific city map
      
      # If just one specific city was selected, then render a leaflet map with one marker
      # on the map and a popup with DETAILED_LABEL displayed
      
      # Then render output plots with an id defined in ui.R
      output$city_bike_map <- renderLeaflet({
        # Complete this function to render a leaflet map
        leaflet(data = bike_filter) %>% 
          addTiles() %>%
          #addMarkers(popup = ~htmlEscape(LABEL)) %>%
          addPopups(lng = ~LNG, lat = ~LAT, ~DETAILED_LABEL) %>%
          addMarkers(lng = ~LNG, lat = ~LAT)
      })
      
      output$temp_line <- renderPlot ({
        p <- ggplot(weather_filter, aes_string(x = weather_filter$row_num, y = weather_filter$TEMPERATURE),
                    label = weather_filter$TEMPERATURE) +
          labs(y="Temperature (Celcius)", title = 'Temperature Chart', x = "Time (3 hours ahead)") #+  # labels
          # theme(
          #   # legend.position = "bottom",
          #   # axis.text.x = element_text(angle = 45, hjust = 1)
          # )
        
        p + geom_line(color = "yellow") + geom_point() + geom_text(label = weather_filter$TEMPERATURE, size=3, 
                                                                   position=position_jitter(width=1,height=1), check_overlap = TRUE)
        
        
      })
      
      output$bike_line <- renderPlot ({
        p <- ggplot(weather_filter, aes_string(x = as.POSIXct(weather_filter$FORECASTDATETIME), y = weather_filter$BIKE_PREDICTION),
                    label = weather_filter$BIKE_PREDICTION) +
          labs(y="Predicted Bike Count", title = 'Bike Chart', x = "Time (3 hours ahead)") #+  # labels
        # theme(
        #   # legend.position = "bottom",
        #   # axis.text.x = element_text(angle = 45, hjust = 1)
        # )
        
        p + geom_line(color = "green", linetype = "dashed") + geom_point() + geom_text(label = weather_filter$BIKE_PREDICTION, size=3, 
                                                                   position=position_jitter(width=1,height=1), check_overlap = TRUE)
        
        
      }) 
      
      
      output$bike_date_output <- renderText({
        paste0("Time: ", as.character(input$plot_click$x), ' BikeCountPred = ', input$plot_click$y)
      })
      
      
      
      output$humidity_pred_chart <- renderPlot ({
        p <- ggplot(weather_filter, aes_string(x = weather_filter$HUMIDITY, y = weather_filter$BIKE_PREDICTION),
                    label = weather_filter$BIKE_PREDICTION) +
          labs(y="BIKE_PREDICTION", title = 'Bike - Humidity Chart', x = "HUMIDITY") #+  # labels
        # theme(
        #   # legend.position = "bottom",
        #   # axis.text.x = element_text(angle = 45, hjust = 1)
        # )
        
        p + geom_point() + geom_smooth(method = 'lm', formula = y ~ poly(x, 4), color = 'red')
        
        
      }) 
      
      
      
    } 
  })
  
  

  

  
})
