library(shiny)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(RColorBrewer)
library(maps)
library(maptools)
library(rgdal)

shinyServer(function(input, output) {

  dataset <- read.csv("data/data.csv")
  # Convert value from integer to numeric
  dataset$Value <- as.numeric(dataset$Value)
  # Convert year values to columns
  data_merge_state <- spread(dataset, year, Value)
  # Change column names of data to be merged
  colnames(data_merge_state) <- c("state_name", paste0("Value_",   colnames(data_merge_state[2:length(data_merge_state)])))
  # Read shapefile
  states <- readOGR("shp/cb_2014_us_state_20m.shp", layer = "cb_2014_us_state_20m", verbose = FALSE)
  # Convert state names to uppercase
  states@data$state_name <- toupper(states@data$NAME)
  # Remove non-contiguous states
  states <- subset(states, states@data$state_name != "DISTRICT OF COLUMBIA" &
                     states@data$state_name != "ALASKA" &
                     states@data$state_name != "HAWAII" &
                     states@data$state_name != "PUERTO RICO")
  # Merge data with shapefile
  states@data <- merge(states@data, data_merge_state, by = "state_name" )
  # Check NAs in Value and change to zeros
  states@data[is.na(states@data)] <- 0

  # Let user download the data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset", '.csv', sep = '')
    },
    content = function(file) {
      write.csv(dataset, file)
    }
  )

  # Create data for plot
  data_plot <- reactive({
    subset(dataset, dataset$state_name %in% input$stateInput & (year >= input$dateRange[1] & year <= input$dateRange[2]))
  })


  # Create data for calculations
  data_user <- reactive({
    subset(dataset, dataset$state_name %in% input$stateInput)
  })

  # Create index ------> THIS IS WHERE YOU CHOOSE THE YEAR (1982)
  Index <- reactive({
    data.frame(state = data_user()$state_name, year = data_user()$year, Index = (data_user()$Value / data_user()[data_user()$year == "1982", "Value"]) * 100)
  })

  # Let user download the index
  output$downloadIndex <- downloadHandler(
    filename = function() {
      paste("index", '.csv', sep = '')
    },
    content = function(file) {
      write.csv(Index(), file)
    }
  )

  # Let user download the index for state
  output$downloadState <- downloadHandler(
    filename = function() {
      paste("state", '.csv', sep = '')
    },
    content = function(file) {
      write.csv(data_user(), file)
    }
  )
  # Calculate results from user input
  # PART 1
  IndexPast1 <- reactive({
    subset(Index(), year == as.numeric(input$past1))[[3]]
  })

  IndexCurrent1 <- reactive({
    subset(Index(), year == as.numeric(input$current1))[[3]]
  })

  output$IndexPast1    <- renderPrint({IndexPast1()})
  output$IndexCurrent1 <- renderPrint({IndexCurrent1()})
  output$estpast <- renderPrint({input$valuecurrent * (IndexPast1()/IndexCurrent1())})

  # Calculate results from user input
  # PART 2

  IndexPast2 <- reactive({
    subset(Index(), year == as.numeric(input$past2))[[3]]
  })

  IndexCurrent2 <- reactive({
    subset(Index(), year == as.numeric(input$current2))[[3]]
  })

  output$IndexPast2    <- renderPrint({IndexPast2()})
  output$IndexCurrent2 <- renderPrint({IndexCurrent2()})
  output$estcurrent <- renderPrint({input$valuepast * (IndexCurrent2()/IndexPast2())})


  # Create Plot
  output$indexPlot <- renderPlot({

    ggplot(data_plot(), aes(year, Value)) + geom_line(size = 2, colour = "red") +
      xlab("") + ylab("Dollars per Acres") + theme_economist() +
      theme(text = element_text(size = 20), axis.title.y = element_text(margin = margin(0,20,0,0)))

  })




  # CREATE MAP -----------------------------------------------------------------

  # User choice for year
  var <- reactive({
    paste0("Value_", input$mapyear)
  })

  # User choice for color
  colorpal <- reactive({
    colorNumeric(input$colors, states@data[,var()])
  })

  # Create Blank Map
  output$map <- renderLeaflet({
    leaflet(states) %>%
      setView(lng = -98.35, lat = 39.50, zoom = 4) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor = "Purples",
        color = ~colorQuantile("YlOrRd",  states@data[,var()])( states@data[,var()])
      )
  })

  # Add polygons
  observe({
    pal <- colorpal()

    leafletProxy("map", data = states) %>%
      clearShapes() %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor = ~pal(states@data[,var()]),
        color = ~colorQuantile("YlOrRd",  states@data[,var()])( states@data[,var()])
      )
  })

  # Add legend
  observe({
    proxy <- leafletProxy("map", data = states)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright", title = "Farmland Value",
                          pal = pal, values = ~states@data[,var()]
      )
    }
  })



# ENDS HERE
})

