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

  # DATA MANIPULATION ----------------------------------------------------------

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

  # LET USER DOWNLOAD DATA SETS ------------------------------------------------
  # Create data for download
  data_user_down <- reactive({
    subset(dataset, dataset$state_name %in% input$stateInputDown)
  })

  Index <- reactive({
    data.frame(state = data_user_down()$state_name, year = data_user_down()$year, Index = (data_user_down()$Value / data_user_down()[data_user_down()$year == input$Index, "Value"]) * 100)
  })


  # Let user download the data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset", '.csv', sep = '')
    },
    content = function(file) {
      write.csv(dataset, file)
    }
  )


  # Let user download the index for state
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
      write.csv(data_user_down(), file)
    }
  )

  # CALCULATIONS ---------------------------------------------------------------
  # Create data for calculations
  data_user_calc1 <- reactive({
    subset(dataset, dataset$state_name %in% input$stateInput1)
  })
  data_user_calc2 <- reactive({
    subset(dataset, dataset$state_name %in% input$stateInput2)
  })

  # Calculate results from user input
  # PART 1
  # Create index
  Index1 <- reactive({
    data.frame(state = data_user_calc1()$state_name, year = data_user_calc1()$year, Index = (data_user_calc1()$Value / data_user_calc1()[data_user_calc1()$year == input$indexYear1, "Value"]) * 100)
  })

  IndexPast1 <- reactive({
    subset(Index1(), year == as.numeric(input$past1))[[3]]
  })

  IndexCurrent1 <- reactive({
    subset(Index1(), year == as.numeric(input$current1))[[3]]
  })

  output$IndexPast1    <- renderPrint({IndexPast1()})
  output$IndexCurrent1 <- renderPrint({IndexCurrent1()})
  output$estpast <- renderPrint({input$valuecurrent1 * (IndexPast1()/IndexCurrent1())})


  # PART 2
  # Create index
  Index2 <- reactive({
    data.frame(state = data_user_calc2()$state_name, year = data_user_calc2()$year, Index = (data_user_calc2()$Value / data_user_calc2()[data_user_calc2()$year == input$indexYear2, "Value"]) * 100)
  })

  IndexPast2 <- reactive({
    subset(Index2(), year == as.numeric(input$past2))[[3]]
  })

  IndexCurrent2 <- reactive({
    subset(Index2(), year == as.numeric(input$current2))[[3]]
  })

  output$IndexPast2    <- renderPrint({IndexPast2()})
  output$IndexCurrent2 <- renderPrint({IndexCurrent2()})
  output$estcurrent <- renderPrint({input$valuepast * (IndexCurrent2()/IndexPast2())})

  # TIME SERIES PLOT -----------------------------------------------------------
  # Create data for plot
  data_plot <- reactive({
    subset(dataset, dataset$state_name %in% input$stateInput & (year >= input$dateRange[1] & year <= input$dateRange[2]))
  })

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

