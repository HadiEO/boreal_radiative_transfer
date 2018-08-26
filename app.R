# library(shiny)
# library(dplyr)
# library(tidyverse)
# library(googleway)
# library(sp)
# library(leaflet)
# library(htmltools)
# library(htmlwidgets)
# library(plotwidgets)
# library(bfast)
# library(bfastSpatial)
# library(bfastPlot)
# library(lubridate)
# library(DBI)
# library(RSQLite)


# Below not run -----------------------------------------------------------

# # Make app data
# S2.BOA.w12.5 <- read_csv2("data/image_extracted_spectra/S2_BOA_w12_5.csv")
# gaps <- read_csv2("results/gaps_and_testNilson.csv")
# temp <- left_join(gaps, S2.BOA.w12.5, by = "ID")
# # Add latlon coordinates
# utmPoints <- SpatialPointsDataFrame(temp[, c("UTM34_E", "UTM34_N")],
#                                     proj4string = CRS("+init=epsg:32634"),
#                                     data = temp[, "ID"])
# latlongPoints <- spTransform(utmPoints, CRS("+init=epsg:4326"))
# latlongDf <- tibble(ID = latlongPoints$ID, lat = coordinates(latlongPoints)[,2], lon = coordinates(latlongPoints)[,1])
# temp <- left_join(temp, latlongDf, by = "ID")
# write_rds(temp, "app_data.rds")

# **************************************************************************************************************************
# The example app data ("app_data.rds") is in 
# C:\LocalUserData\User-data\hadi1\PHD_RESEARCH\STUDY_V_SENSITIVITY\Rproj_SENSITIVITY
# **************************************************************************************************************************
# UI ----------------------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    column(width = 4, wellPanel(h4("Field data interactive exploratory tool"), 
                                tags$p(em("by ", a("Hadi", href = "https://people.aalto.fi/new/hadi.hadi"))))),
    column(width = 4,  wellPanel(fileInput(inputId = 'data', label = 'rds file', multiple = FALSE, accept = '.rds')))),
  fluidRow(
      column(width = 2, wellPanel(
        uiOutput("ui_var1_a"),
        uiOutput("ui_var2_a"),
        uiOutput("ui_varGroup_a"))),
      column(width = 1, wellPanel(
        numericInput(inputId = "xmin_a", label = "xmin", value = NULL),
        numericInput(inputId = "xmax_a", label = "xmax", value = NULL),
        numericInput(inputId = "ymin_a", label = "ymin", value = NULL),
        numericInput(inputId = "ymax_a", label = "ymin", value = NULL))),
      column(width = 8, offset = 1,
             tabsetPanel(
               tabPanel('plot', plotOutput("plot_a", brush = "plot_a_brush", hover = "plot_a_hover"), 
                        verbatimTextOutput("info")),
               tabPanel('table vars', uiOutput("ui_varTable_a")),
               tabPanel('table', tableOutput('table_a')),
               tabPanel('table of selected points', tableOutput('table_a_selected')),
               tabPanel('map', leafletOutput('map_esri'), google_mapOutput("map_google"),
                        actionButton("update_google", "Update google map"))
             )
            )
    ),
  
  fluidRow(
      column(width = 2, wellPanel(
        uiOutput("ui_var1_b"),
        uiOutput("ui_var2_b"),
        uiOutput("ui_varGroup_b"))),
      column(width = 1, wellPanel(
        numericInput(inputId = "xmin_b", label = "xmin", value = NULL),
        numericInput(inputId = "xmax_b", label = "xmax", value = NULL),
        numericInput(inputId = "ymin_b", label = "ymin", value = NULL),
        numericInput(inputId = "ymax_b", label = "ymin", value = NULL))),
      column(width = 5, offset = 1,
             wellPanel(plotOutput("plot_b")))
      
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Reactive to read data from fileInput
  data <- reactive({
    data <- input$data
  if (is.null(data))
    return(NULL)
  data <- read_rds(data$datapath)
  })

  # Generate ui to select input column names in data frame
  # Panel a
  output$ui_var1_a <- renderUI({
    data <- data()
    selectInput("var1_a",
                label = "Var x",
                choices = as.list(names(data)))
  })
  
  output$ui_var2_a <- renderUI({
    data <- data()
    selectInput("var2_a",
                label = "Var y",
                choices = as.list(names(data)))
  })
  
  output$ui_varGroup_a <- renderUI({
    data <- data()
    selectInput("varGroup_a",
                label = "Group Var",
                choices = as.list(names(data)))
  })
  
  # Panel b
  output$ui_var1_b <- renderUI({
    data <- data()
    selectInput("var1_b",
                label = "Var x",
                choices = as.list(names(data)))
  })
  
  output$ui_var2_b <- renderUI({
    data <- data()
    selectInput("var2_b",
                label = "Var y",
                choices = as.list(names(data)))
  })
  
  output$ui_varGroup_b <- renderUI({
    data <- data()
    selectInput("varGroup_b",
                label = "Group Var",
                choices = as.list(names(data)))
  })
  
  
  # Plot a
  output$plot_a <- renderPlot({
    data <- data()
    r_a <- round(cor(data[, input$var1_a], data[, input$var2_a]), 3)
    
    ggplot(data = data, aes_string(x = input$var1_a, y = input$var2_a, col = input$varGroup_a, shape = input$varGroup_a)) +
      geom_point(size = 3) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      scale_x_continuous(limits = c(input$xmin_a, input$xmax_a)) +
      scale_y_continuous(limits = c(input$ymin_a, input$ymax_a)) +
    ggtitle(str_c("r = ", r_a))
      
  })

  # Plot b
  output$plot_b <- renderPlot({
    data <- data()
    r_b <- round(cor(data[, input$var1_b], data[, input$var2_b]), 3)
    
    ggplot(data = data, aes_string(x = input$var1_b, y = input$var2_b, col = input$varGroup_b, shape = input$varGroup_b)) +
      geom_point(size = 3) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      scale_x_continuous(limits = c(input$xmin_b, input$xmax_b)) +
      scale_y_continuous(limits = c(input$ymin_b, input$ymax_b)) +
      ggtitle(str_c("r = ", r_b))
  })
  
  
  # Table var
  output$ui_varTable_a <- renderUI({
    data <- data()
    checkboxGroupInput("varTable_a", "Vars to show in table:",
                       choiceNames = as.list(names(data)),
                       choiceValues = as.list(names(data)))
  })
  
  
  # Table a
  output$table_a <- renderTable({
    data <- data()
    data[, input$varTable_a]
  })
  
  # Table a of selected (brushed) data point
  output$table_a_selected <- renderTable({
    brushedPoints(data(), input$plot_a_brush)[, input$varTable_a]
  })
  
  # ID of selected (hovered) data point
  output$info <- renderPrint({
    hovered <- nearPoints(data(), input$plot_a_hover)[, "ID"]
    unlist(hovered)
  })
  

  # ESRI world imagery map
  output$map_esri <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = brushedPoints(data(), input$plot_a_brush),
                 lng = ~lon, lat = ~lat)
  })
  
  # Google map 
  output$map_google <- renderGoogle_map( {
    google_map(key = 'AIzaSyCcM_lSFnp4H1n4yytVOdABTAt-dir_etw',
               data = brushedPoints(data(), input$plot_a_brush),
               search_box = TRUE) %>%
      add_markers(lat = 'lat', lon = 'lon') })
  
  observeEvent(input$update_google, {
    if(input$update_google > 0) {       # Maybe this condition is unncessary?
      google_map_update(map_id = "map_google") %>% 
        clear_markers() %>% 
        add_markers(data = brushedPoints(data(), input$plot_a_brush),
                    lat = 'lat', lon = 'lon')
    }
  })

  
}



# ShinyApp ----------------------------------------------------------------

shinyApp(ui = ui, server = server)