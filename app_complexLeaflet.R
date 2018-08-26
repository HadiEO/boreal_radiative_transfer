library(shiny)
library(shinydashboard)
library(shinyFiles)

library(dplyr)
library(tidyverse)
library(googleway)
library(sp)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(plotwidgets)
library(bfast)
library(bfastSpatial)
library(bfastPlot)
library(lubridate)
library(DBI)
library(RSQLite)



# tags$hr() for "line spacing"
# tags$p() for empty line
# tags$h4() for bigger text
# 

# Check https://stackoverflow.com/questions/39196743/interactive-directory-input-in-shiny-app-r
# Or convert to json?


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Complex leaflet"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput(inputId = 'vhsrs', label = 'GeoTiff files', multiple = TRUE, accept = ".tif"),
      fileInput(inputId = 'colour_maps', label = 'Colourmap files (.clr)', multiple = TRUE, accept = ".clr"),
      textInput(inputId = "vhsrs_dates", label = "Type the dates of VHSR (separated by ';')"),
      fileInput(inputId = "sample_pixels", label = "Pixels (polygons) selected for interpretation (zipped shp)", accept = ".zip"),
      # tags$h4("Pixels (polygons) selected for interpretation"),
      # shinyFilesButton("sample_pixels", "File Select", "Please select a file (.shp)", multiple = FALSE)),    # using shinyFiles doesn't work
      actionButton(inputId = "go", label = "Run")),
    mainPanel(
      leafletOutput('map_test')
    )

    )
  )

# Server ------------------------------------------------------------------

server <- function(input, output, session){  
  
  prep_vhsr <- reactive({
    # Read and process the input VHSRs
    if (is.null(input$vhsrs)) return(NULL)
    vhsr <- list()
    for(i in 1:nrow(input$vhsrs)) {
      vhsr[[i]] <- raster(input$vhsrs[[i, "datapath"]])
    }
    return(vhsr)
  })
  
  prep_clr <- reactive({
    # Read and process the input colourmaps 
    if (is.null(input$colour_maps)) return(NULL)
    clr <- list()
    for(i in 1:nrow(input$colour_maps)) {
    clr[[i]] <- read_delim(input$colour_maps[[i, "datapath"]],
                           delim = " ", col_names = c("bitValue", "R", "G", "B"))
    }
    return(clr)
  })
    
 prep_pal <- reactive({
   # Convert colourmaps to pallette
   if (is.null(input$colour_maps)) return(NULL)
   pal <- list()
   for(i in 1:nrow(input$colour_maps)) {
     pal[[i]] <- prep_clr()[[i]] %>% dplyr::select(-bitValue) %>% t() %>%
       plotwidgets::rgb2col()
   }
   return(pal)
 })
    
 prep_date <- reactive({
   # Read and process the dates string (textInput)
   if (is.null(input$vhsrs_dates))   return(NULL)
   date <- str_split(input$vhsrs_dates, ";")
   date <- as.list(unlist(date))
   return(date)
 })

 prep_pixels <- reactive({
   # Read and process sample pixels (polygons)
   if (is.null(input$sample_pixels$datapath))   return(NULL)
   # Function to get layer name from dsn
   layerFromDsn <- function(x) {
     temp <- str_split(x, "/")
     temp <- as.list(unlist(temp))
     temp <- temp[[length(temp)]]
     out <- str_split(temp, "\\.")[[1]][1]
     return(out)
   }
   # Method 1: Use shinyFileChoose to select directly the path (not moving file to temporary path)
   # Update 20180125: doesn't work! Package author suggests there may be issues following shiny regression
   # volumes <- c("Sample vector data" = "C:/LocalUserData/User-data/hadi1/learn_shiny/learn_leaflet/data")
   # volumes <- c('Sample vector data' = 'C:/LocalUserData/User-data/hadi1/learn_shiny/learn_leaflet/data')
   # shinyFileChoose(input, "sample_pixels", 
   #                 roots = volumes,
   #                 filetypes = c("shp"),
   #                 session = session,
   #                 restrictions = system.file(package = "base"))
   # 
   # shp_path <- parseFilePaths(volumes, input$sample_pixels)
  
   # Method 2: upload zipped shapefile
   unzipped <- unzip(input$sample_pixels$datapath)
   # browser()                                                                            # ********
   shp <- unzipped[5] # Need to figure out how to automatically select the .shp file
   pixels <- readOGR(dsn = shp,
                     layer = layerFromDsn(shp))
   pixels_unproj <- spTransform(pixels, CRS("+init=epsg:4326")) # Attributes are "Id" and "Visual"
   return(pixels_unproj)
 })
  
 
 # Make map
  observeEvent(input$go, {
    
    output$map_test <- renderLeaflet({
      
      # Make map widget
      leaflet_output <- leaflet() %>%
        # Base groups
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(noWrap = TRUE), 
                         group = "ESRI World Imagery")
      
      # Loop to add the multiple VHSR images
      
      for (i in 1:length(prep_vhsr())) {
        leaflet_output <- addRasterImage(leaflet_output,
                                         x = prep_vhsr()[[i]],
                                         colors = prep_pal()[[i]],
                                         group = prep_date()[[i]],
                                         maxBytes = Inf)
      }
      
      
      
      # Add polygon
      leaflet_output <- leaflet_output %>%
        addPolygons(data = prep_pixels(), layerId = ~Id,
                    color = "#edf8b1", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0,
                    fillColor = "null",
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    group = "Selected Landsat pixels")
      
      
      
      # Add layer controls, need to loop to specify group names.
      # For now, implement conditionals if there are 3, 4, or 5 vhsr images
      if (length(prep_vhsr()) == 3) {
        leaflet_output <- addLayersControl(leaflet_output,
                                           baseGroups = c("OSM (default)", "ESRI World Imagery"),
                                           overlayGroups = c(prep_date()[[1]], prep_date()[[2]], prep_date()[[3]], "Selected Landsat pixels"),                     # "Digital Globe"
                                           options = layersControlOptions(collapsed = FALSE))
      } else if(length(prep_vhsr()) == 4) {
        leaflet_output <- addLayersControl(leaflet_output,
                                           baseGroups = c("OSM (default)", "ESRI World Imagery"),
                                           overlayGroups = c(prep_date()[[1]], prep_date()[[2]], prep_date()[[3]], prep_date()[[4]], "Selected Landsat pixels"),                     # "Digital Globe"
                                           options = layersControlOptions(collapsed = FALSE))
      } else if(length(prep_vhsr()) == 5) {
        leaflet_output <- addLayersControl(leaflet_output,
                                           baseGroups = c("OSM (default)", "ESRI World Imagery"),
                                           overlayGroups = c(prep_date()[[1]], prep_date()[[2]], prep_date()[[3]], prep_date()[[4]], prep_date()[[5]], "Selected Landsat pixels"),                     # "Digital Globe"
                                           options = layersControlOptions(collapsed = FALSE))
      } else {
        stop("Number of VHSR images should be either 3, 4, or 5 images")
      }
      
      
      # Finally display the map
      leaflet_output
      
    })
    
  })
 }



# ShinyApp ----------------------------------------------------------------

shinyApp(ui, server)
