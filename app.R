library(sf)
library(terra)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(markdown)

bp <- 'www/bp_datasets.gpkg'
#bp <- 'H:/Shared drives/Data/bp_datasets.gpkg'
limits <- st_read(bp) %>% st_transform(4326)

ui = dashboardPage(skin="blue",
    dashboardHeader(title = "BEACONs Geopackage Creator", titleWidth=320),
    dashboardSidebar(
        sidebarMenu(id="tabs",
            menuItem("Welcome!", tabName = "overview", icon = icon("th")),
            hr(),
            menuItem("1. Select study area", tabName = "select", icon = icon("th")),
            #checkboxInput("cross", label = 'Cross-boundary study area', value = F),
            fileInput(inputId = "upload_poly", label = "Upload study area:", multiple = FALSE, accept=".gpkg"),
            hr(),
            menuItem("2. Create geopackage", tabName = "data", icon = icon("th")),
            actionButton("goButton", "Create geopackage"),
            numericInput("minYear", label = "Oldest fires to include:", value = 1980),
            hr(),
            menuItem("3. Download geopackage", tabName = "download", icon = icon("th")),
            div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download geopackage")),
            hr()
        )
    ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(".skin-blue .sidebar a { color: #8a8a8a; }")),
    tags$style(type = "text/css", "#map1 {height: calc(100vh - 164px) !important;}"),
    tabItems(
      tabItem(tabName="overview",
            fluidRow(
                #box(title = "Mapview", leafletOutput("map1", height=750) %>% withSpinner(), width=12),
                tabBox(id = "one", width="12",
                    tabPanel("Mapview", leafletOutput("map1") %>% withSpinner()),
                    tabPanel("Overview", includeMarkdown("docs/overview.md")),
                    tabPanel("Datasets", includeMarkdown("docs/datasets.md"))
                )
            )
        )
    )
  )
)

server = function(input, output, session) {

  ##############################################################################
  # Read input data
  ##############################################################################
  line <- reactive({
    if (input$goButton) {
      aoi <- bnd() %>% st_transform(3578)
      st_read(bp, 'sd_line') %>%
        st_intersection(aoi) %>%
        st_cast('MULTILINESTRING')
    }
  })

  poly <- reactive({
    if (input$goButton) {
      aoi <- bnd() %>% st_transform(3578)
      vect(bp, 'sd_poly') %>%
        st_as_sf() %>%
        st_cast('MULTIPOLYGON') %>%
        st_intersection(aoi) %>%
        st_cast('MULTIPOLYGON')
    }
  })
  
   fires <- reactive({
    if (input$goButton) {
      aoi <- bnd() %>% st_transform(3578)
        x=vect(bp, 'fires') %>%
          st_as_sf() %>%
          st_cast('MULTIPOLYGON') %>%
          filter(YEAR > input$minYear) %>%
          st_intersection(aoi) %>%
          st_cast('MULTIPOLYGON')
    }
  })

  ifl_2000 <- reactive({
    if (input$goButton) {
      aoi <- bnd() %>% st_transform(3578)
        st_read(bp, 'ifl_2000') %>%
          st_intersection(aoi)
    }
  })
   
  ifl_2020 <- reactive({
    if (input$goButton) {
      aoi <- bnd() %>% st_transform(3578)
        st_read(bp, 'ifl_2020') %>%
          st_intersection(aoi)
    }
  })

  
  ##############################################################################
  # Uploaded data
  ##############################################################################
  bnd <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if (ext=="gpkg") {
      aoi <- st_read(file, st_layers(file)[1]$name, quiet=T) %>% st_transform(3578)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })


  ##############################################################################
  # View initial set of maps
  ##############################################################################
  output$map1 <- renderLeaflet({
    if (is.null(input$upload_poly)) {
      m <- leaflet(limits) %>%
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
        addPolygons(data=limits, color='black', fill=F, weight=1, group="Database limits") %>%
        addLayersControl(position = "topright",
          baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
          overlayGroups = c("Database limits"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c(""))
      } else {
        region <- bnd() %>% st_transform(4326)
        map_bounds <- region %>% st_bbox() %>% as.character()
        m <- leaflet(region) %>%
          fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>%
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
          addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
          addPolygons(data=limits, color='black', fill=F, weight=1, group="Database limits") %>%
          addPolygons(data=region, color='blue', fill=F, weight=2, group="Study area") %>%
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
            overlayGroups = c("Database limits", "Study area"),
            options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c(""))
      }
    m
  })


  ##############################################################################
  # Update map with intactness/footprint
  ##############################################################################
  observe({
    if (input$goButton) {
      aoi <- bnd() %>% st_transform(3578)
      sd_line <- line() %>% st_transform(4326)
      sd_poly <- poly() %>% st_transform(4326)
      fires <- fires() %>%  st_transform(4326)
      ifl_2000 <- ifl_2000() %>% st_transform(4326)
      ifl_2020 <- ifl_2020() %>% st_transform(4326)
      
      proxy <- leafletProxy("map1") %>%
      #clearGroup('Fires') %>%
      addPolylines(data=sd_line, color='red', weight=2, group="Linear disturbances") %>%
      addPolygons(data=sd_poly, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Areal disturbances") %>%
      addPolygons(data=fires,fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group='Fires') %>%
      addPolygons(data=ifl_2000, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intactness 2000") %>%
      addPolygons(data=ifl_2020, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness 2020") %>%
      addLayersControl(position = "topright",
        baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
        overlayGroups = c("Database limits","Study area", "Linear disturbances", "Areal disturbances", "Fires","Intactness 2000", "Intactness 2020"),
        options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Database limits", "Intactness 2000", "Intactness 2020"))
    }
  })

  ##############################################################################
  # Save features to a geopackage
  ##############################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("disturbances-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
        st_write(bnd(), dsn=file, layer='fda')
        if (input$goButton) {
          st_write(line(), dsn=file, layer='sd_line', append=TRUE)
          st_write(poly(), dsn=file, layer='sd_poly', append=TRUE)
          st_write(fires(), dsn=file, layer='fires', append=TRUE)
          st_write(ifl_2000(), dsn=file, layer='ifl_2000', append=TRUE)
          st_write(ifl_2020(), dsn=file, layer='ifl_2020', append=TRUE)
      }
    }
  )

  #session$onSessionEnded(function() {
  #  stopApp()
  #})

}
shinyApp(ui, server)
