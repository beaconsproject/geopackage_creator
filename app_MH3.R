library(sf)
library(terra)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(markdown)

options(shiny.maxRequestSize=100*1024^2) 
bp <- 'www/bp_datasets.gpkg'
spp <- 'www/species.gpkg'
prj <- 'www/projected.gpkg'
#bp <- 'H:/Shared drives/Data/bp_datasets.gpkg'
limits <- st_read(bp, 'bnd') %>% st_transform(4326)

ui = dashboardPage(skin="blue",
    dashboardHeader(title = "Geopackage Creator", titleWidth=220),
    dashboardSidebar(
        sidebarMenu(id="tabs",
            menuItem("Overview", tabName = "overview", icon = icon("th")),
            menuItem("Select study area", tabName = "select", icon = icon("arrow-pointer")),
            menuItem("Select spatial layers", tabName = "data", icon = icon("arrow-pointer")),
            menuItem("Download data", tabName = "download", icon = icon("th")),
            hr()
        ),
        conditionalPanel(
            condition="input.tabs=='select'",
            fileInput(inputId = "upload_poly", label = "Upload study area:", multiple = FALSE, accept=".gpkg")
        ),
         conditionalPanel(
           condition = "input.tabs == 'data'",
           HTML(paste("<b>&nbsp; &nbsp; Select range of fires size and", 
                      "<b>&nbsp; &nbsp; select optional spatial layers",
                      "<b>&nbsp; &nbsp; to include in the download from ", 
                      "<b>&nbsp; &nbsp; the list on right panel.", sep="<br/>")),
           sliderInput("minmax", label="Range of fires to include:", min=1920, max=2020, value=c(1960, 2020)),
           actionButton("goButton", "Preview geopackage"),
         ),
        conditionalPanel(
            condition="input.tabs=='download'",
            div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download data", style='color: #000'))
            )
    ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(".skin-blue .sidebar a { color: #8a8a8a; }")),
    tags$style(type = "text/css", "#map1 {height: calc(100vh - 164px) !important;}"),
    tabItems(
      tabItem(tabName="overview",
            fluidRow(
                tabBox(id = "one", width="12",
                    tabPanel("Overview", includeMarkdown("www/overview.md")),
                    tabPanel("Datasets", includeMarkdown("www/datasets.md"))
                )
            )
        ),
      tabItem(tabName="select",
            fluidRow(
                tabBox(id = "one", width="8",
                    tabPanel("Mapview", leafletOutput("map1") %>% withSpinner())
                ),
                tabBox(
                    id = "two", width="4",
                    tabPanel("Select Layers", 
                      strong("Required"),
                      disabled(checkboxInput('bp1', label='Study area', value=T)),
                      disabled(checkboxInput('bp2', label='Linear disturbances', value=T)),
                      disabled(checkboxInput('bp3', label='Areal disturbances', value=T)),
                      disabled(checkboxInput('bp4', label='Fires', value=T)),
                      disabled(checkboxInput('bp5', label='Intactness 2000', value=T)),
                      disabled(checkboxInput('bp6', label='Intactness 2020', value=T)),
                      disabled(checkboxInput('bp7', label='Protected areas', value=T)),
                      strong("Optional - projected"),
                      checkboxInput('prj1', label='Quartz Claims', value=F),
                      checkboxInput('prj2', label='Placer Claims', value=F),
                      strong("Optional - miscellaneous"),
                      checkboxInput('spp1', label='Caribou Herds', value=F),
                      checkboxInput('spp2', label='Thinhorn Sheep', value=F),
                      checkboxInput('spp3', label='Key Wetlands 2011', value=F))
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
    aoi <- bnd() %>% st_transform(3578)
    st_read(bp, 'sd_line') %>%
      st_intersection(aoi) %>%
      st_cast('MULTILINESTRING')
  })

  poly <- reactive({
    aoi <- bnd() %>% st_transform(3578)
    vect(bp, 'sd_poly') %>%
      st_as_sf() %>%
      st_cast('MULTIPOLYGON') %>%
      st_intersection(aoi) %>%
      st_cast('MULTIPOLYGON')
  })
  
  fires <- reactive({
    req(bnd())
    aoi <- bnd() %>% st_transform(3578)
      vect(bp, 'fires') %>%
        st_as_sf() %>%
        st_cast('MULTIPOLYGON') %>%
        filter(YEAR >= input$minmax[1] & YEAR <= input$minmax[2]) %>%
        st_intersection(aoi) %>%
        st_cast('MULTIPOLYGON')
  })

  ifl_2000 <- reactive({
      aoi <- bnd() %>% st_transform(3578)
        st_read(bp, 'ifl_2000') %>%
          st_intersection(aoi)
    #}
  })
   
  ifl_2020 <- reactive({
      aoi <- bnd() %>% st_transform(3578)
        st_read(bp, 'ifl_2020') %>%
          st_intersection(aoi)
    #}
  })

  pa_2021 <- reactive({
      aoi <- bnd() %>% st_transform(3578)
        st_read(bp, 'protected_areas') %>%
          st_intersection(aoi)
    #}
  })

  prj1 <- eventReactive(input$goButton, {
    if (input$prj1) {
      aoi <- bnd() %>% st_transform(3578) %>% st_union()
        st_read(prj, 'Quartz Claims') %>%
          st_intersection(aoi) %>%
          filter(TENURE_STATUS=='Active')
    } else {
      return(NULL)
    }
  })
  
  prj2 <- eventReactive(input$goButton, {
     if (input$prj2) {
      aoi <- bnd() %>% st_transform(3578) %>% st_union()
        st_read(prj, 'Placer Claims') %>%
          st_intersection(aoi) %>%
          filter(TENURE_STATUS=='Active')
    } else {
      return(NULL)
    }
  })
  
  spp1 <- eventReactive(input$goButton, {
    if (input$spp1) {
      aoi <- bnd() %>% st_transform(3578) %>% st_union()
      x <- st_read(spp, 'Caribou Herds')
      #x <- x[aoi,]
      x <- st_intersection(x, aoi)
    } else {
      return(NULL)
    }
  })
  
  spp2 <- eventReactive(input$goButton, {
    if (input$spp2) {
      aoi <- bnd() %>% st_transform(3578) %>% st_union()
        st_read(spp, 'Thinhorn Sheep') %>%
          st_intersection(aoi)
    } else {
      return(NULL)
    }
  })

  spp3 <- eventReactive(input$goButton, {
    if (input$spp3) {
      aoi <- bnd() %>% st_transform(3578) %>% st_union()
        st_read(spp, 'Key Wetlands 2011') %>%
          st_intersection(aoi)
    } else {
      return(NULL)
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
      #m <- leaflet(limits, options = leafletOptions(attributionControl=FALSE)) %>%
      m <- leaflet() %>%
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
        addPolygons(data=limits, color='black', fill=F, weight=1, group="Database limits") %>%
        addLayersControl(position = "topright",
          baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
          overlayGroups = c("Database limits"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c(""))
      
      if(!is.null(input$upload_poly)) {
        region <- bnd() %>% st_transform(4326)
        map_bounds <- region %>% st_bbox() %>% as.character()

        m <- m %>%
          fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>%
          addPolygons(data=region, color='blue', fill=F, weight=2, group="Study area") %>%
          addLayersControl(position = "topright",
            baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
            overlayGroups = c("Database limits", "Study area"),
            options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c(""))
      } 
      
      if(input$goButton >0){ 
        aoi <- bnd() %>% st_transform(3578)
        sd_line <- line() %>% st_transform(4326)
        sd_poly <- poly() %>% st_transform(4326)
        fires <- fires() %>%  st_transform(4326)
        ifl_2000 <- ifl_2000() %>% st_transform(4326)
        ifl_2020 <- ifl_2020() %>% st_transform(4326)
        pa_2021 <- pa_2021() %>% st_transform(4326)
        
        m <- m %>%
          addPolylines(data=sd_line, color='red', weight=2, group="Linear disturbances") %>%
          addPolygons(data=sd_poly, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Areal disturbances") %>%
          addPolygons(data=fires, fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group='Fires') %>%
          addPolygons(data=ifl_2000, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intactness 2000") %>%
          addPolygons(data=ifl_2020, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness 2020")
          
        grps <- NULL
        #Isolate allow to wait the trigger goButton to be pushed before looking into Optionals
        tprj1 <- isolate(input$prj1)
        tprj2 <- isolate(input$prj2)
        tspp1 <- isolate(input$spp1)
        tspp2 <- isolate(input$spp2)
        tspp3 <- isolate(input$spp3)
        
        if (tprj1 & length(prj1())>0) { 
          prj1 <- prj1() %>% st_transform(4326)
          m <- m %>% addPolygons(data=prj1, color='red', fill=T, weight=1, group="Quartz Claims")
          grps <- c(grps,"Quartz Claims")
        }
        if (tprj2 & length(prj2())>0) {
          prj2 <- prj2() %>% st_transform(4326)
          m <- m %>% addPolygons(data=prj2, color='red', fill=T, weight=1, group="Placer Claims")
          grps <- c(grps,"Placer Claims")
        }
        if (tspp1 & length(spp1())>0) {
          spp1 <- spp1() %>% st_transform(4326)
          m <- m %>% addPolygons(data=spp1, color='red', fill=T, weight=1, group="Caribou Herds")
          grps <- c(grps,"Caribou Herds")
        }
        if (tspp2 & length(spp2())>0) {
          spp2 <- spp2() %>% st_transform(4326)
          m <- m %>% addPolygons(data=spp2, color='red', fill=T, weight=1, group="Thinhorn Sheep")
          grps <- c(grps,"Thinhorn Sheep")
        }
        if (tspp3 & length(spp3())>0) {
          spp3 <- spp3() %>% st_transform(4326)
          m <- m %>% addPolygons(data=spp3, color='red', fill=T, weight=1, group="Key Wetlands 2011")
          grps <- c(grps,"Key Wetlands 2011")
        }
        
        m <- m %>% #addLayersControl(position = "topright",
          addLayersControl(position = "topright",
                           baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                           overlayGroups = c("Database limits","Study area", "Linear disturbances", "Areal disturbances", "Fires","Intactness 2000", "Intactness 2020", "Protected areas", grps),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c("Database limits", "Intactness 2000", "Intactness 2020", "Protected areas", grps))
      }
     m
  })

  ##############################################################################
  # Save features to a geopackage
  ##############################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("disturbances-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
        showModal(modalDialog("Downloading...", footer=NULL))
        on.exit(removeModal())
        st_write(bnd(), dsn=file, layer='studyarea')
        if (input$goButton) {
          st_write(line(), dsn=file, layer='linear_disturbance', append=TRUE)
          st_write(poly(), dsn=file, layer='areal_disturbance', append=TRUE)
          st_write(fires(), dsn=file, layer='fires', append=TRUE)
          st_write(ifl_2000(), dsn=file, layer='ifl_2000', append=TRUE)
          st_write(ifl_2020(), dsn=file, layer='ifl_2020', append=TRUE)
          st_write(pa_2021(), dsn=file, layer='protected_areas', append=TRUE)
          if (input$prj1 & length(prj1())>0) st_write(prj1(), dsn=file, layer='Quartz Claims', append=TRUE)
          if (input$prj2 & length(prj2())>0) st_write(prj2(), dsn=file, layer='Placer Claims', append=TRUE)
          if (input$spp1 & length(spp1())>0) st_write(spp1(), dsn=file, layer='Caribou Herds', append=TRUE)
          if (input$spp2 & length(spp2())>0) st_write(spp2(), dsn=file, layer='Thinhorn Sheep', append=TRUE)
          if (input$spp3 & length(spp3())>0) st_write(spp3(), dsn=file, layer='Key Wetlands 2011', append=TRUE)
       }
    }
  )

  #session$onSessionEnded(function() {
  #  stopApp()
  #})

}
shinyApp(ui, server)
