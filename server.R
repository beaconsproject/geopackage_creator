server = function(input, output, session) {

  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })
  
  ################################################################################################
  ################################################################################################
  # Observe on selectInput
  ################################################################################################
  observe({
    req(input$upload_poly)
    req(input$sourceSA == 'sagpkg')
    file <- input$upload_poly$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "saLayer", choices = c("Select a layer", layers))
  })
  
  ##############################################################################
  # Read input data
  ##############################################################################
  r <- reactiveValues(goButton = 0,
                      switch_tab =0)
  
  ##############################################################################
  # Uploaded data
  ##############################################################################
  bnd <- reactive({
    if (is.null(input$upload_poly)) {
      return(NULL)  
    }
    
    if(is.null(input$sourceSA)){
      showModal(modalDialog(
        title = "Missing file format",
        "Please set the file format before uploading the studyarea.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      i <- NULL
    } else if(input$sourceSA == "sashp"){
      if(!is.null(input$upload_poly)){
        req(input$upload_poly)
        i <- read_shp_from_upload(input$upload_poly) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid() %>%
          dplyr::select(-any_of(c("fid", "FID")))
        
        geom_type <- unique(sf::st_geometry_type(i))
        if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
        } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
        } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
          i <- suppressWarnings(sf::st_cast(i, "POINT"))
        }
      }
    } else if (input$sourceSA == "sagpkg"){
      req(input$upload_poly)
      if(input$saLayer != "Select a layer" && input$saLayer != ""){
        i <- st_read(input$upload_poly$datapath, layer = input$saLayer, quiet = TRUE) 
      }else{
        i <- NULL
      }
    } 
    return(i)
  })
  
  observeEvent(input$upload_poly, {
    r$goButton <- 0
  })
  observeEvent(input$goButton, {
    r$goButton <- 1
  })
  
  # storage for clipped layers
  clipped_layers <- reactiveValues()
  
  observeEvent(input$goButton, {
    req(bnd())
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Extracting layers. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    aoi <- bnd() %>% st_transform(3578)
    
    # ---- run your clipping once ----
    clipped_layers$line <- st_read(bp, 'sd_line') %>%
      st_filter(aoi, .predicate = st_intersects)
    
    clipped_layers$poly <- vect(bp, 'sd_poly') %>%
      st_as_sf() %>%
      st_filter(aoi, .predicate = st_intersects)
    
    clipped_layers$fires <- vect(bp, 'fires') %>%
      st_as_sf() %>%
      st_cast('MULTIPOLYGON') %>%
      filter(YEAR >= input$minmax[1], YEAR <= input$minmax[2]) %>%
      st_filter(aoi, .predicate = st_intersects)
    
    clipped_layers$ifl_2000 <- st_read(bp, 'Intact_FL_2000') %>%
      st_intersection(aoi)
    
    clipped_layers$ifl_2020 <- st_read(bp, 'Intact_FL_2020') %>%
      st_intersection(aoi)
    
    clipped_layers$pa_2021 <- st_read(bp, 'protected_areas') %>% 
      st_filter(aoi, .predicate = st_intersects)
    
    if (input$prj1) {
      clipped_layers$prj1 <- st_read(prj, 'Quartz Claims') %>%
        filter(TENURE_STATUS == "Active") %>%
        st_filter(st_union(aoi), .predicate = st_intersects)
    } else {
      clipped_layers$prj1 <- NULL
    }
    
    if (input$prj2) {
      clipped_layers$prj2 <- st_read(prj, 'Placer Claims') %>%
        filter(TENURE_STATUS == "Active") %>%
        st_filter(st_union(aoi), .predicate = st_intersects)
    } else {
      clipped_layers$prj2 <- NULL
    }
    if (input$spp1) {
      clipped_layers$spp1 <- st_read(spp, 'Caribou Herds') %>%
        st_filter(st_union(aoi), .predicate = st_intersects)
    } else {
      clipped_layers$spp1 <- NULL
    }
    r$switch_tab <- 1
    removeModal()
  })
 
  ##############################################################################
  # Observe on tab
  ##############################################################################
  observeEvent(isTRUE(r$switch_tab ==1), {
    # Only switch if the map checkbox is NOT checked
    if (!isTRUE(input$enable_map)) {
      updateTabItems(session, "tabs", "download")
    }
  })
  ##############################################################################
  # View initial set of maps
  ##############################################################################
  observe({
    if (isTRUE(input$enable_map)) {
      output$mapUI <- renderUI({
        leafletOutput("map1", height = 600)
      })
      
      output$map1 <- renderLeaflet({
        m <- leaflet() %>%
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
          addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
          addPolygons(data=limits, color='black', fill=F, weight=1, group="Database limits") %>%
          addLayersControl(position = "topright",
                           baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                           overlayGroups = c("Database limits"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c(""))
        
        if(!is.null(bnd())) {
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
        
        #if(input$goButton >0){ 
        if(r$goButton == 1){
          
          aoi <- bnd() %>% st_transform(3578)
          sd_line <- clipped_layers$line %>% st_transform(4326)
          sd_poly <- clipped_layers$poly %>% st_transform(4326)
          fires <- clipped_layers$fires %>%  st_transform(4326)
          ifl_2000 <- clipped_layers$ifl_2000 %>% st_transform(4326)
          ifl_2020 <- clipped_layers$ifl_2020 %>% st_transform(4326)
          pa_2021 <- clipped_layers$pa_2021 %>% st_transform(4326)
          
          m <- m %>%
            addPolylines(data=sd_line, color='red', weight=2, group="Linear disturbances") %>%
            addPolygons(data=sd_poly, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Areal disturbances") %>%
            addPolygons(data=fires, fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group='Fires') %>%
            addPolygons(data=ifl_2000, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intact FL 2000") %>%
            addPolygons(data=ifl_2020, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intact FL 2020")
          
          grps <- NULL
          #Isolate allow to wait the trigger goButton to be pushed before looking into Optionals
          tprj1 <- isolate(input$prj1)
          tprj2 <- isolate(input$prj2)
          tspp1 <- isolate(input$spp1)
          
          if (tprj1 & length(clipped_layers$prj1)>0) { 
            prj1 <- clipped_layers$prj1 %>% st_transform(4326)
            m <- m %>% addPolygons(data=prj1, color='red', fill=T, weight=1, group="Quartz Claims")
            grps <- c(grps,"Quartz Claims")
          }
          if (tprj2 & length(clipped_layers$prj2)>0) {
            prj2 <- clipped_layers$prj2 %>% st_transform(4326)
            m <- m %>% addPolygons(data=prj2, color='red', fill=T, weight=1, group="Placer Claims")
            grps <- c(grps,"Placer Claims")
          }
          if (tspp1 & length(clipped_layers$spp1)>0) {
            spp1 <- clipped_layers$spp1 %>% st_transform(4326)
            m <- m %>% addPolygons(data=spp1, color='red', fill=T, weight=1, group="Caribou Herds")
            grps <- c(grps,"Caribou Herds")
          }
          
          m <- m %>% #addLayersControl(position = "topright",
            addLayersControl(position = "topright",
                             baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                             overlayGroups = c("Database limits","Study area", "Linear disturbances", "Areal disturbances", "Fires","Intact FL 2000", "Intact FL 2020", "Protected areas", grps),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            hideGroup(c("Database limits", "Intact FL 2000", "Intact FL 2020", "Protected areas", grps))
        }
        m
      })
      
    } else {
      output$mapUI <- renderUI({
        div("Mapview is disabled. You can still process and download results without visualization.",
            style = "font-size: 20px; margin-top:50px; font-weight: bold; color: #333;")
      })
    }
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
      if (length(clipped_layers$line)>0)st_write(clipped_layers$line, dsn=file, layer='linear_disturbance', append=TRUE)
      if (length(clipped_layers$poly)>0)st_write(clipped_layers$poly, dsn=file, layer='areal_disturbance', append=TRUE)
      if (length(clipped_layers$fires)>0)st_write(clipped_layers$fires, dsn=file, layer='fires', append=TRUE)
      if (length(clipped_layers$ifl_2000)>0)st_write(clipped_layers$ifl_2000, dsn=file, layer='Intact_FL_2000', append=TRUE)
      if (length(clipped_layers$ifl_2020)>0)st_write(clipped_layers$ifl_2020, dsn=file, layer='Intact_FL_2020', append=TRUE)
      if (length(clipped_layers$pa_2021)>0)st_write(clipped_layers$pa_2021, dsn=file, layer='protected_areas', append=TRUE)
      if (input$prj1 & length(clipped_layers$prj1)>0) st_write(clipped_layers$prj1, dsn=file, layer='Quartz_Claims', append=TRUE)
      if (input$prj2 & length(clipped_layers$prj2)>0) st_write(clipped_layers$prj2, dsn=file, layer='Placer_Claims', append=TRUE)
      if (input$spp1 & length(clipped_layers$spp1)>0) st_write(clipped_layers$spp1, dsn=file, layer='Caribou_Herds', append=TRUE)
    }
  )
  
}