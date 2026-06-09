server = function(input, output, session) {

  output$overviewMD <- renderUI({
    HTML(markdown::markdownToHTML(text = overview_md_text, fragment.only = TRUE))
  })
  
  rv <- reactiveValues(gpkg_layers = NULL)
  clipped_layers <- reactiveValues()
  
  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    isolate({
      leafletProxy("map1") %>%
        clearGroup("Linear disturbances") %>%
        clearGroup("Areal disturbances") %>%
        clearGroup("Fires") %>%
        clearGroup("Intact FL 2000") %>%
        clearGroup("Intact FL 2020") %>%
        clearGroup("Placer claims") %>%
        clearGroup("Quartz claims") %>%
        clearGroup("Protected areas") %>%
        clearGroup("Caribou herds") %>%
        clearGroup("Footprint 500m") %>%
        clearGroup("Undisturbed areas 500m")
      
      rv$gpkg_layers <- NULL
      # Clear all elements in clipped_layers
      lapply(names(reactiveValuesToList(clipped_layers)), function(nm) {
        clipped_layers[[nm]] <- NULL
      })
    })
    
    gc()
    
    session$reload()
  })
  
  ################################################################################################
  ################################################################################################
  # Observe on tabs
  ################################################################################################
  # Observe tab changes
  observeEvent(input$tabs, {
    if (input$tabs != "select" && is.null(r$aoi) && input$tabs != "overview") {
      # Show modal message if tabUpload has not been visited
      showModal(modalDialog(
        title = "Action Required",
        "Please upload a study area prior to select layers to extract.",
        easyClose = TRUE,
        footer = modalButton("Go to Select study area")
      ))
      
      # Redirect user back to tabUpload
      updateTabItems(session = getDefaultReactiveDomain(), "tabs", "select")
    }
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
    rv$gpkg_layers <- layers
    
    updateSelectInput(session = getDefaultReactiveDomain(), "saLayer", choices = c("Select a layer", layers))
  
  })
  
output$addLayersUI <- renderUI({
  
  req(rv$gpkg_layers)
  req(input$saLayer)
  req(input$saLayer != "Select a layer")
  
  other_layers <- setdiff(rv$gpkg_layers, input$saLayer)
  if(length(other_layers)>0){
    div(style = "margin-left: 20px", h5(strong("OPTIONAL - Select additional layers to include in downloaded GeoPackage:"),
                                                          checkboxGroupInput("extraLayers", label = "", choices = other_layers)))
  } else{
    return(NULL)
  }
  
})
  ##############################################################################
  # Read input data
  ##############################################################################
  r <- reactiveValues(goButton = 0,
                      switch_tab =0,
                      display_map = 0,
                      aoi = NULL)
  
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
        i <- read_shp_from_upload(input$upload_poly) |>
          st_zm(drop = TRUE, what = "ZM")  |>
          st_make_valid() |>
          dplyr::select(-any_of(c("fid", "FID")))
        
        geom_type <- unique(sf::st_geometry_type(i))
        if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
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
    ext <- st_read_parquet(file.path(bp, 'bnd.parquet'))
    i <- st_transform(i, 3578)
    
    if (!any(st_intersects(i, ext, sparse = FALSE))) {
      
      showModal(modalDialog(
        title = "Uploaded polygon is outside the supported area",
        "The uploaded study area falls outside the geographic extent covered by this application. Please upload a study area located within the supported region.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    r$aoi <- i
    return(i)
  })
  
  observeEvent(input$upload_poly, {
    r$goButton <- 0
  })
  observeEvent(input$goButton, {
    r$goButton <- 1
  })
  
  observeEvent(input$goButton, {
    req(bnd())
    
    showModal(
      modalDialog(
        div(
          style = "text-align:center; padding:30px;",
          tags$h3("Clipping layers..."),
          tags$div(
            id = "progress_text",
            "Starting..."
          )
        ),
        footer = NULL,
        easyClose = FALSE
      )
    )
    
    aoi <- bnd()
    
    n <- sum(
      isTRUE(input$bp2),
      isTRUE(input$bp3),
      isTRUE(input$bp4),
      isTRUE(input$fp1),
      isTRUE(input$fp2),
      isTRUE(input$bp5),
      isTRUE(input$bp6),
      isTRUE(input$bp7),
      isTRUE(input$prj1),
      isTRUE(input$prj2),
      isTRUE(input$spp1)
    ) 
    
    # ---- run your clipping once ----
    update_progress(1, n, "Loading linear disturbances data...")
    
    clipped_layers$line <- st_read_parquet(file.path(bp, 'linear_disturbances.parquet')) |>
      st_filter(aoi) |>
      st_intersection(aoi)
    
    update_progress(2, n, "Loading areal disturbances data...")
    clipped_layers$poly <- st_read_parquet(file.path(bp, 'areal_disturbances.parquet')) |>
      st_filter(aoi) |>
      st_intersection(aoi)
    
    i <-2
    if (isTRUE(input$bp4)){
      i <-i+1
      update_progress(i, n, "Loading fires data")
      if(input$scFires =="nbac"){
        li <- "fires_nbac.parquet"
      }else{
        li <- "fires_nfdb.parquet"
      }
      clipped_layers$fires <- st_read_parquet(file.path(bp, li)) |>
        st_cast('MULTIPOLYGON') |>
        filter(YEAR >= input$minmax[1], YEAR <= input$minmax[2]) |>
        st_filter(aoi) |>
        st_intersection(aoi)
    }
    if (isTRUE(input$fp1)){
      i <-i+1
      update_progress(i, n, "Loading footprint 500m data")
      clipped_layers$fp1_500m <- st_read_parquet(file.path(bp, 'footprint_500m.parquet')) |>
        st_filter(aoi) |>
        st_intersection(aoi)
    }
    if (isTRUE(input$fp2)){
      i <-i+1
      update_progress(i, n, "Loading undisturbed areas 500m data")
      clipped_layers$fp2_500m <- st_read_parquet(file.path(bp, 'undisturbed_areas_500m.parquet')) |>
        st_filter(aoi) |>
        st_intersection(aoi)
    }
    if (isTRUE(input$bp5)){
      i <-i+1
      update_progress(i, n, "Loading intact fl 2000 data...")
      clipped_layers$ifl_2000 <- st_read_parquet(file.path(bp, 'intact_fl_2000.parquet')) |>
        st_filter(aoi) |>
        st_intersection(aoi)
    }
    if (isTRUE(input$bp6)){
      i <-i+1
      update_progress(i, n, "Loading intact fl 2020 data...")
      clipped_layers$ifl_2020 <- st_read_parquet(file.path(bp, 'intact_fl_2020.parquet')) |>
        st_filter(aoi) |>
        st_intersection(aoi)
    }
    if (isTRUE(input$bp7)){
      i <-i+1
      update_progress(i, n, "Loading protected areas data...")
      clipped_layers$pa_2021 <- st_read_parquet(file.path(bp, 'protected_areas.parquet')) |>
        st_filter(aoi) |>
        st_intersection(aoi)
    }
    
    if (input$prj1) {
      i <-i+1
      update_progress(i, n, "Loading quartz claims data...")
      clipped_layers$prj1 <- st_read_parquet(file.path(prj, 'quartz_claims.parquet')) |>
        st_filter(aoi) |>
        st_intersection(aoi)
    } 
    if (input$prj2) {
      i <-i+1
      update_progress(i, n, "Loading placer claims data...")
      clipped_layers$prj2 <- st_read_parquet(file.path(prj, 'placer_claims.parquet')) |>
        st_filter(aoi) |>
        st_intersection(aoi)
    } else {
      clipped_layers$prj2 <- NULL
    }
    if (input$spp1) {
      i <-i+1
      update_progress(i, n, "Loading caribou herds data...")
      clipped_layers$spp1 <- st_read_parquet(file.path(spp, 'caribou_herds.parquet')) |>
        st_filter(aoi) |>
        st_intersection(aoi)
    } else {
      clipped_layers$spp1 <- NULL
    }
     
    removeModal()
    r$switch_tab <- 1
    r$display_map <- 1
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
  observeEvent(isTRUE(r$switch_tab ==1), {
    if (isTRUE(input$enable_map)) {
      output$mapUI <- renderUI({
        leafletOutput("map1", height = 600)
      })
      
      output$map1 <- renderLeaflet({
        m <- leaflet() |>
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") |>
          addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") |>
          addPolygons(data=limits, color='black', fill=F, weight=1, group="Database limits") |>
          addLayersControl(position = "topright",
                           baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                           overlayGroups = c("Database limits"),
                           options = layersControlOptions(collapsed = FALSE)) |>
          hideGroup(c(""))
        
        if(!is.null(bnd())) {
          region <- bnd() |> st_transform(4326)
          map_bounds <- region |> st_bbox() |> as.character()
          
          m <- m |>
            fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) |>
            addPolygons(data=region, color='blue', fill=F, weight=2, group="Study area") |>
            addLayersControl(position = "topright",
                             baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                             overlayGroups = c("Database limits", "Study area"),
                             options = layersControlOptions(collapsed = FALSE)) |>
            hideGroup(c(""))
        } 
        
        if(r$goButton == 1){
          grps <- NULL 
          
          if (length(clipped_layers$line)>0) { 
            sd_line <- clipped_layers$line |>  st_transform(4326)
            m <- m |> addPolylines(data=sd_line, color='red', weight=2, group="Linear disturbances")
            grps <- c(grps,"Linear disturbances")
          } 
          if (length(clipped_layers$poly)>0) { 
            sd_poly <- clipped_layers$poly |>  st_transform(4326)
            m <- m |> addPolygons(data=sd_poly, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Areal disturbances")
            grps <- c(grps,"Areal disturbances")
          } 
          
          #Isolate allow to wait the trigger goButton to be pushed before looking into Optionals
          tbp4 <- isolate(input$bp4)
          tbp5 <- isolate(input$bp5)
          tbp6 <- isolate(input$bp6)
          tbp7 <- isolate(input$bp7)
          tfp1 <- isolate(input$fp1)
          tfp2 <- isolate(input$fp2)
          tprj1 <- isolate(input$prj1)
          tprj2 <- isolate(input$prj2)
          tspp1 <- isolate(input$spp1)
          
          if (tbp4 & length(clipped_layers$fires)>0) { 
            fires <- clipped_layers$fires |>  st_transform(4326)
            m <- m |> addPolygons(data=fires, fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group='Fires')
            grps <- c(grps,"Fires")
          } 
          if (tbp5 & length(clipped_layers$ifl_2000)>0) { 
            ifl_2000 <- clipped_layers$ifl_2000 |> st_transform(4326)
            m <- m |> addPolygons(data=ifl_2000, fill=T, stroke=F, fillColor='#3366FF', fillOpacity=0.5, group="Intact FL 2000")
            grps <- c(grps,"Intact FL 2000")
          } 
          if (tbp6 & length(clipped_layers$ifl_2020)>0) { 
            ifl_2020 <- clipped_layers$ifl_2020 |> st_transform(4326)
            m <- m |> addPolygons(data=ifl_2020, fill=T, stroke=F, fillColor='#000066', fillOpacity=0.5, group="Intact FL 2020") 
            grps <- c(grps,"Intact FL 2020")
          } 
          if (tbp7 & length(clipped_layers$pa_2021)>0) { 
            pa_2021 <- clipped_layers$pa_2021 |> st_transform(4326)
            m <- m |> addPolygons(data=pa_2021, fill=T, stroke=F, fillColor='#699999', fillOpacity=0.5, group="Protected areas") 
            grps <- c(grps,"Protected areas")
          } 
          if (tfp1 & length(clipped_layers$fp1_500m)>0) { 
            fp1_500m <- clipped_layers$fp1_500m |> st_transform(4326)
            m <- m |> addPolygons(data=fp1_500m, fill=T, stroke=F, fillColor='#663399', fillOpacity=0.5, group="Footprint 500m")
            grps <- c(grps,"Footprint 500m")
          } 
          if (tfp2 & length(clipped_layers$fp2_500m)>0) { 
            fp2_500m <- clipped_layers$fp2_500m |> st_transform(4326)
            m <- m |> addPolygons(data=fp2_500m, fill=T, stroke=F, fillColor='#006600', fillOpacity=0.7, group="Undisturbed areas 500m")
            grps <- c(grps,"Undisturbed areas 500m")
          } 
          if (tprj1 & length(clipped_layers$prj1)>0) { 
            prj1 <- clipped_layers$prj1 |> st_transform(4326)
            m <- m |> addPolygons(data=prj1, color='#999999', fill=T, weight=1, group="Quartz claims")
            grps <- c(grps,"Quartz claims")
          }
          if (tprj2 & length(clipped_layers$prj2)>0) {
            prj2 <- clipped_layers$prj2 |> st_transform(4326)
            m <- m |> addPolygons(data=prj2, color='#333333', fill=T, weight=1, group="Placer claims")
            grps <- c(grps,"Placer claims")
          }
          if (tspp1 & length(clipped_layers$spp1)>0) {
            spp1 <- clipped_layers$spp1 |> st_transform(4326)
            m <- m |> addPolygons(data=spp1, color='red', fill=T, weight=1, group="Caribou herds")
            grps <- c(grps,"Caribou herds")
          }
          
          m <- m |> #addLayersControl(position = "topright",
            addLayersControl(position = "topright",
                             baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                             overlayGroups = c("Database limits","Study area", grps),
                             options = layersControlOptions(collapsed = FALSE)) |>
            hideGroup(c("Database limits"))
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
      
      sa <- bnd() |> st_transform(3578)
      st_write(sa, dsn=file, layer='studyarea')
      if (nrow(clipped_layers$line)>0) st_write(clipped_layers$line, dsn=file, layer='linear_disturbances', append=TRUE)
      if (nrow(clipped_layers$poly)>0) st_write(clipped_layers$poly, dsn=file, layer='areal_disturbances', append=TRUE)
      if (isTRUE(input$bp4 & nrow(clipped_layers$fires)>0)) st_write(clipped_layers$fires, dsn=file, layer='fires', append=TRUE)
      if (isTRUE(input$bp5 & nrow(clipped_layers$ifl_2000)>0)) st_write(clipped_layers$ifl_2000, dsn=file, layer='intact_fl_2000', append=TRUE)
      if (isTRUE(input$bp6 & nrow(clipped_layers$ifl_2020)>0)) st_write(clipped_layers$ifl_2020, dsn=file, layer='intact_fl_2020', append=TRUE)
      if (isTRUE(input$bp7 & nrow(clipped_layers$pa_2021)>0)) st_write(clipped_layers$pa_2021, dsn=file, layer='protected_areas', append=TRUE)
      if (isTRUE(input$fp1 & nrow(clipped_layers$fp1_500m)>0)) st_write(clipped_layers$fp1_500m, dsn=file, layer='footprint_500m', append=TRUE)
      if (isTRUE(input$fp2 & nrow(clipped_layers$fp2_500m)>0)) st_write(clipped_layers$fp2_500m, dsn=file, layer='undisturbed_areas_500m', append=TRUE)
      if (isTRUE(input$prj1 & nrow(clipped_layers$prj1)>0)) st_write(clipped_layers$prj1, dsn=file, layer='quartz_claims', append=TRUE)
      if (isTRUE(input$prj2 & nrow(clipped_layers$prj2)>0)) st_write(clipped_layers$prj2, dsn=file, layer='placer_claims', append=TRUE)
      if (isTRUE(input$spp1 & nrow(clipped_layers$spp1)>0)) st_write(clipped_layers$spp1, dsn=file, layer='caribou_herds', append=TRUE)
      
      if(!is.null(rv$gpkg_layers)){
        if(!is.null(input$extraLayers)){
          so_gpkg <- input$upload_poly$datapath
          
          for (layer in input$extraLayers) {
            la <- sf::st_read(so_gpkg, layer = layer, quiet = TRUE) |>
              st_transform(3578)
            
            st_write(la, dsn = file, layer = layer, append = TRUE)
          }
        }
      }
    }
  )
  
}