ui = dashboardPage(skin="black",
                   title = "BEACONs GeoPackage Creator", 
                   dashboardHeader(title = tags$div(
                     tags$img(
                       src = "logoblanc.png",  # Replace with your logo file name
                       height = "50px",   # Adjust the height of the logo
                       style = "margin-right: 10px;"  # Add some spacing around the logo
                     ), "BEACONs GeoPackage Creator" ), titleWidth = 500,
                     
                     # Add Reload Button Next to Sidebar Toggle
                     tags$li(
                       class = "dropdown",
                       actionButton(
                         "reload_btn",
                         label = "Reload",
                         icon = icon("refresh"),
                         style = "color: black; background-color: orange; border: none; font-size: 16px;"
                       ),
                       style = "position: absolute; left: 50px; top: 10px;"  # Adjust margin for placement next to the toggle
                     ),
                     tags$li(
                       class = "dropdown",  # Required for dropdown functionality
                       dropdownMenu(
                         type = "tasks", 
                         badgeStatus = NULL,
                         icon = icon("life-ring"),  # Life-ring icon triggering dropdown
                         headerText = "",  # No header text in dropdown
                         menuItem("Website", href = "https://beaconsproject.ualberta.ca/", icon = icon("globe")),
                         menuItem("GitHub", href = "https://github.com/beaconsproject/disturbance_explorer", icon = icon("github")),
                         menuItem("Contact us at beacons@ualberta.ca", href = "mailto: beacons@ualberta.ca", icon = icon("address-book"))
                       ),
                       # Plain Text "About Us" Positioned Next to Dropdown
                       tags$span(
                         "About Us", 
                         style = "font-size: 16px; position: relative; top: 15px; right: 10px; white-space: nowrap; color: white;"
                       )
                     )
                   ),
                   dashboardSidebar(
                     sidebarMenu(id="tabs",
                                 menuItem("Welcome", tabName = "overview", icon = icon("th")),
                                 menuItem("Select study area", tabName = "select", icon = icon("arrow-pointer")),
                                 menuItem("Select spatial layers", tabName = "data", icon = icon("arrow-pointer")),
                                 menuItem("Download geopackage", tabName = "download", icon = icon("th")),
                                 hr()
                     ),
                     conditionalPanel(
                       condition="input.tabs=='select'",
                       checkboxInput("enable_map", "Enable interactive map (slower for large study areas)", value = TRUE)
                     ),
                     conditionalPanel(
                       condition="input.tabs=='select'",
                       radioButtons("sourceSA", "Select file format:",
                                    choices = list("Shapefile" = "sashp", 
                                                   "GeoPackage" = "sagpkg"),
                                    selected = character(0), 
                                    inline = TRUE),
                       fileInput("upload_poly", "Upload study area:", multiple = TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.gpkg'))
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'select' && input.sourceSA == 'sagpkg'",
                       div(style = "margin-top: -20px;", selectInput("saLayer", "Select study area layer", choices = NULL,  multiple = FALSE))
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'data'",
                       tags$div(HTML(paste("&nbsp; &nbsp; 1. Select optional spatial layers", 
                                  "&nbsp; &nbsp; from panel on the right",
                                  "&nbsp; &nbsp; to include in geopackage.", sep="<br/>")),
                                style = "margin-bottom: 30px; font-size:15px; font-weight: bold"),
                       tags$div(HTML(paste("&nbsp; &nbsp; 2. Select range of fires",
                                           "&nbsp; &nbsp; to include in geopackage.", sep="<br/>")),
                                style = "font-size:15px; font-weight: bold"),
                       sliderInput("minmax", label="Range of fires to include:", min=1920, max=2023, value=c(1960, 2023)),
                       actionButton("goButton", "Extract selected layers"),
                     ),
                     conditionalPanel(
                       condition="input.tabs=='download'",
                       div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download geopackage", style='color: #000'))
                     )
                   ),
                   dashboardBody(
                     useShinyjs(),
                     tags$head(tags$link(rel = "icon", type = "image/png", href = "logoblanc.png"),
                               tags$link(rel = "stylesheet", type = "text/css", href = "green-theme.css")),
                     tabItems(
                       tabItem(tabName = "overview",
                               fluidRow(
                                 column(width = 8,  # Adjusted from 6 to 8 for better alignment
                                        tabBox(id = "landing", width = 12,
                                               tabPanel("Overview", includeMarkdown("docs/overview.md")),
                                               tabPanel("User Guide", includeMarkdown("docs/user_guide.md")),
                                               tabPanel("Dataset Requirements", includeMarkdown("docs/datasets.md"))
                                        )
                                 ),
                                 absolutePanel(
                                   right = 0, top = 0, width = 250, height = "100%",
                                   style = "background-color: white; padding: 0;margin: 0;border: none; right: 0;overflow: hidden;z-index: 1000;",
                                   tags$img(src = "intact.jpg",width = "100%", style = "display: block;")
                                 )
                               )
                       ),
                       tabItem(tabName="select",
                               fluidRow(
                                 tabBox(id = "one", width="8",
                                        tabPanel("Mapview", uiOutput("mapUI") %>% withSpinner()),
                                        tabPanel("User Guide",
                                                 # Dynamically update the content of Guidance based on selected tab
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'select'",
                                                   includeMarkdown("./docs/select_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'data'",
                                                   includeMarkdown("./docs/data_doc.md")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabs == 'download'",
                                                   includeMarkdown("./docs/dwd_doc.md")
                                                 )
                                        )
                                 ),
                                 conditionalPanel(
                                   condition = "input.tabs == 'data'",
                                   tabBox(
                                   id = "two", width="4",
                                   tabPanel("Select Layers", 
                                            strong("Required"),
                                            disabled(checkboxInput('bp1', label='Study area', value=T)),
                                            disabled(checkboxInput('bp2', label='Linear disturbances', value=T)),
                                            disabled(checkboxInput('bp3', label='Areal disturbances', value=T)),
                                            disabled(checkboxInput('bp4', label='Fires', value=T)),
                                            disabled(checkboxInput('bp5', label='Intact FL 2000', value=T)),
                                            disabled(checkboxInput('bp6', label='Intact FL 2020', value=T)),
                                            disabled(checkboxInput('bp7', label='Protected areas', value=T)),
                                            strong("Optional - projected"),
                                            checkboxInput('prj1', label='Quartz Claims', value=F),
                                            checkboxInput('prj2', label='Placer Claims', value=F),
                                            strong("Optional - miscellaneous"),
                                            checkboxInput('spp1', label='Caribou Herds', value=F))
                                 )
                               )
                               )
                       )
                     )
                   )
)
