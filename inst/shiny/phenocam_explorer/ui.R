# load libraries
require(shiny, quietly = TRUE)
require(shinydashboard, quietly = TRUE)
require(leaflet, quietly = TRUE)
require(plotly, quietly = TRUE)
require(DT, quietly = TRUE)

# source about page content
about = source('about.r')

# vegetation type list
vegtype = c(
  "All" = "ALL",
  "Deciduous Broadleaf" = "DB",
  "Evergreen Needleleaf" = "EN",
  "Shrubland" = "SH",
  "Grassland" = "GR",
  "Tundra" = "TN",
  "Agriculture" = "AG",
  "Evergreen Broadleaf" = "EB",
  "Wetland" = "WL"
)

# interface elements

header <- dashboardHeader(title = "PhenoCam Explorer")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Explore data", tabName = "explorer", icon = icon("bar-chart-o")),
    menuItem("About the data", tabName = "about", icon = icon("info-circle")),
    menuItem("R Toolbox on Github", icon = icon("github"),href = "https://github.com/khufkens")
  )
)

body <- dashboardBody(
  tags$head(
    tags$script(
      HTML("
          window.onload = function() {
            resizeMap();
            resizeTable();
          }
          window.onresize = function() {
            resizeMap();
            resizeTable();
          }
          Shiny.addCustomMessageHandler ('triggerResize',function (val) {
            window.dispatchEvent(new Event('resize'));
          });
          function resizeMap(){
            var h = window.innerHeight - $('.navbar').height() - 280; // Get dashboardBody height
            $('#map').height(h); 
          }
          function resizeTable(){
            var h = window.innerHeight - $('.navbar').height() - 500;
            $('#time_series_plot').height(h);
          }"
      )
    )
  ),
  tags$head(includeCSS("styles.css")),
  tabItems(
    tabItem(
      # the Interactive map and subset interface
      # and time series plotting interface
      tabName = "explorer",
      tabBox(
        side = "left",
        width=12,
        #height="90%",
        selected = "Map & Site selection",
        tabPanel("Map & Site selection",icon=icon("globe"),
                 fluidRow(
                   valueBoxOutput("site_count"),
                   valueBoxOutput("year_count"),
                   column(4,
                          selectInput("colors", "Vegetation Type",vegtype)
                   )
                 ),
                 fluidRow(
                   column(12,
                          box(width=NULL,
                              leafletOutput("map"),
                              # Shiny versions prior to 0.11 should use class="modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 275, left = "auto", right = 70, bottom = "auto",
                                            width = 320, height = 350,
                                            h4("Climatology"),
                                            plotOutput("test", height=280,width=280)
                              )
                          )
                   )
                 )
        ),
        tabPanel("Plot data", icon = icon("bar-chart-o"),
                 fluidRow(
                   column(3,
                          box(width = NULL,
                              h4("Plotting options"),
                              selectInput("percentile", "Percentile",c(90,75,50,"mean"),width="100%"),  
                              selectInput("frequency", "Data Frequency",c(3,1),width="100%"),
                              selectInput("plot_type", "Plot Type",c("daily","yearly"),width="100%")
                          )),
                   column(9,
                          box(width = NULL,
                              DT::dataTableOutput("table")
                          ))
                 ),
                 fluidRow(
                   column(12,
                          box(width = NULL,height = NULL,
                              plotlyOutput("time_series_plot")
                          )
                   ) 
                 )
        )
      )
    ),
    tabItem(
      # the about page
      tabName = "about",
      tabPanel("About", box(width=NULL,about$value))
    )
  )
)

ui <- dashboardPage(skin = "green", header, sidebar, body)