# load libraries
require(shiny, quietly = TRUE)
require(shinydashboard, quietly = TRUE)
require(leaflet, quietly = TRUE)
require(plotly, quietly = TRUE)
require(DT, quietly = TRUE)

# source citation page content
citation = source('./citation.R')

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

# the header
header <- dashboardHeader(title = "PhenoCam Explorer")

# the sidebar
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Explore data", tabName = "explorer", icon = icon("bar-chart-o")
  ),
  menuItem(
    "Cite Data", tabName = "citation", icon = icon("creative-commons")
  )
))

# main body
body <- dashboardBody(tags$head(tags$script(
  HTML(
    "
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
    var h = window.innerHeight - $('.navbar').height() - 225; // Get dashboardBody height
    $('#map').height(h);
    }
    function resizeTable(){
    var h = window.innerHeight - $('.navbar').height() - 535;
    $('#time_series_plot').height(h);
    }"
      )
  )),
  tags$head(includeCSS("styles.css")),
  tabItems(
    tabItem(
      # the Interactive map and subset interface
      # and time series plotting interface
      tabName = "explorer",
      tabBox(
        side = "left",
        width = 12,
        #height="90%",
        selected = "Map & Site selection",
        tabPanel(
          "Map & Site selection",icon = icon("globe"),
          fluidRow(
            infoBoxOutput("site_count"),
            infoBoxOutput("year_count"),
            column(4,
                   selectInput("colors", "Vegetation Type",vegtype))
          ),
          fluidRow(column(
            12,
            box(
              width = NULL,
              leafletOutput("map"),
              absolutePanel(
                id = "controls", class = "panel panel-default",
                fixed = TRUE,
                draggable = FALSE, top = 300, left = "auto", right = 70, bottom = "auto",
                width = 380, height = 350,
                #h4("Climatology", align="center"),
                plotOutput("climate",
                             height = "100%",
                             width = "100%"
                             )
              )
            )
          ))
        ),
        tabPanel(
          "Plot data", icon = icon("bar-chart-o"),
          fluidRow(column(
            3,
            box(
              width = NULL,
              h4("Plotting options"),
              selectInput("percentile", "Percentile",c(90,75,50,"mean"),
                          width = "100%"),
              selectInput("frequency", "Data Frequency",c(3,1),
                          width ="100%"),
              selectInput("plot_type", "Plot Type",
                c("Time Series" = "bydate",
                  "DOY Time Series" = "byyear",
                  "Phenology" = "phenology"),
                width = "100%"),
              conditionalPanel( # dynamic GUI component 
                condition = "input.plot_type == 'bydate'",
                checkboxInput("rccbcc", "plot Rcc / Bcc",
                            value = FALSE,
                            width = "100%")
              )
            )
          ),
          column(9,
                 box(
                   width = NULL,
                   DT::dataTableOutput("table")
                 ))),
          fluidRow(column(
            12,
            box(
              width = NULL,height = NULL,
              plotlyOutput("time_series_plot")
            )
          )),
          fluidRow(column(
            3, downloadButton('downloadData', label = "Download Transition Dates")
          ))
        )
      )
    ),
    tabItem(# the about page
      tabName = "citation",
      tabPanel("Citing Data", box(width = NULL, citation$value)))
  ))

ui <- dashboardPage(skin = "green", header, sidebar, body)