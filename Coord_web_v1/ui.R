library(shiny)
library(leaflet)
library(DT)

shinyUI(fluidPage(
  # titlePanel("Old Faithful Geyser Data"),
  # sidebarLayout(
  #   sidebarPanel(
  #     fileInput('file1', 'Choose file to upload',
  #               accept = c(
  #                 'text/csv',
  #                 'text/comma-separated-values',
  #                 'text/tab-separated-values',
  #                 'text/plain',
  #                 '.csv',
  #                 '.tsv'
  #               )
  #     ),
  #     fileInput('file2', 'Choose file to upload',
  #               accept = c(
  #                 'text/csv',
  #                 'text/comma-separated-values',
  #                 'text/tab-separated-values',
  #                 'text/plain',
  #                 '.csv',
  #                 '.tsv'
  #               )
  #     ),
  #     tags$hr(),
  #     checkboxInput('header', 'Header', TRUE),
  #     radioButtons('sep', 'Separator',
  #                  c(Comma=',',
  #                    Semicolon=';',
  #                    Tab='\t'),
  #                  ','),
  #     tags$hr()
  #   ),
  #   
  #   
  #   mainPanel(
  #     tableOutput("contents_grid"),
  #     tableOutput("contents_sp"),
  #     tableOutput("contents_occ")
  #   )
  # )
  navbarPage("Coord", id="nav",
     tabPanel("Map",
        verticalLayout(
            sidebarPanel(
                  fileInput('file1', 'Choose grid file to upload',
                            accept = c(
                              'text/csv',
                              'text/comma-separated-values',
                              'text/tab-separated-values',
                              'text/plain',
                              '.csv',
                              '.tsv'
                            )
                  ),
                  fileInput('file2', 'Choose sp file to upload',
                            accept = c(
                              'text/csv',
                              'text/comma-separated-values',
                              'text/tab-separated-values',
                              'text/plain',
                              '.csv',
                              '.tsv'
                            )
                  ),
                  tags$hr(),
                  checkboxInput('header', 'Header', TRUE),
                  radioButtons('sep', 'Separator',
                               c(Comma=',',
                                 Semicolon=';',
                                 Tab='\t'),
                               ','),
                  tags$hr()
              ),
             tabsetPanel(
               type = "tabs",
               tabPanel("Grid", DT::dataTableOutput("grid")),
               tabPanel("Sp", DT::dataTableOutput("sp")),
               tabPanel("Map", leafletOutput("map", width="500", height="500")),
               tabPanel("Result", DT::dataTableOutput("result")
             )
            # leafletOutput("map", width="500", height="500")
        )
          # div(class="outer",
              
              # tags$head(
                # Include our custom CSS
                # includeCSS("styles.css"),
                # includeScript("gomap.js")
              # ),
              
              # If not using custom CSS, set height of leafletOutput to a number instead of percent
              # leafletOutput("map", width="500", height="500")
              
              # Shiny versions prior to 0.11 should use class = "modal" instead.
              # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
              #               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
              #               width = 330, height = "auto",
              #               
              #               h2("ZIP explorer"),
              #               
              #               selectInput("color", "Color", vars),
              #               selectInput("size", "Size", vars, selected = "adultpop"),
              #               conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
              #                                # Only prompt for threshold when coloring or sizing by superzip
              #                                numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
              #               ),
              #               
              #               plotOutput("histCentile", height = 200),
              #               plotOutput("scatterCollegeIncome", height = 250)
              # ),
              
              # tags$div(id="cite",
              #          'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
              # )
          # )
     ),
             
             # tabPanel("Data explorer",
             #          fluidRow(
             #            column(3,
             #                   selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
             #            ),
             #            column(3,
             #                   conditionalPanel("input.states",
             #                                    selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
             #                   )
             #            ),
             #            column(3,
             #                   conditionalPanel("input.states",
             #                                    selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
             #                   )
             #            )
             #          ),
             #          fluidRow(
             #            column(1,
             #                   numericInput("minScore", "Min score", min=0, max=100, value=0)
             #            ),
             #            column(1,
             #                   numericInput("maxScore", "Max score", min=0, max=100, value=100)
             #            )
             #          ),
             #          hr(),
             #          DT::dataTableOutput("ziptable")
             # ),
             
     conditionalPanel("false", icon("crosshair"))
  )
  
  
  
)))
