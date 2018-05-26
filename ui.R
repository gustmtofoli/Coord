library(shinydashboard)
library(leaflet)
library(DT)


header <- dashboardHeader(
  titleWidth = 0
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Coord", tabName = "coord", badgeLabel = c("In progress"), badgeColor = "red"),
    menuItem("Indicators", tabName = "indicators", badgeLabel = "In progress", badgeColor = "red")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem("coord",
      fluidRow(
        box(
          width = 4,
          collapsible = TRUE,
          title = "Upload", 
          status = "primary",
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
        
        box(
          width = 4,
          collapsible = TRUE,
          title = "Grid file", 
          status = "primary",
          dataTableOutput("grid")
        ),
        
        box(
          width = 4,
          collapsible = TRUE,
          title = "Sp File", 
          status = "primary",
          dataTableOutput("sp")
        )
      ),
  
      fluidRow(
        box(
          collapsible = TRUE,
          title = "Results", 
          status = "primary",
          DT::dataTableOutput("result")
        )
      )
    ),
    
    
    tabItem(
      "indicators",
      fluidRow(
        infoBoxOutput("total_grid_coordinates"),
        
        box(
          collapsible = TRUE,
          title = "Map", 
          status = "primary",
          tabPanel("Species location", leafletOutput("map", width="300", height="300"))
        )
      )
    )
  )
)

shinyUI(
dashboardPage(
  skin = "blue",
  header,
  sidebar,
  body
)
)