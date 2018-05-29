library(shinydashboard)
library(leaflet)
library(DT)


header <- dashboardHeader(
  title = "Moon",
  titleWidth = 187
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
          status = "success",
          dataTableOutput("grid")
        ),
        
        box(
          width = 4,
          collapsible = TRUE,
          title = "Sp File", 
          status = "info",
          dataTableOutput("sp")
        )
      ),
  
      fluidRow(
        box(
          collapsible = TRUE,
          title = "Results", 
          status = "warning",
          DT::dataTableOutput("result"),
          actionButton("download_results", "Download")
        )
      )
    ),
    
    
    tabItem(
      "indicators",
      fluidRow(
        box(
          collapsible = TRUE,
          title = "Species", 
          status = "success",
          DT::dataTableOutput("species")
        ),
        
        box(
          collapsible = TRUE,
          title = "Species location", 
          status = "success",
          leafletOutput("map_sp", width="300", height="300")
        ),
        
        box(
          collapsible = TRUE,
          title = "Centroids location", 
          status = "info",
          leafletOutput("map_grid", width="300", height="300")
        ),
        
        box(
          collapsible = TRUE,
          title = "Sp Scatter plot", 
          status = "warning",
          plotOutput("scatter_plot"),
          actionButton("download_scatterplot", "Download")
        ),
        
        box(
          collapsible = TRUE,
          title = "Sp Boxplot", 
          status = "warning",
          plotOutput("boxplot"),
          actionButton("download_boxplot", "Download")
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