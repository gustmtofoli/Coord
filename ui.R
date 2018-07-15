library(shinydashboard)
library(leaflet)
library(DT)


header <- dashboardHeader(
  title = "Moon",
  titleWidth = 187
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Coord", tabName = "coord", badgeLabel = c("Updated"), badgeColor = "green"),
    menuItem("Maps",  tabName = "maps", badgeLabel = "Updated", badgeColor = "green"),
    menuItem("Charts", tabName = "charts", badgeLabel = "In Progress", badgeColor = "red"),
    menuItem("Summary", tabName = "summary", badgeLabel = "In Progress", badgeColor = "red")
   
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
          width = 12,
          collapsible = TRUE,
          title = "Results", 
          status = "warning",
          DT::dataTableOutput("result"),
          downloadButton("download_results", "Download")
          # actionButton("download_results", "Download")
        )
      )
    ),
    
    tabItem(
      "summary",
      fluidRow(
        box(
          collapsible = TRUE,
          title = "Species", 
          status = "success",
          DT::dataTableOutput("species")
        )
      )
    ),
    
    tabItem(
      "maps",
      fluidRow(
        box(
          collapsible = TRUE,
          title = "Species location", 
          status = "success",
          width = 6,
          leafletOutput("map_sp", height="650")
        ),
        
        box(
          collapsible = TRUE,
          title = "Centroids location", 
          status = "info",
          width = 6,
          leafletOutput("map_grid", height="650")
        )
      )
    ),

    tabItem(
      "charts",
      fluidRow(
        box(
          width = 4,
          collapsible = TRUE,
          status = "success",
          title = "Configurations",
          uiOutput("chart_checkbox")
        ),
        box(
          width = 8,
          collapsible = TRUE,
          title = "Sp Scatter plot", 
          status = "warning",
          plotOutput("scatter_plot")
        )
      )
    )
  )
)



shinyUI(
dashboardPage(
  skin = "green",
  header,
  sidebar,
  body
)
)