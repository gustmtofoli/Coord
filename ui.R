library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)


header <- dashboardHeader(
  title = "Moon",
  titleWidth = 187
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Coord", 
      tabName = "coord"
    ),
              
    
    menuItem("Maps",  tabName = "maps"),
    menuItem("Charts", tabName = "charts"),
    menuItem("Summary", tabName = "summary")
   
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
      
      # fluidRow(
      
        #  box(
        #    width = 8,
        #    collapsible = TRUE,
        #    title = "Species per occurrence",
        #    status = "warning",
        #    DT::dataTableOutput("sp_per_occ")
        # )
      # ),
  
      fluidRow(
        box(
          width = 4,
          collapsible = TRUE,
          title = "Filter",
          status = "warning",
          uiOutput("filter_sp_occ")
        ),
        
        box(
          width = 8,
          collapsible = TRUE,
          title = "Results", 
          status = "warning",
          DT::dataTableOutput("result"),
          downloadButton("download_results", "Download")
        )
        
      )
      
      
      
      # fluidRow(
      #   box(
      #     width = 8,
      #     collapsible = TRUE,
      #     title = "Species per occurrence",
      #     status = "warning",
      #     plotlyOutput("dfdfds")
      #   )
      # )
    ),
      
    
    
    
    tabItem(
      "summary",
      fluidRow(
        box(
          collapsible = TRUE,
          title = "Species Frequency",
          status = "success",
          DT::dataTableOutput("species"),
          downloadButton("download_species_freq", "Download")
        ),
        box(
          collapsible = TRUE,
          title = "Species Outliers",
          status = "success",
          DT::dataTableOutput("species_outliers"),
          downloadButton("download_species_outliers", "Download")
        ),
        box(
          collapsible = TRUE,
          title = "Species Outliers Frequency",
          status = "success",
          DT::dataTableOutput("species_outliers_freq"),
          downloadButton("download_species_outliers_freq", "Download")
        )
      )
    ),
    
    tabItem(
      "maps",
      fluidRow(
        box(
          width = 4,
          collapsible = TRUE,
          title = "Filter",
          status = "warning",
          uiOutput("filter_sp_map")
        )
      ),
      
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
        ),
        
        box(
          collapsible = TRUE,
          title = "Species location - clustered", 
          status = "info",
          width = 6,
          leafletOutput("map_sp_clustered", height="650")
        )
      )
    ),

    tabItem(
      "charts",
      fluidRow(
        box(
          width = 12,
          collapsible = TRUE,
          title = "Sp Scatter plot", 
          status = "warning",
          plotlyOutput("scatter_plot")
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