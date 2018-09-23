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
    menuItem("Coord", tabName = "coord"),
    menuItem("Explore Data", tabName = "maps"),
    # menuItem("Charts", tabName = "charts"),
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
      
      fluidRow(
        # box(
        #   width = 4,
        #   collapsible = TRUE,
        #   title = "Filter",
        #   status = "warning",
        #   uiOutput("filter_sp_occ")
        # ),
        
        box(
          width = 12,
          collapsible = TRUE,
          title = "Results", 
          status = "warning",
          DT::dataTableOutput("result"),
          downloadButton("download_results", "Download")
        )
        
      )
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
          width = 12,
          collapsible = TRUE,
          title = "Filter",
          status = "warning",
          uiOutput("filter_sp_map"),
          actionButton("select_all_filter_btn", "Select all"),
          actionButton("clean_all_filter_btn", "Clean")
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          collapsible = TRUE,
          title = "Sp Coordinates", 
          status = "warning",
          plotlyOutput("scatter_plot")
        ),
        
        box(
          width = 12,
          collapsible = TRUE,
          title = "Sp Frequency", 
          status = "warning",
          plotlyOutput("sp_freq_plot")
        ), 
        
        box(
          width = 12,
          collapsible = TRUE,
          title = "Sp Outliers", 
          status = "warning",
          plotlyOutput("sp_out_freq_plot")
        )
      ),
      
      fluidRow(
        box(
          collapsible = TRUE,
          title = "Species location", 
          status = "success",
          width = 12,
          leafletOutput("map_sp", height="650")
        ),
        
        box(
          collapsible = TRUE,
          title = "Species location - clustered", 
          status = "info",
          width =12,
          leafletOutput("map_sp_clustered", height="650")
        ),
        
        box(
          collapsible = TRUE,
          title = "Centroids location", 
          status = "info",
          width = 6,
          leafletOutput("map_grid", height="650")
        )
       
      ),
      
      fluidRow(
        box(
          width = 12,
          collapsible = TRUE,
          title = "Sp occ", 
          status = "warning",
          plotlyOutput("sp_occ_scatter_plot")
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