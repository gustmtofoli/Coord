library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(shinycssloaders)
library(highcharter)


header <- dashboardHeader(
  title = "Coord",
  titleWidth = 187
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    # menuItem("Inputs from DBs", tabName = "input"),
    menuItem("Presence/Absence", tabName = "coord"),
    menuItem("Explore Data", tabName = "maps"),
    menuItem("Summary", tabName = "summary"),
    menuItem("Predict", tabName = "predict")
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
          fileInput('file2', "Choose species presence file to upload",
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
          title = "Species presence File", 
          status = "info",
          dataTableOutput("sp")
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          collapsible = TRUE,
          title = "Presence and absence", 
          status = "warning",
          DT::dataTableOutput("result"),
          downloadButton("download_results", "Download")
        )
        
      )
    ),
      
    tabItem(
      "summary",
      fluidRow(
        infoBoxOutput("sp_total_percent"),
        infoBoxOutput("sp_duplicated_percent"),
        infoBoxOutput("sp_outliers_percent"),
        infoBoxOutput("grid_duplicated_percent"),
        infoBoxOutput("number_of_species")
      ),
      
      fluidRow(
        box(
          width = 4,
          collapsible = TRUE,
          title = "Species Frequency",
          status = "success",
          DT::dataTableOutput("species"),
          downloadButton("download_species_freq", "Download")
        ),
        box(
          width = 8,
          collapsible = TRUE,
          title = "Species Outliers",
          status = "success",
          DT::dataTableOutput("species_outliers"),
          downloadButton("download_species_outliers", "Download")
        )
      ),
      fluidRow(
        box(
          width = 4,
          collapsible = TRUE,
          title = "Species Outliers Frequency",
          status = "success",
          DT::dataTableOutput("species_outliers_freq"),
          downloadButton("download_species_outliers_freq", "Download")
        ),
        box(
          width = 4,
          collapsible = TRUE,
          title = "Duplicated records in species",
          status = "success",
          DT::dataTableOutput("duplicated_sp")
        ),
        box(
          width = 4,
          collapsible = TRUE,
          title = "Duplicated coordinates in grid",
          status = "success",
          DT::dataTableOutput("duplicated_grid")
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
          plotlyOutput("scatter_plot") %>% withSpinner(color="#0dc5c1")
        ),
        
        box(
          width = 12,
          collapsible = TRUE,
          title = "Sp Frequency", 
          status = "warning",
          plotlyOutput("sp_freq_plot") %>% withSpinner(color="#0dc5c1")
        )
      ),
      
      fluidRow(
        box(
          collapsible = TRUE,
          title = "Species location", 
          status = "success",
          width = 6,
          leafletOutput("map_sp", height="650") %>% withSpinner(color="#0dc5c1")
        ),
        
        box(
          collapsible = TRUE,
          title = "Species location - clustered", 
          status = "info",
          width =6,
          leafletOutput("map_sp_clustered", height="650") %>% withSpinner(color="#0dc5c1")
        ),
        
        box(
          collapsible = TRUE,
          title = "Centroids location", 
          status = "info",
          width = 6,
          leafletOutput("map_grid", height="650") %>% withSpinner(color="#0dc5c1")
        ),
        
        box(
          width = 6,
          collapsible = TRUE,
          title = "Occurences", 
          status = "warning",
          leafletOutput("map_grid_occ", height="650") %>% withSpinner(color="#0dc5c1")
        )
       
      )
      
    ),
    
    tabItem(
      "predict",
      fluidRow(
        box(
          width = 6,
          collapsible = TRUE,
          title = "Upload Predictors",
          status = "primary",
          fileInput('predictors_files', 'Predictors',
                    accept = c(
                      '.tif'
                    ),
                    multiple = TRUE
          ),
          uiOutput("show_predictors"),
          plotOutput("show_predictors_test") %>% withSpinner(color="#0dc5c1")
        ),
        
        box(
          width = 6,
          collapsible = TRUE,
          title = "Presence/Absence File", 
          status = "primary",
          
          fileInput('occ_file', 'Occurrence file',
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
                       ',')
        ),
        
          box(
            width = 6,
            collapsible = TRUE,
            title = "[TEST] Download from Data Bases", 
            status = "primary",
            textInput("sp_name", "Species name: "),
            uiOutput("select_DB"),
            actionButton("download_from_DB", "Download")
          )
        
      ),
      
      fluidRow(
        box(
          width = 6,
          collapsible = TRUE,
          title = "Algorithm", 
          status = "primary",
          uiOutput("select_algorithm"),
          textInput("training_set", "Training Set (%): "),
          actionButton("run_algorithm_btn", "Run", width = "100%")
        )
      ),
      
     
      
      fluidRow(
        box(
          width = 6,
          collapsible = TRUE,
          title = "AUC",
          status = "primary",
          plotOutput("show_auc_curve") %>% withSpinner(color="#0dc5c1")
        ),
        
        box(
          width = 6,
          collapsible = TRUE,
          title = "Predictive Map",
          status = "primary",
          plotOutput("show_predict_map") %>% withSpinner(color="#0dc5c1")
        ),
        
        box(
          width = 6,
          collapsible = TRUE,
          title = "Execution Info",
          status = "primary",
          DT::dataTableOutput("info_training_testing") %>% withSpinner(color="#0dc5c1")
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