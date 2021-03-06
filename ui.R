library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(DT)
library(plotly)
library(shinycssloaders)
library(highcharter)
library(shinyWidgets)
library(shinyjs)



header <- dashboardHeaderPlus(
  
  title = tagList(
    span(class = "logo-lg", "COORD"),
    img(src = "3d.svg" )),
  # title = "COORD (testing)",
  titleWidth = 187
  # dropdownMenu(
  #   type = "tasks",
  #   badgeStatus = "danger",
  #   taskItem(value = 20, color = "aqua", "Refactor code"),
  #   taskItem(value = 40, color = "green", "Design new layout"),
  #   taskItem(value = 60, color = "yellow", "Another task"),
  #   taskItem(value = 80, color = "red", "Write documentation")
  # ),
  # dropdownBlock(
  #   id = "mydropdown",
  #   title = "Dropdown 1",
  #   icon = icon("sliders"),
  #   sliderInput(
  #     inputId = "n",
  #     label = "Number of observations",
  #     min = 10, max = 100, value = 30
  #   ),
  #   prettyToggle(
  #     inputId = "na",
  #     label_on = "NAs keeped",
  #     label_off = "NAs removed",
  #     icon_on = icon("check"),
  #     icon_off = icon("remove")
  #   )
  # )
  # enable_rightsidebar = TRUE,
  # rightSidebarIcon = "user"
  
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    # menuItem("Inputs from DBs", tabName = "input"),
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Species Data",
             icon = icon("th"),
             menuSubItem(
               "Upload File",
               tabName = "upload_species_file"
             ),
             menuSubItem(
               "From Data Bases",
               tabName = "from_data_bases"
             )
    ),
    menuItem("Predictors",
             icon = icon("leaf"),
             menuSubItem(
               "Upload Files",
               tabName = "upload_predictors"
             ),
             menuSubItem(
               "From Data Bases",
               tabName = "não_existe_ainda"
             )
    ),
    # menuItem("Summary", tabName = "summary", icon = icon("th-list")),
    menuItem("Exploratory Data Analysis", icon = icon("map"),
             menuSubItem(
               "Spatial Distribution",
               tabName = "maps"
             ),
             menuSubItem(
               "Summary",
               tabName = "summary"
             )),
    menuItem("Presence/Absense", tabName = "coord", icon = icon("th-list")),
    menuItem("Predict", tabName = "predict", icon = icon("cube"))
  )
)


body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem(
      "home",
      fluidRow(
        infoBox("Download Species Data", "Integration with GBIF and +9 databases", width = 6, icon = icon("th"), fill = TRUE),
        infoBox("Download Predictors Data", "Integration with Wordclim", width = 6, icon = icon("leaf"), fill = TRUE),
        infoBox("Explore the data", "See the distribution of your data with interactive maps and charts", width = 6, icon = icon("map"), fill = TRUE),
        infoBox("Presence and Absence", "You can generate the presence/absence file from yout own data", width = 6, icon = icon("th-list"), fill = TRUE),
        infoBox("Predict", "Run machine learning algorithms, make an ensemble model and generate the predictive maps.", width = 6, icon = icon("cube"), fill = TRUE)
      )
    ),
    tabItem(
      "upload_species_file",
      fluidRow(
        box(
          width = 12,
          collapsible = TRUE,
          title = "Upload",
          status = "primary",
          fileInput('species_file', 'Upload species file',
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
          width = 12,
          collapsible = TRUE,
          title = "Species File",
          # status = "success",
          dataTableOutput("species_file_table")
        )
      )
    ),
    
    
    tabItem(
      "coord",
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
      "from_data_bases",
      fluidRow(
        box(
          width = 12,
          collapsible = FALSE,
          materialSwitch(inputId = "upload_file_switch_btn", label = "Upload file: ", status = "primary", right = FALSE)
        ),
        
        conditionalPanel(
          "input.upload_file_switch_btn",
          box(
            width = 12,
            collapsible = TRUE,
            title = "Instructions",
            status = "primary",
            solidHeader = TRUE,
            "Your CSV file must contain a column named 'SP'.",
            downloadButton("download_sample_sp", "Download Sample")
          )
        ),
        
        conditionalPanel(
          "input.upload_file_switch_btn",
          box(
            width = 12,
            collapsible = FALSE,
            # materialSwitch(inputId = "upload_file_switch_btn", label = "Upload file with specie(s) name:", status = "primary", right = FALSE),
            # tags$hr(),
            fileInput('file_species_download', "Upload species file:",
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
            # conditionalPanel(
            #   "input.file_species_download",
            uiOutput("filter_sp_download"),
            actionButton("selet_all_download_sp_btn", "Select all"),
            actionButton("clean_download_sp_btn", "Clean")
            
            # )
          )
        ),
        
        conditionalPanel(
          "!input.upload_file_switch_btn",
          box(
            width = 12,
            collapsible = FALSE,
            textInput("sp_name", "Type species name: "),
            "Ex.: Buceros rhinoceros"
          )
        ),
        
        
        box(
          width = 12,
          collapsible = FALSE,
          # title = "Download Species Data",
          status = "primary",
          
          # tags$hr(),
          uiOutput("select_DB"),
          tags$hr(),
          actionButton("download_from_DB", "Search")
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          collapsible = TRUE,
          title = "Downloaded Data",
          status = "primary",
          DT::dataTableOutput("show_downloaded_data") %>% withSpinner(color="#0dc5c1"),
          downloadButton("download_data_from_db_btn", "Download")
        )
        
      ),
      fluidRow(
        infoBoxOutput("sp_download_duplicated"),
        infoBoxOutput("sp_download_na"),
        infoBoxOutput("sp_download_count"),
        infoBoxOutput("sp_download_db")
      )
    ),
    
    tabItem(
      "upload_predictors",
      fluidRow(
        box(
          width = 12,
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
          DT::dataTableOutput("duplicated_sp"),
          downloadButton("download_duplicated_sp", "Download")
        ),
        box(
          width = 4,
          collapsible = TRUE,
          title = "Duplicated coordinates in grid",
          status = "success",
          DT::dataTableOutput("duplicated_grid"),
          downloadButton("download_duplicated_grid", "Download")
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
        infoBoxOutput("species_infobox"),
        infoBoxOutput("predictors_infobox")
      ),
      
      fluidRow (
        box(
          width = 6,
          collapsible = TRUE,
          title = "Algorithm",
          status = "primary",
          uiOutput("select_algorithm"),
          uiOutput("select_eval_method"),
          textInput("training_set", "Training Set (%): "),
          textInput("number_of_executions", "Number of executions: "),
          tags$hr(),
          materialSwitch(inputId = "ensemble_switch_btn", label = "Ensemble: ", status = "primary", right = FALSE),
          conditionalPanel(
            "input.ensemble_switch_btn",
            uiOutput("select_eval_method_ensemble")
          ),
          actionButton("run_algorithm_btn", "Run", width = "100%")
        ),
        
        box(
          width = 6,
          collapsible = TRUE,
          title = "Execution Info",
          status = "primary",
          DT::dataTableOutput("info_training_testing") %>% withSpinner(color="#0dc5c1")
        )
      ),
      
      fluidRow(
        tabBox(
          title = "Model Evaluations",
          width = 12,
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "info_eval_tab_test",
          tabPanel("ROC", DT::dataTableOutput("info_eval_AUC")%>% withSpinner(color="#0dc5c1")),
          tabPanel("TSS", DT::dataTableOutput("info_eval_TSS")%>% withSpinner(color="#0dc5c1"))
        )
      ),
      
      
      fluidRow(
        
        # box(
        #   width = 6,
        #   collapsible = TRUE,
        #   title = "Evaluations",
        #   status = "primary",
        #   plotOutput("show_auc_curve") %>% withSpinner(color="#0dc5c1")
        # ),
        
        box(
          width = 12,
          collapsible = TRUE,
          title = "Potential Distribution Map",
          status = "primary",
          materialSwitch(inputId = "group_pred_maps_btn", label = "Group: ", status = "primary", right = FALSE),
          conditionalPanel(
            "!input.group_pred_maps_btn",
            uiOutput("select_predictive_maps")
          ),
          plotOutput("show_predict_map") %>% withSpinner(color="#0dc5c1"),
          downloadButton("download_distribution_map", "Download")
        ),
        
        box(
          width = 12,
          collapsible = TRUE,
          title = "Ensemble Map",
          status = "primary",
          plotOutput("show_ensemble_map") %>% withSpinner(color="#0dc5c1")
        )
      )
    )
  )
)

shinyUI(
  dashboardPagePlus(
    skin = "green",
    header,
    sidebar,
    body
  )
)
