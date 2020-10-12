library(spocc)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(sp)
library(stringr)

header <- dashboardHeaderPlus(
    title = tagList(span(class = "logo-lg", "COORD"), img(src = "3d.svg" )),
    titleWidth = 187
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            "Species data",
             icon = icon("th"),
             menuSubItem(
               "From databases",
               tabName = "from_data_bases"
             )
        )
    )
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
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
            "Your CSV file must have a column named 'sp'.",
            downloadButton("download_sample_sp", "Download sample")
          )
        ),
        
        conditionalPanel(
          "input.upload_file_switch_btn",
          box(
            width = 12,
            collapsible = FALSE,
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
            uiOutput("filter_sp_download"),
            actionButton("selet_all_download_sp_btn", "Select all", icon = icon("check-double")),
            actionButton("clean_download_sp_btn", "Clean", icon = icon("trash"))
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
          uiOutput("select_DB"),
          tags$hr(),
          actionButton("download_from_DB", "Search", icon = icon("search"))
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          collapsible = TRUE,
          title = "Downloaded and preprocessed data",
          DT::dataTableOutput("show_downloaded_data") %>% withSpinner(color="#0dc5c1"),
          tags$hr(),
          column(2,
              uiOutput("download_data_from_db"),
          ),
          column(2,
              uiOutput("show_distribution_map")
          )
        )
      ),
        
      fluidRow(
          uiOutput("sp_download_duplicated"),
          uiOutput("sp_download_na"),
          uiOutput("sp_download_count"),
          uiOutput("sp_download_db")
      )
    )
  )
)

ui <- fluidPage(
    shinyUI(
        dashboardPagePlus(
            skin = "green",
            header,
            sidebar,
            body
        )
    )
)

# ====================================================================================

server <- function(input, output, session) {

  variables <- reactiveValues(grid_read = NULL, 
                              sp_read = NULL, 
                              species_file = NULL,
                              sp_download_db = NULL,
                              sp_download_db_processed = NULL,
                              sp_without_outliers = NULL, 
                              results = NULL)
  
  status <- reactiveValues(species_status = FALSE,
                           predictors_status = FALSE)
  
  secondary_variables <- reactiveValues(duplicated_sp = NULL, 
                                        duplicated_grid = NULL,
                                        original_sp_nrow = 0,
                                        group_predictive_maps = FALSE)
  
  filter_variables <- reactiveValues(sp_filter = NULL)
  
  predict_variables <- reactiveValues(algorithms = NULL, 
                                      training = NULL, 
                                      testing = NULL,
                                      roc = NULL,
                                      auc = NULL,
                                      predictive_map = NULL,
                                      predictive_model = NULL,
                                      ensemble_model = NULL,
                                      execution_time = 0,
                                      can_run_algorithm = FALSE,
                                      ensemble_map = NULL,
                                      data_bases = c("gbif", 
                                                     "ecoengine",
                                                     "bison",
                                                     "inat",
                                                     "ebird",
                                                     "vertnet",
                                                     "idigbio",
                                                     "ala"))
  
  predict_variables$algorithms <- data.frame(name = c("GAM - Generalized Linear Model", 
                                                      "RF - Random Forest",
                                                      "GLM - Logistic Regression",
                                                      "GBM - Gradient Boosting Machine",
                                                      "CTA - Classification Tree Analysis",
                                                      "ANN - Artificial Neural Network",
                                                      "BIOCLIM - Surface Range Envelop (SRE)",
                                                      "FDA - Flexible Discriminant Analysis",
                                                      "MARS - Multiple Adaptive Regression Splines "),
                                                       
                                             method = c("GAM", 
                                                        "RF",
                                                        "GLM",
                                                        "GBM",
                                                        "CTA",
                                                        "ANN",
                                                        "SRE",
                                                        "FDA",
                                                        "MARS")
                                             )
  
  # ========== SERVICES ===============================================================
  source("SpeciesDataFromDataBasesService.R", local=TRUE)
  # ===================================================================================
  
  # ========== OUTPUTS ===============================================================
  source("SpeciesDataFromDataBasesOutputs.R", local=TRUE)
  # ===================================================================================
}

shinyApp(ui = ui, server = server)