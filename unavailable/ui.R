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
          uiOutput("download_data_from_db")
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

shinyUI(
  dashboardPagePlus(
    skin = "green",
    header,
    sidebar,
    body
  )
)
