##------- set working directory --------
#setwd("H:/RCP/RCP_ALL/SCA-NS/Sentinel Anomalies Maps/anomaly-app-overview")

#path <- "H:/RCP/RCP_ALL/SCA-NS/Sentinel Anomalies Maps/anomaly-app-overview"
library(data.table)
library(geojsonsf)
library(leaflet)
library(modules)
library(sass)
library(shiny)
library(shinydashboard)


consts <- use("constants/constants.R")
map_view <- use("modules/map_view.R")
global_metrics_view <- use("modules/global_metrics_view.R")
local_metrics_view <- use("modules/local_metrics_view.R")
county_view <- use("modules/county_view.R")

# Compile sass to css
sass(
  sass::sass_file("styles/main.scss"),
  cache = NULL,
  options = sass_options(output_style = "compressed"),
  output = "www/css/sass.min.css"
)

ui <- dashboardPage(
  ## Header
  dashboardHeader(
    ## App title visible in browser tab
    title = consts$app_title,
    ## App title visible
    ## 'li' tag defines a list item used inside menu lists
    tags$li(class = "dropdown title", tags$h1(consts$app_title)),
    ## App current version
    # tags$li(class = "dropdown version", tags$p(consts$app_version)),
    # App time range
    tags$li(class = "dropdown time-range", tags$p(consts$app_time_range)),
    ## App logo
    tags$li(
      class = "dropdown logo", consts$rcp_logo
    )
  ),
  # Sidebar
  dashboardSidebar(
    # ICD-10 option to select
    # Options are categorized in groups
    selectInput(
        inputId = "icd10",
        label = shiny::HTML(
          "<p><span style='color: #008d8b'>Condition</span></p>" 
          ),
        choices = c(consts$icd10_opts[which(grepl("^Q00", consts$icd10_opts))]),
        selected = c(consts$icd10_opts[which(grepl("^Q00", consts$icd10_opts))])
      ),
    # Initial year for reporting
    fluidRow(
      column(width = 6,
        selectInput(
        inputId = "init_time",
        label = shiny::HTML(
          "<p><span style='color: #008d8b'>Initial year</span></p>" 
        ),
        choices = c(min(consts$cd_stats$BrthYear)),
        selected = c(min(consts$cd_stats$BrthYear))
        )
        ),
   # Final year for reporting
     column(width = 6,
       selectInput(
       inputId = "end_time",
       label = shiny::HTML(
       "<p><span style='color: #008d8b'>Final year</span></p>" 
     ),
     choices = c(max(consts$cd_stats$BrthYear)),
     selected = c(max(consts$cd_stats$BrthYear))
     )
   )
   )
    #disable = TRUE
  ),
  dashboardBody(
    tags$head(
      # Reset favicon
      tags$link(rel = "shortcut icon", href = "#"),
      # Compiled css file
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sass.min.css"),
      # tags$script(type = "text/javascript", src = "js/main.js"),
      tags$style(HTML(
        ".shiny-output-error-validation {
        color: #008d8b;
        font-weight: bold;
        font-size: 14px !important;
        }"))
    ),
    tags$main(
      tags$div(
        class = "main-content-grid advanced-grid",
        # global_metrics_view$ui("global_metrics_advanced_view"),
        div(
          class = "map-grid-wrapper",
          map_view$ui("map_advanced_view")
        ),
        div(class = "table-grid-wrapper",
            county_view$ui("county_advanced_view")
            ),
        local_metrics_view$ui("local_metrics_advanced_view")
        )
    ),
    # You are not supposed to remove or modify this footer
    tags$footer(class = "footer", consts$rcp_legal)
  )
)
  

server <- function(input, output, session){
  # Using a "server-side selectize" option that massively improves 
  # performance and efficiency (for large numbers of choices)
  updateSelectInput(
    session,
    inputId = "icd10",
    choices = list("All conditions" = "0",
      `Congenital malformations of the nervous system`= 
        consts$icd10_opts[which(grepl("^Q(0[0-7])", consts$icd10_opts))],
      `Congenital malformations of eye, ear, face and neck` =
        consts$icd10_opts[which(grepl("^Q(1[0-8])", consts$icd10_opts))],
      `Congenital malformations of the circulatory system` =
        consts$icd10_opts[which(grepl("^Q(2[0-8])", consts$icd10_opts))],
      `Congenital malformations of the respiratory system` =
        consts$icd10_opts[which(grepl("^Q(3[0-4])", consts$icd10_opts))],
      `Cleft lip and cleft palate` = 
        consts$icd10_opts[which(grepl("^Q(3[5-7])", consts$icd10_opts))],
      `Other congenital malformations of the digestive system` =
        consts$icd10_opts[which(grepl("^Q(3[8-9]|4[0-5])", consts$icd10_opts))],
      `Congenital malformations of genital organs` =
        consts$icd10_opts[which(grepl("^Q(5[0-6])", consts$icd10_opts))],
      `Congenital malformations of the urinary system` =
        consts$icd10_opts[which(grepl("^Q(6[0-4])", consts$icd10_opts))],
      `Congenital malformations and deformations of the musculoskeletal system` =
        consts$icd10_opts[which(grepl("^Q(6[5-9]|7[0-9])", consts$icd10_opts))],
      `Other congenital malformations` =
        consts$icd10_opts[which(grepl("^Q(8[0-9])", consts$icd10_opts))],
      `Chromosomal abnormalities, not elsewhere classified` =
        consts$icd10_opts[which(grepl("^Q(9[0-9])", consts$icd10_opts))]
    ),
    selected = c("0")
  )
  
  ## Update the options for the initial year selectInput
  updateSelectInput(
    session,
    inputId = "init_time",
    choices = c(sort(unique(consts$cd_stats$BrthYear))),
    selected = c(min(consts$cd_stats$BrthYear)) 
  )
  
  ## Make the final year option to be greater than or equal the
  ## initial year option
  observeEvent(input$init_time,{
    updateSelectInput(
      session,
      inputId = "end_time",
      choices = c(sort(unique(consts$cd_stats$BrthYear)))[
        c(sort(unique(consts$cd_stats$BrthYear))) >= input$init_time],
      selected = c(max(consts$cd_stats$BrthYear))
    )
  })
  
  initial_year <- reactive({ input$init_time})
  final_year <- reactive({ input$end_time})
  condition <- reactive({ input$icd10})
  
  session$userData$county_view <- county_view$init_server(
    "county_advanced_view",
    df = consts$cd_stats,
    y1 = initial_year,
    y2 = final_year,
    q = condition
    )

  # global_metrics_view$init_server("global_metrics_advanced_view")

  local_metrics_view$init_server("local_metrics_advanced_view")
  
  session$userData$map_view <- map_view$init_server(
    "map_advanced_view",
    df = consts$cd_stats,
    y1 = initial_year,
    y2 = final_year,
    q = condition
  )
  
}

shinyApp(ui = ui, server = server)
