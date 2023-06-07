# 'use' is similar to 'import' but instead of importing from packages,
# we import from a module.
consts <- use("constants/constants.R")


dashboardPage(
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
    introBox(data.step = 1, data.intro = consts$intro$text[1],
    # ICD-10 option to select
    # Options are categorized in groups
    introBox(data.step = 2, data.intro = consts$intro$text[2],
    selectInput(
      inputId = "icd10",
      label = shiny::HTML(
        "<p><span style='color: #008d8b'>Condition</span></p>" 
      ),
      choices = c(consts$icd10_opts[which(grepl("^Q00", consts$icd10_opts))]),
      selected = c(consts$icd10_opts[which(grepl("^Q00", consts$icd10_opts))])
    )),
    # Initial year for reporting
    introBox(data.step = 3, data.intro = consts$intro$text[3],
             fluidRow(
      column(width = 6,
             selectInput(
               inputId = "init_time",
               label = shiny::HTML(
                 "<p><span style='color: #008d8b'>Initial year</span></p>" 
               ),
               choices = c(min(consts$cd_anom$BrthYear)),
               selected = c(min(consts$cd_anom$BrthYear))
             )
      ),
      # Final year for reporting
      column(width = 6,
             selectInput(
               inputId = "end_time",
               label = shiny::HTML(
                 "<p><span style='color: #008d8b'>Final year</span></p>" 
               ),
               choices = c(max(consts$cd_anom$BrthYear)),
               selected = c(max(consts$cd_anom$BrthYear))
             )
      )
    #disable = TRUE
    )),
    ## Add horizontal gray line to separate inputs
    tags$hr(style = "border-top: 4px solid #E3E7E9;"),
    selectInput(
      inputId = "geo",
      label = shiny::HTML(
        "<p><span style='color: #008d8b'>Geography</span></p>" 
      ),
      choices = c(consts$geo_opts),
      selected = c(consts$geo_opts["csd"])
    ),
    ## Add horizontal gray line to separate inputs
    tags$hr(style = "border-top: 4px solid #E3E7E9;"),
    ## Add introduction tour button
    tagList(
      actionButton(inputId = "intro_btn",
                   label = "Introduction Tour",
                   icon = icon("info-circle"))
    )
  )),
  dashboardBody(
    tags$head(
      # Reset favicon
      tags$link(rel = "shortcut icon", href = "#"),
      # Compiled css file
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sass.min.css"),
      # tags$script(type = "text/javascript", src = "js/main.js"),
      tags$style(HTML(
        ".shiny-output-error-validation {
        color: #008D8B;
        font-weight: bold;
        font-size: 14px !important;
        }"))
    ),
    # useShinyjs(),
    introjsUI(), # must include in UI to use introBox
    tags$main(
      tags$div(
        class = "main-content-grid advanced-grid",
        # global_metrics_view$ui("global_metrics_advanced_view"),
      div(
        class = "map-grid-wrapper",
        map_view$ui("map_advanced_view")
      ),
      div(
        class = "table-grid-wrapper",
        table_view$ui("table_advanced_view")
      ),
      div(class = "line-grid-wrapper",
          line_view$ui("line_advanced_view")
      ),
      div(class = "barchart-grid-wrapper",
          upset_view$ui("bar_advanced_view")
      ),
      local_metrics_view$ui("local_metrics_advanced_view")
      )
    ),
    # You are not supposed to remove or modify this footer
    tags$footer(class = "footer", consts$rcp_legal)
  )
)

