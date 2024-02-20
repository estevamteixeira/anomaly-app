# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("dplyr")
modules::import("plotly")
modules::import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
modules::export("trendUI")
modules::export("trendServer")


# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment.
modules::expose("utilities/data-select.R")
modules::expose("utilities/plotNSline.R")

trendUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 layout_sidebar(
  shinyjs::useShinyjs(),
  sidebar = sidebar(
   ## ICD-10 option to select ----
   # Options are categorized in groups
   selectInput(
    inputId = ns("icd10"),
    label = "Condition",
    choices = NULL,
    selected = NULL
   ),
   ## Put year options side-by-side ----
   layout_column_wrap(
    width = 1/2,
    fill = FALSE,
    selectInput(
     inputId = ns("t0"),
     label = "Initial year",
     choices = NULL,
     selected = NULL
    ),
    selectInput(
     inputId = ns("tn"),
     label = "Final year",
     choices = NULL,
     selected = NULL
    )
   ),
   selectInput(
    inputId = ns("rsk"),
    label = "Risk factor",
    choices = NULL,
    selected = NULL
    ),
   ## Sources option to select ----
   checkboxGroupInput(
    inputId = ns("src"),
    label = "Data source",
    choices = NULL,
    inline = TRUE,
    selected = NULL
    )
   ),
  layout_columns(
   col_widths = c(12),
   card(
    full_screen = TRUE,
    card_header(
     "Surveillance Over Time"
     ),
    card_body(
     plotlyOutput(ns("line"))
     )
    )
   )
  )
}

trendServer <- function(id, df1, df2, df3){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

  # Using a "server-side selectize" option that massively improves
  # performance and efficiency (for large numbers of choices)
  updateSelectInput(
   session,
   inputId = "icd10",
   choices = df1,
   selected = c("Q999")
  )

  ## Update the options for the initial year selectInput
  ## The values should reflect the data availability
  observeEvent(input$icd10,{

   updateSelectInput(
    session,
    inputId = "t0",
    choices = sort(
     unique(
      df2 %>%
       filter(Diag %in% input$icd10) %>%
       select(Birth_Year) %>%
       collect() %>%
       pull()
     )),
    selected = c(
     min(
      df2 %>%
       filter(Diag %in% input$icd10) %>%
       select(Birth_Year) %>%
       collect() %>%
       pull(), na.rm = TRUE
     )
    )
   )
  })

  ## Make the final year option greater than or equal the
  ## initial year option
  observeEvent(c(input$icd10, input$t0),{

   updateSelectInput(
    session,
    inputId = "tn",
    choices = c(
     sort(
      unique(
       df2 %>%
        filter(Diag %in% input$icd10) %>%
        select(Birth_Year) %>%
        collect() %>%
        pull()
      )))[
       c(
        sort(
         unique(
          df2 %>%
           filter(Diag %in% input$icd10) %>%
           select(Birth_Year) %>%
           collect() %>%
           pull()
         )
        )
       ) >= input$t0],
    selected = c(
     max(
      df2 %>%
       filter(Diag %in% input$icd10) %>%
       select(Birth_Year) %>%
       collect() %>%
       pull(), na.rm = TRUE
     )
    )
   )
  })

  # Using a "server-side selectize" option that massively improves
  # performance and efficiency (for large numbers of choices)
  updateSelectInput(
   session,
   inputId = "rsk",
   choices = c("None" = "none",
               df3),
   selected = c("none")
  )

  ## Get source options
  # Should reflect data availability based on filter selection
  observeEvent(c(input$icd10, input$t0,
                 input$tn, input$rsk),{

   # Choices
   ch <- as.vector(
    sort(
     unique(
      df2 %>%
       filter(Diag %in% input$icd10,
              Birth_Year >= as.numeric(input$t0),
              Birth_Year <= as.numeric(input$tn)) %>%
       select(SrceIDs) %>%
       collect() %>%
       pull()
     )))

   ## Name opts

   nam <- c("FADB", "NSAPD", "CARDIO", "CIHI", "MSI", "NeoNatal", "Others")
   names(ch) <- nam[as.numeric(ch)]

   ch_filtered <- setdiff(ch, "5")

   updateCheckboxGroupInput(
    session,
    inputId = "src",
    choices = ch,
    selected = ch_filtered
   )
  })

  # Store risk factor info
  fields <- reactive({
   if(input$rsk %in% "none")
    return(c("count_brth_yr"))

   if(input$rsk %in% "Alcohol_Use")
    return(c("count_brth_alc", input$rsk))

   if(input$rsk %in% "bmipp")
    return(c("count_brth_bmi", input$rsk))

   if(input$rsk %in% "Cannabis_Use")
    return(c("count_brth_cann", input$rsk))

   if(input$rsk %in% "diab")
    return(c("count_brth_diab", input$rsk))

   if(input$rsk %in% "SexNum")
    return(c("count_brth_sex", input$rsk))

   if(input$rsk %in% "matage")
    return(c("count_brth_matage", input$rsk))

   if(input$rsk %in% "smoker")
    return(c("count_brth_smk", input$rsk))
   })

  # `Anom` is a reactive expression whose results will depend on ----
  # the t0, tn, condition, and geo
  anom <- reactive({
   return(
    getSubsetData(df2, c("CaseID","Birth_Year", "Diag", "SrceIDs", fields())) %>%
     filter(Birth_Year >= as.numeric(input$t0),
            Birth_Year <= as.numeric(input$tn),
            Diag %in% input$icd10,
            SrceIDs %in% input$src) %>%
     distinct() %>%
     calcPrevTrend(colsToSelect = fields()) %>%
     collect()
   )
  })


  # Map output ----
  output$line <- renderPlotly({
   req(anom())
   validate(need(nrow(anom()) > 0,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))
   if(input$rsk %in% "none")
    return(
     plot_line(anom(),"prev")
    )

   plot_line_risk(anom(),"prev", input$rsk)
  })

 })
}
