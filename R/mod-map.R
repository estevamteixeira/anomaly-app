# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("dplyr")
modules::import("geoarrow") # remotes::install_github("paleolimbot/geoarrow")
modules::import("DT")
modules::import("leaflet")
modules::import("sf")
modules::import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
modules::export("mapUI")
modules::export("mapServer")


# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment.
modules::expose("R/data-select.R")
modules::expose("R/plotNSmap.R")

mapUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 layout_sidebar(
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
    inputId = ns("geo"),
    label = "Geographic unit",
    choices = NULL,
    selected = NULL
   )
  ),
  layout_columns(
   col_widths = c(8,4),
   card(
   full_screen = TRUE,
   card_header(
    tooltip(
     span("Surveillance Map",
          bsicons::bs_icon("question-circle")
     ),
     "Geographical links were determined based on",strong("mothers'"),
     "postal codes at the time of admission for delivery."
    ),
    # popover(
    #  bsicons::bs_icon("download"),
    #  radioButtons(ns("map_down"),
    #               label = NULL, inline = TRUE,
    #               c(".png",".jpeg")),
    #  title = "Download map"
    # ),
    class = "d-flex justify-content-between align-items-center"
    ),
   card_body(
    leafletOutput(ns("map")),
   )
  ),
  card(
   full_screen = TRUE,
   card_header(
    "Surveillance Table"
   ),
   card_body(
    DT::dataTableOutput(ns("table")),
   )
  ))
  )
}

mapServer <- function(id, df1, df2, df3, df4, df5, df6, df7){
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
   inputId = "geo",
   choices = df3,
   selected = c("cd")
  )

  # Select the shape file depending on the selected geographic unit ----
  shape <- reactive({
   if(input$geo %in% "cd")  return(df4)
   if(input$geo %in% "cl")  return(df5)
   if(input$geo %in% "chn") return(df6)
   if(input$geo %in% "hr")  return(df7)
  })

  # Select field depending on the selected geographic unit ----
  field <- reactive({
   if(input$geo %in% "cd")  return(c("CDuid","count_brth_cd"))
   if(input$geo %in% "cl")  return(c("CLuid","count_brth_cl"))
   if(input$geo %in% "chn") return(c("CHNuid","count_brth_chn"))
   if(input$geo %in% "hr")  return(c("HRuid","count_brth_hr"))
  })

  # `Anom` is a reactive expression whose results will depend on ----
  # the t0, tn, condition, and geo
  anom <- reactive({
   return(
    getSubsetData(df2, c("CaseID","Birth_Year", "Diag", field())) %>%
     filter(Birth_Year >= as.numeric(input$t0),
            Birth_Year <= as.numeric(input$tn),
            Diag %in% input$icd10) %>%
     distinct() %>%
     calcPrev(colsToSelect = field()) %>%
     collect()
   )
  })

  # Create `geo` data: merge `Anom()` with the shape file to draw the map ----

  geo <- reactive({
   merge(
    shape() ,
    anom(),
    by.x = names(shape())[which(grepl("id",tolower(names(shape()))))],
    by.y = names(anom())[which(grepl("id",tolower(names(anom()))))],
    all.x = TRUE
   ) %>%
    geoarrow::geoarrow_collect_sf()
  })

  # Map output ----
  output$map <- renderLeaflet({
   validate(need(nrow(geo()) > 0,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))

   # withProgress(message = "Building map", value = 0, {
   #  for (i in 1:10) {
   #   incProgress(1/10)
   #   Sys.sleep(0.25)
   #  }
   # })
   nsmap(geo(), "prev")
   })

  # Map download ----
  # output$map_down <- downloadHandler(
  #  filename = ""
  # )

  # This allows to add footer with totals
  # Here we need to make use of the isolate() function
  # Othw, this object should be put inside the DT::renderDT() call

  sketch <- htmltools::withTags(table(
   tableHeader(c("GeoUID", "Name", "Prevalence"),
               escape = FALSE),
   # tableFooter(
   #   c("", "Grand Total",
   #     format(
   #       c(sum(unique(consts$cd_stats[,.(BrthYear, CD_UID, cd.count_anom, cat_tier2)])$cd.count_anom),
   #         sum(unique(consts$cd_stats[,.(BrthYear, total_lvb, CD_UID)])$total_lvb),
   #         round(1000*sum(unique(consts$cd_stats[,.(BrthYear, CD_UID, cd.count_anom, cat_tier2)])$cd.count_anom)/sum(unique(consts$cd_stats[,.(BrthYear, total_lvb, CD_UID)])$total_lvb),
   #               0)
   #       ),
   #       big.mark = ","
   #     )
   #   )
   # )
  ))

  # DataTable object
  output$table <- DT::renderDT({

   validate(need(nrow(geo()) > 0,
                 "Sorry, there is no data available for the selected options.
            \nPlease, choose different years and/or conditions."))

   DT::datatable(
    geo() %>% as_tibble() %>% select(-geometry) %>% arrange(desc(prev)),
    container = sketch,
    rownames = FALSE,
    style = "auto",
    selection = 'single',
    extensions = "Buttons",
    caption = "cases per 10,000 total births",
    options = list(
     dom = 'B<t>ftp',
     extensions = "Buttons",
     search = list(regex = TRUE, caseInsensitive = TRUE),
     paging = TRUE,
     pageLength = 5,
     width = c("30px","100px","30px","30px","100px"),
     ordering = TRUE,
     stateSave = TRUE,
     # buttons = list('copy', 'print', list(
     #   extend = 'collection',
     #   buttons = list(
     #     list(extend = 'csv', filename = "geo_view")
     #      # list(extend = 'excel', filename = "trend_results"),
     #      # list(extend = 'pdf', filename = "trend_results")
     #   ),
     #   text = 'Download'
     # )),
     columnDefs = list(list(visible = FALSE,
                            targets = c(0)))
    )
   ) |>
    DT::formatRound(
     columns = c("prev"),
     digits = 2,
     mark = ","
    )
  }, server = FALSE)

  # DataTable proxy object ----
  DTproxy <- DT::dataTableProxy("table")

  return(
   list(line_selected = reactive({
    geo()[input$table_rows_selected,]
   })
   ))

  })
}
