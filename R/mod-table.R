# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("dplyr")
modules::import("DT")
modules::import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
modules::export("tableUI")
modules::export("tableServer")


# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment.
modules::expose("R/data-select.R")

tableUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 card(
    full_screen = TRUE,
    card_header(
     "Surveillance Table"
    ),
    card_body(
     DT::dataTableOutput(ns("geotable")),
    )
   )
}

tableServer <- function(id, df1){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

  # This allows to add footer with totals
  # Here we need to make use of the isolate() function
  # Othw, this object should be put inside the DT::renderDT() call

  sketch <- htmltools::withTags(table(
   tableHeader(c("GeoUID", "Name", "Prevalence"),
               escape = FALSE)
  ))

  # DataTable object
  output$geotable <- DT::renderDT({

   validate(need(nrow(df1) > 0,
                 "Sorry, there is no data available for the selected options.
            \nPlease, choose different years and/or conditions."))

   DT::datatable(
    df1 %>% as_tibble() %>% select(-geometry),
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
  DTproxy <- DT::dataTableProxy("geotable")

  return(
   list(geo_selected = reactive({

    df1[input$geotable_rows_selected,]

   })
   ))

 })
}
