# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
import("arrow")
import("bslib")
import("dplyr")
import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
export("sideUI")
export("sideServer")

# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment
expose("R/data-select.R")

sideUI <- function(id){
 # This ns <- NS() structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 sidebar(
  # ICD-10 option to select
   # Options are categorized in groups
   selectInput(
    inputId = ns("icd10"),
    label = "Condition",
    choices = NULL,
    selected = NULL
    ),
 # Put year options side-by-side
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
   )
 )

}

sideServer <- function(id, df1, df2){
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
              filter(toupper(Diag) %in% toupper(input$icd10)) %>%
              select(Birth_Year) %>%
              collect() %>%
              pull()
          )),
        selected = c(
          min(
            df2 %>%
            filter(toupper(Diag) %in% toupper(input$icd10)) %>%
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
                filter(toupper(Diag) %in% toupper(input$icd10)) %>%
                select(Birth_Year) %>%
                collect() %>%
                pull()
              )))[
            c(
              sort(
                unique(
                  df2 %>%
                    filter(toupper(Diag) %in% toupper(input$icd10)) %>%
                    select(Birth_Year) %>%
                    collect() %>%
                    pull()
                  )
                )
              ) >= input$t0],
        selected = c(
          max(
            df2 %>%
              filter(toupper(Diag) %in% toupper(input$icd10)) %>%
              select(Birth_Year) %>%
              collect() %>%
              pull(), na.rm = TRUE
            )
          )
        )
    })
 })

  # Return reactive output from the selectInputs
  # reactive expression
  return(
    list(id10 = reactive(input$icd10),
         t0 = reactive(input$t0),
         tn = reactive(input$tn))
    )
}
