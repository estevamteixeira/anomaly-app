# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
# import("arrow")
import("bslib")
# import("dplyr")
import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
export("homeUI")
export("homeServer")


#' Title
#'
#' @param id
#'
#' @return Module for showing the app home page.
#' @export
#'
#' @examples
#'
homeUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

page_fillable(
 layout_columns(
  col_widths = 12,
  # Add a horizontal line
  hr(),
  h1(style = "text-align: center; color: #000000;",
     "Welcome to the"),
  h3(style = "text-align: center",
     strong("Nova Scotia Surveillance of Congenital Anomalies")),
  h4(style = "text-align: center; color:#000000;",
   "An interactive open source tool on the prevalence of congenital anomalies in Nova Scotia, Canada ",
   "(built in ",
   a("R", href = "https://cran.r-project.org/"), " with ",
   a("Shiny", href = "https://shiny.rstudio.com/"),")."
   ),
  # Add a horizontal line
  hr(),
  p(style = "text-align: left; color: #000000;",
    strong(style = "color: #4D9B9A;",
           "Survaillance of Congenital Anomalies"), "enables intuitive, rapid and reproducible population-based data on congenital anomalies that will provide information to improve the health of Nova Scotia children and their families."
    ),
  # Add a horizontal line
  hr(),
  p(style = "text-align: left; color: #000000;",
    strong("WHAT YOU CAN DO WITH this app:")
    ),
  tags$ul(
   tags$li(style = "text-align: left; color: #000000;",
           "Provide data on occurrence."),
   tags$li(style = "text-align: left; color: #000000;",
           "Identify populations at increased risk."),
   tags$li(style = "text-align: left; color: #000000;",
           "Monitor changes in occurrence."),
   tags$li(style = "text-align: left; color: #000000;",
           "Investigate clusters."),
   tags$li(style = "text-align: left; color: #000000;",
           "Identify geographic variation and localised clustering of congenital anomalies in Nova Scotia."),
   tags$li(style = "text-align: left; color: #000000;",
           "Create research opportunities.")
  ),
  # hr(),
  p(style = "text-align: left; color: #000000;",
    "If you have any question, please contact ",
    a(style = "color: #4D9B9A;",
      strong("Cora Cole"), href = "mailto:cora.cole@iwk.nshealth.ca"),", ",
    a(style = "color: #4D9B9A;",
      strong("John Fahey"), href = "mailto:john.fahey@iwk.nshealth.ca"), ", or ",
    a(style = "color: #4D9B9A;",
      strong("Estevam Teixeira"), href = "mailto:estevam.teixeira@iwk.nshealth.ca"),"."
  ),
  hr(),
  p(style = "position: fixed; bottom: 0; width: 100%; text-align: center; color: #000000;",
    HTML("&copy; 2023 | All Rights Reserved | Built with \u2764 by"),
    a(style = "color: #4D9B9A;",
      strong("Reproductive Care Program"), href = "http://rcp.nshealth.ca/")
    )
  )
 )
}

homeServer <- function(id){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

 })
}

