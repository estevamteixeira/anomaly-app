# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
# import("arrow")
import("bslib")
# import("dplyr")
import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
export("aboutUI")
export("aboutServer")


#' Title
#'
#' @param id
#'
#' @return Module for showing the app 'about' page.
#' @export
#'
#' @examples
#'
aboutUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 page_fillable(
  layout_columns(
   col_widths = 12,
   # p("This is an interactive open source tool on the prevalence of
   #     congenital anomalies in Nova Scotia, Canada, (built in ",
   #     a("R", href = "https://cran.r-project.org"),
   #     " with ",
   #     a("shiny", href = "https://shiny.rstudio.com"),
   #     ").",
   #     style = "text-align:center;width:50%;margin:auto;"),
   # p(strong("Surveillance of Congenital Anomalies"),
   #        "enables intuitive, rapid and reproducible population-based data on
   #        congenital anomalies that will provide information to improve
   #        the health of Nova Scotia children and their families.",
   #     style = "text-align:center;width:50%;margin:auto;"),
   hr(style="border:2px solid;width:30%;margin:auto;"),
   h2("Contact Our Team", style="text-align:center;"),
   hr(style="border:2px solid;width:30%;margin:auto;"),
   layout_columns(
    col_width = c(-2,6,-2),
    fill = TRUE,
    value_box(
     title = NULL,
     p("Halifax Professional Centre, Suite 700"),
     p("5991 Spring Garden Rd., Halifax, NS, B3H 1Y6, Canada"),
     showcase = bsicons::bs_icon("geo-alt-fill"),
     full_screen = TRUE
    ),
    value_box(
     title = NULL,
     p("Cora Cole"),
     a("cora.cole@iwk.nshealth.ca",
       href = "mailto:cora.cole@iwk.nshealth.ca"),
     showcase = bsicons::bs_icon("envelope-fill"),
     full_screen = TRUE
     )
    ),
   hr(style="border:2px solid;width:30%;margin:auto;"),
   h2("Acknowledgment", style="text-align:center;"),
   hr(style="border:2px solid;width:30%;margin:auto;"),
   withMathJax(
    HTML(
     readLines("modules/about.html"
               # generates HTML output from the R Markdown document
               # rmarkdown::render(input = "modules/summ.Rmd",
               #                   output_format = rmarkdown::html_fragment(),
               #                   quiet = TRUE)
     )))
   )
  )
}

aboutServer <- function(id){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

 })
}

