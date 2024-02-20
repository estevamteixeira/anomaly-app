# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
# import("arrow")
import("bslib")
# import("dplyr")
import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
export("helpUI")
export("helpServer")


#' Title
#'
#' @param id
#'
#' @return Module for showing the app help page.
#' @export
#'
#' @examples
#'
helpUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 layout_columns(
  col_widths = 12,
  withMathJax(
   HTML(
    readLines("modules/summ.html"
     # generates HTML output from the R Markdown document
     # rmarkdown::render(input = "modules/summ.Rmd",
     #                   output_format = rmarkdown::html_fragment(),
     #                   quiet = TRUE)
    )))
 )
}

helpServer <- function(id){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

 })
}

