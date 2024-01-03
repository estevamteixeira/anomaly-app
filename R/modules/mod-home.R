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
  includeMarkdown("R/home.Rmd")
  )
 )
}

homeServer <- function(id){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

 })
}

