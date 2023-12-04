# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
modules::import("arrow")
modules::import("dplyr")

#' Title
#'
#' @param df
#' @param colsToSelect
#'
#' @return An `arrow::arrow_table()` with unique selected columns
#' @export
#'
#' @examples
getSubsetData <- function(df, colsToSelect){
 return(
  df %>%
  select(any_of(colsToSelect)) %>%
  distinct()
  )
}






