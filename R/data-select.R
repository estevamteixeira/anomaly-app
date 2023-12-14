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

#' Title
#'
#' @param df
#' @param colsToSelect
#'
#' @return A data frame
#' @export
#'
#' @examples
calcPrev <- function(df, colsToSelect){
 return(
  df %>%
   group_by(Birth_Year, !!!syms(colsToSelect[grepl("uid", colsToSelect, fixed = TRUE)])) %>%
   mutate(count_ano = n()) %>%
   ungroup() %>%
   select(any_of(c("Birth_Year", "count_ano", colsToSelect))) %>%
   distinct() %>%
   group_by(!!!syms(colsToSelect[grepl("uid", colsToSelect, fixed = TRUE)])) %>%
   mutate(total_ano = sum(count_ano, na.rm = TRUE),
          total_brth = sum(!!!syms(colsToSelect[grepl("count", colsToSelect, fixed = TRUE)]), na.rm = TRUE),
          prev = 10000*total_ano/total_brth) %>%
   ungroup() %>%
   select(-starts_with(c("count","total")), -Birth_Year) %>%
   distinct()
 )
}





