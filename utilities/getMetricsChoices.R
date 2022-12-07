# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("data.table")
import("dplyr")

consts <- use("constants/constants.R")

# get values from Q00 - Q07
grepl("^Q(0[0-7])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(1[0-8])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(2[0-8])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(3[0-4])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(3[5-7])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(3[8-9]|4[0-5])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(5[0-6])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(6[0-4])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(6[5-9]|7[0-9])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(8[0-9])",levels(consts$cd_stats$cat_tier2))
grepl("^Q(9[0-9])",levels(consts$cd_stats$cat_tier2))

createOptionsList <- function(choices, suffix = "") {
  keys <- choices %>%
    lapply("[[", "label") %>%
    unname() %>%
    lapply(function(x) paste(x, suffix))
  
  values <- choices %>%
    lapply("[[", "id")
  
  names(values) <- keys
  return(values)
}
