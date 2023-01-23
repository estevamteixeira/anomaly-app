# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("data.table")
import("dplyr")
import("lubridate")

getSubsetByTimeRange <- function(df, y1, y2, q = NULL) {
  # colsToSelect <- c("CD_UID", "cd_full", "BrthYear", "cd.count_anom", 
  #                   "total_lvb")
  if (is.null(q) || q == "0") {
    subset(
      x = df,
      subset = data.table::between(BrthYear, y1, y2) #includes boundaries
      # select = colsToSelect
    )
  } else if (!q == "0" &&
             is.na(stringr::str_extract(q, pattern = "\\(.*\\)"))){
    subset(
      x = df,
      subset = data.table::between(BrthYear, y1, y2) & cat_tier3 %in% q
      # select = colsToSelect
    )
    } else {
    subset(
      x = df,
      subset = data.table::between(BrthYear, y1, y2) & cat_tier4 %in% q
      # select = colsToSelect
    )
  }
}

getCountyData <- function(df, y1, y2) {
  unique(getSubsetByTimeRange(df, y1, y2, q = NULL)[,
                                             c("CaseID","CD_UID", "BrthYear",
                                               "cat_tier3","cat_tier4")
  ])[,
      `:=` (total_cases = .N), by = .(CD_UID)
    ]
}

sumAllNonNAValues <- function(v) {
  if (length(v[!is.na(v)]) != 0) {
    sum(v, na.rm = TRUE)
  } else {
    return(NA_real_)
  }
}

getCountyDataByCase <- function(df, y1, y2, q){
  unique(getSubsetByTimeRange(df, y1, y2, q)[,
                                      c("CaseID","CD_UID", "BrthYear",
                                        "cat_tier3","cat_tier4")
  ])[,
    `:=` (total_cases = .N),
    by = .(CD_UID)
  ]
}

getCountyDataByCaseRisk <- function(df, risk, y1, y2, q){
  unique(getSubsetByTimeRange(df, y1, y2, q)[,
                                             .SD,
                                             .SDcols =
                                      c("CaseID","CD_UID", "BrthYear",
                                        "cat_tier3","cat_tier4", risk)
  ])[,
     `:=` (total_cases = .N),
     by = .(CD_UID, get(risk))
  ]
}

getCountyDataByRisk <- function(df, risk, y1, y2){
  unique(getSubsetByTimeRange(df, y1, y2, q = NULL)[,
                                                    .SD,
                                                    .SDcols =
                                             c("CaseID","CD_UID", "BrthYear",
                                               "cat_tier3","cat_tier4", risk)
                                             ])[,
      `:=` (total_cases = .N),
      by = .(CD_UID, get(risk))
  ]
}

### Over time

getCountyDataTime <- function(df, y1, y2) {
  unique(getSubsetByTimeRange(df, y1, y2, q = NULL)[,
                                             c("CaseID","CD_UID", "BrthYear",
                                               "cat_tier3","cat_tier4")
  ])[,
     `:=` (total_cases = .N),
     by = .(CD_UID, BrthYear)
  ]
}

getCountyDataByCaseTime <- function(df, y1, y2, q){
  unique(getSubsetByTimeRange(df, y1, y2, q)[,
                                      c("CaseID","CD_UID", "BrthYear",
                                        "cat_tier3","cat_tier4")
  ])[,
      `:=` (total_cases = .N),
      by = .(CD_UID, BrthYear)
  ]
}

getCountyDataRiskTime <- function(df, risk, y1, y2) {
  unique(getSubsetByTimeRange(df, y1, y2, q = NULL)[,
                                                    .SD,
                                                    .SDcols =
                                             c("CaseID","CD_UID", "BrthYear",
                                               "cat_tier3","cat_tier4", risk)
  ])[,
                                             `:=` (total_cases = .N),
                                             by = .(CD_UID, BrthYear, get(risk))
  ]
}

getCountyDataByCaseRiskTime <- function(df, risk, y1, y2, q){
  unique(getSubsetByTimeRange(df, y1, y2, q)[,
                                             .SD,
                                             .SDcols =
                                      c("CaseID","CD_UID", "BrthYear",
                                        "cat_tier3","cat_tier4", risk)
  ])[,
                                      `:=` (total_cases = .N),
                                      by = .(CD_UID, BrthYear, get(risk))
  ]
}
