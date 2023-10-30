# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("data.table")
import("dplyr")
import("lubridate")

getSubsetByTimeRange <- function(df, y1, y2, q = NULL) {
  # colsToSelect <- c("CDuid", "cd_full", "BrthYear", "cd.count_anom", 
  #                   "total_lvb")
  if (is.null(q) || q == "0") 
    return(subset(
      x = df,
      subset = data.table::between(BrthYear, y1, y2) #includes boundaries
      # select = colsToSelect
    ))
  
  if (!q == "0" &&
      is.na(stringr::str_extract(q, pattern = "\\(.*\\)")))
    return(subset(
      x = df,
      subset = data.table::between(BrthYear, y1, y2) & cat_tier3 %in% q
      # select = colsToSelect
    ))
  
  return(subset(
    x = df,
    subset = data.table::between(BrthYear, y1, y2) & cat_tier4 %in% q
    # select = colsToSelect
  ))
  
}

getGeoData <- function(df,
                       y1, y2,
                       ColsToSelect,
                       bygroup) {
  # eval(substitute({
  unique(getSubsetByTimeRange(df, y1, y2, q = NULL)[,
                                                    .SD,
                                                    .SDcols = ColsToSelect
  ])[,
     `:=` (total_cases = .N), by = bygroup
  ]
  # }))
}

sumAllNonNAValues <- function(v) {
  if (length(v[!is.na(v)]) != 0) {
    sum(v, na.rm = TRUE)
  } else {
    return(NA_real_)
  }
}

getGeoDataByCase <- function(df, y1, y2,
                             q,
                             ColsToSelect,
                             bygroup){
  # eval(substitute({
  unique(getSubsetByTimeRange(df, y1, y2, q)[,
                                             .SD,
                                             .SDcols = ColsToSelect
  ])[,
     `:=` (total_cases = .N),
     by = bygroup
  ]
  # }))
}

# Case -------

buildGeoDataByCase <- function(df1, df2,
                               y1, y2,
                               q,
                               geo){
  import("data.table")
  import("scales")
  
  
  if(is.null(q) || q %in% "0"){
    if (tolower(tolower(geo) %in% "csd"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "CSDuid", "CSDName", "CSDType", "BrthYear"),
                c("CSDuid"))[, c("CSDuid", "CSDName", "CSDType", "total_cases")
                ]),
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "CSDName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, dlv)
                    ][,
                      c("CSDuid", "CSDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("CSDuid","BrthYear")
                    ][,
                      c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr")]
              )[,
                `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                          na.rm = TRUE)),
                by = c("CSDuid")
              ][,
                c("CSDuid", "CSDName", "total_lvb_geo")
              ]),
            by = c("CSDuid", "CSDName"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(CSDName), c("CSDuid", "CSDName", "total_cases","total_lvb_geo")
            ])[,
               `:=` (rate = 1000*total_cases/total_lvb_geo),
               by = c("CSDuid")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ]
      )
    
    if (tolower(tolower(geo) %in% "cd"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "CDuid", "CDName", "BrthYear"),
                c("CDuid"))[, c("CDuid", "CDName", "total_cases")
                ]),
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CDuid", "CDName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CDuid, dlv)
                    ][,
                      c("CDuid", "CDName", "BrthYear", "dlv","count_dlv_geo_yr")
                    ][,
                      c("CDuid", "CDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("CDuid","BrthYear")
                    ][,
                      c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")]
              )[,
                `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                          na.rm = TRUE)),
                by = c("CDuid")
              ][,
                c("CDuid", "CDName", "total_lvb_geo")
              ]),
            by = c("CDuid", "CDName"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(CDName), c("CDuid", "CDName", "total_cases","total_lvb_geo")
            ])[,
               `:=` (rate = 1000*total_cases/total_lvb_geo),
               by = c("CDuid")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ]
      )
    
    if (tolower(tolower(geo) %in% "clus"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "Cluster_Number", "ClusterName", "BrthYear"),
                c("Cluster_Number"))[, c("Cluster_Number", "ClusterName", "total_cases")
                ]),
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, Cluster_Number, dlv)
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "dlv","count_dlv_geo_yr")
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("Cluster_Number","BrthYear")
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")]
              )[,
                `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                          na.rm = TRUE)),
                by = c("Cluster_Number")
              ][,
                c("Cluster_Number", "ClusterName", "total_lvb_geo")
              ]),
            by = c("Cluster_Number", "ClusterName"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(ClusterName), c("Cluster_Number", "ClusterName", "total_cases","total_lvb_geo")
            ])[,
               `:=` (rate = 1000*total_cases/total_lvb_geo),
               by = c("Cluster_Number")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ]
      )
    
    if (tolower(tolower(geo) %in% "hn"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "NetworkID", "NetworkName", "BrthYear"),
                c("NetworkID"))[, c("NetworkID", "NetworkName", "total_cases")
                ]),
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, NetworkID, dlv)
                    ][,
                      c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("NetworkID","BrthYear")
                    ][,
                      c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")]
              )[,
                `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                          na.rm = TRUE)),
                by = c("NetworkID")
              ][,
                c("NetworkID", "NetworkName", "total_lvb_geo")
              ]),
            by = c("NetworkID", "NetworkName"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(NetworkName), c("NetworkID", "NetworkName", "total_cases","total_lvb_geo")
            ])[,
               `:=` (rate = 1000*total_cases/total_lvb_geo),
               by = c("NetworkID")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ]
      )
    
    if (tolower(tolower(geo) %in% "zn"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear"),
                c("ZoneID"))[, c("CSDuid", "ZoneID", "ZnName", "total_cases")
                ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, ZoneID, dlv)
                  ][,
                    c("CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                               na.rm = TRUE)),
                     by = c("ZoneID")
                  ][,
                    c("CSDuid", "ZoneID", "ZnName", "total_lvb_geo")
                  ]),
            by = c("ZoneID", "ZnName","CSDuid"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(ZnName),
              c("CSDuid", "ZoneID", "ZnName", "total_cases","total_lvb_geo")
            ])[,
               `:=` (rate = 1000*total_cases/total_lvb_geo),
               by = c("ZoneID")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ]
      )
    
    if (tolower(tolower(geo) %in% "urb"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "CSDuid", "area", "BrthYear"),
                c("area"))[, c("CSDuid", "area", "total_cases")
                ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "area", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, area, dlv)
                  ][,
                    c("CSDuid", "area", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                               na.rm = TRUE)),
                     by = c("area")
                  ][,
                    c("CSDuid", "area", "total_lvb_geo")
                  ]),
            by = c("CSDuid", "area"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(area), c("CSDuid", "area", "total_cases","total_lvb_geo")
            ])[,
               `:=` (rate = 1000*total_cases/total_lvb_geo),
               by = c("area")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ]
      )
    
  } else if (!q == "0" &&
             all(is.na(stringr::str_extract(q, pattern = "\\(.*\\)")))){
    if (tolower(tolower(geo) %in% "csd"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "CSDName", "CSDType", "BrthYear", "cat_tier3"),
                  c("CSDuid", "BrthYear"))[,
                                           c("CSDuid", "CSDName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(CSDuid, cat_tier3)
                  ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "CSDName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, dlv)
                  ][,
                    c("CSDuid", "CSDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("CSDuid","BrthYear")
                  ][,
                    c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                               na.rm = TRUE)),
                     by = c("CSDuid")
                  ],
            by = c("CSDuid", "CSDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(CSDName), c("CSDuid", "CSDName", "total_cases","total_lvb_geo")
            ])[,
               `:=` (rate = 1000*total_cases/total_lvb_geo),
               by = c("CSDuid")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ]
      )
    
    if (tolower(tolower(geo) %in% "cd"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CDuid", "CDName", "BrthYear","cat_tier3"),
                  c("CDuid", "BrthYear"))[,
                                          c("CDuid", "CDName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(CDuid, cat_tier3)
                  ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CDuid", "CDName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CDuid, dlv)
                  ][,
                    c("CDuid", "CDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("CDuid","BrthYear")
                  ][,
                    c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                               na.rm = TRUE)),
                     by = c("CDuid")
                  ],
            by = c("CDuid", "CDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(CDName), c("CDuid", "CDName", "total_cases","total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("CDuid")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
    
    if (tolower(tolower(geo) %in% "clus"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "Cluster_Number", "ClusterName", "BrthYear","cat_tier3"),
                  c("Cluster_Number", "BrthYear"))[,
                                                   c("Cluster_Number", "ClusterName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(Cluster_Number, cat_tier3)
                  ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, Cluster_Number, dlv)
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("Cluster_Number","BrthYear")
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                               na.rm = TRUE)),
                     by = c("Cluster_Number")
                  ],
            by = c("Cluster_Number", "ClusterName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(ClusterName), c("Cluster_Number", "ClusterName", "total_cases","total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("Cluster_Number")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
    
    if (tolower(tolower(geo) %in% "hn"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "NetworkID", "NetworkName", "BrthYear","cat_tier3"),
                  c("NetworkID", "BrthYear"))[,
                                              c("NetworkID", "NetworkName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(NetworkID, cat_tier3)
                  ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, NetworkID, dlv)
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("NetworkID","BrthYear")
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                               na.rm = TRUE)),
                     by = c("NetworkID")
                  ],
            by = c("NetworkID", "NetworkName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(NetworkName), c("NetworkID", "NetworkName", "total_cases", "total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("NetworkID")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
    
    if (tolower(tolower(geo) %in% "zn"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier3"),
                  c("ZoneID", "BrthYear"))[,
                                           c("CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(ZoneID, cat_tier3)
                  ]),
            unique(
              getSubsetByTimeRange(
                df2,
                y1,
                y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                    c("BIRTHID", "CSDuid","ZoneID", "ZnName", "BrthYear", "dlv")
                ][, `:=` (count_dlv_geo_yr = .N),
                  by = list(BrthYear, CSDuid, ZoneID, dlv)
                ][,
                  c("CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv", "count_dlv_geo_yr")
                ])[,
                   `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                             na.rm = TRUE)),
                   by = c("ZoneID")
                ],
            by = c("CSDuid", "ZoneID", "ZnName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(ZnName),
              c("CSDuid", "ZoneID", "ZnName", "total_cases", "total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("ZoneID")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
    
    if (tolower(tolower(geo) %in% "urb"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "area", "BrthYear","cat_tier3"),
                  c("area", "BrthYear"))[,
                                         c("CSDuid", "area", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(area, cat_tier3)
                  ]),
            unique(
              getSubsetByTimeRange(
                df2,
                y1,
                y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                    c("BIRTHID", "CSDuid", "area", "BrthYear", "dlv")
                ][, `:=` (count_dlv_geo_yr = .N),
                  by = list(BrthYear, CSDuid, area, dlv)
                ][,
                  c("CSDuid", "area", "BrthYear", "dlv", "count_dlv_geo_yr")
                ])[,
                   `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                             na.rm = TRUE)),
                   by = c("area")
                ],
            by = c("CSDuid", "area", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(area), c("CSDuid", "area", "total_cases", "total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("area")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
    
  } else{
    if (tolower(tolower(geo) %in% "csd"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "CSDName", "BrthYear","cat_tier4"),
                  c("CSDuid", "BrthYear"))[,
                                           c("CSDuid", "CSDName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(CSDuid, cat_tier4)
                  ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "CSDName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, dlv)
                  ][,
                    c("CSDuid", "CSDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("CSDuid","BrthYear")
                  ][,
                    c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                               na.rm = TRUE)),
                     by = c("CSDuid")
                  ],
            by = c("CSDuid", "CSDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(CSDName), c("CSDuid", "CSDName", "total_cases","total_lvb_geo")
            ])[,
               `:=` (rate = 1000*total_cases/total_lvb_geo),
               by = c("CSDuid")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ]
      )
    
    if (tolower(tolower(geo) %in% "cd"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CDuid", "CDName", "BrthYear","cat_tier4"),
                  c("CDuid", "BrthYear"))[,
                                          c("CDuid", "CDName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(CDuid, cat_tier4)
                  ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CDuid", "CDName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CDuid, dlv)
                  ][,
                    c("CDuid", "CDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("CDuid","BrthYear")
                  ][,
                    c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                               na.rm = TRUE)),
                     by = c("CDuid")
                  ],
            by = c("CDuid", "CDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(CDName), c("CDuid", "CDName", "total_cases","total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("CDuid")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
    
    if (tolower(tolower(geo) %in% "clus"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "Cluster_Number", "ClusterName", "BrthYear","cat_tier4"),
                  c("Cluster_Number", "BrthYear"))[,
                                                   c("Cluster_Number", "ClusterName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(Cluster_Number, cat_tier4)
                  ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, Cluster_Number, dlv)
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("Cluster_Number","BrthYear")
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                               na.rm = TRUE)),
                     by = c("Cluster_Number")
                  ],
            by = c("Cluster_Number", "ClusterName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(ClusterName), c("Cluster_Number", "ClusterName", "total_cases","total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("Cluster_Number")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
    
    if (tolower(tolower(geo) %in% "hn"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "NetworkID", "NetworkName", "BrthYear","cat_tier4"),
                  c("NetworkID", "BrthYear"))[,
                                              c("NetworkID", "NetworkName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(NetworkID, cat_tier4)
                  ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, NetworkID, dlv)
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("NetworkID","BrthYear")
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                               na.rm = TRUE)),
                     by = c("NetworkID")
                  ],
            by = c("NetworkID", "NetworkName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(NetworkName), c("NetworkID", "NetworkName", "total_cases", "total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("NetworkID")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
    
    if (tolower(tolower(geo) %in% "zn"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier4"),
                  c("ZoneID", "BrthYear"))[,
                                           c("CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(ZoneID, cat_tier4)
                  ]),
            unique(
              getSubsetByTimeRange(
                df2,
                y1,
                y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                    c("BIRTHID", "CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv")
                ][, `:=` (count_dlv_geo_yr = .N),
                  by = list(BrthYear, ZoneID, dlv)
                ][,
                  c("CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv", "count_dlv_geo_yr")
                ])[,
                   `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                             na.rm = TRUE)),
                   by = c("ZoneID")
                ],
            by = c("CSDuid", "ZoneID", "ZnName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(ZnName),
              c("CSDuid", "ZoneID", "ZnName", "total_cases", "total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("ZoneID")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
    
    if (tolower(tolower(geo) %in% "urb"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "area", "BrthYear","cat_tier4"),
                  c("area", "BrthYear"))[,
                                         c("CSDuid", "area", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(area, cat_tier4)
                  ]),
            unique(
              getSubsetByTimeRange(
                df2,
                y1,
                y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                    c("BIRTHID", "CSDuid", "area", "BrthYear", "dlv")
                ][, `:=` (count_dlv_geo_yr = .N),
                  by = list(BrthYear, area, dlv)
                ][,
                  c("CSDuid", "area", "BrthYear", "dlv", "count_dlv_geo_yr")
                ])[,
                   `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                             na.rm = TRUE)),
                   by = c("area")
                ],
            by = c("CSDuid", "area", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(area),
              c("CSDuid", "area", "total_cases", "total_lvb_geo")
            ][,
              `:=` (rate = 1000*total_cases/total_lvb_geo),
              by = c("area")
            ][
              order(-rate)
            ][
              !is.na(rate)
            ])
      )
  }
}

## Case x Time -------

buildGeoDataByCaseTime <- function(df1, df2,
                                   y1, y2,
                                   q,
                                   geo){
  import("data.table")
  import("scales")
  
  
  if(is.null(q) || q %in% "0"){
    if (tolower(tolower(geo) %in% "csd"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "CSDuid", "CSDName", "CSDType", "BrthYear"),
                c("CSDuid", "BrthYear"))[,
                                         c("CSDuid", "CSDName", "CSDType", "BrthYear", "total_cases")
                ])[,
                   `:=` (total_cases_yr = sum(total_cases,
                                              na.rm = TRUE)),
                   by = c("BrthYear")
                ],
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "CSDName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, dlv)
                    ][,
                      c("CSDuid", "CSDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("CSDuid","BrthYear")
                    ][,
                      c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr")]
              )[,
                `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                         na.rm = TRUE)),
                by = c("BrthYear")
              ][,
                c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr", "total_lvb_yr")
              ]),
            by = c("CSDuid", "CSDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(CSDName),
              c("CSDuid", "CSDName", "BrthYear",
                "total_cases", "total_cases_yr","total_lvb_geo_yr", "total_lvb_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "cd"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "CDuid", "CDName", "BrthYear"),
                c("CDuid", "BrthYear"))[,
                                        c("CDuid", "CDName", "BrthYear", "total_cases")
                ])[,
                   `:=` (total_cases_yr = sum(total_cases,
                                              na.rm = TRUE)),
                   by = c("BrthYear")
                ],
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CDuid", "CDName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CDuid, dlv)
                    ][,
                      c("CDuid", "CDName", "BrthYear", "dlv","count_dlv_geo_yr")
                    ][,
                      c("CDuid", "CDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("CDuid","BrthYear")
                    ][,
                      c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")]
              )[,
                `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                         na.rm = TRUE)),
                by = c("BrthYear")
              ][,
                c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr", "total_lvb_yr")
              ]),
            by = c("CDuid", "CDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(CDName), c("CDuid", "CDName", "BrthYear",
                                "total_cases","total_cases_yr",
                                "total_lvb_geo_yr","total_lvb_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "clus"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "Cluster_Number", "ClusterName", "BrthYear"),
                c("Cluster_Number","BrthYear"))[,
                                                c("Cluster_Number", "ClusterName", "BrthYear", "total_cases")
                ])[,
                   `:=` (total_cases_yr = sum(total_cases,
                                              na.rm = TRUE)),
                   by = c("BrthYear")
                ],
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, Cluster_Number, dlv)
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "dlv","count_dlv_geo_yr")
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("Cluster_Number","BrthYear")
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")]
              )[,
                `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                         na.rm = TRUE)),
                by = c("BrthYear")
              ][,
                c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_yr", "total_lvb_geo_yr")
              ]),
            by = c("Cluster_Number", "ClusterName", "BrthYear"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(ClusterName), 
              c("Cluster_Number", "ClusterName", "BrthYear",
                "total_cases", "total_cases_yr",
                "total_lvb_yr", "total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "hn"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "NetworkID", "NetworkName", "BrthYear"),
                c("NetworkID", "BrthYear"))[,
                                            c("NetworkID", "NetworkName", "BrthYear", "total_cases")
                ])[,
                   `:=` (total_cases_yr = sum(total_cases,
                                              na.rm = TRUE)),
                   by = c("BrthYear")
                ],
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, NetworkID, dlv)
                    ][,
                      c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("NetworkID","BrthYear")
                    ][,
                      c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")]
              )[,
                `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                         na.rm = TRUE)),
                by = c("BrthYear")
              ][,
                c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr", "total_lvb_yr")
              ]),
            by = c("NetworkID", "NetworkName", "BrthYear"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(NetworkName),
              c("NetworkID", "NetworkName", "BrthYear",
                "total_cases","total_cases_yr", "total_lvb_yr", "total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "zn"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear"),
                c("CSDuid", "ZoneID", "BrthYear"))[,
                                                   c("CSDuid", "ZoneID", "ZnName",
                                                     "BrthYear", "total_cases")
                ])[,
                   `:=` (total_cases_yr = sum(total_cases,
                                              na.rm = TRUE)),
                   by = c("BrthYear")
                ],
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, ZoneID, dlv)
                    ][,
                      c("CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("ZoneID","BrthYear")
                    ][,
                      c("CSDuid", "ZoneID", "ZnName", "BrthYear", "total_lvb_geo_yr", "count_dlv_geo_yr")]
              )[,
                `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                         na.rm = TRUE)),
                by = c("BrthYear")
              ][,
                c("CSDuid", "ZoneID", "ZnName", "BrthYear", "total_lvb_geo_yr", "total_lvb_yr")
              ]),
            by = c("CSDuid", "ZoneID", "ZnName", "BrthYear"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(ZnName),
              c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                "total_cases","total_cases_yr", "total_lvb_yr", "total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "urb"))
      return(
        unique(
          merge(
            unique(
              getGeoDataByCase(
                df1,
                y1,
                y2,
                q = NULL,
                c("CaseID", "CSDuid", "area", "BrthYear"),
                c("CSDuid", "area", "BrthYear"))[,
                                                 c("CSDuid", "area",
                                                   "BrthYear", "total_cases")
                ])[,
                   `:=` (total_cases_yr = sum(total_cases,
                                              na.rm = TRUE)),
                   by = c("BrthYear")
                ],
            unique(
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "area", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, area, dlv)
                    ][,
                      c("CSDuid", "area", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("area","BrthYear")
                    ][,
                      c("CSDuid", "area", "BrthYear", "total_lvb_geo_yr", "count_dlv_geo_yr")]
              )[,
                `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                         na.rm = TRUE)),
                by = c("BrthYear")
              ][,
                c("CSDuid", "area", "BrthYear", "total_lvb_geo_yr", "total_lvb_yr")
              ]),
            by = c("CSDuid", "area", "BrthYear"),
            allow.cartesian = TRUE,
            all.y = TRUE)[
              !is.na(area),
              c("CSDuid", "area", "BrthYear",
                "total_cases","total_cases_yr", "total_lvb_yr", "total_lvb_geo_yr")
            ])
      )
    
  } else if (!q == "0" &&
             all(is.na(stringr::str_extract(q, pattern = "\\(.*\\)")))){
    if (tolower(tolower(geo) %in% "csd"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "CSDName", "CSDType", "BrthYear", "cat_tier3"),
                  c("CSDuid", "BrthYear"))[,
                                           c("CSDuid", "CSDName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(CSDuid, cat_tier3, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier3")
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "CSDName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, dlv)
                  ][,
                    c("CSDuid", "CSDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("CSDuid","BrthYear")
                  ][,
                    c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("CSDuid", "CSDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(CSDName),
              c("CSDuid", "CSDName", "BrthYear", "cat_tier3",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "cd"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CDuid", "CDName", "BrthYear","cat_tier3"),
                  c("CDuid", "BrthYear"))[,
                                          c("CDuid", "CDName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(CDuid, cat_tier3, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier3")
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CDuid", "CDName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CDuid, dlv)
                  ][,
                    c("CDuid", "CDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("CDuid","BrthYear")
                  ][,
                    c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("CDuid", "CDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(CDName),
              c("CDuid", "CDName", "BrthYear", "cat_tier3",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "clus"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "Cluster_Number", "ClusterName", "BrthYear","cat_tier3"),
                  c("Cluster_Number", "BrthYear"))[,
                                                   c("Cluster_Number", "ClusterName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(Cluster_Number, cat_tier3, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier3")
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, Cluster_Number, dlv)
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("Cluster_Number","BrthYear")
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("Cluster_Number", "ClusterName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(ClusterName),
              c("Cluster_Number", "ClusterName", "BrthYear", "cat_tier3",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "hn"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "NetworkID", "NetworkName", "BrthYear","cat_tier3"),
                  c("NetworkID", "BrthYear"))[,
                                              c("NetworkID", "NetworkName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(NetworkID, cat_tier3, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier3")
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, NetworkID, dlv)
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("NetworkID","BrthYear")
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("NetworkID", "NetworkName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(NetworkName),
              c("NetworkID", "NetworkName", "BrthYear", "cat_tier3",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "zn"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier3"),
                  c("CSDuid","ZoneID", "BrthYear"))[,
                                                    c("CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases_geo_yr = sum(total_cases, na.rm = TRUE)),
                     by = list(ZoneID, cat_tier3, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier3")
                  ][,
                    `:=` (total_cases = total_cases_geo_yr)
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, ZoneID, dlv)
                  ][,
                    c("CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("ZoneID","BrthYear")
                  ][,
                    c("CSDuid", "ZoneID", "ZnName", 
                      "BrthYear", "total_lvb_geo_yr", 'count_dlv_geo_yr')
                  ])[,
                     `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("CSDuid", "ZoneID", "ZnName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(ZnName),
              c("CSDuid", "ZoneID", "ZnName", "BrthYear", "cat_tier3",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "urb"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "area", "BrthYear","cat_tier3"),
                  c("CSDuid","area", "BrthYear"))[,
                                                  c("CSDuid", "area", "BrthYear","cat_tier3", "total_cases")
                  ])[,
                     `:=` (total_cases_geo_yr = sum(total_cases, na.rm = TRUE)),
                     by = list(area, cat_tier3, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier3")
                  ][,
                    `:=` (total_cases = total_cases_geo_yr)
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "area", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, area, dlv)
                  ][,
                    c("CSDuid", "area", "BrthYear", "dlv",
                      "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("area","BrthYear")
                  ][,
                    c("CSDuid", "area", 
                      "BrthYear", "total_lvb_geo_yr", 'count_dlv_geo_yr')
                  ])[,
                     `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("CSDuid", "area", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(area),
              c("CSDuid", "area", "BrthYear", "cat_tier3",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
  } else{
    if (tolower(tolower(geo) %in% "csd"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "CSDName", "CSDType", "BrthYear", "cat_tier4"),
                  c("CSDuid", "BrthYear"))[,
                                           c("CSDuid", "CSDName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(CSDuid, cat_tier4, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier4")
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "CSDName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, dlv)
                  ][,
                    c("CSDuid", "CSDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("CSDuid","BrthYear")
                  ][,
                    c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("CSDuid", "CSDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(CSDName),
              c("CSDuid", "CSDName", "BrthYear", "cat_tier4",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "cd"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CDuid", "CDName", "BrthYear","cat_tier4"),
                  c("CDuid", "BrthYear"))[,
                                          c("CDuid", "CDName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(CDuid, cat_tier4, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier4")
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CDuid", "CDName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CDuid, dlv)
                  ][,
                    c("CDuid", "CDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("CDuid","BrthYear")
                  ][,
                    c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("CDuid", "CDName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(CDName),
              c("CDuid", "CDName", "BrthYear", "cat_tier4",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "clus"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "Cluster_Number", "ClusterName", "BrthYear","cat_tier4"),
                  c("Cluster_Number", "BrthYear"))[,
                                                   c("Cluster_Number", "ClusterName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(Cluster_Number, cat_tier4, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier4")
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, Cluster_Number, dlv)
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("Cluster_Number","BrthYear")
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("Cluster_Number", "ClusterName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(ClusterName),
              c("Cluster_Number", "ClusterName", "BrthYear", "cat_tier4",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "hn"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "NetworkID", "NetworkName", "BrthYear","cat_tier4"),
                  c("NetworkID", "BrthYear"))[,
                                              c("NetworkID", "NetworkName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                     by = list(NetworkID, cat_tier4, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier4")
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, NetworkID, dlv)
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("NetworkID","BrthYear")
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")
                  ])[,
                     `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("NetworkID", "NetworkName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(NetworkName),
              c("NetworkID", "NetworkName", "BrthYear", "cat_tier4",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "zn"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier4"),
                  c("CSDuid","ZoneID", "BrthYear"))[,
                                                    c("CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases_geo_yr = sum(total_cases, na.rm = TRUE)),
                     by = list(ZoneID, cat_tier4, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier4")
                  ][,
                    `:=` (total_cases = total_cases_geo_yr)
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, ZoneID, dlv)
                  ][,
                    c("CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("ZoneID","BrthYear")
                  ][,
                    c("CSDuid", "ZoneID", "ZnName", 
                      "BrthYear", "total_lvb_geo_yr", 'count_dlv_geo_yr')
                  ])[,
                     `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("CSDuid", "ZoneID", "ZnName", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(ZnName),
              c("CSDuid", "ZoneID", "ZnName", "BrthYear", "cat_tier4",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
    
    if (tolower(tolower(geo) %in% "urb"))
      return(
        unique(
          merge(
            unique(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q,
                  c("CaseID", "CSDuid", "area", "BrthYear","cat_tier4"),
                  c("CSDuid","area", "BrthYear"))[,
                                                  c("CSDuid", "area", "BrthYear","cat_tier4", "total_cases")
                  ])[,
                     `:=` (total_cases_geo_yr = sum(total_cases, na.rm = TRUE)),
                     by = list(area, cat_tier4, BrthYear)
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE)),
                     by = c("BrthYear","cat_tier4")
                  ][,
                    `:=` (total_cases = total_cases_geo_yr)
                  ],
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1,
                  y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("BIRTHID", "CSDuid", "area", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, CSDuid, area, dlv)
                  ][,
                    c("CSDuid", "area", "BrthYear", "dlv",
                      "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("area","BrthYear")
                  ][,
                    c("CSDuid", "area", 
                      "BrthYear", "total_lvb_geo_yr", 'count_dlv_geo_yr')
                  ])[,
                     `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                              na.rm = TRUE)),
                     by = c("BrthYear")
                  ],
            by = c("CSDuid", "area", "BrthYear"),
            allow.cartesian = TRUE,
            all.x = TRUE)[
              !is.na(area),
              c("CSDuid", "area", "BrthYear", "cat_tier4",
                "total_cases", "total_cases_yr",
                "total_lvb_yr","total_lvb_geo_yr")
            ])
      )
  }
}

### Case x Time X Risk ------

buildGeoDataByCaseTimeRisk <- function(df1, df2,
                                       y1, y2,
                                       q, risk,
                                       geo){
  import("data.table")
  import("scales")
  # NULL risk -----
  if(is.null(risk) || risk %in% "0"){
    if(is.null(q) || q %in% "0"){
      if (tolower(tolower(geo) %in% "csd"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "CSDuid", "CSDName", "CSDType", "BrthYear"),
                  c("CSDuid", "BrthYear"))[,
                                           c("CSDuid", "CSDName", "CSDType", "BrthYear", "total_cases")
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE),
                           total_cases_geo_yr = total_cases),
                     by = c("BrthYear")
                  ][,
                    `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                na.rm = TRUE)),
                    by = c("CSDuid")
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          c("BIRTHID", "CSDuid", "CSDName", "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr = .N),
                        by = list(BrthYear, CSDuid, dlv)
                      ][,
                        c("CSDuid", "CSDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                      ])[,
                         `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                      na.rm = TRUE)),
                         by = c("CSDuid","BrthYear")
                      ][,
                        c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr")]
                )[,
                  `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                           na.rm = TRUE)),
                  by = c("BrthYear")
                ][,
                  `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                            na.rm = TRUE)),
                  by = c("CSDuid")
                ][,
                  c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr",
                    "total_lvb_yr","total_lvb_geo")
                ]),
              by = c("CSDuid", "CSDName", "BrthYear"),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(CSDName),
                c("CSDuid", "CSDName", "BrthYear",
                  "total_cases_geo_yr", "total_cases_yr", "total_cases_geo",
                  "total_lvb_geo_yr", "total_lvb_yr", "total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("CSDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "cd"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "CDuid", "CDName", "BrthYear"),
                  c("CDuid", "BrthYear"))[,
                                          c("CDuid", "CDName", "BrthYear", "total_cases")
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE),
                           total_cases_geo_yr = total_cases),
                     by = c("BrthYear")
                  ][,
                    `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                na.rm = TRUE)),
                    by = c("CDuid")
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          c("BIRTHID", "CDuid", "CDName", "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr = .N),
                        by = list(BrthYear, CDuid, dlv)
                      ][,
                        c("CDuid", "CDName", "BrthYear", "dlv","count_dlv_geo_yr")
                      ][,
                        c("CDuid", "CDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                      ])[,
                         `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                      na.rm = TRUE)),
                         by = c("CDuid","BrthYear")
                      ][,
                        c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")]
                )[,
                  `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                           na.rm = TRUE)),
                  by = c("BrthYear")
                ][,
                  `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                            na.rm = TRUE)),
                  by = c("CDuid")
                ][,
                  c("CDuid", "CDName", "BrthYear",
                    "total_lvb_geo_yr", "total_lvb_yr", "total_lvb_geo")
                ]),
              by = c("CDuid", "CDName", "BrthYear"),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(CDName), c("CDuid", "CDName", "BrthYear",
                                  "total_cases_geo_yr","total_cases_yr", "total_cases_geo",
                                  "total_lvb_geo_yr","total_lvb_yr", "total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("CDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "clus"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "Cluster_Number", "ClusterName", "BrthYear"),
                  c("Cluster_Number","BrthYear"))[,
                                                  c("Cluster_Number", "ClusterName", "BrthYear", "total_cases")
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE),
                           total_cases_geo_yr = total_cases),
                     by = c("BrthYear")
                  ][,
                    `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                na.rm = TRUE)),
                    by = c("Cluster_Number")
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr = .N),
                        by = list(BrthYear, Cluster_Number, dlv)
                      ][,
                        c("Cluster_Number", "ClusterName", "BrthYear", "dlv","count_dlv_geo_yr")
                      ][,
                        c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                      ])[,
                         `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                      na.rm = TRUE)),
                         by = c("Cluster_Number","BrthYear")
                      ][,
                        c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")]
                )[,
                  `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                           na.rm = TRUE)),
                  by = c("BrthYear")
                ][,
                  `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                            na.rm = TRUE)),
                  by = c("Cluster_Number")
                ][,
                  c("Cluster_Number", "ClusterName", "BrthYear",
                    "total_lvb_yr", "total_lvb_geo_yr", "total_lvb_geo")
                ]),
              by = c("Cluster_Number", "ClusterName", "BrthYear"),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(ClusterName), 
                c("Cluster_Number", "ClusterName", "BrthYear",
                  "total_cases_geo_yr", "total_cases_yr", "total_cases_geo",
                  "total_lvb_geo_yr", "total_lvb_yr", "total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("Cluster_Number")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "hn"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "NetworkID", "NetworkName", "BrthYear"),
                  c("NetworkID", "BrthYear"))[,
                                              c("NetworkID", "NetworkName", "BrthYear", "total_cases")
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE),
                           total_cases_geo_yr = total_cases),
                     by = c("BrthYear")
                  ][,
                    `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                na.rm = TRUE)),
                    by = c("NetworkID")
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr = .N),
                        by = list(BrthYear, NetworkID, dlv)
                      ][,
                        c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                      ])[,
                         `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                      na.rm = TRUE)),
                         by = c("NetworkID","BrthYear")
                      ][,
                        c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")]
                )[,
                  `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                           na.rm = TRUE)),
                  by = c("BrthYear")
                ][,
                  `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                            na.rm = TRUE)),
                  by = c("NetworkID")
                ][,
                  c("NetworkID", "NetworkName", "BrthYear",
                    "total_lvb_geo_yr", "total_lvb_yr", "total_lvb_geo")
                ]),
              by = c("NetworkID", "NetworkName", "BrthYear"),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(NetworkName),
                c("NetworkID", "NetworkName", "BrthYear",
                  "total_cases_geo_yr","total_cases_yr", "total_cases_geo",
                  "total_lvb_geo_yr", "total_lvb_yr", "total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("NetworkID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "zn"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear"),
                  c("CSDuid", "ZoneID", "BrthYear"))[,
                                                     c("CSDuid", "ZoneID", "ZnName", "BrthYear", "total_cases")
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE),
                           total_cases_geo_yr = total_cases),
                     by = c("BrthYear")
                  ][,
                    `:=` (total_cases_geo = sum(total_cases,
                                                na.rm = TRUE)),
                    by = c("ZoneID")
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          c("BIRTHID", "CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr = .N),
                        by = list(BrthYear, CSDuid, ZoneID, dlv)
                      ][,
                        c("CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv", "count_dlv_geo_yr")
                      ])[,
                         `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                      na.rm = TRUE)),
                         by = c("ZoneID","BrthYear")
                      ][,
                        c("CSDuid", "ZoneID", "ZnName", "BrthYear", "total_lvb_geo_yr", "count_dlv_geo_yr")]
                )[,
                  `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                           na.rm = TRUE)),
                  by = c("BrthYear")
                ][,
                  `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                            na.rm = TRUE)),
                  by = c("ZoneID")
                ][,
                  c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                    "total_lvb_geo_yr", "total_lvb_yr", "total_lvb_geo")
                ]),
              by = c("CSDuid", "ZoneID", "ZnName", "BrthYear"),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(ZnName),
                c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                  "total_cases_geo_yr","total_cases_yr", "total_cases_geo",
                  "total_lvb_geo_yr", "total_lvb_yr", "total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("ZoneID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "urb"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "CSDuid", "area", "BrthYear"),
                  c("CSDuid", "area", "BrthYear"))[,
                                                   c("CSDuid", "area", "BrthYear", "total_cases")
                  ])[,
                     `:=` (total_cases_yr = sum(total_cases,
                                                na.rm = TRUE),
                           total_cases_geo_yr = total_cases),
                     by = c("BrthYear")
                  ][,
                    `:=` (total_cases_geo = sum(total_cases,
                                                na.rm = TRUE)),
                    by = c("area")
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          c("BIRTHID", "CSDuid", "area", "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr = .N),
                        by = list(BrthYear, CSDuid, area, dlv)
                      ][,
                        c("CSDuid", "area", "BrthYear", "dlv", "count_dlv_geo_yr")
                      ])[,
                         `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                      na.rm = TRUE)),
                         by = c("area","BrthYear")
                      ][,
                        c("CSDuid", "area", "BrthYear", "total_lvb_geo_yr", "count_dlv_geo_yr")]
                )[,
                  `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                           na.rm = TRUE)),
                  by = c("BrthYear")
                ][,
                  `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                            na.rm = TRUE)),
                  by = c("area")
                ][,
                  c("CSDuid", "area", "BrthYear",
                    "total_lvb_geo_yr", "total_lvb_yr", "total_lvb_geo")
                ]),
              by = c("CSDuid", "area", "BrthYear"),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(area),
                c("CSDuid", "area", "BrthYear",
                  "total_cases_geo_yr","total_cases_yr", "total_cases_geo",
                  "total_lvb_geo_yr", "total_lvb_yr", "total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("area")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
    } else if (!q == "0" &&
               all(is.na(stringr::str_extract(q, pattern = "\\(.*\\)")))){
      if (tolower(tolower(geo) %in% "csd"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "CSDName", "CSDType", "BrthYear", "cat_tier3"),
                    c("CSDuid", "BrthYear"))[,
                                             c("CSDuid", "CSDName", "BrthYear","cat_tier3", "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(CSDuid, cat_tier3, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE),
                             total_cases_geo_yr = total_cases),
                       by = c("BrthYear","cat_tier3")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                  na.rm = TRUE)),
                      by = c("CSDuid", "cat_tier3")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "CSDName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, dlv)
                    ][,
                      c("CSDuid", "CSDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("CSDuid","BrthYear")
                    ][,
                      c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                      by = c("CSDuid")
                    ],
              by = c("CSDuid", "CSDName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(CSDName),
                c("CSDuid", "CSDName", "BrthYear", "cat_tier3",
                  "total_cases_geo_yr", "total_cases_yr", "total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr", "total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("CSDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "cd"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CDuid", "CDName", "BrthYear","cat_tier3"),
                    c("CDuid", "BrthYear"))[,
                                            c("CDuid", "CDName", "BrthYear","cat_tier3", "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(CDuid, cat_tier3, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE),
                             total_cases_geo_yr = total_cases),
                       by = c("BrthYear","cat_tier3")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                  na.rm = TRUE)),
                      by = c("CDuid","cat_tier3")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CDuid", "CDName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CDuid, dlv)
                    ][,
                      c("CDuid", "CDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("CDuid","BrthYear")
                    ][,
                      c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                      by = c("CDuid")
                    ],
              by = c("CDuid", "CDName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(CDName),
                c("CDuid", "CDName", "BrthYear", "cat_tier3",
                  "total_cases_geo_yr", "total_cases_yr", "total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr","total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("CDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "clus"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "Cluster_Number", "ClusterName", "BrthYear","cat_tier3"),
                    c("Cluster_Number", "BrthYear"))[,
                                                     c("Cluster_Number", "ClusterName", "BrthYear","cat_tier3", "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(Cluster_Number, cat_tier3, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE),
                             total_cases_geo_yr = total_cases),
                       by = c("BrthYear","cat_tier3")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                  na.rm = TRUE)),
                      by = c("Cluster_Number","cat_tier3")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, Cluster_Number, dlv)
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("Cluster_Number","BrthYear")
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                      by = c("Cluster_Number")
                    ],
              by = c("Cluster_Number", "ClusterName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(ClusterName),
                c("Cluster_Number", "ClusterName", "BrthYear", "cat_tier3",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr", "total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("Cluster_Number")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "hn"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "NetworkID", "NetworkName", "BrthYear","cat_tier3"),
                    c("NetworkID", "BrthYear"))[,
                                                c("NetworkID", "NetworkName", "BrthYear","cat_tier3", "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(NetworkID, cat_tier3, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE),
                             total_cases_geo_yr = total_cases),
                       by = c("BrthYear","cat_tier3")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                  na.rm = TRUE)),
                      by = c("NetworkID","cat_tier3")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, NetworkID, dlv)
                    ][,
                      c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("NetworkID","BrthYear")
                    ][,
                      c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                      by = c("NetworkID")
                    ],
              by = c("NetworkID", "NetworkName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(NetworkName),
                c("NetworkID", "NetworkName", "BrthYear", "cat_tier3",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr","total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("NetworkID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "zn"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier3"),
                    c("CSDuid", "ZoneID", "BrthYear"))[,
                                                       c("CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier3", "total_cases")
                    ])[,
                       `:=` (total_cases_geo_yr = sum(total_cases, na.rm = TRUE)),
                       by = list(ZoneID, cat_tier3, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE)
                       ),
                       by = c("BrthYear","cat_tier3")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases,
                                                  na.rm = TRUE)),
                      by = c("ZoneID","cat_tier3")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, ZoneID, dlv)
                    ][,
                      c("CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("ZoneID","BrthYear")
                    ][,
                      c("CSDuid", "ZoneID", "ZnName", "BrthYear", "total_lvb_geo_yr", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                                na.rm = TRUE)),
                      by = c("ZoneID")
                    ],
              by = c("CSDuid", "ZoneID", "ZnName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(ZnName),
                c("CSDuid", "ZoneID", "ZnName", "BrthYear", "cat_tier3",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr","total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("ZoneID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "urb"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "area", "BrthYear","cat_tier3"),
                    c("CSDuid", "area", "BrthYear"))[,
                                                     c("CSDuid", "area", "BrthYear","cat_tier3", "total_cases")
                    ])[,
                       `:=` (total_cases_geo_yr = sum(total_cases, na.rm = TRUE)),
                       by = list(area, cat_tier3, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE)
                       ),
                       by = c("BrthYear","cat_tier3")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases,
                                                  na.rm = TRUE)),
                      by = c("area","cat_tier3")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "area", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, area, dlv)
                    ][,
                      c("CSDuid", "area", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("area","BrthYear")
                    ][,
                      c("CSDuid", "area", "BrthYear", "total_lvb_geo_yr", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                                na.rm = TRUE)),
                      by = c("area")
                    ],
              by = c("CSDuid", "area", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(area),
                c("CSDuid", "area", "BrthYear", "cat_tier3",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr","total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("area")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
    } else{
      if (tolower(tolower(geo) %in% "csd"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "CSDName", "CSDType", "BrthYear", "cat_tier4"),
                    c("CSDuid", "BrthYear"))[,
                                             c("CSDuid", "CSDName", "BrthYear","cat_tier4", "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(CSDuid, cat_tier4, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE),
                             total_cases_geo_yr = total_cases),
                       by = c("BrthYear","cat_tier4")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                  na.rm = TRUE)),
                      by = c("CSDuid","cat_tier4")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "CSDName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, dlv)
                    ][,
                      c("CSDuid", "CSDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("CSDuid","BrthYear")
                    ][,
                      c("CSDuid", "CSDName", "BrthYear", "total_lvb_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                      by = c("CSDuid")
                    ],
              by = c("CSDuid", "CSDName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(CSDName),
                c("CSDuid", "CSDName", "BrthYear", "cat_tier4",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr","total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("CSDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "cd"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CDuid", "CDName", "BrthYear","cat_tier4"),
                    c("CDuid", "BrthYear"))[,
                                            c("CDuid", "CDName", "BrthYear","cat_tier4", "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(CDuid, cat_tier4, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE),
                             total_cases_geo_yr = total_cases),
                       by = c("BrthYear","cat_tier4")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                  na.rm = TRUE)),
                      by = c("CDuid","cat_tier4")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CDuid", "CDName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CDuid, dlv)
                    ][,
                      c("CDuid", "CDName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("CDuid","BrthYear")
                    ][,
                      c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                      by = c("CDuid")
                    ],
              by = c("CDuid", "CDName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(CDName),
                c("CDuid", "CDName", "BrthYear", "cat_tier4",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr","total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("CDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "clus"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "Cluster_Number", "ClusterName", "BrthYear","cat_tier4"),
                    c("Cluster_Number", "BrthYear"))[,
                                                     c("Cluster_Number", "ClusterName", "BrthYear","cat_tier4", "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(Cluster_Number, cat_tier4, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE),
                             total_cases_geo_yr = total_cases),
                       by = c("BrthYear","cat_tier4")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                  na.rm = TRUE)),
                      by = c("Cluster_Number","cat_tier4")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, Cluster_Number, dlv)
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("Cluster_Number","BrthYear")
                    ][,
                      c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                      by = c("Cluster_Number")
                    ],
              by = c("Cluster_Number", "ClusterName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(ClusterName),
                c("Cluster_Number", "ClusterName", "BrthYear", "cat_tier4",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr","total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("Cluster_Number")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "hn"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "NetworkID", "NetworkName", "BrthYear","cat_tier4"),
                    c("NetworkID", "BrthYear"))[,
                                                c("NetworkID", "NetworkName", "BrthYear","cat_tier4", "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(NetworkID, cat_tier4, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE),
                             total_cases_geo_yr = total_cases),
                       by = c("BrthYear","cat_tier4")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases_geo_yr,
                                                  na.rm = TRUE)),
                      by = c("NetworkID","cat_tier4")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, NetworkID, dlv)
                    ][,
                      c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("NetworkID","BrthYear")
                    ][,
                      c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                                na.rm = TRUE)),
                      by = c("NetworkID")
                    ],
              by = c("NetworkID", "NetworkName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(NetworkName),
                c("NetworkID", "NetworkName", "BrthYear", "cat_tier4",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_go_yr","total_lvb_yr", "total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("NetworkID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "zn"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier4"),
                    c("CSDuid", "ZoneID", "BrthYear"))[,
                                                       c("CSDuid", "ZoneID", "ZnName", "BrthYear","cat_tier4", "total_cases")
                    ])[,
                       `:=` (total_cases_geo_yr = sum(total_cases, na.rm = TRUE)),
                       by = list(ZoneID, cat_tier4, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE)
                       ),
                       by = c("BrthYear","cat_tier4")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases,
                                                  na.rm = TRUE)),
                      by = c("ZoneID","cat_tier4")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, ZoneID, dlv)
                    ][,
                      c("CSDuid", "ZoneID", "ZnName", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("ZoneID","BrthYear")
                    ][,
                      c("CSDuid", "ZoneID", "ZnName", "BrthYear", "total_lvb_geo_yr", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                                na.rm = TRUE)),
                      by = c("ZoneID")
                    ],
              by = c("CSDuid", "ZoneID", "ZnName", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(ZnName),
                c("CSDuid", "ZoneID", "ZnName", "BrthYear", "cat_tier4",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr","total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("ZoneID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "urb"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "area", "BrthYear","cat_tier4"),
                    c("CSDuid", "area", "BrthYear"))[,
                                                     c("CSDuid", "area", "BrthYear","cat_tier4", "total_cases")
                    ])[,
                       `:=` (total_cases_geo_yr = sum(total_cases, na.rm = TRUE)),
                       by = list(area, cat_tier4, BrthYear)
                    ])[,
                       `:=` (total_cases_yr = sum(total_cases,
                                                  na.rm = TRUE)
                       ),
                       by = c("BrthYear","cat_tier4")
                    ][,
                      `:=` (total_cases_geo = sum(total_cases,
                                                  na.rm = TRUE)),
                      by = c("area","cat_tier4")
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "CSDuid", "area", "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr = .N),
                      by = list(BrthYear, CSDuid, area, dlv)
                    ][,
                      c("CSDuid", "area", "BrthYear", "dlv", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                    na.rm = TRUE)),
                       by = c("area","BrthYear")
                    ][,
                      c("CSDuid", "area", "BrthYear", "total_lvb_geo_yr", "count_dlv_geo_yr")
                    ])[,
                       `:=` (total_lvb_yr = sum(count_dlv_geo_yr,
                                                na.rm = TRUE)),
                       by = c("BrthYear")
                    ][,
                      `:=` (total_lvb_geo = sum(count_dlv_geo_yr,
                                                na.rm = TRUE)),
                      by = c("area")
                    ],
              by = c("CSDuid", "area", "BrthYear"),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(area),
                c("CSDuid", "area", "BrthYear", "cat_tier4",
                  "total_cases_geo_yr", "total_cases_yr","total_cases_geo",
                  "total_lvb_geo_yr","total_lvb_yr","total_lvb_geo")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr/total_lvb_geo_yr),
                 by = c("area")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
    }
  } else{ ## when risk is selected ---------
    if(is.null(q) || q %in% "0"){
      if (tolower(tolower(geo) %in% "csd"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "CSDuid", "CSDName", "CSDType", "BrthYear", risk),
                  c("CSDuid", "BrthYear", risk))[,
                                                 .SD,
                                                 .SDcols = c("CSDuid", "CSDName", "CSDType",
                                                             "BrthYear", risk, "total_cases")
                  ])[,
                     `:=` (total_cases_yr_risk = sum(total_cases,
                                                     na.rm = TRUE),
                           total_cases_geo_yr_risk = total_cases),
                     by = c("BrthYear", risk)
                  ][,
                    `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                     na.rm = TRUE)),
                    by = c("CSDuid", risk)
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          .SD,
                          .SDcols = c("BIRTHID", "CSDuid", "CSDName",
                                      "BrthYear", risk, "dlv")
                      ][, `:=` (count_dlv_geo_yr_risk = .N),
                        by = list(BrthYear, CSDuid, dlv, get(risk))
                      ][,
                        .SD,
                        .SDcols = c("CSDuid", "CSDName", "BrthYear",
                                    "dlv", risk, "count_dlv_geo_yr_risk")
                      ])[,
                         `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                           na.rm = TRUE)),
                         by = c("CSDuid","BrthYear", risk)
                      ][,
                        .SD,
                        .SDcols = c("CSDuid", "CSDName", "BrthYear", risk,
                                    "total_lvb_geo_yr_risk")]
                )[,
                  `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                na.rm = TRUE)),
                  by = c("BrthYear", risk)
                ][,
                  `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                 na.rm = TRUE)),
                  by = c("CSDuid", risk)
                ][,
                  .SD,
                  .SDcols = c("CSDuid", "CSDName", "BrthYear", risk,
                              "total_lvb_geo_yr_risk", "total_lvb_yr_risk", "total_lvb_geo_risk")
                ]),
              by = c("CSDuid", "CSDName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(CSDName),
                .SD,
                .SDcols = c("CSDuid", "CSDName", "BrthYear", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk", "total_cases_geo_risk",
                            "total_lvb_geo_yr_risk", "total_lvb_yr_risk", "total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("CSDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "cd"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "CDuid", "CDName", "BrthYear", risk),
                  c("CDuid", "BrthYear", risk))[,
                                                .SD,
                                                .SDcols = c("CDuid", "CDName", "BrthYear",
                                                            risk, "total_cases")
                  ])[,
                     `:=` (total_cases_yr_risk = sum(total_cases,
                                                     na.rm = TRUE),
                           total_cases_geo_yr_risk = total_cases),
                     by = c("BrthYear", risk)
                  ][,
                    `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                     na.rm = TRUE)),
                    by = c("CDuid", risk)
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          .SD,
                          .SDcols = c("BIRTHID", "CDuid", "CDName",
                                      "BrthYear", risk, "dlv")
                      ][, `:=` (count_dlv_geo_yr_risk = .N),
                        by = list(BrthYear, CDuid, dlv, get(risk))
                      ][,
                        .SD,
                        .SDcols = c("CDuid", "CDName", "BrthYear",
                                    risk, "dlv", "count_dlv_geo_yr_risk")
                      ])[,
                         `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                           na.rm = TRUE)),
                         by = c("CDuid","BrthYear", risk)
                      ][,
                        .SD,
                        .SDcols = c("CDuid", "CDName", "BrthYear",
                                    risk, "total_lvb_geo_yr_risk")]
                )[,
                  `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                na.rm = TRUE)),
                  by = c("BrthYear", risk)
                ][,
                  `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                 na.rm = TRUE)),
                  by = c("CDuid", risk)
                ][,
                  .SD,
                  .SDcols = c("CDuid", "CDName", "BrthYear", risk,
                              "total_lvb_geo_yr_risk", "total_lvb_yr_risk", "total_lvb_geo_risk")
                ]),
              by = c("CDuid", "CDName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(CDName),
                .SD,
                .SDcols = c("CDuid", "CDName", "BrthYear", risk,
                            "total_cases_geo_yr_risk","total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("CDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "clus"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "Cluster_Number", "ClusterName", "BrthYear", risk),
                  c("Cluster_Number","BrthYear", risk))[,
                                                        .SD,
                                                        .SDcols = c("Cluster_Number", "ClusterName", "BrthYear", risk, "total_cases")
                  ])[,
                     `:=` (total_cases_yr_risk = sum(total_cases,
                                                     na.rm = TRUE),
                           total_cases_geo_yr_risk = total_cases),
                     by = c("BrthYear", risk)
                  ][,
                    `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                     na.rm = TRUE)),
                    by = c("Cluster_Number", risk)
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          .SD,
                          .SDcols = c("BIRTHID", "Cluster_Number", "ClusterName",
                                      risk, "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr_risk = .N),
                        by = list(BrthYear, Cluster_Number, dlv, get(risk))
                      ][,
                        .SD,
                        .SDcols = c("Cluster_Number", "ClusterName", "BrthYear",
                                    risk, "dlv", "count_dlv_geo_yr_risk")
                      ])[,
                         `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                           na.rm = TRUE)),
                         by = c("Cluster_Number","BrthYear", risk)
                      ][,
                        .SD,
                        .SDcols = c("Cluster_Number", "ClusterName", "BrthYear",
                                    risk, "total_lvb_geo_yr_risk")]
                )[,
                  `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                na.rm = TRUE)),
                  by = c("BrthYear", risk)
                ][,
                  `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                 na.rm = TRUE)),
                  by = c("Cluster_Number", risk)
                ][,
                  .SD,
                  .SDcols = c("Cluster_Number", "ClusterName", "BrthYear",
                              risk, "total_lvb_yr_risk", "total_lvb_geo_yr_risk")
                ]),
              by = c("Cluster_Number", "ClusterName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(ClusterName), 
                .SD,
                .SDcols = c("Cluster_Number", "ClusterName", "BrthYear", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk", "total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("Cluster_Number")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "hn"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "NetworkID", "NetworkName", "BrthYear", risk),
                  c("NetworkID", "BrthYear", risk))[,
                                                    .SD,
                                                    .SDcols = c("NetworkID", "NetworkName", "BrthYear",
                                                                risk, "total_cases")
                  ])[,
                     `:=` (total_cases_yr_risk = sum(total_cases,
                                                     na.rm = TRUE),
                           total_cases_geo_yr_risk = total_cases),
                     by = c("BrthYear", risk)
                  ][,
                    `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                     na.rm = TRUE)),
                    by = c("NetworkID", risk)
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          .SD,
                          .SDcols = c("BIRTHID", "NetworkID", "NetworkName",
                                      risk, "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr_risk = .N),
                        by = list(BrthYear, NetworkID, dlv, get(risk))
                      ][,
                        .SD,
                        .SDcols = c("NetworkID", "NetworkName", "BrthYear",
                                    risk, "dlv", "count_dlv_geo_yr_risk")
                      ])[,
                         `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                           na.rm = TRUE)),
                         by = c("NetworkID","BrthYear", risk)
                      ][,
                        .SD,
                        .SDcols = c("NetworkID", "NetworkName", "BrthYear", risk, 
                                    "total_lvb_geo_yr_risk")]
                )[,
                  `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                na.rm = TRUE)),
                  by = c("BrthYear", risk)
                ][,
                  `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                 na.rm = TRUE)),
                  by = c("NetworkID", risk)
                ][,
                  .SD,
                  .SDcols = c("NetworkID", "NetworkName", "BrthYear", risk, 
                              "total_lvb_geo_yr_risk", "total_lvb_yr_risk","total_lvb_geo_risk")
                ]),
              by = c("NetworkID", "NetworkName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(NetworkName),
                .SD,
                .SDcols = c("NetworkID", "NetworkName", "BrthYear", risk,
                            "total_cases_geo_yr_risk","total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk", "total_lvb_yr_risk", "total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("NetworkID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "zn"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear", risk),
                  c("CSDuid", "ZoneID", "BrthYear", risk))[,
                                                           .SD,
                                                           .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                                                                       risk, "total_cases")
                  ])[,
                     `:=` (
                       total_cases_geo_yr_risk = sum(total_cases,
                                                     na.rm = TRUE)),
                     by = c("ZoneID", "BrthYear", risk)
                  ][,
                    `:=` (total_cases_yr_risk = sum(total_cases,
                                                    na.rm = TRUE)),
                    by = c("BrthYear", risk)
                  ][,
                    `:=` (total_cases_geo_risk = sum(total_cases,
                                                     na.rm = TRUE)),
                    by = c("ZoneID", risk)
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          .SD,
                          .SDcols = c("BIRTHID", "CSDuid", "ZoneID", "ZnName",
                                      risk, "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr_risk = .N),
                        by = list(BrthYear, CSDuid, ZoneID, dlv, get(risk))
                      ][,
                        .SD,
                        .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                                    risk, "dlv", "count_dlv_geo_yr_risk")
                      ])[,
                         `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                           na.rm = TRUE)),
                         by = c("ZoneID","BrthYear", risk)
                      ][,
                        .SD,
                        .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear", risk, 
                                    "total_lvb_geo_yr_risk", "count_dlv_geo_yr_risk")]
                )[,
                  `:=` (total_lvb_yr_risk = sum(count_dlv_geo_yr_risk,
                                                na.rm = TRUE)),
                  by = c("BrthYear", risk)
                ][,
                  `:=` (total_lvb_geo_risk = sum(count_dlv_geo_yr_risk,
                                                 na.rm = TRUE)),
                  by = c("ZoneID", risk)
                ][,
                  .SD,
                  .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear", risk, 
                              "total_lvb_geo_yr_risk", "total_lvb_yr_risk","total_lvb_geo_risk")
                ]),
              by = c("CSDuid", "ZoneID", "ZnName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(ZnName),
                .SD,
                .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear", risk,
                            "total_cases_geo_yr_risk","total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk", "total_lvb_yr_risk", "total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("ZoneID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "urb"))
        return(
          unique(
            merge(
              unique(
                getGeoDataByCase(
                  df1,
                  y1,
                  y2,
                  q = NULL,
                  c("CaseID", "CSDuid", "area", "BrthYear", risk),
                  c("CSDuid", "area", "BrthYear", risk))[,
                                                         .SD,
                                                         .SDcols = c("CSDuid", "area", "BrthYear",
                                                                     risk, "total_cases")
                  ])[,
                     `:=` (
                       total_cases_geo_yr_risk = sum(total_cases,
                                                     na.rm = TRUE)),
                     by = c("area", "BrthYear", risk)
                  ][,
                    `:=` (total_cases_yr_risk = sum(total_cases,
                                                    na.rm = TRUE)),
                    by = c("BrthYear", risk)
                  ][,
                    `:=` (total_cases_geo_risk = sum(total_cases,
                                                     na.rm = TRUE)),
                    by = c("area", risk)
                  ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          .SD,
                          .SDcols = c("BIRTHID", "CSDuid", "area",
                                      risk, "BrthYear", "dlv")
                      ][, `:=` (count_dlv_geo_yr_risk = .N),
                        by = list(BrthYear, CSDuid, area, dlv, get(risk))
                      ][,
                        .SD,
                        .SDcols = c("CSDuid", "area", "BrthYear",
                                    risk, "dlv", "count_dlv_geo_yr_risk")
                      ])[,
                         `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                           na.rm = TRUE)),
                         by = c("area","BrthYear", risk)
                      ][,
                        .SD,
                        .SDcols = c("CSDuid", "area", "BrthYear", risk, 
                                    "total_lvb_geo_yr_risk", "count_dlv_geo_yr_risk")]
                )[,
                  `:=` (total_lvb_yr_risk = sum(count_dlv_geo_yr_risk,
                                                na.rm = TRUE)),
                  by = c("BrthYear", risk)
                ][,
                  `:=` (total_lvb_geo_risk = sum(count_dlv_geo_yr_risk,
                                                 na.rm = TRUE)),
                  by = c("area", risk)
                ][,
                  .SD,
                  .SDcols = c("CSDuid", "area", "BrthYear", risk, 
                              "total_lvb_geo_yr_risk", "total_lvb_yr_risk","total_lvb_geo_risk")
                ]),
              by = c("CSDuid", "area", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.y = TRUE)[
                !is.na(area),
                .SD,
                .SDcols = c("CSDuid", "area", "BrthYear", risk,
                            "total_cases_geo_yr_risk","total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk", "total_lvb_yr_risk", "total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("area")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
    } else if (!q == "0" &&
               all(is.na(stringr::str_extract(q, pattern = "\\(.*\\)")))){
      if (tolower(tolower(geo) %in% "csd"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "CSDName", "CSDType",
                      "BrthYear", "cat_tier3", risk),
                    c("CSDuid", "BrthYear", risk))[,
                                                   .SD,
                                                   .SDcols = c("CSDuid", "CSDName", "BrthYear",
                                                               "cat_tier3", risk, "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(CSDuid, cat_tier3, BrthYear)
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE),
                             total_cases_geo_yr_risk = total_cases),
                       by = c("BrthYear","cat_tier3", risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                       na.rm = TRUE)),
                      by = c("CSDuid", "cat_tier3",risk)
                    ],
              unique(
                unique(
                  unique(
                    getSubsetByTimeRange(
                      df2,
                      y1,
                      y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                          .SD,
                          .SDcols = c("BIRTHID", "CSDuid", "CSDName",
                                      "BrthYear", risk, "dlv")
                      ][, `:=` (count_dlv_geo_yr_risk = .N),
                        by = list(BrthYear, CSDuid, dlv, get(risk))
                      ][,
                        .SD,
                        .SDcols = c("CSDuid", "CSDName", "BrthYear",
                                    risk, "dlv", "count_dlv_geo_yr_risk")
                      ])[,
                         `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                           na.rm = TRUE)),
                         by = c("CSDuid","BrthYear", risk)
                      ][,
                        .SD,
                        .SDcols = c("CSDuid", "CSDName", "BrthYear", risk,
                                    "total_lvb_geo_yr_risk")
                      ])[,
                         `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                       na.rm = TRUE)),
                         by = c("BrthYear", risk)
                      ][,
                        `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                       na.rm = TRUE)),
                        by = c("CSDuid", risk)
                      ]),
              by = c("CSDuid", "CSDName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(CSDName),
                .SD,
                .SDcols = c("CSDuid", "CSDName", "BrthYear", "cat_tier3", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("CSDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "cd"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CDuid", "CDName", "BrthYear","cat_tier3", risk),
                    c("CDuid", "BrthYear", risk))[,
                                                  .SD,
                                                  .SDcols = c("CDuid", "CDName", "BrthYear","cat_tier3",
                                                              risk, "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(CDuid, cat_tier3, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE),
                             total_cases_geo_yr_risk = total_cases),
                       by = c("BrthYear","cat_tier3", risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                       na.rm = TRUE)),
                      by = c("CDuid", "cat_tier3",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "CDuid", "CDName", "BrthYear", 
                                    risk, "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, CDuid, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("CDuid", "CDName", "BrthYear", risk, 
                                  "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("CDuid","BrthYear", risk)
                    ][,
                      .SD,
                      .SDcols = c("CDuid", "CDName", "BrthYear", risk,
                                  "total_lvb_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("CDuid", risk)
                    ],
              by = c("CDuid", "CDName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(CDName),
                .SD,
                .SDcols = c("CDuid", "CDName", "BrthYear", "cat_tier3", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("CDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "clus"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "Cluster_Number", "ClusterName",
                      "BrthYear","cat_tier3", risk),
                    c("Cluster_Number", "BrthYear", risk))[,
                                                           .SD,
                                                           .SDcols = c("Cluster_Number", "ClusterName", "BrthYear", "cat_tier3",
                                                                       risk, "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(Cluster_Number, cat_tier3, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE),
                             total_cases_geo_yr_risk = total_cases),
                       by = c("BrthYear","cat_tier3", risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                       na.rm = TRUE)),
                      by = c("Cluster_Number", "cat_tier3",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "Cluster_Number", "ClusterName",
                                    "BrthYear", "dlv", risk)
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, Cluster_Number, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("Cluster_Number", "ClusterName", "BrthYear", "dlv", risk,
                                  "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("Cluster_Number","BrthYear", risk)
                    ][,
                      .SD,
                      .SDcols = c("Cluster_Number", "ClusterName", "BrthYear",
                                  risk, "total_lvb_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("Cluster_Number", risk)
                    ],
              by = c("Cluster_Number", "ClusterName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(ClusterName),
                .SD,
                .SDcols = c("Cluster_Number", "ClusterName", "BrthYear", "cat_tier3", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("Cluster_Number")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "hn"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "NetworkID", "NetworkName", "BrthYear",
                      "cat_tier3", risk),
                    c("NetworkID", "BrthYear", risk))[,
                                                      .SD,
                                                      .SDcols = c("NetworkID", "NetworkName", "BrthYear",
                                                                  "cat_tier3", risk, "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(NetworkID, cat_tier3, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE),
                             total_cases_geo_yr_risk = total_cases),
                       by = c("BrthYear","cat_tier3", risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                       na.rm = TRUE)),
                      by = c("NetworkID", "cat_tier3",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "NetworkID", "NetworkName",
                                    "BrthYear", risk, "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, NetworkID, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("NetworkID", "NetworkName", "BrthYear",
                                  risk, "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("NetworkID","BrthYear", risk)
                    ][,
                      .SD,
                      .SDcols = c("NetworkID", "NetworkName", "BrthYear",
                                  risk, "total_lvb_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("NetworkID", risk)
                    ],
              by = c("NetworkID", "NetworkName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(NetworkName),
                .SD,
                .SDcols = c("NetworkID", "NetworkName", "BrthYear", "cat_tier3", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("NetworkID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "zn"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear",
                      "cat_tier3", risk),
                    c("CSDuid", "ZoneID", "BrthYear", risk))[,
                                                             .SD,
                                                             .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                                                                         "cat_tier3", risk, "total_cases")
                    ])[,
                       `:=` (total_cases_geo_yr_risk = sum(total_cases,
                                                           na.rm = TRUE)),
                       by = list(ZoneID, cat_tier3, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE)),
                       by = c("BrthYear","cat_tier3", risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases,
                                                       na.rm = TRUE)),
                      by = c("ZoneID", "cat_tier3",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "CSDuid", "ZoneID", "ZnName",
                                    "BrthYear", risk, "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, CSDuid, ZoneID, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                                  risk, "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("ZoneID","BrthYear", risk)
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                                  risk, "total_lvb_geo_yr_risk", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(count_dlv_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(count_dlv_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("ZoneID", risk)
                    ],
              by = c("CSDuid", "ZoneID", "ZnName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(ZnName),
                .SD,
                .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear", "cat_tier3", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("ZoneID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "urb"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "area", "BrthYear",
                      "cat_tier3", risk),
                    c("CSDuid", "area", "BrthYear", risk))[,
                                                           .SD,
                                                           .SDcols = c("CSDuid", "area", "BrthYear",
                                                                       "cat_tier3", risk, "total_cases")
                    ])[,
                       `:=` (total_cases_geo_yr_risk = sum(total_cases,
                                                           na.rm = TRUE)),
                       by = list(area, cat_tier3, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE)),
                       by = c("BrthYear","cat_tier3", risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases,
                                                       na.rm = TRUE)),
                      by = c("area", "cat_tier3",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "CSDuid", "area",
                                    "BrthYear", risk, "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, CSDuid, area, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "area", "BrthYear",
                                  risk, "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("area","BrthYear", risk)
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "area", "BrthYear",
                                  risk, "total_lvb_geo_yr_risk", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(count_dlv_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(count_dlv_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("area", risk)
                    ],
              by = c("CSDuid", "area", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(area),
                .SD,
                .SDcols = c("CSDuid", "area", "BrthYear", "cat_tier3", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("area")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
    } else{ 
      if (tolower(tolower(geo) %in% "csd"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "CSDName", "CSDType",
                      "BrthYear", "cat_tier4", risk),
                    c("CSDuid", "BrthYear", risk))[,
                                                   .SD,
                                                   .SDcols = c("CSDuid", "CSDName", "BrthYear", "cat_tier4",
                                                               risk, "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(CSDuid, cat_tier4, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE),
                             total_cases_geo_yr_risk = total_cases),
                       by = c("BrthYear","cat_tier4", risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                       na.rm = TRUE)),
                      by = c("CSDuid", "cat_tier4",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "CSDuid", "CSDName",
                                    "BrthYear", risk, "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, CSDuid, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "CSDName", "BrthYear",
                                  risk, "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("CSDuid","BrthYear", risk)
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "CSDName", "BrthYear",
                                  risk, "total_lvb_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("CSDuid", risk)
                    ],
              by = c("CSDuid", "CSDName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(CSDName),
                .SD,
                .SDcols = c("CSDuid", "CSDName", "BrthYear", "cat_tier4", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_yr_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("CSDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "cd"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CDuid", "CDName", "BrthYear","cat_tier4", risk),
                    c("CDuid", "BrthYear", risk))[,
                                                  .SD,
                                                  .SDcols = c("CDuid", "CDName", "BrthYear",
                                                              "cat_tier4", risk, "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(CDuid, cat_tier4, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE),
                             total_cases_geo_yr_risk = total_cases),
                       by = c("BrthYear","cat_tier4",risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                       na.rm = TRUE)),
                      by = c("CDuid", "cat_tier4", risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "CDuid", "CDName",
                                    "BrthYear", risk, "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, CDuid, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("CDuid", "CDName", "BrthYear", 
                                  risk, "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("CDuid","BrthYear",risk)
                    ][,
                      .SD,
                      .SDcols = c("CDuid", "CDName", "BrthYear",
                                  risk, "total_lvb_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear",risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("CDuid", risk)
                    ],
              by = c("CDuid", "CDName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(CDName),
                .SD,
                .SDcols = c("CDuid", "CDName", "BrthYear", "cat_tier4", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk", "total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("CDuid")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "clus"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "Cluster_Number", "ClusterName",
                      "BrthYear","cat_tier4",risk),
                    c("Cluster_Number", "BrthYear", risk))[,
                                                           .SD,
                                                           .SDcols = c("Cluster_Number", "ClusterName", "BrthYear",
                                                                       "cat_tier4", "total_cases", risk)
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(Cluster_Number, cat_tier4, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE),
                             total_cases_geo_yr_risk = total_cases),
                       by = c("BrthYear","cat_tier4",risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                       na.rm = TRUE)),
                      by = c("Cluster_Number", "cat_tier4",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "Cluster_Number", "ClusterName",
                                    "BrthYear", risk, "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, Cluster_Number, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("Cluster_Number", "ClusterName", "BrthYear",
                                  risk, "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("Cluster_Number","BrthYear",risk)
                    ][,
                      .SD,
                      .SDcols = c("Cluster_Number", "ClusterName", "BrthYear",
                                  "total_lvb_geo_yr_risk", risk)
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("Cluster_Number",risk)
                    ],
              by = c("Cluster_Number", "ClusterName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(ClusterName),
                .SD,
                .SDcols = c("Cluster_Number", "ClusterName", "BrthYear",
                            "cat_tier4", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_yr_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("Cluster_Number")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "hn"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "NetworkID", "NetworkName",
                      "BrthYear","cat_tier4",risk),
                    c("NetworkID", "BrthYear", risk))[,
                                                      .SD,
                                                      .SDcols = c("NetworkID", "NetworkName", "BrthYear",
                                                                  risk, "cat_tier4", "total_cases")
                    ])[,
                       `:=` (total_cases = sum(total_cases, na.rm = TRUE)),
                       by = list(NetworkID, cat_tier4, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE),
                             total_cases_geo_yr_risk = total_cases),
                       by = c("BrthYear","cat_tier4",risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases_geo_yr_risk,
                                                       na.rm = TRUE)),
                      by = c("NetworkID", "cat_tier4",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "NetworkID", "NetworkName",
                                    risk, "BrthYear", "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, NetworkID, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("NetworkID", "NetworkName", "BrthYear",
                                  risk, "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("NetworkID","BrthYear",risk)
                    ][,
                      .SD,
                      .SDcols = c("NetworkID", "NetworkName", "BrthYear",
                                  risk, "total_lvb_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(total_lvb_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("NetworkID", risk)
                    ],
              by = c("NetworkID", "NetworkName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(NetworkName),
                .SD,
                .SDcols = c("NetworkID", "NetworkName", "BrthYear", "cat_tier4",
                            risk, "total_cases_geo_yr_risk", "total_cases_yr_risk", "total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk", "total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("NetworkID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "zn"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "ZoneID", "ZnName", "BrthYear",
                      "cat_tier4", risk),
                    c("CSDuid", "ZoneID", "BrthYear", risk))[,
                                                             .SD,
                                                             .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                                                                         "cat_tier4", risk, "total_cases")
                    ])[,
                       `:=` (total_cases_geo_yr_risk = sum(total_cases,
                                                           na.rm = TRUE)),
                       by = list(ZoneID, cat_tier4, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE)),
                       by = c("BrthYear","cat_tier4", risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases,
                                                       na.rm = TRUE)),
                      by = c("ZoneID", "cat_tier4",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "CSDuid", "ZoneID", "ZnName",
                                    "BrthYear", risk, "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, CSDuid, ZoneID, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                                  risk, "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("ZoneID","BrthYear", risk)
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear",
                                  risk, "total_lvb_geo_yr_risk", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(count_dlv_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(count_dlv_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("ZoneID", risk)
                    ],
              by = c("CSDuid", "ZoneID", "ZnName", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(ZnName),
                .SD,
                .SDcols = c("CSDuid", "ZoneID", "ZnName", "BrthYear", "cat_tier4", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("ZoneID")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
      
      if (tolower(tolower(geo) %in% "urb"))
        return(
          unique(
            merge(
              unique(
                unique(
                  getGeoDataByCase(
                    df1,
                    y1,
                    y2,
                    q,
                    c("CaseID", "CSDuid", "area", "BrthYear",
                      "cat_tier4", risk),
                    c("CSDuid", "area", "BrthYear", risk))[,
                                                           .SD,
                                                           .SDcols = c("CSDuid", "area", "BrthYear",
                                                                       "cat_tier4", risk, "total_cases")
                    ])[,
                       `:=` (total_cases_geo_yr_risk = sum(total_cases,
                                                           na.rm = TRUE)),
                       by = list(area, cat_tier4, BrthYear, get(risk))
                    ])[,
                       `:=` (total_cases_yr_risk = sum(total_cases,
                                                       na.rm = TRUE)),
                       by = c("BrthYear","cat_tier4", risk)
                    ][,
                      `:=` (total_cases_geo_risk = sum(total_cases,
                                                       na.rm = TRUE)),
                      by = c("area", "cat_tier4",risk)
                    ],
              unique(
                unique(
                  getSubsetByTimeRange(
                    df2,
                    y1,
                    y2)[tolower(dlv) %in% c("lvb", "stillbirth"),
                        .SD,
                        .SDcols = c("BIRTHID", "CSDuid", "area",
                                    "BrthYear", risk, "dlv")
                    ][, `:=` (count_dlv_geo_yr_risk = .N),
                      by = list(BrthYear, CSDuid, area, dlv, get(risk))
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "area", "BrthYear",
                                  risk, "dlv", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_geo_yr_risk = sum(count_dlv_geo_yr_risk,
                                                         na.rm = TRUE)),
                       by = c("area","BrthYear", risk)
                    ][,
                      .SD,
                      .SDcols = c("CSDuid", "area", "BrthYear",
                                  risk, "total_lvb_geo_yr_risk", "count_dlv_geo_yr_risk")
                    ])[,
                       `:=` (total_lvb_yr_risk = sum(count_dlv_geo_yr_risk,
                                                     na.rm = TRUE)),
                       by = c("BrthYear", risk)
                    ][,
                      `:=` (total_lvb_geo_risk = sum(count_dlv_geo_yr_risk,
                                                     na.rm = TRUE)),
                      by = c("area", risk)
                    ],
              by = c("CSDuid", "area", "BrthYear", risk),
              allow.cartesian = TRUE,
              all.x = TRUE)[
                !is.na(area),
                .SD,
                .SDcols = c("CSDuid", "area", "BrthYear", "cat_tier4", risk,
                            "total_cases_geo_yr_risk", "total_cases_yr_risk","total_cases_geo_risk",
                            "total_lvb_geo_yr_risk","total_lvb_yr_risk","total_lvb_geo_risk")
              ])[,
                 `:=` (rate = 1000*total_cases_geo_yr_risk/total_lvb_geo_yr_risk),
                 by = c("area")
              ][
                order(BrthYear, -rate)
              ][
                !is.na(rate)
              ]
        )
    }
  }
  
}

# getGeoDataByCaseRisk <- function(df, risk, y1, y2, q, field){
#   eval(substitute({
#   unique(getSubsetByTimeRange(df, y1, y2, q)[,
#                                              .SD,
#                                              .SDcols =
#                                       c("CaseID", field, "BrthYear",
#                                         "cat_tier3","cat_tier4", risk)
#   ])[,
#      `:=` (total_cases = .N),
#      by = .(field, risk)
#   ]
#   }))
# }

# getCountyDataByRisk <- function(df, risk, y1, y2){
#   unique(getSubsetByTimeRange(df, y1, y2, q = NULL)[,
#                                                     .SD,
#                                                     .SDcols =
#                                              c("CaseID","CDuid", "BrthYear",
#                                                "cat_tier3","cat_tier4", risk)
#                                              ])[,
#       `:=` (total_cases = .N),
#       by = .(CDuid, get(risk))
#   ]
# }

### Over time

# getCountyDataTime <- function(df, y1, y2) {
#   unique(getSubsetByTimeRange(df, y1, y2, q = NULL)[,
#                                              c("CaseID","CDuid", "BrthYear",
#                                                "cat_tier3","cat_tier4")
#   ])[,
#      `:=` (total_cases = .N),
#      by = .(CDuid, BrthYear)
#   ]
# }

# getCountyDataByCaseTime <- function(df, y1, y2, q){
#   unique(getSubsetByTimeRange(df, y1, y2, q)[,
#                                       c("CaseID","CDuid", "BrthYear",
#                                         "cat_tier3","cat_tier4")
#   ])[,
#       `:=` (total_cases = .N),
#       by = .(CDuid, BrthYear)
#   ]
# }

# getCountyDataRiskTime <- function(df, risk, y1, y2) {
#   unique(getSubsetByTimeRange(df, y1, y2, q = NULL)[,
#                                                     .SD,
#                                                     .SDcols =
#                                              c("CaseID","CDuid", "BrthYear",
#                                                "cat_tier3","cat_tier4", risk)
#   ])[,
#                                              `:=` (total_cases = .N),
#                                              by = .(CDuid, BrthYear, get(risk))
#   ]
# }

# getCountyDataByCaseRiskTime <- function(df, risk, y1, y2, q){
#   unique(getSubsetByTimeRange(df, y1, y2, q)[,
#                                              .SD,
#                                              .SDcols =
#                                       c("CaseID","CDuid", "BrthYear",
#                                         "cat_tier3","cat_tier4", risk)
#   ])[,
#                                       `:=` (total_cases = .N),
#                                       by = .(CDuid, BrthYear, get(risk))
#   ]
# }

## Source ------
buildGeoDataBySrce <- function(df1,
                               y1, y2,
                               q,
                               geo){
  import("data.table")
  import("scales")
  
  
  if(is.null(q) || q %in% "0"){
    if (tolower(tolower(geo) %in% "csd"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, CSDuid, CSDName,
                                       BrthYear, Diags, SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "cd"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, CDuid, CDName,
                                       BrthYear, Diags, SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "clus"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, Cluster_Number, ClusterName,
                                       BrthYear, Diags, SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "hn"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, NetworkID, NetworkName,
                                       BrthYear, Diags, SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "zn"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, ZoneID, ZnName,
                                       BrthYear, Diags, SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "urb"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, area,
                                       BrthYear, Diags, SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
  } else if (!q == "0" &&
             all(is.na(stringr::str_extract(q, pattern = "\\(.*\\)")))){
    if (tolower(tolower(geo) %in% "csd"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, CSDuid, CSDName,
                                       BrthYear, Diags, cat_tier3,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "cd"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, CDuid, CDName,
                                       BrthYear, Diags, cat_tier3,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "clus"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, Cluster_Number, ClusterName,
                                       BrthYear, Diags, cat_tier3,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "hn"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, NetworkID, NetworkName,
                                       BrthYear, Diags, cat_tier3,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "zn"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, ZoneID, ZnName,
                                       BrthYear, Diags, cat_tier3,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "urb"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, area,
                                       BrthYear, Diags, cat_tier3,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
  } else{
    if (tolower(tolower(geo) %in% "csd"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, CSDuid, CSDName,
                                       BrthYear, Diags, cat_tier4,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "cd"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, CDuid, CDName,
                                       BrthYear, Diags, cat_tier4,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "clus"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, Cluster_Number, ClusterName,
                                       BrthYear, Diags, cat_tier4,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "hn"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, NetworkID, NetworkName,
                                       BrthYear, Diags, cat_tier4,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "zn"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, ZoneID, ZnName,
                                       BrthYear, Diags, cat_tier4,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
    if (tolower(tolower(geo) %in% "urb"))
      return(
        unique(
          getSubsetByTimeRange(df1,
                               y1,
                               y2)[, .(CaseID, area,
                                       BrthYear, Diags, cat_tier4,
                                       SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      )
    
  }
}