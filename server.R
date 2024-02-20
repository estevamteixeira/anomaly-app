consts <- use("constants/constants.R")

function(input, output, session) {

 # Home tab server ----
 session$userData$homeTab <- homeTab$homeServer("home")

 # Summary tab server ----
 session$userData$summTab <- summTab$summServer(
  id = "summary",
  df1 = consts$icd_lbl,
  df2 = consts$birth,
  df3 = consts$ano)

 # Map tab server ----
 session$userData$mapTab <- mapTab$mapServer(
  id = "map",
  df1 = consts$icd_lbl,
  df2 = consts$ano,
  df3 = consts$geo_lbl,
  df4 = consts$cd_shp,
  df5 = consts$cl_shp,
  df6 = consts$chn_shp,
  df7 = consts$hr_shp
 )

 # Lineplot tab server ----
 session$userData$trendTab <- trendTab$trendServer(
  id = "trend",
  df1 = consts$icd_lbl,
  df2 = consts$ano,
  df3 = consts$risk_lbl
 )
}
