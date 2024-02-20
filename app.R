library(bslib)
library(dplyr)
library(modules)
library(shiny)

consts <- use("constants/constants.R")
homeTab <- use("modules/mod-home.R")
summTab <- use("modules/mod-summary.R")
mapTab <- use("modules/mod-map.R")
trendTab <- use("modules/mod-trend.R")
aboutTab <- use("modules/mod-about.R")

ui <- page_navbar(
 title = consts$app_title,
 id = "navbar",
 collapsible = TRUE,
 theme = bslib::bs_theme(
  version = 5,
  bootswatch = "minty",
  bg = "#FFFFFF",
  fg = "#00706E",
  primary = "#5C9895",
  secondary = "#AAAAAA",
  base_font = font_google("Montserrat", local = TRUE)
  ) %>%
  bs_add_rules(".optgroup-header {font-size: 1rem !important; color: #AAAAAA !important;}"),
      nav_panel("Home",
                homeTab$homeUI("home"),
                icon = bsicons::bs_icon("house-fill")
                ),
      nav_panel("Summary",
                summTab$summUI("summary"),
                icon = bsicons::bs_icon("list-ul")
                ),
      nav_panel("Map Tool",
                mapTab$mapUI("map"),
                icon = bsicons::bs_icon("geo-fill")
      ),
 nav_panel("Trend",
           trendTab$trendUI("trend"),
           icon = bsicons::bs_icon("graph-up-arrow")
           ),
 nav_menu("Help",
          icon = bsicons::bs_icon("question-circle-fill"),
          nav_panel("Summary",
                    icon = bsicons::bs_icon("list-ul"),
                    withMathJax(
                     HTML(
                      readLines("modules/summ.html")))),
          nav_panel("Map Tool",
                    icon = bsicons::bs_icon("geo-fill"),
                    withMathJax(
                     HTML(
                      readLines("modules/map.html")))),
          nav_panel("Trend",
                    icon = bsicons::bs_icon("graph-up-arrow"),
                    withMathJax(
                     HTML(
                      readLines("modules/trend.html"))))
          ),
          nav_panel("About",
           aboutTab$aboutUI("map")
           )
   )

server <- function(input, output, session) {

   # Home tab server ----
   session$userData$homeTab <- homeTab$homeServer("home")

   # Summary tab server ----
   session$userData$summTab <- summTab$summServer(
    id = "summary",
    df1 = consts$icd_lbl,
    df2 = consts$birth,
    df3 = consts$ano,
    df4 = consts$src_lbl)

   # Map tab server ----
   session$userData$mapTab <- mapTab$mapServer(
    id = "map",
    df1 = consts$icd_lbl,
    df2 = consts$ano,
    df3 = consts$geo_lbl,
    df4 = consts$cd_shp,
    df5 = consts$cl_shp,
    df6 = consts$chn_shp,
    df7 = consts$hr_shp)

 # Lineplot tab server ----
   session$userData$trendTab <- trendTab$trendServer(
    id = "trend",
    df1 = consts$icd_lbl,
    df2 = consts$ano,
    df3 = consts$risk_lbl)
 }


 # profvis::profvis({runApp(
shinyApp(ui, server)
 # )})

