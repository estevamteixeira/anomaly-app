page_navbar(
 title = "SCA-NS",
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
 )
)
