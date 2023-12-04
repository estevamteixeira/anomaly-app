library(bslib)
library(dplyr)
library(modules)
library(shiny)

consts <- use("R/constants.R")
homeTab <- use("R/mod-home.R")
summTab <- use("R/mod-summary.R")


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
         bs_add_rules(".optgroup-header {font-size: 1rem !important; color: #00706E;}"),
      # nav_panel("Home",
      #           homeTab$homeUI("home"),
      #           icon = bsicons::bs_icon("house-fill")
      #           ),
      nav_panel("Summary",
                summTab$summUI("summary"),
                icon = bsicons::bs_icon("list-ul")
                )
   )


server <- function(input, output, session) {

   # Home tab server ----
   # homeTab$homeServer("home")

   # Summary tab server ----
   summTab$summServer(
      id = "summary",
      df1 = consts$icd_lbl,
      df2 = consts$birth,
      df3 = consts$ano)

}

shinyApp(ui, server)

