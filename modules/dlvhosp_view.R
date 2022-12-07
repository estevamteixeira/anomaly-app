# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("data.table")
import("ggplot2")
import("plotly")
import("shiny")
import("shinydashboard")
import("tidyverse")
import("utils")

# Define which objects from the module you make available to a user.
# All other objects are kept private, local, to the module.
modules::export("ui")
modules::export("init_server")

# Use and/or register a module as dependency.
# 'use' is similar to 'import' but instead of importing from packages,
# we import from a module.
consts <- use("constants/constants.R")

# It is a variation of 'use' where instead of returning a module 
# as return value, the elements are copied to the calling environment.
expose("utilities/getDataByTimeRange.R")
expose("utilities/plotNSCDBar.R")

ui <- function(id){
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  
  box(
    title = "Top 5 Delivery Hospitals",
    status = "primary",
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 12,
    # This looks the same as your usual piece of code, 
    # except that the id is wrapped into 
    # the ns() function we defined before
    plotly::plotlyOutput(ns("countybar"))
  )
}

init_server <- function(id, df, y1, y2, q){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    cd_selected_data <- session$userData$county_view$cd_selected
    
    # county_data is a reactive expression whose results will depend on
    # the periods (initial, final years), and condition selected
    
    county_data <- reactive({
      if(is.null(q()) || q() %in% "0"){
        unique(
          getCountyDataByRisk(df,
                              "DLHosp",
                              y1(),
                              y2())[, c("CD_UID", "DLHosp", "total_cases")
                                    ]
          )[,
            `:=` (total_cases_cd = sum(total_cases, na.rm = TRUE)),
            by = c("CD_UID")
          ][,
            `:=` (prop = total_cases/total_cases_cd),
            by = c("CD_UID")
          ] %>%
          merge(consts$cd_names, by = c("CD_UID")) %>%
          .[, c("CD_UID", "cd_full", "DLHosp",
                "total_cases", "total_cases_cd", "prop")] %>%
          .[order(-prop)] %>% 
          unique()
      } else{
        unique(
          getCountyDataByCaseRisk(
            df,
            "DLHosp",
            y1(),
            y2(),
            q())[,
                 c("CD_UID", "cat_tier2", "DLHosp", "total_cases")
                 ]
          )[,
            `:=` (total_cases_cd = sum(total_cases, na.rm = TRUE)),
            by = c("CD_UID")
          ][,
            `:=` (prop = total_cases/total_cases_cd),
            by = c("CD_UID")
          ] %>%
          merge(consts$cd_names, by = c("CD_UID")) %>%
          .[,c("CD_UID", "cd_full", "DLHosp",
               "total_cases", "total_cases_cd", "prop")] %>%
          .[order(-prop)] %>% 
          unique()
        }
      })
    
    
    output$countybar <- plotly::renderPlotly({
      
      validate(need(nrow(county_data()) > 0 ||
                      !all(is.na(county_data()$total_cases)),
                    "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))
      
      # selected county from table? If yes...
        if(isTRUE(nrow(cd_selected_data()) > 0)){
          plot_bar(
            data = subset(county_data(),
                          CD_UID %in% cd_selected_data()$CD_UID,
                          select = c(CD_UID, cd_full, DLHosp,
                                     total_cases, total_cases_cd, prop)) %>% 
              .[order(-prop)] %>% 
              head(5),
            "prop"
          )
        } else{ # If no...
          plot_bar(
            data = county_data() %>%
              .[,`:=` (ntc = sum(total_cases, na.rm = TRUE)),
                by = .(DLHosp)] %>%
              .[,`:=` (ntc_cd = sum(total_cases, na.rm = TRUE))
                ] %>%
              .[,.(DLHosp, ntc, ntc_cd)] %>%
              unique() %>% 
              .[,`:=` (prop = ntc/ntc_cd,
                       cd_full = "Nova Scotia")] %>% 
              .[order(-prop)] %>%
              head(5),
            "prop"
          )
        }
    })
  })
}