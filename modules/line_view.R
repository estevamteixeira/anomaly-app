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
expose("utilities/plotNSCDLine.R")


ui <- function(id){
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  
  box(
    title = "Surveillance Over Time",
    status = "primary",
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 12,
    fluidRow(
      column(width = 3,
             selectInput(
               inputId = ns("line_risk"),
               label = shiny::HTML(
                 "<p><span style='color: #008d8b'>Risk Factor</span></p>" 
               ),
               choices = c("None" = "0",
                           "Phenotypical Sex" = "SexNum"),
               selected = c("0"))
             )
      ),
    # This looks the same as your usual piece of code, 
    # except that the id is wrapped into 
    # the ns() function we defined before
    plotly::plotlyOutput(ns("countyline"))
  )
}


init_server <- function(id, df, y1, y2, q){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    cd_selected_data <- session$userData$county_view$cd_selected
    
    line_risk <- reactive({ input$line_risk })
    
    # county_data is a reactive expression whose results will depend on
    # the periods (initial, final years), and condition selected
    
    county_data <- reactive({
      
    if(line_risk() %in% "0"){
      if(is.null(q()) || q() %in% "0"){
          merge(unique(
            getCountyDataTime(df,
                              y1(),
                              y2())[, c("CD_UID", "BrthYear","total_cases")
                                    ]),
            unique(
              getSubsetByTimeRange(consts$cd_birth,
                                   y1(),
                                   y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                                         c("CD_UID", "BrthYear", "cd.count_dlv")
                                         ][,
                                          `:=` (total_lvb_cd = sum(cd.count_dlv,
                                                                   na.rm = TRUE)),
                                          by = c("CD_UID","BrthYear")
                                        ][,
                                          `:=` (total_lvb_year = sum(cd.count_dlv, 
                                                                     na.rm = TRUE)),
                                          by = c("BrthYear")
                                        ]),
            by = c("CD_UID", "BrthYear"))[,
                                         `:=` (rate = 1000*total_cases/total_lvb_cd),
                                         by = c("CD_UID", "BrthYear")
            ][
              order(-rate)
            ] %>%
            merge(consts$cd_names, by = c("CD_UID")) %>%
            .[, .(CD_UID, cd_full, BrthYear, total_cases,
                  total_lvb_cd, rate, total_lvb_year)] %>%
            .[order(-rate)]
        } else{
          merge(unique(
            getCountyDataByCaseTime(
              df,
              y1(),
              y2(),
              q())[,
                   c("CD_UID", "BrthYear","cat_tier2", "total_cases")
            ]),
            unique(
              getSubsetByTimeRange(
                consts$cd_birth,
                y1(),
                y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("CD_UID", "BrthYear", "cd.count_dlv")
                      ][,
                        `:=` (total_lvb_cd = sum(cd.count_dlv,
                                                 na.rm = TRUE)),
                        by = c("CD_UID","BrthYear")
                      ][,
                        `:=` (total_lvb_year = sum(cd.count_dlv, 
                                                   na.rm = TRUE)),
                        by = c("BrthYear")
                      ]),
            by = c("CD_UID", "BrthYear"))[,
                                         `:=` (rate = 1000*total_cases/total_lvb_cd),
                                         by = c("CD_UID", "BrthYear")
            ][
              order(-rate)
            ] %>%
            merge(consts$cd_names, by = c("CD_UID")) %>%
            .[,c("CD_UID", "cd_full", "BrthYear", "total_cases",
                 "total_lvb_cd", "rate", "total_lvb_year")] %>%
            .[order(-rate)]
        }
    } else{
      if(is.null(q()) || q() %in% "0"){
          merge(unique(
            getCountyDataRiskTime(df,
                                  line_risk(),
                                  y1(),
                                  y2())[, c("CD_UID", "BrthYear",
                                            line_risk(), "total_cases")
                                  ]),
            unique(
              getSubsetByTimeRange(
                consts$cd_birth,
                y1(),
                y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                      c("CD_UID", "BrthYear", "cd.count_dlv")
                                   ][,
                                     `:=` (total_lvb_cd = sum(cd.count_dlv,
                                                              na.rm = TRUE)),
                                     by = c("CD_UID","BrthYear")
                                   ][,
                                     `:=` (total_lvb_year = sum(cd.count_dlv, 
                                                                na.rm = TRUE)),
                                     by = c("BrthYear")
                                   ]),
            by = c("CD_UID", "BrthYear"))[,
                                         `:=` (rate = 1000*total_cases/total_lvb_cd),
                                         by = c("CD_UID", "BrthYear", line_risk())
            ][
              order(-rate)
            ] %>%
            merge(consts$cd_names, by = c("CD_UID")) %>%
            .[, c("CD_UID", "cd_full", "BrthYear", line_risk(),
                  "total_cases", "total_lvb_cd", "rate","total_lvb_year")] %>%
            .[order(-rate)]
        } else{
          merge(unique(
            getCountyDataByCaseRiskTime(df,
                                        line_risk(),
                                        y1(),
                                        y2(),
                                        q())[,
                                             c("CD_UID", "BrthYear",
                                               "cat_tier2", line_risk(), "total_cases")
                                        ]),
            unique(getSubsetByTimeRange(consts$cd_birth,
                                        y1(),
                                        y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                                              c("CD_UID", "BrthYear", "cd.count_dlv")
                                        ][,
                                          `:=` (total_lvb_cd = sum(cd.count_dlv,
                                                                   na.rm = TRUE)),
                                          by = c("CD_UID","BrthYear")
                                        ][,
                                          `:=` (total_lvb_year = sum(cd.count_dlv, 
                                                                     na.rm = TRUE)),
                                          by = c("BrthYear")
                                        ]),
            by = c("CD_UID", "BrthYear"))[,
                                         `:=` (rate = 1000*total_cases/total_lvb_cd),
                                         by = c("CD_UID", "BrthYear", line_risk())
            ][
              order(-rate)
            ] %>%
            merge(consts$cd_names, by = c("CD_UID")) %>%
            .[,c("CD_UID", "cd_full", "BrthYear", line_risk(),
                 "total_cases", "total_lvb_cd", "rate", "total_lvb_year")] %>%
            .[order(-rate)]
        }
      }
    })
    
    
    
    
    output$countyline <- plotly::renderPlotly({
      
      validate(need(nrow(county_data()) > 0 ||
                      !all(is.na(county_data()$total_cases)),
                    "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))
      
      # Check risk factor option and construct
      # corresponding data set
      if(line_risk() %in% 0){
        # selected county from table? If yes...
        if(isTRUE(nrow(cd_selected_data()) > 0)){
          plot_line(
            data = subset(county_data(),
                          CD_UID %in% cd_selected_data()$CD_UID,
                          select = c(CD_UID, cd_full, BrthYear,
                                     total_cases, total_lvb_cd, rate)),
            "rate"
            )
        } else{ # If no...
          plot_line(
            data = county_data() %>%
              .[,`:=` (ntc = sum(total_cases, na.rm = TRUE),
                       ntlvb = total_lvb_year),
                by = .(BrthYear)] %>% 
              .[,.(BrthYear, ntc, ntlvb)] %>%
              unique() %>% 
              .[,`:=` (nrate = 1000*ntc/ntlvb,
                       cd_full = "Nova Scotia")],
            "nrate"
            )
        }
      } else{ # risk factors other than 'none'
        # selected county from table? If yes...
        if(isTRUE(nrow(cd_selected_data()) > 0)){
          plot_risk_line(
            data = county_data()[
              CD_UID %in% cd_selected_data()$CD_UID,
              .SD,
              .SDcols = c("CD_UID", "cd_full", "BrthYear",
                          line_risk(), "total_cases", "total_lvb_cd", "rate")
                          ],
            "rate",
            line_risk())
        } else{ # If no...
          plot_risk_line(
            data = county_data() %>%
              .[,`:=` (ntc = sum(total_cases, na.rm = TRUE),
                       ntlvb = total_lvb_year),
              by = c("BrthYear", line_risk())] %>%
              .[,c("BrthYear", line_risk(),"ntc", "ntlvb")] %>%
              unique() %>% 
              .[,`:=` (nrate = 1000*ntc/ntlvb,
                       cd_full = "Nova Scotia")],
            "nrate", line_risk())
          }
        }
      })
    })
}



