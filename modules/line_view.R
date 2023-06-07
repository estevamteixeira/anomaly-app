# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("data.table")
import("ggplot2")
import("plotly")
import("rintrojs")
import("shiny")
import("shinyBS")
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
intro <- readr::read_csv("data/intro.csv")

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
    title = tags$span("Surveillance Over Time",
                      ## little 'i' btn
                      bsButton("line_info",
                               label = "",
                               icon = icon("info"),
                               style = "primary",
                               size = "extra-small")),
    status = "primary",
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 12,
    fluidRow(
      column(width = 3,
             selectInput(
               inputId = ns("line_risk"),
               label = tags$span(
                 style = "color: #008D8B;",
                 "Risk Factor",
                 ## little 'i' btn
                 bsButton("risk_info",
                          label = "",
                          icon = icon("info"),
                          style = "primary",
                          size = "extra-small")
                 ),
               choices = c("None" = "0",
                           consts$risk_opts),
               selected = c("0"))
             )),
    # This looks the same as your usual piece of code, 
    # except that the id is wrapped into 
    # the ns() function we defined before
    plotly::plotlyOutput(ns("geoline")),
    bsPopover(
      id = "risk_info",
      title = "More Information",
      content = paste0(
        "<li> <b>Alcohol Use</b><br>",
        "Any documented alcohol use during pregnancy.</li>",
        "<li> <b>BMI</b><br>",
        "Not obese (BMI < 30), Obese I, II (30 &#8804 BMI < 40), ",
        "Obese (BMI &#8805 40).</li>",
        "<li> <b>Cannabis Use</b><br>",
        "Any use of cannabis or hashish during pregnancy.</li>",
        "<li> <b>Diabetes</b><br> ",
        "Pre-existing, pre-pregnancy, and in-pregnancy diabetes.</li>",
        "<li> <b>Fetal Sex</b><br> ",
        "Female, male.</li>",
        "<li> <b>Location</b><br> ",
        "Urban, rural.</li>",
        "<li> <b>Maternal Age</b><br> ",
        "Mother&#x27s age at birth.</li>",
        "<li> <b>Smoking Use</b><br> ",
        "Any use of cigarettes during or before pregnancy.</li>"
      ),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    )
    )
}


init_server <- function(id, df1, df2, y1, y2, q, lim){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # stores info from geo_view module when clicking the 
    # table
    geo_selected_data <- session$userData$table_view$geo_selected
    
    # stores risk factor info
    line_risk <- reactive({ input$line_risk })
    
    # geo_data is a reactive expression whose results will depend on
    # the periods (initial, final years - y1(), y2()), 
    # the condition - q()
    # the risk - line_risk() and
    # the geography - lim() 
    
    geo_data <- reactive({
      dta <-buildGeoDataByCaseTimeRisk(df1 = df1,
                             df2 = df2,
                             y1 = y1(),
                             y2 = y2(),
                             q = q(),
                             risk = line_risk(),
                             geo = lim())
      
      # For urban rural the logic is different
      # because we are using the CSD's to build the maps
      if(tolower(lim()) %in% "urb"){
        dta <- unique(dta, by = c("area","BrthYear"))[,
                CSDuid := as.numeric(
                  factor(area,
                         levels = c("Urban","Rural")))
                ]
      }
        
      setnames(dta,
               old = colnames(dta)[1:2],
               new = c("GeoUID", "Name"))
      
      return(dta)
    })
    
    
    output$geoline <- plotly::renderPlotly({
      
      validate(need(nrow(geo_data()) > 0 ||
                      !all(is.na(geo_data()$total_cases)),
                    "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))
      
      # Check risk factor option and construct
      # corresponding data set
      if(is.null(line_risk()) || line_risk() %in% 0){
        # selected county from table? If yes...
        if(isTRUE(nrow(geo_selected_data()) > 0)){
          plot_line(
            data = subset(geo_data(),
                          GeoUID %in% geo_selected_data()$GeoUID,
                          select = c(GeoUID, Name, BrthYear,
                                     total_cases_geo_yr, rate)),
            "rate"
            )
        } else{ # If no...
          plot_line(
            data = unique(geo_data()[,`:=` (
              ntc = total_cases_yr,
              ntlvb = total_lvb_yr)
              ][,
                .(BrthYear, ntc, ntlvb)
                ])[,
                  `:=` (rate = 1000*ntc/ntlvb,
                        Name = "Nova Scotia")],
            "rate"
            )
        }
      } else{ # risk factors other than 'none'
        # selected county from table? If yes...
        if(isTRUE(nrow(geo_selected_data()) > 0)){
          plot_risk_line(
            data = geo_data()[
              GeoUID %in% geo_selected_data()$GeoUID,
              .SD,
              .SDcols = c("GeoUID", "Name", "BrthYear",
                          line_risk(), "total_cases_geo_yr_risk", "rate")
                          ],
            "rate",
            line_risk())
        } else{ # If no...
          plot_risk_line(
            data = unique(geo_data()[,`:=` (
              ntc = total_cases_yr_risk,
              ntlvb = total_lvb_yr_risk
              )][,
                 .SD,
                 .SDcols = c("BrthYear", line_risk(), "ntc", "ntlvb")
                ])[,`:=` (
                     rate = 1000*ntc/ntlvb,
                     Name = "Nova Scotia")
                  ],
            "rate", line_risk())
          }
        }
      })
    })
}



