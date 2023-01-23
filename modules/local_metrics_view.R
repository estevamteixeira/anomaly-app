# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("dplyr")
import("rintrojs")
import("shiny")

# Define which objects from the module you make available to a user.
# All other objects are kept private, local, to the module.
export("ui")
export("init_server")

# Use and/or register a module as dependency.
# 'use' is similar to 'import' but instead of importing from packages,
# we import from a module.
consts <- use("constants/constants.R")

# It is a variation of 'use' where instead of returning a module 
# as return value, the elements are copied to the calling environment.
expose("utilities/getDataByTimeRange.R")

ui <- function(id){
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  
  # Metrics with dynamic titles and values depending on selected county
  tagList(
    div(
      class = "box box-primary metric metric-local metric-local-1",
      div(
        class = "icon"
      ),
      div(
        class = "value",
        # We need to ns() all ids 
        uiOutput(ns("metricsboxtitle1")),
        textOutput(ns("metricsbox1"))
      )
    ),
    div(
      class = "box box-primary metric metric-local metric-local-2",
      div(
        class = "icon"
      ),
      div(
        class = "value",
        introBox(data.step = 4, data.intro = consts$intro$text[4],
          # We need to ns() all ids 
        uiOutput(ns("metricsboxtitle2")),
        textOutput(ns("metricsbox2"))
      ))
    ),
    div(
      class = "box box-primary metric metric-local metric-local-3",
      div(
        class = "icon"
      ),
      div(
        class = "value",
        # We need to ns() all ids 
        uiOutput(ns("metricsboxtitle3")),
        textOutput(ns("metricsbox3"))
      )
    )
    # div(
    #   class = "box box-primary metric metric-local metric-local-4",
    #   div(
    #     class = "icon"
    #   ),
    #   div(
    #     class = "value",
    #     # We need to ns() all ids 
    #     uiOutput(ns("metricsboxtitle4")),
    #     textOutput(ns("metricsbox4"))
    #   )
    # )
  )
}

init_server <- function(id, df, y1, y2, q){
  moduleServer(id, function(input, output, session){
    ## Setting id for session
    ns <- session$ns
    
    # userData : An environment to store whatever
    # session-specific data you want.
    # https://shiny.rstudio.com/reference/shiny/latest/session.html
    # 'cd_selected' comes from the 'county_view' module and stores the selected
    # county from the table.
    cd_selected_data <- session$userData$county_view$cd_selected
    
    # county_birth is a reactive expression whose results will depend on
    # the periods (initial, final years)
    county_birth <- reactive({
      unique(getSubsetByTimeRange(consts$cd_birth,
                                      y1(),
                                      y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                                            c("CD_UID", "cd.count_dlv")
                                      ][,
                                        `:=` (total_lvb = sum(cd.count_dlv)),
                                        by = c("CD_UID")
                                      ][,
                                        c("CD_UID","total_lvb")])
    })
    
    # county_anom is a reactive expression whose results will depend on
    # the periods (initial, final years), and condition selected
    
    county_anom <- reactive({
      if(is.null(q()) || q() == "0"){
        merge(unique(
          getCountyData(df,
                        y1(),
                        y2())[, c("CD_UID", "total_cases")
                        ]),
          unique(getSubsetByTimeRange(consts$cd_birth,
                                      y1(),
                                      y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                                            c("CD_UID", "cd.count_dlv")
                                      ][,
                                        `:=` (total_lvb = sum(cd.count_dlv)),
                                        by = c("CD_UID")
                                      ][,
                                        c("CD_UID","total_lvb")]),
          by = c("CD_UID"))
      } else if (!q() == "0" &&
                 is.na(stringr::str_extract(q(), pattern = "\\(.*\\)"))){
        merge(unique(
          getCountyDataByCase(df, y1(), y2(), q())[,
                                                   c("CD_UID", "cat_tier3", "total_cases")
          ]),
          unique(getSubsetByTimeRange(consts$cd_birth,
                                      y1(),
                                      y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                                            c("CD_UID", "cd.count_dlv")
                                      ][,
                                        `:=` (total_lvb = sum(cd.count_dlv)),
                                        by = c("CD_UID")
                                      ][,
                                        c("CD_UID","total_lvb")]),
          by = c("CD_UID"))
      } else{
        merge(unique(
          getCountyDataByCase(df, y1(), y2(), q())[,
                                                   c("CD_UID", "cat_tier4", "total_cases")
          ]),
          unique(getSubsetByTimeRange(consts$cd_birth,
                                      y1(),
                                      y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                                            c("CD_UID", "cd.count_dlv")
                                      ][,
                                        `:=` (total_lvb = sum(cd.count_dlv)),
                                        by = c("CD_UID")
                                      ][,
                                        c("CD_UID","total_lvb")]),
          by = c("CD_UID"))
      }
    })
    
    output$metricsboxtitle1 <- renderUI({
      if(isTRUE(nrow(cd_selected_data()) > 0))
        tags$label(paste(cd_selected_data()$cd_full,
                         "total births"
        )
        )
      else tags$label("Nova Scotia total births")
    })
    
    output$metricsboxtitle2 <- renderUI({
      if(isTRUE(nrow(cd_selected_data()) > 0)){
        tags$label(paste(cd_selected_data()$cd_full,
                         "recorded congenital anomalies"
        )
        ) 
      }
      else{
        tags$label("Nova Scotia recorded congenital anomalies")
      } 
    })
    
    output$metricsboxtitle3 <- renderUI({
      if(isTRUE(nrow(cd_selected_data()) > 0)){
        tags$label(paste(cd_selected_data()$cd_full,
                         "Prevalence \n (* cases per 1,000 total births)"
        )
        ) 
      }
      else{
        tags$label("Nova Scotia prevalence \n (* cases per 1,000 total births)")
      } 
    })
    
    # output$metricsboxtitle4 <- renderUI({
    #   if(isTRUE(nrow(cd_selected_data()) > 0)){
    #     tags$label(paste(cd_selected_data()$cd_full,
    #                      "infants diagnosed"
    #     )
    #     ) 
    #   }
    #   else{
    #     tags$label("Nova Scotia infants diagnosed")
    #   } 
    # })
    
    output$metricsbox1 <- renderText({
      if(isTRUE(nrow(cd_selected_data()) > 0)){
        cd_selected_data()$total_lvb
      } else{
        ifelse(sum(county_birth()$total_lvb) < 5,
               "< 5",
               scales::comma(sum(county_birth()$total_lvb),
                            accuracy = 1))
      }
    })
    
    output$metricsbox2 <- renderText({
      if(isTRUE(nrow(cd_selected_data()) > 0)){
        cd_selected_data()$total_cases
      } else{
        ifelse(sum(county_anom()$total_cases) < 5,
               "< 5",
               scales::comma(sum(county_anom()$total_cases),
                             accuracy = 1))
      }
    })
    
    output$metricsbox3 <- renderText({
      if(isTRUE(nrow(cd_selected_data()) > 0)){
        scales::comma(cd_selected_data()$rate, accuracy = 0.1)
      } else{
        scales::comma(
          1000*sum(county_anom()$total_cases)/sum(county_birth()$total_lvb),
          accuracy = 0.1)
      }
    })
    
    # output$metricsbox4 <- renderText({
    #   if(isTRUE(nrow(cd_selected_data()) > 0)){
    #     scales::comma(cd_selected_data()$rate, accuracy = 0.1)
    #   } else{
    #     scales::comma(
    #       1000*sum(county_anom()$total_cases)/sum(county_birth()$total_lvb),
    #       accuracy = 0.1)
    #   }
    # })
  })
}


