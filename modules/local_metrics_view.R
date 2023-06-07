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
 # consts <- use("constants/constants.R")
intro <- readr::read_csv("data/intro.csv")

# It is a variation of 'use' where instead of returning a module 
# as return value, the elements are copied to the calling environment.
expose("utilities/getDataByTimeRange.R")

ui <- function(id){
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  
  # Metrics with dynamic titles and values depending on selected geography
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
        introBox(data.step = 4, data.intro = intro$text[4],
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
    ),
    div(
      class = "box box-primary metric metric-local metric-local-4",
      div(
        class = "icon"
      ),
      div(
        class = "value",
        # We need to ns() all ids
        uiOutput(ns("metricsboxtitle4")),
        textOutput(ns("metricsbox4"))
      )
    )
  )
}

init_server <- function(id, df1, df2, y1, y2, q, lim){
  moduleServer(id, function(input, output, session){
    ## Setting id for session
    ns <- session$ns
    
    # userData : An environment to store whatever
    # session-specific data you want.
    # https://shiny.rstudio.com/reference/shiny/latest/session.html
    # 'geo_selected' comes from the 'table_view' module and stores the selected
    # county from the table.
    geo_selected_data <- session$userData$table_view$geo_selected
    
    # birth is a reactive expression whose results will depend on -----
    # the periods (initial, final years)
    geo_birth <- reactive({
      if (tolower(lim()) %in% "csd")
        return(
          unique(
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1(),
                  y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
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
            ])
        )
      
      if (tolower(lim()) %in% "cd")
        return(
          unique(
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1(),
                  y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
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
                    c("CDuid", "CDName", "BrthYear", "total_lvb_geo_yr")]
            )[,
              `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                        na.rm = TRUE)),
              by = c("CDuid")
            ][,
              c("CDuid", "CDName", "total_lvb_geo")
            ])
        )
      
      if (tolower(lim()) %in% "clus")
        return(
          unique(
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1(),
                  y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "Cluster_Number", "ClusterName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, Cluster_Number, dlv)
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("Cluster_Number", "ClusterName", "BrthYear")
                  ][,
                    c("Cluster_Number", "ClusterName", "BrthYear", "total_lvb_geo_yr")]
            )[,
              `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                        na.rm = TRUE)),
              by = c("Cluster_Number")
            ][,
              c("Cluster_Number", "ClusterName", "total_lvb_geo")
            ])
        )
      
      if (tolower(lim()) %in% "hn")
        return(
          unique(
            unique(
              unique(
                getSubsetByTimeRange(
                  df2,
                  y1(),
                  y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("BIRTHID", "NetworkID", "NetworkName", "BrthYear", "dlv")
                  ][, `:=` (count_dlv_geo_yr = .N),
                    by = list(BrthYear, NetworkID, dlv)
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "dlv", "count_dlv_geo_yr")
                  ])[,
                     `:=` (total_lvb_geo_yr = sum(count_dlv_geo_yr,
                                                  na.rm = TRUE)),
                     by = c("NetworkID", "NetworkName", "BrthYear")
                  ][,
                    c("NetworkID", "NetworkName", "BrthYear", "total_lvb_geo_yr")]
            )[,
              `:=` (total_lvb_geo = sum(total_lvb_geo_yr,
                                        na.rm = TRUE)),
              by = c("NetworkID")
            ][,
              c("NetworkID", "NetworkName", "total_lvb_geo")
            ])
        )
      
      if (tolower(lim()) %in% "zn")
        return(
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
                ])
        )
      
      if (tolower(lim()) %in% "urb")
        return(
          unique(
            unique(
              getSubsetByTimeRange(
                df2,
                y1(),
                y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
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
                  c("area", "total_lvb_geo")
                ])
        )
      
    })
    
    # anom is a reactive expression whose results will depend on -----
    # the periods (initial, final years), and condition selected
    
    geo_anom <- reactive({
      buildGeoDataByCase(df1 = df1,
                         df2 = df2,
                         y1 = y1(),
                         y2 = y2(),
                         q = q(),
                         geo = lim())
    })
    
    # Title --------
    # output$metricsboxtitle1 <- renderUI({
    #   if(isTRUE(nrow(geo_selected_data()) > 0)){
    #     tags$label(paste(geo_selected_data()[[which(grepl("name",tolower(names(geo_selected_data()))))]],
    #                      "total births"))
    #   } else {
    #     tags$label("Nova Scotia total births")
    #     }
    # })
    
    output$metricsboxtitle1 <- renderUI({
      tags$label("Nova Scotia total births")
    })
    
    # output$metricsboxtitle2 <- renderUI({
    #   if(isTRUE(nrow(geo_selected_data()) > 0)){
    #     tags$label(paste(geo_selected_data()[[which(grepl("name",tolower(names(geo_selected_data()))))]],
    #                      "recorded congenital anomalies")) 
    #   }
    #   else{
    #     tags$label("Nova Scotia recorded congenital anomalies")
    #   } 
    # })
    
    output$metricsboxtitle2 <- renderUI({
      tags$label("Nova Scotia recorded congenital anomalies")
    })
    
    # output$metricsboxtitle3 <- renderUI({
    #   if(isTRUE(nrow(geo_selected_data()) > 0)){
    #     tags$label(paste(geo_selected_data()[[which(grepl("name",tolower(names(geo_selected_data()))))]],
    #                      "Prevalence (* cases per 1,000 total births)")) 
    #   } else{
    #     tags$label("Nova Scotia prevalence (* cases per 1,000 total births)")
    #   } 
    # })
    
    output$metricsboxtitle3 <- renderUI({
      tags$label("Nova Scotia prevalence (* cases per 1,000 total births)")
    })
    
    # output$metricsboxtitle4 <- renderUI({
    #   if(isTRUE(nrow(cd_geo_selected()) > 0)){
    #     tags$label(paste(cd_geo_selected()$cd_full,
    #                      "infants diagnosed"
    #     )
    #     )
    #   }
    #   else{
    #     tags$label("Nova Scotia infants diagnosed")
    #   }
    # })
    
    output$metricsboxtitle4 <- renderUI({
      if(tolower(lim()) %in% "csd")
        return(
          tags$label("Municipality")
          )
      
      if(tolower(lim()) %in% "cd")
        return(
          tags$label("County")
        )
      
      if(tolower(lim()) %in% "clus")
        return(
          tags$label("Community cluster")
        )
      
      if(tolower(lim()) %in% "hn")
        return(
          tags$label("Community health network")
        )
      
      if(tolower(lim()) %in% "zn")
        return(
          tags$label("Management zones")
        )
      
      if(tolower(lim()) %in% "urb")
        return(
          tags$label("Urban-rural")
        )
    })
    
    # Values --------
    # output$metricsbox1 <- renderText({
    #   if(isTRUE(nrow(geo_selected_data()) > 0)){
    #     geo_selected_data()$total_lvb
    #   } else{
    #     ifelse(sum(geo_birth()$total_lvb_geo,
    #                na.rm = TRUE) < 5,
    #            "< 5",
    #            scales::comma(sum(geo_birth()$total_lvb_geo,
    #                              na.rm = TRUE),
    #                         accuracy = 1))
    #   }
    # })
    
    output$metricsbox1 <- renderText({
      ifelse(sum(geo_birth()$total_lvb_geo,
                   na.rm = TRUE) < 5,
               "< 5",
               scales::comma(sum(geo_birth()$total_lvb_geo,
                                 na.rm = TRUE),
                             accuracy = 1))
    })
    
    # output$metricsbox2 <- renderText({
    #   if(isTRUE(nrow(geo_selected_data()) > 0)){
    #     geo_selected_data()$total_cases
    #   } else{
    #     ifelse(sum(geo_anom()$total_cases,
    #                na.rm = TRUE) < 5,
    #            "< 5",
    #            scales::comma(sum(geo_anom()$total_cases,
    #                              na.rm = TRUE),
    #                          accuracy = 1))
    #   }
    # })
    
    output$metricsbox2 <- renderText({
      if(tolower(lim()) %in% "urb")
        return(
          ifelse(sum(unique(geo_anom()[,.(area, total_cases)])$total_cases,
                     na.rm = TRUE) < 5,
                 "< 5",
                 scales::comma(
                   sum(unique(geo_anom()[,.(area, total_cases)])$total_cases,
                       na.rm = TRUE),
                   accuracy = 1))
        )
      
      return(
      ifelse(sum(geo_anom()$total_cases,
                   na.rm = TRUE) < 5,
               "< 5",
               scales::comma(sum(geo_anom()$total_cases,
                                 na.rm = TRUE),
                             accuracy = 1))
      )
    })
    
    # output$metricsbox3 <- renderText({
    #   if(isTRUE(nrow(geo_selected_data()) > 0)){
    #     scales::comma(geo_selected_data()$rate, accuracy = 0.1)
    #   } else{
    #     scales::comma(
    #       1000*sum(geo_anom()$total_cases,
    #                na.rm = TRUE)/sum(geo_birth()$total_lvb_geo,
    #                                  na.rm = TRUE),
    #       accuracy = 0.1)
    #   }
    # })
    
    output$metricsbox3 <- renderText({
      if(tolower(lim()) %in% "urb")
        return(
          scales::comma(
            1000*sum(unique(geo_anom()[,.(area, total_cases)])$total_cases,
                     na.rm = TRUE)/sum(unique(geo_birth()[,.(area, total_lvb_geo)])$total_lvb_geo,
                                       na.rm = TRUE),
            accuracy = 0.1)
        )
      
      return(
        scales::comma(
        1000*sum(geo_anom()$total_cases,
                 na.rm = TRUE)/sum(geo_birth()$total_lvb_geo,
                                   na.rm = TRUE),
        accuracy = 0.1)
        )
    })
    
    # output$metricsbox4 <- renderText({
    #   if(isTRUE(nrow(cd_geo_selected()) > 0)){
    #     scales::comma(cd_geo_selected()$rate, accuracy = 0.1)
    #   } else{
    #     scales::comma(
    #       1000*sum(county_anom()$total_cases)/sum(county_birth()$total_lvb),
    #       accuracy = 0.1)
    #   }
    # })
    
    output$metricsbox4 <- renderText({
      if(tolower(lim()) %in% "csd")
        return(
          95
        )
      
      if(tolower(lim()) %in% "cd")
        return(
          18
        )
      
      if(tolower(lim()) %in% "clus")
        return(
          54
        )
      
      if(tolower(lim()) %in% "hn")
        return(
         14
        )
      
      if(tolower(lim()) %in% "zn")
        return(
          4
        )
      
      if(tolower(lim()) %in% "urb")
        return(
          2
        )
    })
  })
}


