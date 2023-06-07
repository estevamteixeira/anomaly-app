# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("data.table")
import("geojsonsf")
import("leaflet")
# import("leaflet.extras")
import("rintrojs")
import("shiny")
import("shinydashboard")
import("shinyjs")
import("utils")

# Define which objects from the module you make available to a user.
# All other objects are kept private, local, to the module.
export("ui")
export("init_server")

# Use and/or register a module as dependency.
# 'use' is similar to 'import' but instead of importing from packages,
# we import from a module.
consts <- use("constants/constants.R")
intro <- readr::read_csv("data/intro.csv")

# It is a variation of 'use' where instead of returning a module 
# as return value, the elements are copied to the calling environment.
expose("utilities/getDataByTimeRange.R")
expose("utilities/plotNSCDMap.R")


ui <- function(id){
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  
  # introBox(data.step = 6, data.intro = intro$text[6],
  box(
    title = "Surveillance Map",
    # title = HTML("Surveillance Map <br>
    #              <span style='color: #777777;
    #              font-size: 14px' >
    #              Prevalence (*cases per 1,000 total births) </span>"),
    status = "primary",
    # tags$caption(
    #   shiny::HTML(
    #     "<span 
    #     style='color: #777777'>
    #     Prevalence (*cases per 1,000 total births)
    #     </span>" 
    #   )),
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 12,
    useShinyjs(),
    # This looks the same as your usual piece of code, 
    # except that the id is wrapped into 
    # the ns() function we defined before
    leaflet::leafletOutput(ns("geomap")),
    ## conditionally show or hide UI elements based on a JavaScript expression
    ## checks if the geo_selected_data input exists and is not undefined
    ## If both conditions are true, the UI elements will be displayed
    conditionalPanel("typeof input.geo_selected_data() !== 'undefined' &&
                     input.geo_selected_data.length() > 0)",
                     uiOutput(ns("controls"))
                     ))
  # )
}


init_server <- function(id, df1, df2, y1, y2, q, lim){
    moduleServer(id, function(input, output, session){
      
      ns <- session$ns
      
      geo_selected_data <- session$userData$table_view$geo_selected
      
      # geo_data is a reactive expression whose results will depend on
      # the periods (initial, final years), and condition selected
      geo_data <- reactive({
        if(tolower(lim()) %in% "csd")
          return(
        merge(consts$csd_shp,
          buildGeoDataByCase(df1 = df1,
                           df2 = df2,
                           y1 = y1(),
                           y2 = y2(),
                           q = q(),
                           geo = lim())[,
                                        `:=` (total_cases = ifelse(
                                          total_cases < 5,
                                          "< 5",
                                          as.character(
                                            scales::comma(
                                              total_cases,
                                              accuracy = 1))),
                                          total_lvb_geo = ifelse(
                                            total_lvb_geo < 5,
                                            "< 5",
                                            as.character(
                                              scales::comma(
                                                total_lvb_geo,
                                                accuracy = 1)))
                                        )],
          by.x = c("GeoUID"),
          by.y = c("CSDuid"),
          all.x = TRUE)[,
                        c("GeoUID", "name","csd_type","CMA_UID","total_cases",
                          "total_lvb_geo", "rate","geometry")
          ][,
            `:=` (rate = data.table::fifelse(!rate %in% 0,
                                             rate,
                                             NA_integer_))] |> 
          sf::st_sf() |>
          sf::st_transform(4326)
          )
        
        if(tolower(lim()) %in% "cd")
          return(
            merge(consts$cd_shp,
                  buildGeoDataByCase(df1 = df1,
                                     df2 = df2,
                                     y1 = y1(),
                                     y2 = y2(),
                                     q = q(),
                                     geo = lim())[,
                                                  `:=` (total_cases = ifelse(
                                                    total_cases < 5,
                                                    "< 5",
                                                    as.character(
                                                      scales::comma(
                                                        total_cases,
                                                        accuracy = 1))),
                                                    total_lvb_geo = ifelse(
                                                      total_lvb_geo < 5,
                                                      "< 5",
                                                      as.character(
                                                        scales::comma(
                                                          total_lvb_geo,
                                                          accuracy = 1)))
                                                  )],
                  by.x = c("GeoUID"),
                  by.y = c("CDuid"),
                  all.x = TRUE)[,
                                c("GeoUID", "name", "total_cases",
                                  "total_lvb_geo", "rate","geometry")
                  ][,
                    `:=` (rate = data.table::fifelse(!rate %in% 0,
                                                     rate,
                                                     NA_integer_))] |> 
              sf::st_sf() |>
              sf::st_transform(4326)
          )
        
        if(tolower(lim()) %in% "clus")
          return(
            merge(consts$clus,
                  buildGeoDataByCase(df1 = df1,
                                     df2 = df2,
                                     y1 = y1(),
                                     y2 = y2(),
                                     q = q(),
                                     geo = lim())[,
                                                  `:=` (total_cases = ifelse(
                                                    total_cases < 5,
                                                    "< 5",
                                                    as.character(
                                                      scales::comma(
                                                        total_cases,
                                                        accuracy = 1))),
                                                    total_lvb_geo = ifelse(
                                                      total_lvb_geo < 5,
                                                      "< 5",
                                                      as.character(
                                                        scales::comma(
                                                          total_lvb_geo,
                                                          accuracy = 1)))
                                                  )],
                  by.x = c("clusterid"),
                  by.y = c("Cluster_Number"),
                  all.x = TRUE)[,
                                c("clusterid", "cluster", "total_cases",
                                  "total_lvb_geo", "rate","geometry")
                  ][,
                    `:=` (rate = data.table::fifelse(!rate %in% 0,
                                                     rate,
                                                     NA_integer_),
                          ClusterName = cluster,
                          cluster = NULL)] |> 
              sf::st_sf() |>
              sf::st_transform(4326)
          )
        
        if(tolower(lim()) %in% "hn")
          return(
            merge(consts$hn,
                  buildGeoDataByCase(df1 = df1,
                                     df2 = df2,
                                     y1 = y1(),
                                     y2 = y2(),
                                     q = q(),
                                     geo = lim())[,
                                                  `:=` (total_cases = ifelse(
                                                    total_cases < 5,
                                                    "< 5",
                                                    as.character(
                                                      scales::comma(
                                                        total_cases,
                                                        accuracy = 1))),
                                                    total_lvb_geo = ifelse(
                                                      total_lvb_geo < 5,
                                                      "< 5",
                                                      as.character(
                                                        scales::comma(
                                                          total_lvb_geo,
                                                          accuracy = 1)))
                                                  )],
                  by.x = c("network_id"),
                  by.y = c("NetworkID"),
                  all.x = TRUE)[,
                                c("network_id", "network", "total_cases",
                                  "total_lvb_geo", "rate","geometry")
                  ][,
                    `:=` (rate = data.table::fifelse(!rate %in% 0,
                                                     rate,
                                                     NA_integer_),
                          NetworkName = network,
                          network = NULL)] |> 
              sf::st_sf() |>
              sf::st_transform(4326)
          )
        
        if(tolower(lim()) %in% "urb")
          return(
            merge(consts$csd,
                  buildGeoDataByCase(df1 = df1,
                                     df2 = df2,
                                     y1 = y1(),
                                     y2 = y2(),
                                     q = q(),
                                     geo = lim())[,
                                                  `:=` (total_cases = ifelse(
                                                    total_cases < 5,
                                                    "< 5",
                                                    as.character(
                                                      scales::comma(
                                                        total_cases,
                                                        accuracy = 1))),
                                                    total_lvb_geo = ifelse(
                                                      total_lvb_geo < 5,
                                                      "< 5",
                                                      as.character(
                                                        scales::comma(
                                                          total_lvb_geo,
                                                          accuracy = 1)))
                                                  )],
                  by.x = c("GeoUID"),
                  by.y = c("CSDuid"),
                  all.x = TRUE)[,
                                c("GeoUID", "CMA_UID","csd_type", "name","area","total_cases",
                                  "total_lvb_geo", "rate","geometry")
                  ][,
                    `:=` (area = fifelse(
                            is.na(CMA_UID),
                            "Rural",
                            "Urban"))
                    ][,
                      GeoUID := as.numeric(
                        factor(area,
                               levels = c("Urban","Rural")))
                    ][,`:=` (
                      total_cases = unique(total_cases[!is.na(total_cases)]),
                      total_lvb_geo = unique(total_lvb_geo[!is.na(total_lvb_geo)]),
                      rate = unique(rate[!is.na(rate)])
                      ), by = area
                      ] |>
              sf::st_sf() |>
              sf::st_transform(4326)
          )
        
      })
      
      ## Plot map
     output$geomap <- leaflet::renderLeaflet({
        
       validate(need(nrow(geo_data()) > 0 ||
                       !all(is.na(geo_data()$total_cases)),
             "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))
        
       plot_map(geo_data(), "rate", y1(), y2(), lim())
       
      })
     
     ## Reset button
     output$controls <- renderUI({
       req(geo_selected_data())
       
       absolutePanel(id = "controls",
                     top = "auto", bottom = 100, 
                     left = "auto", right = 50,
                     width = "auto", height = "auto",
                     actionButton(inputId = ns("reset"),
                                  label = "Reset",
                                  style = "
                                  background: #008D8B;
                                  color: white;
                                  border-color: #00706Ey;
                                  font-weight: bold;
                                  font-size: 14px !important;"))
       
     })
     
     observeEvent(geo_selected_data(),{
       
       if(tolower(lim()) %in% "csd"){
         new_view <- geo_data()[which(geo_data()$GeoUID == geo_selected_data()$GeoUID),]
       } else if(tolower(lim()) %in% "cd"){
         new_view <- geo_data()[which(geo_data()$GeoUID == geo_selected_data()$GeoUID),]
       }else if(tolower(lim()) %in% "clus"){
         new_view <- geo_data()[which(geo_data()$clusterid == geo_selected_data()$GeoUID),]
       }else if(tolower(lim()) %in% "hn"){
         new_view <- geo_data()[which(geo_data()$network_id == geo_selected_data()$GeoUID),]
       }else{ #if(tolower(lim()) %in% "zn"){
         new_view <- geo_data()[which(geo_data()$ZoneID == geo_selected_data()$GeoUID),]
       # } else {
       # new_view <- geo_data()
       }
       
       
       shinyjs::show("controls")
       
       leaflet::leafletProxy("geomap", data = new_view) |>
         leaflet::removeShape("bounds") |>
         leaflet::setView(
             lat = mean(sf::st_bbox(new_view)[c(2,4)]),
             lng = mean(sf::st_bbox(new_view)[c(1,3)]),
             zoom = 8) |>
           leaflet::addPolygons(data = new_view,
                                color = "#FF0000",
                                opacity = 1,
                                fill = FALSE,
                                weight = 3,
                                layerId = "bounds")
         
     })
     
    
     
     ## Resetting map to original value when reset button is clicked
     observeEvent(input$reset, {
       # browser()
       
       # Hiding the control button
       shinyjs::hide("controls")
       
       # plotting original map
       leaflet::leafletProxy("geomap", data = geo_data()) |>
         leaflet::removeShape("bounds") |>
         leaflet::setView(
           lat = mean(sf::st_bbox(geo_data())[c(2,4)]),
           lng = mean(sf::st_bbox(geo_data())[c(1,3)]),
           zoom = 6.45)
       
       
     })
    })
}



