# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("data.table")
import("geojsonsf")
import("leaflet")
import("rintrojs")
import("shiny")
import("shinydashboard")
import("utils")

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
expose("utilities/plotNSCDMap.R")


ui <- function(id){
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  
  introBox(data.step = 6, data.intro = consts$intro$text[6],
  box(
    title = HTML("Surveillance Map <br>
                 <span style='color: #777777;
                 font-size: 14px' >
                 Prevalence (*cases per 1,000 total births) </span>"),
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
    # This looks the same as your usual piece of code, 
    # except that the id is wrapped into 
    # the ns() function we defined before
    leaflet::leafletOutput(ns("countymap"))
    )
  )
}


init_server <- function(id, df, y1, y2, q){
    moduleServer(id, function(input, output, session){
      
      ns <- session$ns
      
      cd_selected_data <- session$userData$county_view$cd_selected
      
      # county_data is a reactive expression whose results will depend on
      # the periods (initial, final years), and condition selected
      county_data <- reactive({
        if(is.null(q()) || q() == "0"){
          merge(
            unique(
              getCountyData(
                df,
                y1(),
                y2())[, c("CD_UID", "total_cases")
                ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  consts$cd_birth,
                  y1(),
                  y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("CD_UID", "BrthYear", "dlv")
                  ][, `:=` (cd.count_dlv = .N),
                    by = list(BrthYear, CD_UID, dlv)
                  ])[,
                     `:=` (total_lvb_cd = sum(cd.count_dlv,
                                              na.rm = TRUE)),
                     by = c("CD_UID","BrthYear")
                  ][,
                    c("CD_UID", "BrthYear", "total_lvb_cd")]
            )[,
              `:=` (total_lvb = sum(total_lvb_cd,
                                    na.rm = TRUE)),
              by = c("CD_UID")
            ],
            by = c("CD_UID"),
            allow.cartesian = TRUE)[,
                                    `:=` (rate = 1000*total_cases/total_lvb),
                                    by = c("CD_UID")
            ][
              order(-rate)
            ][,
              `:=` (total_cases = ifelse(
                total_cases < 5,
                "< 5",
                as.character(
                  scales::comma(
                    total_cases,
                    accuracy = 1))),
                total_lvb = ifelse(
                  total_lvb < 5,
                  "< 5",
                  as.character(
                    scales::comma(
                      total_lvb,
                      accuracy = 1)))
              )] %>%
            merge(consts$cd_names, by = c("CD_UID")) %>%
            .[, .(CD_UID, cd_full, total_cases, total_lvb, rate)] %>%
            .[order(-rate)] %>% 
            unique()
        } else if (!q() == "0" &&
                   is.na(stringr::str_extract(q(), pattern = "\\(.*\\)"))){
          merge(
            unique(
              getCountyDataByCase(
                df,
                y1(),
                y2(),
                q())[,
                     c("CD_UID", "cat_tier3", "total_cases")
                ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  consts$cd_birth,
                  y1(),
                  y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("CD_UID", "BrthYear", "dlv")
                  ][, `:=` (cd.count_dlv = .N),
                    by = list(BrthYear, CD_UID, dlv)
                  ])[,
                     `:=` (total_lvb_cd = sum(cd.count_dlv,
                                              na.rm = TRUE)),
                     by = c("CD_UID","BrthYear")
                  ][,
                    c("CD_UID", "BrthYear", "total_lvb_cd")]
            )[,
              `:=` (total_lvb = sum(total_lvb_cd,
                                    na.rm = TRUE)),
              by = c("CD_UID")
            ],
            by = c("CD_UID"),
            allow.cartesian = TRUE)[,
                                    `:=` (rate = 1000*total_cases/total_lvb),
                                    by = c("CD_UID")
            ][
              order(-rate)
            ][,
              `:=` (total_cases = ifelse(
                total_cases < 5,
                "< 5",
                as.character(
                  scales::comma(
                    total_cases,
                    accuracy = 1))),
                total_lvb = ifelse(
                  total_lvb < 5,
                  "< 5",
                  as.character(
                    scales::comma(
                      total_lvb, 
                      accuracy = 1)))
              )] %>%
            merge(consts$cd_names, by = c("CD_UID")) %>%
            .[,c("CD_UID", "cd_full", "total_cases", "total_lvb", "rate")] %>%
            .[order(-rate)] %>% 
            unique()
        } else{
          merge(
            unique(
              getCountyDataByCase(
                df,
                y1(),
                y2(),
                q())[,
                     c("CD_UID", "cat_tier4", "total_cases")
                ]),
            unique(
              unique(
                getSubsetByTimeRange(
                  consts$cd_birth,
                  y1(),
                  y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                        c("CD_UID", "BrthYear", "dlv")
                  ][, `:=` (cd.count_dlv = .N),
                    by = list(BrthYear, CD_UID, dlv)
                  ])[,
                     `:=` (total_lvb_cd = sum(cd.count_dlv,
                                              na.rm = TRUE)),
                     by = c("CD_UID","BrthYear")
                  ][,
                    c("CD_UID", "BrthYear", "total_lvb_cd")]
            )[,
              `:=` (total_lvb = sum(total_lvb_cd,
                                    na.rm = TRUE)),
              by = c("CD_UID")
            ],
            by = c("CD_UID"),
            allow.cartesian = TRUE)[,
                                    `:=` (rate = 1000*total_cases/total_lvb),
                                    by = c("CD_UID")
            ][
              order(-rate)
            ][,
              `:=` (total_cases = ifelse(
                total_cases < 5,
                "< 5",
                as.character(
                  scales::comma(
                    total_cases,
                    accuracy = 1))),
                total_lvb = ifelse(
                  total_lvb < 5,
                  "< 5",
                  as.character(
                    scales::comma(
                      total_lvb,
                      accuracy = 1)))
              )] %>%
            merge(consts$cd_names, by = c("CD_UID")) %>%
            .[,c("CD_UID", "cd_full", "total_cases", "total_lvb", "rate")] %>%
            .[order(-rate)] %>% 
            unique()
        }
      })
      
      
     output$countymap <- leaflet::renderLeaflet({
        
       validate(need(nrow(county_data()) > 0 ||
                       !all(is.na(county_data()$total_cases)),
             "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))
        
        dta_map <- merge(consts$cd_shp,
                         county_data(),
                         by.x = c("GeoUID"),
                         by.y = c("CD_UID"),
                         all.x = TRUE)[,
                c("GeoUID", "name","cd_type","total_cases",
                  "total_lvb", "rate","geometry")
                ][,
                `:=` (rate = data.table::fifelse(!rate %in% 0,
                                                 rate,
                                                 NA_integer_))] |> 
          sf::st_sf() |>
          sf::st_transform(4326)
        
        if(isTRUE(nrow(cd_selected_data()) > 0)){
          plot_map(dta_map, "rate", y1(), y2()) |>
            leaflet::setView(lat = mean(sf::st_bbox(
              dta_map[which(dta_map$GeoUID == cd_selected_data()$CD_UID),])[c(2,4)]
              ),
                    lng = mean(sf::st_bbox(
              dta_map[which(dta_map$GeoUID == cd_selected_data()$CD_UID),])[c(1,3)]
              ),
              zoom = 8) |> 
            leaflet::addPolygons(data = dta_map[which(dta_map$GeoUID == cd_selected_data()$CD_UID),],
            color = "#FF0000",
            opacity = 1,
            fill = FALSE,
            weight = 3)
          } else{
          plot_map(dta_map, "rate", y1(), y2())
        }
        
        # }
      })
    })
}



