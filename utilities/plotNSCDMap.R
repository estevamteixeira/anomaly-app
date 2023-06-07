plot_map <- function(df1, var, y1, y2, geo){
  # Use 'import' from the 'modules' package.
  # These listed imports are made available inside the module scope.
  import("leaflet")
  
  dta <- df1
  var <- unlist(var)
  #intv <- pretty(dta[[var]])
  
  pal <- colorBin(c("#cce2e2", "#00706e", "#002423"),
                  domain = dta[[var]]
                  #bins = intv
  )
  
  if(tolower(geo) %in% c("csd")){
    county_label <- ifelse(is.na(dta[[var]]) | dta[[var]] == 0,
                           paste(
                             "<b>",stringr::str_to_title(dta[[which(grepl("name",tolower(names(dta))))]]),"</b>",
                             "<br>",stringr::str_to_title(dta[[which(grepl("type",tolower(names(dta))))]]),
                             # "<br>Census ID:", dta[["GeoUID"]],
                             "<br>No information provided"
                           ),
                           paste(
                             "<b>",stringr::str_to_title(dta[[which(grepl("name",tolower(names(dta))))]]),"</b>",
                             "<br>",stringr::str_to_title(dta[[which(grepl("type",tolower(names(dta))))]]),
                             # "<br>Census ID:", dta[["GeoUID"]],
                             "<br>Reported occurences:", dta[[which(grepl("total_cases",names(dta)))]],
                             # "<br>Total births:", dta[[which(grepl("total_lvb",names(dta)))]],
                             "<br>Prevalence (*cases per 1,000 total births):" , scales::comma(dta[[var]], accuracy = 0.1)
                           )
    ) |> lapply(htmltools::HTML)
    
    ## get urban CSDs based on
    ## https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2021023-eng.htm
    urb <- sort(unique(dta[!is.na(dta[["CMA_UID"]]),][["GeoUID"]]))

    urban_label <- ifelse(is.na(dta[[var]]) | dta[[var]] == 0,
                          paste(
                            "<b>",stringr::str_to_title(dta[dta[["GeoUID"]] %in% urb,][[which(grepl("name",names(dta)))]]),"</b>",
                            "<br>",stringr::str_to_title(dta[dta[["GeoUID"]] %in% urb,][[which(grepl("type",names(dta)))]]),
                            # "<br>Census ID:", dta[["GeoUID"]],
                            "<br>No information provided"
                          ),
                          paste(
                            "<b>",stringr::str_to_title(dta[dta[["GeoUID"]] %in% urb,][[which(grepl("name",names(dta)))]]),"</b>",
                            "<br>",stringr::str_to_title(dta[dta[["GeoUID"]] %in% urb,][[which(grepl("type",names(dta)))]]),
                            # "<br>Census ID:", dta[["GeoUID"]],
                            "<br>Reported occurences:", dta[dta[["GeoUID"]] %in% urb,][[which(grepl("total_cases",names(dta)))]],
                            # "<br>Total births:", dta[dta[["GeoUID"]] %in% urb,][[which(grepl("total_lvb_geo",names(dta)))]],
                            "<br>Prevalence (*cases per 1,000 total births):" , scales::comma(dta[dta[["GeoUID"]] %in% urb,][[which(grepl(var, names(dta)))]], accuracy = 0.1)
                          )
                          ) |> lapply(htmltools::HTML)
  } else if(tolower(geo) %in% c("urb")){
    county_label <- ifelse(is.na(dta[[var]]) | dta[[var]] == 0,
                           paste(
                             "<b>",stringr::str_to_title(dta[[which(grepl("name",tolower(names(dta))))]]),"</b>",
                             "<br>","<b>","<i>",stringr::str_to_title(dta[[which(grepl("area",tolower(names(dta))))]]),"</i>","</b>",
                             "<br>",stringr::str_to_title(dta[[which(grepl("type",tolower(names(dta))))]]),
                             
                             # "<br>Census ID:", dta[["GeoUID"]],
                             "<br>No information provided"
                           ),
                           paste(
                             "<b>",stringr::str_to_title(dta[[which(grepl("name",tolower(names(dta))))]]),"</b>",
                             "<br>","<b>","<i>",stringr::str_to_title(dta[[which(grepl("area",tolower(names(dta))))]]),"</i>","</b>",
                             "<br>",stringr::str_to_title(dta[[which(grepl("type",tolower(names(dta))))]]),
                             # "<br>Census ID:", dta[["GeoUID"]],
                             "<br>Reported occurences:", dta[[which(grepl("total_cases",names(dta)))]],
                             # "<br>Total births:", dta[[which(grepl("total_lvb",names(dta)))]],
                             "<br>Prevalence (*cases per 1,000 total births):" , scales::comma(dta[[var]], accuracy = 0.1)
                           )
    ) |> lapply(htmltools::HTML)
  } else{
    county_label <- ifelse(is.na(dta[[var]]) | dta[[var]] == 0,
                           paste(
                             "<b>",stringr::str_to_title(dta[[which(grepl("name",tolower(names(dta))))]]),"</b>",
                             # "<br>Census ID:", dta[["GeoUID"]],
                             "<br>No information provided"
                           ),
                           paste(
                             "<b>",stringr::str_to_title(dta[[which(grepl("name",tolower(names(dta))))]]),"</b>",
                             # "<br>Census ID:", dta[["GeoUID"]],
                             "<br>Reported occurences:", dta[[which(grepl("total_cases",names(dta)))]],
                             # "<br>Total births:", dta[[which(grepl("total_lvb",names(dta)))]],
                             "<br>Prevalence (*cases per 1,000 total births):" , scales::comma(dta[[var]], accuracy = 0.1)
                           )
    ) |> lapply(htmltools::HTML)
  }
  
  
  # if(tolower(geo) %in% "csd"){
  #   leaflet::leaflet(
  #     data = dta,
  #     padding = 0,
  #     # trying to focus the map
  #     # default = 6.45
  #     options = leafletOptions(minZoom = 6.45)
  #   ) |>
  #     # leaflet::addProviderTiles("CartoDB") |> 
  #     leaflet::addPolygons(
  #       stroke = TRUE,
  #       fillColor = ~pal(dta[[var]]),
  #       weight = 0.5,
  #       opacity = 1,
  #       color = "white",
  #       dashArray = "1",
  #       fillOpacity = 1,
  #       highlightOptions = highlightOptions(
  #         weight = 2,
  #         color = "#00706e",
  #         dashArray = "",
  #         # fillColor = "#cce9e2",
  #         # fillOpacity = 0.1,
  #         bringToFront = TRUE),
  #       label = county_label,
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal",
  #                      padding = "3px 8px"
  #         ),
  #         textsize = "15px",
  #         direction = "auto")) |>
  #     leaflet::addPolygons(
  #       data = dta[dta[["GeoUID"]] %in% urb,] |> 
  #         sf::st_sf() |>
  #         sf::st_transform(4326),
  #       fillColor = "#E08214",
  #       fillOpacity = 0.2,
  #       stroke= TRUE,
  #       weight = 1.5,
  #       opacity = 1,
  #       label = urban_label,
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal",
  #                      padding = "3px 8px"
  #         ),
  #         textsize = "15px",
  #         direction = "auto"),
  #       color = "white",
  #       group = "Urban") |>
  #     addLayersControl(
  #       overlayGroups = c("Urban"),
  #       options = layersControlOptions(
  #         collapsed = FALSE
  #       ))
# } else{
  leaflet::leaflet(
    data = dta,
    padding = 0,
    # trying to focus the map
    # default = 6.45
    options = leafletOptions(minZoom = 6.45)
  ) |>
    # leaflet::addProviderTiles("CartoDB") |> 
    leaflet::addPolygons(
      stroke = TRUE,
      fillColor = ~pal(dta[[var]]),
      weight = 0.5,
      opacity = 1,
      color = "white",
      dashArray = "1",
      fillOpacity = 1,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#00706e",
        dashArray = "",
        # fillColor = "#cce9e2",
        # fillOpacity = 0.1,
        bringToFront = TRUE),
      label = county_label,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal",
                     padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"))
  # }
  
  
  
    # leaflet::addLegend(
    #         pal = pal,
    #         values = ~dta[[var]],
    #         # labFormat = labelFormat(
    #         #   suffix = "Not informed",
    #         #   between = "\n"),
    #         opacity = 1,
    #         title = ~ifelse(y1 == y2, y1, paste(y1,"-",y2)),
    #         position = "bottomright",
    #         na.label = "Not informed"
    #       )
  
  # if(all(is.na(dta[[var]]))){
  #   map |> 
  #     leaflet::addLegend(
  #       pal = pal,
  #       values = ~dta[[var]],
  #       labFormat = labelFormat(
  #         suffix = "No information provided",
  #         between = "\n"),
  #       opacity = 1,
  #       title = ~ifelse(y1 == y2, y1, paste(y1,"-",y2)),
  #       position = "bottomright",
  #       na.label = "No information provided"
  #     )
  # } else{
  #   map |> 
  #   leaflet::addLegend(
  #     pal = pal,
  #     values = ~dta[[var]],
  #     # labFormat = labelFormat(
  #     #   suffix = '%', between = '%-',
  #     #   transform = function(x) 100 * x),
  #     opacity = 1,
  #     title = ~ifelse(y1 == y2,
  #                     paste0("Rate per 1,000 total births</br>","(",y1,")"),
  #                     paste0("Rate per 1,000 total births</br>","(",y1," - ", y2,")")
  #                     ),
  #     position = "bottomright",
  #     na.label = "No information provided"
  #   )
  # }
}
