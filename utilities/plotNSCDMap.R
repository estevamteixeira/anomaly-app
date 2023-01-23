plot_map <- function(data, var, y1, y2){
  # Use 'import' from the 'modules' package.
  # These listed imports are made available inside the module scope.
  import("leaflet")
  
  dta <- data
  var <- unlist(var)
  #intv <- pretty(dta[[var]])
  
  pal <- colorBin(c("#cce2e2", "#00706e", "#002423"),
                  domain = dta[[var]]
                  #bins = intv
  )
  
  county_label <- ifelse(is.na(dta[[var]]) | dta[[var]] == 0,
                 paste(
                   "<b>",stringr::str_to_title(dta[["name"]]),"</b>",
                   # "<br>Census ID:", dta[["GeoUID"]],
                   "<br>No information provided"
                   ),
                 paste(
                   "<b>",stringr::str_to_title(dta[["name"]]),"</b>",
                   # "<br>Census ID:", dta[["GeoUID"]],
                   "<br>Reported occurences:", dta[[which(grepl("total_cases",names(dta)))]],
                   "<br>Total births:", dta[[which(grepl("total_lvb",names(dta)))]],
                   "<br>Prevalence (*cases per 1,000 total births):" , scales::comma(dta[[var]], accuracy = 0.1)
                 )
  ) |> lapply(htmltools::HTML)
  
  
  map <- leaflet::leaflet(dta, padding = 0,
              # trying to focus the map
              # default = 7
                          options = leafletOptions(minZoom = 6.45)) |>
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
