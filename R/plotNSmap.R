nsmap <- function(df, var){
 import("leaflet")

 dta <- df
 var <- unlist(var)

 # color palette

 pal <- leaflet::colorBin(
  c("#CCE2E2", "#00706E", "#002423"), # RCP green colors - https://www.color-hex.com/
  domain = dta[[var]]
 )

 # Create label tag

 lab <- ifelse(
  is.na(dta[[var]]) | dta[[var]] %in% 0,
  paste(
   "<b>",stringr::str_to_title(dta[[which(!grepl("id|geometry|prev",tolower(names(dta))))]]),"</b>",
   "<br>No information provided"
  ),
  paste(
   "<b>",stringr::str_to_title(dta[[which(!grepl("id|geometry|prev",tolower(names(dta))))]]),"</b>",
   "<br>Prevalence:" , scales::comma(dta[[var]], accuracy = 0.01)
  )
  )|> lapply(htmltools::HTML)

 # Build map

 leaflet::leaflet(
  data = dta,
  padding = 0,
  options = leafletOptions(minZoom = 6)
 ) |>
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
    color = "#00706E",
    dashArray = "",
    bringToFront = TRUE),
   label = lab,
   labelOptions = labelOptions(
    style = list("font-weight" = "normal",
                 padding = "3px 8px"
    ),
    textsize = "15px",
    direction = "auto"))
}
