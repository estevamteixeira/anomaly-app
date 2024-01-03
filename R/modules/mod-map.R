# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("dplyr")
modules::import("DT")
modules::import("leaflet")
modules::import("mapview")
modules::import("sf")
modules::import("shiny")
modules::import("shinycssloaders")
modules::import("shinyjs")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
modules::export("mapUI")
modules::export("mapServer")


# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment.
modules::expose("R/data-select.R")
modules::expose("R/plotNSmap.R")

mapUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 layout_sidebar(
  shinyjs::useShinyjs(),
  sidebar = sidebar(
   ## ICD-10 option to select ----
   # Options are categorized in groups
   selectInput(
    inputId = ns("icd10"),
    label = "Condition",
    choices = NULL,
    selected = NULL
   ),
   ## Put year options side-by-side ----
   layout_column_wrap(
    width = 1/2,
    fill = FALSE,
    selectInput(
     inputId = ns("t0"),
     label = "Initial year",
     choices = NULL,
     selected = NULL
    ),
    selectInput(
     inputId = ns("tn"),
     label = "Final year",
     choices = NULL,
     selected = NULL
    )
   ),
   selectInput(
    inputId = ns("geo"),
    label = "Geographic unit",
    choices = NULL,
    selected = NULL
   ),
   ## conditionally show or hide UI elements based on a JavaScript expression
    ## checks if the geo_selected_data input exists and is not undefined
    ## If both conditions are true, the UI elements will be displayed
    conditionalPanel("typeof input.geotable_rows_selected() !== 'undefined' ||
                     input.geotable_rows_selected.length() > 0",
                     uiOutput(ns("controls"))
    )
  ),
  layout_columns(
   col_widths = c(7,5),
   card(
   full_screen = TRUE,
   card_header(
    tooltip(
     span("Surveillance Map",
          bsicons::bs_icon("question-circle")
     ),
     "Geographical links were determined based on",strong("mothers'"),
     "postal codes at the time of admission for delivery."
    ),
    tooltip(
     downloadButton(ns("map_down"),
                  label = "",
                  inline = TRUE,
                  icon = shiny::icon("camera")
                  ),
     "Download map as png"
     ),
    class = "d-flex justify-content-between align-items-center"
    ),
   card_body(
    leafletOutput(ns("geomap")),
   )
  ),
  card(
   full_screen = TRUE,
   card_header(
    "Surveillance Table"
   ),
   card_body(
    DT::dataTableOutput(ns("geotable")),
   )
  ))
  )
}

mapServer <- function(id, df1, df2, df3, df4, df5, df6, df7){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

  # Using a "server-side selectize" option that massively improves
  # performance and efficiency (for large numbers of choices)
  updateSelectInput(
   session,
   inputId = "icd10",
   choices = df1,
   selected = c("Q999")
  )

  ## Update the options for the initial year selectInput
  ## The values should reflect the data availability
  observeEvent(input$icd10,{

   updateSelectInput(
    session,
    inputId = "t0",
    choices = sort(
     unique(
      df2 %>%
       filter(Diag %in% input$icd10) %>%
       select(Birth_Year) %>%
       collect() %>%
       pull()
     )),
    selected = c(
     min(
      df2 %>%
       filter(Diag %in% input$icd10) %>%
       select(Birth_Year) %>%
       collect() %>%
       pull(), na.rm = TRUE
     )
    )
   )
  })

  ## Make the final year option greater than or equal the
  ## initial year option
  observeEvent(c(input$icd10, input$t0),{

   updateSelectInput(
    session,
    inputId = "tn",
    choices = c(
     sort(
      unique(
       df2 %>%
        filter(Diag %in% input$icd10) %>%
        select(Birth_Year) %>%
        collect() %>%
        pull()
      )))[
       c(
        sort(
         unique(
          df2 %>%
           filter(Diag %in% input$icd10) %>%
           select(Birth_Year) %>%
           collect() %>%
           pull()
         )
        )
       ) >= input$t0],
    selected = c(
     max(
      df2 %>%
       filter(Diag %in% input$icd10) %>%
       select(Birth_Year) %>%
       collect() %>%
       pull(), na.rm = TRUE
     )
    )
   )
  })

  # Using a "server-side selectize" option that massively improves
  # performance and efficiency (for large numbers of choices)
  updateSelectInput(
   session,
   inputId = "geo",
   choices = df3,
   selected = c("cd")
  )

  # Select the shape file depending on the selected geographic unit ----
  shape <- reactive({
   if(input$geo %in% "cd")  return(df4)
   if(input$geo %in% "cl")  return(df5)
   if(input$geo %in% "chn") return(df6)
   if(input$geo %in% "hr")  return(df7)
  })

  # Select field depending on the selected geographic unit ----
  fields <- reactive({
   if(input$geo %in% "cd")  return(c("CDuid","count_brth_cd"))
   if(input$geo %in% "cl")  return(c("CLuid","count_brth_cl"))
   if(input$geo %in% "chn") return(c("CHNuid","count_brth_chn"))
   if(input$geo %in% "hr")  return(c("HRuid","count_brth_hr"))
  })

  # `Anom` is a reactive expression whose results will depend on ----
  # the t0, tn, condition, and geo
  anom <- reactive({
   return(
    getSubsetData(df2, c("CaseID","Birth_Year", "Diag", fields())) %>%
     filter(Birth_Year >= as.numeric(input$t0),
            Birth_Year <= as.numeric(input$tn),
            Diag %in% input$icd10) %>%
     distinct() %>%
     calcPrev(colsToSelect = fields()) %>%
     collect()
   )
  })

  # Create `geo` data: merge `Anom()` with the shape file to draw the map ----

  geodta <- reactive({
   merge(
    shape() ,
    anom(),
    by.x = names(shape())[which(grepl("id",tolower(names(shape()))))],
    by.y = names(anom())[which(grepl("id",tolower(names(anom()))))],
    all.x = TRUE
   ) %>%
    arrange(desc(prev))
  })

  map <- reactiveValues(dat = NULL, mapView = NULL)


  # Map output ----
  output$geomap <- renderLeaflet({
   req(geodta())
   validate(need(nrow(geodta()) > 0,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))

   map$dat <- nsmap(geodta(), "prev")
   })

  # Map download ----

  output$map_down <- downloadHandler(

   filename = "map.png",

   content = function(file){
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))

    if(!is.null(map$mapView)){

     withProgress(message = "Creating file", value = 0,{

      Sys.sleep(0.25)
      incProgress(1/10)

     dta <- geodta() %>% filter(GeoUID %in% map$mapView$GeoUID)

     Sys.sleep(0.25)
     incProgress(3/10)

     # color palette
     pal <- leaflet::colorBin(
      c("#CCE2E2", "#00706E", "#002423"), # RCP green colors - https://www.color-hex.com/
      domain = geodta()[["prev"]]
     )

     Sys.sleep(0.25)
     incProgress(5/10)

     # Popup label
     lab <- ifelse(
      is.na(dta[["prev"]]) | dta[["prev"]] %in% 0,
      paste(
       "<b>",stringr::str_to_title(dta[[which(!grepl("id|geometry|prev",tolower(names(dta))))]]),"</b>",
       "<br>No information provided"
      ),
      paste(
       "<b>",stringr::str_to_title(dta[[which(!grepl("id|geometry|prev",tolower(names(dta))))]]),"</b>",
       "<br>Prevalence:" , scales::comma(dta[["prev"]], accuracy = 0.01)
      )
     ) |> lapply(htmltools::HTML)

     Sys.sleep(0.25)
     incProgress(7/10)

     mapview::mapshot(
      nsmap(geodta(), "prev") |>
       leaflet::addLegend(
                pal = pal,
                values = ~dta[["prev"]],
                opacity = 1,
                title = "",
                position = "bottomright",
                na.label = "Not informed"
              ) |>
       leaflet::setView(
        lat = mean(sf::st_bbox(dta)[c(2,4)]),
        lng = mean(sf::st_bbox(dta)[c(1,3)]),
        zoom = 9) |>
       leaflet::addPolygons(data = dta,
                            color = "#FF0000",
                            opacity = 1,
                            fill = FALSE,
                            weight = 3,
                            layerId = "bounds") |>
       leaflet::addPopups(
        lat = mean(sf::st_bbox(dta)[c(2,4)]),
        lng = mean(sf::st_bbox(dta)[c(1,3)]),
        popup = lab,
        options = popupOptions(closeButton = FALSE)),
      file = file)

     Sys.sleep(0.25)
     incProgress(10/10)

    })
    } else{
     withProgress(message = "Creating file, please wait.", value = 0,{
      Sys.sleep(0.25)
      incProgress(1/10)

      # color palette
      pal <- leaflet::colorBin(
       c("#CCE2E2", "#00706E", "#002423"), # RCP green colors - https://www.color-hex.com/
       domain = geodta()[["prev"]]
      )

      Sys.sleep(0.25)
      incProgress(5/10)

     mapview::mapshot(
      map$dat |>
       leaflet::addLegend(
        pal = pal,
        values = ~geodta()[["prev"]],
        opacity = 1,
        title = "",
        position = "bottomright",
        na.label = "Not informed"
       ),
      file = file)

     Sys.sleep(0.25)
     incProgress(10/10)
   }) }
    }
  )

    # This allows to add footer with totals
  # Here we need to make use of the isolate() function
  # Othw, this object should be put inside the DT::renderDT() call

  sketch <- htmltools::withTags(table(
   tableHeader(c("GeoUID", "Name", "Prevalence"),
               escape = FALSE)
  ))

  # DataTable object
  output$geotable <- DT::renderDT({
   req(geodta())
   validate(need(nrow(geodta()) > 0,
                 "Sorry, there is no data available for the selected options.
            \nPlease, choose different years and/or conditions."))

   DT::datatable(
    geodta() %>% as_tibble() %>% select(-geometry) %>% arrange(desc(prev)),
    container = sketch,
    rownames = FALSE,
    style = "auto",
    selection = 'single',
    extensions = "Buttons",
    caption = "Prevalence is expressed as cases per 10,000 total births.",
    options = list(
     dom = 'B<t>ftp',
     extensions = "Buttons",
     search = list(regex = TRUE, caseInsensitive = TRUE),
     paging = TRUE,
     pageLength = 5,
     ordering = TRUE,
     stateSave = TRUE,
     columnDefs = list(list(visible = FALSE,
                            targets = c(0)))
    )
   ) |>
    DT::formatRound(
     columns = c("prev"),
     digits = 2,
     mark = ","
    )
  }, server = FALSE)

  # DataTable proxy object ----
  DTproxy <- DT::dataTableProxy("geotable")

 observeEvent(input$geotable_rows_selected,{

  l <- input$geotable_rows_selected

  # Show the controls when a row is selected
  shinyjs::show("controls")

  # Update map with selected line
  leaflet::leafletProxy("geomap", session, data = geodta()[l,]) |>
    leaflet::removeShape("bounds") |>
    leaflet::setView(
     lat = mean(sf::st_bbox(geodta()[l,])[c(2,4)]),
     lng = mean(sf::st_bbox(geodta()[l,])[c(1,3)]),
     zoom = 8) |>
    leaflet::addPolygons(data = geodta()[l,],
                         color = "#FF0000",
                         opacity = 1,
                         fill = FALSE,
                         weight = 3,
                         layerId = "bounds")

  # Update reactiveValues with current map view
  map$mapView <- leaflet::getMapData(
   leaflet::leafletProxy("geomap", session, data = geodta()[l,])
   )
 })

  ## Reset button ----
  output$controls <- renderUI({
   req(input$geotable_rows_selected)
   absolutePanel(id = "controls",
                 actionButton(inputId = ns("reset"),
                              label = "Reset map"))

  })

  ## Resetting map to original value when reset button is clicked ----
  observeEvent(input$reset, {

   # Hiding the control button
   shinyjs::hide("controls")

   # plotting original map
   leaflet::leafletProxy("geomap", data = geodta()) |>
    leaflet::removeShape("bounds") |>
    leaflet::setView(
     lat = mean(sf::st_bbox(geodta())[c(2,4)]),
     lng = mean(sf::st_bbox(geodta())[c(1,3)]),
     zoom = 6.45)

   # clear row selection ----
   selectRows(DTproxy, selected = NULL)
   # go to page 1
   selectPage(DTproxy, 1)

   # Reset mapView
   map$mapView = NULL
  })

 observeEvent(input$geomap_shape_click,{
  sc <- input$geomap_shape_click$id

  selectRows(DTproxy, selected = which(geodta()[["GeoUID"]] %in% sc))
  selectPage(DTproxy, ceiling(which(geodta()[["GeoUID"]] %in% sc) / input$geotable_state[["length"]]))
 })

 })
}
