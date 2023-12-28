# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("data.table")
modules::import("dplyr")
modules::import("plotly")
modules::import("shiny")
modules::import("stringr")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
modules::export("summUI")
modules::export("summServer")


# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment.
modules::expose("R/data-select.R")


#' Title
#'
#' @param id
#'
#' @return Module for showing the app summary: value boxes and sources x-ray.
#' @export
#'
#' @examples
#'
summUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 layout_sidebar(
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
   )
  ),
  layout_column_wrap(
   width = 1/3,
   fill = FALSE,
   ## Box 1 UI ----
   value_box(
    title = "Total births",
    value = textOutput(ns("brth")),
    showcase = plotlyOutput(ns("brthplot")),
    full_screen = TRUE
   ),
   ## Box 2 UI ----
   as_fill_carrier(uiOutput(ns("case"))),
   ## Box 3 UI ----
   value_box(
    title = "Prevalence",
    p("cases per 10,000 total births"),
    value = textOutput(ns("prev")),
    showcase = fontawesome::fa("people-group", width = "3em")
   )
  ),
  card(
   full_screen = TRUE,
   card_header(
    tooltip(
     span("Data source",
          bsicons::bs_icon("question-circle")
     ),
     "For more information about this plot",
     a(style='color: #FFFFFF', "(click here)",
       href = "https://upset.app/",
       target = "_blank")# open new tab when clicked
    )),
   card_body(
    plotlyOutput(ns("srceplot"))
   )
   # card_footer("For more information about this chart",
   #             a("(click here)", href = "https://upset.app/"))
  ))
}

summServer <- function(id, df1, df2, df3){
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
      df3 %>%
       filter(Diag %in% input$icd10) %>%
       select(Birth_Year) %>%
       collect() %>%
       pull()
     )),
    selected = c(
     min(
      df3 %>%
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
       df3 %>%
        filter(Diag %in% input$icd10) %>%
        select(Birth_Year) %>%
        collect() %>%
        pull()
      )))[
       c(
        sort(
         unique(
          df3 %>%
           filter(Diag %in% input$icd10) %>%
           select(Birth_Year) %>%
           collect() %>%
           pull()
         )
        )
       ) >= input$t0],
    selected = c(
     max(
      df3 %>%
       filter(Diag %in% input$icd10) %>%
       select(Birth_Year) %>%
       collect() %>%
       pull(), na.rm = TRUE
     )
    )
   )
  })

  # `Birth` is a reactive expression whose results will depend on ----
  # the t0, tn
  birth <- reactive({
   return(
    df2 %>%
     filter(BrthYear >= as.numeric(input$t0),
            BrthYear <= as.numeric(input$tn)) %>%
     group_by(BrthYear) %>%
     mutate(total = sum(count_brth)) %>%
     select(BrthYear, total) %>%
     distinct() %>%
     ungroup() %>%
     collect()
   )
  }) %>%
   bindCache(input$t0, input$tn)

  # `Anom` is a reactive expression whose results will depend on ----
  # the t0, tn, condition
  anom <- reactive({
   return(
    getSubsetData(df3, c("CaseID","Birth_Year", "Diag")) %>%
     filter(Birth_Year >= as.numeric(input$t0),
            Birth_Year <= as.numeric(input$tn),
            Diag %in% input$icd10) %>%
     distinct() %>%
     group_by(Birth_Year) %>%
     mutate(count_ano = n()) %>%
     ungroup() %>%
     select(Birth_Year, count_ano) %>%
     distinct() %>%
     collect()
   )
  }) %>%
   bindCache(input$t0, input$tn, input$icd10)

  # Text output - box 1 ----
  output$brth <- renderText({
   birth() %>% pull() %>% sum(na.rm = TRUE) %>% scales::comma(accuracy = 1)
  })

  # Plot output - box 1 ----
  output$brthplot <- renderPlotly({
   plotly_time_series(birth(), x = ~BrthYear, y = ~total)
  })

  ## Function to plot time series in plotly ----

  plotly_time_series <- function(d, x, y) {
   info <- getCurrentOutputInfo()
   large <- isTRUE(info$height() > 200)

   plot_ly(d, x = x, y = y, hovertemplate = "%{y:,.0f}<extra></extra>") %>%
    add_lines(
     color = I(info$fg()),
     span = I(1),
     #hoverinfo = if (!large) "none",
     fill = 'tozeroy',
     alpha = 0.2
    ) %>%
    layout(
     hovermode = "x",
     margin = list(t = 0, r = 0, l = 0, b = 0),
     font = list(color = info$fg()),
     paper_bgcolor = "transparent",
     plot_bgcolor = "transparent",
     xaxis = list(
      title = "",
      visible = large,
      showgrid = FALSE
     ),
     yaxis = list(
      title = "",
      visible = large,
      showgrid = FALSE
     )
    ) %>%
    plotly::style(hoverlabel = list(
     bgcolor  = "black",
     bordercolor = "transparent",
     font = list(
      color = "white",
      size = 14,
      face = "bold"
     )
    )) %>%
    plotly::config(displaylogo = FALSE,
                   displayModeBar = FALSE,
                   modeBarButtonsToRemove = c(
                    "select2d",
                    "zoomIn2D",
                    "zoomOut2d",
                    "zoom2d",
                    "pan2d",
                    "lasso2d",
                    "autoScale2d",
                    "resetScale2d",
                    "hoverClosestCartesian",
                    "hoverCompareCartesian"
                   ))
  }

  # Text output - box 2 ----

  output$case <- renderUI({
   if(input$icd10 %in% "Q999"){
    return(
     value_box(
      title = "Reported congenital anomalies",
      value = anom() %>% pull() %>% sum(na.rm = TRUE) %>% scales::comma(accuracy = 1),
      showcase = renderPlotly({
       plotly_time_series(anom(), x = ~Birth_Year, y = ~count_ano)
      }),
      full_screen = TRUE
     )
    )
   }
   return(
    value_box(
     title = "Reported congenital anomalies",
     value = ifelse(anom() %>% pull() %>% sum(na.rm = TRUE) < 5,
                    "< 5",
                    anom() %>% pull() %>% sum(na.rm = TRUE) %>% scales::comma(accuracy = 1)
     ),
     showcase = fontawesome::fa("dna", width = "3em")
    )
   )
  })

  # Text output - box 3 ----

  observeEvent(anom() %>% pull() %>% sum(na.rm = TRUE),{
   output$prev <- renderText(
    if (anom() %>% pull() %>% sum(na.rm = TRUE) < 5){
     return(
      "Supressed"
     )
    } else {
     return(
      scales::comma(
       10000*(anom() %>% pull() %>% sum(na.rm = TRUE)/birth() %>% pull() %>% sum(na.rm = TRUE)),
       accuracy = 0.01)
     )
    }
   )
  })

  # `Upset` is a reactive expression whose results will depend on ----
  # the t0, tn, condition
  # source: https://github.com/pinin4fjords/upset-shiny-plotly/blob/master/app.R

  upset <- reactive({
   temp <- getSubsetData(df3, c("CaseID","Birth_Year","Diag","SrceIDs")) %>%
    filter(Birth_Year >= as.numeric(input$t0),
           Birth_Year <= as.numeric(input$tn),
           Diag %in% input$icd10) %>%
    select("CaseID","SrceIDs") %>%
    mutate(vals = 1,
           SrceIDs = factor(
            case_when(
             SrceIDs %in% "1" ~ "FADB",
             SrceIDs %in% "2" ~ "NSAPD",
             SrceIDs %in% "3" ~ "Cardio",
             SrceIDs %in% "4" ~ "CIHI",
             SrceIDs %in% "5" ~ "MSI",
             SrceIDs %in% "6" ~ "NeoNatal",
             SrceIDs %in% "7" ~ "Others"))
    ) %>%
    distinct() %>%
    collect()

   ## Create a 0-1 matrix with all the possible combinations
   return(
    tidyr::pivot_wider(temp,
                       names_from = SrceIDs,
                       values_from = vals,
                       values_fill = 0
    )
   )
  })

  # UpsetPlot construction ----

  ## Get source names ----
  getSelectedSetNames <- reactive({

   names(upset())[which(sapply(upset(), function(x){
    all(x %in% c(0, 1))
   }))]

  })

  getValidSets <- reactive({
   # withProgress(message = "Deriving input sets", value = 0, {

   logical_cols <- names(upset())[which(sapply(upset(), function(x){
    all(x %in% c(0, 1)) %in% TRUE
   }))]

   names(logical_cols) <- logical_cols
   lapply(logical_cols, function(x)
    which(upset()[[x]] == 1))
   # }
   # })
  })

  ## Subset sets to those selected ----

  getSelectedSets <- reactive({

   valid_sets <- getValidSets()
   validate(need(!is.null(valid_sets), "Please upload data"))

   chosen_sets <- getSelectedSetNames()
   sets <- valid_sets[chosen_sets]
   sets <- sets[order(unlist(lapply(sets, length)))]

  })

  getSets <- reactive({
   selected_sets <- getValidSets()[
    order(
     unlist(
      lapply(
       getValidSets(), length)))]

   req(length(selected_sets) > 0)

   nsets <- length(selected_sets)
   selected_sets[1:min(nsets, length(selected_sets))]
  })

  ## Compute intersection ----
  ## Retuns a list of all possible combinations of sets

  calculateIntersections <- reactive({
   selected_sets <- getSets()

   # withProgress(message = "Calculating set intersections", value = 0, {
   sets <- getSets()
   nsets <- length(sets)

   # Get all possible combinations of sets

   combinations <- function(items, pick) {
    x <- utils::combn(items, pick)
    lapply(seq_len(ncol(x)), function(i)
     x[, i])
   }

   startsize <- 1

   combos <- lapply(startsize:nsets, function(x) {
    combinations(1:length(selected_sets), x)
   })

   # Calculate the intersections of all these combinations

   # withProgress(message = "Running intersect()", value = 0, {

   intersects <- lapply(combos, function(combonos) {
    lapply(combonos, function(combo) {
     Reduce(intersect, selected_sets[combo])
    })
   })
   # })

   # For UpSet-ness, membership of higher-order intersections takes
   # priority. Otherwise just return the number of entries in each
   # intersection

   intersects <- lapply(1:length(intersects), function(i) {
    intersectno <- intersects[[i]]
    members_in_higher_levels <- unlist(intersects[(i + 1):length(intersects)])
    lapply(intersectno, function(intersect) {
     length(setdiff(intersect, members_in_higher_levels))
    })
   })

   combos <- unlist(combos, recursive = FALSE)
   intersects <- unlist(intersects)

   combos <- combos[which(intersects > 0)]
   intersects <- intersects[which(intersects > 0)]

   # Sort by intersect size

   combos <- combos[order(intersects, decreasing = TRUE)]
   intersects <-
    intersects[order(intersects, decreasing = TRUE)]

   list(combinations = combos, intersections = intersects)

   # })
  })

  ## Dot-line intersection plot ----

  upsetGrid <- reactive({

   info <- getCurrentOutputInfo()
   selected_sets <- getSets()
   ints <- calculateIntersections()

   intersects <- ints$intersections
   combos <- ints$combinations

   # Reduce the maximum number of intersections if we don't
   # have that many

   nintersections <- length(calculateIntersections()[["intersections"]])
   nintersections <- min(nintersections, length(combos))

   # Fetch the number of sets

   nsets <- length(selected_sets)
   setnames <- names(selected_sets)

   # Create dataset
   ddta <- data.table::rbindlist(lapply(1:nintersections, function(combono) {
    data.table::data.table(
     combo = combono,
     x = rep(combono, max(2, length(combos[[combono]]))),
     y = (nsets - combos[[combono]]) + 1,
     name = setnames[combos[[combono]]])
   }))[,`:=` (idx = .N), by = .(x, name)
   ][,`:=` (label = ifelse(idx %in% 1,
                           paste0(name, collapse = " \u2229 "),
                           name)), by = .(x)]

   ### Create base plot ----
   plot <- plot_ly(
    data = ddta,
    type = "scatter",
    mode = "markers",
    marker = list(color = "#E3E7E9", #light-gray
                  size = 5),
    source = "grid",
    customdata = ~label,
    unselected = list(marker = list(opacity = 0.5))
   ) %>%
    add_trace( ### grey dots background ----
               type = "scatter",
               x = rep(1:nintersections,
                       length(selected_sets)),
               y = unlist(lapply(1:length(selected_sets), function(x)
                rep(x - 0.5, nintersections))),
               hoverinfo = "none",
               marker = list(color = "#E3E7E9", #light-gray
                             size = 5),
               customdata= ~rep(unique(label),length(selected_sets))
    ) %>% add_trace( ## green dots - sets
     type = "scatter",
     data = group_by(ddta, combo),
     mode = "lines+markers",
     x = ~x,
     y = ~y - 0.5,
     line = list(width = 3, color = info$fg(), opacity = 0.2),
     marker = list(size = 5, color = info$fg()),
     hoverinfo = "text",
     text = ~label
    ) %>% layout(
     xaxis = list(
      title = "",
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE
     ),
     yaxis = list(
      title = "",
      showticklabels = FALSE,
      showgrid = TRUE,
      range = c(0, nsets),
      zeroline = FALSE,
      range = 1:nsets
     ),
     margin = list(t = 0, b = 0)
    ) %>%
    plotly::style(hoverlabel = list(
     bgcolor  = "black",
     bordercolor = "transparent",
     font = list(
      color = "white",
      size = 14,
      face = "bold"
     )
    )) %>%
    event_register("plotly_click")

   ## Work on the logic to update the color of
   ## hovered dots

   if(is.null(tract_grd())){
    plot
   } else{

    if(is.numeric(tract_grd()$y)){
     # Filter data
     dclick <- ddta %>% filter(combo %in% tract_grd()$x)
    } else{
     dclick <- ddta %>% filter(name %in% tract_grd()$y)
    }

    # Update plot
    plot %>%
     add_trace( ## coloring orange when hovering
      type = "scatter",
      data = group_by(dclick, combo),
      mode = "lines+markers",
      x = ~x,
      y = ~y - 0.5,
      line = list(color = "#FFA500",# orange
                  width = 4),
      marker = list(color = "#CC8400",# dark-orange
                    size = 6),
      hoverinfo = "text",
      text = ~label
     ) %>%
     plotly::style(hoverlabel = list(
      bgcolor  = "black",
      bordercolor = "transparent",
      font = list(
       color = "white",
       size = 14,
       face = "bold"
      )
     ))
   }
  })

  ## Track hover ids ----
  tract_grd <- reactive({
   eventdata <- event_data("plotly_click", source = "grid")
   eventerase <- event_data("plotly_doubleclick", source = "grid")

   if (is.null(eventdata) &
       !is.null(eventerase)){
    return(NULL)
   } else {
    return(eventdata)
   }
  })

  # Horizontal bar chart illustrating set sizes ----

  upsetSetSizeBarChart <- reactive({

   info <- getCurrentOutputInfo()
   selected_sets <- getSets()
   setnames <- names(selected_sets)

   hdta <- data.table::setDT(
    data.table::data.table(
     combo = 1:length(unlist(lapply(selected_sets, length))),
     size  = as.integer(unlist(lapply(selected_sets, length))),
     label = names(unlist(lapply(selected_sets, length))))
   )

   # Base plot
   plot <- plot_ly(
    data = hdta,
    x = ~size,
    y = ~label,
    type = "bar",
    orientation = "h",
    alpha = 0.8,
    line = list(color = info$fg()),
    marker = list(color = info$fg()),
    hovertemplate = ~paste(
     "<b>", label, "</b>",
     "<br> Total reported cases:",
     scales::comma(size,
                   accuracy = 1),
     "<extra></extra>"
    ),
    source = "horizontal",
    customdata = ~label
   ) %>% layout(
    bargap = 0.4,
    yaxis = list(
     title = "",
     categoryarray = ~rev(label),
     categoryorder = "array",
     face = "bold",
     size = 14
     # side = "right"
    ),
    xaxis = list(
     # autorange = "reversed",
     face = "bold",
     size = 14,
     title = "Set Size"
    )
   ) %>%
    plotly::style(hoverlabel = list(
     bgcolor  = "black",
     bordercolor = "transparent",
     font = list(
      color = "white",
      size = 14,
      face = "bold"
     )
    )) %>%
    event_register("plotly_click")

   ## Work on the logic to update the color of
   ## hovered horizontal bars

   if (is.null(tract_grd())){
    plot
   } else {
    # Filter data
    hclick <- hdta %>%
     filter(
      label %in% unique(
       unlist(
        strsplit(
         tract_grd()$customdata, " \u2229 ", fixed = T)))
     )

    # Update plot
    plot %>%
     add_trace( ## coloring orange when hovering
      type = "bar",
      data = hclick,
      x = ~size,
      y = ~label,
      orientation = "h",
      marker = list(color = "#FFA500", # orange
                    line = list(color = "#CC8400", # dark-orange
                                width = 2)),
      hovertemplate = ~paste(
       "<b>", label, "</b>",
       "<br> Total reported cases:",
       scales::comma(size,
                     accuracy = 1),
       "<extra></extra>"
      )
     ) %>%
     layout(barmode = 'overlay') %>%
     plotly::style(hoverlabel = list(
      bgcolor  = "black",
      bordercolor = "transparent",
      font = list(
       color = "white",
       size = 14,
       face = "bold"
      )
     ))
   }
  })

  # Vertical bar chart illustrating intersect set sizes ----

  upsetIntersectSizeBarChart <- reactive({

   info <- getCurrentOutputInfo()
   ints <- calculateIntersections()
   intersects <- ints$intersections
   combos <- ints$combinations
   nintersections <- length(calculateIntersections()[["intersections"]])

   selected_sets <- getSets()
   setnames <- names(selected_sets)

   # Dataset
   vdta <- unique(data.table::rbindlist(
    lapply(1:nintersections, function(combono) {
     tibble(
      combo = combono,
      name = setnames[combos[[combono]]],
      size = intersects[combono])
    }))[,`:=` (idx = .N), by = .(combo, name)
    ][,`:=` (label = ifelse(idx %in% 1,
                            paste0(name, collapse = " \u2229 "),
                            name)), by = .(combo)
    ][,
      .(combo, name, label, size)
    ])

   # Base plot
   plot <- plot_ly(
    data = vdta,
    showlegend = FALSE,
    source = "vertical",
    unselected = list(marker = list(opacity = 0.5)),
    customdata = ~label
   ) %>%
    add_trace(
     x = ~combo,
     y = ~size,
     type = "bar",
     marker = list(color = info$fg()),
     hovertemplate = ~paste(
      "<b> Total reported cases </b>",
      "<br>",scales::comma(size,
                           accuracy = 1),
      "<extra></extra>"
     )
    ) %>% layout(
     barmode = "overlay",
     yaxis = list(
      face = "bold",
      size = 14,
      title = "Intersection Size"
     ),
     xaxis = list(
      title = "",
      face = "bold",
      size = 14
     )
    ) %>%
    plotly::style(hoverlabel = list(
     bgcolor  = "black",
     bordercolor = "transparent",
     font = list(
      color = "white",
      size = 14,
      face = "bold"
     )
    )) %>%
    event_register("plotly_click")

   ## Work on the logic to update the color of
   ## hovered vertical bars
   if (is.null(tract_grd())){
    plot
   } else {
    if(is.numeric(tract_grd()$y)){
     # Filter data
     vclick <- vdta %>% filter(combo %in% tract_grd()$x)
    } else{
     vclick <- vdta %>% filter(name %in% tract_grd()$y)
    }

    # Update plot
    plot %>%
     add_trace( ## coloring orange when hovering
      type = "bar",
      data = vclick,
      x = ~combo,
      y = ~size,
      marker = list(color = "#FFA500", # orange
                    line = list(color = "#CC8400", # dark-orange
                                width = 2)),
      hovertemplate = ~paste(
       "<b> Total reported cases </b>",
       "<br>",scales::comma(size,
                            accuracy = 1),
       "<extra></extra>"
      )
     ) %>%
     layout(barmode = 'overlay') %>%
     plotly::style(hoverlabel = list(
      bgcolor  = "black",
      bordercolor = "transparent",
      font = list(
       color = "white",
       size = 14,
       face = "bold"
      )
     ))
   }
  })

  # Plot output - UpsetPlot ----
  output$srceplot <- renderPlotly({
   validate(need(nrow(upset()) > 3,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))

   ## Putting multiple plots in a single view

   grid <- upsetGrid()
   set_size_chart <- upsetSetSizeBarChart()
   intersect_size_chart <- upsetIntersectSizeBarChart()

   s1 <- subplot(
    plotly_empty(type = "scatter", mode = "markers"),
    plotly_empty(type = "scatter", mode = "markers"),
    plotly_empty(type = "scatter", mode = "markers"),
    set_size_chart,
    nrows = 2,
    widths = c(0.3, 0.7),
    titleX = TRUE,
    margin = 0
   ) %>%
    layout(showlegend = FALSE)

   s2 <- subplot(
    intersect_size_chart,
    grid,
    nrows = 2,
    shareX = TRUE,
    titleY = TRUE,
    margin = 0) %>%
    layout(showlegend = FALSE)

   subplot(
    s1,
    s2,
    widths = c(0.3, 0.7),
    titleX = TRUE,
    titleY = TRUE,
    margin = 0
   ) %>%
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c(
                    "select2d",
                    "zoomIn2D",
                    "zoomOut2d",
                    "zoom2d",
                    "pan2d",
                    "lasso2d",
                    "autoScale2d",
                    "resetScale2d",
                    "hoverClosestCartesian",
                    "hoverCompareCartesian"
                   ))
  })
 })
}
