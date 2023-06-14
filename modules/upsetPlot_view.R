# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("data.table")
import("plotly")
import("rintrojs")
import("shiny")
import("shinydashboard")
import("tidyverse")
# import("UpSetR")
import("utils")

# Define which objects from the module you make available to a user.
# All other objects are kept private, local, to the module.
modules::export("ui")
modules::export("init_server")

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
  
           box(
             title = "Data Sources",
             status = "primary",
             collapsible = FALSE,
             solidHeader = FALSE,
             width = 12,
             # This looks the same as your usual piece of code, 
             # except that the id is wrapped into 
             # the ns() function we defined before
             plotly::plotlyOutput(ns("geoupset"))
           )
}

init_server <- function(id, df1, y1, y2, q, lim){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # cd_selected_data <- session$userData$county_view$cd_selected
    
    # county_data is a reactive expression whose results will depend on
    # the periods (initial, final years), and condition selected
    
    geo_data <- reactive({
      dta <- buildGeoDataBySrce(df1 = df1,
                                y1 = y1(),
                                y2 = y2(),
                                q = q(),
                                geo = lim())
      ## Create a 0-1 matrix with all the possible combinations
      as.data.frame(tidyr::pivot_wider(dta,
                                       names_from = SrceIDs,
                                       values_from = vals,
                                       values_fill = 0))
    })
    
    getSelectedSetNames <- reactive({
      
      names(geo_data())[which(sapply(geo_data(), function(x){
        all(x %in% c(0, 1))
      }))]
      
    })
    
    getValidSets <- reactive({
      # withProgress(message = "Deriving input sets", value = 0, {
      
      logical_cols <-
        names(geo_data())[which(sapply(geo_data(), function(x){
          all(x %in% c(0, 1)) %in% TRUE
        }))]
      
      names(logical_cols) <- logical_cols
      
      lapply(logical_cols, function(x)
        which(geo_data()[[x]] == 1))
      # }
      # })
    })
    
    # Subset sets to those selected
    
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
    
    
    calculateIntersections <- reactive({
      selected_sets <- getSets()
      
      # withProgress(message = "Calculating set intersections", value = 0, {
      sets <- getSets()
      nsets <- length(sets)
      
      # Get all possible combinations of sets
      
      combinations <- function(items, pick) {
        x <- combn(items, pick)
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
        members_in_higher_levels <-
          unlist(intersects[(i + 1):length(intersects)])
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
    
    ## Dot-line intersection plot ------
    upsetGrid <- reactive({
      
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
      dta <- data.table::rbindlist(lapply(1:nintersections, function(combono) {
        data.frame(
          combo = combono,
          x = rep(combono, max(2, length(combos[[combono]]))),
          y = (nsets - combos[[combono]]) + 1,
          name = setnames[combos[[combono]]])
      }))[,`:=` (idx = .N), by = .(x, name)
      ][,`:=` (label = ifelse(idx %in% 1,
                              paste0(name, collapse = " \u2229 "),
                              name)), by = .(x)]
      
      # Create base plot
      plot <- plot_ly(
        data = dta,
        type = "scatter",
        mode = "markers",
        marker = list(color = "#E3E7E9", #light-gray
                      size = 5),
        source = "grid",
        customdata = ~label,
        unselected = list(marker = list(opacity = 0.5))
      ) %>% add_trace( ## grey dots background
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
        data = group_by(dta, combo),
        mode = "lines+markers",
        x = ~x,
        y = ~y - 0.5,
        line = list(color = "#008D8B", width = 3),#green
        marker = list(color = "#008D8B", #green
                      size = 5),
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
      
      if (is.null(tract_grd())){
        
        plot
        
      } else {
        # Filter data
        hoverdata <- setDT(dta)[combo %in% tract_grd()$x]
        
        # Update plot
        plot %>%
          add_trace( ## coloring orange when hovering
            type = "scatter",
            data = group_by(hoverdata, combo),
            mode = "lines+markers",
            x = ~x,
            y = ~y - 0.5,
            line = list(color = "#FFA500",#orange
                        width = 4),
            marker = list(color = "#CC8400",#orange
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
    
    # Tract hover ids
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
    
    # Make the bar chart illustrating set sizes ------
    
    upsetSetSizeBarChart <- reactive({
      
      selected_sets <- getSets()
      setnames <- names(selected_sets)
      
      dta <- data.table::setDT(
        data.frame(
          combo = 1:length(unlist(lapply(selected_sets, length))),
          size  = as.integer(unlist(lapply(selected_sets, length))),
          label = names(unlist(lapply(selected_sets, length))))
      )
      
      # Base plot
      plot <- plot_ly(
        data = dta,
        x = ~size,
        y = ~label,
        type = "bar",
        orientation = "h",
        marker = list(color = "#008D8B"),
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
        hoverdata <- setDT(dta)[
          label %in% unique(
            unlist(
              strsplit(
                tract_grd()$customdata, " \u2229 ", fixed = T)))
        ]
        
        # Update plot
        plot %>%
          add_trace( ## coloring orange when hovering
            type = "bar",
            data = hoverdata,
            x = ~size,
            y = ~label,
            orientation = "h",
            marker = list(color = "#FFA500",
                          line = list(color = "#CC8400",
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
    
    # Make the bar chart illustrating intersect set sizes ------
    
    upsetIntersectSizeBarChart <- reactive({
      
      ints <- calculateIntersections()
      intersects <- ints$intersections
      combos <- ints$combinations
      nintersections <- length(calculateIntersections()[["intersections"]])
      
      selected_sets <- getSets()
      setnames <- names(selected_sets)
      
      # Dataset
      dta <- unique(data.table::rbindlist(
        lapply(1:nintersections, function(combono) {
          data.frame(
            combo = combono,
            name = setnames[combos[[combono]]],
            size = intersects[combono])
        }))[,`:=` (idx = .N), by = .(combo, name)
        ][,`:=` (label = ifelse(idx %in% 1,
                                paste0(name, collapse = " \u2229 "),
                                name)), by = .(combo)
        ][,
          .(combo, label, size)
        ])
      
      # Base plot
      plot <- plot_ly(
        data = dta,
        showlegend = FALSE,
        source = "vertical",
        unselected = list(marker = list(opacity = 0.5)),
        customdata = ~label
      ) %>% 
        add_trace(
          x = ~combo,
          y = ~size,
          type = "bar",
          marker = list(color = "#008D8B"),
          hovertemplate = ~paste(
            "<b> Total reported cases </b>",
            "<br>",scales::comma(size,
                                 accuracy = 1),
            "<extra></extra>"
          )
        ) %>% layout(
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
        # Filter data
        hoverdata <- setDT(dta)[combo %in% tract_grd()$x]
        
        # Update plot
        plot %>% 
          add_trace( ## coloring orange when hovering
            type = "bar",
            data = hoverdata,
            x = ~combo,
            y = ~size,
            marker = list(color = "#FFA500",
                          line = list(color = "#CC8400",
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
    
    output$geoupset <- plotly::renderPlotly({
      
      validate(need(nrow(geo_data()) > 0,
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