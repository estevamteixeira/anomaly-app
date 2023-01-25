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
consts <- use("constants/constants.R")

# It is a variation of 'use' where instead of returning a module 
# as return value, the elements are copied to the calling environment.
expose("utilities/getDataByTimeRange.R")

ui <- function(id){
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  
  introBox(data.step = 9, data.intro = consts$intro$text[9],
  box(
    title = "Data Sources",
    status = "primary",
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 12,
    # This looks the same as your usual piece of code, 
    # except that the id is wrapped into 
    # the ns() function we defined before
    plotly::plotlyOutput(ns("dataupset"))
    )
  )
}

init_server <- function(id, df, y1, y2, q){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # cd_selected_data <- session$userData$county_view$cd_selected
    
    # county_data is a reactive expression whose results will depend on
    # the periods (initial, final years), and condition selected
    
    srce_data <- reactive({
      if(is.null(q()) || q() %in% "0"){
       dta <- unique(
          getSubsetByTimeRange(df,
                              y1(),
                              y2())[, .(CaseID, BrthYear, 
                                        Diags, SrceIDs)
                              ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      } else if (!q() == "0" &&
                 is.na(stringr::str_extract(q(), pattern = "\\(.*\\)"))){
        dta <-  unique(
          getSubsetByTimeRange(df,
                               y1(),
                               y2(),
                               q())[, .(CaseID, BrthYear,
                                        cat_tier3, SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      }else{
      dta <-  unique(
          getSubsetByTimeRange(df,
                               y1(),
                               y2(),
                               q())[, .(CaseID, BrthYear,
                                        cat_tier4, SrceIDs)
                               ]
        )[,`:=` (vals = 1,
                 SrceIDs = factor(
                   fcase(
                     SrceIDs %in% "1", "FADB",
                     SrceIDs %in% "2", "NSAPD",
                     SrceIDs %in% "3", "Cardio",
                     SrceIDs %in% "4", "CIHI",
                     SrceIDs %in% "5", "MSI",
                     SrceIDs %in% "6", "NeoNatal",
                     SrceIDs %in% "7", "Others")))]
      }
      
      as.data.frame(tidyr::pivot_wider(dta,
                                       names_from = SrceIDs,
                                       values_from = vals,
                                       values_fill = 0))
    })
    
    getSelectedSetNames <- reactive({
      names(srce_data())[which(sapply(srce_data(), function(x){
        all(x %in% c(0, 1))
      }))]
      
    })
    
    getValidSets <- reactive({
      # withProgress(message = "Deriving input sets", value = 0, {
        
        logical_cols <-
          names(srce_data())[which(sapply(srce_data(), function(x){
            all(x %in% c(0, 1)) %in% TRUE
          }))]
            
        names(logical_cols) <- logical_cols
        
        lapply(logical_cols, function(x)
          which(srce_data()[[x]] == 1))
        # }
      # })
    })
    
    # Subset sets to those selected
    
    getSelectedSets <- reactive({
      
      valid_sets <- getValidSets()
      validate(need(!is.null(valid_sets), "Please upload data"))
      chosen_sets <- getSelectedSetNames()
      sets <- valid_sets[chosen_sets]
      # if (getSetSort()) {
        sets <- sets[order(unlist(lapply(sets, length)))]
      # }
      sets
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
      
      # assignment_type <- getIntersectionAssignmentType()
      
      # No point starting at size 1 in a non-upset plot
      
      # startsize <- ifelse(assignment_type == "upset", 1, 2)
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
      # For UpSet-ness, membership of higher-order intersections takes priority Otherwise just return the number of entries in each intersection
      
      intersects <- lapply(1:length(intersects), function(i) {
        intersectno <- intersects[[i]]
        members_in_higher_levels <-
          unlist(intersects[(i + 1):length(intersects)])
        lapply(intersectno, function(intersect) {
          # if (assignment_type == "upset") {
            length(setdiff(intersect, members_in_higher_levels))
          # } else {
          # length(intersect)
          # }
        })
      })
      
      combos <- unlist(combos, recursive = FALSE)
      intersects <- unlist(intersects)
      
      # if (!getShowEmptyIntersections()) {
      combos <- combos[which(intersects > 0)]
      intersects <- intersects[which(intersects > 0)]
      # }
      
      # Sort by intersect size
      
      combos <- combos[order(intersects, decreasing = TRUE)]
      intersects <-
        intersects[order(intersects, decreasing = TRUE)]
      
      list(combinations = combos, intersections = intersects)
      
      # })
    })
    
    
    output$dataupset <- plotly::renderPlotly({
      
      validate(need(nrow(srce_data()) > 0 ||
                      !all(is.na(county_data()$total_cases)),
                    "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))
      
      ## Dot-line intersection plot
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
        
        lines <-
          data.table::rbindlist(lapply(1:nintersections, function(combono) {
            data.frame(
              combo = combono,
              x = rep(combono, max(2, length(combos[[combono]]))),
              y = (nsets - combos[[combono]]) + 1,
              name = setnames[combos[[combono]]]
            )
          }))[,`:=` (idx = .N), by = .(x, name)
              ][,`:=` (name = ifelse(idx %in% 1,
                                        paste(name, collapse = " \u2229 "),
                                        name)), by = .(x)]
        
        plot_ly(
          type = "scatter",
          mode = "markers",
          marker = list(color = consts$colors$ash_light,
                        size = 5)
        ) %>% add_trace( ## grey dots background
          type = "scatter",
          x = rep(1:nintersections,
                  length(selected_sets)),
          y = unlist(lapply(1:length(selected_sets), function(x)
            rep(x - 0.5, nintersections))),
          hoverinfo = "none",
          marker = list(color = consts$colors$ash_light,
                        size = 5)
        ) %>% add_trace( ## green dots - sets
          type = "scatter",
          data = group_by(lines, combo),
          mode = "lines+markers",
          x = lines$x,
          y = lines$y - 0.5,
          line = list(color = consts$colors$secondary, width = 3),
          marker = list(color = consts$colors$secondary,
                        size = 5),
          hoverinfo = "text",
          text = ~name
        ) %>% layout(
          xaxis = list(
            showticklabels = FALSE,
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            showticklabels = FALSE,
            showgrid = TRUE,
            range = c(0, nsets),
            zeroline = FALSE,
            range = 1:nsets
          ),
          margin = list(t = 0, b = 0),
          dragmode = "select"
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
      })
      
      # Make the bar chart illustrating set sizes
      
      upsetSetSizeBarChart <- reactive({
        
        selected_sets <- getSets()
        setnames <- names(selected_sets)
        
        
        plot_ly(
          x = unlist(lapply(selected_sets, length)),
          y = setnames,
          type = "bar",
          orientation = "h",
          marker = list(color = consts$colors$secondary),
          hovertemplate = ~paste(
            "<b>", setnames, "</b>",
            "<br> Total reported cases:",
            scales::comma(unlist(lapply(selected_sets, length)),
                          accuracy = 1),
            "<extra></extra>"
          )
        ) %>% layout(
          bargap = 0.4,
          yaxis = list(
            categoryarray = rev(setnames),
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
          ))
      })
      
      upsetIntersectSizeBarChart <- reactive({
        
        ints <- calculateIntersections()
        intersects <- ints$intersections
        combos <- ints$combinations
        nintersections <- length(calculateIntersections()[["intersections"]])
        
        p <-
          plot_ly(showlegend = FALSE) %>% 
          add_trace(
            x = 1:nintersections,
            y = unlist(intersects[1:nintersections]),
            type = "bar",
            marker = list(color = consts$colors$secondary),
            hovertemplate = ~paste(
              "<b> Total reported cases </b>",
              "<br>",scales::comma(unlist(intersects[1:nintersections]),
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
          ))
        
        # bar_numbers <- getBarNumbers()
        
        # if (bar_numbers) {
        #   p <-
        #     p %>% add_trace(
        #       type = "scatter",
        #       mode = "text",
        #       x = 1:nintersections,
        #       y = unlist(intersects[1:nintersections]) + (max(intersects) * 0.05),
        #       text = unlist(intersects[1:nintersections]),
        #       textfont = list(color = "black")
        #     )
        # }
        
        p
      })
      
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