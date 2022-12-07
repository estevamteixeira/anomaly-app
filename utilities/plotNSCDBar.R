plot_bar <- function(data, var){
  # Use 'import' from the 'modules' package.
  # These listed imports are made available inside the module scope.
  import("dplyr")
  import("ggplot2")
  import("plotly")
  
  dta <- data
  var <- unlist(var)
  
  if(any(names(dta) %in% c("ntc","ntc_cd"))){
    names(dta)[which(names(dta) %in% c("ntc","ntc_cd"))] <- c("total_cases", "total_cases_cd")
  }
  
    # Wrap long names
  
  dta$label <- sapply(dta$DLHosp,
                     function(x) {paste(strwrap(x, width = 10), collapse = "<br>")})
  
  pal <- "#008D8B"
  
  plotly::plot_ly(
    data = dta,
    x = ~label,
    y = ~.data[[var]],
    type = "bar",
    hovertemplate = ~paste(
      "<b>", DLHosp, "</b>",
      "<br> Total reported cases:",
      ifelse(total_cases < 5,
             "< 5",
             as.character(scales::comma(total_cases,
                                        accuracy = 1))),
      "<br> Proportion of deliveries:",
      scales::percent(.data[[var]],
                    accuracy = 0.01),
      "<extra></extra>"
    ),
    marker = list(color = pal,
                  line = list(color = pal,
                              opacity = 1) )) %>% 
    plotly::style(hoverlabel = list(
      bgcolor  = "black",
      bordercolor = "transparent",
      font = list(
        color = "white",
        size = 14,
        face = "bold"
      )
    )) %>%
    plotly::layout(
      #showlegend = FALSE,
      legend = list(
        orientation = "h",
        y = -0.25, x = 0.35,
        font = list(
          size = 12,
          face = "bold"
        )
      ),
      xaxis = list(
        title = list(
          text = "",
          face = "bold",
          size = 14
        ),
        tickfont = list(
          face = "bold",
          size = 12
        ),
        categoryorder = "total descending"
      ),
      yaxis = list(
        title = list(
          text = "Proportion of reported cases deliveries",
          face = "bold",
          size = 14
        ),
        tickfont = list(
          face = "bold",
          size = 12
        ),
        tickformat = ".0%",
        range = c(0, NA)
      )
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
}