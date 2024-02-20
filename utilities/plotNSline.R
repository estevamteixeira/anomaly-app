## lineplot for total -------
plot_line <- function(data, var){
 # Use 'import' from the 'modules' package.
 # These listed imports are made available inside the module scope.
 import("dplyr")
 import("ggplot2")
 import("plotly")

 dta <- data
 var <- unlist(var)

 pal <- "#5C9895"

 plotly::plot_ly(
  data = dta %>% arrange(Birth_Year),
  x = ~Birth_Year,
  y = ~.data[[var]],
  type = "scatter",
  mode = "lines+markers",
  hovertemplate = ~paste(
   "<b>", Birth_Year, "</b>",
   "<br> Prevalence:",
   scales::comma(.data[[var]],
                 accuracy = 0.1),
   "<extra></extra>"
  ),
  line = list(color = pal),
  marker = list(color = pal)) %>%
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
    y = -0.25, x = 0.5,
    xanchor = "center", yanchor = "middle",
    font = list(
     size = 12,
     face = "bold"
    )
   ),
   xaxis = list(
    color = "#00706E",
    title = list(
     text = "Year",
     face = "bold",
     size = 14
    ),
    tickfont = list(
     face = "bold",
     size = 14
    ),
    tickformat = "%Y"
   ),
   yaxis = list(
    color = "#00706E",
    title = list(
     text = "Prevalence <br> (cases per 10,000 total births)",
     face = "bold",
     size = 14,
     rangemode = "tozero"
    ),
    tickfont = list(
     face = "bold",
     size = 14
    ),
    tickformat = ","
   ),
   font = list(
    family = "Montserrat",
    size = 14,
    color = "#00706E"
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

plot_line_risk <- function(data, var, risk){

 # Use 'import' from the 'modules' package.
 # These listed imports are made available inside the module scope.
 import("dplyr")
 import("plotly")

 dta <- data
 var <- unlist(var)
 rsk <- unlist(risk)

 # Assign specific colors to category levels

 if(tolower(rsk) %in% "sexnum"){
   dta$SexNum <- factor(dta$SexNum,
                        levels = c("M","F"),
                        labels = c(#"-1",
                                   "Male",
                                   "Female"))
   pal <- c("-1" = "#FFFFFF",
            "Female" = "#E41A1C",
            "Male" = "#5C9895")

  } else if(tolower(rsk) %in% "bmipp"){
   dta$bmipp <- factor(dta$bmipp,
                       levels = c(1,2,3),
                       labels = c(#"-1",
                                  "Not Obese (BMI < 30)",
                                  "Obese I, II (BMI 30-39.9)",
                                  "Obese III (BMI 40+)"
                       )
   )
   pal <- c("-1" = "#FFFFFF",
            "Not Obese (BMI < 30)" = "#E41A1C",
            "Obese I, II (BMI 30-39.9)" = "#377EB8",
            "Obese III (BMI 40+)" = "#4DAF4A"
   )
   } else if(tolower(rsk) %in% "cannabis_use"){
   dta$Cannabis_Use <- factor(dta$Cannabis_Use,
                              levels = c(0,1),
                              labels = c("No",
                                         "Yes"))
   pal <- c("No" = "#5C9895",
            "Yes" = "#E41A1C")
  } else if(tolower(rsk) %in% "diab"){
   dta$diab <- factor(dta$diab,
                      levels = c(0,1),
                      labels = c("No",
                                 "Yes"))
   pal <- c("No" = "#5C9895",
            "Yes" = "#E41A1C")
  } else if(tolower(rsk) %in% "smoker"){
   dta$smoker <- factor(dta$smoker,
                        levels = c(0,1),
                        labels = c(#"-1",
                                   "No",
                                   "Yes"))
   pal <- c("-1" = "#FFFFFF",
            "No" = "#5C9895",
            "Yes" = "#E41A1C")
  } else if(tolower(rsk) %in% "matage"){
   dta$matage <- factor(dta$matage,
                        levels = c(1,2),
                        labels = c("Age < 35",
                                   "Age 35+"))
   pal <- c("Age < 35" = "#E41A1C",
            "Age 35+" = "#5C9895")
  } else if(tolower(rsk) %in% "none"){
   pal <- c("#5C9895")
  }else if(tolower(rsk) %in% "alcohol_use"){
   dta$Alcohol_Use <- factor(dta$Alcohol_Use,
                             levels = c(0,1),
                             labels = c("No",
                                        "Yes"))
   pal <- c("No" = "#E41A1C",
            "Yes" = "#5C9895")
  }

 plotly::plot_ly(data = dta %>% filter(!rsk %in% -1) %>% arrange(Birth_Year),
                 x = ~Birth_Year,
                 y = ~.data[[var]],
                 type = "scatter",
                 mode = "lines+markers",
                 # linetype = ~.data[[rsk]],
                 color = ~.data[[rsk]],
                 colors = pal,
                 symbol = ~.data[[rsk]],
                 hovertemplate = ~paste(
                  "<b>", Birth_Year, "<br>", .data[[rsk]], "</b>",
                  "<br> Prevalence:",
                  scales::comma(.data[[var]],
                                accuracy = 0.1),
                  "<extra></extra>" # removes the trace name from the hover text
                 ),
                 line = list(color = pal),
                 marker = list(col = pal)) %>%
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
    y = -0.35, x = 0.5,
    xanchor = "center", yanchor = "middle",
    font = list(
     size = 12,
     face = "bold"
    )
   ),
   xaxis = list(
    color = "#00706E",
    title = list(
     text = "Year",
     face = "bold",
     size = 14
    ),
    tickfonts = list(
     face = "bold",
     size = 12
    )
   ),
   yaxis = list(
    color = "#00706E",
    title = list(
     text = "Prevalence <br> (*cases per 10,000 total births)",
     face = "bold",
     size = 14,
     rangemode = "tozero"
    ),
    tickfont = list(
     face = "bold",
     size = 14
    ),
    tickformat = ","
   ),
   font = list(
    family = "Montserrat",
    size = 14,
    color = "#00706E",
    zerolinecolor = "#00706E",
    gridcolor = "#00706E"
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
