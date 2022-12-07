import("shiny")

export("ui")
export("init_server")

consts <- use("constants/constants.R")

ui <- function(id){
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  
  metrics <- consts$metrics_list
  metricsLength <- ifelse(id == "global_metrics_simple_view", length(metrics), 4)
  
  # Map list of available metrics to UI elements
  lapply(seq_along(1:metricsLength), function(index) {
    div (
      class = paste0("box box-primary metric metric-global metric-global-", index),
      div(class = "icon"),
      div(
        class = "value",
        tags$label(metrics[[index]]$label),
        # This looks the same as your usual piece of code, 
        # except that the id is wrapped into 
        # the ns() function we defined before.
        # We need to ns() all ids
        textOutput(ns(paste0("metricsbox", index)))
      )
    )
  })
}

init_server <- function(id) {
  callModule(server, id, id)
}

server <- function(input, output, session, id) {
  ns <- session$ns
  
  output$metricsbox1 <- renderText({ consts$metrics_list$births$value })
  output$metricsbox2 <- renderText({ consts$metrics_list$anomalies$value })
  output$metricsbox3 <- renderText({ consts$metrics_list$rate$value})
  output$metricsbox4 <- renderText({ consts$metrics_list$infants$value})
  
  if (id == "global_metrics_simple_view") {
    output$metricsbox3 <- renderText({ consts$metrics_list$rate$value})
    #output$metricsbox4 <- renderText({ CONSTS$metrics_list$shipments_day$value })
  }
}

