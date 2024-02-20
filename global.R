suppressPackageStartupMessages({
 # Load all packages
 library(arrow)
 library(bslib)
 library(data.table)
 library(dplyr)
 library(DT)
 library(ggplot2)
 library(leaflet)
 library(markdown)
 library(modules)
 library(plotly)
 library(sf)
 library(shiny)
 library(shinydashboard)
 library(shinyjs)
 library(stringr)
 library(webshot)
})

# To download the map
Sys.setenv("OPENSSL_CONF"="/dev/null")
webshot::install_phantomjs()

# Modules
homeTab <- use("modules/mod-home.R")
summTab <- use("modules/mod-summary.R")
mapTab <- use("modules/mod-map.R")
trendTab <- use("modules/mod-trend.R")
