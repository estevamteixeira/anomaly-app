# Load all packages
library(data.table)
library(dplyr)
library(geojsonsf)
library(ggplot2)
library(leaflet)
library(lubridate)
library(modules)
library(plotly)
library(readr)
library(rintrojs)
library(sass)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(tidyverse)

# Modules
map_view <- use("modules/map_view.R")
global_metrics_view <- use("modules/global_metrics_view.R")
local_metrics_view <- use("modules/local_metrics_view.R")
county_view <- use("modules/county_view.R")
line_view <- use("modules/line_view.R")
# dlv_view <- use("modules/dlvhosp_view.R")
upset_view <- use("modules/upsetPlot_view.R")

# Compile sass to css
sass(
  input = sass::sass_file("styles/main.scss"),
  cache = NULL,
  # options = sass_options(output_style = "compressed"),
  output = "www/css/sass.min.css"
)

