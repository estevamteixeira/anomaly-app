# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("cancensus")
import("data.table")
import("dplyr")
import("shiny")
import("shinydashboard")
import("stringr")
import("utils")

# App title
app_title <- "Surveillance of Congenital Anomalies"

# App version
app_version <- "0.0.1"

# urls and names
rcp_website <- "http://rcp.nshealth.ca/"
rcp_name <- "Reproductive Care Program of Nova Scotia"
dev_website  <- "https://www.linkedin.com/in/estevam-caixeta/"
dev_name <- "Estevam Teixeira"
rcp_contact <- "https://rcp.nshealth.ca/contact"
iwk_website <- "https://www.iwk.nshealth.ca/"
iwk_name <- "IWK Health Centre"

# key to access cancensus datasets
key = "CensusMapper_f505397ff4bb63467541085d028c9be8"

# importing data sets

cd_anom <- read.csv("./data/cd_anomaly.csv", header = TRUE, stringsAsFactors = TRUE) |> 
  dplyr::mutate(CD_UID = as.character(CD_UID),
                Birth_Date = as.Date(as.character(Birth_Date))) |>
  ## filtering only NS counties
  dplyr::filter(dplyr::between(CD_UID, 1201, 1299)) |> 
  data.table::setDT()

cd_birth <- read.csv("./data/cd_birth.csv", header = TRUE, stringsAsFactors = TRUE) |> 
  dplyr::mutate(CD_UID = as.character(CD_UID)) |> 
  data.table::setDT()

## Help and intro data
steps <- read.csv("data/help.csv")
intro <- read.csv("data/intro.csv")

# Import shapefiles
# Convert them to 'data.tables' by reference
csd_shp <- data.table::setDT(cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = "12"),
  level = "CSD",
  geo_format = "sf",
  api_key = key
))[,`:=` (
  csd_type = data.table::fcase(
    tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(iri)" , "Indian reserve",
    tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(md)" , "Municipal district",
    tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(rgm)" , "Regional municipality",
    tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(rm)" , "Rural municipality",
    tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(sc)" , "Subdivision of county municipality",
    tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(t)" , "Town"
  ),
  name = stringr::str_remove(name, "\\(([^()]*)\\)$"))][,
       c("GeoUID","CD_UID","CMA_UID", "name","csd_type",
       "Dwellings 2016", "Dwellings", "Population 2016", "Population",
       "Households 2016", "Households", "Shape Area",
       "geometry")]

cd_shp <- data.table::setDT(cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = "12"), 
  level = "CD",
  geo_format = "sf",
  api_key = key
))[,`:=` (name = stringr::str_remove(name, " \\(CTY\\)"),
          cd_type = "County")][,
       c("GeoUID", "name", "cd_type", "Dwellings 2016","Dwellings", 
       "Population 2016", "Population", "Households 2016",
       "Households", "Shape Area", "geometry")]

cd_names <- data.frame(CD_UID = cd_shp$GeoUID, cd_full = cd_shp$name)

# Colors
colors <- list(
  white = "#FFFFFF",
  black = "#0A1E2B",
  primary = "#EBF3F2", # pastel green
  secondary = "#008D8B", # RCP green
  terciary = "#00706E", # olive green
  ash = "#B3B8BA", 
  ash_light = "#E3E7E9"
)

# App time range
app_time_range <- paste(min(cd_anom$BrthYear),
                        "-",
                        max(cd_anom$BrthYear))

metrics_list <- list(
  births = list(
    label = "Number of total births",
    value = paste(scales::comma(
      sum(unique(
          cd_birth[tolower(dlv) %in% c("lvb", "stillbirth"),
                   .(BrthYear, cd.count_dlv, CD_UID)])$cd.count_dlv),
      accuracy = 1), "total births")
  ),
  anomalies = list(
    label = "Number of registered congenital anomalies",
    value = paste(scales::comma(
      nrow(unique(
        cd_anom[, .(CaseID, BrthYear, CD_UID, Diags)])),
      accuracy = 1), "records")
  ),
  rate = list(
    label = "Prevalence <br> (* cases per 1,000 live births)",
    value = scales::comma(
      1000*nrow(unique(
        cd_anom[, .(CaseID, BrthYear, CD_UID, Diags)]))/sum(unique(
          cd_birth[tolower(dlv) %in% c("lvb", "stillbirth"),
                   .(BrthYear, cd.count_dlv, CD_UID)])$cd.count_dlv),
      accuracy = 0.1)
  ),
  infants = list(
    label = "Number of infants diagnosed",
    value = paste(scales::comma(
      nrow(unique(
        cd_anom[, .(CaseID, BrthYear, CD_UID)])),
      accuracy = 1), "infants")
  )
)

icd10_opts <- c(unique(levels(cd_anom$cat_tier2)))

risk_opts <- sort(c("SexNum","matage","smoker","bmipp","diab","Cannabis_Use"))
names(risk_opts) <- c("BMI",
                      "Cannabis Use",
                      "Diabetes",
                      "Maternal Age",
                      "Phenotypical Sex",
                      "Smoking Use")

rcp_logo <- tags$a(
  href = rcp_website,
  target = "_blank",
  rel = "nofollow noreferrer",
  class = "logo-link",
  img(src = "images/RCP_IWK_Logo.svg",
      class = "logo-img",
      alt = "RCP Logo")
)

iwk_logo <- tags$a(
  href = iwk_website,
  target = "_blank",
  rel = "nofollow noreferrer",
  class = "logo-link",
  img(src = "images/RCP_hexSticker.svg",
      class = "logo-img",
      alt = "IWK Logo")
)

rcp_legal <- tags$p(
  class = "footer-legal",
  tags$span(HTML("&copy;"),
            paste(format(Sys.Date(), "%Y"),
                  "| All Rights Reserved | Built with ❤ by")),
  tags$a(
    class = "footer-link",
    href = rcp_website,
    target = "_blank",
    rel = "nofollow noreferrer",
    rcp_name
  )
)