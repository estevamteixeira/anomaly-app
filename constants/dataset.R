# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("arrow")
import("cancensus")
import("data.table")
import("dplyr")
import("shiny")
import("shinydashboard")
import("stringr")
import("utils")

# key to access cancensus datasets
key = "CensusMapper_f505397ff4bb63467541085d028c9be8"

# importing data sets

cd_anom <- readr::read_csv("./data/cd_anomaly.csv") |>
  ## filtering only NS counties
  dplyr::filter(dplyr::between(CSDuid, 1201000, 1299999)) |>
  dplyr::filter(!substr(CSDuid, 5, 8) %in% "999") |> 
  dplyr::mutate(CSDuid = as.character(CSDuid),
                Birth_Date = as.Date(as.character(Birth_Date)),
                CDuid = substr(CSDuid, 1, 4),
                Cluster_Number = stringr::str_pad(Cluster_Number, 6, pad = "0"),
                NetworkID = substr(Cluster_Number, 1, 4),
                ZoneID = substr(Cluster_Number, 1, 2),
                CSDuid = ifelse(CSDuid %in% c("1208001","1208002"),
                                "1208003",
                                CSDuid),
                CSDName = ifelse(CSDuid %in% c("1208003"),
                                 "West Hants",
                                 CSDName),
                CSDType = ifelse(CSDuid %in% c("1208003"),
                                 "Rural municipality",
                                 CSDType),
                SexNum = as.numeric(SexNum)) |>
  # data.table::setDT()
  arrow::arrow_table()

cd_birth <- readr::read_csv("./data/cd_birth.csv") |>
  ## filtering only NS counties
  dplyr::filter(dplyr::between(CSDuid, 1201000, 1299999)) |>
  dplyr::filter(!substr(CSDuid, 5, 8) %in% "999") |>
  dplyr::mutate(CSDuid = as.character(CSDuid),
                CDuid = substr(CSDuid, 1, 4),
                Cluster_Number = stringr::str_pad(Cluster_Number, 6, pad = "0"),
                NetworkID = substr(Cluster_Number, 1, 4),
                ZoneID = substr(Cluster_Number, 1, 2),
                CSDuid = ifelse(CSDuid %in% c("1208001","1208002"),
                                "1208003",
                                CSDuid),
                CSDName = ifelse(CSDuid %in% c("1208003"),
                                 "West Hants",
                                 CSDName),
                CSDType = ifelse(CSDuid %in% c("1208003"),
                                 "Rural municipality",
                                 CSDType),
                SexNum = dplyr::case_when(
                  BTSEX %in% "M" ~ 1,
                  BTSEX %in% "F" ~ 2,
                  BTSEX %in% "A" ~ -1
                )) |>
  # data.table::setDT()
  arrow::arrow_table()

## Help and intro data
# steps <- readr::read_csv("data/help.csv")
intro <- readr::read_csv("data/intro.csv")

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

# cd_names <- data.frame(CD_UID = cd_shp$GeoUID, cd_full = cd_shp$name)

## Community clusters -----
## Community Health Networks
## Management Zones

clus <- data.table::setDT(
  sf::read_sf(
    "./data/NSC_clusters.shp") %>%
    sf::st_make_valid()
)

hn <- data.table::setDT(
  sf::read_sf(
    "./data/NSC_hn.shp") %>%
    dplyr::mutate(network = stringr::str_remove(network, " Comm H. Network")) %>% 
    sf::st_make_valid()
)

# urb_shp <- data.table::setDT(
#   sf::read_sf(
#     "H:\\RCP\\RCP_Data\\TeixeiEC\\NS_Maps\\NSC_Urban\\urban_boundary.shp") %>%
#     sf::st_make_valid()
# )
