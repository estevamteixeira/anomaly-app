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
  data.table::setDT()

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
  data.table::setDT()

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
    "H:\\RCP\\RCP_Data\\TeixeiEC\\NS_Maps\\NSC_Clusters\\geo_export_0ed8251f-dbd5-4090-88ef-66c84d140e53.shp") %>%
    sf::st_make_valid()
  )

hn <- data.table::setDT(
  sf::read_sf(
    "H:\\RCP\\RCP_Data\\TeixeiEC\\NS_Maps\\NSC_HealthNetwork\\geo_export_6985c353-2399-43a2-9514-d47b553341c2.shp") %>%
    dplyr::mutate(network = stringr::str_remove(network, " Comm H. Network")) %>% 
    sf::st_make_valid()
)

# urb_shp <- data.table::setDT(
#   sf::read_sf(
#     "H:\\RCP\\RCP_Data\\TeixeiEC\\NS_Maps\\NSC_Urban\\urban_boundary.shp") %>%
#     sf::st_make_valid()
# )

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
      nrow(unique(
          cd_birth[tolower(dlv) %in% c("lvb", "stillbirth"),
                   .(BIRTHID, BrthYear, CSDuid)])),
      accuracy = 1), "total births")
  ),
  anomalies = list(
    label = "Number of registered congenital anomalies",
    value = paste(scales::comma(
      nrow(unique(
        cd_anom[, .(CaseID, BrthYear, CSDuid, Diags)])),
      accuracy = 1), "records")
  ),
  rate = list(
    label = "Prevalence <br> (* cases per 1,000 live births)",
    value = scales::comma(
      1000*nrow(unique(
        cd_anom[, .(CaseID, BrthYear, CSDuid, Diags)]))/nrow(unique(
          cd_birth[tolower(dlv) %in% c("lvb", "stillbirth"),
                   .(BIRTHID, BrthYear, CSDuid)])),
      accuracy = 0.1)
  ),
  infants = list(
    label = "Number of infants diagnosed",
    value = paste(scales::comma(
      nrow(unique(
        cd_anom[, .(CaseID, BrthYear, CSDuid)])),
      accuracy = 1), "infants")
  )
)

# icd10_opts <- c(unique(levels(cd_anom$cat_tier4)))
# icd10_opts_grp <- c(unique(levels(cd_anom$cat_tier3)))

icd10_opts <- list(
  "0",
  c(as.character(unique(cd_anom[grepl("^Q000$|Q0000$|Q01|Q05", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q000$|Q0000$|Q01|Q05", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q02|Q03|Q041|Q042", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q02|Q03|Q041|Q042", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q110|Q11$|Q111|Q112|Q160|Q172|Q16$|Q17$|Q30", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q110|Q11$|Q111|Q112|Q160|Q172|Q16$|Q17$|Q30", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q200|Q20$|Q203|Q212|Q213|Q234|Q251", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q200|Q20$|Q203|Q212|Q213|Q234|Q251", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q35|Q36|Q37", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q35|Q36|Q37", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q39[0-4]|Q41|Q42[0-3]|Q431|Q442", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q39[0-4]|Q41|Q42[0-3]|Q431|Q442", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q53[1-2]|Q539|Q54|Q56|Q640", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q53[1-2]|Q539|Q54|Q56|Q640", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q60[0-2]|Q61[1-5]|Q61[8-9]|Q64[1-3]", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q60[0-2]|Q61[1-5]|Q61[8-9]|Q64[1-3]", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q65", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q65", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q71[4-9]|Q72[4-9]|Q738", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q71[4-9]|Q72[4-9]|Q738", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q79[2-3]", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q79[2-3]", Diags)]$cat_tier4)))),
  c(as.character(unique(cd_anom[grepl("^Q909|Q90$|Q91[4-7]|Q91[0-3]|Q96", Diags)]$cat_tier3)),
    sort(as.character(unique(cd_anom[grepl("^Q909|Q90$|Q91[4-7]|Q91[0-3]|Q96", Diags)]$cat_tier4))))
)

names(icd10_opts) <- c(
  "All conditions",
  as.character(unique(cd_anom[grepl("^Q000$|Q0000$|Q01|Q05", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q02|Q03|Q041|Q042", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q110|Q11$|Q111|Q112|Q160|Q172|Q16$|Q17$|Q30", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q200|Q20$|Q203|Q212|Q213|Q234|Q251", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q35|Q36|Q37", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q39[0-4]|Q41|Q42[0-3]|Q431|Q442", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q53[1-2]|Q539|Q54|Q56|Q640", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q60[0-2]|Q61[1-5]|Q61[8-9]|Q64[1-3]", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q65", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q71[4-9]|Q72[4-9]|Q738", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q79[2-3]", Diags)]$cat_tier3)),
  as.character(unique(cd_anom[grepl("^Q909|Q90$|Q91[4-7]|Q91[0-3]|Q96", Diags)]$cat_tier3))
)

risk_opts <- sort(c("SexNum","matage","smoker","bmipp","diab","Cannabis_Use","Alcohol_Use"#,"area"
                    ))
names(risk_opts) <- c("Alcohol Use",
                      # "Location",
                      "BMI",
                      "Cannabis Use",
                      "Diabetes",
                      "Maternal Age",
                      "Fetal Sex",
                      "Smoking Use")
risk_opts <- risk_opts[order(names(risk_opts))]

geo_opts <- c("csd","cd", "clus", "hn", #"zn",
              "urb")
names(geo_opts) <- c("Municipalities (CSD)",
                     "Counties (CD)",
                     "Community clusters",
                     "Community health networks",
                     # "Management Zones",
                     "Urban-rural"
                     )


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