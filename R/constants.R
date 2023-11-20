# App title ----
app_title <- "Surveillance of Congenital Anomalies"

# App version ----
app_version <- "0.0.3"

# urls and names ----
rcp_website <- "http://rcp.nshealth.ca/"
rcp_name <- "Reproductive Care Program of Nova Scotia"
dev_website  <- "https://www.linkedin.com/in/estevam-caixeta/"
dev_name <- "Estevam Teixeira"
rcp_contact <- "https://rcp.nshealth.ca/contact"
iwk_website <- "https://www.iwk.nshealth.ca/"
iwk_name <- "IWK Health Centre"

# Importing shapefiles ----

## Key to access 'cancensus' shapefiles ----
# key = source("R/key.R")

# This will set the cache path permanently until ovewritten again
cancensus::set_cancensus_cache_path("~/cancensus_cache", install = TRUE)

## Census district (CD) ----

cd_shp <- arrow::read_arrow("./data/NSC_cd.parquet", as_data_frame = FALSE)
 # sf::st_make_valid()
 # geoarrow_collect_sf()

## Community Clusters (CL) ----

cl_shp <- arrow::read_parquet("./data/NSC_cl.parquet", as_data_frame = FALSE)

## Community Health Networks (CHN) ----

geoarrow::read_geoparquet_sf("./data/NSC_chn.parquet") %>%
 sf::st_make_valid()

## Health Authority Zones (HR) ----

hr_shp <- geoarrow::read_geoparquet_sf("./data/NSC_hr.parquet") %>%
 sf::st_make_valid()

# Importing anomaly data ----

ano <- arrow::read_parquet("./data/Anomaly.parquet", as_data_frame = FALSE)

# Data time range
app_time_range <- paste(min(cd_anom$BrthYear),
                        "-",
                        max(cd_anom$BrthYear))
