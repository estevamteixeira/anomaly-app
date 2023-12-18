# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
modules::import("arrow")
options(arrow.pull_as_vector = TRUE)
modules::import("dplyr")
modules::import("geoarrow")
modules::import("shiny")
modules::import("shinydashboard")

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
# cancensus::set_cancensus_cache_path("~/cancensus_cache", install = TRUE)

## Census district (CD) ----

cd_shp <- geoarrow::read_geoparquet_sf("./data/NSC_cd.parquet") %>%
 select(GeoUID, name, geometry) %>%
 arrow_table()
 # sf::st_make_valid()
 # geoarrow_collect_sf()

## Community Clusters (CL) ----

cl_shp <- geoarrow::read_geoparquet_sf("./data/NSC_cl.parquet") %>%
 rename(GeoUID = clusterid) %>%
 arrow_table()

## Community Health Networks (CHN) ----

chn_shp <- geoarrow::read_geoparquet_sf("./data/NSC_chn.parquet") %>%
 rename(GeoUID = network_id) %>%
 arrow_table()

## Health Authority Zones (HR) ----

hr_shp <- geoarrow::read_geoparquet_sf("./data/NSC_hr.parquet") %>%
 select(ZoneID, Name, geometry) %>%
 rename(GeoUID = ZoneID) %>%
 arrow_table()

# Importing anomaly data ----

ano <- arrow::read_parquet("./data/Anomaly.parquet", as_data_frame = FALSE) %>%
 mutate(Diag = ifelse(tolower(substr(Diags,2,2)) %in% "r" |
                       toupper(Diags) %in% "Q999",
                      toupper(Diags),
                      gsub(".*?(Q\\d+\\.?\\d*).*", "\\1", cat)),
        Diag = gsub("\\.", "", Diag)
        ) %>%
 arrow::as_arrow_table()

# Importing birth data ----

birth <- arrow::read_parquet("./data/Birth.parquet", as_data_frame = FALSE)


# Data time range ----
app_time_range <- paste(min(ano %>% select(Birth_Year) %>% collect(), na.rm = TRUE),
                        "-",
                        max(ano %>% select(Birth_Year) %>% collect(), na.rm = TRUE))

# ICD10 labels for selectInput() ----

icd_lbl <- list(
 ## All conditions ----
 stats::setNames(
  ano %>% filter(startsWith(Diags, "Q999")) %>% select(Diags) %>%
   distinct() %>% collect() %>% pull() %>% toupper(),
  ano %>% filter(startsWith(Diags, "Q999")) %>% select(cat) %>%
   distinct() %>% collect() %>% pull()
  ),
 ## Neural tube defects ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR101")|
                  startsWith(Diags,"Q00")|
                  startsWith(Diags,"Q01")|
                  startsWith(Diags,"Q05")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR101")|
                  startsWith(Diags,"Q00")|
                  startsWith(Diags,"Q01")|
                  startsWith(Diags,"Q05")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
   ),
 ## Selected central nervous system defects ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR102")|
                  startsWith(Diags,"Q02")|
                  startsWith(Diags,"Q03")|
                  startsWith(Diags,"Q041")|
                  startsWith(Diags,"Q042")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR102")|
                  startsWith(Diags,"Q02")|
                  startsWith(Diags,"Q03")|
                  startsWith(Diags,"Q041")|
                  startsWith(Diags,"Q042")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected central nervous system defects ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR103")|
                  startsWith(Diags,"Q110")|
                  startsWith(Diags,"Q111")|
                  startsWith(Diags,"Q112")|
                  startsWith(Diags,"Q160")|
                  startsWith(Diags,"Q172")|
                  startsWith(Diags,"Q300")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>%  sort(),
  ano %>% filter(startsWith(Diags,"GR103")|
                  startsWith(Diags,"Q110")|
                  startsWith(Diags,"Q111")|
                  startsWith(Diags,"Q112")|
                  startsWith(Diags,"Q160")|
                  startsWith(Diags,"Q172")|
                  startsWith(Diags,"Q300")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected congenital heart defects ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR104")|
                  startsWith(Diags,"Q200")|
                  startsWith(Diags,"Q201")|
                  startsWith(Diags,"Q203")|
                  startsWith(Diags,"Q205")|
                  startsWith(Diags,"Q212")|
                  startsWith(Diags,"Q213")|
                  startsWith(Diags,"Q234")|
                  startsWith(Diags,"Q251")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR104")|
                  startsWith(Diags,"Q200")|
                  startsWith(Diags,"Q201")|
                  startsWith(Diags,"Q203")|
                  startsWith(Diags,"Q205")|
                  startsWith(Diags,"Q212")|
                  startsWith(Diags,"Q213")|
                  startsWith(Diags,"Q234")|
                  startsWith(Diags,"Q251")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Oro-facial clefts ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR105")|
                  startsWith(Diags,"Q35")|
                  startsWith(Diags,"Q36")|
                  startsWith(Diags,"Q37")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR105")|
                  startsWith(Diags,"Q35")|
                  startsWith(Diags,"Q36")|
                  startsWith(Diags,"Q37")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected gastrointestinal defects ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR106|Q39[0-4]|Q41|Q42[0-3]|Q431|Q442")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR106|Q39[0-4]|Q41|Q42[0-3]|Q431|Q442")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected genital anomalies ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR107")|
                  startsWith(Diags,"Q531")|
                  startsWith(Diags,"Q532")|
                  startsWith(Diags,"Q539")|
                  startsWith(Diags,"Q54")|
                  startsWith(Diags,"Q56")|
                  startsWith(Diags,"Q640")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR107")|
                  startsWith(Diags,"Q531")|
                  startsWith(Diags,"Q532")|
                  startsWith(Diags,"Q539")|
                  startsWith(Diags,"Q54")|
                  startsWith(Diags,"Q56")|
                  startsWith(Diags,"Q640")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected urinary tract defects ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR108")|
                  startsWith(Diags,"Q600")|
                  startsWith(Diags,"Q601")|
                  startsWith(Diags,"Q602")|
                  startsWith(Diags,"Q611")|
                  startsWith(Diags,"Q612")|
                  startsWith(Diags,"Q613")|
                  startsWith(Diags,"Q614")|
                  startsWith(Diags,"Q615")|
                  startsWith(Diags,"Q618")|
                  startsWith(Diags,"Q619")|
                  startsWith(Diags,"Q641")|
                  startsWith(Diags,"Q642")|
                  startsWith(Diags,"Q643")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR108")|
                  startsWith(Diags,"Q600")|
                  startsWith(Diags,"Q601")|
                  startsWith(Diags,"Q602")|
                  startsWith(Diags,"Q611")|
                  startsWith(Diags,"Q612")|
                  startsWith(Diags,"Q613")|
                  startsWith(Diags,"Q614")|
                  startsWith(Diags,"Q615")|
                  startsWith(Diags,"Q618")|
                  startsWith(Diags,"Q619")|
                  startsWith(Diags,"Q641")|
                  startsWith(Diags,"Q642")|
                  startsWith(Diags,"Q643")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Hip dysplasia ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR109")|
                  startsWith(Diags,"Q65")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR109")|
                  startsWith(Diags,"Q65")) %>% arrange(Diag) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Limb deficiency defects ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR110")|
                  startsWith(Diags,"Q714")|
                  startsWith(Diags,"Q715")|
                  startsWith(Diags,"Q716")|
                  startsWith(Diags,"Q717")|
                  startsWith(Diags,"Q718")|
                  startsWith(Diags,"Q719")|
                  startsWith(Diags,"Q724")|
                  startsWith(Diags,"Q725")|
                  startsWith(Diags,"Q726")|
                  startsWith(Diags,"Q727")|
                  startsWith(Diags,"Q728")|
                  startsWith(Diags,"Q729")|
                  startsWith(Diags,"Q738")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR110")|
                  startsWith(Diags,"Q714")|
                  startsWith(Diags,"Q715")|
                  startsWith(Diags,"Q716")|
                  startsWith(Diags,"Q717")|
                  startsWith(Diags,"Q718")|
                  startsWith(Diags,"Q719")|
                  startsWith(Diags,"Q724")|
                  startsWith(Diags,"Q725")|
                  startsWith(Diags,"Q726")|
                  startsWith(Diags,"Q727")|
                  startsWith(Diags,"Q728")|
                  startsWith(Diags,"Q729")|
                  startsWith(Diags,"Q738")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected abdominal wall defects ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR111")|
                  startsWith(Diags,"Q792")|
                  startsWith(Diags,"Q793")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(startsWith(Diags,"GR111")|
                  startsWith(Diags,"Q792")|
                  startsWith(Diags,"Q793")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>%  collect() %>% pull()
 ),
 ## Selected chromosomal defects ----
 stats::setNames(
  ano %>% filter(startsWith(Diags,"GR112")|
                  startsWith(Diags,"Q90")|
                  startsWith(Diags,"Q914")|
                  startsWith(Diags,"Q915")|
                  startsWith(Diags,"Q916")|
                  startsWith(Diags,"Q917")|
                  startsWith(Diags,"Q910")|
                  startsWith(Diags,"Q911")|
                  startsWith(Diags,"Q912")|
                  startsWith(Diags,"Q913")|
                  startsWith(Diags,"Q96")) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
   ano %>% filter(startsWith(Diags,"GR112")|
                   startsWith(Diags,"Q90")|
                   startsWith(Diags,"Q914")|
                   startsWith(Diags,"Q915")|
                   startsWith(Diags,"Q916")|
                   startsWith(Diags,"Q917")|
                   startsWith(Diags,"Q910")|
                   startsWith(Diags,"Q911")|
                   startsWith(Diags,"Q912")|
                   startsWith(Diags,"Q913")|
                   startsWith(Diags,"Q96")) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>%  collect() %>% pull()
 )
)

# List name ----
## The outermost names will be used as the group labels

names(icd_lbl) <- c(
 " ",
 ano %>% filter(startsWith(Diags,"GR101")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR102")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR103")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR104")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR105")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR106")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR107")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR108")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR109")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR110")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR111")) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(startsWith(Diags,"GR112")) %>% select(cat) %>% distinct() %>% collect() %>% pull()
)

# Risk factors for lineplot ----

risk_lbl <- sort(
 c("Alcohol_Use", "Cannabis_Use", "diab", "bmipp", "smoker", "matage", "SexNum")
 )

# The outermost names will be used as label in the dash

names(risk_lbl) <- c(
 "Alcohol Use", "BMI", "Cannabis Use", "Diabetes", "Maternal Age", "Fetal Sex", "Smoking Use"
)

# Order in alphabetical order based on the labels in the dash

risk_lbl <- risk_lbl[order(names(risk_lbl))]

# Geographies for maps ----

geo_lbl <- c(
 "cd", "cl", "chn", "hr"
 )

# The outermost names will be used as label in the dash

names(geo_lbl) <- c(
 "Census Divisions", "Community Clusters", "Community Health Networks", "Health Authority Zones"
)

# Add logo info ----

rcp_logo <- tags$a(
 href = "http://rcp.nshealth.ca/",
 target = "_blank",
 rel = "nofollow noreferrer",
 # class = "logo-link",
 tags$img(src = "www/images/rcp-logo-transparent.svg",
     # class = "logo-img",
     alt = "RCP Logo")
)

iwk_logo <- tags$a(
 href = "https://www.iwk.nshealth.ca/",
 target = "_blank",
 rel = "nofollow noreferrer",
 # class = "logo-link",
 tags$img(src = "www/images/iwk-logo-transparent.svg",
     # class = "logo-img",
     alt = "IWK Logo")
)

