# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
modules::import("arrow")
options(arrow.pull_as_vector = TRUE)
# import("cancensus")
# import("data.table")
modules::import("dplyr")
modules::import("shiny")
modules::import("shinydashboard")
# import("stringr")
# import("utils")

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

cd_shp <- arrow::read_parquet("./data/NSC_cd.parquet", as_data_frame = FALSE)
 # sf::st_make_valid()
 # geoarrow_collect_sf()

## Community Clusters (CL) ----

cl_shp <- arrow::read_parquet("./data/NSC_cl.parquet", as_data_frame = FALSE)

## Community Health Networks (CHN) ----

chn_shp <- arrow::read_parquet("./data/NSC_chn.parquet", as_data_frame = FALSE)

## Health Authority Zones (HR) ----

hr_shp <- arrow::read_parquet("./data/NSC_hr.parquet", as_data_frame = FALSE)

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
  ano %>% filter(grepl("^(Q999)", Diags)) %>% select(Diags) %>%
   distinct() %>% collect() %>% pull() %>% toupper(),
  ano %>% filter(grepl("^(Q999)", Diags)) %>% select(cat) %>%
   distinct() %>% collect() %>% pull()
  ),
 ## Neural tube defects ----
 stats::setNames(
  ano %>% filter(grepl("^(GR101|Q00|Q01|Q05)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR101|Q00|Q01|Q05)", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
   ),
 ## Selected central nervous system defects ----
 stats::setNames(
  ano %>% filter(grepl("^(GR102|Q02|Q03|Q041|Q042)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR102|Q02|Q03|Q041|Q042)", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected central nervous system defects ----
 stats::setNames(
  ano %>% filter(grepl("^(GR103|Q110|Q111|Q112|Q160|Q172|Q300)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>%  sort(),
  ano %>%filter(grepl("^(GR103|Q110|Q111|Q112|Q160|Q172|Q300)", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected congenital heart defects ----
 stats::setNames(
  ano %>% filter(grepl("^(GR104|Q200|Q201|Q203|Q205|Q212|Q213|Q234|Q251)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR104|Q200|Q201|Q203|Q205|Q212|Q213|Q234|Q251)", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Oro-facial clefts ----
 stats::setNames(
  ano %>% filter(grepl("^(GR105|Q35|Q36|Q37)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR105|Q35|Q36|Q37)", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected gastrointestinal defects ----
 stats::setNames(
  ano %>% filter(grepl("^(GR106|Q39[0-4]|Q41|Q42[0-3]|Q431|Q442)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR106|Q39[0-4]|Q41|Q42[0-3]|Q431|Q442)", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected genital anomalies ----
 stats::setNames(
  ano %>% filter(grepl("^(GR107|Q53[1-2]|Q539|Q54|Q56|Q640)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR107|Q53[1-2]|Q539|Q54|Q56|Q640)", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected urinary tract defects ----
 stats::setNames(
  ano %>% filter(grepl("^(GR108|Q60[0-2]|Q61[1-5]|Q61[8-9]|Q64[1-3])", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR108|Q60[0-2]|Q61[1-5]|Q61[8-9]|Q64[1-3])", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Hip dysplasia ----
 stats::setNames(
  ano %>% filter(grepl("^(GR109|Q65)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR109|Q65)", Diags)) %>% arrange(Diag) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Limb deficiency defects ----
 stats::setNames(
  ano %>% filter(grepl("^(GR110|Q71[4-9]|Q72[4-9]|Q738)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR110|Q71[4-9]|Q72[4-9]|Q738)", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>% collect() %>% pull()
 ),
 ## Selected abdominal wall defects ----
 stats::setNames(
  ano %>% filter(grepl("^(GR111|Q79[2-3])", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
  ano %>% filter(grepl("^(GR111|Q79[2-3])", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>%  collect() %>% pull()
 ),
 ## Selected chromosomal defects ----
 stats::setNames(
  ano %>% filter(grepl("^(GR112|Q90|Q91[4-7]|Q91[0-3]|Q96)", Diags)) %>% select(Diag) %>%
   distinct() %>% collect() %>% pull() %>% toupper() %>% sort(),
   ano %>% filter(grepl("^(GR112|Q90|Q91[4-7]|Q91[0-3]|Q96)", Diags)) %>% arrange(Diag) %>%
   select(Diag, cat) %>% distinct() %>%  collect() %>% pull()
 )
)

# List name ----
## The outermost names will be used as the group labels

names(icd_lbl) <- c(
 ano %>% filter(grepl("^(Q999)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR101)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR102)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR103)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR104)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR105)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR106)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR107)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR108)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR109)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR110)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR111)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(GR112)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull()
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
 "Census District", "Community Clusters", "Community Health Networks", "Health Authority Zones"
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

