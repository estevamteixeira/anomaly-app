# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("arrow")
# import("cancensus")
# import("data.table")
import("dplyr")
import("shiny")
import("shinydashboard")
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

ano <- arrow::read_parquet("./data/Anomaly.parquet", as_data_frame = FALSE)

# Data time range ----
app_time_range <- paste(min(ano %>% select(Birth_Year) %>% collect(), na.rm = TRUE),
                        "-",
                        max(ano %>% select(Birth_Year) %>% collect(), na.rm = TRUE))

# ICD10 labels for selectInput() ----

icd_lbl <- list(
 ## All conditions ----
 c(ano %>% filter(grepl("^(Q999)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull()
  ),
 ## Neural tube defects ----
 c(ano %>% filter(grepl("^(QG101)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q00|Q01|Q05)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
   ),
 ## Selected central nervous system defects ----
 c(ano %>% filter(grepl("^(QG102)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q02|Q03|Q041|Q042)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Selected central nervous system defects ----
 c(ano %>% filter(grepl("^(QG103)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>%
    filter(grepl("^(Q110|Q111|Q112|Q160|Q172|Q300)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Selected congenital heart defects ----
 c(ano %>% filter(grepl("^(QG104)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>%
    filter(grepl("^(Q200|Q201|Q203|Q205|Q212|Q213|Q234|Q251)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Oro-facial clefts ----
 c(ano %>% filter(grepl("^(QG105)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q35|Q36|Q37)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Selected gastrointestinal defects ----
 c(ano %>% filter(grepl("^(QG106)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q39[0-4]|Q41|Q42[0-3]|Q431|Q442)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Selected genital anomalies ----
 c(ano %>% filter(grepl("^(QG107)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q53[1-2]|Q539|Q54|Q56|Q640)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Selected urinary tract defects ----
 c(ano %>% filter(grepl("^(QG108)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q60[0-2]|Q61[1-5]|Q61[8-9]|Q64[1-3])", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Hip dysplasia ----
 c(ano %>% filter(grepl("^(QG109)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q65)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Limb deficiency defects ----
 c(ano %>% filter(grepl("^(QG110)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q71[4-9]|Q72[4-9]|Q738)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Selected abdominal wall defects ----
 c(ano %>% filter(grepl("^(QG111)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q79[2-3])", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 ),
 ## Selected chromosomal defects ----
 c(ano %>% filter(grepl("^(QG112)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
   ano %>% filter(grepl("^(Q90|Q91[4-7]|Q91[0-3]|Q96)", Diags)) %>% select(cat) %>% distinct() %>% mutate(cat = stringr::str_extract(cat, "(?<=-\\s).*")) %>% collect() %>% pull()
 )
)

# List name ----
## The outermost names will be used as the group labels

names(icd_lbl) <- c(
 ano %>% filter(grepl("^(Q999)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG101)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG102)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG103)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG104)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG105)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG106)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG107)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG108)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG109)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG110)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG111)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull(),
 ano %>% filter(grepl("^(QG112)", Diags)) %>% select(cat) %>% distinct() %>% collect() %>% pull()
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
 href = rcp_website,
 target = "_blank",
 rel = "nofollow noreferrer",
 class = "logo-link",
 img(src = "www/images/rcp-logo-transparent.svg",
     class = "logo-img",
     alt = "RCP Logo")
)

iwk_logo <- tags$a(
 href = rcp_website,
 target = "_blank",
 rel = "nofollow noreferrer",
 class = "logo-link",
 img(src = "www/images/iwk-logo-transparent.svg",
     class = "logo-img",
     alt = "IWK Logo")
)

# Page footer ----

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

