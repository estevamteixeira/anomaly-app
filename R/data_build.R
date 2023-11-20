# This script is used to create the database used in the app
# In the app, we need information regarding
#    - Births
#    - Anomalies
#    - Risk factors
#    - Shape files

# Births ----
# births and risk factors come from the Atlee database
# all the info is stored in the Monster SAS file

library(arrow)
library(dplyr)
library(haven)
library(stringr)
library(tidyr)

#-----------
# NSAPD ----
#-----------

## Create a vector to get only the fields that are needed ----

my_vec <-c(
 # IDs
 "BIRTHID", "MOTHERID", "CONTCTID",
 # PCODE, birth date, year of birth, sex, birth outcome
 "DLPSTCOD","BTBrthDT","BrthYear","Conception_Date","DLMBDate",
 "BTSEX","BTOUTCOM","GA_BEST",
 # birth order, death date, birth weight
 "BTBRTHOR","BTDethDT","BIRTHWT",
 # Alcohol use, cannabis use, maternal age
 "Alcohol_Use","Cannabis_Use","DMMATAGE",
 # Smoking
 "ADMITSMK", "DLVS1SMK", "SMOKE_20", "DLPRESMK",
 # pre-pregnancy diabetes
 "R014_00900","R014_01000","R014_01100","R014_01200",
 "MO240","MO241","MO242","MO243","MO245","MO24501","MO24502","MO24504",
 "MO246","MO24601","MO24602","MO24604","MO247","MO24701","MO24704",
 "ME10","ME11","ME13","ME14","MH360","MG632","IP701",
 # gestational diabetes
 "R014_01300","R014_01400","MO244","MO24401","MO24404","MO24409",
 "MO248","MO24801","MO24802","MO24803","MO24804","MO249","IP700",
 # BMI
 "PrePBMI"
)

## Get the newest version of the monster file ----

monster <- haven::read_sas("H:\\RCP\\RCP_Data\\TeixeiEC\\Monster\\Monster.sas7bdat",
                           col_select = my_vec)

## Run the PCCF to update the geographies for mapping ----
source("H:/RCP/RCP_Data/TeixeiEC/PCCF/2023/PCCF8A1/PCCFplus_8A1.R")

pcode <- pccf_r(infile = monster %>%
                 select(BIRTHID, CONTCTID, DLPSTCOD, BrthYear))

## Add some geographies of interest to the monster file ----
### re-formatting some variables of interest for analysis ----
dta <- merge(
 monster,
 pcode %>%
  select(ID, PCODE, PR, DAuid, CSDuid, HRuid, HRename),
 by.x = c("BIRTHID", "DLPSTCOD"),
 by.y = c("ID","PCODE"),
 all = TRUE
) %>%
 distinct(BIRTHID, .keep_all = TRUE) %>%
 mutate(CDuid = substr(CSDuid, 1, 4),
        BTBrthDT = as.Date(BTBrthDT, format = "%Y-%m-%d"),
        BTDethDT = as.Date(BTDethDT, format = "%Y-%m-%d"),
        Conception_Date = as.Date(Conception_Date, format = "%Y-%m-%d"),
        DLMBDate = as.Date(DLMBDate, format = "%Y-%m-%d"),
        idx_244 = rowSums(select(., starts_with("MO2440")), na.rm = TRUE),
        idx_245 = rowSums(select(., starts_with("MO2450")), na.rm = TRUE),
        idx_246 = rowSums(select(., starts_with("MO2460")), na.rm = TRUE),
        idx_247 = rowSums(select(., starts_with("MO2470")), na.rm = TRUE),
        idx_248 = rowSums(select(., starts_with("MO2480")), na.rm = TRUE),
        # smk = rowSums(select("ADMITSMK", "DLVS1SMK", "SMOKE_20"), na.rm = TRUE),
        predm_nsapd = case_when(
         R014_00900 >=1 | R014_01000 >=1 |
          R014_01100 >=1 | R014_01200 >=1 ~ 1,
         is.na(R014_00900) & is.na(R014_01000) &
          is.na(R014_01100) & is.na(R014_01200) ~ -1,
         TRUE ~ 0
        ),
        predm_icd = case_when(
         MO240 >=1 | MO241 >=1 | MO242 >=1 | MO243 >=1 |
          MO245 >=1 | idx_245 >=1 |
          MO246 >=1 | idx_246 >=1 |
          MO247 >=1 | idx_247 >=1 |
          ME10 >=1 | ME11 >=1 | ME13 >=1 | ME14 >=1 |
          MH360 >=1 | MG632 >=1 | IP701  >=1 ~ 1,
         is.na(MO240) & is.na(MO241) & is.na(MO242) & is.na(MO243) &
          is.na(MO245) & is.na(idx_245) & is.na(MO246) & is.na(idx_246) &
          is.na(MO247) & is.na(idx_247) & is.na(ME10) & is.na(ME11) &
          is.na(ME13) & is.na(ME14) & is.na(MH360) & is.na(MG632) & is.na(IP701) ~ -1,
         TRUE ~ 0
        ),
        gdm_nsapd = case_when(
         R014_01300 >=1 | R014_01400 >=1 ~ 1,
         is.na(R014_01300) & is.na(R014_01400) ~ -1,
         TRUE ~ 0
        ),
        gdm_icd = case_when(
         MO244 >=1 | MO249 >=1 | MO248 >=1 |
          idx_244 >=1 | idx_248 >=1 |
          IP700  >=1 ~ 1,
         is.na(MO244) & is.na(MO249) & is.na(MO248) &
          is.na(idx_244) & is.na(idx_248) & is.na(IP700) ~ -1,
         TRUE ~ 0
        ),
        # pre-existing diabetes
        prexdiab = case_when(
         predm_nsapd %in% 1 | predm_icd %in% 1 ~ 1,
         is.na(predm_nsapd) & is.na(predm_icd) ~ -1,
         TRUE ~ 0
        ),
        # gestational diabetes
        gestdiab = case_when(
         gdm_nsapd %in% 1 | gdm_icd %in% 1 ~ 1,
         is.na(gdm_nsapd) & is.na(gdm_icd) ~ -1,
         TRUE ~ 0
        ),
        # diabetes
        diab = case_when(
         prexdiab >=1 | gestdiab >=1 ~ 1,
         is.na(prexdiab) & is.na(gestdiab) ~ -1,
         TRUE ~ 0
        ),
        # BMI
        bmipp = case_when(
         PrePBMI < 30 ~ 1,
         30 <= PrePBMI & PrePBMI < 40 ~ 2,
         PrePBMI >= 40 ~ 3,
         is.na(PrePBMI) ~ -1,
         TRUE ~ 0
        ),
        # Smoking
        smoker = case_when(
         ADMITSMK >= 1 | DLVS1SMK >=1 | SMOKE_20 >= 1 ~ 1,
         is.na(ADMITSMK) & is.na(DLVS1SMK) & is.na(DLPRESMK) & is.na(SMOKE_20) ~ -1,
         TRUE ~ 0
        ),
        matage = case_when(
         DMMATAGE < 35 ~ 1,
         DMMATAGE >= 35 ~ 2,
         is.na(DMMATAGE) ~ -1,
         TRUE ~ 0
        )) %>%
 select(BIRTHID,CONTCTID,MOTHERID,BTBrthDT,DLMBDate,
        BrthYear,BTOUTCOM,BTBRTHOR,BTDethDT,BIRTHWT,GA_BEST,
        DLPSTCOD,DAuid,CSDuid,CDuid,HRuid,HRename,
        BTSEX, Alcohol_Use, Cannabis_Use, diab, bmipp, smoker, matage) %>%
 arrange(BTBrthDT, DLMBDate)

#-------------
# ANOMALY ----
#-------------

## Import dataset ----
ano <- read_csv("H:\\RCP\\RCP_Data\\TeixeiEC\\Anomalies\\Data\\XPort23V1.csv")

## Convert date fields to date format ----
ano <- ano %>%
  mutate(
    Birth_Date = as.Date(as.character(stringr::str_pad(Birth_Date, 8, pad = "0")), format = "%d%m%Y"),
    Mat_DoB = as.Date(as.character(stringr::str_pad(Mat_DoB, 8, pad = "0")), format = "%d%m%Y"),
    DeathD8 = as.Date(as.character(stringr::str_pad(DeathD8, 8, pad = "0")), format = "%d%m%Y")
    )

## Transform all fields from ADiags-APrfxs to character ----
ano[,c(16:ncol(ano))] <- sapply(ano[,c(16:ncol(ano))], as.character)

## Transform the data from wide to long format ----

ano_long <- ano %>%
  tidyr::pivot_longer(cols = ADiags1:APrfxs67,
                      names_to = c("set",".value"),
                      names_pattern = "(.)([[:alpha:]]+)") %>%
  ### Keep only registries that starts with Q, not blank or missing ----
  ### Keep only birth dates that can be directly related in the NSAPD ----
  ### Keep only NS residents
  filter(startsWith(toupper(Diags), "Q"),
         ### remove Q544 = Congenital chordee ----
         ### and Q357 = Cleft uvula ----
         !tolower(Diags) %in% c("q544","q357"),
         Birth_Date >= min(dta$BTBrthDT, na.rm = TRUE)
         #Prov_Res %in% 12
         ) %>%
  mutate(
    ### Create categories to be used in the dashboard ----
    ### Grouping anomalies ----
    #### Blocks ----
    cat_tier1 = dplyr::case_when(
      grepl("^Q00|Q01|Q02|Q03|Q04|Q05|Q06|Q07", Diags) ~ "Congenital malformations of the nervous system",
      grepl("^Q10|Q11|Q12|Q13|Q14|Q15|Q16|Q17|Q18", Diags) ~ "Congenital malformations of eye, ear, face and neck",
      grepl("^Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28", Diags) ~ "Congenital malformations of the circulatory system",
      grepl("^Q30|Q31|Q32|Q33|Q34", Diags) ~ "Congenital malformations of the respiratory system",
      grepl("^Q35|Q36|Q37", Diags) ~ "Cleft lip and cleft palate",
      grepl("^Q38|Q39|Q40|Q41|Q42|Q43|Q44|Q45", Diags) ~ "Other congenital malformations of the digestive system",
      grepl("^Q50|Q51|Q52|Q53|Q54|Q55|Q56", Diags) ~ "Congenital malformations of genital organs",
      grepl("^Q60|Q61|Q62|Q63|Q64", Diags) ~ "Congenital malformations of the urinary system",
      grepl("^Q65|Q66|Q67|Q68|Q69|Q70|Q71|Q72|Q73|Q74|Q75|Q76|Q77|Q78|Q79", Diags) ~ "Congenital malformations and deformations of the musculoskeletal system",
      grepl("^Q80|Q81|Q82|Q83|Q84|Q85|Q86|Q87|Q89", Diags) ~ "Other congenital malformations",
      grepl("^Q90|Q91|Q92|Q93|Q95|Q96|Q97|Q98|Q99", Diags) ~ "Chromosomal abnormalities, not elsewhere classified"
    ),
    #### Category ----
    cat_tier2 = dplyr::case_when(
      grepl("^Q00", Diags) ~ "Anencephaly and similar malformations",
      grepl("^Q01", Diags) ~ "Encephalocele",
      grepl("^Q02", Diags) ~ "Microcephaly",
      grepl("^Q03", Diags) ~ "Congenital hydrocephalus",
      grepl("^Q04", Diags) ~ "Other congenital malformations of brain",
      grepl("^Q05", Diags) ~ "Spina bifida",
      grepl("^Q06", Diags) ~ "Other congenital malformations of spinal cord",
      grepl("^Q07", Diags) ~ "Other congenital malformations of nervous system",
      grepl("^Q10", Diags) ~ "Congenital malformations of eyelid, lacrimal apparatus and orbit",
      grepl("^Q11", Diags) ~ "Anophtalmos, microphtalmos and macrophtalmos",
      grepl("^Q12", Diags) ~ "Congenital lens malformations",
      grepl("^Q13", Diags) ~ "Congenital malformations of anterior segment of eye",
      grepl("^Q14", Diags) ~ "Congenital malformations of posterior segment of eye",
      grepl("^Q15", Diags) ~ "Other congenital malformations of eye",
      grepl("^Q16", Diags) ~ "Congenital malformations of ear causing impairment of hearing",
      grepl("^Q17", Diags) ~ "Other congenital malformations of ear",
      grepl("^Q18", Diags) ~ "Other congenital malformations of face and neck",
      grepl("^Q20", Diags) ~ "Congenital malformations of cardiac chambers and connections",
      grepl("^Q21", Diags) ~ "Congenital malformations of cardiac septa",
      grepl("^Q22", Diags) ~ "Congenital malformations of pulmonary and tricuspid valves",
      grepl("^Q23", Diags) ~ "Congenital malformations of aortic and mitral valves",
      grepl("^Q24", Diags) ~ "Other congenital malformations of heart",
      grepl("^Q25", Diags) ~ "Congenital malformations of great arteries",
      grepl("^Q26", Diags) ~ "Congenital malformations of great veins",
      grepl("^Q27", Diags) ~ "Other congenital malformations of peripheral vascular system",
      grepl("^Q28", Diags) ~ "Other congenital malformations of circulatory system",
      grepl("^Q30", Diags) ~ "Congenital malformations of nose",
      grepl("^Q31", Diags) ~ "Congenital malformations of larynx",
      grepl("^Q32", Diags) ~ "Congenital malformations of trachea and bronchus",
      grepl("^Q33", Diags) ~ "Congenital malformations of lung",
      grepl("^Q34", Diags) ~ "Other congenital malformations of respiratory system",
      grepl("^Q35", Diags) ~ "Cleft palate only",
      grepl("^Q36", Diags) ~ "Cleft lip only",
      grepl("^Q37", Diags) ~ "Cleft palate with cleft lip",
      grepl("^Q38", Diags) ~ "Other congenital malformations of tongue, mouth and pharynx",
      grepl("^Q39", Diags) ~ "Congenital malformations of oesophagus",
      grepl("^Q40", Diags) ~ "Other congenital malformations of upper alimentary tract",
      grepl("^Q41", Diags) ~ "Congenital absence, atresia and stenosis of small intestine",
      grepl("^Q42", Diags) ~ "Congenital absence, atresia and stenosis of large intestine",
      grepl("^Q43", Diags) ~ "Other congenital malformations of intestine",
      grepl("^Q44", Diags) ~ "Congenital malformations of gallblader, bile ducts and liver",
      grepl("^Q45", Diags) ~ "Other congenital malformations of digestive system",
      grepl("^Q50", Diags) ~ "Congenital malformations of ovaries, fallopian tubes and broad ligaments",
      grepl("^Q51", Diags) ~ "Congenital malformations of uterus",
      grepl("^Q52", Diags) ~ "Other congenital malformations of female genitalia",
      grepl("^Q53", Diags) ~ "Undescended testicle",
      grepl("^Q54", Diags) ~ "Hypospadias",
      grepl("^Q55", Diags) ~ "Other congenital malformations of male genital organs",
      grepl("^Q56", Diags) ~ "Indeterminate sex and pseudohermaphroditism",
      grepl("^Q60", Diags) ~ "Renal agenesis and other reduction defects of kidney",
      grepl("^Q61", Diags) ~ "Cystic kidney disease",
      grepl("^Q62", Diags) ~ "Congenital obstructive defects of renal pelvis and congenital malformations of ureter",
      grepl("^Q63", Diags) ~ "Other congenital malformations of kidney",
      grepl("^Q64", Diags) ~ "Other congenital malformations of urinary system",
      grepl("^Q65", Diags) ~ "Congenital deformities of hip",
      grepl("^Q66", Diags) ~ "Congenital deformities of feet",
      grepl("^Q67", Diags) ~ "Congenital musculoskeletal deformities of head, face, spine and chest",
      grepl("^Q68", Diags) ~ "Other congenital musculoskeletal deformities",
      grepl("^Q69", Diags) ~ "Polydactyly",
      grepl("^Q70", Diags) ~ "Syndactyly",
      grepl("^Q71", Diags) ~ "Reduction defects of upper limb",
      grepl("^Q72", Diags) ~ "Reduction defects of lower limb",
      grepl("^Q73", Diags) ~ "Reduction defects of unspecified limb",
      grepl("^Q74", Diags) ~ "Other congenital malformations of limb(s)",
      grepl("^Q75", Diags) ~ "Other congenital malformations of skull and face bones",
      grepl("^Q76", Diags) ~ "Congenital malformations of spine and bony torax",
      grepl("^Q77", Diags) ~ "Osteochondrodysplasia with defects of growns of tubular bones and spine",
      grepl("^Q78", Diags) ~ "Other osteochondrodysplasias",
      grepl("^Q79", Diags) ~ "Congenital malformations of the musculoskeletal system, not elsewhere classified",
      grepl("^Q80", Diags) ~ "Congenital ichthyosis",
      grepl("^Q81", Diags) ~ "Epidermolysis bullosa",
      grepl("^Q82", Diags) ~ "Other congenital malformations of skin",
      grepl("^Q83", Diags) ~ "Congenital malformations of breast",
      grepl("^Q84", Diags) ~ "Other congenital malformations of integument",
      grepl("^Q85", Diags) ~ "Phakomatoses, not elsewhere classified",
      grepl("^Q86", Diags) ~ "Congenital malformation syndromes due to known exogenous causesm not elsewhere classified",
      grepl("^Q87", Diags) ~ "Other specified congenital malformation syndromes affecting multiple systems",
      grepl("^Q89", Diags) ~ "Other congenital malformations, not elsewhere classified",
      grepl("^Q90", Diags) ~ "Down's syndrome",
      grepl("^Q91", Diags) ~ "Edwards' syndrome and Patau's syndrome",
      grepl("^Q92", Diags) ~ "Other trisomies and partial trisomies of the autosomes, not elsewhere classified",
      grepl("^Q93", Diags) ~ "Monosomies and deletions from the autosomes, not elsewhere classified",
      grepl("^Q95", Diags) ~ "Balanced rearrangements and structural markers, not elsewhere classified",
      grepl("^Q96", Diags) ~ "Turner's syndrome",
      grepl("^Q97", Diags) ~ "Other sex chromosome abnormalities, female phenotype, not elsewhere classified",
      grepl("^Q98", Diags) ~ "Other sex chromosome abnormalities, male phenotype, not elsewhere classified",
      grepl("^Q99", Diags) ~ "Other chromosome abnormalities, not elsewhere classified"
    ),
    cat_tier2 = ifelse(nchar(Diags) > 3,
                       paste0("(",substr(Diags,1,3),")",
                              " - ",
                              cat_tier2),
                       paste0("(",Diags,")",
                              " - ",
                              cat_tier2)),
    #### Categories ----
    cat_tier3 = dplyr::case_when(
      grepl("^Q000$|Q0000$|Q01|Q05", Diags) ~ "Neural tube defects",
      grepl("^Q02|Q03|Q041|Q042", Diags) ~ "Selected central nervous system defects",
      grepl("^Q110|Q111|Q112|Q160|Q172|Q30", Diags) ~ "Selected sense organ defects",
      grepl("^Q200|Q201|Q205|Q203|Q212|Q213|Q234|Q251", Diags) ~ "Selected congenital heart defects",
      grepl("^Q35|Q36|Q37", Diags) ~ "Oro-facial clefts",
      grepl("^Q39[0-4]|Q41|Q42[0-3]|Q431|Q442", Diags) ~ "Selected gastrointestinal defects",
      grepl("^Q53[1-2]|Q539|Q54|Q56|Q640", Diags) ~ "Selected genital anomalies",
      grepl("^Q60[0-2]|Q61[1-5]|Q61[8-9]|Q64[1-3]", Diags) ~ "Selected urinary tract defects",
      grepl("^Q65", Diags) ~ "Hip dysplasia",
      grepl("^Q71[4-9]|Q72[4-9]|Q738", Diags) ~ "Limb deficiency defects",
      grepl("^Q79[2-3]", Diags) ~ "Selected abdominal wall defects",
      grepl("^Q90|Q91[4-7]|Q91[0-3]|Q96", Diags) ~ "Selected chromosomal defects",
      TRUE ~ NA_character_
    ),
    #### Categories ----
    cat_tier4 = dplyr::case_when(
      grepl("^Q000$|Q0000$", Diags) ~ "(Q00) - Anencephaly and similar malformations",
      grepl("^Q01", Diags) ~ "(Q01) - Encephalocele",
      grepl("^Q05", Diags) ~ "(Q05) - Spina bifida",
      grepl("^Q02", Diags) ~ "(Q02) - Microcephaly",
      grepl("^Q03", Diags) ~ "(Q03) - Congenital hydrocephalus",
      grepl("^Q041|Q042", Diags) ~ "(Q04.1, Q04.2) - Arhinencephaly / Holoprosencephaly",
      grepl("^Q110|Q110$|Q111|Q112", Diags) ~ "(Q11.0-Q11.2) - Anophtalmos / Microphtalmos",
      grepl("^Q160|Q172", Diags) ~ "(Q16.0, Q17.2) - Anotia / Microtia",
      grepl("^Q30", Diags) ~ "(Q30) - Choanal atresia",
      grepl("^Q200|Q200$", Diags) ~ "(Q20.0) - Commom truncus",
      grepl("^Q203|Q201|Q205", Diags) ~ "(Q20.1, Q20.3, Q20.5) - Transposition of great vessels",
      grepl("^Q212", Diags) ~ "(Q21.2) - Atrioventricular septal defect",
      grepl("^Q213", Diags) ~ "(Q21.3) - Tetralogy of Fallot",
      grepl("^Q234", Diags) ~ "(Q23.4) - Hypoplastic left heart syndrome",
      grepl("^Q251", Diags) ~ "(Q25.1) - Coarctation of aorta",
      grepl("^Q35", Diags) ~ "(Q35) - Cleft palate only",
      grepl("^Q36", Diags) ~ "(Q36) - Cleft lip only",
      grepl("^Q37", Diags) ~ "(Q37) - Cleft palate with cleft lip",
      grepl("^Q39[0-4]", Diags) ~ "(Q39.0-Q39.4) - Oesophageal atresia / stenosis, tracheoesophageal fistula",
      grepl("^Q41", Diags) ~ "(Q41) - Small intestine absence / atresia / stenosis",
      grepl("^Q42[0-3]", Diags) ~ "(Q42.0-Q42.3) - Ano-rectal absence / atresia / stenosis",
      grepl("^Q431", Diags) ~ "(Q43.1) - Hirschsprung disease",
      grepl("^Q442", Diags) ~ "(Q44.2) - Atresia of bile ducts",
      grepl("^Q53[1-2]|Q539", Diags) ~ "(Q53.1, Q53.2, Q53.9) - Cryptorchidism / undescended testicles",
      grepl("^Q54", Diags) ~ "(Q54, excluding Q54.4) - Hypospadias",
      grepl("^Q56", Diags) ~ "(Q56) - Indeterminate sex",
      grepl("^Q640", Diags) ~ "(Q64.0) - Epispadias",
      grepl("^Q60[0-2]", Diags) ~ "(Q60.0-Q60.2) - Renal agenesis",
      grepl("^Q61[1-5]|Q61[8-9]", Diags) ~ "(Q61.1-Q61.5, Q61.8, Q61.9) - Cystic kidney",
      grepl("^Q641", Diags) ~ "(Q64.1) - Bladder and cloacal exstrophy",
      grepl("^Q64[2-3]", Diags) ~ "(Q64.2, Q64.3) - Lower urinary tract obstruction",
      grepl("^Q65", Diags) ~ "(Q65) - Hip dysplasia",
      grepl("^Q71[4-9]|Q72[4-9]|Q738", Diags) ~ "(Q71.4-Q71.9, Q72.4-Q72.9, Q73.8, excluding Q71.6, Q71.7, Q72.7) - Limb deficiency defects",
      grepl("^Q792", Diags) ~ "(Q79.2) - Omphalocele / Exomphalos",
      grepl("^Q793", Diags) ~ "(Q79.3) - Gastroschisis",
      grepl("^Q90", Diags) ~ "(Q90) - Down Syndrome",
      grepl("^Q91[4-7]", Diags) ~ "(Q91.4-Q91.7) - Trisomy 13 - Patau",
      grepl("^Q91[0-3]", Diags) ~ "(Q91.0-Q91.3) - Trisomy 18 - Edwards",
      grepl("^Q96", Diags) ~ "(Q96) - Turner syndrome",
      TRUE ~ NA_character_
    ),
    SexNum = dplyr::case_when(
      SexNum %in% 1 ~ "M",
      SexNum %in% 2 ~ "F",
      SexNum %in% 3 ~ "A",
      TRUE ~ NA_character_
    ),
    #### Fix postal codes with 7 digits ----
    #### Keep first 6 digits
    Post_Code = ifelse(
      nchar(Post_Code) %in% 7,
      substr(Post_Code, 1, 6),
      Post_Code),
    Birth_Date = as.Date(Birth_Date, format = "%Y-%m-%d"),
    inAno = row_number()
  ) %>%
  select(inAno,CaseID,Post_Code,Prov_Birth,Prov_Res,
         Birth_Date,Mat_DoB,DeathD8,SexNum,
         OutcomeNum,ToP,NumFet,BWNum,GANum,
         Diags,RowIDs,SrceIDs,Prfxs,
         cat_tier1,cat_tier2,cat_tier3,cat_tier4)

# Merge Anomaly and Atlee ----
## 1st run ----

tmp_1 <- merge(
  ano_long,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c("Post_Code","Birth_Date","Mat_DoB","SexNum","BWNum"),
  by.y = c("DLPSTCOD","BTBrthDT","DLMBDate","BTSEX","BIRTHWT"),
  all = TRUE
) %>%
  distinct(inAno, .keep_all = TRUE) %>%
  filter(!is.na(inAno))

### Not found on the 1st run ----

tmp_1_miss <- tmp_1 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 1st run ----

tmp_1_hit <- tmp_1 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 1)

## 2nd run: if maternal birth date is inconsistent ----

tmp_2 <- merge(
  tmp_1_miss,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c("Post_Code","Birth_Date",#"Mat_DoB",
           "SexNum","BWNum"),
  by.y = c("DLPSTCOD","BTBrthDT",#"DLMBDate",
           "BTSEX","BIRTHWT"),
  all = TRUE
) %>%
  distinct(inAno, .keep_all = TRUE) %>%
  filter(!is.na(inAno))

### Not found on the 2nd run ----

tmp_2_miss <- tmp_2 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 2nd run ----

tmp_2_hit <- tmp_2 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 2)

## 3rd run: if phenotypic sex is inconsistent ----

tmp_3 <- merge(
  tmp_2_miss,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c("Post_Code","Birth_Date","Mat_DoB",#"SexNum",
           "BWNum"),
  by.y = c("DLPSTCOD","BTBrthDT","DLMBDate",#"BTSEX",
           "BIRTHWT"),
  all = TRUE
) %>%
  distinct(inAno, .keep_all = TRUE) %>%
  filter(!is.na(inAno))

### Not found on the 3d run ----

tmp_3_miss <- tmp_3 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 3rd run ----

tmp_3_hit <- tmp_3 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 3)

## 4th run: if birth weight is inconsistent ----

tmp_4 <- merge(
  tmp_3_miss,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c("Post_Code","Birth_Date","Mat_DoB","SexNum"#,"BWNum"
           ),
  by.y = c("DLPSTCOD","BTBrthDT","DLMBDate","BTSEX"#,"BIRTHWT"
           ),
  all = TRUE
) %>%
  distinct(inAno, .keep_all = TRUE) %>%
  filter(!is.na(inAno))

### Not found on the 4th run ----

tmp_4_miss <- tmp_4 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 4th run ----

tmp_4_hit <- tmp_4 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 4)

## 5th run: if postal code is inconsistent ----

tmp_5 <- merge(
  tmp_4_miss,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c(#"Post_Code",
    "Birth_Date","Mat_DoB","SexNum","BWNum"),
  by.y = c(#"DLPSTCOD",
    "BTBrthDT","DLMBDate","BTSEX","BIRTHWT"),
  all = TRUE
) %>%
  distinct(inAno, .keep_all = TRUE) %>%
  filter(!is.na(inAno))

### Not found on the 5th run ----

tmp_5_miss <- tmp_5 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 5th run ----

tmp_5_hit <- tmp_5 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 5)

## 6th run: if birth date is inconsistent ----

tmp_6 <- merge(
  tmp_5_miss,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c("Post_Code",#"Birth_Date",
           "Mat_DoB","SexNum","BWNum"),
  by.y = c("DLPSTCOD",#"BTBrthDT",
    "DLMBDate","BTSEX","BIRTHWT"),
  all = TRUE
) %>%
  distinct(inAno, .keep_all = TRUE) %>%
  filter(!is.na(inAno))

### Not found on the 6th run ----

tmp_6_miss <- tmp_6 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 6th run ----

tmp_6_hit <- tmp_6 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 6)

## 7th run: if birth date and birth weight are inconsistent ----

tmp_7 <- merge(
  tmp_6_miss,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c("Post_Code",#"Birth_Date",
           "Mat_DoB","SexNum"#,"BWNum"
           ),
  by.y = c("DLPSTCOD",#"BTBrthDT",
           "DLMBDate","BTSEX"#,"BIRTHWT"
           ),
  all = TRUE
) %>%
  distinct(inAno, .keep_all = TRUE) %>%
  filter(!is.na(inAno))

### Not found on the 7th run ----

tmp_7_miss <- tmp_7 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 7th run ----

tmp_7_hit <- tmp_7 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 7)

## 8th run: if birth date and postal code are consistent ----

tmp_8 <- merge(
  tmp_7_miss,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c("Post_Code","Birth_Date"#,"Mat_DoB","SexNum","BWNum"
  ),
  by.y = c("DLPSTCOD","BTBrthDT"#,"DLMBDate","BTSEX","BIRTHWT"
  ),
  all = TRUE
) %>%
  distinct(inAno, .keep_all = TRUE) %>%
  filter(!is.na(inAno))

### Not found on the 8th run ----

tmp_8_miss <- tmp_8 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 8th run ----

tmp_8_hit <- tmp_8 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 8)

## 9th run: if postal code and maternal birth date are consistent ----

tmp_9 <- merge(
  tmp_8_miss,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c("Post_Code",#"Birth_Date",
           "Mat_DoB"#,"SexNum","BWNum"
  ),
  by.y = c("DLPSTCOD",#"BTBrthDT",
           "DLMBDate"#,"BTSEX","BIRTHWT"
  ),
  all = TRUE
) %>%
  filter(!is.na(inAno)) %>%
  distinct(inAno, .keep_all = TRUE)


### Not found on the 9th run ----

tmp_9_miss <- tmp_9 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 9th run ----

tmp_9_hit <- tmp_9 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 9)

## 10th run: if birth date and maternal birth date are consistent ----

tmp_10 <- merge(
  tmp_9_miss,
  dta %>%
    mutate(inDta = row_number()),
  by.x = c(#"Post_Code",
    "Birth_Date","Mat_DoB"#,"SexNum","BWNum"
  ),
  by.y = c(#"DLPSTCOD",
    "BTBrthDT","DLMBDate"#,"BTSEX","BIRTHWT"
  ),
  all = TRUE
) %>%
  filter(!is.na(inAno)) %>%
  distinct(inAno, .keep_all = TRUE)


### Not found on the 10th run ----

tmp_10_miss <- tmp_10 %>%
  filter(is.na(inDta)) %>%
  select(names(ano_long))

### Found on the 10th run ----

tmp_10_hit <- tmp_10 %>%
  filter(!is.na(inDta)) %>%
  mutate(Link_Source = 10)

tmp_ano <- bind_rows(
  tmp_1_hit,
  tmp_2_hit,
  tmp_3_hit,
  tmp_4_hit,
  tmp_5_hit,
  tmp_6_hit,
  tmp_7_hit,
  tmp_8_hit,
  tmp_9_hit,
  tmp_10_hit,
  tmp_10_miss
) %>%
  mutate(
    Post_Code = ifelse(DLPSTCOD %in% c("", " ", NA),
                       Post_Code,
                       DLPSTCOD),
    Birth_Date = ifelse(BTBrthDT %in% c("", " ", NA),
                        Birth_Date,
                        BTBrthDT),
    Birth_Date = as.Date(Birth_Date, format = "%Y-%m-%d", origin = "1970-01-01"),
    Birth_Year = as.integer(substr(Birth_Date, 1, 4)),
    DeathD8 = ifelse(DeathD8 %in% c("", " ", NA),
                     DeathD8,
                     BTDethDT),
    DeathD8 = as.Date(DeathD8, format = "%Y-%m-%d", origin = "1970-01-01"),
    Mat_DoB = ifelse(Mat_DoB %in% c("", " ", NA),
                     Mat_DoB,
                     DLMBDate),
    Mat_DoB = as.Date(Mat_DoB, format = "%Y-%m-%d", origin = "1970-01-01"),
    SexNum = ifelse(BTSEX %in% c("", " ", NA),
                    SexNum,
                    BTSEX),
    BWNum = ifelse(BIRTHWT %in% c("", " ", NA),
                   BWNum,
                   BIRTHWT),
    GANum = ifelse(GA_BEST %in% c("", " ", NA),
                   GANum,
                   GA_BEST)
  ) %>%
  select("CaseID","BIRTHID","CONTCTID","MOTHERID", # IDs
         "Post_Code","Prov_Birth", "Prov_Res", # address
         "DAuid", "CSDuid", "CDuid", "HRuid", "HRename",
         "Birth_Date","Birth_Year","Mat_DoB","DeathD8", # dates
         "Alcohol_Use","Cannabis_Use","diab","bmipp","smoker", # risk factors
         "matage","Link_Source",
         "Diags","cat_tier1", "cat_tier2", "cat_tier3", "cat_tier4", # condition
         "SrceIDs","BTOUTCOM","SexNum","BWNum","GANum"
  ) %>%
  arrange(CaseID) %>%
  ## keep only NS postal codes for map building
  ## keep only those matched with the Atlee
  ## keep CDuid not equal to 1200
  filter(substr(Post_Code,1,1) %in% "B",
         !is.na(BIRTHID),
         !CDuid %in% c(1200, NA)) %>%
  ## filling in missing information based on DAuid
  group_by(DAuid) %>%
    tidyr::fill(c(CSDuid,CDuid,HRuid,HRename),
                .direction = "downup") %>%
  ungroup() %>%
  ## filling in missing information based on 7-digits DAuid
  group_by(substr(DAuid,1,7)) %>%
  tidyr::fill(c(CSDuid,CDuid,HRuid,HRename),
              .direction = "downup") %>%
  ungroup() %>%
  ## filling in missing information based on CDuid
  group_by(CDuid) %>%
  tidyr::fill(c(CSDuid,CDuid,HRuid,HRename),
              .direction = "downup") %>%
  ungroup() %>%
  # fix HRuid to match '.shp' file
  mutate(
    HRuid = case_when(
      HRename %in% "Zone 1 - Western" ~ "01",
      HRename %in% "Zone 2 - Northern" ~ "02",
      HRename %in% "Zone 3 - Eastern" ~ "03",
      HRename %in% "Zone 4 - Central" ~ "04"
    ),
    CLuid = case_when(
      DAuid %in% c(
        12060176, 12060077, 12060078, 12060079, 12060080, 12060081, 12060082, 12060083, 12060084,
        12060085, 12060087, 12060088, 12060089, 12060090, 12060091, 12060092, 12060093, 12060094,
        12060095, 12060096, 12060097, 12060098, 12060099, 12060100, 12060170, 12060169, 12060171,
        12060172, 12060118, 12060119, 12060120, 12060186, 12060187, 12060188, 12060190, 12060124,
        12060125, 12060126, 12060184, 12060185, 12060128, 12060129, 12060130, 12060131, 12060132,
        12060133, 12060134, 12060160, 12060161, 12060162, 12060163, 12060168, 12060166, 12060167,
        12060181, 12060182, 12060183) ~ "010101",
      DAuid %in% c(
        12060071, 12060072, 12060073, 12060074, 12060075, 12060135, 12060136, 12060137, 12060138,
        12060139, 12060140, 12060141, 12060142, 12060143, 12060144, 12060145, 12060146, 12060147,
        12060148, 12060149, 12060150, 12060151, 12060152, 12060153, 12060154, 120906771) ~ "010102",
      DAuid %in% c(
        12040025, 12040026, 12040027, 12040028, 12040029, 12040030, 12040031, 12040032, 12040033,
        12040034, 12040035, 12040036, 12040037, 12040038, 12040039, 12040040, 12040041, 12040042,
        12040043, 12040044, 12040045, 12040046, 12040047, 12040048, 12040049, 12040051, 12040053) ~ "010103",
      DAuid %in% c(
        12060105, 12060106, 12060107, 12060108, 12060109, 12060110, 12060111, 12060112, 12060113,
        12060114, 12060115, 12060116, 12060156, 12060157, 12060158, 12060173, 12060174, 12060177,
        12060178, 12060179, 12060180) ~ "010104",
      DAuid %in% c(
        12030040, 12030041, 12030042, 12030043, 12030044, 12030045, 12030046, 12030047, 12030048,
        12030051, 12030052, 12030053, 12030054, 12030055, 12030056, 12030057, 12030058, 12030059,
        12030060, 12030061, 12030062, 12030063, 12030064, 12030065, 12030066, 12030067, 12030068,
        12030069, 12030070, 12030071, 12030072, 12030073, 12030074, 12030075, 12030076, 12030077,
        12030078, 12030079, 12030082, 12050078, 12050080, 12050085, 12050090, 12050093) ~ "010205",
      DAuid %in% c(
        12010030, 12010031, 12010032, 12010033, 12010034, 12010035, 12010036, 12010037, 12010038,
        12010039, 12010040, 12010041, 12010042, 12010043, 12010044, 12010045, 12010046, 12010047,
        12010048, 12010049, 12010050, 12010051, 12010052, 12010053, 12010054, 12010055, 12010056) ~ "010206",
      DAuid %in% c(
        12020043, 12020044, 12020045, 12020046, 12020047, 12020048, 12020049, 12020050, 12020051,
        12020052, 12020053, 12020054, 12020055, 12020056, 12020057, 12020058, 12020059, 12020060,
        12020061, 12020062, 12020063, 12020064, 12020065, 12020066, 12020067, 12020068, 12020069,
        12020070, 12020071, 12020072, 12020073, 12020074, 12020075, 12020076, 12020077, 12020078,
        12020079, 12020080, 12020081, 12020082, 12020083, 12020084, 12020085, 12020086, 12020087) ~ "010207",
      DAuid %in% c(
        12050069, 12050070, 12050071, 12050072, 12050073, 12050074, 12050075, 12050076, 12050077,
        12050082, 12050083, 12050084, 12050086, 12050087) ~ "010308",
      DAuid %in% c(
        12070131, 12070132, 12070133, 12070134, 12070135, 12070136, 12070137, 12070138, 12070139,
        12070140, 12070141, 12070142, 12070143, 12070144, 12070150, 12070171, 12070172, 12070173,
        12070187, 12070188, 12070189, 12070190, 12070191, 12070192, 12070193, 12070194, 12070195) ~ "010309",
      DAuid %in% c(
        12070088, 12070089, 12070090, 12070091, 12070092, 12070093, 12070094, 12070095, 12070096,
        12070097, 12070098, 12070099, 12070100, 12070101, 12070102, 12070103, 12070104, 12070105,
        12070106, 12070107, 12070108, 12070109, 12070110, 12070111, 12070112, 12070113, 12070114,
        12070115, 12070116, 12070117, 12070118, 12070119, 12070120, 12070121, 12070126, 12070127,
        12070128, 12070129, 12070130, 12070145, 12070146, 12070147, 12070148, 12070149) ~ "010310",
      DAuid %in% c(
        12050044, 12050045, 12050046, 12050047, 12050048, 12050049, 12050050, 12050051, 12050052,
        12050053, 12050054, 12050055, 12050056, 12050057, 12050058, 12050059, 12050060, 12050061,
        12050062, 12050063, 12050064, 12050065, 12050066, 12050067, 12050068, 12050088, 12050089,
        12070174, 12070175, 12070176, 12070177, 12070178, 12070179, 12070180, 12070181, 12070182,
        12070183, 12070184, 12070185, 12070186, 12070193) ~ "010311",
      DAuid %in% c(
        12070081, 12070082, 12070083, 12070084, 12070085, 12070086, 12070087, 12070122, 12070123,
        12070124, 12070125, 12070151, 12070152, 12070153, 12070154, 12070155, 12070156, 12070157,
        12070158, 12070159, 12070160, 12070161, 12070162, 12070163, 12070164, 12070165, 12070168,
        12070169, 12070170) ~ "010312",
      DAuid %in% c(
        12080063, 12080064, 12080065, 12080069, 12080070, 12080071, 12080072, 12080073, 12080074,
        12080123, 12080127, 12080128, 12080129, 12090596, 12090597, 12090598, 12090600) ~ "020413",
      DAuid %in% c(
        12100128, 12100129, 12100130, 12100131, 12100132, 12100133, 12100134, 12100135, 12100136,
        12100137) ~ "020414",
      DAuid %in% c(
        12080056, 12080076, 12080077, 12080078, 12080079, 12080080, 12080081, 12080082, 12080083,
        12080110, 12080111, 12080112, 12080113) ~ "020415",
      DAuid %in% c(
        12080056, 12080057, 12080058, 12080059, 12080130, 12080131, 12080061, 12080132, 12080133,
        12100152, 12100153, 12100154, 12100158, 12100159, 12100160, 12100161, 12100162, 12100163,
        12100164, 12100165, 12100166, 12100167, 12100168, 12100169, 12100170) ~ "020416",
      DAuid %in% c(
        12100082, 12100083, 12100084, 12100085, 12100086, 12100087, 12100088, 12100089, 12100090,
        12100091, 12100092, 12100093, 12100094, 12100095, 12100096, 12100097, 12100098, 12100099,
        12100100, 12100102, 12100103, 12100104, 12100105, 12100106, 12100107, 12100108, 12100109,
        12100110, 12100111, 12100112, 12100113, 12100114, 12100115, 12100116, 12100117, 12100118,
        12100119, 12100120, 12100121, 12100122, 12100123, 12100124, 12100125, 12100126, 12100127,
        12100138, 12100139, 12100140, 12100141, 12100142, 12100143, 12100144, 12100145, 12100146,
        12100147, 12100148, 12100149, 12100150, 12100151, 12100152, 12100153, 12100155, 12100156,
        12100157, 12100161) ~ "020417",
      DAuid %in% c(
        12110090, 12110091, 12110092, 12110093, 12110094, 12110095, 12110096, 12110097, 12110098,
        12110099, 12110100, 12110101, 12110102, 12110103, 12110104, 12110105, 12110106, 12110107,
        12110108, 12110109, 12110110, 12110111, 12110112, 12110113, 12110114, 12110115, 12110116) ~ "020518",
      DAuid %in% c(
        12100077, 12100078, 12100079, 12100080, 12100081, 12100101, 12110052, 12110053, 12110054,
        12110055, 12110056, 12110057, 12110058, 12110059, 12110060, 12110077, 12110078, 12110079) ~ "020519",
      DAuid %in% c(
        12110080, 12110081, 12110082, 12110083, 12110084, 12110085, 12110086, 12110087, 12110088) ~ "020520",
      DAuid %in% c(
        12110061, 12110062, 12110063, 12110064, 12110065, 12110066, 12110067, 12110068, 12110069,
        12110070, 12110071, 12110072, 12110073, 12110074, 12110075, 12110076, 12110089, 12110117,
        12110118) ~ "020521",
      DAuid %in% c(
        12120063, 12120064, 12120065, 12120066, 12120067, 12120068, 12120069, 12120070, 12120071,
        12120072, 12120073, 12120074, 12120075, 12120076, 12120077, 12120078, 12120079, 12120080,
        12120081, 12120082, 12120083, 12120084, 12120085, 12120086, 12120087, 12120088, 12120089,
        12120090, 12120091, 12120092, 12120093, 12120094, 12120095, 12120096, 12120097, 12120098,
        12120099, 12120100, 12120101, 12120102, 12120103, 12120104, 12120105, 12120106, 12120107,
        12120108, 12120109, 12120110, 12120111, 12120112, 12120113, 12120114, 12120115, 12120116,
        12120117, 12120118, 12120119, 12120120, 12120121, 12120139, 12120140, 12120141, 12120142,
        12120143, 12120144, 12120145, 12120146, 12120147, 12120148, 12120149, 12120150, 12120151,
        12120152, 12120153) ~ "020622",
      DAuid %in% c(
        12120122, 12120123, 12120124, 12120125, 12120126, 12120127, 12120128, 12120129, 12120130,
        12120131, 12120132, 12120133, 12120134, 12120135, 12120136, 12120137, 12120138, 12120154,
        12120155, 12120156, 12120157, 12120158) ~ "020623",
      DAuid %in% c(
        12140039, 12140040, 12140041, 12140042, 12140043, 12140044, 12140045, 12140046, 12140047,
        12140048, 12140049, 12140050, 12140051, 12140052, 12140053, 12140054, 12140055, 12140056,
        12140057, 12140058, 12140059, 12140060, 12140061, 12140062, 12140063, 12140064, 12140065,
        12140066, 12140067, 12140068, 12140069, 12140070, 12140071, 12140072, 12140073, 12140074,
        12140075, 12140076, 12140077, 12140078, 12140079, 12140080) ~ "030724",
      DAuid %in% c(
        12130021, 12130022, 12130023, 12130024, 12130025, 12130027, 12130032, 12130033, 12130034,
        12130035, 12130036) ~ "030725",
      DAuid %in% c(
        12130026, 12130028, 12130029, 12130030, 12130031) ~ "030726",
      DAuid %in% c(
        12150056, 12150057, 12150058, 12150059, 12150060, 12150061, 12150062, 12150063, 12150064,
        12150065, 12150066, 12150067, 12150068, 12150069, 12150070, 12160017, 12160018, 12160019,
        12160020, 12160021, 12160022, 12160023, 12160024, 12160025, 12160026, 12160027, 12160028,
        12160029, 12160030, 12160031, 12160032, 12160033, 12160034) ~ "030827",
      DAuid %in% c(
        12180026, 12180027, 12180028, 12180029, 12180030, 12180031, 12180032, 12180033, 12180034,
        12180035) ~ "030828",
      DAuid %in% c(
        12150034, 12150035, 12150036, 12150037, 12150038, 12150039, 12150040, 12150041, 12150042) ~ "030829",
      DAuid %in% c(
        12180020, 12180021, 12180022, 12180023, 12180024, 12180025) ~ "030830",
      DAuid %in% c(
        12150043, 12150044, 12150045, 12150046, 12150047, 12150048, 12150049, 12150050, 12150051,
        12150052, 12150053, 12150054, 12150055, 12150071, 12150072, 12150073, 12150074) ~ "030831",
      DAuid %in% c(
        12170356, 12170359, 12170360, 12170363, 12170365, 12170366, 12170367, 12170368,
        12170370, 12170371, 12170372, 12170373, 12170379, 12170380, 12170381, 12170382,
        12170383, 12170384, 12170385, 12170394) ~ "030932",
      DAuid %in% c(
        12170342, 12170343, 12170346, 12170347, 12170348, 12170349, 12170350, 12170402, 12170416, 12170429, 12170447, 12170448,
        12170449, 12170450, 12170451, 12170452, 12170453, 12170454, 12170455, 12170456, 12170457, 12170458, 12170459, 12170460,
        12170461, 12170462, 12170463, 12170464, 12170465, 12170466, 12170467, 12170468, 12170469, 12170470, 12170471, 12170472,
        12170473, 12170474, 12170475, 12170476, 12170477, 12170478, 12170479, 12170480, 12170481, 12170482, 12170483, 12170484,
        12170485, 12170486, 12170487, 12170488, 12170489, 12170490, 12170491, 12170492, 12170493, 12170494, 12170495, 12170497,
        12170498, 12170499, 12170500, 12170501, 12170502, 12170503, 12170504, 12170505, 12170506, 12170507, 12170508, 12170509,
        12170510, 12170511, 12170512, 12170513, 12170514, 12170515, 12170516, 12170517, 12170518, 12170519, 12170520, 12170521,
        12170523, 12170524, 12170525, 12170526, 12170527, 12170528, 12170529, 12170534, 12170535, 12170536, 12170537, 12170538,
        12170540, 12170541, 12170542, 12170544, 12170546, 12170547) ~ "030933",
      DAuid %in% c(
        12170393, 12170395, 12170396, 12170397, 12170398, 12170399, 12170401, 12170403, 12170405, 12170408, 12170410, 12170411, 12170412,
        12170413, 12170415, 12170418, 12170420, 12170422, 12170423, 12170424, 12170426, 12170427, 12170430, 12170431, 12170433, 12170434,
        12170435, 12170436, 12170437, 12170438, 12170439, 12170440, 12170441, 12170442, 12170443, 12170444, 12170445, 12170446, 12170530,
        12170531, 12170532, 12170533, 12170543) ~ "030934",
      DAuid %in% c(
        12170344, 12170352, 12170353, 12170354, 12170355, 12170357, 12170358, 12170361, 12170362, 12170364, 12170369, 12170374, 12170375,
        12170376, 12170377, 12170378, 12170386, 12170387, 12170388, 12170389, 12170390, 12170391, 12170392, 12170400, 12170404, 12170406,
        12170407, 12170409, 12170414, 12170417, 12170419, 12170421, 12170425, 12170428, 12170432, 12170522, 12180026, 12180027) ~ "030935",
      DAuid %in% c(
        12090179, 12090182, 12090183, 12090184, 12090185, 12090193, 12090194, 12090195, 12090196, 12090197, 12090198, 12090199, 12090200,
        12090201, 12090202, 12090203, 12090205, 12090206, 12090209, 12090210, 12090211, 12090212, 12090213, 12090214, 12090215, 12090216,
        12090217, 12090220, 12090562, 12090571, 12090576, 12090577, 12090846) ~ "041036",
      DAuid %in% c(
        12090162, 12090163, 12090164, 12090165, 12090166, 12090167, 12090168, 12090169, 12090170, 12090171, 12090172, 12090175, 12090176,
        12090177, 12090178, 12090204, 12090362, 12090365, 12090366, 12090367, 12090368, 12090369, 12090370, 12090371, 12090372, 12090373,
        12090406, 12090407, 12090570, 12090573, 12090751, 12090783, 12090784, 12090785, 12090786, 12090791, 12090792) ~ "041037",
      DAuid %in% c(
        12090128, 12090130, 12090131, 12090132, 12090133, 12090134, 12090135, 12090136,
        12090137, 12090138, 12090139, 12090140, 12090141, 12090142, 12090143, 12090144,
        12090145, 12090146, 12090147, 12090148, 12090149, 12090150, 12090152, 12090153,
        12090154, 12090158, 12090159, 12090160, 12090161, 12090186, 12090187, 12090188,
        12090189, 12090190, 12090191, 12090192, 12090415, 12090420, 12090421, 12090563,
        12090564, 12090787, 12090788, 12090789, 12090790) ~ "041038",
      DAuid %in% c(
        12090733, 12090734, 12090735, 12090736, 12090737, 12090738, 12090739, 12090740,
        12090741, 12090743, 12090745, 12090746, 12090747, 12090748, 12091002, 12091003,
        12090750, 12090752, 12090753, 12090754, 12090755, 12090756, 12090757, 12090758,
        12090759, 12090760, 12090761, 12090762, 12090763, 12090764, 12090765, 12090769,
        12090770, 12090771, 12090772, 12090773, 12090774, 12090775, 12090776, 12090777,
        12090778, 12090779, 12090780, 12090781, 12090782, 12090794, 12090795, 12090796,
        12090886, 12090888, 12090890, 12090899, 12090900, 12090980, 12090981, 12090983,
        12091004, 12091005, 12091006, 12091007, 12091008, 12091010, 12091011, 12091012,
        12091013, 12091014, 12091015, 12091016, 12091017, 12091018, 12091019, 12091020,
        12091021, 12091022, 12091023, 12091024, 12091025) ~ "041039",
      DAuid %in% c(
        12090797, 12090798, 12090799, 12090805, 12090811, 12090812, 12090813, 12090814,
        12090815, 12090816, 12090817, 12090819, 12090982, 12090984, 12090985, 12090986) ~ "041040",
      DAuid %in% c(
        12090648, 12090649, 12090650, 12090651, 12090652, 12090653, 12090655, 12090671,
        12090672, 12090673, 12090674, 12090678, 12090684, 12090685, 12090915, 12090916,
        12090917, 12090925, 12090926, 12090972, 12090973, 12090974, 12090997, 12090998) ~ "041141",
      DAuid %in% c(
        12090648, 12090649, 12090650, 12090651, 12090652, 12090653, 12090655, 12090671,
        12090672, 12090673, 12090674, 12090678, 12090684, 12090685, 12090915, 12090916,
        12090917, 12090925, 12090926, 12090972, 12090973, 12090974, 12090997, 12090998,
        12090656, 12090657, 12090658, 12090659, 12090660, 12090661, 12090662, 12090663,
        12090664, 12090665, 12090666, 12090667, 12090668, 12090721, 12090835, 12090836,
        12090837, 12090838, 12090977, 12090978) ~ "041142",
      DAuid %in% c(
        12090250, 12090251, 12090252, 12090253, 12090254, 12090255, 12090256, 12090259,
        12090260, 12090261, 12090262, 12090263, 12090264, 12090265, 12090267, 12090270,
        12090271, 12090272, 12090273, 12090277, 12090474, 12090475, 12090480, 12090481,
        12090569, 12090722, 12090723, 12090724, 12090725, 12090728, 12090729, 12090730,
        12090731, 12090896, 12090897, 12090898, 12090966, 12090967, 12090976) ~ "041143",
      DAuid %in% c(
        12090223, 12090224, 12090225, 12090226, 12090227, 12090228, 12090229, 12090230,
        12090231, 12090232, 12090337, 12090340, 12090341, 12090342, 12090343, 12090344,
        12090345, 12090346, 12090347, 12090348, 12090349, 12090350, 12090351, 12090355,
        12090356, 12090358, 12090847, 12090848, 12090849, 12090850, 12090851,
        12090852, 12090877, 12090878) ~ "041144",
      DAuid %in% c(
        12090244, 12090245, 12090246, 12090247, 12090248, 12090311, 12090312, 12090313,
        12090324, 12090325, 12090326, 12090327, 12090334, 12090335, 12090336, 12090476,
        12090477, 12090478, 12090479, 12090869, 12090870, 12090871, 12090872, 12090873,
        12090874, 12090875, 12090876, 12090879, 12090880, 12090881, 12090882, 12090883,
        12090937, 12090960) ~ "041145",
      DAuid %in% c(
        12090241, 12090249, 12090482, 12090483, 12090484, 12090485, 12090486, 12090487,
        12090488, 12090489, 12090494, 12090495, 12090496, 12090497, 12090498, 12090500,
        12090501, 12090502, 12090534, 12090535, 12090536, 12090537, 12090538, 12090565,
        12090939, 12090962) ~ "041146",
      DAuid %in% c(
        12090490, 12090491, 12090492, 12090493, 12090506, 12090507, 12090508, 12090509,
        12090510, 12090513, 12090514, 12090516, 12090517, 12090518, 12090520, 12090521,
        12090523, 12090528, 12090530, 12090531, 12090532, 12090533, 12090539, 12090540,
        12090541, 12090542, 12090567, 12090574, 12090580, 12090581, 12090582, 12090891,
        12090892, 12090902, 12090950, 12090951, 12090956, 12090957, 12090958, 12090969,
        12090970, 12090999) ~ "041147",
      DAuid %in% c(
        12090279, 12090280, 12090281, 12090282, 12090291, 12090292, 12090293, 12090294,
        12090295, 12090296, 12090297, 12090298, 12090328, 12090329, 12090330, 12090331,
        12090332, 12090333, 12090357, 12090359, 12090572, 12090853, 12090854, 12090855,
        12090856, 12090858, 12090860, 12090863, 12090864, 12090865, 12090866, 12090867,
        12090868, 12090934, 12090936, 12090963, 12090965) ~ "041148",
      DAuid %in% c(
        12090103, 12090104, 12090105, 12090106, 12090108, 12090114, 12090115, 12090116,
        12090117, 12090118, 12090119, 12090120, 12090121, 12090122, 12090123, 12090578,
        12090579, 12090640, 12090641, 12090642, 12090643, 12090680, 12090682, 12090683,
        12090687, 12090688, 12090839, 12090840, 12090841, 12090842, 12090843, 12090844,
        12090901, 12090903, 12090904, 12090905, 12090919, 12090920, 12090928, 12090952,
        12090954, 12090955, 12091000, 12091001) ~ "041249",
      DAuid %in% c(
        12090612, 12090613, 12090614, 12090615, 12090616, 12090617, 12090618, 12090619,
        12090620, 12090621, 12090622, 12090623, 12090624, 12090631, 12090632, 12090633,
        12090634, 12090635, 12090636, 12090637, 12090638, 12090697, 12090698, 12090699,
        12090700, 12090701, 12090702, 12090703, 12090704, 12090705, 12090706, 12090707,
        12090895) ~ "041250",
      DAuid %in% c(
        12080119, 12080120, 12080121, 12080122, 12090625, 12090627, 12090628, 12090629,
        12090630, 12090690, 12090691, 12090693, 12090694, 12090695, 12090696, 12090709,
        12090710, 12090711, 12090712, 12090713, 12090715, 12090717, 12090893, 12090894,
        12090927, 12090929, 12090930) ~ "041251",
      DAuid %in% c(
        12090599, 12090602, 12090608, 12090609, 12090610, 12090611, 12090708,
        12090716, 12090719, 12090793, 12090947, 12090948, 12090988, 12090989, 12090990,
        12090991, 12090992, 12090993, 12090994, 12090995, 12090996) ~ "041252",
      DAuid %in% c(
        12090583, 12090587, 12090588, 12090590, 12090591, 12090592, 12090593, 12090594,
        12090595, 12090598, 12090808, 12090809, 12090810, 12090818, 12090820, 12090823,
        12090824, 12090828, 12090832, 12090909, 12090910, 12090911, 12090921, 12090922,
        12090924, 12090932, 12090933, 12090941, 12090942, 12090943, 12090944, 12090945) ~ "041353",
      DAuid %in% c(
        12070166, 12070167, 12080084, 12080085, 12080086, 12080087, 12080088, 12080089, 12080090,
        12080091, 12080092, 12080093, 12080094, 12080095, 12080096, 12080097, 12080098, 12080099,
        12080100, 12080101, 12080102, 12080103, 12080104, 12080105, 12080106, 12080107, 12080108,
        12080109, 12080114, 12080115, 12080116, 12080117, 12080118) ~ "041454"
      )
    ) %>%
  # filling in missing information based on 5-digits PCODE
  group_by(substr(Post_Code,1,5)) %>%
  tidyr::fill(c(CLuid),
              .direction = "downup") %>%
  ungroup() %>%
  # filling in missing information based on 4-digits PCODE
  group_by(substr(Post_Code,1,4)) %>%
  tidyr::fill(c(CLuid),
              .direction = "downup") %>%
  ungroup() %>%
  # filling in missing information based on 3-digits PCODE
  group_by(substr(Post_Code,1,3)) %>%
  tidyr::fill(c(CLuid),
              .direction = "downup") %>%
  ungroup() %>%
  mutate(
    CLName = case_when(
      CLuid %in% "010101" ~ "Bridgewater",
      CLuid %in% "010102" ~ "Chester and Area",
      CLuid %in% "010103" ~ "Liverpool",
      CLuid %in% "010104" ~ "Lunenburg/Mahone Bay",
      CLuid %in% "010205" ~ "Digby/Clare/Weymouth",
      CLuid %in% "010206" ~ "Shelburne/Lockeport",
      CLuid %in% "010207" ~ "Yarmouth",
      CLuid %in% "010308" ~ "Annapolis Royal",
      CLuid %in% "010309" ~ "Berwick",
      CLuid %in% "010310" ~ "Kentville",
      CLuid %in% "010311" ~ "Middleton",
      CLuid %in% "010312" ~ "Wolfville",
      CLuid %in% "020413" ~ "East Hants Corridor",
      CLuid %in% "020414" ~ "Economy/Glenholme",
      CLuid %in% "020415" ~ "Hants North",
      CLuid %in% "020416" ~ "South Colchester",
      CLuid %in% "020417" ~ "Truro and Area",
      CLuid %in% "020518" ~ "Amherst",
      CLuid %in% "020519" ~ "Cumberland North/North Shore",
      CLuid %in% "020520" ~ "South Cumberland",
      CLuid %in% "020521" ~ "Springhill",
      CLuid %in% "020622" ~ "New Glasgow/Westville/Stellarton",
      CLuid %in% "020623" ~ "Pictou West",
      CLuid %in% "030724" ~ "Antigonish",
      CLuid %in% "030725" ~ "Guysborough/Canso",
      CLuid %in% "030726" ~ "Sherbrooke",
      CLuid %in% "030827" ~ "Port Hawkesbury/L'Ardoise/Isle Madame",
      CLuid %in% "030828" ~ "Baddeck/Whycocomagh",
      CLuid %in% "030829" ~ "Cheticamp",
      CLuid %in% "030830" ~ "Dingwall",
      CLuid %in% "030831" ~ "Inverness",
      CLuid %in% "030932" ~ "New Waterford",
      CLuid %in% "030933" ~ "Sydney and Area",
      CLuid %in% "030934" ~ "Dominion/Glace Bay",
      CLuid %in% "030935" ~ "Florence/Sydney Mines/North Sydney",
      CLuid %in% "041036" ~ "Dartmouth North",
      CLuid %in% "041037" ~ "Dartmouth South",
      CLuid %in% "041038" ~ "Dartmouth East",
      CLuid %in% "041039" ~ "Cole Harbour/Eastern Passage",
      CLuid %in% "041040" ~ "Preston/Lawrencetown/Lake Echo",
      CLuid %in% "041141" ~ "Tantallon/Timberlea/SMB",
      CLuid %in% "041142" ~ "Sambro Rural Loop",
      CLuid %in% "041143" ~ "Armdale/Spryfield/Herring Cove",
      CLuid %in% "041144" ~ "Halifax Needham",
      CLuid %in% "041145" ~ "Halifax Chebucto",
      CLuid %in% "041146" ~ "Fairview",
      CLuid %in% "041147" ~ "Clayton Park",
      CLuid %in% "041148" ~ "Halifax Citadel",
      CLuid %in% "041249" ~ "Bedford/Hammonds Plains",
      CLuid %in% "041250" ~ "Sackville South",
      CLuid %in% "041251" ~ "Sackville North and Area",
      CLuid %in% "041252" ~ "Fall River and Area",
      CLuid %in% "041353" ~ "Eastern Shore/Musquodoboit",
      CLuid %in% "041454" ~ "West Hants"
    ),
    CHNuid = substr(CLuid, 1, 4),
    CHNName = case_when(
      CHNuid %in% "0101" ~ "Lunenburg & Queens",
      CHNuid %in% "0102" ~ "Yarmouth, Shelburne, & Digby",
      CHNuid %in% "0103" ~ "Annapolis and Kings",
      CHNuid %in% "0204" ~ "Colchester East Hants",
      CHNuid %in% "0205" ~ "Cumberland",
      CHNuid %in% "0206" ~ "Pictou",
      CHNuid %in% "0307" ~ "Antigonish & Guysborough",
      CHNuid %in% "0308" ~ "Inverness, Victoria, & Richmond",
      CHNuid %in% "0309" ~ "Cape Breton",
      CHNuid %in% "0410" ~ "Dartmouth & Southeastern",
      CHNuid %in% "0411" ~ "Halifax Peninsula & Chebucto",
      CHNuid %in% "0412" ~ "Bedford & Sackville",
      CHNuid %in% "0413" ~ "Eastern Shore Musquodoboit",
      CHNuid %in% "0414" ~ "West Hants"
    )
  ) %>%
 # Keep only the reported data
 filter(!is.na(cat_tier4))

# Preparing NSPAD data to compute number of births ----

tmp_dta <- dta %>%
 ### Keep only data with postal codes ----
 ### Keep only data with NS postal code ----
 filter(!DLPSTCOD %in% c(""," ",NA),
        substr(DLPSTCOD,1,1) %in% "B") %>%
 mutate(CDuid = ifelse(CDuid %in% 1200,
                       NA,
                       CDuid)) %>%
 ### filling in missing information based on DAuid ----
 group_by(DAuid) %>%
 tidyr::fill(c(CSDuid,CDuid,HRuid,HRename),
             .direction = "downup") %>%
 ungroup() %>%
 ## filling in missing information based on CDuid
 group_by(CDuid) %>%
 tidyr::fill(c(CSDuid,CDuid,HRuid,HRename),
             .direction = "downup") %>%
 ungroup() %>%
 # filling in missing information based on 6-digits PCODE
 group_by(DLPSTCOD) %>%
 tidyr::fill(c(CSDuid,CDuid,HRuid,HRename),
             .direction = "downup") %>%
 ungroup() %>%
 # filling in missing information based on 5-digits PCODE
 group_by(substr(DLPSTCOD,1,5)) %>%
 tidyr::fill(c(CSDuid,CDuid,HRuid,HRename),
             .direction = "downup") %>%
 ungroup() %>%
 # filling in missing information based on 4-digits PCODE
 group_by(substr(DLPSTCOD,1,4)) %>%
 tidyr::fill(c(CSDuid,CDuid,HRuid,HRename),
             .direction = "downup") %>%
 ungroup() %>%
 # filling in missing information based on 3-digits PCODE
 group_by(substr(DLPSTCOD,1,3)) %>%
 tidyr::fill(c(CSDuid,CDuid,HRuid,HRename),
             .direction = "downup") %>%
 ungroup() %>%
 # fix HRuid to match '.shp' file
 mutate(
  HRuid = case_when(
   HRename %in% "Zone 1 - Western" ~ "01",
   HRename %in% "Zone 2 - Northern" ~ "02",
   HRename %in% "Zone 3 - Eastern" ~ "03",
   HRename %in% "Zone 4 - Central" ~ "04"
  ),
  CLuid = case_when(
   DAuid %in% c(
    12060176, 12060077, 12060078, 12060079, 12060080, 12060081, 12060082, 12060083, 12060084,
    12060085, 12060087, 12060088, 12060089, 12060090, 12060091, 12060092, 12060093, 12060094,
    12060095, 12060096, 12060097, 12060098, 12060099, 12060100, 12060170, 12060169, 12060171,
    12060172, 12060118, 12060119, 12060120, 12060186, 12060187, 12060188, 12060190, 12060124,
    12060125, 12060126, 12060184, 12060185, 12060128, 12060129, 12060130, 12060131, 12060132,
    12060133, 12060134, 12060160, 12060161, 12060162, 12060163, 12060168, 12060166, 12060167,
    12060181, 12060182, 12060183) ~ "010101",
   DAuid %in% c(
    12060071, 12060072, 12060073, 12060074, 12060075, 12060135, 12060136, 12060137, 12060138,
    12060139, 12060140, 12060141, 12060142, 12060143, 12060144, 12060145, 12060146, 12060147,
    12060148, 12060149, 12060150, 12060151, 12060152, 12060153, 12060154, 120906771) ~ "010102",
   DAuid %in% c(
    12040025, 12040026, 12040027, 12040028, 12040029, 12040030, 12040031, 12040032, 12040033,
    12040034, 12040035, 12040036, 12040037, 12040038, 12040039, 12040040, 12040041, 12040042,
    12040043, 12040044, 12040045, 12040046, 12040047, 12040048, 12040049, 12040051, 12040053) ~ "010103",
   DAuid %in% c(
    12060105, 12060106, 12060107, 12060108, 12060109, 12060110, 12060111, 12060112, 12060113,
    12060114, 12060115, 12060116, 12060156, 12060157, 12060158, 12060173, 12060174, 12060177,
    12060178, 12060179, 12060180) ~ "010104",
   DAuid %in% c(
    12030040, 12030041, 12030042, 12030043, 12030044, 12030045, 12030046, 12030047, 12030048,
    12030051, 12030052, 12030053, 12030054, 12030055, 12030056, 12030057, 12030058, 12030059,
    12030060, 12030061, 12030062, 12030063, 12030064, 12030065, 12030066, 12030067, 12030068,
    12030069, 12030070, 12030071, 12030072, 12030073, 12030074, 12030075, 12030076, 12030077,
    12030078, 12030079, 12030082, 12050078, 12050080, 12050085, 12050090, 12050093) ~ "010205",
   DAuid %in% c(
    12010030, 12010031, 12010032, 12010033, 12010034, 12010035, 12010036, 12010037, 12010038,
    12010039, 12010040, 12010041, 12010042, 12010043, 12010044, 12010045, 12010046, 12010047,
    12010048, 12010049, 12010050, 12010051, 12010052, 12010053, 12010054, 12010055, 12010056) ~ "010206",
   DAuid %in% c(
    12020043, 12020044, 12020045, 12020046, 12020047, 12020048, 12020049, 12020050, 12020051,
    12020052, 12020053, 12020054, 12020055, 12020056, 12020057, 12020058, 12020059, 12020060,
    12020061, 12020062, 12020063, 12020064, 12020065, 12020066, 12020067, 12020068, 12020069,
    12020070, 12020071, 12020072, 12020073, 12020074, 12020075, 12020076, 12020077, 12020078,
    12020079, 12020080, 12020081, 12020082, 12020083, 12020084, 12020085, 12020086, 12020087) ~ "010207",
   DAuid %in% c(
    12050069, 12050070, 12050071, 12050072, 12050073, 12050074, 12050075, 12050076, 12050077,
    12050082, 12050083, 12050084, 12050086, 12050087) ~ "010308",
   DAuid %in% c(
    12070131, 12070132, 12070133, 12070134, 12070135, 12070136, 12070137, 12070138, 12070139,
    12070140, 12070141, 12070142, 12070143, 12070144, 12070150, 12070171, 12070172, 12070173,
    12070187, 12070188, 12070189, 12070190, 12070191, 12070192, 12070193, 12070194, 12070195) ~ "010309",
   DAuid %in% c(
    12070088, 12070089, 12070090, 12070091, 12070092, 12070093, 12070094, 12070095, 12070096,
    12070097, 12070098, 12070099, 12070100, 12070101, 12070102, 12070103, 12070104, 12070105,
    12070106, 12070107, 12070108, 12070109, 12070110, 12070111, 12070112, 12070113, 12070114,
    12070115, 12070116, 12070117, 12070118, 12070119, 12070120, 12070121, 12070126, 12070127,
    12070128, 12070129, 12070130, 12070145, 12070146, 12070147, 12070148, 12070149) ~ "010310",
   DAuid %in% c(
    12050044, 12050045, 12050046, 12050047, 12050048, 12050049, 12050050, 12050051, 12050052,
    12050053, 12050054, 12050055, 12050056, 12050057, 12050058, 12050059, 12050060, 12050061,
    12050062, 12050063, 12050064, 12050065, 12050066, 12050067, 12050068, 12050088, 12050089,
    12070174, 12070175, 12070176, 12070177, 12070178, 12070179, 12070180, 12070181, 12070182,
    12070183, 12070184, 12070185, 12070186, 12070193) ~ "010311",
   DAuid %in% c(
    12070081, 12070082, 12070083, 12070084, 12070085, 12070086, 12070087, 12070122, 12070123,
    12070124, 12070125, 12070151, 12070152, 12070153, 12070154, 12070155, 12070156, 12070157,
    12070158, 12070159, 12070160, 12070161, 12070162, 12070163, 12070164, 12070165, 12070168,
    12070169, 12070170) ~ "010312",
   DAuid %in% c(
    12080063, 12080064, 12080065, 12080069, 12080070, 12080071, 12080072, 12080073, 12080074,
    12080123, 12080127, 12080128, 12080129, 12090596, 12090597, 12090598, 12090600) ~ "020413",
   DAuid %in% c(
    12100128, 12100129, 12100130, 12100131, 12100132, 12100133, 12100134, 12100135, 12100136,
    12100137) ~ "020414",
   DAuid %in% c(
    12080056, 12080076, 12080077, 12080078, 12080079, 12080080, 12080081, 12080082, 12080083,
    12080110, 12080111, 12080112, 12080113) ~ "020415",
   DAuid %in% c(
    12080056, 12080057, 12080058, 12080059, 12080130, 12080131, 12080061, 12080132, 12080133,
    12100152, 12100153, 12100154, 12100158, 12100159, 12100160, 12100161, 12100162, 12100163,
    12100164, 12100165, 12100166, 12100167, 12100168, 12100169, 12100170) ~ "020416",
   DAuid %in% c(
    12100082, 12100083, 12100084, 12100085, 12100086, 12100087, 12100088, 12100089, 12100090,
    12100091, 12100092, 12100093, 12100094, 12100095, 12100096, 12100097, 12100098, 12100099,
    12100100, 12100102, 12100103, 12100104, 12100105, 12100106, 12100107, 12100108, 12100109,
    12100110, 12100111, 12100112, 12100113, 12100114, 12100115, 12100116, 12100117, 12100118,
    12100119, 12100120, 12100121, 12100122, 12100123, 12100124, 12100125, 12100126, 12100127,
    12100138, 12100139, 12100140, 12100141, 12100142, 12100143, 12100144, 12100145, 12100146,
    12100147, 12100148, 12100149, 12100150, 12100151, 12100152, 12100153, 12100155, 12100156,
    12100157, 12100161) ~ "020417",
   DAuid %in% c(
    12110090, 12110091, 12110092, 12110093, 12110094, 12110095, 12110096, 12110097, 12110098,
    12110099, 12110100, 12110101, 12110102, 12110103, 12110104, 12110105, 12110106, 12110107,
    12110108, 12110109, 12110110, 12110111, 12110112, 12110113, 12110114, 12110115, 12110116) ~ "020518",
   DAuid %in% c(
    12100077, 12100078, 12100079, 12100080, 12100081, 12100101, 12110052, 12110053, 12110054,
    12110055, 12110056, 12110057, 12110058, 12110059, 12110060, 12110077, 12110078, 12110079) ~ "020519",
   DAuid %in% c(
    12110080, 12110081, 12110082, 12110083, 12110084, 12110085, 12110086, 12110087, 12110088) ~ "020520",
   DAuid %in% c(
    12110061, 12110062, 12110063, 12110064, 12110065, 12110066, 12110067, 12110068, 12110069,
    12110070, 12110071, 12110072, 12110073, 12110074, 12110075, 12110076, 12110089, 12110117,
    12110118) ~ "020521",
   DAuid %in% c(
    12120063, 12120064, 12120065, 12120066, 12120067, 12120068, 12120069, 12120070, 12120071,
    12120072, 12120073, 12120074, 12120075, 12120076, 12120077, 12120078, 12120079, 12120080,
    12120081, 12120082, 12120083, 12120084, 12120085, 12120086, 12120087, 12120088, 12120089,
    12120090, 12120091, 12120092, 12120093, 12120094, 12120095, 12120096, 12120097, 12120098,
    12120099, 12120100, 12120101, 12120102, 12120103, 12120104, 12120105, 12120106, 12120107,
    12120108, 12120109, 12120110, 12120111, 12120112, 12120113, 12120114, 12120115, 12120116,
    12120117, 12120118, 12120119, 12120120, 12120121, 12120139, 12120140, 12120141, 12120142,
    12120143, 12120144, 12120145, 12120146, 12120147, 12120148, 12120149, 12120150, 12120151,
    12120152, 12120153) ~ "020622",
   DAuid %in% c(
    12120122, 12120123, 12120124, 12120125, 12120126, 12120127, 12120128, 12120129, 12120130,
    12120131, 12120132, 12120133, 12120134, 12120135, 12120136, 12120137, 12120138, 12120154,
    12120155, 12120156, 12120157, 12120158) ~ "020623",
   DAuid %in% c(
    12140039, 12140040, 12140041, 12140042, 12140043, 12140044, 12140045, 12140046, 12140047,
    12140048, 12140049, 12140050, 12140051, 12140052, 12140053, 12140054, 12140055, 12140056,
    12140057, 12140058, 12140059, 12140060, 12140061, 12140062, 12140063, 12140064, 12140065,
    12140066, 12140067, 12140068, 12140069, 12140070, 12140071, 12140072, 12140073, 12140074,
    12140075, 12140076, 12140077, 12140078, 12140079, 12140080) ~ "030724",
   DAuid %in% c(
    12130021, 12130022, 12130023, 12130024, 12130025, 12130027, 12130032, 12130033, 12130034,
    12130035, 12130036) ~ "030725",
   DAuid %in% c(
    12130026, 12130028, 12130029, 12130030, 12130031) ~ "030726",
   DAuid %in% c(
    12150056, 12150057, 12150058, 12150059, 12150060, 12150061, 12150062, 12150063, 12150064,
    12150065, 12150066, 12150067, 12150068, 12150069, 12150070, 12160017, 12160018, 12160019,
    12160020, 12160021, 12160022, 12160023, 12160024, 12160025, 12160026, 12160027, 12160028,
    12160029, 12160030, 12160031, 12160032, 12160033, 12160034) ~ "030827",
   DAuid %in% c(
    12180026, 12180027, 12180028, 12180029, 12180030, 12180031, 12180032, 12180033, 12180034,
    12180035) ~ "030828",
   DAuid %in% c(
    12150034, 12150035, 12150036, 12150037, 12150038, 12150039, 12150040, 12150041, 12150042) ~ "030829",
   DAuid %in% c(
    12180020, 12180021, 12180022, 12180023, 12180024, 12180025) ~ "030830",
   DAuid %in% c(
    12150043, 12150044, 12150045, 12150046, 12150047, 12150048, 12150049, 12150050, 12150051,
    12150052, 12150053, 12150054, 12150055, 12150071, 12150072, 12150073, 12150074) ~ "030831",
   DAuid %in% c(
    12170356, 12170359, 12170360, 12170363, 12170365, 12170366, 12170367, 12170368,
    12170370, 12170371, 12170372, 12170373, 12170379, 12170380, 12170381, 12170382,
    12170383, 12170384, 12170385, 12170394) ~ "030932",
   DAuid %in% c(
    12170342, 12170343, 12170346, 12170347, 12170348, 12170349, 12170350, 12170402, 12170416, 12170429, 12170447, 12170448,
    12170449, 12170450, 12170451, 12170452, 12170453, 12170454, 12170455, 12170456, 12170457, 12170458, 12170459, 12170460,
    12170461, 12170462, 12170463, 12170464, 12170465, 12170466, 12170467, 12170468, 12170469, 12170470, 12170471, 12170472,
    12170473, 12170474, 12170475, 12170476, 12170477, 12170478, 12170479, 12170480, 12170481, 12170482, 12170483, 12170484,
    12170485, 12170486, 12170487, 12170488, 12170489, 12170490, 12170491, 12170492, 12170493, 12170494, 12170495, 12170497,
    12170498, 12170499, 12170500, 12170501, 12170502, 12170503, 12170504, 12170505, 12170506, 12170507, 12170508, 12170509,
    12170510, 12170511, 12170512, 12170513, 12170514, 12170515, 12170516, 12170517, 12170518, 12170519, 12170520, 12170521,
    12170523, 12170524, 12170525, 12170526, 12170527, 12170528, 12170529, 12170534, 12170535, 12170536, 12170537, 12170538,
    12170540, 12170541, 12170542, 12170544, 12170546, 12170547) ~ "030933",
   DAuid %in% c(
    12170393, 12170395, 12170396, 12170397, 12170398, 12170399, 12170401, 12170403, 12170405, 12170408, 12170410, 12170411, 12170412,
    12170413, 12170415, 12170418, 12170420, 12170422, 12170423, 12170424, 12170426, 12170427, 12170430, 12170431, 12170433, 12170434,
    12170435, 12170436, 12170437, 12170438, 12170439, 12170440, 12170441, 12170442, 12170443, 12170444, 12170445, 12170446, 12170530,
    12170531, 12170532, 12170533, 12170543) ~ "030934",
   DAuid %in% c(
    12170344, 12170352, 12170353, 12170354, 12170355, 12170357, 12170358, 12170361, 12170362, 12170364, 12170369, 12170374, 12170375,
    12170376, 12170377, 12170378, 12170386, 12170387, 12170388, 12170389, 12170390, 12170391, 12170392, 12170400, 12170404, 12170406,
    12170407, 12170409, 12170414, 12170417, 12170419, 12170421, 12170425, 12170428, 12170432, 12170522, 12180026, 12180027) ~ "030935",
   DAuid %in% c(
    12090179, 12090182, 12090183, 12090184, 12090185, 12090193, 12090194, 12090195, 12090196, 12090197, 12090198, 12090199, 12090200,
    12090201, 12090202, 12090203, 12090205, 12090206, 12090209, 12090210, 12090211, 12090212, 12090213, 12090214, 12090215, 12090216,
    12090217, 12090220, 12090562, 12090571, 12090576, 12090577, 12090846) ~ "041036",
   DAuid %in% c(
    12090162, 12090163, 12090164, 12090165, 12090166, 12090167, 12090168, 12090169, 12090170, 12090171, 12090172, 12090175, 12090176,
    12090177, 12090178, 12090204, 12090362, 12090365, 12090366, 12090367, 12090368, 12090369, 12090370, 12090371, 12090372, 12090373,
    12090406, 12090407, 12090570, 12090573, 12090751, 12090783, 12090784, 12090785, 12090786, 12090791, 12090792) ~ "041037",
   DAuid %in% c(
    12090128, 12090130, 12090131, 12090132, 12090133, 12090134, 12090135, 12090136,
    12090137, 12090138, 12090139, 12090140, 12090141, 12090142, 12090143, 12090144,
    12090145, 12090146, 12090147, 12090148, 12090149, 12090150, 12090152, 12090153,
    12090154, 12090158, 12090159, 12090160, 12090161, 12090186, 12090187, 12090188,
    12090189, 12090190, 12090191, 12090192, 12090415, 12090420, 12090421, 12090563,
    12090564, 12090787, 12090788, 12090789, 12090790) ~ "041038",
   DAuid %in% c(
    12090733, 12090734, 12090735, 12090736, 12090737, 12090738, 12090739, 12090740,
    12090741, 12090743, 12090745, 12090746, 12090747, 12090748, 12091002, 12091003,
    12090750, 12090752, 12090753, 12090754, 12090755, 12090756, 12090757, 12090758,
    12090759, 12090760, 12090761, 12090762, 12090763, 12090764, 12090765, 12090769,
    12090770, 12090771, 12090772, 12090773, 12090774, 12090775, 12090776, 12090777,
    12090778, 12090779, 12090780, 12090781, 12090782, 12090794, 12090795, 12090796,
    12090886, 12090888, 12090890, 12090899, 12090900, 12090980, 12090981, 12090983,
    12091004, 12091005, 12091006, 12091007, 12091008, 12091010, 12091011, 12091012,
    12091013, 12091014, 12091015, 12091016, 12091017, 12091018, 12091019, 12091020,
    12091021, 12091022, 12091023, 12091024, 12091025) ~ "041039",
   DAuid %in% c(
    12090797, 12090798, 12090799, 12090805, 12090811, 12090812, 12090813, 12090814,
    12090815, 12090816, 12090817, 12090819, 12090982, 12090984, 12090985, 12090986) ~ "041040",
   DAuid %in% c(
    12090648, 12090649, 12090650, 12090651, 12090652, 12090653, 12090655, 12090671,
    12090672, 12090673, 12090674, 12090678, 12090684, 12090685, 12090915, 12090916,
    12090917, 12090925, 12090926, 12090972, 12090973, 12090974, 12090997, 12090998) ~ "041141",
   DAuid %in% c(
    12090648, 12090649, 12090650, 12090651, 12090652, 12090653, 12090655, 12090671,
    12090672, 12090673, 12090674, 12090678, 12090684, 12090685, 12090915, 12090916,
    12090917, 12090925, 12090926, 12090972, 12090973, 12090974, 12090997, 12090998,
    12090656, 12090657, 12090658, 12090659, 12090660, 12090661, 12090662, 12090663,
    12090664, 12090665, 12090666, 12090667, 12090668, 12090721, 12090835, 12090836,
    12090837, 12090838, 12090977, 12090978) ~ "041142",
   DAuid %in% c(
    12090250, 12090251, 12090252, 12090253, 12090254, 12090255, 12090256, 12090259,
    12090260, 12090261, 12090262, 12090263, 12090264, 12090265, 12090267, 12090270,
    12090271, 12090272, 12090273, 12090277, 12090474, 12090475, 12090480, 12090481,
    12090569, 12090722, 12090723, 12090724, 12090725, 12090728, 12090729, 12090730,
    12090731, 12090896, 12090897, 12090898, 12090966, 12090967, 12090976) ~ "041143",
   DAuid %in% c(
    12090223, 12090224, 12090225, 12090226, 12090227, 12090228, 12090229, 12090230,
    12090231, 12090232, 12090337, 12090340, 12090341, 12090342, 12090343, 12090344,
    12090345, 12090346, 12090347, 12090348, 12090349, 12090350, 12090351, 12090355,
    12090356, 12090358, 12090847, 12090848, 12090849, 12090850, 12090851,
    12090852, 12090877, 12090878) ~ "041144",
   DAuid %in% c(
    12090244, 12090245, 12090246, 12090247, 12090248, 12090311, 12090312, 12090313,
    12090324, 12090325, 12090326, 12090327, 12090334, 12090335, 12090336, 12090476,
    12090477, 12090478, 12090479, 12090869, 12090870, 12090871, 12090872, 12090873,
    12090874, 12090875, 12090876, 12090879, 12090880, 12090881, 12090882, 12090883,
    12090937, 12090960) ~ "041145",
   DAuid %in% c(
    12090241, 12090249, 12090482, 12090483, 12090484, 12090485, 12090486, 12090487,
    12090488, 12090489, 12090494, 12090495, 12090496, 12090497, 12090498, 12090500,
    12090501, 12090502, 12090534, 12090535, 12090536, 12090537, 12090538, 12090565,
    12090939, 12090962) ~ "041146",
   DAuid %in% c(
    12090490, 12090491, 12090492, 12090493, 12090506, 12090507, 12090508, 12090509,
    12090510, 12090513, 12090514, 12090516, 12090517, 12090518, 12090520, 12090521,
    12090523, 12090528, 12090530, 12090531, 12090532, 12090533, 12090539, 12090540,
    12090541, 12090542, 12090567, 12090574, 12090580, 12090581, 12090582, 12090891,
    12090892, 12090902, 12090950, 12090951, 12090956, 12090957, 12090958, 12090969,
    12090970, 12090999) ~ "041147",
   DAuid %in% c(
    12090279, 12090280, 12090281, 12090282, 12090291, 12090292, 12090293, 12090294,
    12090295, 12090296, 12090297, 12090298, 12090328, 12090329, 12090330, 12090331,
    12090332, 12090333, 12090357, 12090359, 12090572, 12090853, 12090854, 12090855,
    12090856, 12090858, 12090860, 12090863, 12090864, 12090865, 12090866, 12090867,
    12090868, 12090934, 12090936, 12090963, 12090965) ~ "041148",
   DAuid %in% c(
    12090103, 12090104, 12090105, 12090106, 12090108, 12090114, 12090115, 12090116,
    12090117, 12090118, 12090119, 12090120, 12090121, 12090122, 12090123, 12090578,
    12090579, 12090640, 12090641, 12090642, 12090643, 12090680, 12090682, 12090683,
    12090687, 12090688, 12090839, 12090840, 12090841, 12090842, 12090843, 12090844,
    12090901, 12090903, 12090904, 12090905, 12090919, 12090920, 12090928, 12090952,
    12090954, 12090955, 12091000, 12091001) ~ "041249",
   DAuid %in% c(
    12090612, 12090613, 12090614, 12090615, 12090616, 12090617, 12090618, 12090619,
    12090620, 12090621, 12090622, 12090623, 12090624, 12090631, 12090632, 12090633,
    12090634, 12090635, 12090636, 12090637, 12090638, 12090697, 12090698, 12090699,
    12090700, 12090701, 12090702, 12090703, 12090704, 12090705, 12090706, 12090707,
    12090895) ~ "041250",
   DAuid %in% c(
    12080119, 12080120, 12080121, 12080122, 12090625, 12090627, 12090628, 12090629,
    12090630, 12090690, 12090691, 12090693, 12090694, 12090695, 12090696, 12090709,
    12090710, 12090711, 12090712, 12090713, 12090715, 12090717, 12090893, 12090894,
    12090927, 12090929, 12090930) ~ "041251",
   DAuid %in% c(
    12090599, 12090602, 12090608, 12090609, 12090610, 12090611, 12090708,
    12090716, 12090719, 12090793, 12090947, 12090948, 12090988, 12090989, 12090990,
    12090991, 12090992, 12090993, 12090994, 12090995, 12090996) ~ "041252",
   DAuid %in% c(
    12090583, 12090587, 12090588, 12090590, 12090591, 12090592, 12090593, 12090594,
    12090595, 12090598, 12090808, 12090809, 12090810, 12090818, 12090820, 12090823,
    12090824, 12090828, 12090832, 12090909, 12090910, 12090911, 12090921, 12090922,
    12090924, 12090932, 12090933, 12090941, 12090942, 12090943, 12090944, 12090945) ~ "041353",
   DAuid %in% c(
    12070166, 12070167, 12080084, 12080085, 12080086, 12080087, 12080088, 12080089, 12080090,
    12080091, 12080092, 12080093, 12080094, 12080095, 12080096, 12080097, 12080098, 12080099,
    12080100, 12080101, 12080102, 12080103, 12080104, 12080105, 12080106, 12080107, 12080108,
    12080109, 12080114, 12080115, 12080116, 12080117, 12080118) ~ "041454"
  )
 ) %>%
 # filling in missing information based on 5-digits PCODE
 group_by(substr(DLPSTCOD,1,5)) %>%
 tidyr::fill(c(CLuid),
             .direction = "downup") %>%
 ungroup() %>%
 # filling in missing information based on 4-digits PCODE
 group_by(substr(DLPSTCOD,1,4)) %>%
 tidyr::fill(c(CLuid),
             .direction = "downup") %>%
 ungroup() %>%
 # filling in missing information based on 3-digits PCODE
 group_by(substr(DLPSTCOD,1,3)) %>%
 tidyr::fill(c(CLuid),
             .direction = "downup") %>%
 ungroup() %>%
 mutate(
  CLName = case_when(
   CLuid %in% "010101" ~ "Bridgewater",
   CLuid %in% "010102" ~ "Chester and Area",
   CLuid %in% "010103" ~ "Liverpool",
   CLuid %in% "010104" ~ "Lunenburg/Mahone Bay",
   CLuid %in% "010205" ~ "Digby/Clare/Weymouth",
   CLuid %in% "010206" ~ "Shelburne/Lockeport",
   CLuid %in% "010207" ~ "Yarmouth",
   CLuid %in% "010308" ~ "Annapolis Royal",
   CLuid %in% "010309" ~ "Berwick",
   CLuid %in% "010310" ~ "Kentville",
   CLuid %in% "010311" ~ "Middleton",
   CLuid %in% "010312" ~ "Wolfville",
   CLuid %in% "020413" ~ "East Hants Corridor",
   CLuid %in% "020414" ~ "Economy/Glenholme",
   CLuid %in% "020415" ~ "Hants North",
   CLuid %in% "020416" ~ "South Colchester",
   CLuid %in% "020417" ~ "Truro and Area",
   CLuid %in% "020518" ~ "Amherst",
   CLuid %in% "020519" ~ "Cumberland North/North Shore",
   CLuid %in% "020520" ~ "South Cumberland",
   CLuid %in% "020521" ~ "Springhill",
   CLuid %in% "020622" ~ "New Glasgow/Westville/Stellarton",
   CLuid %in% "020623" ~ "Pictou West",
   CLuid %in% "030724" ~ "Antigonish",
   CLuid %in% "030725" ~ "Guysborough/Canso",
   CLuid %in% "030726" ~ "Sherbrooke",
   CLuid %in% "030827" ~ "Port Hawkesbury/L'Ardoise/Isle Madame",
   CLuid %in% "030828" ~ "Baddeck/Whycocomagh",
   CLuid %in% "030829" ~ "Cheticamp",
   CLuid %in% "030830" ~ "Dingwall",
   CLuid %in% "030831" ~ "Inverness",
   CLuid %in% "030932" ~ "New Waterford",
   CLuid %in% "030933" ~ "Sydney and Area",
   CLuid %in% "030934" ~ "Dominion/Glace Bay",
   CLuid %in% "030935" ~ "Florence/Sydney Mines/North Sydney",
   CLuid %in% "041036" ~ "Dartmouth North",
   CLuid %in% "041037" ~ "Dartmouth South",
   CLuid %in% "041038" ~ "Dartmouth East",
   CLuid %in% "041039" ~ "Cole Harbour/Eastern Passage",
   CLuid %in% "041040" ~ "Preston/Lawrencetown/Lake Echo",
   CLuid %in% "041141" ~ "Tantallon/Timberlea/SMB",
   CLuid %in% "041142" ~ "Sambro Rural Loop",
   CLuid %in% "041143" ~ "Armdale/Spryfield/Herring Cove",
   CLuid %in% "041144" ~ "Halifax Needham",
   CLuid %in% "041145" ~ "Halifax Chebucto",
   CLuid %in% "041146" ~ "Fairview",
   CLuid %in% "041147" ~ "Clayton Park",
   CLuid %in% "041148" ~ "Halifax Citadel",
   CLuid %in% "041249" ~ "Bedford/Hammonds Plains",
   CLuid %in% "041250" ~ "Sackville South",
   CLuid %in% "041251" ~ "Sackville North and Area",
   CLuid %in% "041252" ~ "Fall River and Area",
   CLuid %in% "041353" ~ "Eastern Shore/Musquodoboit",
   CLuid %in% "041454" ~ "West Hants"
  ),
  CHNuid = substr(CLuid, 1, 4),
  CHNName = case_when(
   CHNuid %in% "0101" ~ "Lunenburg & Queens",
   CHNuid %in% "0102" ~ "Yarmouth, Shelburne, & Digby",
   CHNuid %in% "0103" ~ "Annapolis and Kings",
   CHNuid %in% "0204" ~ "Colchester East Hants",
   CHNuid %in% "0205" ~ "Cumberland",
   CHNuid %in% "0206" ~ "Pictou",
   CHNuid %in% "0307" ~ "Antigonish & Guysborough",
   CHNuid %in% "0308" ~ "Inverness, Victoria, & Richmond",
   CHNuid %in% "0309" ~ "Cape Breton",
   CHNuid %in% "0410" ~ "Dartmouth & Southeastern",
   CHNuid %in% "0411" ~ "Halifax Peninsula & Chebucto",
   CHNuid %in% "0412" ~ "Bedford & Sackville",
   CHNuid %in% "0413" ~ "Eastern Shore Musquodoboit",
   CHNuid %in% "0414" ~ "West Hants"
  )
 ) %>%
 ### keep only complete data ----
 filter(!is.na(CLuid))

#-------------------
# DASHBOAD DATA ----
#-------------------

## By diagnosis ----

tmp_ano_diag <- tmp_ano %>%
 select("CaseID", "CDuid","CLuid", "CLName","CHNuid","CHNName","HRuid", "HRename", "Birth_Year", "Alcohol_Use",
        "Cannabis_Use", "diab", "bmipp", "smoker", "matage", "SrceIDs",
        #"Link_Source",
        "Diags", "cat_tier4", "BTOUTCOM", "SexNum"
 ) %>%
 # Keep only categories that will bee shown in the dashboard
 filter(!is.na(cat_tier4)) %>%
 unique() %>%
 mutate(#Diags = substr(Diags, 1,3),
  cat = cat_tier4) %>%
 select("CaseID", "CDuid","CLuid", "CLName","CHNuid","CHNName","HRuid", "HRename", "Birth_Year", "Alcohol_Use",
        "Cannabis_Use", "diab", "bmipp", "smoker", "matage",
        "SrceIDs", "Diags", "cat", "BTOUTCOM", "SexNum") %>%
 distinct() %>%
 arrange(CDuid, Birth_Year, Diags)

## By category ----

tmp_ano_cat <- tmp_ano %>%
 select("CaseID", #"BIRTHID", "CONTCTID", "MOTHERID", "Post_Code",
        #"Prov_Birth", "Prov_Res",
        "CDuid","CLuid", "CLName","CHNuid","CHNName", "HRuid", "HRename", #"Birth_Date",
        "Birth_Year", "Alcohol_Use",
        "Cannabis_Use", "diab", "bmipp", "smoker", "matage", #"Link_Source",
        "SrceIDs", "cat_tier3", "BTOUTCOM", "SexNum") %>%
 # Keep only categories that will bee shown in the dashboard
 filter(!is.na(cat_tier3)) %>%
 unique() %>%
 mutate(Diags = case_when(
  str_detect(tolower(cat_tier3), "neural") ~ "Q101",
  str_detect(tolower(cat_tier3), "nervous") ~ "Q102",
  str_detect(tolower(cat_tier3), "sense") ~ "Q103",
  str_detect(tolower(cat_tier3), "heart") ~ "Q104",
  str_detect(tolower(cat_tier3), "clefts") ~ "Q105",
  str_detect(tolower(cat_tier3), "gastrointestinal") ~ "Q106",
  str_detect(tolower(cat_tier3), "genital") ~ "Q107",
  str_detect(tolower(cat_tier3), "urinary") ~ "Q108",
  str_detect(tolower(cat_tier3), "hip") ~ "Q109",
  str_detect(tolower(cat_tier3), "limb") ~ "Q110",
  str_detect(tolower(cat_tier3), "abdominal") ~ "Q111",
  str_detect(tolower(cat_tier3), "chromosomal") ~ "Q112"
 ),
 cat = cat_tier3) %>%
 select("CaseID", "CDuid","CLuid", "CLName","CHNuid","CHNName","HRuid", "HRename", "Birth_Year", "Alcohol_Use",
        "Cannabis_Use", "diab", "bmipp", "smoker", "matage",
        "SrceIDs", "Diags", "cat", "BTOUTCOM", "SexNum") %>%
 distinct() %>%
 arrange(CDuid, Birth_Year, Diags)

## By everything ----

tmp_ano_all <- tmp_ano %>%
 select("CaseID", #"BIRTHID", "CONTCTID", "MOTHERID", "Post_Code",
        #"Prov_Birth", "Prov_Res",
        "CDuid","CLuid", "CLName","CHNuid","CHNName","HRuid", "HRename", #"Birth_Date",
        "Birth_Year", "Alcohol_Use",
        "Cannabis_Use", "diab", "bmipp", "smoker", "matage",# "Link_Source",
        "SrceIDs", "cat_tier3", "BTOUTCOM", "SexNum") %>%
 # Keep only categories that will bee shown in the dashboard
 filter(!is.na(cat_tier3)) %>%
 mutate(cat = "All conditions",
        Diags = "Q999") %>%
 unique() %>%
 select("CaseID", "CDuid","CLuid", "CLName","CHNuid","CHNName","HRuid", "HRename", "Birth_Year", "Alcohol_Use",
        "Cannabis_Use", "diab", "bmipp", "smoker", "matage",
        "SrceIDs", "Diags", "cat", "BTOUTCOM", "SexNum") %>%
 distinct() %>%
 arrange(CDuid, Birth_Year, Diags)

final_ano <- bind_rows(
 tmp_ano_all,
 tmp_ano_cat,
 tmp_ano_diag
) %>%
 arrange(CDuid, Birth_Year, Diags)

## Add birth info to anomaly data

final_ano <- merge(
 final_ano,
 tmp_dta_brth_cd %>%
  rename(count_brth_cd = count_brth),
 by.x = c("CDuid","Birth_Year"),
 by.y = c("CDuid", "BrthYear"),
 all.x = TRUE
) %>% merge(
  tmp_dta_brth_cl %>%
   rename(count_brth_cl = count_brth),
  by.x = c("CLuid", "CLName","Birth_Year"),
  by.y = c("CLuid", "CLName","BrthYear"),
  all.x = TRUE
 ) %>%
 merge(
  tmp_dta_brth_chn %>%
   rename(count_brth_chn = count_brth),
  by.x = c("CHNuid", "CHNName","Birth_Year"),
  by.y = c("CHNuid", "CHNName","BrthYear"),
  all.x = TRUE
 ) %>%
 merge(
  tmp_dta_brth_hr %>%
   rename(count_brth_hr = count_brth),
  by.x = c("HRuid", "HRename","Birth_Year"),
  by.y = c("HRuid", "HRename","BrthYear"),
  all.x = TRUE
 )


# Export files as .csv and .parquet

write_csv(final_ano,
          paste0(getwd(), "/data/Anomaly.csv"))

write_parquet(final_ano,
              paste0(getwd(), "/data/Anomaly.parquet"))








