##-------- Births ---------------
## Live births
## Stillbirths
## Termination of pregnancy (ToP)

dta <- readr::read_csv("H:\\RCP\\RCP_Data\\TeixeiEC\\NSAtleePD\\data\\NSAtleePD.csv") %>%
  setDT() %>%
  .[,`:=` (BTBrthDT = as.Date(BTBrthDT),
           BTDethDT = as.Date(BTDethDT))]

## PCCF = Postal Code Conversion File

pccf <- read.csv("H:\\RCP\\RCP_Data\\TeixeiEC\\PCCF.csv")

# Anomaly data

anom <- read.csv("H:\\RCP\\RCP_Data\\TeixeiEC\\NS_Maps\\XPort22V1.csv")
anom$Birth_Date <- as.Date(as.character(anom$Birth_Date), format = "%d/%m/%Y")
anom$Mat_DoB <- as.Date(as.character(anom$Mat_DoB), format = "%d/%m/%Y")
anom$DeathD8 <- as.Date(as.character(anom$DeathD8), format = "%d/%m/%Y")

anom[,c(16:283)] <- sapply(anom[,c(16:283)], as.character)

## transform to long format
## keeping only Q codes with diagnostics
## create ICD10 categories

anom_l = anom %>%
  tidyr::pivot_longer(cols = ADiags1:APrfxs67,
                      names_to = c("set",".value"),
                      names_pattern = "(.)([[:alpha:]]+)") %>%
  # Keep only anomalies starting with Q
  # Keep only postal codes that are not blank or missing
  # Keep only patients living in NS
  # Keep only anomalies that are not blank or missing
  dplyr::filter(
    grepl("^Q", Diags),
    !Post_Code %in% c(""," ", NA),
    # !SGC_Res %in% c(""," ", NA),
    # Prov_res %in% 12,
    !Diags %in% c("", " ", NA)) %>%
  # dplyr::select("Post_Code", "CaseID","Prov_Birth","Prov_res","SGC_Res","Birth_Date","Diags","SexNum") %>%
  dplyr::mutate(BrthYear = lubridate::year(Birth_Date),
                CD_UID = substr(SGC_Res,1,4),
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
                  grepl("^Q35", Diags) ~ "Cleft palate",
                  grepl("^Q36", Diags) ~ "Cleft lip",
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
                                   paste0(substr(Diags,1,3),
                                          " - ",
                                          cat_tier2),
                                   paste0(Diags,
                                          " - ",
                                          cat_tier2)),
                sentinel = dplyr::case_when(
                  grepl("^Q390", Diags) ~ "Atresia of oesophagus without fistula",
                  grepl("^Q428", Diags) ~ "Congenital absence, atresia and stenosis of other parts of large intestine",
                  grepl("^Q423", Diags) ~ "Congenital absence, atresia and stenosis of anus without fistula",
                  grepl("^Q792", Diags) ~ "Exomphalus",
                  grepl("^Q00|Q05", Diags) ~ "Anencephaly / Spina bifida",
                  grepl("^Q03", Diags) ~ "Congenital hydrocephalus",
                  grepl("^Q793", Diags) ~ "Gastroschisis",
                  grepl("^Q212", Diags) ~ "Atrioventricular septal defect",
                  grepl("^Q213", Diags) ~ "Tetralogy of Fallot",
                  grepl("^Q203", Diags) ~ "Transposition of great vessels",
                  grepl("^Q234", Diags) ~ "Hypoplastic left heart syndrome",
                  grepl("^Q71|Q72|Q73", Diags) ~ "Limb reduction defects",
                  grepl("^Q65", Diags) ~ "Congenital deformities of hip",
                  grepl("^Q35|Q36", Diags) ~ "cleft palate / Cleft lip",
                  grepl("^Q600|Q601", Diags) ~ "Renal agenesis",
                  grepl("^Q54|Q64", Diags) ~ "Hypospadias / Epispadias",
                  grepl("^Q90", Diags) ~ "Down's syndrome",
                  TRUE ~ "Other"
                ),
                SexNum = dplyr::case_when(
                  SexNum %in% 1 ~ "M",
                  SexNum %in% 2 ~ "F",
                  TRUE ~ "A"
                )) %>%
  dplyr::select("CaseID", "Post_Code", "Prov_Birth","Prov_res","SGC_Res",
                "Birth_Date","BrthYear","Mat_DoB","DeathD8","OutcomeNum","BWNum", "GANum","Diags","SexNum",
                "Diags","cat_tier1", "cat_tier2","sentinel", "SrceIDs") %>%
  unique() %>%
  setDT() %>%
  # .[, `:=` (pst.count_anom = .N), by = list(BrthYear, Post_Code, Diags)] %>%
  # .[,c("CaseID", "Post_Code", "SGC_Res","CD_UID","Birth_Date", "BrthYear", "Mat_DoB",
  #      "DeathD8", "OutcomeNum","SexNum", "BWNum", "GANum","Diags","cat_tier2",
  #      "sentinel", "RowIDs", "SrceIDs","pst.count_anom")] %>%
  # unique() %>%
  .[order(CaseID, Post_Code, BrthYear, cat_tier2)]

anom_l$idx_anom <- 1:nrow(anom_l)

#--------------------------------------------------------------------------------
# Keeping only the variables that will be utilized and creating new variables
# for further analysis
# Filtering data after 1986 and those that have postal code (postal is the key to join with anomaly data)
#
# NASOnly             =   'Documented Neonatal Abstinence Syndrome'
# NAS                 =   'Documented NAS or Finnegan or ESC - Diagnosis'
# NAS_MRDx            =   'NAS as most responsible diagnosis'
# NOWS                =   'NAS with opioid identified'
# Any_OAT             =   'Any opioid angonist therapy'
# NASorAny_OAT        =   'NAS or any OAT'
# RxOpioid            =   'Prscrptn opioids incl OAT'
# OpdAbuse            =   'Non-Rx use of opioids + OUD'
# MatOpUse            =   'Prscrptn or non-Rx use of opioids'
# Rx_w_NAS_Poten      =   'Prscrptn meds w/ NAS potential'
# NonRx_w_NAS_Poten   =   'Drug (ab)use not Rx w/ NAS potential'
# Alcohol_Use         =   'Any documented alcohol use during pregnancy'
# Cannabis_Use        =   'Any use of cannabis or hashish during preg'
# DrgNchmclAbus       =   'Any substance abuse or alcohol/cannabis/OAT/NAS'
# DnCAnoCan           =   'Drug aNd Chemical Abuse - ignore alcohol and cannabis'
#--------------------------------------------------------------------------------

dta_nom <- dta[,.(
  BIRTHID, CONTCTID, MOTHERID, DLPSTCOD, CountyNo,
  DLCOUNTY, BTBRTHOR,BTBrthDT, BrthYear, BTDethDT,
  BTOUTCOM, DLHSPDHA, DLHosp, BIRTHWT, GA_BEST,
  # Phenotypic sex
  BTSEX,
  # Maternal age
  DMMATAGE,
  # pre-pregnancy diabetes
  R014_00900, R014_01000, R014_01100, R014_01200,
  MO240, MO241, MO242, MO243,
  MO245, MO24501, MO24502, MO24504,
  MO246, MO24601, MO24602, MO24604,
  MO247, MO24701, MO24704,
  ME10, ME11, ME13, ME14,
  MH360,
  MG632,
  IP701,
  # gestational diabetes
  R014_01300, R014_01400,
  MO244, MO24401, MO24404, MO24409,
  MO248, MO24801, MO24802, MO24803, MO24804,
  MO249,
  IP700,
  # Obesity
  PrePBMI,
  # Smoke
  ADMITSMK, DLPRESMK, DLVS1SMK, SMOKE_20,
  # Alcohol Intake During Pregnancy (Y/N)
  ALCOHOL,
  # Chemical Abuse
  MABUSC, MABUS,
  MxSmkngPrgnc,
  # Drug Withdrawal Syndrome
  IDWDW, IDWDWC,
  # Maternal Drug Use During Present Pregnancy
  MDRUGC,
  # Convulsion due to drug withdrawal
  ICDWD,
  # Fetus and newborn affected by maternal use of alcohol, drugs
  IP043, IP044,
  # Neontl wthdrwl symp fr mtrnl use drugs adctn
  IP961,
  # Mntl & behav dsrdrs d/t use alcohol / opioids
  MF10, MF11,
  # Depression
  MF32, MF33, MF34, MF39, R016_00200,
  # Anxiety
  MF40, MF41, R016_00100,
  # the variable formerly known as Finnegan_Score
  NAS_SCREEN,
  R004_00200, # maternal prescribed anti-depressants
  R004_00500, # maternal prescribed narcotic
  R004_00700, # methadone therapy                              
  R004_00710, # Buprenorphine/naloxone (Suboxone)              
  R004_00720, # Buprenorphine (Subutex)                        
  R004_00730, # Other Opioid Agonist Therapy                   
  R004_00800, # maternal prescribed 'other psychiatric mdctn'  
  R004_01300, # maternal prescribed anxiolytic                 
  R005,       # Maternal drug and chemical use during pregnancy
  R005_00100, # Alcohol abuse (Chronic or binge - NOT social)  
  R005_00200, # Ativan                                         
  R005_00300, # Cocaine/Crack                                  
  R005_00400, # add codeine to the list                        
  R005_00500, # meperidine (Demerol)                           
  R005_00600, # Dilaudid                                       
  R005_00700, # Hash                                           
  R005_00800, # Heroin                                         
  R005_00900, # Cannabis                                       
  R005_01000, # Methadone                                      
  R005_01100, # Morphine                                       
  R005_01200, # Prescription Medication Abuse                  
  R005_01300, # Solvents                                       
  R005_01400, # Valium                                         
  R005_01500, # Other Specified Abuse                          
  R005_01600, # Oxycodone                                      
  R005_01700, # Ecstasy                                        
  R005_01800, # Alcohol abuse - Chronic                        
  R005_01900, # Alcohol abuse - Binge                          
  R005_02000, # Alcohol abuse - Unknown if Chronic or Binge    
  R005_02100, # E-Cigarette (vaping)                           
  R005_02200, # Nicotine replacement                           
  R005_02300, # Chewing tobacco                                
  R063_00600, # Convulsions(seizures) - Drug withdrawal        
  R066_08700, # morphine administration except IWK             
  R066_08800, # Narcan (naloxone) not coded for IWK infants    
  R067,       # Neonatal Abstinence Syndrome                   
  R067_00900, # heroin                                         
  R067_01000, # dilaudid                                       
  R067_01200, # meperidine                                     
  R067_01300, # methadone                                      
  R067_01400, # morphine                                       
  R067_01700, # Talwin - Pentazocine                           
  R067_02100, # oxycontin                                      
  R067_02300, # Buprenorphine (Subutex)                        
  R067_02400
)] %>%
  # live birth, stillbirth, ToP (termination of pregnancy)
  .[, `:=` (dlv = ifelse(
    !tolower(BTOUTCOM) %in% "ftd" & # live birth
      (BIRTHWT >= 500 | GA_BEST >= 20 | 
         is.na(BIRTHWT) | is.na(GA_BEST)), "lvb",
    ifelse(
      tolower(BTOUTCOM) %in% "ftd" & # stillbirth
        (BIRTHWT >= 500 | GA_BEST >= 20 | 
           is.na(BIRTHWT) | is.na(GA_BEST)), "stillbirth",
      ifelse(
        !tolower(BTOUTCOM) %in% "lvd" & GA_BEST < 20, "ToP", # ToP
        "other"
      ))))
  ] %>%
  unique() %>%
  .[, idx_244 := rowSums(.SD, na.rm = TRUE), .SDcols = names(dta)[which(grepl("MO2440.", names(dta)))]] %>%
  .[, idx_245 := rowSums(.SD, na.rm = TRUE), .SDcols = names(dta)[which(grepl("MO2450.", names(dta)))]] %>%
  .[, idx_246 := rowSums(.SD, na.rm = TRUE), .SDcols = names(dta)[which(grepl("MO2460.", names(dta)))]] %>%
  .[, idx_247 := rowSums(.SD, na.rm = TRUE), .SDcols = names(dta)[which(grepl("MO2470.", names(dta)))]] %>%
  .[, idx_248 := rowSums(.SD, na.rm = TRUE), .SDcols = names(dta)[which(grepl("MO2480.", names(dta)))]] %>%
  .[, `:=` (predm_nsapd = fcase(
    R014_00900 >=1 | R014_01000 >=1 | 
      R014_01100 >=1 | R014_01200 >=1, 1,
    default = 0
  ),
  predm_icd = fcase(
    MO240 >=1 | MO241 >=1 | MO242 >=1 | MO243 >=1 |
      MO245 >=1 | idx_245 >=1 |
      MO246 >=1 | idx_246 >=1 |
      MO247 >=1 | idx_247 >=1 |
      ME10 >=1 | ME11 >=1 | ME13 >=1 | ME14 >=1 |
      MH360 >=1 | MG632 >=1 | IP701  >=1, 1,
    default = 0
  ),
  gdm_nsapd = fcase(
    R014_01300 >=1 | R014_01400 >=1, 1,
    default = 0
  ),
  gdm_icd = fcase(
    MO244 >=1 | MO249 >=1 | MO248 >=1 |
      idx_244 >=1 | idx_248 >=1 |
      IP700  >=1, 1,
    default = 0
  ))] %>%
  .[, `:=` (prexdiab = fcase(
    predm_nsapd %in% 1 | predm_icd %in% 1, 1,
    default = 0
  ),
  gestdiab = fcase(
    gdm_nsapd %in% 1 | gdm_icd %in% 1, 1,
    default = 0
  ))] %>%
  .[, `:=` (bmipp = fcase(
    # PrePBMI < 18.5, 1,
    # 18 <= PrePBMI & PrePBMI < 25, 2,
    # 25 <= PrePBMI & PrePBMI < 30, 3,
    30 <= PrePBMI & PrePBMI < 35, 1,
    35 <= PrePBMI & PrePBMI < 40, 2,
    PrePBMI >= 40, 3,
    default = -1
  ))] %>%
  .[, smk := rowSums(.SD, na.rm = TRUE), .SDcols = c("ADMITSMK", "DLVS1SMK", "SMOKE_20")] %>%
  .[, `:=` (smoker = fcase(
    ADMITSMK >= 1 | DLVS1SMK >=1 | SMOKE_20 >= 1, 1,
    is.na(ADMITSMK) & is.na(DLVS1SMK) & is.na(DLPRESMK) & is.na(SMOKE_20), -1, 
    default = 0))] %>%
  # .[!smoker %in% 2 & !is.na(smoker)] %>%
  # remove missing and BMI < 30
  # .[!bmipp %in% 0 &
  #     !bmipp %in% c(1,2,3)] %>%
  .[, `:=` (matage = fcase(
    DMMATAGE < 20, 1,
    DMMATAGE >= 20 & DMMATAGE < 25, 2,
    DMMATAGE >= 25 & DMMATAGE < 30, 3,
    DMMATAGE >= 30 & DMMATAGE < 35, 4,
    DMMATAGE >= 35 & DMMATAGE < 40, 5,
    DMMATAGE >= 40, 6,
    default = -1
  ))] %>%
  .[, nas := rowSums(.SD, na.rm = TRUE), .SDcols = c("R067", "IP961")] %>%
  .[, mum := rowSums(.SD, na.rm = TRUE), .SDcols = c("R004_00700", "R004_00710", "R004_00720", "R004_00730", "R005_00400", "R005_00500", "R005_00600", "R005_00800", "R005_01100", "R005_01600", "MF11")] %>%
  .[,`:=` (nas_opi = fcase(
    nas >=1, 1,
    default = 0
  ),
  mum_opi = fcase(
    mum >=1, 1,
    default = 0
  ))] %>%
  .[,`:=` (noas = fcase(
    nas_opi >=1 & mum_opi >= 1, 1,
    default = 0
  ))] %>%
  .[, `:=` (diab = fcase(
    prexdiab >=1 | gestdiab >=1, 1,
    default= 0
  ))] %>%
  .[, `:=` (
    NASOnly = fcase(
      R067 >=1 | IDWDW >=1 | R063_00600 >=1 | ICDWD >= 1 | IP961 >= 1, 1,
      default= 0
    ),
    NAS_MRDx = 0)] %>%
  .[, `:=` (
    NAS = fcase(
      NASOnly >= 1 | ( NAS_SCREEN %in% c( 1, 2, 4, 5) ), 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    Any_OAT = fcase(
      R004_00700 >= 1 | R004_00710 >= 1 | R004_00720 >= 1 | R004_00730 >= 1, 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    NASorAny_OAT = fcase(
      NAS >= 1 | Any_OAT >= 1, 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    RxOpioid = fcase(
      Any_OAT >= 1 | R004_00500 >= 1 | (MDRUGC %in% "C07"), 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    OpdAbuse = fcase(
      R005_00400 >= 1 | R005_00500 >= 1 | R005_00600 >= 1 | R005_00800 >= 1 |
        R005_01000 >= 1 | R005_01100 >= 1 | R005_01600 >= 1 | MF11 >= 1 |
        (MABUSC %in% 'C02' & NAS >= 1), 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    MatOpUse = fcase(
      RxOpioid >= 1 | OpdAbuse >= 1, 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    NOWS = fcase(
      ( R067_00900 >= 1 | ( IDWDWC %in% 'HER' ) | R067_01000 >= 1 |
          R067_01200 >= 1 | ( IDWDWC %in% 'DEM' ) |
          R067_01300 >= 1 | ( IDWDWC %in% 'MTH' ) |
          R067_01400 >= 1 | ( IDWDWC %in% 'M|' ) | R067_01700 >= 1 |
          R067_02100 >= 1 | R067_02300 >= 1 | R067_02400 >= 1 ) |
        ( NAS >= 1 & MatOpUse >= 1), 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    Rx_w_NAS_Poten = fcase(
      R004_00200 >= 1 | ( MDRUGC %in% 'C04' ) |
        RxOpioid >= 1 |
        R004_00800 >= 1 | R004_01300 >= 1, 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    NonRx_w_NAS_Poten = fcase(
      OpdAbuse >= 1 | ( MABUSC %in% 'C02' ), 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    Alcohol_Use = fcase(
      R005_00100 >= 1 | R005_01800 >= 1 | R005_01900 >= 1 | R005_02000 >= 1 |
        MF10 >= 1 | IP043 >= 1 | ( MABUSC %in% 'C01' ) | ( ALCOHOL %in% 'Y' ), 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    Cannabis_Use = fcase(
      R005_00700 >= 1 | R005_00900 >= 1, 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    DrgNchmclAbus = fcase(
      MABUS >= 1 | (R005>=1 | R005_00100>=1 | R005_00200>=1 |
                      R005_00300>=1 | R005_00400>=1 | R005_00500>=1 | R005_00600>=1 |
                      R005_00700>=1 | R005_00800>=1 | R005_00900>=1 | R005_01000>=1 |
                      R005_01100>=1 | R005_01200>=1 | R005_01300>=1 | R005_01400>=1 |
                      R005_01500>=1 | R005_01600>=1 | R005_01700>=1 | R005_01800>=1 |
                      R005_01900>=1 | R005_02000>=1 | R005_02100>=1 | R005_02200>=1 |
                      R005_02300 >= 1) | NAS >= 1 | Alcohol_Use >= 1 | Cannabis_Use >= 1 |
        IP044 >= 1 | Any_OAT >= 1, 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    Substance_Use = fcase(
      DrgNchmclAbus >= 1 | MatOpUse >= 1 | 
        Rx_w_NAS_Poten  >= 1 |  NonRx_w_NAS_Poten >= 1, 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    DnCAnoCan = fcase(
      MABUS >= 1 | (R005>=1 | R005_00100>=1 | R005_00200>=1 |
                      R005_00300>=1 | R005_00400>=1 | R005_00500>=1 | R005_00600>=1 |
                      R005_00700>=1 | R005_00800>=1 | R005_00900>=1 | R005_01000>=1 |
                      R005_01100>=1 | R005_01200>=1 | R005_01300>=1 | R005_01400>=1 |
                      R005_01500>=1 | R005_01600>=1 | R005_01700>=1 | R005_01800>=1 |
                      R005_01900>=1 | R005_02000>=1 | R005_02100>=1 | R005_02200>=1 |
                      R005_02300 >= 1) | ICDWD >= 1 | R063_00600 >= 1 | IDWDW >= 1 |
        R067 >= 1 | IP043 >= 1 | IP044 >= 1 | ( NAS_SCREEN %in% c(1,2,4,5) ) |
        R004_00700 >= 1 | R004_00710 >= 1 | R004_00720 >= 1 | R004_00730 >= 1, 1, 
      default = 0
    ))] %>%
  .[, `:=` (
    drugs = fcase(
      NASOnly >= 1 | NAS >= 1 | NAS_MRDx >=1 | Any_OAT >= 1 | 
        NASorAny_OAT >= 1 | RxOpioid >= 1 | OpdAbuse >= 1 | MatOpUse >= 1 | NOWS >= 1 | 
        Rx_w_NAS_Poten >= 1 | NonRx_w_NAS_Poten >= 1 | Cannabis_Use >= 1 |
        DrgNchmclAbus >= 1 | DnCAnoCan >= 1, 1, 
      default = 0
    ))] %>%
  .[BrthYear >= min(anom_l$BrthYear) #&
      # !is.na(DLPSTCOD)
    ] %>%
  .[,`:=` (CD_UID = fcase(
    CountyNo %in% 1, 1201,
    CountyNo %in% 2, 1202,
    CountyNo %in% 3, 1203,
    CountyNo %in% 4, 1204,
    CountyNo %in% 5, 1205,
    CountyNo %in% 6, 1206,
    CountyNo %in% 7, 1207,
    CountyNo %in% 8, 1208,
    CountyNo %in% 9, 1209,
    CountyNo %in% 10, 1210,
    CountyNo %in% 11, 1211,
    CountyNo %in% 12, 1212,
    CountyNo %in% 13, 1213,
    CountyNo %in% 14, 1214,
    CountyNo %in% 15, 1215,
    CountyNo %in% 16, 1216,
    CountyNo %in% 17, 1217,
    CountyNo %in% 18, 1218
  ))] %>% 
  .[, `:=` (cd.count_dlv = .N), by = list(BrthYear, CD_UID, dlv)] %>%
  unique()


# Counting births by county ID, birth year
cd_lvb <- dta_nom[,.(CD_UID, BrthYear, dlv)] %>% 
  .[, `:=` (cd.count_dlv = .N), by = list(BrthYear, CD_UID, dlv)] %>%
  .[order(BrthYear, CD_UID, dlv)] %>% 
  unique() %>% 
  .[!is.na(CD_UID)]
cd_lvb$idx_cdlvb <- 1:nrow(cd_lvb)

## Bring Atlee information to patients with anomaly

dta_anom <- merge(anom_l,
                  dta_nom,
                  by.x = c("Post_Code",
                    "Birth_Date",
                    "BrthYear","SexNum",
                    "OutcomeNum",
                    "DeathD8",
                    "BWNum"),
                  by.y = c("DLPSTCOD",
                    "BTBrthDT",
                    "BrthYear",
                    "BTSEX",
                    "BTBRTHOR", 
                    "BTDethDT",
                    "BIRTHWT"),
                  all.x = TRUE) %>%
  .[order(idx_anom)] %>%
  .[,c("BIRTHID", "MOTHERID", "CONTCTID", "CaseID", "DLHSPDHA", "DLHosp","BTOUTCOM",
       "Post_Code", "SGC_Res", "CD_UID", "DLCOUNTY","Birth_Date", "BrthYear", "SexNum", "Mat_DoB", "GANum", 
       "Diags","cat_tier2", "sentinel", "idx_anom", "GA_BEST",
       "DMMATAGE", "R004_00200","R004_00800", "R004_01300", "MDRUGC",
       "smoker","bmipp","matage","diab", "NASOnly", "NAS_MRDx", "NAS", "Any_OAT", "NASorAny_OAT", 
       "RxOpioid", "OpdAbuse", "MatOpUse", "NOWS", "Rx_w_NAS_Poten", 
       "NonRx_w_NAS_Poten", "Alcohol_Use", "Cannabis_Use", "DrgNchmclAbus", 
       "Substance_Use", "DnCAnoCan", "drugs","SrceIDs")
  ]

## Bring the county ID to the anomaly data
# anom_l <- merge(anom_l,
#            pccf[,c(2,5,6)],
#            by.x = c("Post_Code"#, "SGC_Res"
#            ),
#            by.y = c("Postal_Code"#, "CSDUID"
#            ),
#            all.x = TRUE) %>% 
#   unique(by = "idx_anom")

## Since not all county codes could be matched we will use another source
## of information
## CSD can change between census (https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/Definition-eng.cfm?ID=geo012)

# those that weren't matched
d1 <- dta_anom[is.na(dta_anom$BIRTHID), .(idx_anom)] %>% 
  unique()

# those that were matched
d2 <- dta_anom[!is.na(dta_anom$BIRTHID)] %>% 
  unique()

#== try again 2 ====
#== in case the postal code is different
 
d1 <- merge(anom_l[anom_l$idx_anom %in% d1$idx_anom],
            dta_nom,
            by.x = c(#"Post_Code",
              "Birth_Date",
              "BrthYear",
              "SexNum",
              "OutcomeNum",
              "DeathD8",
              "BWNum"),
            by.y = c(#"DLPSTCOD",
              "BTBrthDT",
              "BrthYear",
              "BTSEX",
              "BTBRTHOR", 
              "BTDethDT",
              "BIRTHWT"),
            all.x = TRUE) %>%
  .[order(idx_anom)] %>%
  .[,c("BIRTHID", "MOTHERID", "CONTCTID", "CaseID", "DLHSPDHA", "DLHosp","BTOUTCOM",
       "Post_Code", "SGC_Res", "CD_UID","DLCOUNTY", "Birth_Date", "BrthYear", "SexNum", "Mat_DoB", "GANum", 
       "Diags","cat_tier2", "sentinel", "idx_anom", "GA_BEST",
       "DMMATAGE", "R004_00200","R004_00800", "R004_01300", "MDRUGC",
       "smoker","bmipp","matage","diab", "NASOnly", "NAS_MRDx", "NAS", "Any_OAT", "NASorAny_OAT", 
       "RxOpioid", "OpdAbuse", "MatOpUse", "NOWS", "Rx_w_NAS_Poten", 
       "NonRx_w_NAS_Poten", "Alcohol_Use", "Cannabis_Use", "DrgNchmclAbus", 
       "Substance_Use", "DnCAnoCan", "drugs","SrceIDs")
  ]

## put together those matched and uniquely identified
d2 <- rbind(d2, unique(d1[!is.na(d1$BIRTHID) &
                            !duplicated(d1$idx_anom)])) %>% 
  .[order(idx_anom)]

## those that weren't uniquely matched
d1 <- dta_anom[!dta_anom$idx_anom %in% d2$idx_anom]

#== try again 3 ====
# death d8 is missing

d1 <- merge(anom_l[anom_l$idx_anom %in% d1$idx_anom],
            dta_nom,
            by.x = c("Post_Code",
              "Birth_Date",
              "BrthYear",
              "SexNum",
              "OutcomeNum",
              #"DeathD8",
              "BWNum"),
            by.y = c("DLPSTCOD",
              "BTBrthDT",
              "BrthYear",
              "BTSEX",
              "BTBRTHOR", 
              #"BTDethDT",
              "BIRTHWT"),
            all.x = TRUE) %>% 
  .[order(idx_anom)] %>%
  .[,c("BIRTHID", "MOTHERID", "CONTCTID", "CaseID", "DLHSPDHA", "DLHosp","BTOUTCOM",
       "Post_Code", "SGC_Res", "CD_UID","DLCOUNTY", "Birth_Date", "BrthYear", "SexNum", "Mat_DoB", "GANum", 
       "Diags","cat_tier2", "sentinel", "idx_anom", "GA_BEST",
       "DMMATAGE", "R004_00200","R004_00800", "R004_01300", "MDRUGC",
       "smoker","bmipp","matage","diab", "NASOnly", "NAS_MRDx", "NAS", "Any_OAT", "NASorAny_OAT", 
       "RxOpioid", "OpdAbuse", "MatOpUse", "NOWS", "Rx_w_NAS_Poten", 
       "NonRx_w_NAS_Poten", "Alcohol_Use", "Cannabis_Use", "DrgNchmclAbus", 
       "Substance_Use", "DnCAnoCan", "drugs","SrceIDs")
  ]

## put together those matched and uniquely identified
d2 <- rbind(d2, unique(d1[!is.na(d1$BIRTHID) &
                            !duplicated(d1$idx_anom)])) %>% 
  .[order(idx_anom)]

## those that weren't uniquely matched
d1 <- dta_anom[!dta_anom$idx_anom %in% d2$idx_anom]

#== try again 4 ====
# BIRTHWT is missing

d1 <- merge(anom_l[anom_l$idx_anom %in% d1$idx_anom],
            dta_nom,
            by.x = c("Post_Code",
              "Birth_Date",
              "BrthYear",
              "SexNum"
              #"OutcomeNum",
              #"DeathD8",
              #"BWNum"
              ),
            by.y = c("DLPSTCOD",
              "BTBrthDT",
              "BrthYear",
              "BTSEX"
              #"BTBRTHOR", 
              #"BTDethDT",
              #"BIRTHWT"
              ),
            all.x = TRUE) %>%
  .[order(idx_anom)] %>%
  .[,c("BIRTHID", "MOTHERID", "CONTCTID", "CaseID", "DLHSPDHA", "DLHosp","BTOUTCOM",
       "Post_Code", "SGC_Res", "CD_UID","DLCOUNTY", "Birth_Date", "BrthYear", "SexNum", "Mat_DoB", "GANum", 
       "Diags","cat_tier2", "sentinel", "idx_anom", "GA_BEST",
       "DMMATAGE", "R004_00200","R004_00800", "R004_01300", "MDRUGC",
       "smoker","bmipp","matage","diab", "NASOnly", "NAS_MRDx", "NAS", "Any_OAT", "NASorAny_OAT", 
       "RxOpioid", "OpdAbuse", "MatOpUse", "NOWS", "Rx_w_NAS_Poten", 
       "NonRx_w_NAS_Poten", "Alcohol_Use", "Cannabis_Use", "DrgNchmclAbus", 
       "Substance_Use", "DnCAnoCan", "drugs","SrceIDs")
  ]

## put together those matched and uniquely identified
d2 <- rbind(d2, unique(d1[!is.na(d1$BIRTHID) &
                            !duplicated(d1$idx_anom)])) %>% 
  .[order(idx_anom)]

## those that weren't uniquely matched
d1 <- dta_anom[!dta_anom$idx_anom %in% d2$idx_anom]

#== try again 5 ====
#== sex is different between datasets
# caseID=345 (example)

d1 <- merge(anom_l[anom_l$idx_anom %in% d1$idx_anom],
            dta_nom,
            by.x = c("Post_Code",
                     "Birth_Date",
                     "BrthYear",
                     #"SexNum"
                     #"OutcomeNum",
                     #"DeathD8",
                     "BWNum"
            ),
            by.y = c("DLPSTCOD",
                     "BTBrthDT",
                     "BrthYear",
                     #"BTSEX",
                     #"BTBRTHOR", 
                     #"BTDethDT",
                     "BIRTHWT"
            ),
            all.x = TRUE) %>%
  .[order(idx_anom)] %>%
  .[,c("BIRTHID", "MOTHERID", "CONTCTID", "CaseID", "DLHSPDHA", "DLHosp","BTOUTCOM",
       "Post_Code", "SGC_Res", "CD_UID","DLCOUNTY", "Birth_Date", "BrthYear", "SexNum", "Mat_DoB", "GANum", 
       "Diags","cat_tier2", "sentinel", "idx_anom", "GA_BEST",
       "DMMATAGE", "R004_00200","R004_00800", "R004_01300", "MDRUGC",
       "smoker","bmipp","matage","diab", "NASOnly", "NAS_MRDx", "NAS", "Any_OAT", "NASorAny_OAT", 
       "RxOpioid", "OpdAbuse", "MatOpUse", "NOWS", "Rx_w_NAS_Poten", 
       "NonRx_w_NAS_Poten", "Alcohol_Use", "Cannabis_Use", "DrgNchmclAbus", 
       "Substance_Use", "DnCAnoCan", "drugs","SrceIDs")
  ]

## put together those matched and uniquely identified
d2 <- rbind(d2, unique(d1[!is.na(d1$BIRTHID) &
                            !duplicated(d1$idx_anom)])) %>% 
  .[order(idx_anom)]

## those that weren't uniquely matched
d1 <- dta_anom[!dta_anom$idx_anom %in% d2$idx_anom]

#== try again 6 ====
#== postal and brthdate only

d1 <- merge(anom_l[anom_l$idx_anom %in% d1$idx_anom],
            dta_nom,
            by.x = c("Post_Code",
                     "Birth_Date",
                     "BrthYear"
                     #"SexNum",
                     #"OutcomeNum",
                     #"DeathD8",
                     #"BWNum",
                     #"GANum"
            ),
            by.y = c("DLPSTCOD",
                     "BTBrthDT",
                     "BrthYear"
                     #"BTSEX",
                     #"BTBRTHOR", 
                     #"BTDethDT",
                     #"BIRTHWT",
                     #"GA_BEST"
            ),
            all.x = TRUE) %>%
  .[order(idx_anom)] %>%
  .[,c("BIRTHID", "MOTHERID", "CONTCTID", "CaseID", "DLHSPDHA", "DLHosp","BTOUTCOM",
       "Post_Code", "SGC_Res", "CD_UID","DLCOUNTY","Birth_Date", "BrthYear", "SexNum", "Mat_DoB", "GANum", 
       "Diags","cat_tier2", "sentinel", "idx_anom", "GA_BEST",
       "DMMATAGE", "R004_00200","R004_00800", "R004_01300", "MDRUGC",
       "smoker","bmipp","matage","diab", "NASOnly", "NAS_MRDx", "NAS", "Any_OAT", "NASorAny_OAT", 
       "RxOpioid", "OpdAbuse", "MatOpUse", "NOWS", "Rx_w_NAS_Poten", 
       "NonRx_w_NAS_Poten", "Alcohol_Use", "Cannabis_Use", "DrgNchmclAbus", 
       "Substance_Use", "DnCAnoCan", "drugs","SrceIDs")
  ]

# ## put together those matched and uniquely identified
# names(d1)[which(tolower(names(d1)) %in% "ganum")[2]] <- "GA_BEST"

d2 <- rbind(d2, unique(d1[!is.na(d1$BIRTHID) &
                            !duplicated(d1$idx_anom)])) %>% 
  .[order(idx_anom)]

## those that weren't uniquely matched
d1 <- dta_anom[!dta_anom$idx_anom %in% d2$idx_anom]

## Selecting variables, removing possible CD missingness and

anom_cd <- d2 %>% 
  .[#!is.na(CD_UID)
    ,
    c("CaseID","BIRTHID","MOTHERID","CONTCTID","CD_UID", "DLCOUNTY","Birth_Date",
      "BrthYear", "DLHSPDHA", "DLHosp","SexNum", "Diags","cat_tier2", "sentinel",
      "GA_BEST", "DMMATAGE", "R004_00200", "R004_00800", 
      "R004_01300", "MDRUGC", "smoker", "bmipp", "matage", "diab", 
      "NASOnly", "NAS_MRDx", "NAS", "Any_OAT", "NASorAny_OAT", "RxOpioid", 
      "OpdAbuse", "MatOpUse", "NOWS", "Rx_w_NAS_Poten", "NonRx_w_NAS_Poten", 
      "Alcohol_Use", "Cannabis_Use", "DrgNchmclAbus", "Substance_Use", 
      "DnCAnoCan", "drugs", "SrceIDs")] %>% 
  unique() %>%
  .[order(CaseID, BIRTHID,MOTHERID,CONTCTID, CD_UID, BrthYear, cat_tier2)] %>% 
  # .[CD_UID > 1200 & CD_UID < 1299] %>% 
  .[,`:=` (DLCOUNTY = factor(DLCOUNTY,
                            #levels = unique(as.character(d2$DLCOUNTY)),
                            labels = c(
                              'ANN' = 'Annapolis',
                              'ANT' = 'Antigonish',
                              'CBT' = 'Cape Breton',
                              'COL' = 'Colchester',
                              'CUM' = 'Cumberland',
                              'DIG' = 'Digby',
                              'GUY' = 'Guysborough',
                              'HAN' = 'Hants',
                              'HFX' = 'Halifax',
                              'INV' = 'Inverness',
                              'KIN' = 'Kings',
                              'LUN' = 'Lunenburg',
                              'NBK' = 'New Brunswick',
                              'NFD' = 'Newfoundland',
                              'OTR' = 'Not Atlantic',
                              'PEI' = 'Prince Edward Island',
                              'PIC' = 'Pictou',
                              'QNS' = 'Queens',
                              'RMD' = 'Richmond',
                              'SHB' = 'Shelburne',
                              'VIC' = 'Victoria',
                              'YAR' = 'Yarmouth')),
           DLHosp = factor(DLHosp,
                           labels = c(
                             "-15" = 'Sackville Memorial',
                             "-14" = 'Queen Elizabeth',
                             "-12" = 'Moncton Hospital',
                             "-11" = 'George Dumont',
                             "-10" = 'Chaleur Regional',
                             "-5" = 'Midwife Delivery At Home',
                             "-3" = 'Outborn',
                             "-2" = 'Planned Home Birth',
                             "-1" = 'Unplanned Birth',
                             "11" = 'Aberdeen Hospital',
                             "13" = 'Annapolis Community',
                             "14" = 'Health Services Assoc Of SS',
                             "15" = 'Buchanan Memorial',
                             "18" = 'Colchester Regional',
                             "20" = 'Digby General',
                             "21" = 'Eastern Kings Memorial',
                             "23" = 'Eastern Shore Memorial',
                             "24" = "Fishermen's Memorial",
                             "30" = 'Cumberland Regional Health Care Centre',
                             "34" = 'Inverness Consolidated',
                             "37" = 'Hants Community',
                             "38" = "Queen's General",
                             "39" = 'Roseway Hospital',
                             "43" = "St. Martha's Regional",
                             "47" = 'Sacred Heart Memorial',
                             "48" = "Soldier's Memorial",
                             "50" = 'Sutherland-Harris',
                             "53" = 'Victoria County Memorial',
                             "55" = 'Western Kings Mem. Health C.',
                             "56" = 'Western Regional Health Centre',
                             "63" = 'New Waterford Consolidated',
                             "65" = 'Dartmouth General',
                             "67" = 'Valley Regional',
                             "85" = 'QE II - All Sites',
                             "86" = 'IWK Grace',
                             "87" = 'New C.B. Regional')))]

## export datasets

write.csv(cd_lvb,
          "H:/RCP/RCP_Data/TeixeiEC/Anomalies/anomaly-app-overview/data/cd_birth.csv")

write.csv(anom_cd,
          "H:/RCP/RCP_Data/TeixeiEC/Anomalies/anomaly-app-overview/data/cd_anomaly.csv")



