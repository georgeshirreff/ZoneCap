library(tidyverse)

load(verbose = T, file = "INSEEage/Cleaned/dep_pop.RData")
load(verbose = T, file = "INSEEage/Cleaned/reg_pop.RData")
load(verbose = T, file = "INSEEage/Cleaned/dep_pop_age.RData")
load(verbose = T, file = "INSEEage/Cleaned/reg_pop_age.RData")

load(verbose = T, file = "INSEEage/Cleaned/pmsi_postcode.RData")


DiagCat_translation <- openxlsx::read.xlsx("ATIH/diag_translation.xlsx")


pmsi_countries <- readxl::read_excel("ATIH/PMSIcode_pays.xlsx") %>% 
  {bind_rows(tibble(PMSIcode = "99999", Pays = "UNKNOWN", Subregion = 0), 
             tibble(PMSIcode = "98000", Pays = "MONACO", Subregion = 0), 
             .)} %>% 
  filter(Subregion == 0)



reg_levels = c("84", "27", "11", "44", "93", "76", "75", "53", "52", "32", "24", "28", "94", "01", "02", "03", "04", "06", "TOM", "Others", "Reg missing")
reg_levels_met = reg_levels[1:13]

dep_levels_met = dep_reg %>% 
  filter(RegCode != "TOM", 
         !(RegCode %in% paste0("0", 1:6)), 
         DepCode != "20") %>% 
  pull(DepCode) %>% sort

age20_vec = c("0-18", "19-40", "41-60", "61-80", "81+") 


##### reading data #####

reg_year_all_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_ALL_REG_NoAge.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

reg_year_finess_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_FINESS_REG_NoAge.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

reg_age20year_all_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_ALL_REG_Age.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

reg_age20year_finess_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_FINESS_REG_Age.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

reg_age20year_hcl_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/RegHCL/mco_MET_HCL_", .x, "_REG Age.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}


dep_year_all_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_ALL_DEP_NoAge.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

dep_year_finess_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_FINESS_DEP_NoAge.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

dep_age20year_all_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_ALL_DEP_Age.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

dep_age20year_finess_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_FINESS_DEP_Age.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}


pmsi_year_all_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_ALL_PMSI_NoAge.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

pmsi_year_finess_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_FINESS_PMSI_NoAge.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

pmsi_age20year_all_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_ALL_PMSI_Age.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

pmsi_age20year_finess_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESS/mco_MET_", .x, "_FINESS_PMSI_Age.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}


depDOM_year_all_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESSdom/mco_DOMTOM_", .x, "_ALL_DEP_NoAge.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

depDOM_year_finess_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESSdom/mco_DOMTOM_", .x, "_FINESS_DEP_NoAge.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}


depEVERY_year_all_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/RegDep_NoAges/mco_", .x, "_DEP.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}


pmsi_year_finessgeo_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESSGeo/mco_MET_", .x, "_FINESS-Geo_PMSI.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

dep_year_finessgeo_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESSGeo/mco_MET_", .x, "_FINESS-Geo_DEP.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

pmsiDOM_year_finessgeo_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESSGeo/mco_DOM_", .x, "_FINESS-Geo_PMSI.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

depDOM_year_finessgeo_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FINESSGeo/mco_DOM_", .x, "_FINESS-Geo_DEP.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}


dep_year_diag_all_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/DiagCat/mco_MET_", .x, "_DiagCat_DEP.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}

dep_year_diag_finess_raw <- purrr::map(2016:2025, ~{
  readxl::read_excel(paste0("ATIH/DiagCat/mco_MET_", .x, "_FINESS-DiagCat_DEP.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)}


gr <- expand.grid(list(Year = 2016:2025, age_group = age_vec))
dep_ageyear_all_raw <- purrr::map2(.x = gr$Year, .y = gr$age_group, ~{
  readxl::read_excel(paste0("ATIH/FIRESPages/mco_MET_", .y, "_", .x, "_ALL_DEP.xlsx"), skip = 1) %>% 
    mutate(Year = .x, 
           age_group = .y)
}) %>% 
  {do.call("rbind", .)}

dep_ageyear_hcl_raw <- purrr::map2(.x = gr$Year, .y = gr$age_group, ~{
  readxl::read_excel(paste0("ATIH/FIRESPages/mco_MET_", .y, "_", .x, "_FINESSHCL_DEP.xlsx"), skip = 1) %>% 
    mutate(Year = .x, 
           age_group = .y) %>% 
    select(-starts_with("Finess"))
}) %>% 
  {do.call("rbind", .)}


dep_age0year_all_raw <- purrr::map(.x = 2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FIRESPages/mco_MET_", "0_", .x, "_ALL_DEP.xlsx"), skip = 1) %>% 
    mutate(Year = .x, 
           age_group = 0)
}) %>% 
  {do.call("rbind", .)}

dep_age0year_hcl_raw <- purrr::map(.x = 2016:2025, ~{
  readxl::read_excel(paste0("ATIH/FIRESPages/mco_MET_", "0_", .x, "_FINESSHCL_DEP.xlsx"), skip = 1) %>% 
    mutate(Year = .x, 
           age_group = 0)
}) %>% 
  {do.call("rbind", .)}




gr_ara <- expand.grid(list(Year = 2019:2025, age_group = age_vec))
pmsi_ageyear_all_raw <- purrr::map2(.x = gr_ara$Year, .y = gr_ara$age_group, ~{
  readxl::read_excel(paste0("ATIH/FIRESPages/mco_MET_", .y, "_", .x, "_ALL_PMSI.xlsx"), skip = 1) %>% 
    mutate(Year = .x, 
           age_group = .y)
}) %>% 
  {do.call("rbind", .)}

pmsi_ageyear_hcl_raw <- purrr::map2(.x = gr_ara$Year, .y = gr_ara$age_group, ~{
  readxl::read_excel(paste0("ATIH/FIRESPages/mco_MET_", .y, "_", .x, "_FINESSHCL_PMSI.xlsx"), skip = 1) %>% 
    mutate(Year = .x, 
           age_group = .y) %>% 
    select(-starts_with("Finess"))
}) %>% 
  {do.call("rbind", .)}

##### cleaning functions #####

clean_RegCode <- function(tib){
  tib %>% mutate(RegCode = case_when(RegCode == "Others" ~ "Others", #manage the RegCode
                               T ~ gsub("\\D", "", RegCode)))
}

clean_FinessHosp <- function(tib){
  tib %>% 
    filter(!(grepl("930300025", FINESS_Hosp) & grepl("PARIS", FINESS_Hosp))) %>% #removes a duplicated entry
    mutate(FINESS = gsub("([0-9]+) - .*", "\\1", FINESS_Hosp)) %>% 
    group_by(FINESS) %>% 
    mutate(FINESS_Hosp = unique(FINESS_Hosp[Year == max(Year)])) %>% 
    ungroup %>% 
    select(-FINESS)
  
  # tib %>% mutate(FINESS_Hosp = case_when(FINESS_Hosp == "310781406 - CHR TOULOUSE" ~ "310781406 - CHU TOULOUSE", 
  #                                        FINESS_Hosp == "010007987 - CH HAUTEVILLE-LOMPNES" ~ "010007987 - CH HAUTEVILLE",
  #                                        FINESS_Hosp == "760024042 - CHIC ELBEUF LOUVIERS" ~ "760024042 - CHIC ELBEUF LOUVIERS VAL DE REUIL"
  #                                        T ~ FINESS_Hosp))
}
  
  
clean_GeoHosp <- function(tib){
  tib %>% 
    filter(!(grepl("930300025", Geo_Hosp) & grepl("PARIS", Geo_Hosp))) %>% #removes a duplicated entry
    mutate(Geo = gsub("([0-9]+) - .*", "\\1", Geo_Hosp)) %>% 
    group_by(FINESS_Hosp, Geo) %>% 
    mutate(Geo_Hosp = unique(Geo_Hosp[Year == max(Year)])) %>% 
    ungroup %>% 
    select(-Geo) %>% 
    mutate(Geo_Hosp = gsub(" - HCL$", "", Geo_Hosp))
  
  # tib %>% 
  #   mutate(Geo_Hosp = gsub(" - HCL$", "", Geo_Hosp)) %>% 
  #   mutate(Geo_Hosp = ifelse(grepl("690783154", Geo_Hosp), "690783154 - HÔPITAL EDOUARD HERRIOT", Geo_Hosp)) %>% 
  #   mutate(Geo_Hosp = ifelse(grepl("690784137", Geo_Hosp), "690784137 - HÔPITAL LYON SUD", Geo_Hosp)) %>% 
  #   mutate(Geo_Hosp = ifelse(grepl("690784152", Geo_Hosp), "690784152 - HÔPITAL CROIX-ROUSSE", Geo_Hosp)) %>% 
  #   mutate(Geo_Hosp = ifelse(grepl("690784178", Geo_Hosp), "690784178 - HÔPITAL PIERRE WERTHEIMER", Geo_Hosp)) %>% 
  #   mutate(Geo_Hosp = ifelse(grepl("690784186", Geo_Hosp), "690784186 - HÔPITAL LOUIS PRADEL", Geo_Hosp)) %>% 
  #   # mutate(Geo_Hosp = ifelse(grepl("690784194", Geo_Hosp), "690784194 - HÔPITAL DES CHARPENNES", Geo_Hosp)) %>% 
  #   # mutate(Geo_Hosp = ifelse(grepl("690784202", Geo_Hosp), "690784202 - HÔPITAL HENRY GABRIELLE", Geo_Hosp)) %>% 
  #   # mutate(Geo_Hosp = ifelse(grepl("690787429", Geo_Hosp), "690787429 - HÔPITAL DR FREDERIC DUGOUJON", Geo_Hosp)) %>% 
  #   mutate(Geo_Hosp = ifelse(grepl("690787452", Geo_Hosp), "690787452 - HÔPITAL ANTOINE CHARIAL", Geo_Hosp)) %>% 
  #   # mutate(Geo_Hosp = ifelse(grepl("690787478", Geo_Hosp), "690787478 - HÔPITAL PIERRE GARRAUD", Geo_Hosp))
  #   mutate(Geo_Hosp = ifelse(grepl("690000575", Geo_Hosp), "690000575 - CH NORD OUEST - VILLEFRANCHE", Geo_Hosp)) %>% 
  #   mutate(Geo_Hosp = ifelse(grepl("690807599", Geo_Hosp), "690807599 - CLINIQUE DE L'UNION", Geo_Hosp))
}

clean_age20 <- function(tib){
  tib %>% mutate(age20 = case_when(age20 == "Others" ~ "Others", #manage the age20
                                   age20 == "> 80 ans" ~ "81+", 
                                   T ~ gsub("\\[([-0-9]+) .*", "\\1", age20)
  ))
}

clean_DepCode <- function(tib){
  tib %>% mutate(DepCode = case_when(DepCode == "Others" ~ "Others", #manage the RegCode
                                     grepl("^2A", DepCode) ~ "2A", 
                                     grepl("^2B", DepCode) ~ "2B", 
                                     grepl("^NA", DepCode) ~ "Dep missing", 
                                     T ~ gsub("\\D", "", DepCode)))
}

clean_PMSIcode <- function(tib){
  tib %>% separate(PMSIcode, into = c("PMSIcode", "PMSIcode_Name"), sep = " - ") %>% 
    left_join(pmsi_postcode %>% select(PMSIcode, DepCode) %>% unique) %>% 
    mutate(DepCode = case_when(grepl("^99100", PMSIcode) ~ "Dep missing", 
                               grepl("^99", PMSIcode) ~ NA_character_, 
                               grepl("^9A", PMSIcode) ~ "971", 
                               grepl("^9B", PMSIcode) ~ "972", 
                               grepl("^9C", PMSIcode) ~ "973", 
                               grepl("^9D", PMSIcode) ~ "974", 
                               grepl("^9E", PMSIcode) ~ "975", 
                               grepl("^9F", PMSIcode) ~ "976", 
                               grepl("^9G", PMSIcode) ~ "986", 
                               grepl("^9H", PMSIcode) ~ "987", 
                               grepl("^9J", PMSIcode) ~ "988", 
                               grepl("999$", PMSIcode) ~ substr(PMSIcode, 1, 2), 
                               grepl("000$", PMSIcode) ~ substr(PMSIcode, 1, 2), 
                               PMSIcode == "Others" ~ "Others", 
                               T ~ DepCode)) %>% 
    left_join(y = pmsi_countries %>% 
                filter(Pays != "FRANCE") %>% 
                select(PMSIcode, OtherCountry = Pays)) %>% 
    mutate(DepCode = ifelse(DepCode %in% c("97", "98", "20"), "Dep missing", DepCode)) %>% 
    left_join(dep_reg %>% 
                select(DepCode, RegCode) %>% 
                rbind(tibble(DepCode = "Dep missing", RegCode = "Reg missing")))
}



##### cleaning #####

reg_year_all <- reg_year_all_raw %>% 
  transmute(RegCode = `Région (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(RegCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_RegCode %>% 
  mutate(RegCode = factor(RegCode, reg_levels_met)) %>% 
  ungroup %>% complete(RegCode, Year, fill = list(n = NA)) #complete (not always necessary)

reg_year_finess <- reg_year_finess_raw %>% 
  transmute(FINESS_Hosp = `Finess PMSI`, #rename
            RegCode = `Région (domiciliation patient)`, 
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:RegCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_FinessHosp %>% 
  clean_RegCode %>% 
  mutate(RegCode = factor(RegCode, c(reg_levels_met, "Others")))


# reg_age20year_all
# needs a repair job because the Corsican children have all been put in "Others"
# it's below in this same script
reg_age20year_all_toRepair <- reg_age20year_all_raw %>% 
  transmute(age20 = `Tranche d'âge`, 
            RegCode = `Région (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(age20:RegCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_age20 %>% 
  clean_RegCode %>%  
  mutate(RegCode = factor(RegCode, c(reg_levels_met, "Others")), 
         age20 = factor(age20, c(age20_vec, "Others"))) %>% 
  ungroup %>% complete(RegCode, age20, Year, fill = list(n = NA)) #complete (not always necessary)

# reg_age20year_finess
reg_age20year_finess <- reg_age20year_finess_raw %>% 
  transmute(FINESS_Hosp = `Finess PMSI`, #rename
            age20 = `Tranche d'âge`, 
            RegCode = `Région (domiciliation patient)`, 
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:RegCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_FinessHosp %>% 
  clean_age20 %>% 
  clean_RegCode %>%  
  mutate(RegCode = factor(RegCode, c(reg_levels_met, "Others")), 
         age20 = factor(age20, c(age20_vec, "Others"))) #%>% 
  # ungroup %>% complete(RegCode, age20, Year, fill = list(n = NA)) #complete (not always necessary)

# dep_year_all
dep_year_all <- dep_year_all_raw %>% 
  transmute(DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_DepCode %>% 
  ungroup %>% complete(DepCode, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  select(RegCode, everything())

# dep_year_finess

dep_year_finess <- dep_year_finess_raw %>% 
  transmute(FINESS_Hosp = `Finess PMSI`,  #rename
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_FinessHosp %>% 
  clean_DepCode %>% 
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  select(RegCode, everything())


# dep_age20year_all
dep_age20year_all <- dep_age20year_all_raw %>% 
  transmute(age20 = `Tranche d'âge`, 
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(age20:DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_age20 %>% 
  clean_DepCode %>% 
  mutate(DepCode = factor(DepCode, levels = dep_levels_met), 
         age20 = factor(age20, c(age20_vec, "Others"))) %>% 
  ungroup %>% complete(age20, DepCode, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")

# dep_age20year_finess

dep_age20year_finess <- dep_age20year_finess_raw %>% 
  transmute(FINESS_Hosp = `Finess PMSI`, 
            age20 = `Tranche d'âge`, 
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_FinessHosp %>% 
  clean_age20 %>% 
  clean_DepCode %>% 
  mutate(DepCode = factor(DepCode, levels = dep_levels_met), 
         age20 = factor(age20, c(age20_vec, "Others"))) %>% 
  # ungroup %>% complete(DepCode, age20, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")

# pmsi_year_all

pmsi_year_all <- pmsi_year_all_raw %>% 
  transmute(PMSIcode = `Code géographique de domiciliation du patient`, 
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(PMSIcode, ~gsub("^Autres.*", "Others", .x))) %>% 
  clean_PMSIcode %>% 
  transmute(RegCode, DepCode, PMSIcode, PMSIcode_Name, OtherCountry, Year, n)

# pmsi_year_finess

pmsi_year_finess <- pmsi_year_finess_raw %>% 
  transmute(FINESS_Hosp = `Finess PMSI`, 
            PMSIcode = `Code géographique de domiciliation du patient`, 
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:PMSIcode, ~gsub("^Autres.*", "Others", .x))) %>% 
  clean_FinessHosp %>% 
  clean_PMSIcode %>% 
  transmute(FINESS_Hosp, RegCode, DepCode, PMSIcode, PMSIcode_Name, OtherCountry, Year, n)

# pmsi_age20year_all

pmsi_age20year_all <- pmsi_age20year_all_raw %>% 
  transmute(age20 = `Tranche d'âge`, 
            PMSIcode = `Code géographique de domiciliation du patient`, 
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(age20:PMSIcode, ~gsub("^Autres.*", "Others", .x))) %>% 
  clean_age20 %>% 
  clean_PMSIcode %>% 
  transmute(age20, RegCode, DepCode, PMSIcode, PMSIcode_Name, OtherCountry, Year, n)

# pmsi_age20year_finess

pmsi_age20year_finess <- pmsi_age20year_finess_raw %>% 
  transmute(age20 = `Tranche d'âge`, 
            FINESS_Hosp = `Finess PMSI`, 
            PMSIcode = `Code géographique de domiciliation du patient`, 
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(age20:PMSIcode, ~gsub("^Autres.*", "Others", .x))) %>% 
  clean_FinessHosp %>% 
  clean_age20 %>% 
  clean_PMSIcode %>% 
  transmute(FINESS_Hosp, age20, RegCode, DepCode, PMSIcode, PMSIcode_Name, OtherCountry, Year, n)

## repair job on reg_age20year_all in the Corsica data

repair_piece <- dep_age20year_all %>% 
  filter(RegCode == 94, age20 == "0-18") %>% 
  group_by(RegCode, age20, Year) %>% 
  summarise(n_repair = sum(n)) %>% 
  ungroup %>% 
  {rbind(transmute(., RegCode = "94", age20, Year, n_repair), 
         transmute(., RegCode = "Others", age20 = "Others", Year, n_repair = -n_repair))}

reg_age20year_all <- reg_age20year_all_toRepair %>% 
  left_join(repair_piece) %>% 
  # filter(RegCode == 94 & age20 == "0-18" & Year >= 2017)
  mutate(n = case_when(RegCode == 94 & age20 == "0-18" & Year >= 2017 ~ n_repair, 
                       RegCode == "Others" & age20 == "Others" & Year >= 2017 ~ n + n_repair, 
                       T ~ n))

# depDOM_year_all

depDOM_year_all <- depDOM_year_all_raw %>% 
  transmute(DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_DepCode %>% 
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = DepCode)

# depDOM_year_finess
depDOM_year_finess <- depDOM_year_finess_raw %>% 
  transmute(FINESS_Hosp = `Finess PMSI`,  #rename
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_FinessHosp %>% 
  clean_DepCode %>% 
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = DepCode)

# depEVERY_year_all
depEVERY_year_all <- depEVERY_year_all_raw %>% 
  transmute(DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_DepCode %>% 
  left_join(dep_reg %>% 
              select(DepCode, RegCode) %>% 
              rbind(tibble(DepCode = "Dep missing", RegCode = "Reg missing"))) %>% 
  relocate(RegCode, .before = DepCode)

# dep_year_diag_all

dep_year_diag_all <- dep_year_diag_all_raw %>% 
  transmute(DiagCat = `Catégorie Majeure de Diagnostic`, 
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(DiagCat:DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  left_join(DiagCat_translation) %>% mutate(DiagCat_FR = DiagCat, DiagCat = DiagCat_EN, DiagCat_EN = NULL) %>% 
  clean_DepCode %>% 
  mutate(DepCode = factor(DepCode, levels = dep_levels_met)) %>% 
  ungroup %>% complete(DiagCat, DepCode, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")

# dep_year_diag_finess

dep_year_diag_finess <- dep_year_diag_finess_raw %>% 
  transmute(FINESS_Hosp = `Finess PMSI`, 
            DiagCat = `Catégorie Majeure de Diagnostic`, 
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  left_join(DiagCat_translation) %>% mutate(DiagCat_FR = DiagCat, DiagCat = DiagCat_EN, DiagCat_EN = NULL) %>% 
  clean_FinessHosp %>% 
  clean_DepCode %>% 
  mutate(DepCode = factor(DepCode, levels = dep_levels_met)) %>% 
  # ungroup %>% complete(DepCode, age20, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")

# pmsi_year_finessgeo
pmsi_year_finessgeo <- pmsi_year_finessgeo_raw %>% transmute(FINESS_Hosp = `Finess PMSI`, 
                                      Geo_Hosp = `Finess géographique`, 
                                      PMSIcode = `Code géographique de domiciliation du patient`, 
                                      Year, 
                                      n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:PMSIcode, ~gsub("^Autres.*", "Others", .x))) %>% 
  clean_FinessHosp %>% 
  clean_GeoHosp %>% 
  clean_PMSIcode %>% 
  transmute(FINESS_Hosp, Geo_Hosp, RegCode, DepCode, PMSIcode, PMSIcode_Name, OtherCountry, Year, n)

dep_year_finessgeo <- dep_year_finessgeo_raw %>% transmute(FINESS_Hosp = `Finess PMSI`, 
                                                             Geo_Hosp = `Finess géographique`, 
                                                             DepCode = `Département (domiciliation patient)`, 
                                                             Year, 
                                                             n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:DepCode, ~gsub("^Autres.*", "Others", .x))) %>% 
  clean_FinessHosp %>% 
  clean_GeoHosp %>% 
  clean_DepCode %>% 
  mutate(DepCode = factor(DepCode, levels = dep_levels_met)) %>% 
  # ungroup %>% complete(DepCode, age20, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")

pmsiDOM_year_finessgeo <- pmsiDOM_year_finessgeo_raw %>% transmute(FINESS_Hosp = `Finess PMSI`, 
                                                             Geo_Hosp = `Finess géographique`, 
                                                             PMSIcode = `Code géographique de domiciliation du patient`, 
                                                             Year, 
                                                             n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:PMSIcode, ~gsub("^Autres.*", "Others", .x))) %>% 
  clean_FinessHosp %>% 
  clean_GeoHosp %>% 
  clean_PMSIcode %>% 
  transmute(FINESS_Hosp, Geo_Hosp, RegCode, DepCode, PMSIcode, PMSIcode_Name, OtherCountry, Year, n)

depDOM_year_finessgeo <- depDOM_year_finessgeo_raw %>% transmute(FINESS_Hosp = `Finess PMSI`, 
                                                           Geo_Hosp = `Finess géographique`, 
                                                           DepCode = `Département (domiciliation patient)`, 
                                                           Year, 
                                                           n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(FINESS_Hosp:DepCode, ~gsub("^Autres.*", "Others", .x))) %>% 
  clean_FinessHosp %>% 
  clean_GeoHosp %>% 
  clean_DepCode %>% 
  # mutate(DepCode = factor(DepCode, levels = dep_levels_met)) %>% 
  # ungroup %>% complete(DepCode, age20, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")






# dep_ageyear_all

dep_ageyear_all <- dep_ageyear_all_raw %>% 
  transmute(age_group, 
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_DepCode %>% 
  mutate(DepCode = factor(DepCode, levels = dep_levels_met), 
         age_group = factor(age_group, age_vec)) %>% 
  ungroup %>% complete(age_group, DepCode, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")

# dep_ageyear_hcl

dep_ageyear_hcl <- dep_ageyear_hcl_raw %>% 
  transmute(FINESS_Hosp = "690781810 - HOSPICES CIVILS DE LYON", 
            age_group, 
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_DepCode %>% 
  mutate(DepCode = factor(DepCode, levels = dep_levels_met), 
         age_group = factor(age_group, age_vec)) %>% 
  # ungroup %>% complete(DepCode, age20, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")

# dep_age0year_all

dep_age0year_all <- dep_age0year_all_raw %>% 
  transmute(age_group, 
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_DepCode %>% 
  mutate(DepCode = factor(DepCode, levels = dep_levels_met)) %>% 
  ungroup %>% complete(age_group, DepCode, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")

# dep_age0year_hcl

dep_age0year_hcl <- dep_age0year_hcl_raw %>% 
  transmute(FINESS_Hosp = "690781810 - HOSPICES CIVILS DE LYON", 
            age_group, 
            DepCode = `Département (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(DepCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_DepCode %>% 
  mutate(DepCode = factor(DepCode, levels = dep_levels_met)) %>% 
  # ungroup %>% complete(DepCode, age20, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  relocate(RegCode, .before = "DepCode")


reg_age20year_hcl <- reg_age20year_hcl_raw %>% 
  transmute(FINESS_Hosp = "690781810 - HOSPICES CIVILS DE LYON", 
            age20 = `Tranche d'âge`, 
            RegCode = `Région (domiciliation patient)`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(age20:RegCode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_age20 %>% 
  clean_RegCode %>% 
  # ungroup %>% complete(DepCode, age20, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  transmute(FINESS_Hosp, age20, RegCode, Year, n)



# pmsi_ageyear_all

pmsi_ageyear_all <- pmsi_ageyear_all_raw %>% 
  transmute(age_group, 
            PMSIcode = `Code géographique de domiciliation du patient`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(PMSIcode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_PMSIcode %>% 
  mutate(age_group = factor(age_group, age_vec)) %>% 
  # ungroup %>% complete(age_group, DepCode, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  relocate(RegCode, .before = "DepCode") %>% 
  transmute(age_group, RegCode, DepCode, PMSIcode, PMSIcode_Name, OtherCountry, Year, n)

# pmsi_ageyear_hcl

pmsi_ageyear_hcl <- pmsi_ageyear_hcl_raw %>% 
  transmute(FINESS_Hosp = "690781810 - HOSPICES CIVILS DE LYON", 
            age_group, 
            PMSIcode = `Code géographique de domiciliation du patient`, #rename
            Year, 
            n = as.numeric(gsub(" ", "", Effectif))) %>% 
  mutate(across(PMSIcode, ~gsub("^Autres.*", "Others", .x))) %>% #manage the RegCode
  clean_PMSIcode %>% 
  mutate(age_group = factor(age_group, age_vec)) %>% 
  # ungroup %>% complete(age_group, DepCode, Year, fill = list(n = NA)) %>%  #complete (not always necessary)
  relocate(RegCode, .before = "DepCode") %>% 
  transmute(FINESS_Hosp, age_group, RegCode, DepCode, PMSIcode, PMSIcode_Name, OtherCountry, Year, n)


# Urg ARA'data needs special treatment


ara_raw = read_csv2("Urgences/2025_11_20_demande_attractivite_HCL_2019_24.csv")


ara_with69000 <- ara_raw %>% 
  transmute(Year = annee, 
            Postcode = case_match(CP_clean, 
                                  # "59777" ~ "59000", 
                                  # "74480" ~ "74190", 
                                  "75116" ~ "75016", 
                                  "76420" ~ "76000", 
                                  "83530" ~ "83700", 
                                  .default = CP_clean), 
            DepCode = case_when(grepl("^97", Postcode) | grepl("^98", Postcode) ~ substr(Postcode, 1, 3), 
                                T ~ substr(Postcode, 1, 2)), 
            age_group = case_match(age_class, 
                                   "[0,1]" ~ "0-1", 
                                   "(1,4]" ~ "2-4", 
                                   "(4,17]" ~ "5-17", 
                                   "(17,64]" ~ "18-64", 
                                   "(64,79]" ~ "65-79", 
                                   "(79,Inf]" ~ "80+") %>% 
              factor(levels = age_vec), 
            anonymised = as.numeric(N_HCL == "1-4"), 
            n = as.numeric(N_RPU), 
            n_HCL = as.numeric(N_HCL), 
            part_HCL = as.numeric(part_HCL)) %>% 
  left_join(pmsi_postcode %>% filter(DepCode %in% c("2A", "2B")) %>% transmute(Postcode, new_DepCode = DepCode) %>% unique, by = "Postcode") %>% 
  mutate(DepCode = ifelse(DepCode == "20", new_DepCode, DepCode)) %>% 
  select(-new_DepCode) %>% 
  left_join(dep_reg %>% 
              select(DepCode, RegCode))


# as 69000 is present in the data for people who have just been listed as "Lyon", we take these and distribute them according to the population distribution among the 9 arondissements of Lyon
ara_69000transformed <- ara_with69000 %>% 
  filter(Postcode == "69000") %>% 
  select(-Postcode) %>% 
  left_join(pPostcode_69000, relationship = "many-to-many") %>% 
  transmute(Year, Postcode, age_group, extra_n = n*pPostcode, extra_n_HCL = n_HCL*pPostcode)


ara <- ara_with69000 %>% 
  filter(Postcode != "69000") %>% 
  left_join(ara_69000transformed) %>% 
  # mutate(old_part_HCL = part_HCL) %>% 
  mutate(n = ifelse(is.na(extra_n), n, n + extra_n), 
         n_HCL = ifelse(is.na(extra_n_HCL), n_HCL, n_HCL + extra_n_HCL), 
         part_HCL = ifelse(is.na(extra_n), part_HCL, n_HCL/n)) %>% 
  select(-starts_with("extra")) %>% 
  ungroup
