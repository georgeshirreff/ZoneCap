library(tidyverse)
library(sf)

# 1 population data
age_vec = c("0-1", "2-4", "5-17", "18-64", "65-79", "80+")

# a file containing the complete list of departements and their regions
# https://www.insee.fr/fr/statistiques/2012692
dep_reg <- readxl::read_excel("data/INSEEage/Cleaned/dep_reg.xlsx") %>% 
  rename(DepName = Dep, RegName = Reg) %>% 
  mutate(DepCode = paste0(ifelse(nchar(DepCode) == 1, "0", ""), DepCode), 
         RegCode = paste0(ifelse(nchar(RegCode) == 1, "0", ""), RegCode)) %>% 
  bind_rows(tibble(RegCode = "Others", DepCode = "Others"))
y = 2024

# https://www.insee.fr/fr/statistiques/8331297
# Estimation de population par département, sexe et âge quinquennal - Années 1975 à 2025
# https://www.insee.fr/fr/statistiques/fichier/8331297/estim-pop-dep-sexe-aq-1975-2025.xlsx
dep_pop <- map_df(2016:2025, 
                  .f = function(y){
                    readxl::read_excel("data/INSEEage/estim-pop-dep-sexe-aq-1975-2025.xlsx", sheet = as.character(y), skip = 4, .name_repair = "unique_quiet") %>% 
                      select(`...1`:`Total...23`) %>% 
                      select(-`Total...23`) %>% 
                      rename(DepCode = `...1`, DepName = `...2`) %>% 
                      filter((!is.na(as.numeric(DepCode)) | DepCode %in% c("2A", "2B", "DOM"))) %>% 
                      rename_all(~gsub("^([0-9]+) à ([0-9]+) ans.*", "\\1-\\2", .x)) %>% 
                      rename_all(~gsub("^(95+) ans.*", "\\1+", .x)) %>% 
                      pivot_longer(-c("DepCode", "DepName"), names_to = "AgeGrp", values_to = "Pop") %>% 
                      mutate(Year = y)
                  } ) %>% 
  # mutate(RegName = ifelse(RegName == "Île-de-France", "Ile-de-France", RegName)) %>% 
  mutate(DepCode = ifelse(DepCode == "DOM", "TOM", DepCode)) %>%
  left_join(dep_reg %>% 
              select(-DepName) %>% 
              rbind(tibble(DepCode = "TOM", RegCode = "TOM", RegName = "TOM")))

dep_reg %>% 
  select(-DepName) %>% 
  rbind(tibble(DepCode = "TOM", RegCode = "TOM", RegName = "TOM")) %>% 
  print(n = 200)

# https://www.insee.fr/fr/statistiques/8331297
# Estimation de population par région, sexe et âge quinquennal - Années 1975 à 2025
# https://www.insee.fr/fr/statistiques/fichier/8331297/estim-pop-nreg-sexe-aq-1975-2025.xlsx
reg_pop <- map_df(2016:2025, 
                  .f = function(y){
                    readxl::read_excel("data/INSEEage/estim-pop-nreg-sexe-aq-1975-2025.xlsx", sheet = as.character(y), skip = 4, .name_repair = "unique_quiet") %>% 
                      select(`...1`:`Total...22`) %>% 
                      select(-`Total...22`) %>% 
                      rename(RegName = `...1`) %>%
                      filter(!grepl("^France m", RegName), !grepl("^Source", RegName), !is.na(RegName)) %>% 
                      rename_all(~gsub("^([0-9]+) à ([0-9]+) ans.*", "\\1-\\2", .x)) %>% 
                      rename_all(~gsub("^(95+) ans.*", "\\1+", .x)) %>% 
                      pivot_longer(-c("RegName"), names_to = "AgeGrp", values_to = "Pop") %>% 
                      mutate(Year = y)
                  } ) %>% 
  mutate(RegName = ifelse(RegName == "Centre-Val-de-Loire", "Centre-Val de Loire", RegName)) %>% 
  mutate(RegName = ifelse(grepl("le-de-France", RegName), "Ile-de-France", RegName)) %>%
  mutate(RegName = ifelse(RegName == "DOM", "TOM", RegName)) %>%
  left_join(dep_reg %>% select(RegName, RegCode) %>% unique)


dep_pop %>% 
  group_by(RegCode, AgeGrp, Year) %>% 
  summarise(depSumPop = sum(Pop)) %>% 
  full_join(reg_pop %>% 
              select(-RegName)) %>% 
  ggplot(aes(x = Pop, y = depSumPop)) + geom_point()

# postcode

# https://www.atih.sante.fr/mise-jour-2024-de-la-liste-de-correspondance-codes-postaux-codes-geographiques-pmsi
# https://www.atih.sante.fr/sites/default/files/public/content/4765/codepost2024.xlsx
# postcode_pmsi_pop_raw <- readxl::read_excel("ScanSanté/codepost2025.xlsx", sheet = "table")
# postcode_pmsi_pop_raw <- readxl::read_excel("ScanSanté/codepost2024.xlsx", sheet = "table")
# y = 2016
postcode_pmsi_pop_year_raw <- purrr::map(2016:2025, function(y) {
  readxl::read_excel(paste0("data/ATIH/codepost", y, ".xlsx"), sheet = "table") %>% 
    transmute(Postcode = get(paste0("Code postal ",y)), 
              Postcode_Name = `Libellé poste`,
              PMSIcode = get(paste0("Code géographique PMSI ", y)), 
              # PopPostcode_min = get(paste0("Population ", y-3, " desservie par le code postal, au minimum (en ne comptabilisant aucune commune multidistribuée")), 
              # PopPostcode_max = get(paste0("Population ", y-3, " desservie par le code postal, au maximum (en comptabilisant toutes les communes multidistribuée")), 
              PopPMSI = get(paste0("Population ", y-3, " desservie par le code géo PMSI"))
    ) %>% 
    mutate(Year = y-3, 
           Postcode_Year = y)
}) %>% 
  {do.call("rbind", .)}

# https://www.data.corsica/explore/dataset/code-postal-code-insee-2015/export/?sort=insee_com
corse_linker <- read_csv2("Shapefiles/CorseLinker_code-postal-code-insee-2015.csv") %>% 
  transmute(CorseInsee = INSEE_COM, CorsePostcode = Code_postal) %>% unique
  
postcode_pmsi_pop_year_raw_dep <- postcode_pmsi_pop_year_raw %>% 
  mutate(Postcode = case_match(Postcode, 
                               "59777" ~ "59000", 
                               # "74480" ~ "74190", 
                               "75116" ~ "75016", 
                               "76420" ~ "76000", 
                               "83530" ~ "83700", 
                               .default = Postcode)) %>% 
  mutate(DepCode = case_when(grepl("^97", Postcode) | grepl("^98", Postcode) ~ substr(Postcode, 1, 3), 
                             grepl(c("^20"), Postcode) ~ substr(PMSIcode, 1, 2), 
                             PMSIcode == "2BC11" ~ "2B", 
                             PMSIcode == "91550" ~ "91", 
                             T ~ substr(Postcode, 1, 2)
  )) %>% 
  left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
  filter(DepCode != "99") %>% 
  mutate(RegCode = replace_na(RegCode, "TOM"))


# replace the Corsica postcodes using the linker table
postcode_pmsi_pop_year_raw_dep_corse <- postcode_pmsi_pop_year_raw_dep %>% 
  left_join(corse_linker, by = c("Postcode" = "CorseInsee")) %>% 
  mutate(Postcode = ifelse(!is.na(CorsePostcode), CorsePostcode, Postcode)) %>% 
  mutate(Postcode = gsub("^2[AB]", "20", Postcode)) %>% 
  select(-CorsePostcode)


pmsi_pop_to2022 = postcode_pmsi_pop_year_raw_dep_corse %>% 
  select(RegCode, DepCode, PMSIcode, Year, PopPMSI) %>% unique

pmsi_postcode = postcode_pmsi_pop_year_raw_dep_corse %>% 
  select(Postcode, PMSIcode, DepCode, RegCode) %>% 
  unique

dep_pop %>% 
  group_by(RegCode, DepCode, Year) %>% 
  summarise(depPop = sum(Pop)) %>% 
  full_join(pmsi_pop_to2022 %>% 
              group_by(RegCode, DepCode, Year) %>% 
              summarise(pmsiPop = sum(PopPMSI))) %>% 
  # mutate(diff_pct = (pmsiPop -depPop)/depPop) %>%
  # filter(abs(diff_pct) > 0.02)
  ggplot(aes(x = pmsiPop, y = depPop)) + geom_point()

# project the postcode populations forward in time using the department totals

pmsi_pop_to2025 <- pmsi_pop_to2022 %>% 
  group_by(DepCode, Year) %>% 
  mutate(sum_Departement_Pop = sum(PopPMSI)) %>% # select the final year and calculate the sum across each departement
  mutate(Year = Year + 3) %>% 
  left_join(dep_pop %>%  #merge in the true departmental values 
              group_by(RegCode, DepCode, Year) %>% 
              summarise(real_Departement_Pop = sum(Pop)) %>% 
              ungroup) %>% 
  mutate(PopPMSI = PopPMSI * real_Departement_Pop/sum_Departement_Pop) %>% # then adjust the PMSI values according to this ratio
  select(-sum_Departement_Pop, -real_Departement_Pop) %>% 
  ungroup

# and project forward a few PMSI that are missing years
project_pop <- function(pmsi_orig, pmsi_vec){
  output_pmsi_orig <- pmsi_orig
  for(this_pmsi in pmsi_vec){
    this_pmsi_orig <- pmsi_orig %>% 
      filter(PMSIcode == this_pmsi)
    this_pmsi_new <- this_pmsi_orig %>% 
      select(RegCode:PMSIcode) %>% 
      unique %>% 
      cross_join(    this_pmsi_orig %>% 
                       {Hmisc::approxExtrap(x = .$Year, y = .$PopPMSI, xout = 2016:2025)} %>% 
                       {tibble(Year = .$x, PopPMSI = .$y)}
      )
    
    output_pmsi_orig <- output_pmsi_orig %>% 
      filter(PMSIcode != this_pmsi) %>% 
      rbind(this_pmsi_new)
  }
  return(output_pmsi_orig %>% 
           arrange(Year, PMSIcode))
}

pmsi_pop <- project_pop(pmsi_pop_to2025, pmsi_vec = c("69600", "69310"))

save(dep_pop, file = "data/INSEEage/Cleaned/dep_pop.RData")
save(reg_pop, file = "data/INSEEage/Cleaned/reg_pop.RData")
save(pmsi_pop, file = "data/INSEEage/Cleaned/pmsi_pop.RData")
save(pmsi_postcode, file = "data/INSEEage/Cleaned/pmsi_postcode.RData")

# now broken down by age

age_vec
#made in get_met_ages.R
age_cuts = c(0, 2, 5, 18, 65, 80, 200)

# https://www.insee.fr/fr/outil-interactif/5014911/pyramide.htm
# Sources: 1990-2023 Recensement de la population- 2024-2026 Estimations localisées de population
# Champ: France hors Mayotte jusqu'en 2013, France à partir de 2014
# https://www.insee.fr/fr/outil-interactif/5014911/data/FR/donnees_pyramide_act.csv
francemet_age <- read_csv2("data/INSEEage/donnees_pyramide_act.csv") %>% 
  transmute(Year = ANNEE, Sex = SEXE, Age = AGE, Pop = POP) %>% 
  group_by(Age, Year) %>% summarise(Pop = sum(Pop)) %>% 
  filter(Year >= 2010) %>% 
  arrange(-Year) %>% ungroup



# reg_age_pop
group5 <- reg_pop$AgeGrp %>% unique
age20_vec <- c("0-18", "19-40", "41-60", "61-80", "81+") 
# dep_pop$AgeGrp %>% unique

# get the proportions of each age group within that 5-year age window, from the FranceMet data
francemet_age_proportions <- francemet_age %>% 
  mutate(group5 = cut(Age + 0.1, c(seq(0, 95, by = 5), 200), include.lowest = T, labels = group5)) %>% 
  group_by(Year, group5) %>% 
  mutate(age5_proportion = Pop/sum(Pop)) %>% 
  arrange(Year, Age) %>% 
  ungroup %>% 
  transmute(Year, Age, group5, age5_proportion)


# now expand this to have a population per age group
reg_pop_ageFine <- reg_pop %>% 
  left_join(francemet_age_proportions, by = c("Year", "AgeGrp" = "group5"), relationship = "many-to-many") %>% 
  mutate(group5_Pop = Pop, Pop = Pop*age5_proportion) %>% 
  select(-AgeGrp, -age5_proportion, -group5_Pop)

# now contract to our own age groups
reg_pop_age <- reg_pop_ageFine %>% 
  mutate(age_group = cut(Age + 0.1, age_cuts, include.lowest = T, labels = age_vec)) %>% 
  group_by(RegName, RegCode, Year, age_group) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup

reg_pop_age20 <- reg_pop_ageFine %>% 
  mutate(age20_group = cut(Age + 0.1, include.lowest = T, labels = age20_vec, 
                         breaks =  as.numeric(c(gsub("^([0-9]+)[-+].*", "\\1", age20_vec), "200"))
                         )) %>% 
  group_by(RegName, RegCode, Year, age20_group) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup

# now expand this to have a population per age group
dep_pop_ageFine <- dep_pop %>% 
  left_join(francemet_age_proportions, by = c("Year", "AgeGrp" = "group5"), relationship = "many-to-many") %>% 
  mutate(group5_Pop = Pop, Pop = Pop*age5_proportion) %>% 
  select(-AgeGrp, -age5_proportion, -group5_Pop)

dep_pop_age <- dep_pop_ageFine %>% 
  mutate(age_group = cut(Age + 0.1, age_cuts, include.lowest = T, labels = age_vec)) %>% 
  group_by(RegName, RegCode, DepName, DepCode, Year, age_group) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup

dep_pop_age20 <- dep_pop_ageFine %>% 
  mutate(age20_group = cut(Age + 0.1, include.lowest = T, labels = age20_vec, 
                           breaks =  as.numeric(c(gsub("^([0-9]+)[-+].*", "\\1", age20_vec), "200"))
  )) %>% 
  group_by(RegName, RegCode, DepName, DepCode, Year, age20_group) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup




dep_pop_age %>% 
  group_by(RegCode, age_group, Year) %>% 
  summarise(depSumPop = sum(Pop)) %>% 
  full_join(reg_pop_age %>% 
              select(-RegName)) %>% 
  ggplot(aes(x = Pop, y = depSumPop)) + geom_point()

dep_pop_age20 %>% 
  group_by(RegCode, age20_group, Year) %>% 
  summarise(depSumPop = sum(Pop)) %>% 
  full_join(reg_pop_age20 %>% 
              select(-RegName)) %>% 
  ggplot(aes(x = Pop, y = depSumPop)) + geom_point()

# now split out the postcode populations by age

# https://www.data.gouv.fr/datasets/base-officielle-des-codes-postaux/
# https://www.data.gouv.fr/api/1/datasets/r/008a2dda-2c60-4b63-b910-998f6f818089
# 019HexaSmal.csv
linker <- read_csv2("data/INSEEage/Codes_INSEE_postal.csv", locale=locale(encoding="latin1")) %>% 
  # slice(1:30000) %>% 
  mutate(across(starts_with("Code"), ~paste0(ifelse(nchar(.x) == 4, "0", ""), .x))) %>% 
  transmute(CODGEO = Code_commune_INSEE, POSTCODE_GEO = Code_postal, NOM = Nom_de_la_commune) %>% 
  dplyr::rows_insert(tibble(CODGEO = "01330", POSTCODE_GEO = "01260", NOM = "RUFFIEU"))

linker2 <- read_csv2("data/INSEEage/correspondance-code-cedex-code-insee.csv") %>% 
  transmute(CODGEO = `Code INSEE`, Postcode = `Code Postal / CEDEX`, Type = `Type de code`) %>% 
  filter(Type == "Code Postal") %>% 
  filter(!is.na(CODGEO)) %>% 
  unique


# https://www.insee.fr/fr/statistiques/8202264?sommaire=8202287
#Recensement de la population - base des tableaux détaillés
# POP1B – Population par sexe et âge
# https://www.insee.fr/fr/statistiques/fichier/8202264/TD_POP1B_2021_csv.zip

insee2021_raw <- read_csv2(file = unz("data/INSEEage/TD_POP1B_2021.zip", filename = "TD_POP1B_2021.csv")) %>% 
# insee2021_raw <- read_csv2("INSEEage/TD_POP1B_2021.csv") %>% 
  transmute(CODGEO, LIBGEO, Sex = SEXE, Age = as.numeric(AGED100), Pop2021 = as.numeric(NB))

# ages by postcode (don't worry about it being duplicated, we're averaging)
postcode_Age_2021 <- insee2021_raw %>% 
  group_by(CODGEO, LIBGEO, Age) %>% 
  summarise(Pop2021 = sum(Pop2021)) %>% 
  left_join(linker %>% transmute(CODGEO, Postcode = POSTCODE_GEO) %>% unique, relationship = "many-to-many") %>% 
  left_join(linker2 %>% transmute(CODGEO, Postcode2 = Postcode) %>% unique, relationship = "many-to-many") %>%
  mutate(Postcode = ifelse(!is.na(Postcode), Postcode, Postcode2)) %>%
  select(-Postcode2) %>%
  mutate(Postcode = case_match(CODGEO,
                               "13055" ~ "13001",
                               "69123" ~ "69001",
                               "75056" ~ "75101",
                               .default = Postcode
  )) %>% 
  ungroup

# get proportion of each Age by PMSI area
pmsi_pAge_2021 <- postcode_Age_2021 %>%
  left_join(pmsi_postcode %>% select(RegCode, DepCode, Postcode, PMSIcode)) %>% #this is a dodgy merge but it doesnt matter because we are only taking average population ages
  group_by(RegCode, DepCode, PMSIcode, Postcode, Age) %>% 
  summarise(Pop2021 = mean(Pop2021, na.rm = T)) %>% 
  group_by(RegCode, DepCode, PMSIcode, Age) %>% 
  summarise(Pop2021 = sum(Pop2021, na.rm = T)) %>% 
  group_by(RegCode, DepCode, PMSIcode) %>% 
  mutate(pAge = Pop2021/sum(Pop2021, na.rm = T)) %>% 
  select(RegCode, DepCode, PMSIcode, Age, pAge) %>% 
  ungroup

# postcode_pAge2021 <- postcode_Age2021 %>% 
#   group_by(Postcode, age_group) %>% 
#   summarise(Pop2021 = mean(Pop2021)) %>% 
#   group_by(Postcode) %>% 
#   mutate(pAge = Pop2021/sum(Pop2021)) %>% 
#   ungroup %>% 
#   select(-Pop2021)

# pPostcode_69000 <- postcode_Age2021 %>% 
#   filter(grepl("^690", Postcode)) %>% 
#   filter(CODGEO != "69123") %>% 
#   group_by(age_group) %>% 
#   mutate(pPostcode = Pop2021/sum(Pop2021)) %>% 
#   ungroup %>% 
#   select(Postcode, age_group, pPostcode) %>% 
#   arrange(age_group)

# get the proportion of population in each of the 9 Lyon postcodes
# for processing the Urg'ARA data
# many individuals have the non-specific postcode 69000, so we assume these are spread equally among the 9 postcodes
pPostcode_69000 <- postcode_Age_2021 %>% 
  mutate(age_group = case_when(Age < 2 ~ "0-1",
                               Age < 5 ~ "2-4",
                               Age < 18 ~ "5-17",
                               Age < 65 ~ "18-64",
                               Age < 80 ~ "65-79",
                               Age >= 80 ~ "80+")) %>% 
  mutate(age_group = factor(age_group, levels = age_vec)) %>% 
  group_by(CODGEO, Postcode, age_group) %>% 
  summarise(Pop2021 = sum(Pop2021)) %>% 
  filter(grepl("^690", Postcode)) %>%
  filter(CODGEO != "69123") %>%
  group_by(age_group) %>% 
  mutate(pPostcode = Pop2021/sum(Pop2021)) %>% 
  ungroup %>% 
  select(Postcode, age_group, pPostcode) %>%
  arrange(age_group)




DepCode_mean_pAge = pmsi_pAge_2021 %>% 
  group_by(RegCode, DepCode, Age) %>% 
  summarise(median_pAge = median(pAge)) %>% 
  ungroup

RegCode_mean_pAge = pmsi_pAge_2021 %>% 
  group_by(RegCode, Age) %>% 
  summarise(median_pAge = median(pAge)) %>% 
  ungroup

COM_mean_pAge = pmsi_pAge_2021 %>% 
  filter(RegCode %in% c("01", "02", "03", "04", "05", "06", "DOM")) %>% 
  group_by(Age) %>% 
  summarise(median_pAge = median(pAge)) %>% 
  ungroup


pmsi_pop_age_cooking <- pmsi_pop %>% 
  left_join(y = pmsi_pAge_2021 %>% select(-RegCode, -DepCode), relationship = "many-to-many") %>% 
  {bind_rows(filter(., !is.na(Age)), 
             filter(., is.na(Age)) %>% 
               select(-Age, -pAge) %>% 
               left_join(DepCode_mean_pAge %>% transmute(DepCode, Age, pAge = median_pAge, depAverage = T))
  )} %>% 
  {bind_rows(filter(., !is.na(Age)), 
             filter(., is.na(Age)) %>% 
               select(-Age, -pAge) %>% 
               left_join(RegCode_mean_pAge %>% transmute(RegCode, Age, pAge = median_pAge, regAverage = T))
  )} %>%
  {bind_rows(filter(., !is.na(Age)), 
             filter(., is.na(Age)) %>% 
               select(-Age, -pAge) %>% 
               cross_join(COM_mean_pAge %>% transmute(Age, pAge = median_pAge, comAverage = T))
  )}

pmsi_pop_age <- pmsi_pop_age_cooking %>% 
  mutate(age_group = case_when(Age < 2 ~ "0-1",
                               Age < 5 ~ "2-4",
                               Age < 18 ~ "5-17",
                               Age < 65 ~ "18-64",
                               Age < 80 ~ "65-79",
                               Age >= 80 ~ "80+")) %>% 
  mutate(age_group = factor(age_group, levels = age_vec)) %>% 
  group_by(RegCode, DepCode, PMSIcode, Year, age_group) %>% 
  summarise(Pop = sum(PopPMSI*pAge)) %>% 
  ungroup

pmsi_pop_age20 <- pmsi_pop_age_cooking %>% 
  mutate(age20_group = case_when(Age <= 18 ~ "0-18", 
                                 Age <= 40 ~ "19-40", 
                                 Age <= 60 ~ "41-60", 
                                 Age <= 80 ~ "61-80", 
                                 Age >= 80 ~ "81+")) %>%
  mutate(age20_group = factor(age20_group, levels = age20_vec)) %>% 
  group_by(RegCode, DepCode, PMSIcode, Year, age20_group) %>% 
  summarise(Pop = sum(PopPMSI*pAge)) %>% 
  ungroup

pmsi_pop_age %>% 
  group_by(RegCode, age_group, Year) %>% 
  summarise(pmsiSumPop = sum(Pop)) %>% 
  full_join(reg_pop_age %>% 
              select(-RegName)) %>% 
  ggplot(aes(x = Pop, y = pmsiSumPop)) + geom_point() + 
  scale_x_log10() + scale_y_log10()

save(dep_pop_age, file = "data/INSEEage/Cleaned/dep_pop_age.RData")
save(reg_pop_age, file = "data/INSEEage/Cleaned/reg_pop_age.RData")
save(pmsi_pop_age, file = "data/INSEEage/Cleaned/pmsi_pop_age.RData")
save(pmsi_pop_age20, file = "data/INSEEage/Cleaned/pmsi_pop_age20.RData")


reg_pop_age20$RegCode %>% unique
dep_pop_age20$RegCode %>% unique
reg_pop_age$RegCode %>% unique
dep_pop_age$RegCode %>% unique
reg_pop$RegCode %>% unique
dep_pop$RegCode %>% unique

pmsi_pop_age %>% 
  group_by(RegCode, age_group, Year) %>% 
  summarise(pmsiSumPop = sum(Pop)) %>% 
  full_join(reg_pop_age %>% 
              select(-RegName)) %>% 
  filter(!(RegCode %in% c(paste0("0", 1:6), "TOM"))) %>%
  ggplot(aes(x = Pop, y = pmsiSumPop)) + geom_point() + 
  scale_x_log10() + scale_y_log10()

pmsi_pop_age %>% 
  group_by(DepCode, age_group, Year) %>% 
  summarise(pmsiSumPop = sum(Pop)) %>% 
  full_join(dep_pop_age %>% 
              select(-RegName)) %>% 
  filter(!(RegCode %in% c(paste0("0", 1:6), "TOM"))) %>%
  ggplot(aes(x = Pop, y = pmsiSumPop)) + geom_point() + 
  scale_x_log10() + scale_y_log10()
