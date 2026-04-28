# conduct analyses and make basic tables and figures

# merge data
# handle the DOM and missing regions
# create a corrected and uncorrected version

### HCL

## reg

# merge
reg_ageyear_allhcl <- full_join(reg_ageyear_all, 
          reg_ageyear_hcl) %>% 
  left_join(reg_pop_age %>% select(-RegName))

# examine the edge cases
reg_ageyear_allhcl %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                            RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                            T ~ "Metropolitan")) %>% 
  group_by(Year, RegCat) %>% 
  # mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  # filter((RegCode %in% c("Others", "Reg missing", "COM"))) %>% 
  # group_by(Year, RegCode) %>% 
  summarise(across(c("n", "n_HCL"), ~sum(.x, na.rm = T))) %>% 
  arrange(RegCat)




# calculate the size of the catchment population
reg_ageyear_catchment <- reg_ageyear_allhcl %>% 
  group_by(Year) %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter(!(RegCode %in% c("Others", "Reg missing", "COM"))) %>% 
  mutate(p_HCL = ifelse(n == 0, 0, replace_na(n_HCL, 0)/n), 
         catchment_HCL = p_HCL*Pop)

reg_ageyear_catchment %>% 
  group_by(Year) %>% 
  summarise(catchment_HCL = sum(catchment_HCL))

## dep
dep_ageyear_allhcl <- full_join(dep_ageyear_all %>% select(DepCode, RegCode, Year, age_group, n), 
                                dep_ageyear_hcl %>% select(DepCode, RegCode, Year, age_group, n_HCL)) %>% 
  left_join(dep_pop_age %>% select(DepCode, Year, age_group, Pop))

# examine edge cases compare this to the metropolitan contribution
dep_ageyear_allhcl %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                            RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                            T ~ "Metropolitan")) %>% 
  group_by(Year, RegCat) %>% 
  # filter(!(RegCat %in% c("Others", "Reg missing", "COM"))) %>% 
  summarise(across(c("n", "n_HCL"), ~sum(.x, na.rm = T))) %>% 
  arrange(RegCat)

# calculate the size of the catchment population
dep_ageyear_catchment <- dep_ageyear_allhcl %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                            RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                            T ~ "Metropolitan")) %>% 
  filter(RegCat == "Metropolitan") %>% 
  group_by(Year) %>% 
  mutate(p_HCL = ifelse(n == 0, 0, replace_na(n_HCL, 0)/n), 
         catchment_HCL = p_HCL*Pop)

dep_ageyear_catchment %>% 
  group_by(Year) %>% 
  summarise(catchment_HCL = sum(catchment_HCL))

dep_ageyear_catchment %>% 
  mutate(age_group = factor(age_group, levels = age_vec)) %>% 
  {rbind(., 
        group_by(., Year) %>% summarise(catchment_HCL = sum(catchment_HCL), age_group = "ALL")
  )} %>% 
  group_by(Year, age_group) %>% 
  summarise(catchment_HCL = sum(catchment_HCL)) %>% 
  pivot_wider(id_cols = age_group, names_from = Year, values_from = catchment_HCL) %>% 
  write_csv2("output/dep_ageyear_catchment_summTable.csv")

write_csv2(dep_ageyear_catchment, "output/dep_ageyear_catchment.csv")


# postcode
pmsi_year_allhcl <- full_join(pmsi_year_all %>% select(-PMSIcode_Name), 
          pmsi_year_hcl %>% select(-PMSIcode_Name), relationship = "one-to-one") %>% 
  left_join(pmsi_pop)

# examine the edge cases
pmsi_year_allhcl %>% 
  group_by(Year) %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter((RegCode %in% c("Others", "Reg missing", "COM")) | !is.na(OtherCountry)) %>% 
  group_by(Year, RegCode, OtherCountry) %>% 
  summarise(across(c("n", "n_HCL"), ~sum(.x, na.rm = T))) %>% 
  ungroup %>% 
  arrange(RegCode) %>% 
  print(n = 100)

# compare this to the metropolitan contribution
pmsi_year_allhcl %>% 
  group_by(Year) %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter(!(RegCode %in% c("Others", "Reg missing", "COM")), is.na(OtherCountry)) %>% 
  summarise(across(c("n", "n_HCL"), ~sum(.x, na.rm = T)))

# calculate the size of the catchment population
pmsi_year_catchment <- pmsi_year_allhcl %>% 
  group_by(Year) %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter(!(RegCode %in% c("Others", "Reg missing", "COM")), 
         is.na(OtherCountry), 
         !grepl("999$", PMSIcode)
         ) %>% 
  mutate(p_HCL = ifelse(n == 0, 0, replace_na(n_HCL, 0)/n), 
         catchment_HCL = p_HCL*PopPMSI)

pmsi_year_catchment %>% 
  group_by(Year) %>% 
  summarise(catchment_HCL = sum(catchment_HCL))

# now a comparison of all the three resolutions together

rbind(reg_ageyear_catchment %>% 
        {rbind(., 
               group_by(., Year) %>% summarise(catchment_HCL = sum(catchment_HCL), age_group = "ALL")
        )} %>% 
        group_by(Year, age_group) %>% 
        summarise(catchment_HCL = sum(catchment_HCL)) %>% 
        mutate(Resolution = "Region")
      , dep_ageyear_catchment %>% 
        {rbind(., 
               group_by(., Year) %>% summarise(catchment_HCL = sum(catchment_HCL), age_group = "ALL")
        )} %>% 
        group_by(Year, age_group) %>% 
        summarise(catchment_HCL = sum(catchment_HCL)) %>% 
        mutate(Resolution = "Department")
      , pmsi_year_catchment %>% 
        group_by(Year) %>% 
        summarise(catchment_HCL = sum(catchment_HCL)) %>% 
        mutate(age_group = "ALL", Resolution = "PMSI area")) %>% 
  mutate(Resolution = factor(Resolution, c("Region", "Department", "PMSI area"))) %>% 
  mutate(age_group = factor(age_group, levels = c(age_vec, "ALL"))) %>%
  filter(Year %in% 2020:2024) %>% 
  ggplot(aes(x = as.character(Year), y = catchment_HCL, fill = Resolution)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~age_group) + 
  theme_bw() + 
  labs(x = "Year", y = "Size of catchment population") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("output/dep_reg_pmsi_comparison_HCL.png", width = 20, height = 10, units = "cm")
      
# calculate proportion excluded

reg_ageyear_allhcl %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                            RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                            T ~ "Metropolitan")) %>% 
  group_by(Year, RegCat) %>% 
  # mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  # filter((RegCode %in% c("Others", "Reg missing", "COM"))) %>% 
  # group_by(Year, RegCode) %>% 
  summarise(across(c("n", "n_HCL"), ~sum(.x, na.rm = T))) %>% 
  arrange(RegCat) %>% 
  group_by(Year) %>% 
  mutate(Total_n = sum(n), Total_n_HCL = sum(n_HCL)) %>% 
  mutate(p_excluded_n = n/Total_n, p_excluded_nHCL = n_HCL/Total_n_HCL) %>% 
  filter(RegCat == "Others")


dep_ageyear_allhcl %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                            RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                            T ~ "Metropolitan")) %>% 
  group_by(Year, RegCat) %>% 
  # mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  # filter((RegCode %in% c("Others", "Reg missing", "COM"))) %>% 
  # group_by(Year, RegCode) %>% 
  summarise(across(c("n", "n_HCL"), ~sum(.x, na.rm = T))) %>% 
  group_by(Year) %>% 
  mutate(Total_n = sum(n), Total_n_HCL = sum(n_HCL)) %>% 
  filter(RegCat == "Others") %>% 
  ungroup %>% 
  summarise(across(n:Total_n_HCL, sum)) %>% 
  mutate(p_excluded_n = n/Total_n, p_excluded_nHCL = n_HCL/Total_n_HCL)


pmsi_year_allhcl %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                            RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                            T ~ "Metropolitan")) %>% 
  group_by(Year, RegCat) %>% 
  # mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  # filter((RegCode %in% c("Others", "Reg missing", "COM"))) %>% 
  # group_by(Year, RegCode) %>% 
  summarise(across(c("n", "n_HCL"), ~sum(.x, na.rm = T))) %>% 
  group_by(Year) %>% 
  mutate(Total_n = sum(n), Total_n_HCL = sum(n_HCL)) %>% 
  filter(RegCat == "Others") %>% 
  ungroup %>% 
  summarise(across(n:Total_n_HCL, sum)) %>% 
  mutate(p_excluded_n = n/Total_n, p_excluded_nHCL = n_HCL/Total_n_HCL)

# ageyear

# diagnostic

dep_diag_allhcl <- full_join(dep_diag_all %>% select(DepCode, RegCode, DiagCat, n), 
                             dep_diag_hcl %>% select(DepCode, RegCode, DiagCat, n_HCL)) %>% 
  mutate(Year = 2024) %>% 
  left_join(dep_pop %>% 
              filter(Year == 2024) %>% 
              group_by(DepCode) %>% 
              summarise(Pop = sum(Pop))) %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                            RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                            T ~ "Metropolitan"))




# examine edge cases compare this to the metropolitan contribution
dep_diag_allhcl %>% 
  group_by(RegCat) %>% 
  # filter(!(RegCat %in% c("Others", "Reg missing", "COM"))) %>% 
  summarise(across(c("n", "n_HCL"), ~sum(.x, na.rm = T))) %>% 
  arrange(RegCat) %>% 
  print(n = 200)

# calculate the size of the catchment population
dep_diag_catchment <- dep_diag_allhcl %>% 
  mutate(p_HCL = ifelse(n == 0, 0, replace_na(n_HCL, 0)/n), 
         catchment_HCL = p_HCL*Pop)


dep_diag_catchment %>% 
  openxlsx::write.xlsx("output/dep_diag_catchment.xlsx")

dep_diag_catchment %>% 
  filter(RegCat == "Metropolitan") %>% 
  group_by(DiagCat) %>% 
  summarise(catchment_HCL = sum(catchment_HCL, na.rm = T), 
            n_HCL = sum(n_HCL, na.rm = T), 
            n = sum(n, na.rm = T) 
            ) %>% 
  arrange(-catchment_HCL) %>% 
  mutate(p = n_HCL/n) %>% 
  select(DiagCat, catchment_HCL) %>% 
    openxlsx::write.xlsx("output/dep_diag_catchment_summTable.xlsx")

reg_diag_allhcl <- full_join(reg_diag_all %>% select(RegCode, DiagCat, n), 
                             reg_diag_hcl %>% select(RegCode, DiagCat, n_HCL)) %>% 
  left_join(reg_pop %>% 
              filter(Year == 2024) %>% 
              group_by(RegCode) %>% 
              summarise(Pop = sum(Pop))) %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                            RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                            T ~ "Metropolitan"))




# examine edge cases compare this to the metropolitan contribution
reg_diag_allhcl %>% 
  group_by(RegCat) %>% 
  # filter(!(RegCat %in% c("Others", "Reg missing", "COM"))) %>% 
  summarise(across(c("n", "n_HCL"), ~sum(.x, na.rm = T))) %>% 
  arrange(RegCat) %>% 
  print(n = 200)

# calculate the size of the catchment population
reg_diag_catchment <- reg_diag_allhcl %>% 
  filter(RegCat == "Metropolitan") %>% 
  mutate(p_HCL = ifelse(n == 0, 0, replace_na(n_HCL, 0)/n), 
         catchment_HCL = p_HCL*Pop)


reg_diag_catchment %>% 
  group_by(DiagCat) %>% 
  summarise(catchment_HCL = sum(catchment_HCL, na.rm = T)) %>% 
  arrange(-catchment_HCL)




# HCL vs FINESS


# ara

urg_catchment <- ara %>% 
  group_by(RegCode, DepCode, age_group, Year) %>% 
  summarise(n = sum(n, na.rm = T), n_HCL = sum(n_HCL, na.rm = T)) %>% 
  # left_join(dep_pop %>% group_by(DepCode, Year) %>% summarise(Pop = sum(Pop))) %>% 
  left_join(dep_pop_age %>% select(DepCode, Year, age_group, Pop)) %>% 
  mutate(age_group = factor(age_group, levels = age_vec)) %>% 
  mutate(p_HCL = n_HCL/n) %>% 
  mutate(catchment_HCL = p_HCL*Pop)



urg_catchment %>% 
  filter(RegCode == "84") %>% 
  filter(!is.na(age_group)) %>% 
  openxlsx::write.xlsx(file = "output/dep_urg_catchment.xlsx")

urg_catchment %>% 
  filter(RegCode == "84") %>% 
  filter(!is.na(age_group)) %>% 
  group_by(Year, age_group) %>% 
  summarise(catchment_HCL = sum(catchment_HCL)) %>% 
  mutate(Type = "Emergency") %>% 
  rbind(urg_catchment %>% 
          filter(RegCode == "84") %>% 
          filter(!is.na(age_group)) %>% 
          group_by(Year) %>% 
          summarise(catchment_HCL = sum(catchment_HCL)) %>% 
          mutate(Type = "Emergency", age_group = "All")) %>% 
  rbind(dep_ageyear_catchment %>% 
          filter(RegCode == "84") %>% 
          group_by(Year) %>% 
          summarise(catchment_HCL = sum(catchment_HCL)) %>% 
          mutate(Type = "Hospitalisation", age_group = "All")) %>% 
  ungroup %>% 
  mutate(Region = "ARA") %>% 
  pivot_wider(id_cols = c("Region", "Type", "age_group"), names_from = "Year", values_from = "catchment_HCL") %>% 
  write_csv2("output/urgARA_table.csv")


## all finess

finess_year_pmsi_allhosp <- finess_year_pmsi %>% 
  transmute(FINESS_Hosp, RegCode = RegCode_Patient, DepCode = DepCode_Patient, PMSIcode = PMSIcode_Patient, OtherCountry, Year, n_Hosp = n) %>% 
  left_join(pmsi_year_all %>% 
              transmute(PMSIcode = PMSIcode, Year, n)) %>% 
  left_join(pmsi_pop %>% transmute(PMSIcode, Year, PopPMSI))

# check the edge cases
finess_year_pmsi_allhosp %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter((RegCode %in% c("Others", "Reg missing", "COM")) | !is.na(OtherCountry)) %>% 
  group_by(Year, RegCode, OtherCountry) %>% 
  summarise(across(c("n_Hosp"), ~sum(.x, na.rm = T))) %>% 
  ungroup %>% 
  arrange(-n_Hosp) %>% 
  filter(Year == 2024) %>% 
  print(n = 100)

finess_year_pmsi_catchment <- finess_year_pmsi_allhosp %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter(!(RegCode %in% c("Others", "Reg missing", "COM")), 
         is.na(OtherCountry), 
         !grepl("999$", PMSIcode), 
         PMSIcode != "99100") %>% 
  ungroup %>% 
  mutate(pHosp = n_Hosp/n, 
         catchment_Hosp = pHosp*PopPMSI)

# this is the total, it should be close to the population of France
finess_year_pmsi_catchment %>% 
  group_by(Year) %>% 
  summarise(catchment_Hosp = sum(catchment_Hosp))

finess_year_pmsi_catchment %>% 
  filter(grepl("HOSPICES CIVILS DE LYON", FINESS_Hosp)) %>% 
  group_by(Year) %>% 
  summarise(catchment_Hosp = sum(catchment_Hosp))



# dep 
finess_year_dep_allhosp <- finess_year_dep %>% 
  transmute(FINESS_Hosp, RegCode = RegCode_Patient, DepCode = DepCode_Patient, Year, n_Hosp = n) %>% 
  left_join(dep_year_all %>% 
              transmute(DepCode, Year, n)) %>% 
  left_join(dep_pop %>% group_by(DepCode, Year) %>% summarise(Pop = sum(Pop)))


# check the edge cases
finess_year_dep_allhosp %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter((RegCode %in% c("Others", "Reg missing", "COM"))) %>% 
  group_by(Year, RegCode) %>% 
  summarise(across(c("n_Hosp"), ~sum(.x, na.rm = T))) %>% 
  ungroup %>% 
  arrange(-n_Hosp) %>% 
  filter(Year == 2024) %>% 
  print(n = 100)

dep_year_all %>% 
  group_by(Year) %>% 
  summarise(n = sum(n, na.rm = T))

finess_year_dep_allhosp %>% 
  mutate(RegCode = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                             RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                             T ~ "Metropolitan")) %>% 
  mutate(RegCode = ifelse(RegCode %in% c("Others", "Reg missing", "COM"), RegCode, "Metropolitan")) %>% 
  group_by(Year, RegCode) %>% 
  summarise(across(c("n_Hosp"), ~sum(.x, na.rm = T))) %>% 
  ungroup %>% 
  arrange(-n_Hosp) %>% 
  filter(Year == 2024) %>% 
  print(n = 100)

dep_year_all %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                             RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                             T ~ "Metropolitan")) %>% 
  group_by(Year, RegCat) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  pivot_wider(id_cols = "Year", names_from = "RegCat", values_from = "n") %>% 
  mutate(Total = COM + Metropolitan + `Reg missing`) %>% 
  select(Year, Metropolitan, COM, `Reg missing`, Total) %>% 
  mutate(met_pct = Metropolitan/Total, com_pct = COM/Total, missing_pct = `Reg missing`/Total) %>%
  write_csv2("output/total_hospitalisations.csv")



finess_year_dep_catchment <- finess_year_dep_allhosp %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter(!(RegCode %in% c("Others", "Reg missing", "COM")), 
         # is.na(OtherCountry), 
         # !grepl("999$", PMSIcode), 
         # PMSIcode != "99100"
         ) %>% 
  ungroup %>% 
  mutate(pHosp = n_Hosp/n, 
         catchment_Hosp = pHosp*Pop)

# this is the total, it should be close to the population of France
finess_year_dep_catchment %>% 
  group_by(Year) %>% 
  summarise(catchment_Hosp = sum(catchment_Hosp))

finess_year_dep_catchment %>% 
  filter(grepl("HOSPICES CIVILS DE LYON", FINESS_Hosp)) %>% 
  group_by(Year) %>% 
  summarise(catchment_Hosp = sum(catchment_Hosp))


dep_year_all %>% 
  mutate(RegCat = case_when(RegCode %in% c(paste0("0", 1:6), "TOM") ~ "COM", 
                            RegCode %in% c("Others", "Reg missing") ~ RegCode, 
                            T ~ "Metropolitan")) %>% 
  group_by(Year, RegCat) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  pivot_wider(id_cols = "Year", names_from = "RegCat", values_from = "n") %>% 
  mutate(Total = COM + Metropolitan + `Reg missing`) %>% 
  select(Year, Metropolitan, COM, `Reg missing`, Total) %>% 
  left_join(finess_year_dep_catchment %>% 
              group_by(Year) %>% 
              summarise(catchment_Metropolitan = sum(catchment_Hosp))) %>% 
  left_join(dep_pop %>% filter(!(RegCode %in% c(paste0("0", 1:6), "TOM"))) %>% group_by(Year) %>% summarise(MetropolitanPop = sum(Pop))) %>% 
  mutate(met_pct = Metropolitan/Total, com_pct = COM/Total, missing_pct = `Reg missing`/Total) %>%
  write_csv2("output/total_hospitalisations.csv")

finess_year_dep_catchment %>% 
  mutate(FINESS_Hosp = case_when(FINESS_Hosp == "310781406 - CHR TOULOUSE" ~ "310781406 - CHU TOULOUSE", 
                                 T ~ FINESS_Hosp)) %>% 
  group_by(FINESS_Hosp, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), catchment_Hosp = sum(catchment_Hosp)) %>% 
  group_by(FINESS_Hosp) %>% 
  summarise(n_Hosp = mean(n_Hosp), catchment_Hosp = mean(catchment_Hosp)) %>% 
  arrange(-n_Hosp) %>% 
  mutate(p = n_Hosp/catchment_Hosp) %>% 
  openxlsx::write.xlsx("output/finess_mean_catchment_200.xlsx")


## all finess

finessgeo_year_pmsi_allhosp <- finessgeo_year_pmsi %>% 
  transmute(FINESS_Hosp, Geo_Hosp, RegCode = RegCode_Patient, DepCode = DepCode_Patient, PMSIcode = PMSIcode_Patient, OtherCountry, Year, n_Hosp = n) %>% 
  left_join(pmsi_year_all %>% 
              transmute(PMSIcode = PMSIcode, Year, n)) %>% 
  left_join(pmsi_pop %>% transmute(PMSIcode, Year, PopPMSI))

# check the edge cases
finessgeo_year_pmsi_allhosp %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter((RegCode %in% c("Others", "Reg missing", "COM")) | !is.na(OtherCountry)) %>% 
  group_by(Year, RegCode, OtherCountry) %>% 
  summarise(across(c("n_Hosp"), ~sum(.x, na.rm = T))) %>% 
  ungroup %>% 
  arrange(-n_Hosp) %>% 
  filter(Year == 2024) %>% 
  print(n = 100)

finessgeo_year_pmsi_catchment <- finessgeo_year_pmsi_allhosp %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter(!(RegCode %in% c("Others", "Reg missing", "COM")), 
         is.na(OtherCountry), 
         !grepl("999$", PMSIcode), 
         PMSIcode != "99100") %>% 
  ungroup %>% 
  mutate(pHosp = n_Hosp/n, 
         catchment_Hosp = pHosp*PopPMSI)

# this is the total, it should be close to the population of France
finessgeo_year_pmsi_catchment %>% 
  group_by(Year) %>% 
  summarise(catchment_Hosp = sum(catchment_Hosp))

finessgeo_year_pmsi_catchment %>% 
  filter(grepl("HOSPICES CIVILS DE LYON", FINESS_Hosp)) %>% 
  group_by(Year) %>% 
  summarise(catchment_Hosp = sum(catchment_Hosp))


# dep 
finessgeo_year_dep_allhosp <- finessgeo_year_dep %>% 
  transmute(FINESS_Hosp, Geo_Hosp, RegCode = RegCode_Patient, DepCode = DepCode_Patient, Year, n_Hosp = n) %>% 
  left_join(dep_year_all %>% 
              transmute(DepCode, Year, n)) %>% 
  left_join(dep_pop %>% group_by(DepCode, Year) %>% summarise(Pop = sum(Pop)))

# check the edge cases
finessgeo_year_dep_allhosp %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter((RegCode %in% c("Others", "Reg missing", "COM"))) %>% 
  group_by(Year, RegCode) %>% 
  summarise(across(c("n_Hosp"), ~sum(.x, na.rm = T))) %>% 
  ungroup %>% 
  arrange(-n_Hosp) %>% 
  filter(Year == 2024) %>% 
  print(n = 100)

finessgeo_year_dep_catchment <- finessgeo_year_dep_allhosp %>% 
  mutate(RegCode = ifelse(RegCode %in% c(paste0("0", 1:6), "TOM"), "COM", RegCode)) %>% 
  filter(!(RegCode %in% c("Others", "Reg missing", "COM")), 
         # is.na(OtherCountry), 
         # !grepl("999$", PMSIcode), 
         # PMSIcode != "99100"
  ) %>% 
  ungroup %>% 
  mutate(pHosp = n_Hosp/n, 
         catchment_Hosp = pHosp*Pop)

finessgeo_year_dep_catchment %>% 
  mutate(FINESS_Hosp = case_when(FINESS_Hosp == "310781406 - CHR TOULOUSE" ~ "310781406 - CHU TOULOUSE", 
                                 T ~ FINESS_Hosp)) %>% 
  group_by(Geo_Hosp, FINESS_Hosp, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), catchment_Hosp = sum(catchment_Hosp)) %>% 
  group_by(Geo_Hosp, FINESS_Hosp) %>% 
  summarise(n_Hosp = mean(n_Hosp), catchment_Hosp = mean(catchment_Hosp)) %>% 
  arrange(-n_Hosp) %>% 
  mutate(p = n_Hosp/catchment_Hosp) %>% 
  openxlsx::write.xlsx("output/finessgeo_mean_catchment_200.xlsx")


# this is the total, it should be close to the population of France
finessgeo_year_dep_catchment %>% 
  group_by(Year) %>% 
  summarise(catchment_Hosp = sum(catchment_Hosp))

finess_year_dep_catchment %>% 
  group_by(Year) %>% 
  summarise(catchment_Hosp = sum(catchment_Hosp))

dep_pop %>% 
  group_by(Year) %>% 
  summarise(Pop = sum(Pop))

finessgeo_year_dep_catchment %>% 
  filter(grepl("HOSPICES CIVILS DE LYON", FINESS_Hosp)) %>% 
  group_by(Year) %>% 
  summarise(catchment_Hosp = sum(catchment_Hosp))


