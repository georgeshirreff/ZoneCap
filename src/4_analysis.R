
library(tidyverse)
library(lemon)

# conduct analyses and make basic tables and figures

# merge data
# handle the DOM and missing regions
# create a corrected and uncorrected version


# make catchment tables
reg_year_finess_catchment_poparea <- reg_year_finess %>% 
  rename(n_Hosp = n) %>% 
  full_join(reg_year_all) %>% 
  left_join(reg_pop %>% group_by(Year, RegCode) %>% summarise(Pop = sum(Pop))) %>% 
  left_join(shp_reg %>% 
              select(RegCode, Area_km2) %>% 
              st_drop_geometry() %>% 
              ungroup) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp, 
         catchmentArea = pHosp*Area_km2)

# remove(dep_year_finess_catchment)

dep_year_finess_catchment_poparea <- dep_year_finess %>% 
  rename(n_Hosp = n) %>% 
  full_join(dep_year_all) %>% 
  left_join(dep_pop %>% group_by(Year, DepCode) %>% summarise(Pop = sum(Pop))) %>% 
  left_join(shp_dep %>% 
              select(DepCode, Area_km2) %>% 
              st_drop_geometry() %>% 
              ungroup) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp, 
         catchmentArea = pHosp*Area_km2)


# dep_year_finessgeo_catchment <- dep_year_finessgeo %>% 
#   rename(n_Hosp = n) %>% 
#   full_join(dep_year_all) %>% 
#   left_join(dep_pop %>% group_by(Year, DepCode) %>% summarise(Pop = sum(Pop))) %>% 
#   mutate(pHosp = n_Hosp/n, 
#          catchmentHosp = Pop*pHosp)

# remove(dep_year_finessgeo_catchment)
dep_year_finessgeo_catchment_poparea <- dep_year_finessgeo %>% #this can replace the previous assignment
  rename(n_Hosp = n) %>% 
  full_join(dep_year_all) %>% 
  left_join(dep_pop %>% group_by(Year, DepCode) %>% summarise(Pop = sum(Pop))) %>% 
  left_join(shp_dep %>% 
              select(DepCode, Area_km2) %>% 
              st_drop_geometry() %>% 
              ungroup) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp, 
         catchmentArea = pHosp*Area_km2)

depDOM_year_finessgeo_catchment_poparea <- depDOM_year_finessgeo %>% 
  rename(n_Hosp = n) %>% 
  full_join(depDOM_year_all) %>% 
  left_join(dep_pop %>% group_by(Year, DepCode) %>% summarise(Pop = sum(Pop))) %>% 
  left_join(rbind(
    shp_depDOM %>% 
      select(DepCode, Area_km2) %>% 
      st_drop_geometry() %>% 
      ungroup, 
    COM_area
  )) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp, 
         catchmentArea = pHosp*Area_km2)

# remove(depDOM_year_finessgeo_catchment)
# depDOM_year_finess_catchment_poparea <- depDOM_year_finess_catchment %>% #can be combined with previous assignment
#   left_join(rbind(
#     shp_depDOM %>% 
#       select(DepCode, Area_km2) %>% 
#       st_drop_geometry() %>% 
#       ungroup, 
#     COM_area
#   )) %>% 
#   mutate(catchmentArea = pHosp*Area_km2)

pmsi_year_finess_catchment_poparea <- pmsi_year_finess %>% 
  rename(n_Hosp = n) %>% 
  full_join(pmsi_year_all) %>% 
  left_join(pmsi_pop %>% transmute(Year, RegCode, DepCode, PMSIcode, Pop = PopPMSI)) %>% 
  left_join(shp_pmsi %>% 
              select(PMSIcode, Area_km2) %>% 
              st_drop_geometry() %>% 
              ungroup) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp, 
         catchmentArea = Area_km2*pHosp)

# remove(pmsi_year_finess_catchment)

pmsi_year_finessgeo_catchment_poparea <- pmsi_year_finessgeo %>% 
  rename(n_Hosp = n) %>% 
  full_join(pmsi_year_all %>% transmute(Year, PMSIcode, n)) %>% 
  left_join(pmsi_pop %>% transmute(Year, PMSIcode, Pop = PopPMSI)) %>% 
  left_join(shp_pmsi %>% 
              select(PMSIcode, Area_km2) %>% 
              st_drop_geometry() %>% 
              ungroup) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp, 
         catchmentArea = Area_km2*pHosp)

# remove(pmsi_year_finessgeo_catchment)
depDOM_year_finess_catchment_poparea <- depDOM_year_finess %>% 
  rename(n_Hosp = n) %>% 
  full_join(depDOM_year_all) %>% 
  left_join(dep_pop %>% group_by(Year, DepCode) %>% summarise(Pop = sum(Pop))) %>% 
  left_join(rbind(
    shp_depDOM %>% 
      select(DepCode, Area_km2) %>% 
      st_drop_geometry() %>% 
      ungroup, 
    COM_area
  )) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp, 
         catchmentArea = Area_km2*pHosp)


dep_ageyear_hcl_catchment <- dep_ageyear_hcl %>% 
  rename(n_Hosp = n) %>% 
  full_join(dep_ageyear_all) %>% 
  left_join(dep_pop_age %>% select(DepCode, Year, age_group, Pop)) %>% 
  fill(FINESS_Hosp, .direction = "down") %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp)

dep_age0year_hcl_catchment <- dep_age0year_hcl %>% 
  rename(n_Hosp = n) %>% 
  full_join(dep_age0year_all) %>% 
  left_join(dep_pop_ageFine %>% filter(Age == 0) %>% select(DepCode, Year, Pop)) %>% 
  fill(FINESS_Hosp, .direction = "down") %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp)

dep_age20year_finess_catchment <- dep_age20year_finess %>% 
  rename(n_Hosp = n) %>% 
  full_join(dep_age20year_all) %>% 
  left_join(dep_pop_age20 %>% transmute(DepCode, Year, age20 = age20_group, Pop)) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp)

reg_age20year_finess_catchment <- reg_age20year_finess %>% 
  rename(n_Hosp = n) %>% 
  full_join(reg_age20year_all) %>% 
  left_join(reg_pop_age20 %>% transmute(RegCode, Year, age20 = age20_group, Pop)) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp)

pmsi_age20year_finess_catchment <- pmsi_age20year_finess %>% 
  rename(n_Hosp = n) %>% 
  full_join(pmsi_age20year_all) %>% 
  left_join(pmsi_pop_age20 %>% transmute(PMSIcode, Year, age20 = age20_group, Pop)) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp)

# pmsi_ageyear_HCLcatchment <- pmsi_ageyear_hcl %>% 
#   rename(n_Hosp = n) %>% 
#   full_join(pmsi_ageyear_all) %>% 
#   left_join(pmsi_pop_age %>% transmute(Year, PMSIcode, age_group, Pop)) %>% 
#   mutate(pHosp = n_Hosp/n, 
#          catchmentHosp = Pop*pHosp)

# remove(pmsi_ageyear_HCLcatchment)
pmsi_ageyear_HCLcatchment_poparea <- pmsi_ageyear_hcl %>% # can replace previous assignment
  rename(n_Hosp = n) %>% 
  full_join(pmsi_ageyear_all) %>% 
  left_join(pmsi_pop_age %>% transmute(Year, PMSIcode, age_group, Pop)) %>% 
  left_join(shp_pmsi %>% 
              st_drop_geometry() %>% 
              select(PMSIcode, Area_km2)) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp, 
         catchmentArea = Area_km2*pHosp)



# dep_year_diag_catchment <- dep_year_diag_finess %>% 
#   rename(n_Hosp = n) %>% 
#   full_join(dep_year_diag_all) %>% 
#   left_join(dep_pop %>% group_by(DepCode, Year) %>% summarise(Pop = sum(Pop))) %>% 
#   mutate(pHosp = n_Hosp/n, 
#          catchmentHosp = Pop*pHosp)

# remove(dep_year_diag_catchment)
dep_year_diag_catchment_poparea <- dep_year_diag_finess %>% #can replace the previous assignment
  rename(n_Hosp = n) %>% 
  full_join(dep_year_diag_all) %>% 
  left_join(dep_pop %>% group_by(DepCode, Year) %>% summarise(Pop = sum(Pop))) %>% 
  left_join(shp_dep %>% 
              select(DepCode, Area_km2) %>% 
              st_drop_geometry() %>% 
              ungroup) %>% 
  mutate(pHosp = n_Hosp/n, 
         catchmentHosp = Pop*pHosp, 
         catchmentArea = pHosp*Area_km2)


# dep_year_diag_finess




# catchment summaries

reg_year_finess_catchment_poparea %>% 
  group_by(Year) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T))

dep_year_finess_catchment_poparea %>% 
  group_by(Year) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T))

pmsi_year_finess_catchment_poparea %>% 
  group_by(Year) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T))

depDOM_year_finess_catchment_poparea %>% 
  group_by(Year) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T))

dep_year_diag_catchment_poparea %>% 
  group_by(Year, DiagCat) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T))


#### summary tables ####

# table showing yearly totals

dep_year_all %>% 
  group_by(Year) %>% 
  summarise(Hospitalisations_MET = sum(n, na.rm = T)) %>% 
  left_join(depDOM_year_all %>% 
              group_by(Year) %>% 
              summarise(Hospitalisations_DOM = sum(n, na.rm = T))
  ) %>% 
  left_join(depEVERY_year_all %>% 
              filter(RegCode == "Reg missing") %>% 
              transmute(Year, Hospitalisations_missing = n)) %>% 
  mutate(Hospitalisations_TOTAL = Hospitalisations_MET + Hospitalisations_DOM + Hospitalisations_missing) %>% 
  left_join(dep_year_finess_catchment_poparea %>% 
              group_by(Year) %>% 
              summarise(catchmentHosp = sum(catchmentHosp, na.rm = T), 
                        catchmentArea = sum(catchmentArea, na.rm = T))) %>% 
  left_join(reg_pop %>% 
              filter(RegCode %in% reg_levels_met) %>% 
              group_by(Year) %>%  
              summarise(Total_MET = sum(Pop, na.rm = T))) %>% 
  left_join(
    tibble(Year = 2016:2025, Area_MET = c(rep(549134, 4), rep(549087, 6)) + 105.4) #https://www.insee.fr/fr/statistiques/1405599?geo=ARR-751 + #https://www.insee.fr/fr/statistiques/serie/001751672
  ) %>% 
  mutate(met_pct = Hospitalisations_MET/Hospitalisations_TOTAL, 
         com_pct = Hospitalisations_DOM/Hospitalisations_TOTAL, 
         missing_pct = Hospitalisations_missing/Hospitalisations_TOTAL) %>%
  write_csv2("output/total_hospitalisations.csv")

# table showing the 200 largest hospital groups

# dep_year_finess_catchment %>% 
#   rename(FINESS_Hosp = Finess_Hosp) %>% 
#   mutate(FINESS_Hosp = case_when(FINESS_Hosp == "310781406 - CHR TOULOUSE" ~ "310781406 - CHU TOULOUSE", 
#                                  T ~ FINESS_Hosp)) %>% 
#   group_by(FINESS_Hosp, Year) %>% 
#   summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp)) %>% 
#   group_by(FINESS_Hosp) %>% 
#   summarise(n_Hosp = mean(n_Hosp), catchmentHosp = mean(catchmentHosp)) %>% 
#   arrange(-n_Hosp) %>% 
#   mutate(p = n_Hosp/catchmentHosp) %>% 
#   filter(FINESS_Hosp != "Others") %>% 
#   openxlsx::write.xlsx("output/finess_mean_catchment_200.xlsx")

dep_year_finess_catchment_poparea %>% 
  group_by(FINESS_Hosp, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp), catchmentArea = sum(catchmentArea)) %>% 
  group_by(FINESS_Hosp) %>% 
  summarise(n_Hosp = mean(n_Hosp), catchmentHosp = mean(catchmentHosp), catchmentArea = mean(catchmentArea)) %>% 
  arrange(-n_Hosp) %>% 
  mutate(p = n_Hosp/catchmentHosp) %>% 
  filter(FINESS_Hosp != "Others") %>% 
  openxlsx::write.xlsx("output/finess_mean_catchment_FULL.xlsx")


# table showing the 200 largest individual hospitals
# dep_year_finessgeo_catchment %>% 
#   group_by(Geo_Hosp, FINESS_Hosp, Year) %>% 
#   summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp)) %>% 
#   group_by(Geo_Hosp, FINESS_Hosp) %>% 
#   summarise(n_Hosp = mean(n_Hosp), catchmentHosp = mean(catchmentHosp)) %>% 
#   arrange(-n_Hosp) %>% 
#   mutate(p = n_Hosp/catchmentHosp) %>% 
#   filter(FINESS_Hosp != "Others") %>% 
#   openxlsx::write.xlsx("output/finessgeo_mean_catchment_200.xlsx")

dep_year_finessgeo_catchment_poparea %>% 
  group_by(Geo_Hosp, FINESS_Hosp, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp), catchmentArea = sum(catchmentArea)) %>% 
  group_by(Geo_Hosp, FINESS_Hosp) %>% 
  summarise(n_Hosp = mean(n_Hosp), catchmentHosp = mean(catchmentHosp), catchmentArea = mean(catchmentArea)) %>% 
  arrange(-n_Hosp) %>% 
  mutate(p = n_Hosp/catchmentHosp) %>% 
  filter(FINESS_Hosp != "Others") %>% 
  openxlsx::write.xlsx("output/finessgeo_mean_catchment_FULL.xlsx")



# table showing the 200 largest hospital groups in the DOM
# depDOM_year_finess_catchment %>% 
#   rename(FINESS_Hosp = Finess_Hosp) %>% 
#   filter(FINESS_Hosp != "930300025 - HÔPITAL EUROPEEN DE PARIS") %>% 
#   group_by(FINESS_Hosp, Year) %>% 
#   summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
#   group_by(FINESS_Hosp) %>% 
#   summarise(n_Hosp = mean(n_Hosp), catchmentHosp = mean(catchmentHosp)) %>% 
#   arrange(-n_Hosp) %>% 
#   mutate(p = n_Hosp/catchmentHosp) %>% 
#   filter(FINESS_Hosp != "Others") %>% 
#   openxlsx::write.xlsx("output/finess_DOM_mean_catchment_200.xlsx")

# remove(depDOM_year_finess_catchment)
depDOM_year_finess_catchment_poparea %>% 
  group_by(FINESS_Hosp, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp, na.rm = T), catchmentArea = sum(catchmentArea, na.rm = T)) %>% 
  group_by(FINESS_Hosp) %>% 
  summarise(n_Hosp = mean(n_Hosp), catchmentHosp = mean(catchmentHosp), catchmentArea = mean(catchmentArea)) %>% 
  arrange(-n_Hosp) %>% 
  mutate(p = n_Hosp/catchmentHosp) %>% 
  filter(FINESS_Hosp != "Others") %>% 
  openxlsx::write.xlsx("output/finess_DOM_mean_catchment_FULL.xlsx")


# table with HCL age breakdown


dep_ageyear_hcl_catchment %>% 
  write_csv2("output/dep_ageyear_HCLcatchment.csv")

HCLcatchment_ageyear <- dep_ageyear_hcl_catchment %>% 
  filter(DepCode != "Others") %>% 
  group_by(Year, age_group) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
  ungroup

HCLcatchment_age0year <- dep_age0year_hcl_catchment %>% 
  filter(DepCode != "Others") %>% 
  group_by(Year, age_group) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
  ungroup




HCLcatchment_ageyear %>% 
  ggplot(aes(x = as.factor(Year), fill = age_group, y = catchmentHosp)) + geom_bar(stat = "identity") + 
  theme_bw() + 
  labs(x = "Year", y = "Catchment population", fill = "Age group")

ggsave("output/dep_ageyear_HCLcatchment.png", width = 16, height = 8, units = "cm", dpi = 300)

# save the equivalent for the rhone population
dep_pop_age %>% 
  ggplot(aes(x = as.factor(Year), fill = age_group, y = Pop)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  labs(x = "Year", y = "Rhône population", fill = "Age group")

ggsave("output/dep_age_pop_Rhone.png", width = 16, height = 8, units = "cm", dpi = 300)


# compare different sources of catchment population

metropole_PMSIcodes = c(metropole$Postcode, 
                        paste0("6900", 1:9)
                        # , "69310"
                        , "69C02"
)


rbind(
  pmsi_pop_age %>% 
    filter(DepCode == "69") %>% 
    group_by(Year, age_group) %>% 
    summarise(catchmentHosp = sum(Pop)) %>% 
    mutate(Source = "Rhône")
  ,
  pmsi_pop_age %>% 
    filter(PMSIcode %in% metropole_PMSIcodes) %>% 
    group_by(Year, age_group) %>% 
    summarise(catchmentHosp = sum(Pop)) %>% 
    mutate(Source = "Lyon Metropole")
  ,
  pmsi_ageyear_HCLcatchment_poparea %>% 
    group_by(Year, age_group) %>% 
    summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
    mutate(Source = "Proportional flow")
) %>% 
  mutate(Source = factor(Source, levels = c("Rhône", "Lyon Metropole", "Proportional flow"))) %>% 
  filter(Year > 2018) %>% 
  ggplot(aes(x = as.factor(Year), y = catchmentHosp, fill = Source)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + 
  facet_wrap(.~age_group, scales = "free_y") + 
  scale_fill_viridis_d(option = "viridis") + 
  # scale_fill_brewer(palette = "Set2") + 
  labs(x = "Year", y = "Estimated catchment population", fill = "Data source")

ggsave("output/pmsi_age_pop_comparisonSource.png", width = 30, height = 15, units = "cm", dpi = 300)




# save this for use in FIRESP and other work
save(HCLcatchment_ageyear, file = "output/HCLcatchment_ageyear.RData")
save(HCLcatchment_age0year, file = "output/HCLcatchment_age0year.RData")



HCLcatchment_ageyear %>% 
  {rbind(., 
         group_by(., Year) %>% summarise(catchmentHosp = sum(catchmentHosp), age_group = "ALL") 
    )} %>% 
  pivot_wider(id_cols = age_group, names_from = Year, values_from = catchmentHosp) %>% 
  write_csv2("output/dep_ageyear_HCLcatchment_summTable.csv")

francemet_age %>% 
  filter(Year %in% (2016:2025)) %>% 
  mutate(age_group = case_when(Age < 2 ~ "0-1",
                               Age < 5 ~ "2-4",
                               Age < 18 ~ "5-17",
                               Age < 65 ~ "18-64",
                               Age < 80 ~ "65-79",
                               Age >= 80 ~ "80+") %>% 
           factor(levels = age_vec)) %>% 
  group_by(Year, age_group) %>% 
  summarise(Pop = sum(Pop)) %>% 
  group_by(Year) %>% 
  mutate(pPop = Pop/sum(Pop)) %>% 
  {rbind(., 
         group_by(., Year) %>% summarise(pPop = sum(pPop), age_group = "ALL") 
  )} %>% 
  pivot_wider(id_cols = age_group, names_from = Year, values_from = pPop) %>% 
  write_csv2("output/ageyear_pPop.csv")


dep_age20year_finess_catchment %>% 
  filter(grepl("690781810", FINESS_Hosp)) %>% 
  group_by(Year, age20) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
  ungroup %>% 
  {rbind(., 
         group_by(., Year) %>% summarise(catchmentHosp = sum(catchmentHosp), age20 = "ALL") 
  )} %>% 
  pivot_wider(id_cols = age20, names_from = Year, values_from = catchmentHosp) %>% 
  write_csv2("output/dep_age20year_HCLcatchment_summTable.csv")


# lineplot showing size of catchment by age and by resolution
 
rbind(reg_age20year_finess_catchment %>% 
        select(FINESS_Hosp, Year, age20, catchmentHosp) %>% 
        {rbind(., 
               group_by(., FINESS_Hosp, Year) %>% summarise(catchmentHosp = sum(catchmentHosp, na.rm = T), age20 = "ALL")
        )} %>% 
        group_by(FINESS_Hosp, Year, age20) %>% 
        summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
        mutate(Resolution = "Region")
      , dep_age20year_finess_catchment %>% 
        select(FINESS_Hosp, Year, age20, catchmentHosp) %>% 
        {rbind(., 
               group_by(., FINESS_Hosp, Year) %>% summarise(catchmentHosp = sum(catchmentHosp, na.rm = T), age20 = "ALL")
        )} %>% 
        group_by(FINESS_Hosp, Year, age20) %>% 
        summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
        mutate(Resolution = "Department")
      , pmsi_age20year_finess_catchment %>% 
        select(FINESS_Hosp, Year, age20, catchmentHosp) %>% 
        {rbind(., 
               group_by(., FINESS_Hosp, Year) %>% summarise(catchmentHosp = sum(catchmentHosp, na.rm = T), age20 = "ALL")
        )} %>% 
        group_by(FINESS_Hosp, Year, age20) %>% 
        summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
        mutate(Resolution = "PMSI code")) %>% 
  mutate(Resolution = factor(Resolution, c("Region", "Department", "PMSI code"))) %>% 
  filter(age20 != "Others") %>% 
  mutate(age20 = factor(age20, levels = c(age20_vec, "ALL"))) %>%
  filter(grepl("690781810", FINESS_Hosp)) %>% 
  ggplot(aes(x = Year, y = catchmentHosp, fill = Resolution, colour = Resolution, group = Resolution)) + 
  # geom_bar(stat = "identity", position = "dodge") + 
  geom_line(stat = "identity") + 
  facet_wrap(.~age20, scales = "free_y") + 
  theme_bw() + 
  labs(x = "Year", y = "Size of catchment population") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  coord_cartesian(ylim = c(0, NA)) + 
  scale_x_continuous(breaks = 2016:2025)

ggsave("output/dep_reg_pmsi_comparison_HCL.png", width = 20, height = 10, units = "cm", dpi = 300)

# proportion of masking


reg_n <- reg_age20year_all %>% 
  pull(n) %>% 
  sum(na.rm = T)

reg_n_other <- reg_age20year_all %>% 
  filter(RegCode == "Others") %>% 
  pull(n) %>% 
  sum(na.rm = T)

reg_hcl_n <- reg_age20year_hcl %>% 
  pull(n) %>% 
  sum(na.rm = T)

reg_hcl_n_other <- reg_age20year_hcl %>% 
  filter(RegCode == "Others") %>% 
  pull(n) %>% 
  sum(na.rm = T)



dep_n <- dep_ageyear_all %>% 
  pull(n) %>% 
  sum(na.rm = T)

dep_n_other <- dep_ageyear_all %>% 
  filter(RegCode == "Others") %>% 
  pull(n) %>% 
  sum(na.rm = T)

dep_hcl_n <- dep_ageyear_hcl %>% 
  pull(n) %>% 
  sum(na.rm = T)

dep_hcl_n_other <- dep_ageyear_hcl %>% 
  filter(RegCode == "Others") %>% 
  pull(n) %>% 
  sum(na.rm = T)


pmsi_n <- pmsi_ageyear_all %>% 
  pull(n) %>% 
  sum(na.rm = T)

pmsi_n_other <- pmsi_ageyear_all %>% 
  filter(PMSIcode == "Others") %>% 
  pull(n) %>% 
  sum(na.rm = T)

pmsi_hcl_n <- pmsi_ageyear_hcl %>% 
  pull(n) %>% 
  sum(na.rm = T)

pmsi_hcl_n_other <- pmsi_ageyear_hcl %>% 
  filter(PMSIcode == "Others") %>% 
  pull(n) %>% 
  sum(na.rm = T)

(reg_n_other/reg_n) %>% {paste0(round(., 4)*100, "%")} %>% cat
(reg_hcl_n_other/reg_hcl_n)  %>% {paste0(round(., 4)*100, "%")} %>% cat

(dep_n_other/dep_n) %>% {paste0(round(., 4)*100, "%")} %>% cat
(dep_hcl_n_other/dep_hcl_n) %>% {paste0(round(., 4)*100, "%")} %>% cat

(pmsi_n_other/pmsi_n) %>% {paste0(round(., 4)*100, "%")} %>% cat
(pmsi_hcl_n_other/pmsi_hcl_n) %>% {paste0(round(., 4)*100, "%")} %>% cat


# table of diagnostics 
# dep_year_diag_catchment %>% 
#   openxlsx::write.xlsx("output/dep_diag_catchment.xlsx")

dep_year_diag_catchment_poparea %>% 
  filter(grepl("690781810", FINESS_Hosp)) %>% 
  openxlsx::write.xlsx("output/dep_diag_HCLcatchment.xlsx")


# dep_diag_catchment_summTable <- dep_year_diag_catchment %>% 
#   filter(grepl("690781810", Finess_Hosp)) %>% 
#   group_by(DiagCat, DiagCat_FR, Year) %>% 
#   summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
#   group_by(DiagCat, DiagCat_FR) %>% 
#   summarise(catchmentHosp = mean(catchmentHosp, na.rm = T)) %>% 
#   arrange(-catchmentHosp) %>% 
#   ungroup

dep_diag_catchment_poparea_summTable <- dep_year_diag_catchment_poparea %>% 
  filter(grepl("690781810", FINESS_Hosp)) %>% 
  group_by(DiagCat, DiagCat_FR, Year) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T), 
            catchmentArea = sum(catchmentArea, na.rm = T)) %>% 
  group_by(DiagCat, DiagCat_FR) %>% 
  summarise(catchmentHosp = mean(catchmentHosp, na.rm = T), 
            catchmentArea = mean(catchmentArea, na.rm = T)) %>% 
  arrange(-catchmentHosp) %>% 
  ungroup

diagcat_order <- dep_diag_catchment_poparea_summTable$DiagCat

# remove(dep_diag_catchment_summTable)
# dep_diag_catchment_summTable %>% 
#   select(DiagCat, catchmentHosp) %>% 
#   openxlsx::write.xlsx("output/dep_diag_catchment_summTable.xlsx")

dep_diag_catchment_poparea_summTable %>% 
  select(DiagCat, catchmentHosp, catchmentArea) %>% 
  openxlsx::write.xlsx("output/dep_diag_catchment_summTable.xlsx")


dep_diag_catchment_poparea_summTable %>% 
  mutate(DiagCat_FR = factor(DiagCat_FR, levels = rev(unique(DiagCat_FR)))) %>% 
  ggplot(aes(x = catchmentHosp, y = DiagCat_FR)) + 
  geom_bar(stat = "identity") + 
  geom_vline(xintercept = 1.1e6, linetype = "dashed") + 
  theme_bw() + 
  labs(x = "HCL catchment population", y = "Catégorie diagnostique majeur")

ggsave("output/dep_diag_catchment_summ.png", width = 25, height = 10, units = "cm", dpi = 300)


dep_year_diag_catchment_poparea %>% 
  filter(grepl("690781810", FINESS_Hosp)) %>% 
  group_by(DiagCat, Year) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
  group_by(DiagCat) %>% 
  summarise(catchmentHosp = mean(catchmentHosp, na.rm = T)) %>% 
  arrange(-catchmentHosp)


finess10_order <- dep_year_diag_catchment_poparea %>% 
  rename(FINESS_Hosp = FINESS_Hosp) %>% 
  filter(FINESS_Hosp != "Others") %>% 
  # mutate(FINESS_Hosp = case_when(FINESS_Hosp == "310781406 - CHR TOULOUSE" ~ "310781406 - CHU TOULOUSE", 
  #                                T ~ FINESS_Hosp)) %>% 
  group_by(FINESS_Hosp, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp), catchmentArea = sum(catchmentArea)) %>% 
  group_by(FINESS_Hosp) %>% 
  summarise(n_Hosp = mean(n_Hosp)) %>% 
  arrange(-n_Hosp) %>% 
  slice(1:10) %>% 
  pull(FINESS_Hosp)

finess10_order

dep_year_diag_catchment_poparea %>% 
  filter(FINESS_Hosp != "Others") %>% 
  mutate(FINESS_Hosp = case_when(FINESS_Hosp == "310781406 - CHR TOULOUSE" ~ "310781406 - CHU TOULOUSE", 
                                 T ~ FINESS_Hosp)) %>% 
  mutate(FINESS_Hosp = factor(FINESS_Hosp, finess10_order), 
         DiagCat = factor(DiagCat, diagcat_order)) %>% 
  filter(!is.na(FINESS_Hosp)) %>% 
  group_by(FINESS_Hosp, DiagCat, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp), catchmentArea = sum(catchmentArea)) %>% 
  group_by(FINESS_Hosp, DiagCat) %>% 
  summarise(n_Hosp = mean(n_Hosp), catchmentHosp = mean(catchmentHosp), catchmentArea = mean(catchmentArea)) %>% 
  ungroup %>% 
  group_by(FINESS_Hosp) %>% 
  mutate(p_catchmentHosp = catchmentHosp/sum(catchmentHosp), 
         p_catchmentArea = catchmentArea/sum(catchmentArea), 
         p_nHosp = n_Hosp/sum(n_Hosp)) %>% 
  pivot_wider(id_cols = DiagCat, names_from = FINESS_Hosp, values_from = p_catchmentHosp, values_fn = ~scales::percent(.x, accuracy = 0.1)) %>% 
  write_csv2("output/dep_finess10_diagcat_pct.csv")

# Urg'ARA table

urg_catchment <- ara %>% 
  group_by(RegCode, DepCode, age_group, Year) %>% 
  summarise(n = sum(n, na.rm = T), n_HCL = sum(n_HCL, na.rm = T)) %>% 
  # left_join(dep_pop %>% group_by(DepCode, Year) %>% summarise(Pop = sum(Pop))) %>% 
  left_join(dep_pop_age %>% select(DepCode, Year, age_group, Pop)) %>% 
  mutate(p_HCL = n_HCL/n) %>% 
  mutate(catchment_HCL = p_HCL*Pop)

urg_catchment_poparea <- ara %>% # can replace the previous assignment
  group_by(RegCode, DepCode, age_group, Year) %>% 
  summarise(n = sum(n, na.rm = T), n_HCL = sum(n_HCL, na.rm = T)) %>% 
  # left_join(dep_pop %>% group_by(DepCode, Year) %>% summarise(Pop = sum(Pop))) %>% 
  left_join(dep_pop_age %>% select(DepCode, Year, age_group, Pop)) %>% 
  left_join(shp_dep %>% 
              select(DepCode, Area_km2) %>% 
              st_drop_geometry() %>% 
              ungroup) %>% 
  mutate(p_HCL = n_HCL/n) %>% 
  mutate(catchment_HCL = p_HCL*Pop, 
         catchmentArea = p_HCL*Area_km2)

urg_catchment %>% 
  write_csv2("output/urgARA_HCLcatchment.csv")


HCLcatchment_ageyear_Urg_ARA <- urg_catchment %>% 
  filter(RegCode == "84") %>% 
  filter(!is.na(age_group)) %>% 
  group_by(Year, age_group) %>% 
  summarise(catchment_Urg_ARA = sum(catchment_HCL, na.rm = T))


# save this for use in FIRESP and other work
save(HCLcatchment_ageyear_Urg_ARA, file = "output/HCLcatchment_ageyear_Urg_ARA.RData")


# urg_catchment %>% 
#   filter(RegCode == "84") %>% 
#   filter(!is.na(age_group)) %>% 
#   group_by(Year, age_group) %>% 
#   summarise(catchment_HCL = sum(catchment_HCL)) %>% 
#   mutate(Type = "Emergency") %>% 
#   rbind(urg_catchment %>% 
#           filter(RegCode == "84") %>% 
#           filter(!is.na(age_group)) %>% 
#           group_by(Year) %>% 
#           summarise(catchment_HCL = sum(catchment_HCL)) %>% 
#           mutate(Type = "Emergency", age_group = "All")) %>% 
#   rbind(pmsi_ageyear_HCLcatchment %>% 
#           filter(RegCode == "84", Year %in% 2019:2024) %>% 
#           group_by(Year) %>% 
#           summarise(catchment_HCL = sum(catchmentHosp, na.rm = T)) %>% 
#           mutate(Type = "Hospitalisation", age_group = "All")) %>% 
#   ungroup %>% 
#   mutate(Region = "ARA") %>% 
#   pivot_wider(id_cols = c("Region", "Type", "age_group"), names_from = "Year", values_from = "catchment_HCL") %>% 
#   write_csv2("output/urgARA_table.csv")


urgARA_long <- urg_catchment_poparea %>% 
  filter(RegCode == "84") %>% 
  filter(!is.na(age_group)) %>% 
  group_by(Year, age_group) %>% 
  summarise(catchment_HCL = sum(catchment_HCL), catchmentArea = sum(catchmentArea)) %>% 
  mutate(Type = "Emergency") %>% 
  rbind(urg_catchment_poparea %>% 
          filter(RegCode == "84") %>% 
          filter(!is.na(age_group)) %>% 
          group_by(Year) %>% 
          summarise(catchment_HCL = sum(catchment_HCL), 
                    catchmentArea = sum(catchmentArea)) %>% 
          mutate(Type = "Emergency", age_group = "All")) %>% 
  rbind(pmsi_ageyear_HCLcatchment_poparea %>% 
          filter(RegCode == "84", Year %in% 2019:2024) %>% 
          group_by(Year) %>% 
          summarise(catchment_HCL = sum(catchmentHosp, na.rm = T), 
                    catchmentArea = sum(catchmentArea, na.rm = T)) %>% 
          mutate(Type = "Hospitalisation", age_group = "All")) %>% 
  ungroup %>% 
  mutate(Region = "ARA")

urgARA_long %>% 
  select(-catchmentArea) %>%
  pivot_wider(id_cols = c("Region", "Type", "age_group"), names_from = "Year",
              values_from = "catchment_HCL") %>%
  write_csv2("output/urgARA_table.csv")

urgARA_long %>% 
  select(-catchment_HCL) %>% 
  pivot_wider(id_cols = c("Region", "Type", "age_group"), names_from = "Year", 
              values_from = "catchmentArea") %>% 
  write_csv2("output/urgARA_table_area.csv")






# distribution of catchment size by types of hospital

catchment_byFINESS = dep_year_finess_catchment_poparea %>% 
  filter(FINESS_Hosp != "Others") %>% 
  group_by(FINESS_Hosp, Year) %>% 
  summarise(across(c("n_Hosp", "catchmentHosp", "catchmentArea"), sum), .groups = "drop") %>% 
  mutate(FINESS = gsub("([0-9]+) - .*", "\\1", FINESS_Hosp))



catchment_byFINESS %>% 
  left_join(FINESScategories %>% select(-Year)) %>% 
  filter(!grepl("^97", FINESS_Hosp)) %>% #remove the hospitals from DOM which draw people from Metropolitan France
  mutate(CategoryClean = replace_na(CategoryClean, replace = "Other")) %>% 
  group_by(CategoryClean) %>% 
  mutate(CategoryHeader = paste0("<b>", CategoryClean, "</b><br>", "(n=", n_distinct(FINESS), ")")) %>% 
  arrange(CategoryClean == "Other", CategoryClean) %>% 
  ungroup %>% 
  mutate(CategoryHeader = factor(CategoryHeader, levels = unique(CategoryHeader))) %>% 
  ungroup %>% 
  transmute(CategoryHeader, Hospitalisations = n_Hosp, `Catchment population` = catchmentHosp, `Catchment area` = catchmentArea) %>% 
  pivot_longer(-CategoryHeader, names_to = "stat") %>% 
  group_by(CategoryHeader, stat) %>% 
  mutate(med_value = median(value)) %>% 
  mutate(Measure = ifelse(grepl("Catchment area", stat), "Area (km2)", "Population")) %>% 
  # mutate(CategoryHeader = paste0(CategoryClean, "\n(n=", n_distinct(FINESS), ")")) %>% 
  ggplot(aes(y = value)) + 
  geom_histogram(aes(x=..density.., fill = stat), alpha = 0.6) +
  geom_hline(aes(yintercept = med_value, colour = stat)) + 
  # geom_histogram(stat = "density") + 
  facet_grid(Measure~CategoryHeader, switch = "y", scales = "free_y") + 
  theme_bw() + 
  theme(strip.placement = "outside",
        strip.text.x = ggtext::element_markdown(size = 10, hjust = 0), 
        strip.background.y = element_blank()) + 
  scale_y_continuous(trans = "log10", n.breaks = 6, limits = c(0.1, NA), labels = scales::label_comma(big.mark = " ")) + #limits = c(0.1, 1e6), breaks = 10^(-1:6), labels = scales::breaks_log(n = 7)) + 
  labs(x = "Density", y = "", fill = "", colour = "")

ggsave("output/category_histograms.png", width = 30, height = 15, units = "cm", dpi = 600)


