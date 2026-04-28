pmsi_year_finessgeo_catchment_poparea %>%
  filter(grepl("690007539", Geo_Hosp)) %>% 
  mutate(PopDensity = Pop/Area_km2) %>% 
  # arrange(-PopDensity) %>% 
  mutate(PMSIcode_Name = ifelse(PMSIcode_Name == "Non renseigné", NA, PMSIcode_Name)) %>% 
  group_by(FINESS_Hosp, Geo_Hosp, RegCode, DepCode, PMSIcode) %>% 
  summarise(catchmentHosp = mean(catchmentHosp, na.rm = T), PMSIpop = mean(Pop), PMSIpopdensity = mean(PopDensity), PMSIcode_Name = last(unique(na.omit(PMSIcode_Name)))) %>% 
  ungroup %>% 
  arrange(-catchmentHosp) %>% 
  write_csv2(file = "output/HFME_PMSIcode_meanCatchment.csv")


pmsi_ageyear_HCLcatchment_poparea %>% 
  mutate(PopDensity = Pop/Area_km2) %>% 
  # arrange(-PopDensity) %>% 
  mutate(PMSIcode_Name = ifelse(PMSIcode_Name == "Non renseigné", NA, PMSIcode_Name)) %>% 
  group_by(FINESS_Hosp, RegCode, DepCode, PMSIcode, age_group) %>% 
  summarise(catchmentHosp = mean(catchmentHosp, na.rm = T), PMSIpop = mean(Pop), PMSIpopdensity = mean(PopDensity), PMSIcode_Name = last(unique(na.omit(PMSIcode_Name)))) %>% 
  ungroup %>% 
  arrange(age_group, -catchmentHosp) %>%
  write_csv2(file = "output/HCL_PMSIcode_age_meanCatchment.csv")



shp_pmsi
shp_postcode_raw

shp_pmsi$nPostcodes %>% table
shp_pmsi %>% 
  filter(nPostcodes > 1) %>% 
  View




dep_year_diag_catchment_poparea %>% 
  filter(FINESS_Hosp != "Others") %>% 
  group_by(DiagCat, Year) %>% 
  mutate(n_Hosp_diag = sum(n_Hosp, na.rm = T)) %>% 
  group_by(DiagCat) %>% 
  mutate(n_Hosp_diag = mean(n_Hosp_diag, na.rm = T)) %>% 
  arrange(-n_Hosp_diag) %>% 
  ungroup %>% 
  mutate(DiagCat = factor(DiagCat, levels = unique(DiagCat))) %>% 
  group_by(FINESS_Hosp, DiagCat, DepCode) %>% 
  summarise(n_Hosp = mean(n_Hosp, na.rm = T), 
            catchmentHosp = mean(catchmentHosp, na.rm = T), 
            catchmentArea = mean(catchmentArea, na.rm = T)) %>% 
  filter(grepl("HOSPICES CIVILS DE LYON|BOURGES|AP-HP", FINESS_Hosp)) %>% 
  ggplot(aes(x = catchmentHosp, y = catchmentArea, colour = FINESS_Hosp)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = T) +
  # ggpubr::ggscatter(x = 'catchmentHosp',
  #                   y = 'catchmentArea',
  #                   facet.by = c("Finess_Hosp", "DiagCat"), 
  #                   # facet.by = "Finess_Hosp",
  #                   color = "Finess_Hosp",
  #                   cor.method = "spearman",
  #                   cor.coef = TRUE,
  #                   conf.int = TRUE,
  #                   size = 0.5,
  #                   add = "reg.line",
  #                   add.params = list(color = 'red'),
  # ) +
  facet_wrap(.~DiagCat) + 
  scale_x_log10() + scale_y_log10() +
  theme_bw() + 
  labs(x = "Catchment population (persons per year)", y = "Catchment area (km2)")

ggsave("output/area_pop_correlation_diagcat_HCL.AP-HP.Bourges.png", width = 30, height = 20, units = "cm", dpi = 600)


dep_year_finess_catchment_poparea %>% 
  filter(FINESS_Hosp != "Others") %>% 
  mutate(FINESS_Hosp = case_when(FINESS_Hosp == "310781406 - CHR TOULOUSE" ~ "310781406 - CHU TOULOUSE", 
                                 T ~ FINESS_Hosp)) %>% 
  group_by(FINESS_Hosp, DepCode, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), 
            catchmentHosp = sum(catchmentHosp), 
            catchmentArea = mean(catchmentArea)) %>% 
  group_by(FINESS_Hosp, DepCode) %>% 
  summarise(n_Hosp = mean(n_Hosp), 
            catchmentHosp = mean(catchmentHosp), 
            catchmentArea = mean(catchmentArea)) %>% 
  group_by(FINESS_Hosp) %>% 
  mutate(total_n_Hosp = sum(n_Hosp)) %>%  
  arrange(-total_n_Hosp) %>% 
  ungroup %>% 
  mutate(FINESS_Hosp = factor(FINESS_Hosp, levels = unique(FINESS_Hosp))) %>% 
  filter(as.numeric(FINESS_Hosp) <= 10) %>% 
  ggplot(aes(x = catchmentHosp, y = catchmentArea)) + 
  geom_point() + 
  facet_wrap(.~FINESS_Hosp, nrow = 2) + 
  scale_x_log10() + scale_y_log10() + 
  theme_bw() + 
  labs(x = "Catchment population (persons per year)", y = "Catchment area (km2)")


ggsave("output/area_pop_correlation_FINESS10.png", width = 30, height = 15, units = "cm", dpi = 600)








choice_threshold = 0.01
dep_year_diag_catchment_choices <- dep_year_diag_catchment_poparea %>% 
  mutate(DepCode_Hosp = substr(Finess_Hosp, 1, 2)) %>% 
  # filter(grepl("^01", DiagCat), Year == 2024) %>% 
  filter(Finess_Hosp != "Others", DiagCat != "Others", DepCode != "Others") %>% 
  # group_by(Year, DiagCat) %>% 
  # mutate(DiagCattotal_catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% #total national catchment for this DiagCat
  group_by(Year, DiagCat, DepCode) %>% 
  mutate(pDiagcatchmentHosp = catchmentHosp/sum(catchmentHosp)) %>% #within each department, the propo
  summarise(LocalChoices = sum(pDiagcatchmentHosp > choice_threshold & DepCode_Hosp == DepCode)) %>% 
  group_by(DiagCat, DepCode) %>% 
  summarise(LocalChoices = mean(LocalChoices))

shp_dep %>% 
  left_join(dep_year_diag_catchment_choices %>% 
              filter(grepl("01|02|03|04", DiagCat))
            ) %>% 
  ggplot() + 
  geom_sf(aes(fill = LocalChoices)) + 
  facet_wrap(.~DiagCat)
  

dep_year_diag_catchment_poparea %>% 
  # filter(grepl("^01", DiagCat), Year == 2024) %>%
  mutate(DepCode_Hosp = substr(Finess_Hosp, 1, 2)) %>% 
  filter(Finess_Hosp != "Others", DiagCat != "Others", DepCode != "Others") %>% 
  group_by(DiagCat, Year) %>% 
  summarise(catchmentHosp_inDep = sum(catchmentHosp[DepCode == DepCode_Hosp], na.rm = T), 
            catchmentHosp = sum(catchmentHosp, na.rm = T)
  ) %>% 
  group_by(DiagCat) %>% 
  summarise(catchmentHosp_inDep = mean(catchmentHosp_inDep), 
            catchmentHosp = mean(catchmentHosp)) %>% 
  mutate(p = catchmentHosp_inDep/catchmentHosp) %>% 
  View
dep_year_finess_catchment_poparea


