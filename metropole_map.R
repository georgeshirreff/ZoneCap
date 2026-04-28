library(tidyverse)
library(sf)

metropole <- read_csv2("Metropole/Communes_metropoleLyon.csv")

metropole_PMSIcodes = c(metropole$Postcode, 
  paste0("6900", 1:9)
  # , "69310"
  , "69C02"
)


metropole
metropole %>% print(n = 100)

pmsi_ageyear_HCLcatchment


metropole %>% 
  mutate(z = Postcode %in% unique(ara$Postcode)) %>% 
  print(n = 100)

metropole_data_urg <- shp_pmsi %>% 
  right_join(ara %>% 
               filter((Postcode %in% c(metropole$Postcode, paste0("6900", 1:9)))) %>% 
               filter(Year == 2024) %>% 
               transmute(PMSIcode = Postcode, age_group, pHosp = part_HCL)) %>% 
  left_join(metropole %>% 
              group_by(Postcode) %>% 
              arrange(-Population) %>% 
              slice(1) %>% 
              ungroup %>% 
              transmute(PMSIcode = as.character(Postcode), PMSIcode_Name = PostcodeName), relationship = 'many-to-one') %>% 
  mutate(PMSIcode_Name = case_when(is.na(PMSIcode_Name) ~ PMSIcode, 
                                   # Encoding(PMSIcode_Name) == "UTF-8" ~ iconv(PMSIcode_Name, from = "UTF-8", to = "UTF-8"), 
                                   T ~ PMSIcode_Name)) %>% 
  filter(!is.na(age_group))



metropole_data_hospitalisation <- shp_pmsi %>%
  right_join(pmsi_ageyear_HCLcatchment %>%
  filter((PMSIcode %in% c(metropole$Postcode, paste0("6900", 1:9)))) %>%
  filter(Year == 2024))


metropole_data_urg %>% 
  ggplot() + 
  geom_sf(data = shp_dep %>% 
            filter(DepCode == "69") %>% 
            st_transform(st_crs(shp_pmsi)), colour = "black", fill = "lightgrey") + 
  geom_sf(aes(fill = pHosp), colour = NA) +
  geom_sf(data = shp_dep %>% 
            filter(DepCode == "69") %>% 
            st_transform(st_crs(shp_pmsi)), colour = "black", fill = NA) + 
  geom_sf_label(aes(label = PMSIcode_Name), size =0.7, label.size  = NA, alpha = 0.5) + #colour = "black", fill = alpha("white", 0.5)) + 
  scale_fill_continuous(na.value = "lightgrey") + 
  facet_wrap(.~age_group) + 
  theme(panel.background = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  labs(fill = "Preference for HCL")

ggsave("output/metropole_age_2024.png", units = "cm", width = 60, height = 40, limitsize = F)



# first attempt at maps showing overlapping catchment populations

pmsi_finess_catchment_metropole <- pmsi_year_finess_catchment %>%
  rename(FINESS_Hosp = Finess_Hosp) %>% 
  filter(PMSIcode %in% metropole_PMSIcodes) %>% 
  group_by(FINESS_Hosp, PMSIcode) %>% 
  summarise(catchmentHosp = mean(catchmentHosp, na.rm = T), 
            pHosp = mean(pHosp, na.rm = T)) %>% 
  ungroup

pmsi_finessgeo_catchment_metropole <- pmsi_year_finessgeo_catchment %>% 
  filter(PMSIcode %in% metropole_PMSIcodes) %>% 
  clean_Geo_Hosp %>% 
  group_by(FINESS_Hosp, Geo_Hosp, PMSIcode) %>% 
  summarise(catchmentHosp = mean(catchmentHosp, na.rm = T), 
            pHosp = mean(pHosp, na.rm = T)) %>% 
  ungroup

shp_pmsi %>% 
  right_join(pmsi_finess_catchment_metropole %>% 
               filter(grepl("^69", FINESS_Hosp)) %>% 
               mutate(FINESS_Hosp = gsub(" - ", "\n", FINESS_Hosp)) %>% 
               group_by(FINESS_Hosp) %>% 
               filter(any(pHosp > 0.1))) %>% 
  ggplot() + 
  geom_sf(aes(fill = pHosp)) + 
  facet_wrap(.~FINESS_Hosp)




pmsi_finessgeo_catchment_metropoleInclude <- pmsi_finessgeo_catchment_metropole %>% 
  filter(grepl("^69", FINESS_Hosp)) %>% 
  left_join(tibble(Geo_Hosp = hcl_locations$Geo_Hosp, HCL = 1)) %>% 
  mutate(HCL = replace_na(HCL, 0)) %>% 
  group_by(Geo_Hosp) %>% 
  filter(any(pHosp > 0.2) | any(catchmentHosp > 5000) | HCL == 1)

pmsi_finessgeo_catchment_metropoleInclude %>% 
  filter(HCL == 0) %>% 
  pull(Geo_Hosp) %>% unique %>% cat(sep = "\n")



  
shp_pmsi %>% 
  right_join(pmsi_finessgeo_catchment_metropoleInclude) %>% 
  mutate(Geo_Hosp = gsub(" - ", "\n", Geo_Hosp)) %>% 
  ggplot() + 
  geom_sf(data = shp_pmsi %>% filter(PMSIcode %in% metropole_PMSIcodes), fill = "lightgrey") + 
  geom_sf(aes(fill = pHosp)) + 
  geom_sf(data = rbind(hcl_locations %>% 
                         filter(!grepl("^830100558", Geo_Hosp)), 
                       nonHCL_locations %>% 
                         filter(Geo_Hosp %in% pmsi_finessgeo_catchment_metropoleInclude$Geo_Hosp)) %>%  
                      mutate(Geo_Hosp = gsub(" - ", "\n", Geo_Hosp)) 
                    # filter(grepl("440000289", FINESS_Hosp))
                    , colour = "red") + 
  scale_fill_continuous(na.value = "lightgrey") + 
  facet_wrap(.~Geo_Hosp, nrow = 3) +
  theme(panel.background = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  labs(fill = "Hospital preference")

ggsave(filename = "output/metropole_allBigHospitals.png", width = 40, height = 25, units = "cm")



plot_69 <- rbind(nonHCL_locations, 
                 hcl_locations) %>% 
  right_join(pmsi_year_finessgeo_catchment %>% 
               filter(PMSIcode %in% codes_69) %>% 
               filter(Year == 2024))
shp_69 <- shp_pmsi %>% 
  filter(PMSIcode %in% codes_69) %>% 
  rename(PMSIcode_x = PMSIcode) %>% 
  cross_join(shp_pmsi %>% 
               filter(PMSIcode %in% codes_69) %>% 
               st_drop_geometry() %>%
               select(PMSIcode)) %>%
  mutate(highlight = PMSIcode_x == PMSIcode)

plot_69 %>% 
  ggplot() + 
  geom_sf(data = shp_69, aes(fill = highlight)) + 
  # geom_sf(data = shp_pmsi %>% filter(PMSIcode == codes_69), aes(fill = "orange")) +
  geom_sf_label(aes(label = gsub(".* - ", "", Geo_Hosp)), size = 0.8, alpha = 0.5, nudge_x = 0.01) + 
  geom_sf(aes(colour = pHosp)) +
  scale_colour_continuous(trans = "log10") + 
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = F) + 
  facet_grid(.~PMSIcode) + 
  coord_sf(ylim = c(45.6, 45.85)) 
  

ggsave(filename = "output/hospitals_69007.png", width = 40, height = 10, units = "cm")


