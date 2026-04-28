dep_year_diag_catchment_poparea

dep_year_finess_catchment_poparea %>% 
  mutate(FINESS_Hosp = case_when(FINESS_Hosp == "310781406 - CHR TOULOUSE" ~ "310781406 - CHU TOULOUSE", 
                                 T ~ FINESS_Hosp)) %>% 
  group_by(FINESS_Hosp, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp), catchmentArea = sum(catchmentArea)) %>% 
  group_by(FINESS_Hosp) %>% 
  mutate(mean_nHosp = mean(n_Hosp, na.rm = T)) %>% 
  mutate(scale = case_when(mean_nHosp > 10000 ~ "Big", 
                           mean_nHosp > 1000 ~ "Medium", 
                           T ~ "Small")) %>% 
  ggplot(aes(x = Year, y = catchmentHosp, group = FINESS_Hosp)) + 
  geom_line(alpha = 0.1) + 
  facet_grid(scale~., scale = "free_y") + 
  scale_y_log10() + 
  theme_bw()


dep_year_finess_catchment_poparea %>% 
  mutate(FINESS_Hosp = case_when(FINESS_Hosp == "310781406 - CHR TOULOUSE" ~ "310781406 - CHU TOULOUSE", 
                                 T ~ FINESS_Hosp)) %>% 
  group_by(FINESS_Hosp, Year) %>% 
  summarise(n_Hosp = sum(n_Hosp), catchmentHosp = sum(catchmentHosp), catchmentArea = sum(catchmentArea)) %>% 
  mutate(mean_nHosp = mean(n_Hosp, na.rm = T)) %>% 
  mutate(scale = case_when(mean_nHosp > 10000 ~ "Big", 
                           mean_nHosp > 1000 ~ "Medium", 
                           T ~ "Small")) %>% 
  arrange(FINESS_Hosp, Year) %>% 
  group_by(FINESS_Hosp) %>% 
  mutate(pIncrease = n_Hosp/lag(n_Hosp, n = 1)) %>% 
  # filter(Year != 2016)
  ggplot(aes(x = pIncrease)) + 
  geom_histogram(binwidth = 0.1) + 
  facet_grid(Year~scale, scale = "free_y") + 
  # scale_y_log10() + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 3))



dep_year_diag_catchment_poparea %>% 
  group_by(FINESS_Hosp, DiagCat, Year) %>% 
  summarise(catchmentHosp = sum(catchmentHosp), n_Hosp = sum(n_Hosp)) %>% 
  filter(grepl("04|19|22|27", DiagCat)) %>% 
  ggplot(aes(x = Year, y = n_Hosp)) + 
  geom_line(aes(group = FINESS_Hosp), alpha = 0.1)  + 
  facet_wrap(.~DiagCat) + 
  scale_y_log10() + 
  theme_bw()

dep_year_diag_catchment_poparea %>% 
  group_by(FINESS_Hosp, DiagCat, Year) %>% 
  summarise(catchmentHosp = sum(catchmentHosp), n_Hosp = sum(n_Hosp)) %>% 
  filter(grepl("04|19|22|27", DiagCat)) %>% 
  arrange(DiagCat, FINESS_Hosp, Year) %>% 
  group_by(DiagCat, FINESS_Hosp) %>% 
  mutate(pIncrease = n_Hosp/lag(n_Hosp, n = 1)) %>% 
  # filter(Year != 2016)
  ggplot(aes(x = pIncrease)) + 
  geom_histogram(binwidth = 0.1) + 
  facet_grid(Year~DiagCat, scale = "free_y") + 
  # scale_y_log10() + 
  theme_bw() + 
  coord_cartesian(xlim = c(0, 3))
