library(tidyverse) 

# data from HCL (not available without permission from the Department of Health Data at the Hospices Civils de Lyon)

dep_ageyear_firesp_hosp_p <- data_sejours %>% 
  filter(type_sej == "HOSPITALISATION") %>%
  mutate(DepCode = paste0(ifelse(dept_res < 10, "0", ""), dept_res)) %>% 
  mutate(Year = year_admitted) %>% 
  group_by(DepCode, age_group, Year) %>% 
  summarise(n_FIRESP = n_distinct(IPP_sejour)) %>% 
  group_by(Year, age_group) %>% 
  mutate(p_FIRESP = n_FIRESP/sum(n_FIRESP, na.rm = T)) %>% 
  select(-n_FIRESP)

dep_ageyear_firesp_urg_p <- data_sejours %>% 
  filter(catego_UM_agg == "Urg") %>%
  mutate(DepCode = paste0(ifelse(dept_res < 10, "0", ""), dept_res)) %>% 
  mutate(Year = year_admitted) %>% 
  group_by(DepCode, age_group, Year) %>% 
  summarise(n_FIRESP = n_distinct(IPP_sejour)) %>% 
  group_by(Year, age_group) %>% 
  mutate(p_FIRESP = n_FIRESP/sum(n_FIRESP, na.rm = T)) %>% 
  select(-n_FIRESP)

write_csv2(dep_ageyear_firesp_hosp_p, "~/ZoneCap/data/HCL/dep_ageyear_firesp_hosp_p.csv")
write_csv2(dep_ageyear_firesp_urg_p, "~/ZoneCap/data/HCL/dep_ageyear_firesp_urg_p.csv")


