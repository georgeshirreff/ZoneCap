library(tidyverse) 
# NB this needs to be run in the FIRESP project so as to access that data

# data_sejours %>% 
#   group_by(IPP_sejour) %>% 
#   mutate(n = n_distinct(dept_res)) %>% 
#   filter(n > 1) %>% 
#   select(IPP_sejour, dept_res)

dep_ageyear_catchment <- read_csv2("~/ZoneCap/output/dep_ageyear_HCLcatchment.csv")
dep_year_diag_catchment <- readxl::read_xlsx("~/ZoneCap/output/dep_diag_HCLcatchment.xlsx")
urg_catchment <- read_csv2("~/ZoneCap/output/urgARA_HCLcatchment.csv")


# dep_ageyear_firesp <- data_sejours %>% 
#   # filter(type_sej == "HOSPITALISATION") %>% 
#   mutate(DepCode = paste0(ifelse(dept_res < 10, "0", ""), dept_res)) %>% 
#   mutate(Year = year_admitted) %>% 
#   group_by(DepCode, type_sej, age_group, Year) %>% 
#   summarise(n_FIRESP = n_distinct(IPP_sejour))
# 
# dep_year_firesp <- data_sejours %>% 
#   # filter(type_sej == "HOSPITALISATION") %>% 
#   mutate(DepCode = paste0(ifelse(dept_res < 10, "0", ""), dept_res)) %>% 
#   mutate(Year = year_admitted) %>% 
#   group_by(DepCode, type_sej, Year) %>% 
#   summarise(n_FIRESP = n_distinct(IPP_sejour))


dep_ageyear_firesp_hosp <- data_sejours %>% 
  filter(type_sej == "HOSPITALISATION") %>%
  mutate(DepCode = paste0(ifelse(dept_res < 10, "0", ""), dept_res)) %>% 
  mutate(Year = year_admitted) %>% 
  group_by(DepCode, age_group, Year) %>% 
  summarise(n_FIRESP = n_distinct(IPP_sejour)) %>% 
  ungroup


dep_ageyear_firesp_urg <- data_sejours %>% 
  filter(catego_UM_agg == "Urg") %>%
  mutate(DepCode = paste0(ifelse(dept_res < 10, "0", ""), dept_res)) %>% 
  mutate(Year = year_admitted) %>% 
  group_by(DepCode, age_group, Year) %>% 
  summarise(n_FIRESP = n_distinct(IPP_sejour)) %>% 
  ungroup



comparison_hosp <- dep_ageyear_catchment %>% 
  transmute(DepCode, RegCode = as.character(RegCode), Year, age_group, Pop, n_Hosp, catchmentHosp) %>% 
  left_join(dep_ageyear_firesp_hosp) %>% 
  # group_by(Year, DepCode) %>% 
  # summarise(across(c("n_FIRESP", "catchment_HCL", "n_HCL", "Pop"), sum)) %>% 
  group_by(Year, age_group) %>% 
  mutate(p_FIRESP = n_FIRESP/sum(n_FIRESP, na.rm = T), 
         p_catchmentHCL = catchmentHosp/sum(catchmentHosp, na.rm = T), 
         p_Hosp = n_Hosp/sum(n_Hosp, na.rm = T), 
         p_Pop = Pop/sum(Pop, na.rm = T)
  ) %>% mutate(type_sej = "Hospitalisation")

comparison_urg <- urg_catchment %>% 
  filter(RegCode %in% c("84")) %>% 
  transmute(DepCode, RegCode = as.character(RegCode), Year, age_group, Pop, n_HCL, catchment_HCL) %>% 
  left_join(dep_ageyear_firesp_urg) %>% 
  filter(Year %in% 2019:2024) %>% 
  # group_by(Year, DepCode) %>% 
  # summarise(across(c("n_FIRESP", "catchment_HCL", "n_HCL", "Pop"), sum)) %>% 
  group_by(Year, age_group) %>% 
  mutate(p_FIRESP = n_FIRESP/sum(n_FIRESP, na.rm = T), 
         p_catchmentHCL = catchment_HCL/sum(catchment_HCL, na.rm = T), 
         p_HCL = n_HCL/sum(n_HCL, na.rm = T), 
         p_Pop = Pop/sum(Pop, na.rm = T)
  ) %>% mutate(type_sej = "Emergency")


rbind(comparison_hosp, comparison_urg) %>% 
  mutate(type_sej = factor(type_sej, levels = c("Hospitalisation", "Emergency"))) %>% 
  # mutate(z = (p_catchmentHCL > 0.015) & (p_FIRESP < 0.004)) %>% 
  ggpubr::ggscatter(x = 'p_catchmentHCL',
                    y = 'p_FIRESP',
                    # facet.by = "type_sej",
                    color = 'grey50',
                    cor.method = "spearman",
                    cor.coef = TRUE,
                    conf.int = TRUE,
                    size = 0.5,
                    add = "reg.line",
                    add.params = list(color = 'red'),
  ) + 
  facet_wrap(.~type_sej) +
  # scale_y_log10() + scale_x_log10() + 
  labs(x = "Proportion estimated in\ncatchment for HCL", y = "Proportion observed in\nrespiratory admissions") + 
  geom_abline(slope = 1, intercept = 0, colour = "black", linetype = "dotted") + 
  scale_x_continuous(#limits = c(1e-4, 1), 
    trans = "log10", label = function(x) as.character(x)) + 
  scale_y_continuous(#limits = c(1e-4, 1), 
    trans = "log10", label = function(x) as.character(x))

ggsave(filename = "~/ZoneCap/output/atih_firesp_comparison_HospUrg.png", width = 20, height = 12, units = "cm", dpi = 300)

png(filename = "~/ZoneCap/output/atih_firesp_comparison_HospUrg_legend.png", width = 10, height = 10, units = "cm", res = 300)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", title="", 
       legend = names, 
       lty=c(NA, 1, 1, 2), 
       col = c('grey50', 'red', 'salmon', 'black'), 
       pch = c(16, NA, NA, NA), 
       lwd=c(NA, 2, 8, 2), 
       cex=1.25,
       bty='n')
dev.off()

dep_year_diag_catchment %>% 
  filter(grepl("04", DiagCat)) %>%
  # filter(grepl("18", DiagCat)) %>% 
  transmute(DepCode, RegCode = as.character(RegCode), Year, DiagCat, Pop, n_Hosp, catchmentHosp) %>% 
  left_join(dep_year_firesp %>% 
              filter(type_sej == "HOSPITALISATION")) %>% 
  filter(Year %in% 2017:2024) %>% 
  # group_by(Year, DepCode) %>% 
  # summarise(across(c("n_FIRESP", "catchment_HCL", "n_HCL", "Pop"), sum)) %>% 
  group_by(DiagCat, Year) %>% 
  mutate(p_FIRESP = n_FIRESP/sum(n_FIRESP, na.rm = T), 
         p_catchmentHCL = catchmentHosp/sum(catchmentHosp, na.rm = T), 
         p_Hosp = n_Hosp/sum(n_Hosp, na.rm = T), 
         p_Pop = Pop/sum(Pop, na.rm = T)
  ) %>% 
  ggpubr::ggscatter(x = 'p_catchmentHCL',
                    y = 'p_FIRESP',
                    color = 'grey50',
                    cor.method = "spearman",
                    cor.coef = TRUE,
                    conf.int = TRUE,
                    size = 0.5,
                    add = "reg.line",
                    add.params = list(color = 'red'),
  ) + scale_y_log10() + scale_x_log10() + 
  labs(x = "Proportion estimated in\ncatchment for HCL\n(respiratory only)", y = "Proportion observed in\nrespiratory admissions") + 
  geom_abline(slope = 1, intercept = 0, colour = "black", linetype = "dashed")

  ggsave(filename = "~/ZoneCap/output/atih_firesp_comparison_respiratory only.png", width = 15, height = 10, units = "cm")
  
  


### investigate why the aberrant points on the validation plot
  
data_sejours %>% 
  group_by(dept_res, year_admitted, age_group) %>% 
  summarise(n = n_distinct(IPP_sejour)) %>% 
  ggplot(aes(x = year_admitted, y = n, colour = age_group, group = age_group)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(.~dept_res) + 
  scale_y_log10()


data_sejours$dept_res

data_sejours %>% 
  group_by(dept_res) %>% 
  summarise(n = n_distinct(IPP_sejour)) %>% 
  print(n = 1000)


comparison_hosp %>% 
  mutate(z = (p_catchmentHCL > 0.015) & (p_FIRESP < 0.004)) %>%
  ggpubr::ggscatter(x = 'p_catchmentHCL',
                    y = 'p_FIRESP',
                    # facet.by = "type_sej",
                    # color = 'grey50',
                    color = 'z', 
                    cor.method = "spearman",
                    cor.coef = TRUE,
                    conf.int = TRUE,
                    size = 0.5,
                    add = "reg.line",
                    add.params = list(color = 'red'),
  ) + 
  geom_abline(slope = 1, intercept = 0, colour = "black", linetype = "dashed") + 
  scale_x_continuous(#limits = c(1e-4, 1), 
    trans = "log10", label = function(x) as.character(x)) + 
  scale_y_continuous(#limits = c(1e-4, 1), 
    trans = "log10", label = function(x) as.character(x))


comparison_hosp %>% 
  mutate(z = (p_catchmentHCL > 0.015) & (p_FIRESP < 0.004)) %>% 
  filter(z)


comparison_hosp_stats <- dep_ageyear_catchment %>% 
  transmute(DepCode, RegCode = as.character(RegCode), Year, age_group, Pop, n_Hosp, catchmentHosp) %>% 
  left_join(dep_ageyear_firesp %>% 
              filter(type_sej == "HOSPITALISATION")) %>% 
  # group_by(Year, DepCode) %>% 
  # summarise(across(c("n_FIRESP", "catchment_HCL", "n_HCL", "Pop"), sum)) %>% 
  group_by(Year, age_group) %>% 
  mutate(p_FIRESP = n_FIRESP/sum(n_FIRESP, na.rm = T), 
         p_catchmentHCL = catchmentHosp/sum(catchmentHosp, na.rm = T), 
         sum_n_FIRESP = sum(n_FIRESP, na.rm = T), 
         sum_catchmentHosp = sum(catchmentHosp, na.rm = T), 
         p_Hosp = n_Hosp/sum(n_Hosp, na.rm = T), 
         p_Pop = Pop/sum(Pop, na.rm = T)
  ) %>% mutate(type_sej = "Hospitalisation") %>% 
  mutate(z = (p_catchmentHCL > 0.015) & (p_FIRESP < 0.004)) %>%
  filter(!is.na(Pop), !is.na(p_FIRESP), !is.na(p_catchmentHCL)) %>% 
  rowwise() %>% 
  mutate(p_value = binom.test(x = n_FIRESP, n = sum_n_FIRESP, p = p_catchmentHCL)$p.value)

comparison_hosp_stats %>% 
  # mutate(z = (p_catchmentHCL > 0.015) & (p_FIRESP < 0.004)) %>%
  mutate(z = p_value < 0.05) %>% 
  ggpubr::ggscatter(x = 'p_catchmentHCL',
                    y = 'p_FIRESP',
                    # facet.by = "type_sej",
                    # color = 'grey50',
                    # color = 'p_value', 
                    color = 'z',
                    cor.method = "spearman",
                    cor.coef = TRUE,
                    conf.int = TRUE,
                    size = 0.5,
                    add = "reg.line",
                    add.params = list(color = 'red'),
  ) + 
  geom_abline(slope = 1, intercept = 0, colour = "black", linetype = "dashed") + 
  scale_x_continuous(#limits = c(1e-4, 1), 
    trans = "log10", label = function(x) as.character(x)) + 
  scale_y_continuous(#limits = c(1e-4, 1), 
    trans = "log10", label = function(x) as.character(x))

comparison_hosp_stats %>% 
  mutate(exp_FIRESP = sum_n_FIRESP*p_catchmentHCL) %>% 
  filter(z) %>% 
  View


dep_ageyear_firesp %>% 
  filter(type_sej == "HOSPITALISATION") %>% 
  # filter(DepCode == 74, age_group == "65-79")
  filter(DepCode == 26, age_group == "80+")

dep_ageyear_firesp %>% 
  filter(type_sej == "HOSPITALISATION") %>% 
  # filter(DepCode == 74, age_group == "65-79")
  # filter(DepCode == 26, age_group == "80+")
  filter(DepCode == 83, age_group %in% c("65-79", "80+")) 




###########################################################
# compare incidence between FIRESP and other sources

#Cohen et al (Marta paper) doi: 10.1186/s12879-025-12039-2

load(file = "~/ZoneCap/output/HCLcatchment_age0year.RData", verbose = T)

Cohen <- tibble(Year = 2016:2023, Inc1000 = c(40.7, 47.6, 45.0, 47.0, 21.4, 52.7, 57.2, 43.4))

data_sejours %>% 
  rename(Year = year_admitted) %>% 
  mutate(month_num = month(date_ent_sejour)) %>% 
  filter(
    (DP %in% c("J210_", "J205_", "J121_")) |
      (DR %in% c("J210_", "J205_", "J121_")) |
      ((DP %in% c("B974_", "J208_", "J209_", "J218_", "J219_", "J45_")) & (month_num %in% c(10:12, 1:3))) |
      ((DR %in% c("B974_", "J208_", "J209_", "J218_", "J219_", "J45_")) & (month_num %in% c(10:12, 1:3)))) %>%
  filter(age == 0) %>% 
  filter(type_sej == "HOSPITALISATION") %>% 
  group_by(Year) %>% 
  summarise(n = n_distinct(IPP_alea), .groups = "drop") %>% 
  left_join(HCLcatchment_age0year %>% 
              transmute(Year, catchmentHosp = catchmentHosp)) %>% 
  mutate(Inc1e3 = n/catchmentHosp*1e3) %>% 
  transmute(Year, Inc1000 = Inc1e3, Dataset = "HCL cohort") %>% 
  rbind(Cohen %>% mutate(Dataset = "National")) %>% 
  filter(Year %in% 2017:2023) %>% 
  ggplot(aes(x = as.factor(Year))) + 
  geom_bar(aes(y = Inc1000, fill = Dataset), stat = "identity", position = "dodge") + 
  theme_bw() + 
  labs(x = "Year", y = "Incidence per 1000 population")

ggsave("~/ZoneCap/output/FIRESP_vs_Cohen.png", width = 15, height = 10, units = "cm", dpi = 600)



# https://link.springer.com/article/10.1186/s12879-025-12039-2
# https://www.medrxiv.org/content/10.64898/2026.01.13.26344008v1.full
# DOI:10.1093/ofid/ofaf528

load("~/ZoneCap/output/HCLcatchment_ageyear.RData", verbose = T)
load("~/ZoneCap/output/HCLcatchment_ageyear_Urg_ARA.RData", verbose = T)



data_PCR %>% 
  mutate(Year = year(date_preleve)) %>% 
  filter(Year %in% c(2017:2024)) %>% 
  filter(Virus == "RSV", resultatbis == "POSITIVE") %>% 
  left_join(data_sejours %>% 
              group_by(IPP_alea) %>% 
              summarise(type_sej = max(type_sej))
  ) %>% 
  group_by(type_sej, Year, age_group) %>% 
  summarise(n = n_distinct(IPP_alea), .groups = "drop") %>% 
  filter(type_sej == "HOSPITALISATION") %>% 
  left_join(HCLcatchment_ageyear) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ggplot(aes(x = Year, y = Inc1e5, fill = age_group)) + geom_bar(stat = "identity") + 
  facet_wrap(.~age_group, scales = "free_y")

data_sejours %>% 
  mutate(Year = year_admitted) %>% 
  filter(((DP %in% c("J210_", "J205_", "J121_", "B974_")) 
          | (DR %in% c("J210_", "J205_", "J121_", "B974_"))
  ) & type_sej == "HOSPITALISATION") %>% 
  group_by(Year, age_group) %>% 
  summarise(n = n_distinct(IPP_alea), .groups = "drop") %>% 
  left_join(HCLcatchment_ageyear) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  ggplot(aes(x = Year, y = Inc1e5, fill = age_group)) + geom_bar(stat = "identity") + 
  facet_wrap(.~age_group, scales = "free_y")

#actually do it by season to match data
Marot <- tibble(season_admitted = c("17/18", "18/19", "19/20", "21/22"), 
                Marotinc1e5 = c(27.5, 45.4, 34.9, 21.3))

data_sejours %>% 
  mutate(season_admitted) %>% 
  # filter() %>% 
  filter(
    ((DP %in% c("J210_", "J205_", "J121_", "B974_")) | (DR %in% c("J210_", "J205_", "J121_", "B974_"))),
    # IPP_sejour %in% (data_PCR %>% filter(Virus == "RSV", resultatbis == "POSITIVE") %>% pull(IPP_sejour) %>% unique), 
  type_sej == "HOSPITALISATION") %>% 
  group_by(season_admitted, age_group) %>% 
  summarise(n = n_distinct(IPP_alea), .groups = "drop") %>% 
  mutate(Year = gsub("([0-9]+)/([0-9]+)", "20\\2", season_admitted) %>% as.numeric) %>% 
  left_join(HCLcatchment_ageyear) %>% 
  mutate(Inc1e5 = n/catchmentHosp*1e5) %>% 
  filter(age_group == "80+") %>% 
  left_join(Marot) %>% 
  ggplot(aes(x = n)) + geom_histogram()
  ggplot(aes(x = Year, y = Inc1e5, fill = age_group)) + geom_bar(stat = "identity") + 
  facet_wrap(.~age_group, scales = "free_y")


