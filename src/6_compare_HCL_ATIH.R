library(tidyverse)

dep_ageyear_catchment <- read_csv2("output/dep_ageyear_HCLcatchment.csv")
dep_year_diag_catchment <- readxl::read_xlsx("output/dep_diag_HCLcatchment.xlsx")
urg_catchment <- read_csv2("output/urgARA_HCLcatchment.csv")

# read proportions of each departement from the FIRESP data
dep_ageyear_firesp_hosp_p <- read_csv2("data/HCL/dep_ageyear_firesp_hosp_p.csv")
dep_ageyear_firesp_urg_p <- read_csv2("data/HCL/dep_ageyear_firesp_urg_p.csv")


comparison_hosp <- dep_ageyear_catchment %>% 
  transmute(DepCode, RegCode = as.character(RegCode), Year, age_group, Pop, n_Hosp, catchmentHosp) %>% 
  # group_by(Year, DepCode) %>% 
  # summarise(across(c("n_FIRESP", "catchment_HCL", "n_HCL", "Pop"), sum)) %>% 
  group_by(Year, age_group) %>% 
  mutate(p_catchmentHCL = catchmentHosp/sum(catchmentHosp, na.rm = T), 
         p_Hosp = n_Hosp/sum(n_Hosp, na.rm = T), 
         p_Pop = Pop/sum(Pop, na.rm = T)
  ) %>% 
  left_join(dep_ageyear_firesp_hosp_p) %>% 
  mutate(type_sej = "Hospitalisation")

comparison_urg <- urg_catchment %>% 
  filter(RegCode %in% c("84")) %>% 
  transmute(DepCode, RegCode = as.character(RegCode), Year, age_group, Pop, n_HCL, catchment_HCL) %>% 
  # group_by(Year, DepCode) %>% 
  # summarise(across(c("n_FIRESP", "catchment_HCL", "n_HCL", "Pop"), sum)) %>% 
  group_by(Year, age_group) %>% 
  mutate(p_catchmentHCL = catchment_HCL/sum(catchment_HCL, na.rm = T), 
         p_HCL = n_HCL/sum(n_HCL, na.rm = T), 
         p_Pop = Pop/sum(Pop, na.rm = T)
  ) %>% 
  left_join(dep_ageyear_firesp_urg_p) %>% 
  mutate(type_sej = "Emergency")



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

ggsave(filename = "output/atih_firesp_comparison_HospUrg.png", width = 20, height = 12, units = "cm", dpi = 300)

png(filename = "output/atih_firesp_comparison_HospUrg_legend.png", width = 10, height = 10, units = "cm", res = 300)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", title="", 
       legend = c("Data", "Regression line", "Confidence interval", "Equality"), 
       lty=c(NA, 1, 1, 9), 
       col = c('grey50', 'red', 'salmon', 'black'), 
       pch = c(16, NA, NA, NA), 
       lwd=c(NA, 2, 8, 2), 
       cex=1.25,
       bty='n')
dev.off()


