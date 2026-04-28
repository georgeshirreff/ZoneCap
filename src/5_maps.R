# make maps of analysis
library(sf)
library(tidyverse)


library(gridExtra)
library(grid)

grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    combined
    # grid.newpage()
    # grid.draw(combined)
    # 
    # # return gtable invisibly
    # invisible(combined)
    
  }


## HCL

# resolution

# density_colour_scale = c(#"#C6DBEF", 
#                          "#6BAED6", "#08306B", "#041F4A")
density_colour_scale = c(
  # "#DEEBF7",
"#9ECAE1",
"#6BAED6",
"#3182BD",
"#08519C",
"#08306B",
"#041F4A")
na_colour = "#E0E0E0"

bind_rows(shp_reg %>% 
            left_join(reg_year_finess_catchment_poparea %>% 
                        filter(grepl("690781810", FINESS_Hosp)) %>% 
                        group_by(RegCode) %>% 
                        summarise(catchmentHosp = mean(catchmentHosp, na.rm = T))) %>% 
            mutate(Resolution = "Region"),
          shp_dep %>% 
            left_join(dep_year_finess_catchment_poparea %>% 
                        filter(grepl("690781810", FINESS_Hosp)) %>% 
                        group_by(DepCode) %>% 
                        summarise(catchmentHosp = mean(catchmentHosp, na.rm = T))) %>% 
            mutate(Resolution = "Department"),
          shp_pmsi %>% 
            left_join(pmsi_year_finess_catchment_poparea%>% 
                        filter(grepl("690781810", FINESS_Hosp)) %>% 
                        group_by(PMSIcode) %>% 
                        summarise(catchmentHosp = mean(catchmentHosp, na.rm = T))) %>% 
            mutate(Resolution = "PMSI code") %>% 
            st_transform(st_crs(shp_dep))) %>% 
  mutate(Resolution = factor(Resolution, c("Region", "Department", "PMSI code"))) %>% 
  ggplot() + 
  geom_sf(aes(fill = catchmentHosp/Area_km2), colour = NA) + 
  geom_sf(data = chu_locations %>% filter(grepl("LYON", FINESS_Hosp)), colour = "red") + 
  geom_sf(data = shp_fr, fill = NA, colour = "darkgrey", linewidth = 0.5) + 
  facet_wrap(.~Resolution) + 
  # scale_fill_continuous(trans = "log10", na.value = "lightgrey"
  #                       # breaks = c(0.01, 0.1, 1, 10, 100), 
  #                       # labels = scales::label_number_auto()
  #                       # labels = c("0.01", "0.1", "1", "10", "100")
  # ) +
  scale_fill_gradientn(colours = density_colour_scale, trans = "log10", na.value = na_colour) + 
  theme(panel.background = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  labs(fill = "Catchment density\n(persons per km2)")

ggsave("output/HCL_resolution.png", width = 30, height = 10, units = "cm", dpi = 600)


## all finess


finess_year_dep_meancatchment <- dep_year_finess_catchment_poparea %>% 
  # filter(Year %in% 2020:2024) %>% 
  filter(FINESS_Hosp != "Others") %>% 
  group_by(RegCode, DepCode, FINESS_Hosp) %>% 
  summarise(n_Hosp = mean(n_Hosp, na.rm = T), catchmentHosp = mean(catchmentHosp, na.rm = T)) %>% 
  group_by(FINESS_Hosp) %>% 
  mutate(n_Hosp_TOTAL = sum(n_Hosp, na.rm = T), catchmentHosp_TOTAL = sum(catchmentHosp, na.rm = T)) %>% 
  ungroup %>% 
  arrange(-n_Hosp_TOTAL)


finess_year_dep_meancatchment %>% 
  # filter(n_Hosp_TOTAL > 500000) %>% 
  filter(n_Hosp_TOTAL > 69730) %>% 
  select(FINESS_Hosp) %>% unique %>% unlist %>% cat(sep = "\n")

finess_toPlot <- finess_year_dep_meancatchment %>% 
  # filter(n_Hosp_TOTAL > 500000) %>% 
  filter(n_Hosp_TOTAL > 69730) %>%
  arrange(-n_Hosp_TOTAL) %>% 
  mutate(Hosp = gsub(" - ", "\n", FINESS_Hosp)) %>% 
  mutate(Hosp = factor(Hosp, unique(Hosp))) %>% 
  select(DepCode, Hosp, catchmentHosp) %>% 
  complete(DepCode, Hosp, fill = list(catchmentHosp = NA))


ggplot() + 
  geom_sf(data = shp_dep %>% 
            left_join(finess_toPlot) %>% 
            # filter(grepl("440000289", Hosp)) %>%
            mutate(catchment_density = catchmentHosp/Area_km2),
          aes(fill = catchment_density), linewidth = 0.1, colour = "darkgrey") + 
  geom_sf(data = chu_locations %>% 
            filter(!grepl("GRENOBLE", FINESS_Hosp)) %>% 
            mutate(Hosp = gsub(" - ", "\n", FINESS_Hosp), 
                   Hosp = factor(Hosp, levels = levels(finess_toPlot$Hosp)))
            # filter(grepl("440000289", FINESS_Hosp))
            , colour = "red", size = 0.5) + 
  facet_wrap(.~Hosp, nrow = 2) + 
  # scale_fill_continuous(trans = "log10", 
  #                       # breaks = c(0.01, 0.1, 1, 10, 100), 
  #                       # labels = scales::label_number_auto()
  #                       # labels = c("0.01", "0.1", "1", "10", "100")
  #                       ) +
  scale_fill_gradientn(colours = density_colour_scale, trans = "log10", na.value = na_colour) + 
  theme(panel.background = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  labs(fill = "Catchment density\n(persons per km2)")

ggsave("output/FINESS_catchment_10.png", width = 30, height = 15, units = "cm", dpi = 600)

# HCL Geo_Hosp


pmsi_year_finessgeo_catchmentHCL <- pmsi_year_finessgeo_catchment_poparea %>%
  filter(grepl("690781810", FINESS_Hosp)) %>% 
  clean_GeoHosp %>% 
  group_by(Geo_Hosp, PMSIcode) %>% 
  summarise(catchmentHosp = mean(catchmentHosp, na.rm = T)) %>% 
  ungroup %>% 
  mutate(PMSIcode = factor(PMSIcode, levels = shp_pmsi %>% 
                             filter(RegCode %in% c("27", "44", "84", "93")) %>% 
                             pull(PMSIcode))) %>% 
  complete(Geo_Hosp, PMSIcode, fill = list(catchment_Hosp = NA))

Geo_Hosp_order <- pmsi_year_finessgeo_catchmentHCL %>% 
  group_by(Geo_Hosp) %>% 
  summarise(catchmentHosp = sum(catchmentHosp, na.rm = T)) %>% 
  arrange(-catchmentHosp) %>% 
  pull(Geo_Hosp) %>% 
  unique

# pmsi_year_finessgeo_catchmentHCL %>% 
#   pull(Geo_Hosp) %>% unique %>% cat(sep = '\n')


ggplot() +
  geom_sf(data = shp_pmsi %>% 
            filter(RegCode %in% c("27", "44", "84", "93")) %>%
            filter(RegCode %in% c("27", "84")) %>% 
            st_transform(st_crs(shp_reg)) %>% 
            left_join(pmsi_year_finessgeo_catchmentHCL %>% 
                        mutate(Geo_Hosp = factor(gsub(" - ", "\n", Geo_Hosp), 
                                                 levels = gsub(" - ", "\n", Geo_Hosp_order)))) %>% 
            filter(!grepl("^830100558|690787452|690787478|690784202|690787429", Geo_Hosp))
          # filter(!grepl("^830100558", Geo_Hosp))
          ,
          
          aes(fill = catchmentHosp/Area_km2), colour = NA) + 
  geom_sf(data = shp_dep %>% 
            left_join(dep_reg %>% select(DepCode, RegCode)) %>% 
            # filter(RegCode %in% c("27", "44", "84", "93")), 
            filter(RegCode %in% c("27", "84")), 
          linewidth = 0.1, colour = "grey60", fill = NA) + 
  geom_sf(data = shp_reg %>% 
            # filter(RegCode %in% c("27", "44", "84", "93")), 
            filter(RegCode %in% c("27", "84")), 
          linewidth = 0.2, colour = "black", fill = NA) + 
  geom_sf(data = hcl_locations %>% 
            mutate(Geo_Hosp = gsub(" - ", "\n", Geo_Hosp), 
                   Geo_Hosp = factor(Geo_Hosp, levels = gsub(" - ", "\n", Geo_Hosp_order))) %>% 
            filter(!grepl("^830100558|690787452|690787478|690784202|690787429", Geo_Hosp))
          # filter(!grepl("^830100558", Geo_Hosp))
          , 
          # filter(grepl("440000289", FINESS_Hosp))
          , colour = "red", size = 0.3) + 
  facet_wrap(.~Geo_Hosp, nrow = 2) + 
  # scale_fill_continuous(trans = "log10", na.value = "lightgrey"
  #                       # breaks = c(0.01, 0.1, 1, 10, 100), 
  #                       # labels = scales::label_number_auto()
  #                       # labels = c("0.01", "0.1", "1", "10", "100")
  # ) +
  scale_fill_gradientn(colours = density_colour_scale, trans = "log10", na.value = na_colour) +
  theme(panel.background = element_blank(), 
        strip.text = element_text(size = 7), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  labs(fill = "Catchment density\n(persons per km2)")

ggsave("output/HCL_sites.png", width = 30, height = 12, units = "cm", dpi = 600)


