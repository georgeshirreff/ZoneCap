library(sf)

rds = readRDS(file = "~/FIRESP/CatchmentPop/config_analyse_territoriale.rds")

names(rds)

# rds$zone_polyg %>% 
  rds$zone_agreg_polyg %>% 
ggplot() + 
  geom_sf()

   %>% 
     %>% 
    ggplot() + 
    geom_sf()
  
ggplot() + 
  geom_sf(data = rds$zone_polyg, aes(fill = pop)) + 
  geom_sf(data = rds$zone_agreg_polyg, fill = NA, linewidth = 1) + 
  geom_sf(data = rds$zone_coord %>% 
            sf::st_as_sf(coords = c("long", "lat")) %>% 
            st_set_crs(st_crs(rds$zone_polyg))
          , fill = NA, linewidth = 1)


  

  CRS()
