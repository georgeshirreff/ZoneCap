# 2 shapefiles

## all ages
# region

area_reg <- read_csv2("Shapefiles/Region_Area.csv") %>% 
  transmute(RegCode = as.character(RegCode), Area_km2)

# https://data-interne.ademe.fr/datasets/geo-contours-regions
# https://data-interne.ademe.fr/data-fair/api/v1/datasets/geo-contours-regions/data-files/GEO_Contours_Regions.zip

shp_reg <- read_sf("/vsizip/Shapefiles/GEO_Contours_Regions.zip") %>% 
  select(-DREG_L_LIB) %>% 
  rename(RegCode = DREG_C_COD) %>% 
  filter(!(RegCode %in% paste0("0", 1:6))) %>% 
  left_join(area_reg)


setdiff(shp_reg$RegCode %>% sort, 
        reg_pop$RegCode %>% unique %>% sort)
setdiff(reg_pop$RegCode %>% unique %>% sort, 
        shp_reg$RegCode %>% sort
        
)

shp_reg %>% 
  left_join(reg_pop_age) %>% 
  filter(Year == 2024) %>% 
  ggplot() + geom_sf(aes(fill = Pop/Area_km2)) + 
  facet_wrap(.~age_group) + 
  scale_fill_continuous(trans = "log10")
  

# dep

# https://www.data.gouv.fr/datasets/contours-des-departements-francais-issus-d-openstreetmap
# https://www.data.gouv.fr/api/1/datasets/r/eb36371a-761d-44a8-93ec-3d728bec17ce
shp_dep <- read_sf("/vsizip/Shapefiles/departements-20180101-shp.zip") %>% 
  transmute(DepCode = code_insee, DepName = nom, Area_km2 = surf_km2) %>% 
  filter(!(DepCode %in% c(971:976))) %>% 
  mutate(DepName = ifelse(DepCode %in% c("69M", "69D"), "Rhône", DepName), 
         DepCode = ifelse(DepCode %in% c("69M", "69D"), "69", DepCode)) %>% 
  group_by(DepCode, DepName) %>% 
  summarise(Area_km2 = sum(Area_km2))

shp_depDOM <- read_sf("/vsizip/Shapefiles/departements-20180101-shp.zip") %>% 
  transmute(DepCode = code_insee, DepName = nom, Area_km2 = surf_km2) %>% 
  filter((DepCode %in% c(971:976)))

COM_area <- tibble(DepCode = c("977", "978"), Area_km2 = c(24, 53.2))


setdiff(shp_dep$DepCode %>% sort, 
        dep_pop$DepCode %>% unique %>% sort)
setdiff(dep_pop$DepCode %>% unique %>% sort, 
        shp_dep$DepCode %>% sort
        )

shp_dep %>% 
  left_join(dep_pop_age) %>% 
  filter(Year == 2024) %>% 
  ggplot() + geom_sf(aes(fill = Pop/Area_km2)) + 
  facet_wrap(.~age_group) + 
  scale_fill_continuous(trans = "log10")


# postcode

# https://www.data.gouv.fr/datasets/fond-de-carte-des-codes-postaux
shp_postcode_raw <- read_sf("/vsizip/Shapefiles/codes_postaux_V5.zip")


shp_postcode <- shp_postcode_raw %>% 
  transmute(Postcode = ID, #ifelse(ID == "74480", "74190", ID), 
            Postcode_Name = LIB, 
            DepCode = DEP, 
            Area_km2 = SURF, 
            POP2010) %>% 
  group_by(DepCode, Postcode, Postcode_Name) %>% 
  summarise(Area_km2 = sum(Area_km2), POP2010 = sum(POP2010)) %>% 
  ungroup %>% 
  left_join(pmsi_postcode %>% filter(DepCode %in% c("2A", "2B")) %>% transmute(Postcode, new_DepCode = DepCode) %>% unique, by = "Postcode") %>% 
  mutate(DepCode = ifelse(DepCode == "20", new_DepCode, DepCode))




  
  
# read_sf("Shapefiles/codes_postaux_V5/codes_postaux_region.shp") %>% 
#   filter(DEP == "59") %>% 
#   filter(grepl("Lille", LIB)) %>% 
#   ggplot() + geom_sf() + 
#   geom_sf_label(aes(label = paste(LIB, ID)))


setdiff(shp_postcode$Postcode, pmsi_postcode$Postcode)
setdiff(pmsi_postcode$Postcode, shp_postcode$Postcode)
setdiff(shp_postcode$DepCode, pmsi_postcode$DepCode)
setdiff(pmsi_postcode$DepCode, shp_postcode$DepCode)


# pmsi

shp_pmsi <- shp_postcode %>% 
  select(-DepCode) %>% 
  left_join(pmsi_postcode %>% select(Postcode, PMSIcode, DepCode, RegCode)) %>% 
  group_by(RegCode, DepCode, PMSIcode) %>% 
  summarise(Area_km2 = sum(Area_km2), POP2010 = sum(POP2010), nPostcodes = n(), Postcode_Names = paste(collapse = '_', unique(Postcode_Name))) %>% 
  ungroup


shp_pmsi %>% 
  left_join(pmsi_pop_age) %>% 
  filter(Year == 2024) %>% 
  ggplot() + geom_sf(aes(fill = Pop/Area_km2), colour = NA) + 
  facet_wrap(.~age_group) + 
  scale_fill_continuous(trans = "log10") + 
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

shp_fr <- shp_dep %>% 
  ungroup %>% 
  summarise(Area_km2 = sum(Area_km2), do_union = T)


save(shp_fr, file = "Shapefiles/Cleaned/shp_fr.RData")
save(shp_reg, file = "Shapefiles/Cleaned/shp_reg.RData")
save(shp_dep, file = "Shapefiles/Cleaned/shp_dep.RData")
save(shp_postcode, file = "Shapefiles/Cleaned/shp_postcode.RData")
save(shp_pmsi, file = "Shapefiles/Cleaned/shp_pmsi.RData")




# FINESS

finess_ref <- purrr::map(c(2016:2025), ~{
  readxl::read_excel(paste0("data/ATIH/FINESSref/mco_", .x, "_DEP_FINESS.xlsx"), skip = 1) %>% 
    mutate(Year = .x)
}) %>% 
  {do.call("rbind", .)} %>% 
  rename(Dep_Dom = `Département (localisation établissement)`) %>% 
  transmute(DepCode_Hosp = case_when(grepl("^NA", Dep_Dom) ~ "Dep missing", 
                                     # grepl("^TOM", Dep_Dom) ~ "TOM", 
                                     grepl("^Autre", Dep_Dom) ~ "Others", 
                                     grepl("^2[AB]", Dep_Dom) ~ gsub("^(2[AB]).*", "\\1", Dep_Dom), 
                                     T ~ gsub("\\D", "", Dep_Dom)), 
            FINESS_Hosp = `Finess PMSI`, 
            Geo_Hosp = `Finess géographique`) %>% 
  unique


chu_locations = readxl::read_xlsx("Shapefiles/top_CHUs.xlsx") %>% 
  separate(col = Coords, into = c("lat", "long"), sep = ", ") %>% 
  sf::st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(st_crs(shp_dep))

hcl_locations = readxl::read_xlsx("Shapefiles/HCL_sites.xlsx") %>% 
  separate(col = Coords, into = c("lat", "long"), sep = ", ") %>% 
  sf::st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(st_crs(shp_dep))

nonHCL_locations = readxl::read_xlsx("Shapefiles/nonHCL_sites.xlsx") %>% 
  separate(col = Coords, into = c("lat", "long"), sep = ", ") %>% 
  sf::st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(st_crs(shp_dep))


# get FINESS data, which has hospital categories and also geolocation data

# https://finess.esante.gouv.fr/fininter/jsp/index.jsp
# https://www.data.gouv.fr/datasets/finess-extraction-du-fichier-des-etablissements
# https://www.data.gouv.fr/datasets/finess-extraction-des-entites-juridiques

FINESSref_ej <- read_csv2(paste0("Shapefiles/FINESSref/etalab-cs1100501-stock-20260311-0344.csv"))

FINESSref_et <- read_csv2(paste0("Shapefiles/FINESSref/etalab-cs1100507-stock-20260311-0343.csv"))

FINESSref_historical_ej <- purrr::map(2004:2025, ~{
  print(.x)
  read_csv2(paste0("Shapefiles/FINESSref/etalab_stock_ej_", .x, "1231.csv")) %>% 
    mutate(Year = .x) %>% 
    mutate(across(contains(c("numvoie", "siren")), function(v) as.character(v)))
}) %>% 
  {do.call("bind_rows", .)}

FINESSref_historical_et <- purrr::map(2004:2025, ~{
  print(.x)
  read_csv2(paste0("Shapefiles/FINESSref/etalab_stock_et_", .x, "1231.csv")) %>% 
    mutate(Year = .x) %>% 
    mutate(across(contains(c("numvoie", "siren", "siret", "coordx", "coordy", "categretab", "sph")), function(v) as.character(v))) %>% 
    mutate(across(contains(c("dategeocod", "datemaj")), function(v) as.Date(v)))
}) %>% 
  {do.call("bind_rows", .)}


FINESScategories = rbind(FINESSref_ej %>% transmute(Year = 2026, FINESS = nofiness, CatNum = categetab, Category = libcategetab), 
                         FINESSref_et %>% transmute(Year = 2026, FINESS = nofinesset, CatNum = categagretab, Category = libcategagretab), 
                         FINESSref_historical_ej %>% transmute(Year, FINESS = nofinessej, CatNum = categej, Category = libcategej), 
                         FINESSref_historical_et %>% transmute(Year, FINESS = nofinesset, CatNum = categretab, Category = libcategretab)) %>% 
  filter(!is.na(Category)) %>% 
  group_by(FINESS) %>% 
  filter(Year == max(Year)) %>% 
  mutate(CategoryClean = case_when(CatNum == "101" ~ "CHR", 
                                   CatNum == "106" ~ "CHexHL", 
                                   CatNum == "1102" ~ "CH", 
                                   CatNum == "1103" ~ "CHspMental", 
                                   CatNum == "1104" ~ "CHspCancer", 
                                   CatNum == "1107" ~ "SSR", 
                                   CatNum == "1110" ~ "SCD", 
                                   CatNum == "1111" ~ "Other", #"Mental", 
                                   CatNum == "1203" ~ "Other", #"Dialysis", 
                                   CatNum == "1205" ~ "Other", 
                                   CatNum == "2205" ~ "Army", 
                                   CatNum == "292" ~ "CHspMental", 
                                   CatNum == "355" ~ "CH", 
                                   CatNum == "699" ~ "Other", 
                                   T ~ "Other"
  ))

metropole <- read_csv2("Shapefiles/Communes_metropoleLyon.csv")