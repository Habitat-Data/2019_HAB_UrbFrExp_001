################################################################################
## 04
## Calculate statistics
################################################################################
#date: 20220621
#author: Noemie Lacroix-D.

# Calculate statistics at cities and sectors scale 

####SETTING WORK DIRECTORY####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0.SetEnvironment.R")

memory.limit(size=90000)

####LOAD DATA####
sectors <- st_read(paste0(pathOutput, "Temp/sectors_delim.shp"))
cities <- st_read(paste0(pathOutput, "Temp/cities_delim.shp"))
tree_inv <- read.csv(paste0(pathOutput, "explorateur_inv_se.csv"), encoding="UTF-8")

####DATA ANALYSIS####
## 1. transform csv to shp
myProj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
tree_inv <- st_as_sf(tree_inv, coords = c("longi", "latid"), crs = myProj)


## 2. Intersect tree inventory and delimitation scales
tree_inv %>% 
  st_intersection(., cities) %>%
  data.frame -> tree_cities

tree_inv %>%
  st_intersection(., sectors) %>%
  data.frame -> tree_sectors

rm(tree_inv)

## 3. Calculate statistics
# 3.1 Richesse spécifique
# Villes 
tree_cities %>%
  dplyr::select(ltnspp_simple, Nom) %>% # is not including cultivars
  group_by(Nom) %>%
  dplyr::summarise(rich_spec = n_distinct(ltnspp_simple)) -> rich_spec_cities

cities <- merge(cities, rich_spec_cities)

# Secteurs
tree_sectors %>%
  dplyr::select(ltnspp_simple, secteurs) %>%
  group_by(secteurs) %>%
  dplyr::summarise(rich_spec = n_distinct(ltnspp_simple)) -> rich_spec_sectors

sectors <- merge(sectors, rich_spec_sectors)
rm(rich_spec_cities, rich_spec_sectors)

## 3.2 Diversité fonctionnelle
# Ville 
tree_cities %>%
  dplyr::select(Nom, fctgr10) %>%
  filter(! fctgr10 %in% c("","-")) %>%
  group_by(Nom, fctgr10) %>%
  dplyr::summarise(sum = n()) %>%
  pivot_wider(names_from = "fctgr10", values_from = "sum") %>%
  column_to_rownames(var="Nom")-> div_fct_villes

div_fct_villes <- as.data.frame(exp(diversity(div_fct_villes, index = "shannon")))
div_fct_villes <- rownames_to_column(div_fct_villes, "Nom")
colnames(div_fct_villes) <- c("Nom", "fct_div")
div_fct_villes$fct_div <- round(div_fct_villes$fct_div, digits = 2)

cities <- merge(cities, div_fct_villes)

# Secteurs
tree_sectors %>%
  dplyr::select(secteurs, fctgr10) %>%
  filter(fctgr10 != "-") %>%
  group_by(secteurs, fctgr10) %>%
  dplyr::summarise(sum = n()) %>%
  pivot_wider(names_from = "fctgr10", values_from = "sum") %>%
  column_to_rownames(var="secteurs") -> div_fct_sect

div_fct_sect[is.na(div_fct_sect)]=0
div_fct_sect <- as.data.frame(exp(diversity(div_fct_sect, index = "shannon")))
div_fct_sect <- rownames_to_column(div_fct_sect, "secteurs")
colnames(div_fct_sect) <- c("secteurs", "fct_div")
div_fct_sect$fct_div <- round(div_fct_sect$fct_div, digits = 2)

sectors <- merge(sectors, div_fct_sect)
rm(div_fct_sect, div_fct_villes)

## 3.3 Valeurs totales des SE
# Ville 
tree_cities %>% 
  dplyr::select(csqkgyr, rnfm3yr, plrkgyr, Nom) %>%
  group_by(Nom) %>%
  dplyr::summarise(across(everything(), sum)) -> se_cities

cities <- merge(cities, se_cities)

#Secteurs
tree_sectors %>% 
  dplyr::select(csqkgyr, rnfm3yr, plrkgyr, secteurs) %>%
  group_by(secteurs) %>%
  dplyr::summarise(across(everything(), sum)) -> se_sect

sectors <- merge(sectors, se_sect)
rm(se_cities, se_sect)

## 3.4 Nb arbres
# Ville 
tree_cities %>%
  group_by(Nom) %>%
  dplyr::summarise(nb_arbre = n()) -> arbres_cities

cities <- merge(cities, arbres_cities)

# Secteurs
tree_sectors %>%
  group_by(secteurs) %>%
  dplyr::summarise(nb_arbre = n()) -> arbres_sect

sectors <- merge(sectors, arbres_sect)
rm(arbres_cities, arbres_sect)

## 3.5 Nb arbres par hectares
# Ville 
cities %>%
  mutate(sup_ha = round(as.numeric(st_area(geometry) * 0.0001), digits=0),
         arb_par_ha = nb_arbre/sup_ha) -> cities

# Secteurs
sectors %>%
  mutate(sup_ha = round(as.numeric(st_area(geometry) * 0.0001), digits=0),
         arb_par_ha = nb_arbre/sup_ha) -> sectors

## 4. Last modifications 
cities %>%
  dplyr::mutate(across(where(is.numeric), ~round(., 1))) %>%
  dplyr::select(Nom, Type, rich_spec, fct_div, csqkgyr, rnfm3yr, plrkgyr, 
                nb_arbre, sup_ha, arb_par_ha, geometry)-> cities

sectors %>%
  dplyr::mutate(across(where(is.numeric), ~round(., 1))) %>%
  dplyr::select(Ville, secteurs, rich_spec, fct_div, csqkgyr, rnfm3yr, plrkgyr, 
                nb_arbre, sup_ha, arb_par_ha, geometry) -> sectors
  
  

## IMPORTANT !!! I changed some values for Candiac in order to match the 
# values in candiac report directly in QGIS after that (for species richness, 
# functional diversity and number of tree).

####EXPORT DATA####
st_write(cities, 
         paste0(pathOutput, "cities_info.shp"), 
         delete_layer = TRUE)

st_write(sectors, 
         paste0(pathOutput, "sectors_info.shp"), 
         delete_layer = TRUE)

#End of script#