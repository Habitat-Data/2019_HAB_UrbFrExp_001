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
sectors <- st_read(paste0(pathOutput, "sectors_delim.shp"))
cities <- st_read(paste0(pathOutput, "cities_delim.shp"))
tree_inv <- st_read(paste0(pathOutput, "isl_inv_se.shp"))

####DATA ANALYSIS####
## 1. Intersect tree inventory and delimitation scales
tree_inv %>% 
  st_intersection(., cities) %>%
  data.frame -> tree_cities

tree_inv %>%
  st_intersection(., sectors) %>%
  data.frame -> tree_sectors

rm(tree_inv)

## 2. Calculate statistics
# 2.1 Richesse spécifique
# Villes 
tree_cities %>%
  dplyr::select(ltnspp_sim, Nom) %>% # is not including cultivars
  group_by(Nom) %>%
  dplyr::summarise(rich_spec = n_distinct(ltnspp_sim)) -> rich_spec_cities

cities <- merge(cities, rich_spec_cities)

# Secteurs
tree_sectors %>%
  dplyr::select(ltnspp_sim, secteurs) %>%
  group_by(secteurs) %>%
  dplyr::summarise(rich_spec = n_distinct(ltnspp_sim)) -> rich_spec_sectors

sectors <- merge(sectors, rich_spec_sectors)
rm(rich_spec_cities, rich_spec_sectors)

## 2.2 Diversité fonctionnelle
# Ville 
tree_cities %>%
  dplyr::select(Nom, fctgr10) %>%
  filter(fctgr10 != "-") %>%
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

## 2.3 Valeurs totales des SE
# Ville 
tree_cities %>% 
  dplyr::select(csqkgyr, rnfm3yr, plrgyr, Nom) -> temp

#Secteurs


## 2.4 Nb arbres
# Ville 

# Secteurs


## 2.5 Nb arbres par hectares
# Ville 

# Secteurs


## IMPORTANT !!! I changed some values for Candiac in order to match the 
# values in candiac report directly in QGIS after that (for species richness, 
# functional diversity and number of tree).

####EXPORT DATA####

#End of script#