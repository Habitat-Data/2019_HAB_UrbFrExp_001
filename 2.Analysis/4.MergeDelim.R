################################################################################
## 04
## Merge delimitations
################################################################################
#date: 20220621
#author: Noemie Lacroix-D.

# Merge Montreal and Candiac delimitations

####SETTING WOEK DIRECTORY####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0.SetEnvironment.R")


####LOAD DATA####
cand_sect <- st_read(paste0(pathInput, "delim_secteurs_Candiac.shp"))
cand_ville <- st_read(paste0(pathInput, "delim_ville_Candiac.shp"))
mtl_sect <- st_read(paste0(pathInput, "delim_secteurs_mtl.shp"))
mtl_ville <- st_read(paste0(pathInput, "delim_ville_mtl.shp"))
ville_liees <- st_read(paste0(pathInput, "delim_villes_liées.shp"))


####DATA ANALYSIS####
## 1. Merge sectors delimitations
cand_sect %>%
  mutate(Ville="Candiac") %>%
  dplyr::select(Ville, secteurs = ID_SECTEUR, geometry) %>%
  st_transform(., crs(mtl_sect)) -> cand_sect

mtl_sect %>%
  mutate(Ville="Montréal") %>%
  dplyr::select(Ville, secteurs = NOM, geometry) %>%
  rbind(cand_sect) %>%
  filter(secteurs != "SCT_011") %>% # no tree in this sector
  group_by(Ville, secteurs) %>%
  dplyr::summarise(geometry = st_union(geometry)) -> sect_merged


## 2. Merge cities delimitations
cand_ville %>%
  dplyr::select(Type = MUS_DE_IND, Nom = MUS_NM_MUN, geometry) %>%
  st_transform(., crs(mtl_ville))-> cand_ville

mtl_ville %>%
  mutate(Nom = "Montréal") %>%
  dplyr::select(Type = TYPE, Nom, geometry) %>%
  rbind(cand_ville) -> ville_merged

ville_liees %>%
  dplyr::select(Type = TYPE, Nom = NOM, geometry) %>%
  group_by(Type, Nom) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  rbind(ville_merged) %>% 
  dplyr::select(Nom != "Dollard-des-Ormeaux")-> ville_merged
                  
####EXPORT DATA####
st_write(sect_merged, 
         paste0(pathOutput, "/sectors_delim.shp"), 
         append=FALSE)

st_write(ville_merged, 
         paste0(pathOutput, "/cities_delim.shp"),
         append=FALSE)

#End of script#
