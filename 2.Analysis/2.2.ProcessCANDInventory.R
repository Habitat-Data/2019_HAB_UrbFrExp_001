################################################################################
## 2.2
## Process the Candiac Tree Inventory
################################################################################
#date: 20220621
#author: Noemie Lacroix-D.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0.SetEnvironment.R")

####LOAD DATA####
# Candiac tree inventory
cand_inv <- st_read(paste0(pathInput, "Arbre_candiac_2021.shp"))
# Ecosystem services estimations
itree_es=read.csv(paste0(pathInput, "Candiac_out_Itree_2018_final.csv"))
# Tree name database
name_db=read.csv(paste0(pathTreeTraits, "Taxonomy/Tree_Name_Database.csv"))

####DATA ANALYSIS####

### 1. Prep data
#How many observations in the original inventory
nObsStart=dim(cand_inv)[1]
print(paste0(nObsStart, " observations in the original Candiac database." ))

#Isolate just the relevant columns in the candiac inventory
cand_inv %>%
  cbind(st_coordinates(.)) %>% # get latitude and longitude 
  data.frame %>% # change to data frame 
  dplyr::select(UUID, ESSENCE, ESSENCE_TY, NOM_LATIN, DHP, X.1, Y.1) %>%
  filter(!ESSENCE %in% c("AAAA", "AUTRE")) %>% # remove fallen log and unknown species
  filter(!is.na(ESSENCE)) %>% # Remove NA 
  mutate(nom_fr = paste0(ESSENCE, " ", ESSENCE_TY)) %>% # Put genus and species in same column
  mutate(nom_fr = ifelse(is.na(ESSENCE_TY), ESSENCE, nom_fr)) %>% # remove NA if only genus specified 
  dplyr::select(UUID, nom_latin = NOM_LATIN, nom_fr, dhp = DHP, lat = Y.1, long = X.1) %>%
  mutate(nom_fr = Hmisc::capitalize(tolower(nom_fr)), 
         nom_latin = Hmisc::capitalize(tolower(nom_latin))) -> cand_inv

print(paste0(dim(cand_inv)[1], " observations after removing fallen logs and unknown species" ))

#Isolate just the relevant columns in the tree name database
name_db %>%
  dplyr::select(Full_latin, latin_simple, frgen, enggen, engsp, fctgr10) %>%
  mutate(Full_latin = Hmisc::capitalize(tolower(Full_latin))) -> name_db

### 2. Merge the tree names into the candiac inventory
# See the correspondance between the tree inventory and the name database
summary(cand_inv$nom_latin %in% name_db$Full_latin) # not perferct

# Change the names that do not match
cand_inv %>% 
  filter(!nom_latin %in% name_db$Full_latin) %>% 
  dplyr::select(nom_latin) %>% 
  unique -> no_occ

no_occ %>% 
  mutate(new_name=c("Gleditsia triacanthos var. inermis", "Prunus virginiana", "Ulmus", 
                    "Amelanchier x grandiflora", "Ulmus 'accolade'", "Populus",
                    "Gleditsia triacanthos var. inermis", "Gleditsia triacanthos", 
                    "Ulmus 'morton'", "Populus nigra",  "Syringa", 
                   "Acer platanoides", "Ulmus minor", "Ulmus glabra 'pendula'", 
                    NA, "Salix", "Populus", "Crataegus x mordenensis", "Taxus", 
                    "Prunus domestica 'italienne'", "Amelanchier x grandiflora 'robin hill'", 
                    "Quercus robur", "Ostrya", NA, "Elaeagnus angustifolia"), 
         new_fr_name = c("Févier d'amérique sans épine", "Cerisier a grappes", 
                         "Orme", "Amelanchier", "Orme 'Accolade'", "Peuplier", 
                         "Févier d'amérique sans épine", "Févier d'amérique", 
                         "Orme hybride", "Peuplier noir", "Lilas", "Érable de norvège", 
                         "Orme petit", "Orme parasol", NA, "Saule", "Peuplier", 
                         "Aubépine", "If", "Cerisier", "Amelanchier", "Chêne", 
                         "Ostryer", NA, "Olivier de bohème")) -> no_occ

# Change names
cand_inv %>%
  mutate(nom_latin = ifelse(nom_latin %in% no_occ$nom_latin, no_occ$new_name, nom_latin), 
         nom_fr = ifelse(nom_latin %in% no_occ$nom_latin, no_occ$new_fr_name, nom_fr)) -> cand_inv

# Merge with name database
cand_inv %>%
  left_join(name_db, by=c("nom_latin" = "Full_latin")) %>%
  filter(!is.na(latin_simple)) -> cand_inv # remove rows with no occurence

print(paste0(dim(cand_inv)[1], " observations after merging in species names." ))


### 3. Remove NA and 0 DBH values
cand_inv=cand_inv[!is.na(cand_inv$dhp),]
cand_inv=cand_inv[cand_inv$dhp!=0,]
print(paste0(dim(cand_inv)[1], " observations after removing NA and 0 DBH values." ))

#Round the DBH value to the closest 0.5 cm increment, make the max value 250
cand_inv$dhp=as.numeric(cand_inv$dhp)
cand_inv$dhp=round_any(cand_inv$dhp, 0.5)
cand_inv$dhp[cand_inv$dhp>250]=250

### 4. Add the ES values to the tree inventory
# Keep relevant columns 
itree_es %>%
  dplyr::select(UUID, Pollution.Removal..g.yr._2010, Avoided.Runoff..m.3.yr., 
                Gross.Carbon.Sequestration..kg.yr.) %>%
  setNames(c("UUID", "plrgyr", "rnfm3yr", "csqkgyr")) -> itree_es

# Merge
cand_inv %>%
  left_join(itree_es) -> cand_inv

# Reformat the plrgyr column to numeric
cand_inv %>%
  mutate(plrgyr = as.numeric(gsub(",", "", plrgyr))) -> cand_inv

#Reorder the columns names
head(cand_inv)
cand_inv %>%
  dplyr::select(ltnspp = nom_latin, ltnspp_simple = latin_simple, frspp = nom_fr,
                frgen, engspp = engsp, enggen, dhp, fctgr10, csqkgyr, rnfm3yr, 
                plrgyr, longi = long, latid = lat) -> cand_inv

print(paste0(dim(cand_inv)[1], " observations after adding ES estimates." ))

#How many observations retained?
nObsEnd=dim(cand_inv)[1]
nObsDropped=nObsStart-nObsEnd
percKept=round((nObsEnd/nObsStart)*100,2)
print(paste0("In total, ", nObsDropped, " observations dropped and ", percKept, 
             "% of ", "the Candiac city dataset retained."))

####EXPORT THE DATA####

write.csv(cand_inv, paste0(pathOutput, "Temp/cand_inv_se.csv"), 
          row.names=FALSE)


#End of script#
