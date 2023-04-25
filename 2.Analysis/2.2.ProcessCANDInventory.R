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
cand_inv <- read.csv(paste0(pathInput, "Arbre_candiac_2021.csv"), fileEncoding = "latin1")
# Ecosystem services estimations
itree_es <- read.csv(paste0(pathInput, "Candiac_out_Itree_2018_final.csv"))
# Tree name database
name_db <- read.csv(paste0(pathTreeTraits, "Taxonomy/Tree_Name_Database.csv"))
# Functional groups database
fctgrp <- read.csv(paste0(pathTreeTraits, "FunctionalDiversity/functionalGroups_simplified.csv"))


####DATA ANALYSIS####
### 1. Prep data
#How many observations in the original inventory
nObsStart=dim(cand_inv)[1]
print(paste0(nObsStart, " observations in the original Candiac database." ))

#Isolate just the relevant columns in the candiac inventory
cand_inv <- cand_inv %>%
  filter(!ESSENCE %in% c("AAAA", "AUTRE")) %>% # remove fallen log and unknown species
  filter(!is.na(ESSENCE)) %>% # Remove NA 
  mutate(nom_fr = paste0(ESSENCE, " ", ESSENCE_TY)) %>% # Put genus and species in same column
  mutate(nom_fr = ifelse(is.na(ESSENCE_TY), ESSENCE, nom_fr)) %>% # remove NA if only genus specified 
  dplyr::select(UUID, nom_latin = NOM_LATIN, nom_fr, dhp = DHP, lat = Y.1, long = X.1) %>%
  mutate(nom_fr = Hmisc::capitalize(tolower(nom_fr)), 
         nom_latin = Hmisc::capitalize(tolower(nom_latin)))

print(paste0(dim(cand_inv)[1], " observations after removing fallen logs and unknown species" ))

# Prep name database
name_db <- name_db %>%
  dplyr::select(Full_latin, acc_latin_simple, frgen, enggen, engsp) %>%
  left_join(fctgrp[, c("Full_latin", "fctgrp_2022")]) %>% # add functional groups
  mutate(fctgrp_2022 = ifelse(fctgrp_2022 == "Arbuste", NA, fctgrp_2022))

### 2. Merge the tree names into the candiac inventory
# See the correspondance between the tree inventory and the name database
summary(cand_inv$nom_latin %in% name_db$Full_latin) # not perfect

# Change the names that do not match
cand_inv %>% 
  filter(!nom_latin %in% name_db$Full_latin) %>% 
  dplyr::select(nom_latin) %>% 
  unique -> no_occ

no_occ <- no_occ[order(no_occ[,"nom_latin"]), ] %>% 
  data.frame %>% 
  setNames(c("nom_latin"))

no_occ$new_name <- c("Acer platanoides", "Acer platanoides 'Columnare'",
                     "Acer platanoides 'Crimson King'", "Acer platanoides 'Drumondii'",
                     "Acer platanoides 'Globosum'", "Acer platanoides 'Royal Red'",
                     "Acer rubrum 'Armstrong'", "Acer rubrum 'Northwood'",
                     "Aesculus x carnea", "Amelanchier", 
                     "Amelanchier x grandiflora", "Chamaecyparis nootkatensis", 
                     "Elaeagnus angustifolia", NA, 
                     "Fraxinus pennsylvanica 'Summit'", "Gleditsia triacanthos var. inermis",
                     "Gleditsia triacanthos", "Gleditsia triacanthos var. inermis",
                     "Morus alba 'Pendula'", "Ostrya", 
                     "Picea pungens 'Glauca'", "Pinus nigra 'Austriaca'", 
                     "Populus alba 'Pyramidalis'", "Populus nigra 'Italica'", 
                     "Populus", "Prunus domestica 'Italienne'", 
                     "Prunus virginiana 'Shubert'", "Quercus robur 'Fastigiata'", 
                     "Quercus robur", "Salix alba 'Tristis'", 
                     NA, "Ulmus minor", 
                     "Ulmus glabra 'Pendula'", "Ulmus 'Accolade'", 
                     "Ulmus davidiana 'Discovery'", "Ulmus 'Morton'")

no_occ$new_fr_name <- c("Érable de norvège", "Érable de norvège",
                        "Érable de norvège", "Érable de norvège", 
                        "Érable de norvège", "Érable de norvège", 
                        "Érable rouge", "Érable rouge", 
                        "Marronier rouge", "Amélanchier", 
                        "Amélanchier", "Faux cypres de nootka", 
                        "Olivier de bohème", NA, 
                        "Frêne de pensylvannie", "Févier d'Amérique", 
                        "Févier d'Amérique", "Févier d'Amérique", 
                        "Mûrier blanc", "Ostryer", 
                        "Épinette du colorado", "Pin noir", 
                        "Peuplier blanc", "Peuplier noir", 
                        "Peuplier", "Prunier", 
                        "Cerisier de Virginie", "Chêne pédonculé", 
                        "Chêne pédonculé", "Saule blanc", 
                        NA, "Orme champêtre", 
                        "Orme de montagne", "Orme accolade", 
                        "Orme discovery", "Orme")

# Change names
cand_inv  <- cand_inv %>%
  mutate(nom_latin = ifelse(nom_latin %in% no_occ$nom_latin, no_occ$new_name, nom_latin), 
         nom_fr = ifelse(nom_latin %in% no_occ$nom_latin, no_occ$new_fr_name, nom_fr))

summary(cand_inv$nom_latin %in% name_db$Full_latin) # way better

# Merge with name database
cand_inv <- cand_inv %>%
  left_join(name_db, by = c("nom_latin" = "Full_latin")) %>%
  filter(!is.na(acc_latin_simple)) # remove rows with no occurence

print(paste0(dim(cand_inv)[1], " observations after merging in species names." ))


### 3. Remove NA and 0 DBH values
cand_inv <- cand_inv[!is.na(cand_inv$dhp),]
cand_inv <- cand_inv[cand_inv$dhp != 0,]
print(paste0(dim(cand_inv)[1], " observations after removing NA and 0 DBH values." ))

#Round the DBH value to the closest 0.5 cm increment, make the max value 250
cand_inv$dhp <- as.numeric(cand_inv$dhp)
cand_inv$dhp <- round_any(cand_inv$dhp, 0.5)
cand_inv$dhp[cand_inv$dhp > 250] = 250

### 4. Add the ES values to the tree inventory
# Keep relevant columns 
itree_es <- itree_es %>%
  dplyr::select(UUID, plrgyr = Pollution.Removal..g.yr._2010, 
                rnfm3yr = Avoided.Runoff..m.3.yr., 
                csqkgyr = Gross.Carbon.Sequestration..kg.yr.)

# Merge
cand_inv <- cand_inv %>%
  left_join(itree_es)

# Reformat the plrgyr column to numeric
cand_inv <- cand_inv %>%
  mutate(plrgyr = as.numeric(gsub(",", "", plrgyr))) 

#Reorder the columns names
names(cand_inv)
cand_inv <- cand_inv %>%
  dplyr::select(ltnspp = nom_latin, ltnspp_simple = acc_latin_simple, 
                frspp = nom_fr, frgen, engspp = engsp, enggen, dhp, 
                fct_grp = fctgrp_2022, csqkgyr, rnfm3yr, plrgyr, 
                longi = long, latid = lat)

print(paste0(dim(cand_inv)[1], " observations after adding ES estimates." ))

#How many observations retained?
nObsEnd <- dim(cand_inv)[1]
nObsDropped <- nObsStart - nObsEnd
percKept <- round((nObsEnd/nObsStart) * 100, 2)
print(paste0("In total, ", nObsDropped, " observations dropped and ", percKept, 
             "% of ", "the Candiac city dataset retained."))

####EXPORT THE DATA####
write.csv(cand_inv, 
          paste0(pathOutput, "Temp/cand_inv_se.csv"), 
          row.names = FALSE)

#End of script#