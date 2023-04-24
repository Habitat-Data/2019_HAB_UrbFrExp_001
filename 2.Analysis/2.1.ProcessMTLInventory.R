################################################################################
## 2.1
## Process the Montreal Tree Inventory
################################################################################
#date: 20210526
#author: Kyle T. Martins

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0.SetEnvironment.R")

####LOAD THE DATA####
# Montreal tree database
mtl_inv <- read.csv(paste0(pathInput, "arbres-publics.csv"), fileEncoding="UTF-8")

#Load the tree name database
name_db <- read.csv(paste0(pathTreeTraits, "Taxonomy/Tree_Name_Database.csv"))

#Load the functional groups database
fctgrp <- read.csv2(paste0(pathTreeTraits, "FunctionalDiversity/functionalGroups_simplified.csv"))

#Load the iTree ecosystem service estimations
itree_es <- read.csv(paste0(pathTreeTraits, "iTreeData/iTreeOutputSppxDBH.csv"))


####DATA ANALYSIS####
## 1. Prep mtl inventory data
# How many observations in the original inventory ?
nObsStart <- nrow(mtl_inv)

# Isolate relevant columns in the MTL inventory
mtl_inv <- mtl_inv[c("SIGLE", "Essence_latin", "Essence_fr", "DHP", "Longitude", 
                     "Latitude")]

print(paste0(dim(mtl_inv)[1], " observations in the original MTL database." ))

# Isolate relevant columns in the tree name database
name_db <- name_db[c("mtl_code", "Full_latin", "itree_code", "acc_latin_simple", "frgen", 
                     "enggen", "engsp")]

# Merge the tree names into the MTL inventory
summary(mtl_inv$SIGLE %in% name_db$mtl_code)
name_db <- name_db[name_db$mtl_code %in% mtl_inv$SIGLE,]
name_db <- name_db[!duplicated(name_db$mtl_code),]
mtl_inv <- merge(mtl_inv, name_db, by.x = "SIGLE", by.y = "mtl_code")

# Correct some names
gltr_codes <- c("GLTRSU", "GLTRST", "GLTRNA", "GLTRRU", "GLTRSH", "GLTRSK", 
                "GLTRTS")
mtl_inv <- mtl_inv %>% 
  mutate(acc_latin_simple = ifelse(SIGLE %in% gltr_codes, "Gleditsia triacanthos", 
                                   acc_latin_simple), 
         acc_latin_simple = ifelse(SIGLE == "JU", "Juglans", acc_latin_simple))

print(paste0(dim(mtl_inv)[1], " observations after merging in species names." ))


## 2. Add iTree ecosystem services 
# Prep iTree data
itree_es <- itree_es %>%
  dplyr::select(sp_eng = Species.Name, itree_code = itreecode, DHP = DBH..cm., 
                carbonseqkhyr = Gross.Carbon.Sequestration..kg.yr., 
                runoffm3yr = Avoided.Runoff..m.3.yr., 
                polremgyr = Pollution.Removal..g.yr.) %>%
  mutate(polremgyr = gsub(",", "", polremgyr),
         polremgyr = as.numeric(polremgyr))

head(itree_es)

# Remove japanese larch since duplicated (already have Larix kaempferi)
itree_es <- itree_es[itree_es$sp_eng != "Japanese larch", ]

# Remove AAAA observations
mtl_inv <- mtl_inv[mtl_inv$SIGLE != "AAAA", ]
print(paste0(dim(mtl_inv)[1], " observations after removing AAAA." ))

# Make corrections to the itree codes in the MTL inventory to facilitate the 
# merge
mtl_inv <- mtl_inv[mtl_inv$itree_code != "" ,]
head(mtl_inv)
itree_codes <- as.character(unique(mtl_inv$itree_code))
summary(itree_codes %in% itree_es$itree_code)
missing_codes <- itree_codes[!itree_codes %in% itree_es$itree_code]
mtl_inv$itree_code[mtl_inv$itree_code %in% 
                    missing_codes & nchar(mtl_inv$itree_code) > 4] =
  substr(mtl_inv$itree_code[mtl_inv$itree_code %in% 
                             missing_codes & nchar(mtl_inv$itree_code) > 4],1,4)
itree_codes <- as.character(unique(mtl_inv$itree_code))
summary(itree_codes %in% itree_es$itree_code)
missing_codes <- itree_codes[!itree_codes %in% itree_es$itree_code]

print(paste0(dim(mtl_inv)[1], " observations after correcting iTree codes." ))

# Remove NA DBH values
mtl_inv <- mtl_inv[!is.na(mtl_inv$DHP),]

print(paste0(dim(mtl_inv)[1], " observations after removing NA DBH values." ))

# Round the DBH value to the closest 0.5 cm increment, make the max value 250
mtl_inv$DHP <- round_any(mtl_inv$DHP, 0.5)
mtl_inv$DHP[mtl_inv$DHP > 250] = 250

# Add the ES estimates to the tree inventory
dim(mtl_inv)
mtl_inv <- merge(mtl_inv, itree_es, by = c("itree_code", "DHP"))
dim(mtl_inv)

print(paste0(dim(mtl_inv)[1], " observations after adding ES estimates." ))


## 3. Add functional groups
summary(mtl_inv$Essence_latin %in% fctgrp$Full_latin)
summary(mtl_inv$Full_latin %in% fctgrp$Full_latin)

mtl_inv <- mtl_inv %>% 
  left_join(fctgrp[, c("Full_latin", "fctgrp_2022")]) %>%
  mutate(fctgrp_2022 = ifelse(fctgrp_2022 == "Arbuste", NA, fctgrp_2022))


## 4. Finalize data
# Reorder the columns names
names(mtl_inv)
mtl_inv <- mtl_inv %>%
  dplyr::select(ltnspp = Essence_latin, ltnspp_simple = acc_latin_simple, 
                frspp = Essence_fr, frgen, engspp = engsp, enggen,  dhp = DHP,
                fct_grp = fctgrp_2022, csqkgyr = carbonseqkhyr, rnfm3yr = runoffm3yr, 
                plrgyr = polremgyr, longi = Longitude, latid = Latitude)

head(mtl_inv)

# Remove outlyers
mtl_inv  <- mtl_inv %>% 
  filter(latid > 45.33 & latid < 45.7026 & longi > -73.937 & longi < -73.477)

print(paste0(dim(mtl_inv)[1], " observations after removing outlyers." ))

# How many observations retained?
nObsEnd <- nrow(mtl_inv)
nObsDropped <- nObsStart - nObsEnd
percKept <- round((nObsEnd/nObsStart) * 100, 2)
print(paste0("In total, ", nObsDropped, " observations dropped and ", percKept, 
      "% of ", "the MTL city dataset retained."))


####EXPORT THE DATA####
write.csv(mtl_inv, 
          paste0(pathOutput, "Temp/mtl_inv_se.csv"), 
          row.names = FALSE)

#End of script#