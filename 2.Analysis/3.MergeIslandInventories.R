################################################################################
## 03
## Merge Island Inventories
################################################################################
#date: 20210526
#author: Kyle T. Martins

#Add inventory data for neighboring municipalities to updated MTL inventory data

####SETTING WOEK DIRECTORY####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0.SetEnvironment.R")


####LOAD DATA####
#Load the processed version of the Montreal tree database (updated in script 1)
mtl_inv=read.csv(paste0(pathOutput, "Temp/mtl_inv_se.csv"))
head(mtl_inv)

#Load the 2019 version of the island inventory
print("Loading MTL island dataset.")
isl_inv=read.csv(paste0(pathTreeInventory,
                        "montreal-island-tree-db-2019-05-28.csv"),
                 stringsAsFactors=FALSE)[,-1]
head(isl_inv)

#Load the 2021 data for Parc Jean Drapeau
pjd_inv=read.csv(paste0(pathInput, "JNDRP_InvArbre_UrbFrExp.csv"))

#Load Candiac inventory
cand_inv <- read.csv(paste0(pathOutput, "Temp/cand_inv_se.csv"))

#Load the tree name database
name_db=read.csv(paste0(pathTreeTraits, "Taxonomy/Tree_Name_Database.csv"))

####DATA ANALYSIS####
# Only keep relevant columns from name_db table
name_db %>%
  dplyr::select(acc_sp, latin_simple, fctgr10) %>%
  filter(!fctgr10 %in% c("", "-")) %>%
  unique() %>%
  filter(!latin_simple %in% c("Betula verrucosa", "Crataegus crus-galli var. inermis", 
                              "Larix leptolepis", "Magnolia spp", "Magnolia rustica",
                              "Magnolia x soulangiana", "Picea pungens var. glauca",
                              "Salix matsudana", "Ulmus amurensis", "Ulmus x prospectoro",
                              "Ulmus propinqua", "Ulmus wilsoniana")) -> name_db_temp

#For the island inventory, select exclude the municipalities that did not want
#to be included OR boroughs already in the city of montreal (in mtl_inv)
print(paste0(
  "Removing data for cities that didn't want to be included in the Tree", 
      " Explorer."))
isl_inv=isl_inv[!isl_inv$nom_arr %in% c("Mont-Royal", "Hampstead", "Westmount", 
                            "Dollard-Des-Ormeaux", "Kirkland"),]
isl_inv=isl_inv[isl_inv$source!="PortailDonneesOuvertesMTL"|
                  isl_inv$nom_arr=="Lachine",]
sort(unique(isl_inv$nom_arr))

#only the relevant columns and update the names
isl_inv %>%
  left_join(name_db_temp) %>%
  mutate(frspp = frgen, 
         engspp = enggen) %>% 
  dplyr::select(ltnspp = acc_sp, ltnspp_simple = latin_simple, frspp, frgen, 
                engspp, enggen, dhp, fctgr10, csqkgyr = carbonseqkgyr, rnfm3yr = runoffm3yr, 
                plrgyr = polremgyr, longi = long, latid = lat) -> isl_inv

#Update some of the french names in the island inventory file
isl_inv[grepl("Acer saccharinum", 
              isl_inv$ltnspp),]$frspp="Érable argenté"
isl_inv[grepl("Acer saccharinum", 
              isl_inv$ltnspp),]$engspp="Silver maple"
isl_inv[grepl("Acer platanoides", 
              isl_inv$ltnspp),]$frspp="Érable de Norvège"
isl_inv[grepl("Acer platanoides", 
              isl_inv$ltnspp),]$engspp="Norway maple"
isl_inv[grepl("Fraxinus pennsylvanica", 
              isl_inv$ltnspp),]$frspp="Frêne de Pennsylvanie"
isl_inv[grepl("Fraxinus pennsylvanica", 
              isl_inv$ltnspp),]$engspp="Green ash"
isl_inv[grepl("Tilia cordata", 
              isl_inv$ltnspp),]$frspp="Tilleul à petites feuilles"
isl_inv[grepl("Tilia cordata", 
              isl_inv$ltnspp),]$engspp="Small-leaved linden"

# Reformat pdl inventory a little
pjd_inv %>% 
  dplyr::select(-engspp) %>%
  left_join(name_db[,c("Full_latin", "latin_simple", "frgen", "enggen", "engsp",
                       "fctgr10")], by = c("ltnspp" = "Full_latin")) %>%
  dplyr::select(ltnspp, ltnspp_simple = latin_simple, frspp, frgen, engspp = engsp, 
                enggen, dhp, fctgr10, csqkgyr, rnfm3yr, plrgyr, longi, latid) -> pjd_inv

#Update some of the french names in the pjd inventory file
pjd_inv[grepl("Acer saccharinum", 
              pjd_inv$ltnspp),]$frspp="Érable argenté"
pjd_inv[grepl("Acer platanoides", 
              pjd_inv$ltnspp),]$frspp="Érable de Norvège"
pjd_inv[grepl("Fraxinus pennsylvanica", 
              pjd_inv$ltnspp),]$frspp="Frêne de Pennsylvanie"
pjd_inv[grepl("Tilia cordata", 
              pjd_inv$ltnspp),]$frspp="Tilleul à petites feuilles"

#Combine all the inventories together
nObsStart=dim(isl_inv)[1]
isl_inv=rbind(mtl_inv, isl_inv, pjd_inv, cand_inv)
nObsEnd=dim(isl_inv)[1]
remove(mtl_inv)
remove(pjd_inv)

print(paste0("Total of ", nObsStart, " observations added, for a total of ", 
             nObsEnd, " observations to be uploaded to the Tree Explorer."))

#Remove NA values for longitude and latitude
isl_inv %>%
  filter(!is.na(longi)) -> isl_inv

# Change plrgyr to kg 
isl_inv %>%
  mutate(plrkgyr = plrgyr/1000) %>%
  dplyr::select(ltnspp, ltnspp_simple, frspp, frgen, engspp, enggen, dhp, 
                fctgr10, csqkgyr, rnfm3yr, plrgyr, plrkgyr, longi, latid) -> isl_inv

# Some problems with fctgr column (quick fix)
unique(isl_inv$fctgr10)

isl_inv_nofct <- isl_inv %>% filter(fctgr10 %in% c("","-"))
isl_inv_withfct <- isl_inv %>% filter(!fctgr10 %in% c("","-"))

isl_inv_nofct %>% 
  dplyr::select(-fctgr10) %>%
  left_join(name_db_temp[,c("latin_simple", "fctgr10")], by=c("ltnspp_simple"="latin_simple")) %>%
  dplyr::select(ltnspp, ltnspp_simple, frspp, frgen, engspp, enggen, dhp, 
                fctgr10, csqkgyr, rnfm3yr, plrgyr, plrkgyr, longi, latid) -> isl_inv_nofct

isl_inv <- rbind(isl_inv_withfct, isl_inv_nofct)


####EXPORT DATA####
write.csv(isl_inv, 
          paste0(pathOutput, "isl_inv_se.csv"), 
          fileEncoding = "UTF-8",
          row.names = FALSE)

#End of script