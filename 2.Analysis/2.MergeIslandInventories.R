################################################################################
## 02
## Merge Island Inventories
################################################################################
#date: 20210526
#author: Kyle T. Martins

#Load the relevant libraries
library(sf)
library(stringr)
library(dplyr)
library(plyr)

#Add inventory data for neighboring municipalities to updated MTL inventory data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
for(i in 1:1){setwd("..")}

####DEFINE DATA PATHS####

pathTreeData=paste0("G:/My Drive/Habitat/Database/TreeInventories/QC-MTL/",
                    "inventory_2019-SENSITIVE/")

#Load the processed version of the Montreal tree database (updated in script 1)
mtl_inv=read.csv(paste0("3.Output/Temp/mtl_inv_se.csv"))
head(mtl_inv)

#Load the 2019 version of the island inventory
print("Loading MTL island dataset.")
isl_inv=read.csv(paste0(pathTreeData,
                        "montreal-island-tree-db-2019-05-28.csv"),
                 stringsAsFactors=FALSE)[,-1]
head(isl_inv)

#Load the 2021 data for Parc Jean Drapeau
pjd_inv=read.csv(paste0("1.Input/JNDRP_InvArbre_UrbFrExp.csv"))

####DATA ANALYSIS####

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
column_select=c("frgen", "enggen", "acc_sp", "dhp", "carbonseqkgyr",
                "runoffm3yr", "polremgyr", "long", "lat")
isl_inv=isl_inv[column_select]
names(isl_inv)=names(mtl_inv)
head(isl_inv)

#Update some of the french names in the island inventory file
isl_inv[grepl("Acer saccharinum", 
              isl_inv$ltnspp),]$frspp="Érable argent?"
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

#Combine the mtl and island inventory data
nObsStart=dim(isl_inv)[1]
isl_inv=rbind(mtl_inv, isl_inv, pjd_inv)
isl_inv=rbind(mtl_inv, isl_inv)
nObsEnd=dim(isl_inv)[1]
remove(mtl_inv)
remove(pjd_inv)

print(paste0("Total of ", nObsStart, " observations added, for a total of ", 
             nObsEnd, " observations to be uploaded to the Tree Explorer."))


####EXPORT DATA####

write.csv(isl_inv,
          paste0("3.Output/isl_inv_se.csv"), row.names=FALSE)

#End of script