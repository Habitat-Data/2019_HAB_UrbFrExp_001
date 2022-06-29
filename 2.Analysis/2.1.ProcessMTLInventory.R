################################################################################
## 2.1
## Process the Montreal Tree Inventory
################################################################################
#date: 20210526
#author: Kyle T. Martins

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0.SetEnvironment.R")

####LOAD THE DATA####
#Load the most recent version of the Montreal tree database
mtl_inv=read.csv(paste0(pathInput, "arbres-publics.csv"),
                 fileEncoding="UTF-8")
head(mtl_inv)

#Load the tree name database
name_db=read.csv(paste0(pathTreeTraits, "Taxonomy/Tree_Name_Database.csv"))
head(name_db)

#Load the iTree ecosystem service estimations
itree_es=read.csv(paste0(pathTreeTraits, "iTreeData/iTreeOutputSppxDBH.csv"))

####DATA ANALYSIS####

#How many observations in the original inventory
nObsStart=dim(mtl_inv)[1]

#Isolate just the relevant columns in the MTL inventory
mtl_inv=mtl_inv[c("SIGLE", "Essence_latin", "Essence_fr", "DHP", "Longitude", 
                  "Latitude")]

print(paste0(dim(mtl_inv)[1], " observations in the original MTL database." ))

#Isolate just the relevant columns in the tree name database
name_db=name_db[c("mtlcode", "itreecode", "latin_simple", "frgen", "enggen", 
                  "engsp", "fctgr10")]

#Merge the tree names into the MTL inventory
summary(mtl_inv$SIGLE %in% name_db$mtlcode)
name_db=name_db[name_db$mtlcode %in% mtl_inv$SIGLE,]
name_db=name_db[!duplicated(name_db$mtlcode),]
mtl_inv=merge(mtl_inv, name_db, by.x="SIGLE", by.y="mtlcode")

print(paste0(dim(mtl_inv)[1], " observations after merging in species names." ))

#Isolate the relevant columns in the iTree service estimate database
#Correct the formatting error for the polution removal ES estimates
head(itree_es)
itree_es=
itree_es[c("Species.Name", "itreecode", "DBH..cm.", 
           "Gross.Carbon.Sequestration..kg.yr.",
           "Avoided.Runoff..m.3.yr.", "Pollution.Removal..g.yr.")]
names(itree_es)=c("sp_eng", "itreecode", "DHP", "carbonseqkhyr", "runoffm3yr",
                  "polremgyr")
itree_es$polremgyr=as.character(itree_es$polremgyr)
itree_es$polremgyr=as.numeric(as.character(gsub(",", "", itree_es$polremgyr)))
head(itree_es)
#Remove japanese larch since duplicated
itree_es=itree_es[itree_es$sp_eng!="Japanese larch",]

#Remove AAAA observations
mtl_inv=mtl_inv[mtl_inv$SIGLE!="AAAA",]
print(paste0(dim(mtl_inv)[1], " observations after removing AAAA." ))

#Merge in iTree data

#Make corrections to the itree codes in the MTL inventory to facilitate the 
#merge
mtl_inv=mtl_inv[mtl_inv$itreecode!="" ,]
head(mtl_inv)
mtl_inv$itreecode=as.character(mtl_inv$itreecode)
itree_codes=as.character(unique(mtl_inv$itreecode))
summary(itree_codes %in% itree_es$itreecode)
missing_codes=itree_codes[!itree_codes %in% itree_es$itreecode]
mtl_inv$itreecode[mtl_inv$itreecode %in% 
                    missing_codes & nchar(mtl_inv$itreecode)>4]=
  substr(mtl_inv$itreecode[mtl_inv$itreecode %in% 
                             missing_codes & nchar(mtl_inv$itreecode)>4],1,4)
itree_codes=as.character(unique(mtl_inv$itreecode))
summary(itree_codes %in% itree_es$itreecode)
missing_codes=itree_codes[!itree_codes %in% itree_es$itreecode]

print(paste0(dim(mtl_inv)[1], " observations after correcting iTree codes." ))

#Remove NA DBH values
mtl_inv=mtl_inv[!is.na(mtl_inv$DHP),]

print(paste0(dim(mtl_inv)[1], " observations after removing NA DBH values." ))

#Round the DBH value to the closest 0.5 cm increment, make the max value 250
mtl_inv$DHP=round_any(mtl_inv$DHP, 0.5)
mtl_inv$DHP[mtl_inv$DHP>250]=250

#Add the ES estimates to the tree inventory
dim(mtl_inv)
mtl_inv=merge(mtl_inv, itree_es, by=c("itreecode", "DHP"))
dim(mtl_inv)

#Reorder the columns names
head(mtl_inv)
mtl_inv %>%
  dplyr::select(ltnspp = Essence_latin, ltnspp_simple = latin_simple, 
                frspp = Essence_fr, frgen, engspp = engsp, enggen,  dhp = DHP,
                fctgr10, csqkgyr = carbonseqkhyr, rnfm3yr = runoffm3yr, 
                plrgyr = polremgyr, longi = Longitude, latid = Latitude) -> mtl_inv

head(mtl_inv)

print(paste0(dim(mtl_inv)[1], " observations after adding ES estimates." ))

# Remove outlyers
mtl_inv %>% 
  filter(latid > 45.33 & latid < 45.7026 & longi > -73.937 & longi < -73.477) -> mtl_inv

print(paste0(dim(mtl_inv)[1], " observations after removing outlyers." ))

#How many observations retained?
nObsEnd=dim(mtl_inv)[1]
nObsDropped=nObsStart-nObsEnd
percKept=round((nObsEnd/nObsStart)*100,2)
print(paste0("In total, ", nObsDropped, " observations dropped and ", percKept, 
      "% of ", "the MTL city dataset retained."))

####EXPORT THE DATA####

write.csv(mtl_inv, paste0(pathOutput, "Temp/mtl_inv_se.csv"), 
          row.names=FALSE)


#End of script#