################################################################################
## 05
## Replace Candiac values
################################################################################
#date: 20220621
#author: Noemie Lacroix-D.

# Replace Candiac values to fit report results

####SETTING WORK DIRECTORY####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0.SetEnvironment.R")

####LOAD DATA####
sectors <- st_read(paste0(pathOutput, "sectors_info.shp"))
cities <- st_read( paste0(pathOutput, "cities_info.shp"))
sect_res <- read.csv(paste0(pathInput, "results_candiac_report_sectors.csv"), fileEncoding="UTF-8-BOM")
muni_res <- read.csv(paste0(pathInput, "results_candiac_report_muni.csv"), fileEncoding="UTF-8-BOM")

####DATA ANALYSIS####
cities[match(muni_res$Nom, cities$Nom), c(3:11)] <- muni_res[,c(3:11)]
sectors[match(sect_res$secteurs, sectors$secteurs), c(3:5,9:11)] <- sect_res[,c(2:7)]


####EXPORT DATA####
st_write(cities, 
         paste0(pathOutput, "cities_info.shp"), 
         layer_options = "ENCODING=UTF-8", 
         delete_layer = TRUE)

st_write(sectors, 
         paste0(pathOutput, "sectors_info.shp"), 
         layer_options = "ENCODING=UTF-8", 
         delete_layer = TRUE)

#End of script#