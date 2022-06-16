################################################################################
## 03
## Run workflow
################################################################################
#date: 20210526
#author: Kyle T. Martins

#'Objective: Update the Montreal tree inventory data using lattest data from
#'the city of Montreal and from neighboring municipalities

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
for(i in 1:1){setwd("..")}

####DEFINE DATA PATHS#### 

pathFunctions=c("./2.Analysis/")

####LOAD WORKFLOW####

start_time <- Sys.time()

source(paste0(pathFunctions, "0.DownloadMTLInventory.R"))

source(paste0(pathFunctions, "1.ProcessMTLInventory.R"))

source(paste0(pathFunctions, "2.MergeIslandInventories.R"))

end_time <- Sys.time()


end_time-start_time

#End of script#

