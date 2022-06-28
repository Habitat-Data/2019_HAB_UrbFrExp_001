################################################################################
## 03
## Run workflow
################################################################################
#date: 20210526
#author: Kyle T. Martins

#'Objective: Update the Montreal tree inventory data using lattest data from
#'the city of Montreal and from neighboring municipalities

####SETTING WOEK DIRECTORY####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0.SetEnvironment.R")


####LOAD WORKFLOW####

start_time <- Sys.time()

source(paste0(pathAnalyses, "1.DownloadMTLInventory.R"))

source(paste0(pathAnalyses, "2.1.ProcessMTLInventory.R"))

source(paste0(pathAnalyses, "2.2.ProcessCANDInventory.R"))

source(paste0(pathAnalyses, "3.MergeIslandInventories.R"))

source(paste0(pathAnalyses, "4.MergeDelim.R"))

source(paste0(pathAnalyses, "5.CalcStatistics.R"))

end_time <- Sys.time()

end_time-start_time

#End of script#