################################################################################
## 01
## Download MTL tree inventory data
################################################################################
#date: 20210526
#author: Kyle T. Martins

#Download the most recent version of the MTL Inventory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0.SetEnvironment.R")

pathURL=paste0("https://data.montreal.ca/dataset/",
               "b89fd27d-4b49-461b-8e54-fa2b34a628c4/resource/",
               "64e28fe6-ef37-437a-972d-d1d3f1f7d891/",
               "download/arbres-publics.csv")

download.file(pathURL, paste0(pathInput, "/arbres-publics.csv"))

#End of script#
