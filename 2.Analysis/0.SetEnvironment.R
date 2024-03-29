################################################################################
## 0
## Set environment
################################################################################
#date: 20220621
#author: Kyle T. Martins


#Path descriptions:
#Script folder: this is the folder where the R scripts are saved. The path to
#               folder is linked to Github and is separate from the overall
#               project folder

#Analyses Folder: path to the analyis folder in the main project directory

#Data Repo Folder: path to the overall database folder

#Data Folder: path to data for the project

#Path Functions: path to where the helper functions are stored

#


#Load the libraries

shhh <- function(x){suppressWarnings(suppressPackageStartupMessages(x))}

shhh(library(stringr))
shhh(library(rpostgis))
shhh(library(rgdal))
shhh(library(udunits2))
shhh(library(sf))
shhh(library(raster))
shhh(library(tidyr))
shhh(library(fasterize))
shhh(library(rpostgis))
shhh(library(terra))
shhh(library(rgrass7))
shhh(library(sp))
shhh(library(vegan))
shhh(library(getPass))
shhh(library(data.table))
shhh(library(dplyr))
shhh(library(udunits2))
shhh(library(FD))
shhh(library(plyr))
shhh(library(tidyverse))

print("Libraries loaded.")

####DEFINE FUNCTIONS####


####DATA ANALYSIS####

#This script is used to define file paths and the environment for the project

#Start by setting the working directory to the source file location
#Tis is where all the R scripts will be saved (linked to Github)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Define the project codes
projectCode="2019_HAB_UrbFrExp_001"   
geodatabaseCode=tolower("2019_HAB_UrbFrExp_001")

#Path to Analysis Folder
pathAnalyses=paste0("G:/My Drive/Github/",
                    projectCode,"/2.Analysis/")

pathDataRepo = "G:/Shared drives/Database/"

pathTreeTraits=paste0(pathDataRepo, "TreeTraits/")

pathTreeInventory=paste0(pathDataRepo, 
                         "TreeInventories/QC-MTL/inventory_2019-SENSITIVE/")

#Get the full path to the data folder
pathInput=paste0("G:/My Drive/Github/", projectCode, "/1.Input/")
pathOutput=paste0("G:/My Drive/Github/", projectCode, "/3.Output/")

print("Paths to project files defined.")

#Define path to where helper functions are stored
pathFunctions=paste0("G:/My Drive/Github/2020_HAB_HabPackg/")


#End of script#