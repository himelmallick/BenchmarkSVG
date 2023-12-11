###################
# Clear workspace #
###################

rm(list = ls())

####################
#install libraries #
####################
# install.packages('devtools')
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("SpatialExperiment")
# 
# devtools::install_github('xzhoulab/SPARK')
# devtools::install_github("estfernan/boost")

##################
# Load libraries #
##################

library(devtools) 
library(boost)
library(data.table)
library(tidyverse)
library(mgcv)
library(MASS)
library(SpatialExperiment)
library(nnSVG)
library(Rfssa) # Load Data from GitHub 
library(httr)
library(supportR) # List Data from GitHub  
library(dplyr)
library(ggplot2) #For Plot
library(ggpubr) #For Plot


###########################################
## USER INPUT1: FOLDER and GITHUB LOCATION#
###########################################

mother_dir= "C:/1_RAship/1_Spatial_Tweedie/1_Codes/2_spatial_GAM/Sent_to_HM"

url_name = "https://github.com/Xijiang1997/BOOST-MI/blob/master/data/simulated%20data"
fldr_name="./data/simulated data"

#########################
##USER INPUT2: SCENARIOS##
#########################

patterns<-c('bc', 'gp', 'spot')
zeroProp<-as.character(c(0)) 
replicates<-as.character(seq(1,2, 1))
method_list<-c('GAM_CSIDE', 'GAM_CSIDE_2')

################
##LOAD ROUTINES#
################
setwd(paste(mother_dir,"1_subroutines", sep = "/"))
pkgmaker::source_files(paste(mother_dir,"1_subroutines", sep = "/"),'*.R')


##############################################
##LOAD AND STORE MASTER DATA LIST FROM GITHUB#
##############################################

master_list =Get_subrtn_dataprep(url_string=url_name,fldr_string=fldr_name)
write.csv(master_list, paste(mother_dir,"master_list.csv",sep = "/") , row.names=FALSE)
UID=c(apply(expand.grid(patterns,zeroProp,replicates), 1, function(x) paste0(x, collapse="_")))


####################################
# RUN THE MODELS + STORE THE RESULT#
####################################
master_output=list()

  for (var_scenario in UID) {
  
    
    master_output= rbind(master_output, Get_subrtn_SVG_benchmark(scenario_id = var_scenario, methods= method_list, ref_file=master_list))
    
  }


Output_benchmark=summary_scenario(master_output)
##############################
#CREATE A SUMMARIZATION CODE##
##############################









