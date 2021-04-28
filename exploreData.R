rm(list = ls())

#each time you open R
library(randomForest) #to fit random forest
library(ggplot2) #for professional exploratory graphics
library(pROC) #for ROC curves

#read in data
data = read.csv("~/Desktop/drake/STAT172/final_project/cps_stat172.csv", stringsAsFactors = T)

################################################
###-------DATA PREPARATION----------------------
################################################

View(data)


