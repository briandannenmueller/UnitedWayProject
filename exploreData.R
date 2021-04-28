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


# education # of people with at least associates
# everything else is just counts of groups
# treat married as categorical
# 

#TODO:
  # Change numerics to categorical
  # Clustering? 
  # Random Forest? 

cps <- cps[!is.na(cps$fsecurity),]
# THIS SHOULD REMOVE ALL NA's from fsecurity.

# WE ALSO NEED TO REMOVE THE X.1, X, and ID rows from the dataset
# THEY ARE NOT NEEDED
# SHOULD I REMOVE THEM FROM THE TRAINING DATASET OR FROM THE DATASET ENTIRELY?
cps2 = subset(cps, select = -c(X.1, X, id))

# WANT TO LOOK AT THE VARIABLES
# SINCE THEY ARE ALL CATEGORICAL I WILL LOOK AT THE HISTOGRAMS

# NEED TO FIX THESE, AND CHANGE CERTAIN VARIABLES TO CATEGORICAL.
cps2 <- within(cps2, {
  disability_cat <- NA # need to initialize variable
  disability_cat[disability = 0] <- "No_Disability"
  disability_cat[disability = 1] <- "Disability"
} )

cps2$disability_cat <- factor(cps2$disability_cat, levels = c("No_Disability", "Disability"))

cps2$disability_cat = ifelse(cps2$disability > 0, "Disability", "No_Disability")
cps2$disability_cat = as.factor(cps2$disability_cat)

cps2$fsecurity_cat = ifelse(cps2$fsecurity > 0, "yes", "no")
cps2$fsecurity_cat = as.factor(cps2$fsecurity_cat)
