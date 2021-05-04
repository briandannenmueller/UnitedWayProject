rm(list = ls())

#read in cps
cps = read.csv("~/Desktop/drake/STAT172/final_project/cps_stat172.csv", stringsAsFactors = T)

################################################
###-------cps PREPARATION----------------------
################################################

cps <- cps[!is.na(cps$fsecurity),]
# THIS SHOULD REMOVE ALL NA's from fsecurity.

# WANT TO LOOK AT THE VARIABLES
# SINCE THEY ARE ALL CATEGORICAL I WILL LOOK AT THE HISTOGRAMS

# NEED TO FIX THESE, AND CHANGE CERTAIN VARIABLES TO CATEGORICAL.
cps <- within(cps, {
  disability_cat <- NA # need to initialize variable
  disability_cat[disability = 0] <- "No_Disability"
  disability_cat[disability = 1] <- "Disability"
} )

cps$disability_cat <- factor(cps$disability_cat, levels = c("No_Disability", "Disability"))

cps$disability_cat = ifelse(cps$disability > 0, "Disability", "No_Disability")
cps$disability_cat = as.factor(cps$disability_cat)

cps$fsecurity_cat = ifelse(cps$fsecurity > 0, "yes", "no")
cps$fsecurity_cat = as.factor(cps$fsecurity_cat)

# Look at unique values for cps validation of each column
unique(cps$fsecurity) # NA values need to be removed and this needs to be binary
unique(cps$hhsize) # there are fractional values -- what do they mean? 

cps2 = subset(cps, select = -c(fsecurity, disability, X.1, X, id))
View(cps2)

################################################
###-------Decision Tree-------------------------
################################################

library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)

#set seed to ensure reproducibility
RNGkind(sample.kind = "default") #sets the sampling algorithm
set.seed(27892789) #number is arbitrary, just as long as we all use the same one
#train.idx will contain a random sample of row indices
train.idx = sample(x = 1:nrow(cps2), size = floor(.8*nrow(cps2)))
#make training cps
train.df = cps2[train.idx,]
#the rest will be for testing
test.df = cps2[-train.idx,] #note the negative

View(train.df)

set.seed(27892789) #again, for reproducibility
# This takes some time to run
tree = rpart(fsecurity_cat ~ .,
             data = train.df,
             method = "class",
             control = rpart.control(cp = 0.0001, minsplit = 1))

rpart.plot(tree, box.palette = "Blues")

printcp(tree)
# Looks like the lowest xerror will be found after no split, which means decision trees cannot model
# this data. 

################################################
###-------Random Forest-------------------------
################################################

library(randomForest)
#Next, we will try a random forest and see if we can get a useful model
forest = randomForest(fsecurity_cat ~ .,
                      data = train.df, #TRAINING DATA
                      ntree = 1000, #fit B = 1000 separate classification trees
                      mtry = 4, #choose m - sqrt(10) = 3.16 - i rounded up  to 4
                      importance = FALSE) #importance can help us identify important predictors (later)

forest

#Confusion matrix:
#     no yes class.error
#no  294  11  0.03606557
#yes  34   3  0.91891892

(294 + 3)/(294 + 3 + 11 + 34) #accuracy = 0.8684211
1-0.8684211 #OOB error rate = 0.1315789

# accuracy is pretty good, but we'll want to check specificity, sensitivity, false positive. 
# This scenario seems to favor false positives (falsely predict not food secure) vs false negatives since we don't want to unfairly 
# deprive people of needed assistance due to modeling errors

34 / (34 + 294) # False Positive rate = 0.1036585
294 / (294 + 34) # Specificity (true negatives) = 0.8963415
294 / (294 + 3) # sensitivity (true positives) = 0.989899

# TUNING
mtry <- c(1:10)

#make room for m, OOB error
keeps2 <- data.frame(m = rep(NA,length(mtry)),
                     OOB_err_rate = rep(NA, length(mtry)))

for (idx in 1:length(mtry)){
  tempforest<- randomForest(fsecurity_cat ~ .,
                            data = train.df, 
                            ntree = 1000, 
                            mtry = mtry[idx]) #mtry is varying
  #record iteration's m value
  keeps2[idx, "m"] <- mtry[idx]
  #record what our OOB error rate was
  keeps2[idx,"OOB_err_rate"] <- mean(predict(tempforest)!= train.df$fsecurity_cat)
  
}

# interestingly enough, OOB error rate is lowest on mtry = 1 and 2
# OOB shoots up at mtry = 3 ... why? 
qplot(m, OOB_err_rate, geom = c("line", "point"), data = keeps2) + 
  theme_bw() + labs(x = "m (mtry) value", y = "OOB error rate")

# final forest uses mtry = 1
final_forest<- randomForest(fsecurity_cat ~ .,
                            data = train.df, 
                            ntree = 1000, 
                            mtry = 2,#based on tuning
                            importance = TRUE) # now we can make variable importance plot


################################################
###-------PREDICTION      ----------------------
################################################

#make a column of predictions on the test set
test.df$forest_pred <- predict(final_forest, test.df, type = "class")
#confusion matrix where pi* = 0.5. Forest always guesses "no"!
table(test.df$forest_pred, test.df$fsecurity_cat)

# Confusion matrix before pi-hat optimization:
#   no yes
#no  79   6
#yes  0   1

pi_hat <- predict(final_forest, test.df, type = "prob")[,"yes"] 



View(test.df)
rocCurve <- roc(response = test.df$fsecurity_cat,
                predictor = pi_hat,
                levels = c("no", "yes"))

# AUC = 0.657. Optimized threshold is pi* = 0.008, very small! 
plot(rocCurve,print.thres = TRUE, print.auc=TRUE)

# This adjusts the predicts based on the selected pi_hat, will probably experiment more. 
# There is clearly a class imbalance problem 
test.df$forest_pred = ifelse(pi_hat > 0.008, "yes", "no")
table(test.df$forest_pred, test.df$fsecurity_cat)

# we would predict "no food security" 60.8% of the time that there is actually no food security (TN = .608)
# we would predict "yes" 71.4% of the time that they actually have food security. (TP = .714)
# hard to say if this was better than the standard cutoff of pi* = 0.5; we are producing false negatives
# in order to get true positives (we had none before)

# most important variables are "employed", "married", "elderly", and "hhsize". "education" is the least important!
varImpPlot(final_forest, type = 1)

