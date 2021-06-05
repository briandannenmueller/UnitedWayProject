# Group 6 R Code
#Brian Dannenmueller, Zohaib Hussain, David Holmes, and Ryan Holt
rm(list = ls())

#read in cps
cps = read.csv("cps_stat172.csv", stringsAsFactors = T)

################################################
###-------PREPARATION---------------------------
################################################

# Call libraries needed for plotting & Color Palettes
library(ggplot2)
library(reshape2)
library(RColorBrewer)

str(cps2)
summary(cps)
# THERE ARE 160 NA's in the data that need to be removed. 
cps <- cps[!is.na(cps$fsecurity),]
# THIS SHOULD REMOVE ALL NA's from fsecurity.
# WE ALSO NEED TO REMOVE THE X.1, X, and ID rows from the dataset
cps2 = subset(cps, select = -c(X.1, X, id))

# I NEED TO CHANGE THE RESPONSE VARIABLE AND DISABILITY INTO CATEGORICAL VARIABLES
cps2$disability_cat = ifelse(cps2$disability > 0, "Disability", "No_Disability")
cps2$disability_cat = as.factor(cps2$disability_cat)
cps2$fsecurity_cat = ifelse(cps2$fsecurity > 0, "yes", "no")
cps2$fsecurity_cat = as.factor(cps2$fsecurity_cat)


################################################
###-------VISUALIZATION-------------------------
################################################

# CREATE BARPLOTS OF THE CATEGORICAL VARIABLES TO OBSERVE DISTRIBUTION OF DATA AMONG CATEGORIES
ggplot(data = cps2, aes (x = fsecurity_cat))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1)
ggplot(data = cps2, aes (x = disability_cat))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1)


# CREATE HISTOGRAMS TO SHOW PROPORTIONALITY AMONG NUMERIC VARIABLES
ggplot(data = cps2) + 
  geom_histogram(aes(x = hhsize, fill = fsecurity_cat), position = 'fill', binwidth = 1)  +  
  ggtitle("Food Security as Household Size Increase") + 
  labs(x = "Household Size", y = "Proportion") + 
  scale_fill_grey("Food Insecure") +
  theme_bw()
ggplot(data = cps2) +
  geom_histogram(aes(x = elderly, fill = fsecurity_cat), position = 'fill', binwidth = 1)  +  
  ggtitle("Food Security as Elders Within Household Increase") + 
  labs(x = "Elderly", y = "Proportion") + 
  scale_fill_grey("Food Insecure") +
  theme_bw()

ggplot(data = cps2) + 
  geom_histogram(aes(x = hhsize, fill = fsecurity_cat), position = 'fill', binwidth = 1)  +  
  ggtitle("Food Security as Household Size Increase") + 
  labs(x = "Household Size", y = "Proportion") + 
  scale_fill_grey("Food Insecure") +
  theme_bw()
ggplot(data = cps2) +
  geom_histogram(aes(x = elderly, fill = fsecurity_cat), position = 'fill', binwidth = 1)  +  
  ggtitle("Food Security as Elders Within Household Increase") + 
  labs(x = "Elderly", y = "Proportion") + 
  scale_fill_grey("Food Insecure") +
  theme_bw()

################################################
###-------CLUSTERING----------------------------
################################################

cps_X = subset(cps2, select = -c(fsecurity_cat,disability_cat, fsecurity, disability))
# WE ALSO NEED TO STANDARDIZE THE VARIABLES IN ORDER TO LIMIT CONTROL, KEEP IT 
# EVEN BETWEEN ALL.
cps_stand = apply(cps_X, 2, function(x){(x - mean(x))/sd(x)})
summary(cps_stand)  
wss = (nrow(cps_stand)-1)*sum(apply(cps_stand,2,var))
for (i in 2:15) {
  wss[i] = sum(kmeans(cps_stand, centers = i)$withinss)}
plot(1:15, wss, type = "b", xlab = "Number of clusters", main = "Elbow Plot")
# MAKES ME THINK THAT 4 WOULD BE A GOOD POINT
# WE WILL NOW DO HIERARCHICAL CLUSTERING
cps_dist = dist(cps_stand, method = "euclidean")
cps_clust = hclust(cps_dist, method = "ward.D")
plot(cps_clust)

# This creates red rectangles to create clusters for the dendrogram
rect.hclust(cps_clust, k = 4, border = "red")
cps_kmeans = kmeans(cps_stand, 4)
str(cps_kmeans)
cps_X$km_cluster = as.factor(cps_kmeans$cluster)
cps_long = melt(cps_X, id.vars = c("km_cluster"))
View(cps_long)

# This will create boxplots for the clusters among the specified variables in cps_X
ggplot(data = cps_long) + 
  geom_boxplot(aes (x = km_cluster, y = value, fill = km_cluster)) +
  facet_wrap(~variable, scales = "free") + 
  scale_fill_brewer(palette = "Blues")
display.brewer.all(colorblindFriendly = T)
# I WILL NOW CREATE A HEATMAP
cps2$groups = as.factor(cutree(cps_clust, k = 4))
rownames(cps_stand) = paste(cps2$groups, ": ", cps$Country)
heatmap(as.matrix(cps_stand),
        col = paste("grey", 1:99, sep = ""),
        hclustfun = function(x){hclust(x, method = "ward.D")})



################################################
###-------Decision Tree-------------------------
################################################

library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)

cps2 = subset(cps2, select = -c(fsecurity, disability))

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
             control = rpart.control(cp = 0.00001, minsplit = 1))

# this full tree visualization might take a while to run, FYI
rpart.plot(tree, box.palette = "Blues")

printcp(tree)
# Looks like the lowest xerror will be found after no split, which means decision trees cannot model
# this data. 

################################################
###-------Random Forest-------------------------
################################################

library(randomForest)
library(caret)

#Next, we will try a random forest and see if we can get a useful model
set.seed(27892789)
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

#Confusion matrix:
#  no yes class.error
#no  295  10  0.03278689
#yes  34   3  0.91891892

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

set.seed(27892789)
# final forest uses mtry = 1
final_forest<- randomForest(fsecurity_cat ~ .,
                            data = train.df, 
                            ntree = 1000, 
                            mtry = 2,#based on tuning
                            importance = TRUE) # now we can make variable importance plot


#make a column of predictions on the test set
test.df$forest_pred <- predict(final_forest, test.df, type = "class")
#confusion matrix where pi* = 0.5. Forest always guesses "no"!
table(test.df$forest_pred, test.df$fsecurity_cat)

# Construct a confusion matrix visualization
# code for this courtesy of Cybernetic at 
# https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package/42940553
cm <- confusionMatrix(data = test.df$forest_pred, reference = test.df$fsecurity_cat)
cm
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Food Secure', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Not Food Secure', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Food Secure', cex=1.2, srt=90)
  text(140, 335, 'Not Food Secure', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

draw_confusion_matrix(cm)

# Confusion matrix before pi-hat optimization:
#   no yes
#no  79   7
#yes  0   0

pi_hat <- predict(final_forest, test.df, type = "prob")[,"yes"] 



View(test.df)
rocCurve <- roc(response = test.df$fsecurity_cat,
                predictor = pi_hat,
                levels = c("no", "yes"))

# AUC = 0.657. Optimized threshold is pi* = 0.008, very small! 
plot(rocCurve,print.thres = TRUE, print.auc=TRUE)

# This adjusts the predicts based on the selected pi_hat, will probably experiment more. 
# There is clearly a class imbalance problem 
test.df$forest_pred = as.factor(ifelse(pi_hat > 0.085, "yes", "no"))
table(test.df$forest_pred, test.df$fsecurity_cat)

cm2 <- confusionMatrix(data = test.df$forest_pred, reference = test.df$fsecurity_cat)
# Confusion matrix validates ROC curve result and shows improvement
# However, if we wanted to minimize our false negative rate (guessing "food secure" when people are not)
# we may need to smaller, more aggresive pi_hat. 
draw_confusion_matrix(cm2)



# most important variables are "employed", "married", "elderly", and "hhsize". "education" is the least important!
varImpPlot(final_forest, type = 1)

################################################
###-------GLM-----------------------------------
################################################

rm(list = ls())

library(boot)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(RColorBrewer)

fs <- subset(cps, select = -c(1,2,3))

# Making binary variables out of food security and disability
fs$fsecurity[fs$fsecurity > 0] <- 1

fs$disability_cat = ifelse(fs$disability > 0, "Disability", "No_Disability")
fs$disability_cat = as.factor(fs$disability_cat)



# First model with every variable
model1 <- glm(fs$fsecurity ~ fs$hhsize + fs$female + fs$kids + fs$elderly + fs$black + fs$hispanic + fs$education + fs$employed + fs$married + fs$disability_cat, family=binomial(link="logit"))
# summary(model1)
# BIC(model1)
# 296.5758

# Second model using employed, disability_cat, elderly, hhsize
model2 <- glm(fs$fsecurity ~ fs$hhsize + fs$elderly + fs$employed + fs$disability_cat, family=binomial(link="logit"))
summary(model2)
confint(model2)
BIC(model2)
# 283.8745

anova(model2, test = "Chisq")

