
#exploratory data analysis for the below data set
mdData=GermanCredit_assgt_S18

### impute zeros to code credit purpose
mdData$NEW_CAR[is.na(mdData$NEW_CAR)] <- 0
mdData$USED_CAR[is.na(mdData$USED_CAR)] <- 0
mdData$FURNITURE[is.na(mdData$FURNITURE)] <- 0
mdData$`RADIO/TV`[is.na(mdData$`RADIO/TV`)] <- 0
mdData$EDUCATION[is.na(mdData$EDUCATION)] <- 0
mdData$RETRAINING[is.na(mdData$RETRAINING)] <- 0

dataset<-mdData[c(2:32)]

### impute median age
dataset$AGE[is.na(dataset$AGE)] <- round(mean(dataset$AGE, na.rm = TRUE))


### correct the variable type of categorical variables
cols <- c("CHK_ACCT", "RESPONSE", "FOREIGN", "TELEPHONE", "OWN_RES", "NEW_CAR", "USED_CAR", "FURNITURE", 'RADIO/TV', "SAV_ACCT"
          , "EMPLOYMENT", "MALE_DIV", "MALE_SINGLE", 'CO-APPLICANT', "PRESENT_RESIDENT", "PROP_UNKN_NONE"
          , "OTHER_INSTALL", "RENT", "JOB", "HISTORY", "EDUCATION", "RETRAINING", "MALE_MAR_or_WID", "GUARANTOR", "REAL_ESTATE"
)
dataset[cols] <- lapply(dataset[cols], factor)
sapply(dataset, class)


##Histograms of continuous variables
par(mfrow=c(2,3))
hist(dataset$DURATION, main = "Dist. of DURATION")
hist(dataset$AMOUNT, main = "Dist. of AMOUNT")
hist(dataset$INSTALL_RATE, main = "Dist. of INST. RATE")
hist(dataset$AGE, main = "Dist. of AGE")
hist(dataset$NUM_CREDITS, main = "Dist. of NUM_CRED")
hist(dataset$NUM_DEPENDENTS, main = "Dist. of DEPENDENTS")

##Summary of continuous variables
install.packages("pastecs")
library(pastecs)
hs0<-as.data.frame(colnames(dataset)) 
attach(hs0)
num_vars <-cbind(dataset$DURATION, dataset$AMOUNT, dataset$INSTALL_RATE, dataset$AGE, 
                 dataset$NUM_CREDITS,dataset$NUM_DEPENDENTS)
stat.desc(num_vars, basic = F)

### frequencies of categorical variables
library(plyr)
count(dataset, 'CHK_ACCT')
count(dataset, 'HISTORY')
count(dataset, 'SAV_ACCT')
count(dataset, 'EMPLOYMENT')
count(dataset, 'PRESENT_RESIDENT')
count(dataset, 'JOB')

(count(dataset, 'NEW_CAR'))
(count(dataset, 'USED_CAR'))
(count(dataset, 'FURNITURE'))
(count(dataset$`RADIO/TV`))
(count(dataset, 'EDUCATION'))
(count(dataset, 'RETRAINING'))


library(ggplot2)
dat <- data.frame(table(dataset$FOREIGN,dataset$RESPONSE))
names(dat) <- c("FOREIGN","RESPONSE","Count")
ggplot(data=dat, aes(x=FOREIGN, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

dat <- data.frame(table(dataset$HISTORY,dataset$RESPONSE))
names(dat) <- c("HISTORY","RESPONSE","Count")
ggplot(data=dat, aes(x=HISTORY, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

### Correlation Matrix
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
xvars <- dataset[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)]
chart.Correlation(xvars, histogram=TRUE, pch=19)
summachart.Correlation

----------------------------------------------------------------------------------------------------------
### LOGISTIC REGRESSION
model <- glm(formula = dataset$RESPONSE ~ ., family = binomial(link = "logit"), 
             data = dataset)
summary(model)

-----------------------------------------------------------------------------------------------------------

## Decision tree build for entire dataset
### Decision tree with no parameters
install.packages("rpart")
library("rpart")
DT1 <- rpart(RESPONSE ~., data = dataset , method="class") 
print(DT1)
summary(DT1)
plot(DT1, uniform = TRUE)
text(DT1, use.n=TRUE, all=TRUE, cex=.7)
### ???
rpart.plot::prp(DT1, type=2, extra=2)  ## type = info display, extra = zoom

### Decison tree with varying different parameters - 1
DT2<- rpart(RESPONSE ~., data = dataset , method="class", maxdepth = 15, 
            minsplit = 15, xval = 10, cp=.01, parms = list(split = 'information'))
print(DT2)
summary(DT2)

###Lift
predTrnProb=predict(DT2, dataset, type='prob')
head(predTrnProb)
trnSc <- subset(dataset, select=c("RESPONSE"))
trnSc$score<-predTrnProb[, 1]
trnSc<-trnSc[order(trnSc$score, decreasing=TRUE),]
str(trnSc)
trnSc$RESPONSE<-as.numeric(as.character(trnSc$RESPONSE))
str(trnSc)
trnSc$cumDefault<-cumsum(trnSc$RESPONSE)
head(trnSc)
plot(seq(nrow(trnSc)), trnSc$cumDefault,type = "l", xlab='#cases', ylab='#default')
library(dplyr)
trnSc["bucket"]<-ntile(-trnSc[,"score"], 10)  
decGroups<-group_by(trnSc, bucket)
decLifts<-summarise(decGroups, count=n(), numDefaults=sum(RESPONSE))
decLifts<-decLifts %>% mutate(defRate=numDefaults/count, cumDefRate=cumsum(numDefaults)/cumsum(count),lift=cumDefRate/(sum(numDefaults)/sum(count)) )
plot(decLifts$bucket, decLifts$lift, xlab="deciles", ylab="Cumulative Decile Lift", type="l")
barplot(decLifts$numDefaults, main="numDefaults by decile", xlab="deciles")
rm(trnSc)
### Decision tree varying different parameters - 2
DT3<- rpart(RESPONSE ~., data = dataset , method="class", maxdepth = 15, 
            minsplit = 40, xval = 10, parms = list(split = 'gini'))
print(DT3)
summary(DT3)

### Decision tree varying different parameters - 3
DT4<- rpart(RESPONSE ~., data = dataset , method="class", maxdepth = 5, 
            minsplit = 50, xval = 15, cp= .02, parms = list(split = 'information'))
print(DT4)
summary(DT4)

#Obtain the model's predictions on the entire dataset
predTrn_whole=predict(DT4, data=dataset, type='class')
#Confusion table
table(pred = predTrn_whole, true=dataset$RESPONSE)
#Accuracy
mean(predTrn_whole==dataset$RESPONSE)



### LIFT CHARTS
library(dplyr)

#get the 'scores' from applying the model to the data
predProb=predict(DT2, dataset, type='prob')
head(predTrnProb)

### lets make a table with response and score for each observation

#we need the score and actual class (OUTCOME) values
Scores <- subset(dataset, select=c("RESPONSE"))  # selects the OUTCOME column into trnSc
Scores$score<-predProb[, 1]  #add a column named 'Score' with prob values

Scores ## first glace, does not look very accurate, lets sort
Scores<-Scores[order(Scores$score, decreasing=TRUE),]

## Accuracy....
pred <- predict(DT2, dataset, type='class')
A1 <- mean(pred==dataset$RESPONSE)
str(Scores)
Scores$cumDefault<-cumsum(Scores$RESPONSE)
head(Scores)

### polt curve
plot(seq(nrow(Scores)), Scores$cumDefault,type = "l", xlab='#cases', ylab='#default')

### Divide the data into 10 (for decile lift) equal groups
# this creates a new column with group number for each row
Scores["bucket"]<-ntile(-Scores[,"score"], 10)  
-------------------------------------------------------------------------------------------
#split the data into training and test(validation) sets 
set.seed(123)
nr=nrow(dataset)
trnIndex = sample(1:nr, size = round(0.8*nr), replace=FALSE)
mdTrn=dataset[trnIndex,]   #training data with the randomly selected row-indices
mdTst = dataset[-trnIndex,]  #test data with the other row-indices
dim(mdTrn) 
dim(mdTst)

### Model1 TRAIN

Model1<- rpart(RESPONSE ~., data = mdTrn , method="class", maxdepth = 15, 
               minsplit = 15, cp=.01, parms = list(split = 'information'))
Model2<- rpart(RESPONSE ~., data = mdTrn , method="class", maxdepth = 10, 
               minsplit = 20, cp=.02, parms = list(split = 'information'))
Model3<- rpart(RESPONSE ~., data = mdTrn , method="class", maxdepth = 20, 
               minsplit = 50, cp=.0001, parms = list(split = 'information'))
Model4<- rpart(RESPONSE ~., data = mdTrn , method="class", maxdepth = 30, 
               minsplit = 150, cp=.01, parms = list(split = 'information'))

### Model1 Evaluation
predTest=predict(Model1, mdTst, type='class')
table(pred = predTest, true=mdTst$RESPONSE)
mean(predTest==mdTst$RESPONSE)

### Performance Measures 
cm <- table(pred=predict(Model1,mdTst, type="class"), true=mdTst$RESPONSE)
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1


### ROC and AUC
#score test data set
install.packages("ROCR")
library(ROCR)

mdTst$score<-predict(Model1,type='prob',mdTst)
pred<-prediction(mdTst$score[,2],mdTst$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

##AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

### ROC
install.packages("pROC")
install.packages("OptimalCutpoints")
library(pROC)
library(OptimalCutpoints)

rocobj <- plot.roc(mdTst$RESPONSE, mdTst$score[,2], percent = TRUE, main="ROC", col="#1c61b6", add=FALSE)

mdTst$RESPONSENEW <- as.character(mdTst$RESPONSE)
mdTst$Score2 <- as.numeric(mdTst$score[,2])

plot(rocobj)

optimal.cutpoint.Youden <- optimal.cutpoints(X = mdTst$Score2, status = mdTst$RESPONSENEW, tag.healthy = 0,
                                             methods = "Youden", data = mdTst, pop.prev = NULL,
                                             control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, 
                                             trace = FALSE)
summary(optimal.cutpoint.Youden)
plot(optimal.cutpoint.Youden)


### c50 package
install.packages("C50")
library(C50)
set.seed(123)

tree1 <- C5.0(RESPONSE~., data=mdTrn, method="class")
summary(tree1)
# plot(tree1)
tree1pred <- predict.C5.0(tree1, newdata = mdTst[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)], type="class")
table(pred = tree1pred, true=mdTst$RESPONSE)
mean(tree1pred==mdTst$RESPONSE)


tree2 <- C5.0(RESPONSE~., data=mdTrn, method="class", subset = TRUE, winnow = TRUE, rules = TRUE)
tree2pred <- predict.C5.0(tree2, newdata = mdTst[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)], type="class")
table(pred = tree2pred, true=mdTst$RESPONSE)
mean(tree2pred==mdTst$RESPONSE)
summary(tree2)
C5imp(tree2)

tree3 <- C5.0(RESPONSE~., data=mdTrn, method="class", winnow = FALSE, rules = TRUE,  earlyStopping = TRUE, CF = .00001, miniCases = 10, seed = sample.int(4096, size = 1) -
                1L)
#summary(tree3)
tree3pred <- predict.C5.0(tree3, newdata = mdTst[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)], type="class")
table(pred = tree3pred, true=mdTst$RESPONSE)
mean(tree3pred==mdTst$RESPONSE)

C5.0Control(subset = TRUE, bands = 0, winnow = FALSE,
            noGlobalPruning = FALSE, , minCases = 2,
            fuzzyThreshold = FALSE,, seed = sample.int(4096, size = 1) -
              1L, earlyStopping = TRUE, label = "outcome")

### ROC and AUC
#score test data set
install.packages("ROCR")
library(ROCR)

mdTst$score<-predict(tree2,type='prob',mdTst)
pred<-prediction(mdTst$score[,2],mdTst$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

##AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]



### Alternate Thresholds
CTHRESH=0.6
predProbTrn=predict(Model1, dataset, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[,'1'] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=dataset$RESPONSE)
#Accuracy
mean(predTrn==dataset$RESPONSE)

### Cost Matrix FOR WRONG PREDICTIONS
costMatrix <- matrix(c(0,1,5, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
costMatrix

rpTree = rpart(RESPONSE ~ ., data=mdTrn, method="class", parms = list( prior = c(.70,.30), 
                                                                       loss = costMatrix, split = "information"))
summary(rpTree)
#Obtain the model's predictions on the training data
predTrn=predict(rpTree, mdTrn, type='class')
#Confusion table
table(pred = predTrn, true=mdTrn$RESPONSE)
#Accuracy
mean(predTrn==mdTrn$RESPONSE)


#ROC & AUC curves
mdTst1 <- mdTst
library(ROCR)
#score test data set
mdTst1$score<-predict(rpTree,type='prob',mdTst1)
predTst<-prediction(mdTst1$score[,2],mdTst1$RESPONSE)
perf <- performance(predTst,"lift","rpp")
plot(perf, main="lift curve",colorize=T)

##ROC curve : 
perf3 <- performance(predTst,"tpr","fpr")
plot(perf3,colorize=T)
summary(perf3)

#AUC vaue
auc.perf = performance(predTst, measure = "auc")
auc.perf@y.values 

#optimal cutoff
ost.perf = performance(predTst, "cost")
predTst@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


## and then performance 

#BUILD A MODEL ON DT2 WITH Cost Matrix INCLUDED
DT2<- rpart(RESPONSE ~., data = mdTst , method="class", maxdepth = 15, 
            minsplit = 15, xval = 10, cp=.01, 
            parms = list(split = 'information', loss = costMatrix))

predTest=predict(DT2, dataset, type='class')
table(pred = predTest, true=dataset$RESPONSE)
mean(predTest==dataset$RESPONSE)

CTHRESH=0.6942446
predProbTrn=predict(DT2, dataset, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[,2] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=dataset$RESPONSE)
#Accuracy
mean(predTrn==dataset$RESPONSE)

#Calculate and apply the ‘theoretical’ threshold and assess performance – what do you
#notice, and how does this relate to the answer from (a) above. 

### ROC and AUC
#score test data set
install.packages("ROCR")
library(ROCR)

Cscore<-predict(DT2,type='prob',dataset)
pred<-prediction(Cscore[,2],dataset$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

##AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


#When the cost is included we find the threshold score which gives us the maximum profit on our best model

library(dplyr)

PROFITVAL=100
COSTVAL=-500

scoreTst=predict(DT2,mdTst, type="prob")[,2] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, mdTst$RESPONSE)
#check what is in prLifts ....head(prLifts)

prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`mdTst$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits)

#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))
