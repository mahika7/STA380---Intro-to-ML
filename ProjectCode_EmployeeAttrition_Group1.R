##EMPLOYEEATTRITION

rm(list = ls())

##Preliminary analysis
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
##Libraries Necessary

#--------------------------------------

box_data <- read.csv(file = 'EmployeeAttrition.csv', fileEncoding = "UTF-8-BOM", na.strings = '..')
##Creates data frame and cleans up output to read characters properly

str(box_data)

#--------------------------------------

box_dataCSA <- box_data[1:1470, -c(9, 10, 22)]
##Removes irrelevant variables and creates new data frame

str(box_dataCSA)

#--------------------------------------

box_dataCSA2 = box_dataCSA[,c(1,2)]

ggplot(data = box_dataCSA2, aes(x=Attrition,y=Age, color=Attrition)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  #geom_jitter(shape=16, position=position_jitter(0.2))+ ##Removing the # before this line enables the plotting of individual data points in box plots
  labs(title = 'Does age affect attrition?',
       y='Age',x='Attrition')

box_dataCSA3 = box_dataCSA[,c(2,6)]

ggplot(data = box_dataCSA3, aes(x=Attrition,y=DistanceFromHome, color=Attrition)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  #geom_jitter(shape=16, position=position_jitter(0.2))+ ##Removing the # before this line enables the plotting of individual data points in box plots
  labs(title = 'Does distance from home affect attrition?',
       y='Distance From Home',x='Attrition')

box_dataCSA4 = box_dataCSA[,c(2,26)]

ggplot(data = box_dataCSA4, aes(x=Attrition,y=TotalWorkingYears, color=Attrition)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  #geom_jitter(shape=16, position=position_jitter(0.2))+ ##Removing the # before this line enables the plotting of individual data points in box plots
  labs(title = 'Do total number of working years affect attrition?',
       y='Total Number of Working Years',x='Attrition')

box_dataCSA5 = box_dataCSA[,c(2,29)]

ggplot(data = box_dataCSA5, aes(x=Attrition,y=YearsAtCompany, color=Attrition)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  #geom_jitter(shape=16, position=position_jitter(0.2))+ ##Removing the # before this line enables the plotting of individual data points in box plots
  labs(title = 'Do the years they have been at the company affect attrition?',
       y='Years at Company',x='Attrition')

box_dataCSA6 = box_dataCSA[,c(2,30)]

ggplot(data = box_dataCSA6, aes(x=Attrition,y=YearsInCurrentRole, color=Attrition)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  #geom_jitter(shape=16, position=position_jitter(0.2))+ ##Removing the # before this line enables the plotting of individual data points in box plots
  labs(title = 'Do the years they have spent in their current role affect attrition?',
       y='Years in Current Role',x='Attrition')

box_dataCSA7 = box_dataCSA[,c(2,17)]

ggplot(data = box_dataCSA7, aes(x=Attrition,y=MonthlyIncome, color=Attrition)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  #geom_jitter(shape=16, position=position_jitter(0.2))+ ##Removing the # before this line enables the plotting of individual data points in box plots
  labs(title = 'Does monthly income affect attrition?',
       y='MonthlyIncome',x='Attrition')

box_dataCSA8 = box_dataCSA[,c(2,32)]

ggplot(data = box_dataCSA8, aes(x=Attrition,y=YearsWithCurrManager, color=Attrition)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  #geom_jitter(shape=16, position=position_jitter(0.2))+ ##Removing the # before this line enables the plotting of individual data points in box plots
  labs(title = 'Do the years with your current manager affect attrition?',
       y='Years W/ Current Manager',x='Attrition')



########################################################################################################

##KNN

rm(list = ls())

library(class) ## a library with lots of classification tools
library(kknn) ## knn library
library(caret)
library(e1071)
library(ROSE)

#read in and prep dataset
attrition_full_data <- read.csv('EmployeeAttrition.csv')
attrition_full_data[attrition_full_data == ''] <- NA
attrition_data <- na.omit(attrition_full_data)

#subset dataset to variables we want to include in the model
attrition_data <- attrition_data[c('Attrition', 'BusinessTravel', 'EnvironmentSatisfaction', 
                                   'JobSatisfaction', 'MaritalStatus', 'NumCompaniesWorked', 
                                   'OverTime')]

#scale non-categorical data for better distance measures in knn model
attrition_data$NumCompaniesWorked <- scale(attrition_data$NumCompaniesWorked)
attrition_data$EnvironmentSatisfaction <- scale(attrition_data$EnvironmentSatisfaction)
attrition_data$JobSatisfaction <- scale(attrition_data$JobSatisfaction)

#over sample to balance classes
over_sampled_data <- ovun.sample(Attrition~., data = attrition_data, method = "over")$data

#view pre- and post-over sampling class % frequencies
table(attrition_data$Attrition) / nrow(attrition_data)
table(over_sampled_data$Attrition) / nrow(over_sampled_data)

#k-fold cross validation, number = k-folds
trControl <- trainControl(method  = "cv", number  = 10)

set.seed(4)

#knn model - original data
fit <- train(Attrition ~.,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:100),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = attrition_data)

#knn model - over sampled data
over_sampled_fit <- train(Attrition ~.,
                          method     = "knn",
                          tuneGrid   = expand.grid(k = 1:100),
                          trControl  = trControl,
                          metric     = "Accuracy",
                          data       = over_sampled_data)

#get accuracy values and their k_values
accuracy <- fit$results$Accuracy
k_values <- fit$results$k

over_sampled_acc <- over_sampled_fit$results$Accuracy
over_sampled_k_values <- over_sampled_fit$results$k

#plot accuracy of original data
par(mfrow=c(1,1))
plot(k_values,accuracy,type="l",ylab="Accuracy",col=4,lwd=2,
     main="Employee Attrition (knn)",xlab="K - # nearest neighbors")

#plot accuracy of over sampled data
par(mfrow=c(1,1))
plot(over_sampled_k_values,over_sampled_acc,type="l",ylab="Accuracy",col=4,lwd=2,
     main="Employee Attrition - Over Sampled (knn)",xlab="K - # nearest neighbors")


#find best k
best_k <- fit$results[which.max(fit$results$Accuracy), 'k']
best_k

#find best k - over sampled
over_sampled_best_k <- fit$results[which.max(over_sampled_fit$results$Accuracy), 'k']
over_sampled_best_k

rm(list = ls())

################################################################################################################

##Decision Trees & Ensemble Methods
rm(list = ls())

##loading libraries
library(rmarkdown)
library(tinytex)
library(mice)
library(Rcpp)
library(tree)
library(randomForest)
library(gbm)
library(xgboost)
library(ROSE)
library(rpart)
library(caret)
library(dplyr)
library(ggplot2)
library(caret)
library(readr)
library(tidyr)
library(Ckmeans.1d.dp)

##load the datsets
train <- read.csv("EmployeeAttrition.csv", na.strings=c("","NA"))

##converting characters to factors for analysis and checking for null values
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], 
                                             as.factor)
count <- as.data.frame(colSums(is.na(train)))
count1 <- count[count$`colSums(is.na(train))`>0,]

##data cleaning

##removal of variables based on intution & values (constant throughout the data like employee count = 1, standard hours = 80, over18 is Y for every employee or extremely random as employee id)
## Also renaming of age variable
train1 <- train[,- which(names(train) %in% c("EmployeeCount", "StandardHours"))]
train1$Over18 <- NULL
train1$Age <- train1$ï..Age
train1$ï..Age <- NULL
train1$EmployeeNumber <- NULL

##Addition of variables based on EDA and analysis in Excel

#Hours worked per week, Average tenure of employee at previous companies, above tenure means whether an employee is above his avg tenure in current company
train1$Hoursperweek <- train1$MonthlyRate/train1$HourlyRate/7
train1$avgtenure <- train1$TotalWorkingYears/(train1$NumCompaniesWorked+1)
train1$abovetenure <- 0
for(i in 1:nrow(train1)){
  if(train1$avgtenure[i] < train1$YearsAtCompany[i]){
    train1$abovetenure[i] <- 1
  }
}
train1$abovetenure <- as.factor(train1$abovetenure)


##correlation with attrition - Chi for categorical variables and Anova for numerical variables
name <- NULL
cor <- NULL
for(i in 1:(ncol(train1)-1)){
  name[i] = colnames(train1)[i]
  if(is.factor(train1[,i])){
    cor[i] = chisq.test(train1[,i], train1$Attrition)[3]
  }
  else{
    cor[i] = summary(aov(train1[,i] ~ train1$Attrition))[[1]][["Pr(>F)"]][1]
  }
}
chi <- as.data.frame(cbind(name,cor))

chi$imp <- 0
chi$imp[chi$cor < 10^-3] <- 1
chi$imp[2] <- 0

##copying contents to another data frame
train2 <- train1[,colnames(train1) %in% chi$name[chi$imp == 1]]

##separating attrition for analysis
y <- train1$Attrition

##visualizing the data
boxplot(train2$Age ~ y)
boxplot(train2$MonthlyIncome ~ y)
boxplot(train2$TotalWorkingYears ~ y)
boxplot(train2$YearsAtCompany ~ y)
boxplot(train2$YearsInCurrentRole ~ y)
boxplot(train2$YearsWithCurrManager ~ y)
boxplot(train2$avgtenure ~ y)
boxplot(train1$PercentSalaryHike ~ y)

table(train1$Attrition, train1$BusinessTravel)
table(train1$Attrition, train1$JobInvolvement)
table(train1$Attrition, train1$JobLevel)
table(train1$Attrition, train1$JobRole)
table(train1$Attrition, train1$MaritalStatus)
table(train1$Attrition, train1$OverTime)
table(train1$Attrition, train1$StockOptionLevel)

##in case missingvalues imputation via multiple chained equations
train3 <- mice(train1, m=5, maxit = 10)
train4 <- complete(train3, 1)

##VariableImportance:
rfimp <- randomForest(Attrition ~ ., data=train4, ntree=1000,importance=TRUE)
rfImp(rfimp)
s <- as.numeric(importance(rfimp, type=2))
x <- as.data.frame(cbind(colnames(train4)[c(2:34)],s))
x$s <- as.numeric(x$s)
x <- x[order(x$s, decreasing = T),]

##Modelling

set.seed(123)

#Dividing in train and test set
sample <- sample.int(n = nrow(train4), size = floor(0.8*nrow(train4)), replace = F)
trtrain <- train4[sample, ]
trtest  <- train4[-sample, ]

#Oversampling as imbalanced data
trtrain2 <- ovun.sample(Attrition ~ ., data = trtrain, method = "over", p = 0.4)$data

#Data for modelling using subset of features
trtrain3 <- trtrain2[, c('Attrition', x$V1[1:20])]
trtest3 <- trtest[,c('Attrition', x$V1[1:20])]

#Trees - All variables, just to understand lower bound of accuracy
train_control1 = rpart.control(method = "cv", number = 10, repeats = 5)
set.seed(123)
treemodel1 <- rpart(Attrition ~ .,data = trtrain2, method = "class", control = train_control1)

predict1 <- predict(treemodel1, trtest, type = "class")
cm <- table(trtest$Attrition, predict1)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#Trees - Subset of variables
train_control2 = rpart.control(method = "repeatedcv", number = 10, repeats = 5)
set.seed(123)
treemodel2 <- rpart(Attrition ~ .,data = trtrain3, method = "class", control = train_control2)

predict2 <- predict(treemodel2, trtest3, type = "class")
cm <- table(trtest3$Attrition, predict2)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#randomforest - all variables
train_Control3 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid <- expand.grid(.mtry = c(1:36), .ntree = c(100,500, 1000))
set.seed(123)
rfmodel1 <- randomForest(Attrition ~ .,data = trtrain2, method = "class", control = train_control3, tuneGrid = tunegrid, classwt = c("No" = 5, "Yes" = 1))

predict3 <- predict(rfmodel1, trtest, type = "class")
cm <- table(trtest$Attrition, predict3)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#randomforest - subset of variables
train_Control4 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid <- expand.grid(.mtry = c(1:36), .ntree = c(100,500,1000))
set.seed(123)
rfmodel2 <- randomForest(Attrition ~ .,data = trtrain3, method = "class", control = train_control4, tuneGrid = tunegrid, classwt = c("No" = 5, "Yes" = 1))

predict4 <- predict(rfmodel2, trtest3, type = "class")
cm <- table(trtest3$Attrition, predict4)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#XGboosting - all variables with encoding all categorical variables into separate level variables into a sparse matrix (hyperparameter values based on manual trial and tests)
tr_data <- data.matrix(trtrain2[,-1])
tr_label <- trtrain2$Attrition
test_data <- data.matrix(trtest[,-1])
test_label <- trtest$Attrition
new_tr <- model.matrix(~.+0,data = trtrain2[,-1])
new_test <- model.matrix(~.+0,data = trtest[,-1])
new_labtr <- as.numeric(tr_label)-1
new_labte <- as.numeric(test_label)-1

dtrain <- xgb.DMatrix(data = new_tr, label = new_labtr)
set.seed(123)
xgb1 <- xgboost(data = dtrain, max.depth = 10, eta = 0.1, nthread = 2, nrounds = 1000, verbose = 1, objective = "binary:logistic")

dtest <- xgb.DMatrix(data = new_test, label = new_labte)
pred1<- predict(xgb1, dtest)
xgbpred1 <- ifelse (pred1 > 0.8,1,0)
cm <- table(new_labte, xgbpred1)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

#boost - subset (hyperparameter values based on manual trial and tests)
tr_data <- data.matrix(trtrain3[,-1])
tr_label <- trtrain3$Attrition
test_data <- data.matrix(trtest3[,-1])
test_label <- trtest3$Attrition
new_tr <- model.matrix(~.+0,data = trtrain3[,-1])
new_test <- model.matrix(~.+0,data = trtest3[,-1])
new_labtr <- as.numeric(tr_label)-1
new_labte <- as.numeric(test_label)-1

dtrain <- xgb.DMatrix(data = new_tr, label = new_labtr)
set.seed(123)
xgb1 <- xgboost(data = dtrain, max.depth = 2, eta = 0.1, nthread = 2, nrounds = 1000, verbose = 1, objective = "binary:logistic")

dtest <- xgb.DMatrix(data = new_test, label = new_labte)
pred2<- predict(xgb1, dtest)
xgbpred2 <- ifelse (pred2 > 0.8,1,0)
cm <- table(new_labte, xgbpred2)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

##cross validation on xgb model for all variables

tr_data <- data.matrix(trtrain2[,-1])
tr_label <- trtrain2$Attrition
test_data <- data.matrix(trtest[,-1])
test_label <- trtest$Attrition
new_tr <- model.matrix(~.+0,data = trtrain2[,-1])
new_test <- model.matrix(~.+0,data = trtest[,-1])
new_labtr <- as.numeric(tr_label)-1
new_labte <- as.numeric(test_label)-1

dtrain <- xgb.DMatrix(data = new_tr, label = new_labtr)

xgb_grid_1 = expand.grid(nrounds = 1000,eta = c(0.2, 0.1, 0.01, 0.001), max_depth = c(2,3, 4, 6, 8, 10), gamma = 0, colsample_bytree = 1,
                         min_child_weight = 1,subsample = 1)

xgb_trcontrol_1 = trainControl(method = "cv",number = 5,verboseIter = TRUE,returnResamp = "all", classProbs = TRUE,
                               summaryFunction = twoClassSummary,allowParallel = TRUE)
set.seed(123)
xgb_train_1 = train(
  x = as.matrix(new_tr),
  y = as.factor(tr_label),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

##plot to check ROC estimation for hyperparameter tuning
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

##prediction using cv model
pred6<- predict(xgb_train_1, new_test)
cm <- table(new_labte, pred6)
accuracy <- sum(diag(cm))/sum(cm)
accuracy

##feature importance in final model
importance <- xgb.importance(feature_names = colnames(new_tr), model = xgb_train_1$finalModel)
xgb.ggplot.importance(importance, rel_to_first = TRUE, xlab = "Relative Importance")


################################################################################################################

##Regression - Lasso
rm(list = ls())

library(infotheo)
library(dplyr)
library(ggplot2)
library(corrplot)
library(glmnet)

input_data = read.csv('EmployeeAttrition.csv', header = T)

## remove dummy variables

# empolyee count, Ober18, Standard hours
input_data = input_data[, -c(9, 22, 27)]

num_obs = dim(input_data)[1]

X = input_data[,-2]
Y = input_data[,2]

# fix dataframe types
categorical_var_indices = c(2, 4, 6, 7, 9, 10, 12, 13, 14, 
                            15, 16, 20, 22, 23, 24, 27)

for (i in categorical_var_indices){
  X[,i] = as.factor(X[,i])
  X[is.na(X[,i]),i] <- mode(X[,i])
}
for (i in seq(dim(X)[2])){
  if(!i %in% categorical_var_indices)
  {
    X[,i] = as.numeric(X[,i])
    X[is.na(X[,i]),i] <- median(X[,i], na.rm = T)
  }
}

Y = as.factor(Y)

### EDA
input_data %>% 
  ggplot(aes(x = Attrition, y = MonthlyIncome, color = Attrition)) + 
  geom_boxplot() + ylab("Monthly  Income($)")
input_data %>% 
  ggplot(aes(x = Attrition, y = YearsAtCompany, color = Attrition)) + 
  geom_boxplot() + ylab("Years at Current Company")
input_data %>% 
  ggplot(aes(x = Attrition, y = YearsWithCurrManager, color = Attrition)) + 
  geom_boxplot() + ylab("Years with Current Manager")

mi = data.frame(var = character(), score = double())

for (i in 1:dim(X)[2])
{
  if(i == 3) next
  new_data = data.frame(var = names(X)[i], score = mutinformation(X[,i], Y))
  mi = rbind(mi, new_data)
}

### Categorical data correlation

n = length(categorical_var_indices)

high_corr = data.frame(var1 = character(),
                       var2 = character(),
                       p_value = double())
for (i in seq(n))
{
  for (j in seq(i+1, n))
  {
    if(j > n) next
    if(i > n) next
    
    a = categorical_var_indices[i]
    b = categorical_var_indices[j]
    chi <- chisq.test(table(X[,c(a, b)]))
    if(chi$p.value < 0.05)
    {
      new_data = data.frame(var1 = names(X)[a],
                            var2 = names(X)[b],
                            p_value = chi$p.value)
      high_corr = rbind(high_corr, new_data)
    }
  }
}

### numerical correlation

var_count = dim(X)[2]
p = dim(X)[2]
for (i in seq(p))
{
  for (j in seq(i+1,p))
  {
    if(j > p)
    {
      next
    }
    if ((i %in% categorical_var_indices) | (j %in% categorical_var_indices))
    {
      next
    }
    pearson_corr <- cor.test(X[,i], X[,j], alternative = "two.sided")
    if(!is.na(pearson_corr$p.value))
    {
      if(pearson_corr$p.value < 0.01)
      {
        new_data = data.frame(var1 = names(X)[i],
                              var2 = names(X)[j],
                              p_value = pearson_corr$p.value)
        high_corr = rbind(high_corr, new_data)
      }
    }
  }
}

processed_data = cbind(X,Y)

#train_size = 0.75
train_indices = c(1:1029)

wt = rep(1, length(train_indices))
wt[Y[train_indices] == "Yes"] = 3

decision_boundary = 0.3

######### logistic fitting - baseline ################
glm.fit = glm(Y ~ ., data = processed_data, family = "binomial", 
              subset = train_indices)
glm.probs <- predict(glm.fit,type = "response", 
                     newdata = processed_data[-train_indices,])
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + .2, y=-0.05, 
           label="Decision Boundary", color="black")

######## logistic fitting - with different class weights ###############


glm.fit1 = glm(Y ~ ., data = processed_data[train_indices,], 
               family = "binomial", weights = wt)
glm.probs <- predict(glm.fit1,type = "response", 
                     newdata = processed_data[-train_indices,])
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + 0.2, y=-0.05, 
           label="Decision Boundary", color="black")


######## logistic fitting - lasso ###############

alpha = 1
lambda = 0.01
lasso_model_matric = model.matrix(~ .,data = X)

glm.fit2 = glmnet(lasso_model_matric, Y, subset = train_indices, 
                  family = "binomial", alpha = alpha)
glm.probs <- predict(glm.fit2,type = "response", 
                     newx = lasso_model_matric[-train_indices,], 
                     s = lambda)
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + 0.2, y=-0.05, 
           label="Decision Boundary", color="black")
coef(glm.fit2, s = lambda)




######## lr mode with interaction ##############

###### backward without weights###########
lr_models = model.matrix(Y ~ ., data = processed_data)
lr_models = data.frame(Y, lr_models)

lr_models_train = lr_models[train_indices,]
lr_models_test = lr_models[-train_indices,]

full1 = glm(Y ~ ., data = lr_models_train, family = "binomial")
null1 = glm(Y ~ 1, data = lr_models_train, family = "binomial")

regBackward = step(full1, direction="backward", k = log(num_obs))
glm.probs <- predict(regBackward,type = "response", 
                     newdata = lr_models_test)
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + 0.2, y=-0.05, 
           label="Decision Boundary", color="black")

mat  = cor(X[,-categorical_var_indices])
corrplot(mat, method="circle")

#############backward with validation set - without interaction##########
lr_models2 = model.matrix(Y ~ ., data = processed_data)
lr_models2 = data.frame(Y, lr_models2)

lr_models_train1 = lr_models2[train_indices,]
lr_models_test1 = lr_models2[-train_indices,]

full2 = glm(Y ~ ., data = lr_models_train1, 
            family = "binomial", weights = wt)
null2 = glm(Y ~ 1, data = lr_models_train1, 
            family = "binomial", weights = wt)
regBackward1 = step(full2, direction="backward", k = log(num_obs))
glm.probs <- predict(regBackward1,type = "response", 
                     newdata = lr_models_test1)
glm.pred <- ifelse(glm.probs > decision_boundary, "Yes", "No")
table(glm.pred, Y[-train_indices])
plot_df = data.frame(glm.probs, Y[-train_indices])
plot_df %>% 
  ggplot(aes(x = glm.probs, color = Y..train_indices.)) + 
  geom_boxplot() + xlab("logistic regression output") + 
  labs(color = "Attrition") +
  geom_vline(xintercept = decision_boundary) +
  annotate(geom="text", x=decision_boundary + 0.2, y=-0.05, 
           label="Decision Boundary", color="black")

#############################################################################################
