setwd("/Users/andrewhu/desktop/Kaggle")
source('function.R')
source('hw3_function.R')

library(tidyverse)
library(dplyr)

############################## Read File and create basic features

# read in the transaction file
ord = read.csv("orders.csv")
dim(ord)
head(ord)
# the date of the offer was 11/25/2014, so t is time since action
ord$t = as.numeric(as.Date("2014/11/25") - as.Date(ord$orddate, "%d%b%Y"))/365.25
summary(ord$t)
hist(ord$t)

#read in the customer file with one row per customer
customer = read.csv("customer.csv")
names(customer)
head(customer)
table(customer$train) 

# rollup order file to create RFM variables
rfm = ord %>%
  group_by(id) %>%
  summarise(tof=max(t), r = min(t), fitem=n(), ford=n_distinct(ordnum), m=sum(price*qty)) 


# this shows you how you can roll up order file counting purchases by category
cats = sort(unique(ord$category))  # list of all unique categories
cats
rfm2 = ord %>%
  group_by(id, category) %>%
  summarise(f=n()) %>%
  spread(category,f, fill=0)  %>%
  setNames(c("id", paste("f", cats, sep="")))
head(rfm2)
summary(rfm2)

#Another apporach:  Money spent  by category
cats = sort(unique(ord$category))  # list of all unique categories
cats
rfm3 = ord %>%
  group_by(id, category) %>%
  summarise(mf=sum(price*qty)) %>%
  spread(category,mf, fill=0)  %>%
  setNames(c("id", paste("mf", cats, sep="")))
head(rfm3)
summary(rfm3)

# this joins the customer, RFM and RFM by category tables
all = left_join(customer, rfm, by="id") %>%
  left_join(rfm2, by="id")
summary(all)
names(all)

#Join using original rfm and m by category

all_alt = left_join(customer,rfm,by="id") %>%
  left_join(rfm2, by="id") %>%
  left_join(rfm3, by="id")
summary(all_alt)
names(all_alt)
# Join the RFM
# Note that the dependent variable is in col 3 and the predictors 
# are in columns 4-38.


############################## create more features

### Monetary related
# recent_purchased_percentage (2013)
# sales_mean
# sales amount 2008-2009, 2010-2011, 2012-2013

ord$mydate = as.Date(ord$orddate, "%d%b%Y")
ord$year = format(ord$mydate,"%Y")


monetart_df = ord %>% group_by(id, year) %>% dplyr::summarise(sls_amt=sum(price*qty))
monetart_df2 = monetart_df %>% tidyr::spread(year,sls_amt)
monetart_df2[is.na(monetart_df2)] = 0

all_alt2 = merge(x = all_alt, y = monetart_df2, by = "id", all = TRUE)


names(all_alt2)

#Unique orders acount 2008-2009, 2010-2011, 2012-2013
yearord_df = ord %>% group_by(id, year) %>% dplyr::summarise(year_ord=n_distinct(ordnum))
yearord_df2 = yearord_df %>% tidyr::spread(year,year_ord)
yearord_df2[is.na(yearord_df2)] = 0

all_alt3 = merge(x = all_alt2, y = yearord_df2, by = "id", all = TRUE)

colnames(all_alt3)[69] <- "m_2007"
colnames(all_alt3)[70] <- "m_2008"
colnames(all_alt3)[71] <- "m_2009"
colnames(all_alt3)[72] <- "m_2010"
colnames(all_alt3)[73] <- "m_2011"
colnames(all_alt3)[74] <- "m_2012"
colnames(all_alt3)[75] <- "m_2013"
colnames(all_alt3)[76] <- "m_2014"
colnames(all_alt3)[77] <- "ord_2007"
colnames(all_alt3)[78] <- "ord_2008"
colnames(all_alt3)[79] <- "ord_2009"
colnames(all_alt3)[80] <- "ord_2010"
colnames(all_alt3)[81] <- "ord_2011"
colnames(all_alt3)[82] <- "ord_2012"
colnames(all_alt3)[83] <- "ord_2013"
colnames(all_alt3)[84] <- "ord_2014"

all3= all_alt3

all3$m_per_order= (all3$m / all3$ford)
all3$tof_per_item = (all3$tof/ all3$fitem)
all3$tof_per_order =(all3$tof/all3$ford)

############################## transformation
# This command logs all of the predictor variables. You may want to try
# other transformations, or not transform some variables at at all.
#

for(i in 4:84) all3[[i]] = log(all3[[i]]+1)
#summary(all)





############################## create train/test
train = (all3$train==1) 

train_df = all %>% filter(train==1) 
test_df = all %>% filter(train==0)



train_df2 = all2 %>% filter(train==1) 
test_df2 = all2 %>% filter(train==0)

train_df3 = all3 %>% filter(train==1) 
test_df3 = all3 %>% filter(train==0)




#Subset for mini_Train and mini_test, might not needed.
train_df3$train_alt = runif(nrow(train_df3))>.4
names(train_df3)

mini_train= train_df3 %>% filter(train_alt==1)
mini_test= train_df3 %>% filter(train_alt==0)


train_df4$train_alt = runif(nrow(train_df3))>.3
names(train_df4)

mini_train= train_df4 %>% filter(train_alt==1)
mini_test= train_df4 %>% filter(train_alt==0)


############################## try models

##### SLR using all features, all3, 0.08

fit_slr = lm(logtarg ~ ., all3[train, -c(1,2)])  # -c(1,2) drops columns 1 and 2
summary(fit_slr)



#mini
fit_slr= lm(logtarg~., mini_train[-c(1,2)])
yhat = predict(fit_slr, mini_test)
mean((mini_test$logtarg - yhat)^2)


##### SLR selected features
# tof, fitem, f1, f6, f14, f19, f20, f27, f50

fit_2_formula = as.formula("logtarg~ tof+fitem+f1+f6+f14+f19+f20+f27+f50")
fit_2 = lm(fit_2_formula, all[train, -c(1,2)])  # -c(1,2) drops columns 1 and 2
summary(fit_2)


##### SLR using all features, all3. R square: 0.08, original model
fit1 = lm(logtarg ~ ., all3[train, -c(1,2)])
summary(fit1)

###SLR using selected features, all3 R square: 0.07
fit1_formula_alt= as.formula("logtarg~f1+f14+f17+mf6+m_2014+ord_2013+ord_2014")
fit2 = lm(fit1_formula_alt, all3[train, -c(1,2)]) 
summary(fit2)


##### Lasso 0.075
y = all3 %>% filter(train==1)
y = y$logtarg
x=model.matrix(logtarg~.,all3[train, -c(1,2)])
lasso = my_cv_glmnet(y,x,1)
lasso

fit_lasso_formula = as.formula("logtarg~ fitem+f1+f14+f19+f20+f27+f50+mf6+ord_2012+ord_2013+ord_2014")
fit_lasso = lm(fit_lasso_formula, all3[train, -c(1,2)])  # -c(1,2) drops columns 1 and 2
summary(fit_lasso)
yhat_lasso <-predict(fit_lasso, all3[!train,4:84])

##### Lasso for train_df3
y = train_df3
y = y$logtarg
x=model.matrix(logtarg~.,train_df3[, -c(1,2)])
lasso = my_cv_glmnet(y,x,1)
lasso

fit_lasso_formula_alt = as.formula("logtarg~ fitem+mf20+m_2013+m_2014+ord_2012+ord_2013+ord_2014")
fit_lasso = lm(fit_lasso_formula_alt, all3[train, -c(1,2)])  # -c(1,2) drops columns 1 and 2
summary(fit_lasso)


yhat= predict(fit_lasso,all3[!train,4:84])
mean((mini_test$logtarg - yhat)^2)



#Ridge for train_df3



#Stepwise for train_df3 R square: 0.129

step(fit_slr,direct="both")
stepwise = lm(formula = logtarg ~ tof + r + ford + m + f1 + f14 + f17 + 
                f19 + f22 + f26 + f27 + f50 + mf1 + mf5 + mf6 + mf7 + mf14 + 
                mf17 + mf26 + mf35 + mf40 + mf50 + m_2010 + m_2013 + m_2014 + 
                ord_2008 + ord_2010 + ord_2012 + ord_2013 + ord_2014, data = all3[train, 
                                                                                  -c(1, 2)])
stepwise1_formula= as.formula("logtarg ~ tof + r + ford + m + f1 + f14 + f17 + 
                f19 + f22 + f26 + f27 + f50 + mf1 + mf5 + mf6 + mf7 + mf14 + 
                              mf17 + mf26 + mf35 + mf40 + mf50 + m_2010 + m_2013 + m_2014 + 
                              ord_2008 + ord_2010 + ord_2012 + ord_2013 + ord_2014")

summary(stepwise)

stepwise2= lm(formula = logtarg ~ ford + f1 + f8 + f12 + f14 + f17 + f20 + 
     f27 + f31 + f35 + f36 + f50 + mf3 + mf6 + mf7 + mf8 + mf12 + 
     mf14 + mf20 + mf31 + mf35 + mf38 + m_2010 + m_2013 + ord_2007 + 
     ord_2008 + ord_2009 + ord_2010 + ord_2011 + ord_2012 + ord_2013, 
   data = all3[train, -c(1, 2)])

stepwise2_formula= as.formula("logtarg ~ ford + f1 + f8 + f12 + f14 + f17 + f20 + 
     f27 + f31 + f35 + f36 + f50 + mf3 + mf6 + mf7 + mf8 + mf12 + 
                              mf14 + mf20 + mf31 + mf35 + mf38 + m_2010 + m_2013 + ord_2007 + 
                              ord_2008 + ord_2009 + ord_2010 + ord_2011 + ord_2012 + ord_2013")

step(fit_slr,direct="both")
step_org = as.formula("logtarg ~ tof + r + fitem + ford + f3 + f6 + f9 + 
    f14 + f17 + f22 + f26 + f27 + f39 + f40 + f50")

summary(stepwise)
summary(stepwise2)

step3<- as.formula("logtarg ~ ford + f1 + f9 + f12 + f14 + f17 + f20 + 
                        f27 + f31 + f35 + f36 + f50 + mf6 + mf7 + mf8 + mf12 + mf14 + 
                        mf20 + mf31 + mf35 + mf38 + ord_2007 + ord_2008 + ord_2009 + 
                        ord_2010 + ord_2011 + ord_2012 + ord_2013")
stepwise3 <- step(fit_slr)
summary(stepwise3)
# tof
# fitem
# ford
# f1, 6, 8, 9, 12, 14, 17, 19, 20, 22, 26, 27, 35, 41, 50, 99





########### Using new features
##### SLR using all features
formula_3 = as.formula("logtarg ~ .")
fit_3 = lm(formula_3, all2[train, -c(1,2)])  # -c(1,2) drops columns 1 and 2
summary(fit_3) # 0.0772 

##### SLR selected features
# tof, fitem, f1, f6, f14, f19, f20, f27, f50
# tof+ford+f1+f6+f14+f17+2011+2013+2014

formula_4 = as.formula("logtarg~ tof+ford+f1+f6+f14+f17+`2011`+`2013`+`2014`")
fit_5 = lm(formula_4, all2[train, -c(1,2)])  # -c(1,2) drops columns 1 and 2
summary(fit_5)


##### Lasso
y = all2 %>% filter(train==1)
y = y$logtarg
x=model.matrix(logtarg~.,all2[train, -c(1,2)])
lasso = my_cv_glmnet(y,x,1)

# r+ford+f1+f6+f14+f20+`2012`+`2013`+`2014`
formula_5 = as.formula("logtarg~ r+ford+f1+f6+f14+f20+`2012`+`2013`+`2014`")
fit_6 = lm(formula_5, all2[train, -c(1,2)])  # -c(1,2) drops columns 1 and 2
summary(fit_6)


########### Interactions

# tof, fitem, f1, f6, f14, f19, f20, f27, f50
# tof+ford+f1+f6+f14+f17+2011+2013+2014

formula_7 = as.formula("logtarg~ .+`2013`*`2014`+`2011`*`2013`")
fit_7 = lm(formula_7, all2[train, -c(1,2)])  # -c(1,2) drops columns 1 and 2
summary(fit_7) # 0.07772 


########## Try Non-linear model

### GBM
gbm_MSE = my_gbm_cv(df=all3[train, -c(1,2)],
                    response="logtarg",
                    Nrep=1,
                    best.iter=2000,
                    shrinkage=0.001,
                    interaction.depth=4,
                    cv.folds=5)
gbm_MSE
# CV MSE: [1] 0.8255


gbm_base = gbm(logtarg~., data=all3[train, -c(1,2)], distribution="gaussian", n.trees=2000, shrinkage=0.001, interaction.depth=4, bag.fraction = .5, train.fraction = 1, n.minobsinnode = 10, cv.folds = 5, verbose=FALSE) #var.monotone=rep(0,8), keep.data=TRUE
best.iter = gbm.perf(gbm_base,method="cv");best.iter # get the best number of iteration
summary(gbm_base, n.trees=best.iter) # check the important features
gbm_predict = predict(gbm_base, all3[!train,-c(1,2)], n.trees=best.iter)




### RF
param_tune_rf = param_grid_rf(df=all3[train, -c(1,2)],
                                response="logtarg",
                                ntree=500,
                                Nrep=1,
                                mtry_list=c(4,5),
                                nodesize_list=c(28,29,30,31,32))

# get the oob r2 estimate using the best parameters
oobr2_list_best = my_rf_oob(df=all3[train, -c(1,2)],
                            response="logtarg",
                            mtry=param_tune_rf$mtry,
                            ntree=500,
                            nodesize=param_tune_rf$nodesize,
                            Nrep=1)
# $MSE
# [1] 0.8293
# 
# $oobr2
# [1] 0.06417226

rf_base = randomForest(logtarg~., data=all3[train, -c(1,2)], mtry=param_tune_rf$mtry, ntree = 50, nodesize=param_tune_rf$nodesize, importance = TRUE) 
rf_predict = predict(rf_base, all3[!train,-c(1,2)])



############################## choose best model CV############################################
set.seed(12345)
train_df4$cv = as.integer(runif(nrow(train_df4))*5) # K=5 random split
table(train_df4$cv)

#1 most simple using all MSE= 0.84


yhat = rep(NA, nrow(train_df3)) # set up vector for held-out predictions
# use a for loop to predict each fold after estimation using the other folds
for(i in 0:4){ fit = lm(logtarg~ . , train_df3, subset=(cv!=i))
yhat[train_df3$cv==i] = predict(fit, train_df3[train_df3$cv==i,]) }
mean((train_df3$logtarg-yhat)^2) # CV MSE


fit = lm(logtarg~., all3) # Estimate with all data
mean(fit$residuals^2) # compare with MSE using all data


#1-2: Most simple using selected  MSE=0.83
yhat = rep(NA, nrow(train_df3)) # set up vector for held-out predictions
# use a for loop to predict each fold after estimation using the other folds
for(i in 0:4){ fit = lm(logtarg ~ ford+f1+mf19+ mf20+mf27+mf35+m_2010+m_2013+ord_2007+ord_2008+ord_2011+ord_2012+ord_2013,train_df3, subset=(cv!=i))
yhat[train_df3$cv==i] = predict(fit, train_df3[train_df3$cv==i,]) }
mean((train_df3$logtarg-yhat)^2) # CV MSE


fit = lm(logtarg~., all3) # Estimate with all data
mean(fit$residuals^2) # compare with MSE using all data

#2 Lasso MSE= 0.815 ********

yhat = rep(NA, nrow(train_df3)) # set up vector for held-out predictions
# use a for loop to predict each fold after estimation using the other folds
for(i in 0:4){ fit = lm(logtarg~ fitem+mf20+m_2013+m_2014+ord_2012+ord_2013+ord_2014, train_df3, subset=(cv!=i))
yhat[train_df3$cv==i] = predict(fit, train_df3[train_df3$cv==i,]) }
mean((train_df3$logtarg-yhat)^2) # CV MSE



yhat = rep(NA, nrow(train_df4)) # set up vector for held-out predictions
# use a for loop to predict each fold after estimation using the other folds
for(i in 0:4){ fit = lm(logtarg~ fitem+f27+mf20+ord_2012+ord_2013+ord_2014, train_df4, subset=(cv!=i))
yhat[train_df4$cv==i] = predict(fit, train_df4[train_df4$cv==i,]) }
mean((train_df4$logtarg-yhat)^2) # CV MSE

#3. Stepwise MSE= 0.82, 0.817

yhat = rep(NA, nrow(train_df)) # set up vector for held-out predictions
# use a for loop to predict each fold after estimation using the other folds
for(i in 0:4){ fit = lm(formula = logtarg ~ ford + f1 + f14 + f19 + f20 + f27 + f35 + 
                          mf6 + mf19 + mf20 + mf27 + mf35 + mf38 + mf41 + m_2010 + 
                          m_2011 + m_2013 + ord_2007 + ord_2008 + ord_2009 + ord_2011 + 
                          ord_2012 + ord_2013 + cv + f31 + mf99 + f10, train_df3, subset=(cv!=i))
yhat[train_df3$cv==i] = predict(fit, train_df3[train_df3$cv==i,]) }
mean((train_df3$logtarg-yhat)^2) # CV MSE


#Stepwise 2
yhat = rep(NA, nrow(train_df3)) # set up vector for held-out predictions
# use a for loop to predict each fold after estimation using the other folds
for(i in 0:4){ fit = lm(formula = logtarg ~ ford + f1 + f8 + f12 + f14 + f17 + f20 + 
                          f27 + f31 + f35 + f36 + f50 + mf3 + mf6 + mf7 + mf8 + mf12 + 
                          mf14 + mf20 + mf31 + mf35 + mf38 + m_2010 + m_2013 + ord_2007 + 
                          ord_2008 + ord_2009 + ord_2010 + ord_2011 + ord_2012 + ord_2013, train_df3, subset=(cv!=i))
yhat[train_df3$cv==i] = predict(fit, train_df3[train_df3$cv==i,]) }
mean((train_df3$logtarg-yhat)^2) # CV MSE


#Stepwise 3 #0.8
simple_cv(step3,train_df4,5)


#----------------------------------------------------------

all_feature_formula = as.formula("logtarg ~ .")
my_cv(all_feature_formula, train_df, 5) # 0.8247637

my_cv(fit_2_formula, train_df, 5) # 0.8329105

my_cv(fit_lasso_formula, train_df, 5) # 0.8275806

my_cv(formula_3, train_df2, 5) # 0.8082 ***

my_cv(formula_4, train_df2, 5) # 0.8144983

my_cv(formula_5, train_df2, 5) # 0.8168515

my_cv(formula_7, train_df2, 5) # 0.8074572 *****

my_cv(fitq,train_df3,5)





my_cv(all_feature_formula,train_df3[,-c(1,2)],5) #0.799 simple formula may be misleading

my_cv(fit2_formula_alt,train_df3[,-c(1,2)],5) #0.82

simple_cv(fit_lasso_formula_alt,train_df4[,-c(1,2)],5) #0.81

my_cv(stepwise1_formula,train_df3[,-c(1,2)],5) #0.8

my_cv(stepwise2_formula,train_df3[,-c(1,2)],5) #0.799 **********












############################## Predict final result

yhat = predict(fit_3, all2[!train,])
yhat[yhat<0] = 0
length(yhat) # matches number of test cases.


# the sampans.csv file is what you upload to Kaggle. File format: id,predicted logtarg
yhat= predict(fit1, all3[!train,])
yhat[yhat<0]=0
write.csv(data.frame(id=all3$id[!train], logtarg=yhat), "Las.csv", row.names=F)




######## TO DO
# 試試看 logistic + linear regression ，用每個id的期望值當作最後的預測
# category: 試試看用 total_price取代count


######## Outline
##### potential features


### Other
# category?? 看有沒有哪個category會影響customer會不會買

### Interaction


##### EDA 
# 找有沒有有趣的category

##### Model
# Linear Model
# Stepwise
# Ridge/Lasso


##### Evalation
# 10-fold cv -> 決定要用哪些feature


##### Prediction





############ Try Logistic with linear

all4 = all3 
all4$logtarg_binary = ifelse(all4$logtarg>0, 1, 0)

fit_logistic = glm(logtarg_binary ~ ., family=binomial, data=all4[train, -c(1,2,3)])
summary(fit_logistic)
vif(fit_logistic)

fit_logistic2 = glm(logtarg_binary ~ tof+r+ford+sls_2008+sls_2010, family=binomial, data=all4[train, -c(1,2,3)])

fit_log_predict2 = predict(fit_logistic2, newdata=all4[!train, -c(1,2,3)], type="response")


yhat = predict(fit_7, all2[!train,])
yhat[yhat<0] = 0
length(yhat) # matches number of test cases.
final_df = data.frame(id=all2$id[!train], predict=yhat)
final_df$prob = fit_log_predict2
final_df = final_df %>% mutate(logtarg = predict*prob) %>% dplyr::select(id, logtarg)
write.csv(final_df, "sampans2.csv", row.names=F)




#Another XGBoost  for mini  

library(xgboost)
set.seed(123)
## Model parameters trained using xgb.cv function
xgbFit = xgboost(data = as.matrix(train_df3[,4:84]), nfold = 5, label = as.matrix(train_df3$logtarg[4:84]), 
                 nrounds = 2200, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", 
                 nthread = 8, eta = 0.01, gamma = 0.0468, max_depth = 6, min_child_weight = 1.7817, 
                 subsample = 0.5213, colsample_bytree = 0.4603)
## print(xgbFit)

## Predictions
preds2 <- predict(xgbFit, newdata = as.matrix(mini_test[,4:84]))
rmse(mini_test$logtarg, preds2)
mean((mini_test$logtarg - preds2)^2)



