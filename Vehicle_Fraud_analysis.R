##clusters as variables
setwd("E:/1/Travellers/all")
train = read.csv("uconn_comp_2018_train.csv")
test = read.csv("uconn_comp_2018_test.csv")

### variable data type ##
dim(train)
dim(test)
str(train)
train[train$fraud==-1,"fraud"] = 0
train$fraud = as.factor(train$fraud)
predictor = train$fraud
summary(train$fraud)
names(train)
names(test)

# combined = rbind.data.frame(train[,!(names(train) %in% "fraud")],test)
train$marital_status = as.factor(train$marital_status)
train$claim_date = as.Date(train$claim_date,"%m/%d/%Y")
train$witness_present_ind = as.factor(train$witness_present_ind)
train$high_education_ind = as.factor(train$high_education_ind)
train$policy_report_filed_ind = as.factor(train$policy_report_filed_ind)
##impute zero zip_code
train$zip_code = ifelse(train$zip_code =="0",NA,train$zip_code)
train$zip_code = as.factor(train$zip_code)
train$address_change_ind = as.factor(train$address_change_ind)
##### split date into year,month,day
train$Year = as.factor(format(train$claim_date,"%Y"))
train$Month = as.factor(format(train$claim_date,"%m")) 
train$Day = as.factor(format(train$claim_date,"%d"))


#remove date
train = train[,!(names(train) %in% c("claim_date"))]

#check for missing values
sum(is.na(train))
#Missing values
missing_values = data.frame(count = apply(train,2,function(x){sum(is.na(x))}))
missing_values$prop = round(missing_values$count*100/nrow(train),2)

#treat missing values - mean,mode imputation
library(DMwR)
train = centralImputation(train)


##state generation
library(zipcode)
data(zipcode)
zips = zipcode[,c(1,3)]
names(zips)[1]="zip_code"

train =merge(train,zips,by="zip_code",all.x = T)
train = train[,!(names(train) %in% c("zip_code"))]
train$state = as.factor(train$state)
##impute age outliers
train$age_of_driver = ifelse(train$age_of_driver>100,
                                mean(train$age_of_driver[train$age_of_driver<100]),
                                train$age_of_driver)
apply( train[,quantitative], 2, summary)

############ test ####################
test$marital_status = as.factor(test$marital_status)
test$claim_date = as.Date(test$claim_date,"%m/%d/%Y")
test$witness_present_ind = as.factor(test$witness_present_ind)
test$high_education_ind = as.factor(test$high_education_ind)
test$policy_report_filed_ind = as.factor(test$policy_report_filed_ind)
##impute zero zip_code
test$zip_code = ifelse(test$zip_code =="0",NA,test$zip_code)
test$zip_code = as.factor(test$zip_code)
test$address_change_ind = as.factor(test$address_change_ind)
##### split date into year,month,day
test$Year = as.factor(format(test$claim_date,"%Y"))
test$Month = as.factor(format(test$claim_date,"%m")) 
test$Day = as.factor(format(test$claim_date,"%d"))


#remove date
test = test[,!(names(test) %in% c("claim_date"))]

#check for missing values
sum(is.na(test))
#Missing values
missing_values = data.frame(count = apply(test,2,function(x){sum(is.na(x))}))
missing_values$prop = round(missing_values$count*100/nrow(test),2)

#treat missing values - mean,mode imputation
library(DMwR)
test = centralImputation(test)

##state generation
library(zipcode)
data(zipcode)
zips = zipcode[,c(1,3)]
names(zips)[1]="zip_code"

test =merge(test,zips,by="zip_code",all.x = T)
test = test[,!(names(test) %in% c("zip_code"))]
test$state = as.factor(test$state)
##impute age outliers
test$age_of_driver = ifelse(test$age_of_driver>100,
                             mean(test$age_of_driver[test$age_of_driver<100]),
                             test$age_of_driver)
summary(train$annual_income)
hist(train$annual_income)
train$annual_income = ifelse(train$annual_income<0,mean(train$annual_income),
                                                        train$annual_income)
hist(train$annual_income) 
train = train[order(train$claim_number),]
rownames(train) = 1:nrow(train)
test = test[order(test$claim_number),]
rownames(test) = 1:nrow(test)

###############EDA #################
#############categorical and numerical features ################
qualitative = c()
quantitative = c()
for(i in 1: ncol(train)){
  if(class(train[,i]) == "factor"){
    qualitative = append(qualitative,names(train[i]))
  }else{
    quantitative= append(quantitative,names(train[i]))
  }
}


#correlation analysis
quant_list = c(unlist(quantitative))
quant_list_1 =  quant_list[!(quant_list%in% c("claim_date"))]

cor_table = round(cor(train[,quant_list_1]),2)
View(cor_table)
############### data distribution #########
for(i in 1:length(qualitative)){
  plot(train[,qualitative[i]],main=qualitative[i])
}

for(i in 1:length(quantitative)){
  plot(train[,quantitative[i]],main=quantitative[i])
}

### outlier detection ########
for(i in 1:length(quantitative)){
  # plot(train[,quantitative[i]],main=quantitative[i])
  boxplot(train[,quantitative[i]],main=quantitative[i])
  print(boxplot.stats(train[,quantitative[i]])$out)
}

for(i in 1:length(quantitative)){
  # plot(train[,quantitative[i]],main=quantitative[i])
  plot(train[,quantitative[i]],main=quantitative[i])
}

#capping outliers
for(i in 1:length(quantitative)){
x <- train[,quantitative[i]]
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
train[,quantitative[i]][train[,quantitative[i]] < (qnt[1] - H)]= caps[1]
train[,quantitative[i]][train[,quantitative[i]]>(qnt[2] + H)] = caps[2]
}

### var imp


###### feature creation ##########

##### income_per_age ######
train$income_per_age = train$annual_income/train$age_of_driver
test$income_per_age = test$annual_income/test$age_of_driver

train$price_per_age = train$vehicle_price*train$age_of_vehicle
test$price_per_age = test$vehicle_price*test$age_of_vehicle

##### age_class ######
b <- seq(0,100,10)
train$age_class = .bincode(train$age_of_driver, b, TRUE,TRUE)
train$age_class  = as.factor(train$age_class)
class(train$age_class )

test$age_class = as.factor(.bincode(test$age_of_driver, b, TRUE,TRUE))

##### veh_age_class ###
a =seq(0,16,4)
summary(train$age_of_vehicle)
train$veh_age_class  = as.factor(ifelse(train$age_of_vehicle<3,"less_3",
                              ifelse(train$age_of_vehicle>8,"more_8",
                                     (train$age_of_vehicle))))
summary(train$veh_age_class )
summary(as.factor(train$age_of_vehicle))
test$veh_age_class = as.factor(ifelse(test$age_of_vehicle<3,"less_3",
                                      ifelse(test$age_of_vehicle>8,"more_8",
                                             (test$age_of_vehicle))))

##### liab/-prct #######
c <- seq(0,100,20)
train$liab_class = .bincode(train$liab_prct, c, TRUE,TRUE)
train$liab_class  = as.factor(train$liab_class)
summary(train$liab_class )

test$liab_class = as.factor(.bincode(test$liab_prct, c, TRUE,TRUE))

summary(train$claim_day_of_week)
plot(train$liab_class)

########products ###
summary(train$living_status)

### depreciation ##
#V = C(1-r)^t
##assume 15% depreciation per year
train$current_value = train$vehicle_price*(1-0.10)^train$age_of_vehicle
test$current_value = test$vehicle_price*(1-0.10)^test$age_of_vehicle

# wt,price,claim_est,past_num,safety_rating
hist( (train$vehicle_weight))
boxplot(train$vehicle_weight)
train$vehicle_weight[which.max(train$vehicle_weight)] =mean(train$vehicle_weight[-which.max(train$vehicle_weight)])

hist(train$claim_est_payout)
train[c(3214,5179,8341,3990,5450),]
glm_model_fin = glm(fraud~.,train[,!(names(train) %in% c("claim_number"))],family = "binomial")
# glm_model_fin = step(glm_model_fin)
summary(glm_model_fin)
############categorical #################
library(ggplot2)
ggplot(combined,aes(x=gender))+geom_bar(aes(fill =gender )  )
ggplot(combined,aes(x=living_status ))+geom_bar(aes( fill=living_status ))
ggplot(combined,aes(x=claim_day_of_week ))+geom_bar(aes( fill=claim_day_of_week ))
ggplot(combined,aes(x=accident_site ))+geom_bar(aes( fill=accident_site ))
ggplot(combined,aes(x=witness_present_ind ))+geom_bar(aes( fill=witness_present_ind ))
ggplot(combined,aes(x=channel ))+geom_bar(aes( fill=channel ))
ggplot(combined,aes(x=vehicle_category ))+geom_bar(aes( fill=vehicle_category ))
ggplot(combined,aes(x=vehicle_color ))+geom_bar(aes( fill=vehicle_color ))
ggplot(combined,aes(x=high_education_ind ))+geom_bar(aes( fill=high_education_ind ))
ggplot(combined,aes(x=address_change_ind ))+geom_bar(aes( fill=address_change_ind ))
ggplot(combined,aes(x=policy_report_filed_ind ))+geom_bar(aes( fill=policy_report_filed_ind ))
ggplot(combined,aes(x=Year ))+geom_bar(aes( fill=Year ))
ggplot(combined,aes(x=Month ))+geom_bar(aes( fill=Month ))
ggplot(combined,aes(x=Day ))+geom_bar(aes( fill=Day ))


############ continuous ##############
ggplot(combined, aes(hwy)) + geom_area(stat = "bin")

library(ggplot2)
######### Bivariate ############
###### qualitative variables vs Fraud -11 var ###########
ggplot(train,aes(x=gender,fill=factor(fraud)))+geom_boxplot(stat = "count",position = "dodge")
ggplot(train,aes(x=living_status,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=claim_day_of_week,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=accident_site,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=witness_present_ind,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=channel,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=vehicle_category,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=vehicle_color,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=high_education_ind,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=address_change_ind,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=policy_report_filed_ind,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=Year,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=Month,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")
ggplot(train,aes(x=Day,fill=factor(fraud)))+geom_bar(stat = "count",position = "dodge")



##date###
ggplot(train,aes(x=claim_date,fill=fraud))+geom_density(alpha=0.25)+ggtitle("Distribution of age_of_driver")

##zipcode#
length(unique(train$zip_code))
ggplot(train,aes(x=zip_code,fill=fraud))+geom_density(alpha=0.25)+ggtitle("Distribution of age_of_driver")

######## quantitative variables vs Fraud ###########
ggplot(train,aes(x=age_of_driver,fill=fraud))+geom_density(alpha=0.4)
ggplot(train,aes(x=safty_rating,fill=fraud))+geom_density(alpha=0.4)+ggtitle("Distribution of safty_rating")
ggplot(train,aes(x=annual_income,fill=fraud))+geom_density(alpha=0.4)+ggtitle("Distribution of annual_income")
ggplot(train,aes(x=past_num_of_claims,fill=fraud))+geom_density(alpha=0.4)+ggtitle("Distribution of past_num_of_claims")
ggplot(train,aes(x=liab_prct,fill=fraud))+geom_density(alpha=0.4)+ggtitle("Distribution of liab_prct")
ggplot(train[train$claim_est_payout>2900,],aes(x=claim_est_payout,fill=fraud))+geom_density(alpha=0.4)+ggtitle("Distribution of claim_est_payout")
ggplot(train,aes(x=age_of_vehicle,fill=fraud))+geom_density(alpha=0.4)+ggtitle("Distribution of age_of_vehicle")
ggplot(train,aes(x=vehicle_price,fill=fraud))+geom_density(alpha=0.4)+ggtitle("Distribution of vehicle_price")
ggplot(train,aes(x=vehicle_weight,fill=fraud))+geom_density(alpha=0.4)+ggtitle("Distribution of vehicle_weight")


###claim_number, claim_date,zip_code -difn variables


###### variable importance ##################
###### BORUTA VARIMP ##########
set.seed(123)
library(Boruta)
boruta.train <- Boruta(fraud~.,train[,!(names(train) %in% c("claim_number"))], doTrace = 2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

retain_features = getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

# retain_features
# [1] "age_of_driver"       "marital_status"      "safty_rating"        "annual_income"      
# [5] "high_education_ind"  "address_change_ind"  "living_status"       "accident_site"      
# [9] "past_num_of_claims"  "witness_present_ind" "channel"             "claim_est_payout"

train1 = train[,names(train)%in%retain_features]
train1$claim_number = train$claim_number
train1$fraud = train$fraud


####### split train , validation #######
index = sample(1:nrow(train),round(0.8*nrow(train)))
train_set = train[index,]
validation_set = train[-index,]

############ glm ##################
glm_model = glm(fraud  ~ gender + marital_status + safty_rating +
                  high_education_ind + address_change_ind + living_status +
                  accident_site + past_num_of_claims + witness_present_ind +
                  liab_prct + claim_est_payout + age_of_vehicle + vehicle_category +
                  Year + state + age_class + liab_class + age_of_vehicle*claim_est_payout
                +age_of_vehicle*liab_prct+age_of_vehicle*past_num_of_claims+
                  age_of_vehicle*safty_rating+claim_est_payout*liab_prct+
                  claim_est_payout*past_num_of_claims+liab_prct*safty_rating+
                safty_rating*claim_est_payout+past_num_of_claims*liab_prct
                ,train_set[,!(names(train_set) %in% c("claim_number"))],family = "binomial")
glm_model = step(glm_model)
summary(glm_model)

##accuracy
#validation
pred_val = predict(glm_model,validation_set[,names(validation_set)!= "claim_number"],type='response' )
y = as.factor(ifelse(pred_val>0.5,1,0))
table(validation_set$fraud,y)
library(Metrics)
Metrics::logLoss(as.numeric(train_set$fraud),glm_model$fitted.values)
Metrics::logLoss(as.numeric(validation_set$fraud),pred_val)

##2.36 to 2.3 by outlier capping
##2.308 with product of variables
###no effect of new variables
##step has no improvemt
##impact of clusters

#prediction
glm_model_fin = glm(fraud  ~ gender + marital_status + safty_rating +
                      high_education_ind + address_change_ind + living_status +
                      accident_site + past_num_of_claims + witness_present_ind +
                      liab_prct + claim_est_payout + age_of_vehicle + vehicle_category +
                      Year + state + age_class + liab_class + age_of_vehicle*claim_est_payout
                    +age_of_vehicle*liab_prct+age_of_vehicle*past_num_of_claims+
                      age_of_vehicle*safty_rating+claim_est_payout*liab_prct+
                      claim_est_payout*past_num_of_claims+liab_prct*safty_rating+
                      safty_rating*claim_est_payout+past_num_of_claims*liab_prct,
                    train[,!(names(train) %in% c("claim_number"))],family = "binomial")
glm_model_fin = step(glm_model_fin)
summary(glm_model_fin)
Metrics::logLoss(as.numeric(train$fraud),glm_model_fin$fitted.values)

pred_glm = predict(glm_model_fin,test[,names(test)!= "claim_number"],type='response' )
#before 2.322
#with new product features 2.314

# fraud ~ gender + marital_status + safty_rating +
# high_education_ind + address_change_ind + living_status +
#   accident_site + past_num_of_claims + witness_present_ind +
#   liab_prct + claim_est_payout + age_of_vehicle + vehicle_category +
#   Year + state + age_class + liab_class

########### xgb ###############
library(xgboost)
library(caret)
qual = unlist(qualitative)
paste(qualitative,collapse = "+")
dummies_1<- dummyVars(~gender+marital_status+high_education_ind+address_change_ind+living_status+claim_day_of_week+accident_site+witness_present_ind+channel+policy_report_filed_ind+vehicle_category+vehicle_color+Year+Month+Day+state,
                      data = train)
df_all_ohe <- as.data.frame(predict(dummies_1, newdata = train))
df_all_train <- cbind(train[,-c(which(colnames(train) %in% qual))],df_all_ohe)
df_all_train1 = data.matrix(df_all_train[,-1])

dummies_2<- dummyVars(~ gender+marital_status+high_education_ind+address_change_ind+living_status+claim_day_of_week+accident_site+witness_present_ind+channel+policy_report_filed_ind+vehicle_category+vehicle_color+Year+Month+Day+state,
                      data = test)
df_all_ohe2 <- as.data.frame(predict(dummies_2, newdata = test))
df_all_test<- cbind(test[,-c(which(colnames(test) %in% qual))],df_all_ohe2)
# df_all_combined = cbind(combined$)
df_all_test1 = data.matrix(df_all_test[,-1])
X = df_all_train1
y = data.matrix(train$fraud)
test_data =  df_all_test1

dtrain <- xgb.DMatrix(data = X, label= y)
dtest <- xgb.DMatrix(data = test_data)
# train xgboost and set parameters
library(xgboost)
model <- xgboost(data = dtrain, # the data   
                 nround = 100, # max number of boosting iterations
                 objective = "binary:logistic",
                 # booster = gbtree,
                 eta=0.1 ) # 0-1
pred_ttrain =predict(model, dtrain)
pred1 <- predict(model, dtest)
Metrics::logLoss(as.numeric(train$fraud),pred_ttrain)
# 2.85: training,eta=0.5
# 2.31: training,eta=0.1

pred2 = (3*pred_glm+pred1 +pred_rf[,"1"])/5
## (4*pred+pred1)/3 ---0.62
##(3*pred+pred1)/2 ---0.64
##pred+(pred1/3) ---0.58

############ validation########
dummies_11<- dummyVars(~ gender+marital_status+high_education_ind+address_change_ind+living_status+claim_day_of_week+accident_site+witness_present_ind+channel+policy_report_filed_ind+vehicle_category+vehicle_color+Year+Month+Day+state+age_class+veh_age_class+liab_class,data = train_set)
df_all_ohe1 <- as.data.frame(predict(dummies_11, newdata = train_set))
df_all_ts <- cbind(train_set[,-c(which(colnames(train_set) %in% qual))],df_all_ohe1)
df_all_ts1 = data.matrix(df_all_ts[,-1])

dummies_21<- dummyVars(~ gender+marital_status+high_education_ind+address_change_ind+living_status+claim_day_of_week+accident_site+witness_present_ind+channel+policy_report_filed_ind+vehicle_category+vehicle_color+Year+Month+Day+state+age_class+veh_age_class+liab_class,
                       data = validation_set)
df_all_ohe21 <- as.data.frame(predict(dummies_21, newdata = validation_set))
df_all_vs<- cbind(validation_set[,-c(which(colnames(validation_set) %in% qual))],df_all_ohe21)
# df_all_combined = cbind(combined$)
df_all_vs1 = data.matrix(df_all_vs[,-1])
X = df_all_ts1
y = data.matrix(train_set$fraud)
test_data =  df_all_vs1

dtrain <- xgb.DMatrix(data = X, label= y)
dtest <- xgb.DMatrix(data = test_data)
# train xgboost and set parameters
library(xgboost)
model <- xgboost(data = dtrain, # the data   
                 nround = 200, # max number of boosting iterations
                 objective = "binary:logistic",
                 # booster = gbtree,
                 eta=0.05 ) # 0-1
pred_ts = predict(model, dtrain)
pred_vs <- predict(model, dtest)
Metrics::logLoss(as.numeric(train_set$fraud),pred_ts)
Metrics::logLoss(as.numeric(validation_set$fraud),pred_vs)

Metrics::auc(validation_set$fraud, pred_vs)

##3.32 : validation set,eta =0.5
##2.84 : validation,2.59 train_set,eta=0.8
##2.42 : validation,2.31 train_set,eta=0.1
##2.42 : validation,2.31 train_set,eta=0.1 ==new features

########### lasso #################
library(glmnet)
library(Metrics)
set.seed(123)
# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.
 

library(glmnet)
# Find the best lambda using cross-validation
set.seed(123) 

cv.lasso <- cv.glmnet(df_all_train1, train$fraud, alpha = 0, family = "binomial")
plot(cv.lasso)

cv.lasso$lambda.min
cv.lasso$lambda.1se
# Fit the final model on the training data
model <- glmnet(df_all_train1, train$fraud, alpha = 0, family = "binomial",
                lambda = cv.lasso$lambda.1se)
# Display regression coefficients
coef(model)

lasso_prob <- predict(model,newx = df_all_train1,s=cv.lasso$lambda.1se,
                      type="response")
Metrics::logLoss(as.numeric(train$fraud),lasso_prob)

lasso_prob_test <- predict(model,newx = df_all_test1,s=cv.lasso$lambda.1se,
                      type="response")
##2.27: lambda.min ,lasso
##2.18: lambda.1se ,lasso
##2.17: lambda.1se ,ridge
##2.28:elastic net,0.3
##2.25: elastic net,0.8

## validation set
cv.lasso <- cv.glmnet(df_all_ts1, train_set$fraud, alpha = 0, family = "binomial")
plot(cv.lasso)

cv.lasso$lambda.min
cv.lasso$lambda.1se
# Fit the final model on the training data
model <- glmnet(df_all_ts1, train_set$fraud, alpha = 0, family = "binomial",
                lambda = cv.lasso$lambda.1se)
lasso_prob_vs <- predict(model,newx = df_all_vs1,s=cv.lasso$lambda.1se,
                           type="response")
Metrics::logLoss(as.numeric(validation_set$fraud),lasso_prob_vs)

 
Metrics::auc(validation_set$fraud, lasso_prob_vs)

##### adaboost ########
results_ada = train(x = train[,!(names(train)%in% c("claim_number",
                                                    "fraud"))], y = train$fraud, method="ada")
cv_opts = trainControl(method="cv", number=10)
results_ada = train(fraud~., data=df_all_train1, method="ada", 
                    trControl=cv_opts, maxdepth=10, nu=0.1, iter=50)
adatest = predict(results_ada,test)
############ svm ##################
library(e1071)
svm_model = svm(fraud ~ .,train[,names(train)!= "claim_number"],probability=TRUE )
# lm_model2 = step(lm_model)
summary(svm_model)
x= (train[,names(train)!= "claim_number"])
y=train$fraud
svm_tune <- tune(svm , fraud~., data = train[,names(train)!= "claim_number"],
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)
#prediction
#train
library(e1071)
library(caret)

svm_model = svm(fraud~.,train_set[,names(train_set)!= "claim_number"] ,
                probability=TRUE)
summary(svm_model)
svm_tune <- tune.svm(fraud~.,data=train_set[,names(train_set)!= "claim_number"] , 
                     kernel="radial",  gamma = 2^(-1:1), cost = 2^(2:4))
confusionMatrix(svm_model$fitted,train_set$fraud)
# Sensitivity : 1.0000          
# Specificity : 0.0000 
#validation
pred_svm = predict(svm_model,validation_set[,names(validation_set)!= "claim_number"],
                   probability=TRUE)
confusionMatrix(pred_svm,validation_set$fraud)
# Sensitivity : 1.0000          
# Specificity : 0.0000  
summary(pred_svm)
View(pred_svm)

###total train
pred_svm_1 = predict(svm_model,train[,names(train)!= "claim_number"] ,
                     probability=TRUE)
# pred_svm_1 = as.factor(ifelse(pred_svm_1>0.5,1,0))
library(e1071)
svm_tune <- tune.svm(fraud~.,data=train[,names(train)!= "claim_number"] , 
                     kernel="radial",  gamma = 2^(-1:1), cost = 2^(2:4))
confusionMatrix(svm_tune,train$fraud)
##pred test
pred = predict(svm_tune,test[,names(test)!= "claim_number"]  )

############### decision tree ###########
# Classification Tree with rpart
library(rpart)
model <- rpart(fraud~.,data=train[,names(train)!= "claim_number"], method = "class" )

## to get the probabilities of each record
pred_dec <- predict(model, test[,names(test)!= "claim_number"], type = "prob")





############# random forest  #############  
library(randomForest)
rf_model = randomForest(fraud~. ,train[,!(names(train)%in% c("claim_number","zip_code"))])
summary(rf_model)
pred_rf_train = predict(rf_model,train[,!(names(train)%in% c("claim_number","zip_code"))] ,
                  type="prob")
pred_rf = predict(rf_model,test[,!(names(test)%in% c("claim_number","zip_code"))] ,
                     type="prob")
pred_rf = pred_rf[,"1"]

names(pred_rf[,"1"])
Metrics::logLoss(as.numeric(train$fraud),pred_rf_train[,"1"])

##accuracy
library(caret)
pred_train = predict(rf_model,train_set[,!(names(train_set)%in% c("claim_number",
                                                                  "zip_code"))] ,
                     type="prob")
confusionMatrix(pred_train,train_set$fraud)

#prediction
pred = predict(rf_model,validation_set[,!(names(validation_set)%in% c("claim_number","zip_code"))])
confusionMatrix(pred ,validation_set$fraud)

############ decision tree ############
library(rpart)
dec_tree = rpart(fraud~.,train[,!(names(train) %in% c("claim_number"))])
tree.pred = predict(dec_tree, test[,!(names(test) %in% c("claim_number"))],
                    method = "class",type="prob")
library(rpart)
model <- rpart(fraud~.,train[,!(names(train) %in% c("claim_number"))])

## to get the probabilities of each record
prob <- predict(model, test[,!(names(test) %in% c("claim_number"))], type = "prob")



######### lightgbm ##############
library(lightgbm)
model <- lightgbm(data = dtrain, # the data   
                 nround = 100, # max number of boosting iterations
                 objective = "binary:logistic",
                 # booster = gbtree,
                 eta=0.1 ) # 0-1
pred_ts = predict(model, dtrain)
pred_vs <- predict(model, dtest)
Metrics::logLoss(as.numeric(train_set$fraud),pred_ts)
Metrics::logLoss(as.numeric(validation_set$fraud),pred_vs)


######### H20 ###################

## https://www.r-bloggers.com/things-to-try-after-user-part-1-deep-learning-with-h2o/
## Start a local cluster with 1GB RAM (default)
library(h2o)
localH2O <- h2o.init( )

## Start a local cluster with 2GB RAM
# localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, 
#                     Xmx = '2g')
# train_h2o <-  as.h2o(train_set[,!(names(train_set) %in% c("claim_number"))])  # remove the ID column
# test_h2o <- as.h2o(validation_set[,!(names(validation_set) %in% c("claim_number"))])

train_h2o <-  as.h2o(train[,!(names(train) %in% c("claim_number"))])  # remove the ID column
test_h2o <- as.h2o(test[,!(names(test) %in% c("claim_number"))])
model <- h2o.deeplearning(x = c(1:21,23:ncol(train)),  # column numbers for predictors
                   y = 22,   # column number for label
                   training_frame = train_h2o, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.3, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(100,100,100), # three layers of 50 nodes
                   epochs = 100) # max. no. of epochs

# model = h2o.randomForest(x = c(1:21,23:26),  # column numbers for predictors
#                  y = 22,   # column number for label
#                  training_frame = train_h2o)
## Using the DNN model for predictions
h2o_yhat_test <- h2o.predict(model, test_h2o)

## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_yhat_test)
pred_dl = df_yhat_test[,3]

## Converting H2O format into data frame
h2o_yhat_train <- h2o.predict(model, train_h2o)
df_yhat_train <- as.data.frame(h2o_yhat_train)
pred_dl_train = df_yhat_train[,3]
Metrics::logLoss(as.numeric(train_set$fraud),pred_dl_train )
Metrics::logLoss(as.numeric(train$fraud),pred_dl )
#validation ,Rectifierwith dropout  2.109569
#tanHwith dropout 1.925
#tanHwith dropout 1.913 new features
#tanHwith dropout 1.84 new features 100,100,50 hidden layer
#tanHwith dropout 1.812 new features 100,100,100 hidden layer,0.2 input drop
#tanHwith dropout train=1.66,validat=1.6567 new features 100,100,100 hidden layer,0.3 i.p drop

#maxwithdroput 2.525


############# gbm  #############  
library(gbm)
library(caret)
set.seed(123)

train_g = train
train_g$fraud = ifelse(train$fraud==1,1,0)
gbmCV = gbm(formula = fraud~.,
            distribution = "bernoulli",
            data = train_g[,names(train_g)!= "claim_number"],
            n.trees = 500,
            shrinkage = 0.0005,
            n.minobsinnode = 15,
            cv.folds = 5,
            n.cores = 1)

optimalTreeNumberPredictionCV = gbm.perf(gbmCV)
gbmTest = predict(object = gbmCV,
                  newdata =test[,names(test)!= "claim_number"],
                  n.trees = optimalTreeNumberPredictionCV,
                  type = "response")

#2.26 :500 trees,0.1 shrinkage
#2.29 : 1000 trees,0.1 shrinkage
#2.23 : 500 trees,0.05 shrinkage
#2.13 : 500 trees,0.0005/0001
# gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 6, 9, 10),
#                         n.trees = (0:50)*50, 
#                         shrinkage = seq(.0005, .05,.0005),
#                         n.minobsinnode = 10) # you can also put something        like c(5, 10, 15, 20)
# 
# fitControl <- trainControl(method = "repeatedcv",
#                            repeats = 5,
#                            preProcOptions = list(thresh = 0.95),
#                            ## Estimate class probabilities
#                            classProbs = TRUE,
#                            ## Evaluate performance using
#                            ## the following function
#                            summaryFunction = twoClassSummary)
# 
# # Method + Date + distribution
# set.seed(1)
# system.time(GBM0604ada <- train(Outcome ~ ., data = training,
#                                 distribution = "adaboost",
#                                 method = "gbm", bag.fraction = 0.5,
#                                 nTrain = round(nrow(training) *.75),
#                                 trControl = fitControl,
#                                 verbose = TRUE,
#                                 tuneGrid = gbmGrid,
#                                 ## Specify which metric to optimize
#                                 metric = "ROC")) 


######### validation ############
train_g = train_set
train_g$fraud = ifelse(train_set$fraud==1,1,0)
library(gbm)
gbmCV = gbm(formula = fraud~.,
            distribution = "bernoulli",
            data = train_g[,names(train_g)!= "claim_number"],
            n.trees = 500,
            shrinkage = .0005,
            n.minobsinnode = 15,
            cv.folds = 5,
            n.cores = 1)

optimalTreeNumberPredictionCV = gbm.perf(gbmCV)

validation_g = validation_set
validation_g$fraud = ifelse(validation_g$fraud==1,1,0)

gbmTest = predict(object = gbmCV,
                  newdata =validation_g[,names(validation_g)!= "claim_number"],
                  n.trees = optimalTreeNumberPredictionCV,
                  type = "response")

Metrics::auc(validation_set$fraud, gbmTest)


################# boruta features #############
library(e1071)
svm_model = svm(fraud~.,train[,names(train)!= "claim_number"] )
# lm_model2 = step(lm_model)
summary(svm_model)
svm_tune <- tune(svm , fraud~., data = train1[,names(train1)!= "claim_number"],
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
Metrics::logLoss(as.numeric(train$fraud),svm_model$fitted.values)
print(svm_tune)
#prediction
pred_svm = predict(svm_model,test[,names(test)!= "claim_number"]  )

summary(pred_svm)

################ adaboost ################
Grid <- expand.grid(maxdepth=25,nu=2,iter=100)
library(caret)
cv_opts = trainControl(method="cv", number=10)
results_ada = train(fraud~., data = train[,names(train)!= "claim_number"],
                    method="ada",
                    trControl=cv_opts,tuneGrid=Grid)
pred_ada = predict(results_ada,test)

########### k-medoids clustering ###################
df_all_train_cl = data.frame(apply(df_all_train,2,as.numeric))
str(df_all_train_cl)
clusters = kmeans(train[,quantitative],6)
train$Clusters <- as.factor(clusters$cluster)
df_all_train$fraud = train$fraud


library(factoextra)
# plots to compare
 fviz_cluster(clusters, geom = "point", data =  train[,quantitative]) + ggtitle("k = 2")


############# glm,dl,xgb ################
pred_comb = (pred_glm+2*pred_dl)/3
pred_final = data.frame(claim_number = test$claim_number,fraud = gbmTest)
##xgb
df = data.frame(lasso_prob_test,gbmTest,pred_dl,pred1)

df1 = data.frame(lasso_prob_vs,gbmTest,pred_vs)
#0.64:gbm,0.708:xgb,0.723:lasso
df1$pp = (0.35*df1$X1+0.6*df1$gbmTest+0.05*df1$pred_vs  ) 
Metrics::auc(validation_set$fraud, df1$pp)
# 0.3*df1$X1+0.6*df1$gbmTest+0.1*df1$pred_vs ==>0.72597
#(0.35*df1$X1+0.6*df1$gbmTest+0.05*df1$pred_vs  ) ==>0.72614
pred11 = (3*lasso_prob_test+  pred1+5*gbmTest+pred_rf+pred_dl)/11
pred3= (2*lasso_prob_test+ pred1+3*gbmTest)/6
pred_final = data.frame(claim_number = test$claim_number,fraud = pred_rf,pred_dl)
names(pred_final)[2]="fraud"
df_summary = data.frame("glm" = round(pred_glm,2),"xgb" = round(pred1,2),
                        "dl"=round(pred_dl,2),"rf"=pred_rf[,2])
write.csv(pred_final,"E:/1/UConn/Travellers/all/submission_xgb.csv",row.names=F)
sum(is.na(pred_final))

#0.75328 = (2*lasso_prob_test+ pred1+3*gbmTest)/6:lasso,xgb,gbm,old features
#0.75283 = (2*lasso_prob_test+ pred1+3*gbmTest)/6:lasso,xgb,gbm,old features
#0.75116 = (2*lasso_prob_test+ pred1+4*gbmTest)/7:lasso,xgb,gbm
#0.75123 =(2*lasso_prob_test+ pred1+3*gbmTest)/6:lasso,xgb,gbm
#0.748 = gbm
#0.75027 = (((2*lasso_prob_test+pred1+2*pred_dl)/5)+3*gbmTest)/4:lasso,xgb,dl,gbm
#0.7487 = (((2*lasso_prob_test+pred1+2*pred_dl)/5)+ gbmTest)/2 :lasso,xgb,dl,gbm
#0.7462 =(2*lasso_prob_test+pred1+2*pred_dl)/5:lasso,xgb,dl
#0.7462 =(2*lasso_prob_test+pred1+pred_dl)/4:lasso,xgb,dl
#0.744 avg xgb,lasso
##0.7346 xgb,0.1
##0.738 lasso
# 0.74506 4,9 wt to all
##0.74400 glm,xgb,rf
# 0.74495-avg 2
# 0.74518 - 3 wt to old model
#0.74571 - 3 wt to old model(xgb-new)
#0.7437 - 2 wt to old model(lasso new)
##0.683 ---deep learning
##0.74 --- DL,xgb,glm,rf
##0.743 --xgb,glm,rf
###0.7337--glm,dl,xgb
##0.734-step(glm)
## rf - 
# (2*pred_glm+pred_dl)/3---0.726
