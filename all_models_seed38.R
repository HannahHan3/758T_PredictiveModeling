library(readr)
Hospitals_Train <- read.csv('Hospitals_Train.csv')
Hospitals_Test_X <- read.csv("Hospitals_Test_X.csv")

set.seed(88)
Hospitals_Train$RETURN=ifelse(Hospitals_Train$RETURN=='Yes',1,0)
Hospitals_Train$RETURN=as.factor(Hospitals_Train$RETURN)

RETURN<-Hospitals_Train[,27]
Hospitals_Train<-Hospitals_Train[,-27]
Hospitals_Data<-rbind(Hospitals_Train,Hospitals_Test_X)
Hospitals_Data<-Hospitals_Data[c(2,3,4,5,6,7,9,14,15,16,17,19,21,22,23,24,25,26)]
#Hospitals_Data<-Hospitals_Data[c(2,3,4,5,6,7,14,15,16,17,18,19,20,22,23,24,25,26)]
Hospitals_Data$WEEKDAY_ARR=as.factor(Hospitals_Data$WEEKDAY_ARR)
Hospitals_Data$HOUR_ARR=as.factor(Hospitals_Data$HOUR_ARR)
Hospitals_Data$MONTH_ARR=as.factor(Hospitals_Data$MONTH_ARR)

Hospitals_Data$WEEKDAY_DEP=as.factor(Hospitals_Data$WEEKDAY_DEP)
Hospitals_Data$HOUR_DEP=as.factor(Hospitals_Data$HOUR_DEP)
Hospitals_Data$MONTH_DEP=as.factor(Hospitals_Data$MONTH_DEP)

Hospitals_Data$CONSULT_IN_ED <- ifelse(is.na(Hospitals_Data$CONSULT_IN_ED),0,Hospitals_Data$CONSULT_IN_ED)
Hospitals_Data$CONSULT_IN_ED=as.factor(Hospitals_Data$CONSULT_IN_ED)

levels(Hospitals_Data$RACE)[c(1,2,3,5,6,7,8,9,10,11)]='Lower Than Mode'

#levels(Hospitals_Data$ETHNICITY)[c(1,5)]='Unknown'

levels(Hospitals_Data$FINANCIAL_CLASS)[c(1,2,3,7,8,9,10,11,12,13)]='Lower Than Mode'
levels(Hospitals_Data$FINANCIAL_CLASS)[c(3,4)]='Higher Than Mode'

levels(Hospitals_Data$HOUR_ARR)[c(2,3,4,5,6,7,8,9,10,11)]='midnight'
levels(Hospitals_Data$HOUR_ARR)[c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]='day'

levels(Hospitals_Data$HOUR_DEP)[c(2,3,4,5,6,7,8,9,10,11)]='midnight'
levels(Hospitals_Data$HOUR_DEP)[c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]='day'

Hospitals_Data$SAME_DAY=as.factor(Hospitals_Data$SAME_DAY)

levels(Hospitals_Data$ED_RESULT)[c(1,2,3,5,6,13,14,15,16)]='Lower Than Mode'
levels(Hospitals_Data$ED_RESULT)[c(2,4,5,6,7,8)]='Higher Than Mode'

levels(Hospitals_Data$ACUITY_ARR)[1]='MISSING'
#levels(Hospitals_Data$ACUITY_ARR)[c(5,6)]='4 or 5'

levels(Hospitals_Data$DC_RESULT)[c(19,37,38,39,40,41,42,43)]='Mode'
levels(Hospitals_Data$DC_RESULT)[c(4,5,6,8,9,12,13,14,15,16,17,18,20,21,22,28,30,32,33,34,35,36)]='Lower Than Mode'
levels(Hospitals_Data$DC_RESULT)[c(1,2,3,5,6,7,9,10,11,12,13,14,15)]='Higher Than Mode'

levels(Hospitals_Data$ADMIT_RESULT)[1]='MISSING'

Hospitals_Data$CONSULT_ORDER=as.factor(Hospitals_Data$CONSULT_ORDER)
Hospitals_Data$CONSULT_CHARGE=as.factor(Hospitals_Data$CONSULT_CHARGE)
levels(Hospitals_Data$RISK)[1]='MISSING'
levels(Hospitals_Data$SEVERITY)[1]='MISSING'

Hospitals_Data$CHARGES <- as.single(Hospitals_Data$CHARGES)
Hospitals_Data$CHARGES <- ifelse(is.na(Hospitals_Data$CHARGES),2147.67,Hospitals_Data$CHARGES)

Hospitals_Train<-Hospitals_Data[1:38077,]
Hospitals_Test_X<-Hospitals_Data[38078:50109,]
Hospitals_Train$RETURN<-RETURN
num_rows=nrow(Hospitals_Train)
sample_obs=sample(num_rows,0.8*num_rows)
Hospitals_rest=Hospitals_Train[sample_obs,]
Hospitals_test=Hospitals_Train[-sample_obs,]
num_rows1=nrow(Hospitals_rest)
sample_obs1=sample(num_rows1,0.8*num_rows1)
Hospitals_train=Hospitals_rest[sample_obs1,]
Hospitals_valid=Hospitals_rest[-sample_obs1,]
baselineacc=1-sum(as.numeric(Hospitals_test$RETURN)-1)/nrow(Hospitals_test)
baselineacc


#logistic
Log_model <- glm(RETURN~.,data=Hospitals_rest, family="binomial")
Log_preds=predict(Log_model,newdata=Hospitals_test,type="response")
Log_class=ifelse(Log_preds>0.37,1,0)
Log_confuse_test=table(Hospitals_test$RETURN,Log_class,dnn=c('Actual','Predicted'))
Log_acc=(Log_confuse_test[1,1]+Log_confuse_test[2,2])/sum(Log_confuse_test)
Log_acc
Log_confuse_test

#LASSO
library(glmnet)
Hospitals_rest_X=Hospitals_rest[,-19]
Hospitals_test_X=Hospitals_test[,-19]
Hospitals_valid_X=Hospitals_valid[,-19]
Hospitals_rest_X <- model.matrix( ~ .-1, Hospitals_rest[,c(1:18)])
Hospitals_test_XX <- model.matrix( ~ .-1, Hospitals_Test_X[,c(1:18)])
Hospitals_test_X=model.matrix( ~ .-1, Hospitals_test[,c(1:18)])
Hospitals_valid_X=model.matrix( ~ .-1, Hospitals_valid[,c(1:18)])
glmnet_lasso = glmnet(Hospitals_rest_X, Hospitals_rest$RETURN, family = 'binomial', alpha = 1)
glmnet_lasso.cv = cv.glmnet(Hospitals_rest_X,Hospitals_rest$RETURN,family="binomial",alpha=1)
plot(glmnet_lasso.cv)
best.lambda1=glmnet_lasso.cv$lambda.min
best.lambda1
predict(glmnet_lasso,s=best.lambda1,type="coefficients")
lasso_probs = predict(glmnet_lasso,s=best.lambda1,newx=Hospitals_test_X,type="response")
lasso_class = ifelse(lasso_probs>0.367,1,0)
sum(ifelse(lasso_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)
table(Hospitals_test$RETURN,lasso_class,dnn=c('Actual','Predicted'))



#rigde
glmnet_ridge = glmnet(Hospitals_rest_X, Hospitals_rest$RETURN, family = 'binomial', alpha = 0)
glmnet_ridge.cv = cv.glmnet(Hospitals_rest_X,Hospitals_rest$RETURN,family="binomial",alpha=0)
plot(glmnet_ridge.cv)
best.lambda2=glmnet_ridge.cv$lambda.min
best.lambda2
predict(glmnet_ridge,s=best.lambda2,type="coefficients")
ridge_probs = predict(glmnet_ridge,s=best.lambda2,newx=Hospitals_test_X,type="response")
ridge_class = ifelse(ridge_probs>0.37,1,0)
sum(ifelse(ridge_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)
table(Hospitals_test$RETURN,ridge_class,dnn=c('Actual','Predicted'))



#STEP BACKWARD/FORWARD
Hospitals_all=glm(Hospitals_train$RETURN~.,data=Hospitals_train,family='binomial')
Hospitals_null<-glm(Hospitals_train$RETURN~1,data=Hospitals_train,family='binomial')
forward_Hospitals_both = step(Hospitals_null, scope=list(upper=Hospitals_all), direction="both",trace=0)
backward_Hospitals_both = step(Hospitals_all, direction="both",trace=0)
summary(backward_Hospitals_both)
summary(forward_Hospitals_both)
backward_pre_both=predict(backward_Hospitals_both,newdata=Hospitals_valid,type='response')
forward_pre_both=predict(forward_Hospitals_both,newdata=Hospitals_valid,type='response')
cutoff=c(0.4,0.45,0.5,0.55,0.6,0.65,0.7)
backward_acc=vector()
forward_acc=vector()
for (i in 1:7){
  backward_class=ifelse(backward_pre_both>cutoff[i],1,0)
  forward_class=ifelse(forward_pre_both>cutoff[i],1,0)
  backward_table=table(Hospitals_valid$RETURN,backward_class)
  forward_table=table(Hospitals_valid$RETURN,forward_class)
  backward_acc[i]=(backward_table[1,1]+backward_table[2,2])/sum(backward_table)
  forward_acc[i]=(forward_table[1,1]+forward_table[2,2])/sum(forward_table)
}
backward_acc
forward_acc

backward_pre_both1=predict(backward_Hospitals_both,newdata=Hospitals_test,type='response')
forward_pre_both1=predict(forward_Hospitals_both,newdata=Hospitals_test,type='response')
backward_class=ifelse(backward_pre_both1>0.37,1,0)
forward_class=ifelse(forward_pre_both1>0.37,1,0)
backward_table=table(Hospitals_test$RETURN,backward_class)
forward_table=table(Hospitals_test$RETURN,forward_class)
(backward_table[1,1]+backward_table[2,2])/sum(backward_table)
backward_table
(forward_table[1,1]+forward_table[2,2])/sum(forward_table)
forward_table



#Write the prediction for testing data in csv(the format of the csv may be wrong,we should
#change the format with adding column name and reindex by ourselevs directly in csv file)
#backward_pre_both2=predict(backward_Hospitals_both,newdata=Hospitals_Test_X,type='response')
#backward_class1=ifelse(backward_pre_both2>0.5,1,0)
#Class=ifelse(backward_class1>0.5,'Yes','No')
#write.csv(Class,file = 'submission_TEAM_19.csv')


library(randomForest)
library(tree)
ncol(Hospitals_rest)
bag.trees=randomForest(Hospitals_rest$RETURN~.,data=Hospitals_rest,ntree=300,importance=TRUE)
bag_preds=predict(bag.trees,newdata=Hospitals_test,type="prob")
bag_probs=bag_preds[,2]
bag_class=ifelse(bag_probs>0.279,1,0)
bag_confuse_test=table(Hospitals_test$RETURN,bag_class,dnn=c('Actual','Predicted'))
bag_acc=(bag_confuse_test[1,1]+bag_confuse_test[2,2])/sum(bag_confuse_test)
bag_acc
bag_confuse_test
varImpPlot(bag.trees)



#random forest
rf.trees=randomForest(Hospitals_rest$RETURN~.,data=Hospitals_rest,ntree=300,importance=TRUE,mtry=5)
rf_preds=predict(rf.trees,newdata=Hospitals_test,type="prob")
rf_probs=rf_preds[,2]
rf_class=ifelse(rf_probs>0.32,1,0)
sum(ifelse(rf_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)
rf_confuse_test=table(Hospitals_test$RETURN,rf_class,dnn=c('Actual','Predicted'))
rf_confuse_test
varImpPlot(rf.trees)


#boosting
library(gbm)
boost.hospitalS=gbm(as.numeric(Hospitals_rest$RETURN)-1~as.numeric(HOSPITAL)+ as.numeric(GENDER)+ as.numeric(RACE)+ as.numeric(ETHNICITY)
                    + as.numeric(FINANCIAL_CLASS)+ as.numeric(SAME_DAY)+ as.numeric(ACUITY_ARR)
                    + as.numeric(DC_RESULT)
                    + as.numeric(DIAGNOSIS)+ as.numeric(RISK)+ as.numeric(SEVERITY)+AGE+as.numeric(HOUR_ARR)
                    +as.numeric(ED_RESULT)
                    +as.numeric(CONSULT_ORDER)+as.numeric(CONSULT_IN_ED)+DIAG_DETAILS+CHARGES,data=Hospitals_rest,distribution="bernoulli",n.trees=1000)
boost_test_pred=predict(boost.hospitalS,newdata=Hospitals_test,n.trees=1000,type="response")
boost_class=ifelse(boost_test_pred>0.351,1,0)
boosting_confuse_test=table(Hospitals_test$RETURN,boost_class,dnn=c('Actual','Predicted'))
boosting_acc=(boosting_confuse_test[1,1]+boosting_confuse_test[2,2])/sum(boosting_confuse_test)
boosting_acc
boosting_confuse_test
summary(boost.hospitalS)



#knn
Hospitals_train_x<-model.matrix( ~ .-1, Hospitals_train[1:18])
Hospitals_valid_x<-model.matrix( ~ .-1, Hospitals_valid[1:18])
Hospitals_test_x<-model.matrix( ~ .-1, Hospitals_test[1:18])
Hospitals_train_y<-as.numeric(Hospitals_train$RETURN)
Hospitals_valid_y<-as.numeric(Hospitals_valid$RETURN)
Hospitals_test_y<-as.numeric(Hospitals_test$RETURN)
library(class)

knn_valid_accuracy<-vector()
n=1
for (i in c(1:40)){
  knn_pred=knn(Hospitals_train_x,Hospitals_valid_x,Hospitals_train_y,k=i)
  knn_table=table(Hospitals_valid_y,knn_pred)
  knn_valid_accuracy[n]= (knn_table[1,1]+knn_table[2,2])/sum(knn_table)
  n=n+1
}
knn_valid_accuracy

knn_test_pred=knn(Hospitals_train_x,Hospitals_test_x,Hospitals_train_y,k=28)
knn_table=table(Hospitals_test_y,knn_test_pred)
knn_test_accuracy= (knn_table[1,1]+knn_table[2,2])/sum(knn_table)
knn_test_accuracy


#xgboost
library(xgboost)
labels_valid=as.numeric(Hospitals_valid$RETURN)-1
labels_train=as.numeric(Hospitals_train$RETURN)-1
xg_train=model.matrix( ~ .-1, Hospitals_train[1:18])
xg_valid_x<-model.matrix( ~ .-1, Hospitals_valid[1:18])
xg_test_x<-model.matrix( ~ .-1, Hospitals_test[1:18])


xg_valid_accuracy<-vector()
n=1
for (i in c(6,7,8)){
  bst=xgboost(data=xg_train,label=labels_train,max.depth=i,eta=0.3,nround=100,objective='binary:logistic')
  xg_valid_pred=predict(bst,xg_valid_x)
  xg_class=ifelse(xg_valid_pred>0.5,1,0)
  xg_valid_accuracy[n]= sum(ifelse(xg_class==Hospitals_valid$RETURN,1,0))/nrow(Hospitals_valid)
  n=n+1
}
xg_valid_accuracy
bst=xgboost(data=xg_train,label=labels_train,max.depth=6,eta=0.3,nround=100,objective='binary:logistic')
xg_test_pred=predict(bst,xg_test_x)
xg_class=ifelse(xg_test_pred>0.38,1,0)
sum(ifelse(xg_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)
table(Hospitals_test$RETURN,xg_class)

#bag_preds1=predict(bag.trees,newdata=Hospitals_Test_X,type="prob")
#bag_probs1=bag_preds1[,2]
#bag_class1=ifelse(bag_probs1>0.5,1,0)
#Class=ifelse(bag_class1>0.5,'Yes','No')
#write.csv(Class,file = 'submission_TEAM_19.csv')
#xg_Hospitals_Test_x<-model.matrix( ~ .-1, Hospitals_Test_X)
#xg_test_pred=predict(bst,xg_Hospitals_Test_x)
#xg_class=ifelse(xg_test_pred>0.5,1,0)
#Class=ifelse(xg_class>0.5,'Yes','No')
#write.csv(Class,file = 'submission_TEAM_19.csv')
