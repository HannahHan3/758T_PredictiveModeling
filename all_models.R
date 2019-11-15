library(readr)
Hospitals_Train <- read.csv('Hospitals_Train.csv')
Hospitals_Test_X <- read.csv("Hospitals_Test_X.csv")

set.seed(88)
Hospitals_Train$RETURN=ifelse(Hospitals_Train$RETURN=='Yes',1,0)
Hospitals_Train$RETURN=as.factor(Hospitals_Train$RETURN)

RETURN<-Hospitals_Train[,27]
Hospitals_Train<-Hospitals_Train[,-27]
Hospitals_Data<-rbind(Hospitals_Train,Hospitals_Test_X)

#Hospitals_Data<-Hospitals_Data[c(2,3,4,5,6,7,8,9,10,14,15,16,17,18,20,21,22,23,24,25,26)]
#Hospitals_Data<-Hospitals_Data[c(2,3,4,5,6,7,14,15,16,17,18,19,20,22,23,24,25,26)]
Hospitals_Data$CONSULT_IN_ED <- ifelse(is.na(Hospitals_Data$CONSULT_IN_ED),0,Hospitals_Data$CONSULT_IN_ED)
Hospitals_Data$HOSPITAL=as.factor(Hospitals_Data$HOSPITAL)
Hospitals_Data$GENDER=as.factor(Hospitals_Data$GENDER)
Hospitals_Data$AGE=as.numeric(Hospitals_Data$AGE)
Hospitals_Data$RACE=as.factor(Hospitals_Data$RACE)
Hospitals_Data$ETHNICITY=as.factor(Hospitals_Data$ETHNICITY)
Hospitals_Data$FINANCIAL_CLASS=as.factor(Hospitals_Data$FINANCIAL_CLASS)
Hospitals_Data$WEEKDAY_ARR=as.factor(Hospitals_Data$WEEKDAY_ARR)
Hospitals_Data$HOUR_ARR=as.factor(Hospitals_Data$HOUR_ARR)
Hospitals_Data$MONTH_ARR=as.factor(Hospitals_Data$MONTH_ARR)
Hospitals_Data$WEEKDAY_DEP=as.factor(Hospitals_Data$WEEKDAY_DEP)
Hospitals_Data$HOUR_DEP=as.factor(Hospitals_Data$HOUR_DEP)
Hospitals_Data$MONTH_DEP=as.factor(Hospitals_Data$MONTH_DEP)
Hospitals_Data$SAME_DAY=as.factor(Hospitals_Data$SAME_DAY)
Hospitals_Data$ED_RESULT=as.factor(Hospitals_Data$ED_RESULT)
Hospitals_Data$ACUITY_ARR=as.factor(Hospitals_Data$ACUITY_ARR)
Hospitals_Data$DC_RESULT=as.factor(Hospitals_Data$DC_RESULT)
Hospitals_Data$ADMIT_RESULT=as.factor(Hospitals_Data$ADMIT_RESULT)
Hospitals_Data$CONSULT_ORDER=as.factor(Hospitals_Data$CONSULT_ORDER)
Hospitals_Data$CONSULT_CHARGE=as.factor(Hospitals_Data$CONSULT_CHARGE)
Hospitals_Data$CONSULT_IN_ED=as.factor(Hospitals_Data$CONSULT_IN_ED)
Hospitals_Data$DIAGNOSIS=as.factor(Hospitals_Data$DIAGNOSIS)
Hospitals_Data$DIAG_DETAILS=as.numeric(Hospitals_Data$DIAG_DETAILS)
Hospitals_Data$RISK=as.factor(Hospitals_Data$RISK)
Hospitals_Data$SEVERITY=as.factor(Hospitals_Data$SEVERITY)
Hospitals_Data$CHARGES <- as.single(Hospitals_Data$CHARGES)


Hospitals_Data$CHARGES <- ifelse(is.na(Hospitals_Data$CHARGES),2147.67,Hospitals_Data$CHARGES)


levels(Hospitals_Data$DC_RESULT)[1]='Missing'
levels(Hospitals_Data$RISK)[1]='Missing'
levels(Hospitals_Data$SEVERITY)[1]='Missing'
levels(Hospitals_Data$ADMIT_RESULT)[1]="Missing"
levels(Hospitals_Data$DC_RESULT)[1]='Missing'
levels(Hospitals_Data$ACUITY_ARR)[1]='Missing'
levels(Hospitals_Data$ETHNICITY)[1]='Missing'
levels(Hospitals_Data$RACE)[1]='Missing'

levels(Hospitals_Data$RACE)[c(1,2,3,5,6,7,8,9,10,11)]='Lower Than Mode'
#levels(Hospitals_Data$ETHNICITY)[c(1,5)]='Unknown'

levels(Hospitals_Data$FINANCIAL_CLASS)[c(1,2,3,7,8,9,10,11,12,13)]='Lower Than Mode'
levels(Hospitals_Data$FINANCIAL_CLASS)[c(3,4)]='Higher Than Mode'
levels(Hospitals_Data$HOUR_ARR)[c(2,3,4,5,6,7,8,9,10,11)]='midnight'
levels(Hospitals_Data$HOUR_ARR)[c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]='day'
levels(Hospitals_Data$ED_RESULT)[c(1,2,3,5,6,13,14,15,16)]='Lower Than Mode'
levels(Hospitals_Data$ED_RESULT)[c(2,4,5,6,7,8)]='Higher Than Mode'
#levels(Hospitals_Data$ACUITY_ARR)[c(5,6)]='4 or 5'
levels(Hospitals_Data$DC_RESULT)[c(19,37,38,39,40,41,42,43)]='Mode'
levels(Hospitals_Data$DC_RESULT)[c(4,5,6,8,9,12,13,14,15,16,17,18,20,21,22,28,30,32,33,34,35,36)]='Lower Than Mode'
levels(Hospitals_Data$DC_RESULT)[c(1,2,3,5,6,7,9,10,11,12,13,14,15)]='Higher Than Mode'

#pca
hospital_pca <- model.matrix( ~ ., Hospitals_Data[c(2,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)])
hospital_pca
PCs = prcomp(hospital_pca)
PCs
summary(PCs)
PC1 = PCs$x[,1]
PC2 = PCs$x[,2]
PC3 = PCs$x[,3]
PC4 = PCs$x[,4]
PC5 = PCs$x[,5]
PC6 = PCs$x[,6]
PC7 = PCs$x[,7]
PC8 = PCs$x[,8]
PC9 = PCs$x[,9]
PC10 = PCs$x[,10]
PC11 = PCs$x[,11]
PC12 = PCs$x[,12]
PC13 = PCs$x[,13]
PC14 = PCs$x[,14]
PC15 = PCs$x[,15]
PC16 = PCs$x[,16]
PC17 = PCs$x[,17]
PC18 = PCs$x[,18]
PC19 = PCs$x[,19]
PC20 = PCs$x[,20]
PC21 = PCs$x[,21]
PC22= PCs$x[,22]
PC23= PCs$x[,23]
PC24= PCs$x[,24]
PC25= PCs$x[,25]
PC26= PCs$x[,26]
PC27= PCs$x[,27]
PC28= PCs$x[,28]
PC29 = PCs$x[,29]
PC30= PCs$x[,30]
PC31= PCs$x[,31]
PC32= PCs$x[,32]
PC33= PCs$x[,33]
PC34= PCs$x[,34]
PC35= PCs$x[,35]
PC36= PCs$x[,36]
PC37= PCs$x[,37]
PC38= PCs$x[,38]
PC39= PCs$x[,39]
PC40= PCs$x[,40]
PC41= PCs$x[,41]
PC42= PCs$x[,42]
PC43= PCs$x[,43]
PC44= PCs$x[,44]
PC45= PCs$x[,45]
PC46= PCs$x[,46]
PC47= PCs$x[,47]
PC48= PCs$x[,48]
PC49= PCs$x[,49]
PC50= PCs$x[,50]

New_Data=data.frame(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11,PC12,PC13,PC14,PC15,PC16,PC17,PC18,PC19,PC20,PC21,PC22,PC23,
                    PC24,PC25,PC26,PC27,PC28,PC29,PC30,PC31,PC32,PC33,PC34,PC35,PC36,PC37,PC38,PC39,PC40,PC41,PC42,PC43,PC44,
                    PC45,PC46,PC47,PC48,PC49,PC50,Hospitals_Data$AGE,Hospitals_Data$CHARGES)
Hospitals_Train<-New_Data[1:38078,]
Hospitals_Test_X<-New_Data[38079:50110,]
Hospitals_Train$RETURN<-RETURN
num_rows=nrow(Hospitals_Train)
sample_obs=sample(num_rows,0.8*num_rows)
Hospitals_rest=Hospitals_Train[sample_obs,]
Hospitals_test=Hospitals_Train[-sample_obs,]
num_rows1=nrow(Hospitals_rest)
sample_obs1=sample(num_rows1,0.8*num_rows1)
Hospitals_train=Hospitals_rest[sample_obs1,]
Hospitals_valid=Hospitals_rest[-sample_obs1,]

#bagging logistic regression
num_rest=nrow(Hospitals_rest)
bootstrap_sample=sample(seq(1,num_rest),num_rest,replace=T)
B=50
bagged.log=vector("list", B)
for(i in 1:B){
  
  bootstrap_sample=sample(seq(1,num_rest),num_rest,replace=T)
  bagged.log[[i]]=glm(RETURN~.,data=Hospitals_rest[bootstrap_sample,],family="binomial")
}
#PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+
 # PC24+PC25+PC26+PC27+PC28+PC29+PC30+PC31+PC32+PC33+PC34+PC35+PC36+PC37+PC38+PC39+PC40+PC41+PC42+PC43+PC44+
  #PC45+PC46+PC47+PC48+PC49+PC50+AGE+CHARGES
bag_preds=vector("list", B)
for (i in 1:B){
bag_preds[[i]]=predict(bagged.log[[i]],newdata=Hospitals_test,type = 'response')
}

df=data.frame(bag_preds)
df
final_pred=rowMeans(df[,])
final_pred  
final_class=ifelse(final_pred>=0.38,1,0)
A=table(Hospitals_test$RETURN,final_class)
BAG_TPR=A[4]/(A[2]+A[4])

BAG_TPR
bagging_acc=sum(ifelse(final_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)
bagging_acc


#bag_preds_Test=vector("list", B)
#for (i in 1:B){
 # bag_preds_Test[[i]]=predict(bagged.log[[i]],newdata=Hospitals_Test_X,type = 'response')}

#df=data.frame(bag_preds_Test)
#df
#final_pred_Test=rowMeans(df[,])
#final_pred_Test  
#final_class=ifelse(final_pred_Test>=0.5,1,0)

#write.csv(final_class,file = 'submission_TEAM_19.csv')




#logistic
Log_model <- glm(RETURN~.,data=Hospitals_train, family="binomial")
Log_preds=predict(Log_model,newdata=Hospitals_test,type="response")
Log_class=ifelse(Log_preds>0.4,1,0)
Log_confuse_test=table(Hospitals_test$RETURN,Log_class,dnn=c('Actual','Predicted'))
Log_confuse_test
Log_acc=(Log_confuse_test[1])/sum(Log_confuse_test)
Log_acc

#LASSO
library(glmnet)
Hospitals_rest_X=Hospitals_rest[,-53]
Hospitals_test_X=Hospitals_test[,-53]
Hospitals_valid_X=Hospitals_valid[,-53]
Hospitals_rest_X <- model.matrix( ~ .-1, Hospitals_rest[,c(1:52)])
Hospitals_test_X=model.matrix( ~ .-1, Hospitals_test[,c(1:52)])
Hospitals_valid_X=model.matrix( ~ .-1, Hospitals_valid[,c(1:52)])
glmnet_lasso = glmnet(Hospitals_rest_X, Hospitals_rest$RETURN, family = 'binomial', alpha = 1)
glmnet_lasso.cv = cv.glmnet(Hospitals_rest_X,Hospitals_rest$RETURN,family="binomial",alpha=1)
plot(glmnet_lasso.cv)
best.lambda1=glmnet_lasso.cv$lambda.min
best.lambda1
predict(glmnet_lasso,s=best.lambda1,type="coefficients")
lasso_probs = predict(glmnet_lasso,s=best.lambda1,newx=Hospitals_test_X,type="response")
lasso_class = ifelse(lasso_probs>0.4,1,0)
B=table(Hospitals_test$RETURN,lasso_class)

LASS0_TPR=B[4]/(B[2]+B[4])
LASS0_TPR
lasso_acc=sum(ifelse(lasso_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)
lasso_acc

#rigde
glmnet_ridge = glmnet(Hospitals_rest_X, Hospitals_rest$RETURN, family = 'binomial', alpha = 0)
glmnet_ridge.cv = cv.glmnet(Hospitals_rest_X,Hospitals_rest$RETURN,family="binomial",alpha=0)
plot(glmnet_ridge.cv)
best.lambda2=glmnet_ridge.cv$lambda.min
best.lambda2
predict(glmnet_ridge,s=best.lambda2,type="coefficients")
ridge_probs = predict(glmnet_ridge,s=best.lambda2,newx=Hospitals_test_X,type="response")
ridge_class = ifelse(ridge_probs>0.4,1,0)
C=table(Hospitals_test$RETURN,ridge_class)
RIDGE_TPR=C[4]/(C[2]+C[4])
RIDGE_TPR
ridge_acc=sum(ifelse(ridge_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)
ridge_acc

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
backward_class=ifelse(backward_pre_both1>0.5,1,0)
forward_class=ifelse(forward_pre_both1>0.5,1,0)
backward_table=table(Hospitals_test$RETURN,backward_class)
forward_table=table(Hospitals_test$RETURN,forward_class)
BACKWORD_ACC=(backward_table[1,1]+backward_table[2,2])/sum(backward_table)
FORWARD_ACC=(forward_table[1,1]+forward_table[2,2])/sum(forward_table)
BACKWARD_TPR=backward_table[4]/(backward_table[2]+backward_table[4])
FORWARD_TPR=forward_table[4]/(forward_table[2]+forward_table[4])
#Write the prediction for testing data in csv(the format of the csv may be wrong,we should
#change the format with adding column name and reindex by ourselevs directly in csv file)
#backward_pre_both2=predict(backward_Hospitals_both,newdata=Hospitals_Test_X,type='response')
#backward_class1=ifelse(backward_pre_both2>0.5,1,0)
#Class=ifelse(backward_class1>0.5,'Yes','No')
#write.csv(Class,file = 'submission_TEAM_19.csv')


library(randomForest)
library(tree)
ncol(Hospitals_rest)
bag.trees=randomForest(Hospitals_rest$RETURN~.,data=Hospitals_rest,ntree=50,importance=TRUE)
bag_preds=predict(bag.trees,newdata=Hospitals_test,type="prob")
bag_probs=bag_preds[,2]
bag_class=ifelse(bag_probs>0.45,1,0)
E=table(Hospitals_test$RETURN,bag_class)
BAG_TREE_TPR=E[4]/(E[4]+E[2])
BAG_TREE_TPR
sum(ifelse(bag_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)

#bag_preds1=predict(bag.trees,newdata=Hospitals_Test_X,type="prob")
#bag_probs1=bag_preds1[,2]
#bag_class1=ifelse(bag_probs1>0.5,1,0)
#Class=ifelse(bag_class1>0.5,'Yes','No')
#write.csv(Class,file = 'submission_TEAM_19.csv')


#random forest
rf.trees=randomForest(Hospitals_rest$RETURN~.,data=Hospitals_rest,ntree=50,importance=TRUE,mtry=5)
rf_preds=predict(rf.trees,newdata=Hospitals_test,type="prob")
rf_probs=rf_preds[,2]
rf_class=ifelse(rf_probs>0.45,1,0)
F=table(Hospitals_test$RETURN,rf_class)
sum(ifelse(rf_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)
RF_TPR=F[4]/(F[4]+F[2])
RF_TPR
#boosting
library(gbm)
boost.hospitalS=gbm(Hospitals_rest$RETURN~.,data=Hospitals_rest,distribution="bernoulli",n.trees=50)
boost_test_pred=predict(boost.hospitalS,newdata=Hospitals_test,n.trees=50,type="response")
boost_test_pred
boost_class=ifelse(boost_test_pred>0.4,1,0)
boost_class
G=table(Hospitals_test$RETURN,boost_class)
BOO_TPR=G[4]/(G[2]+G[4])
sum(ifelse(boost_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)


#knn
Hospitals_train_x<-scale(Hospitals_train[1:52])
Hospitals_valid_x<-scale(Hospitals_valid[1:52])
Hospitals_test_x<-scale(Hospitals_test[1:52])
Hospitals_train_y<-as.numeric(Hospitals_train$RETURN)
Hospitals_valid_y<-as.numeric(Hospitals_valid$RETURN)
Hospitals_test_y<-as.numeric(Hospitals_test$RETURN)
library(class)


knn_valid_acc<-vector()
n=1
for (i in c(1:200)){
  knn_pred=knn(Hospitals_train_x,Hospitals_valid_x,Hospitals_train_y,k=i)
  knn_table=table(Hospitals_valid_y,knn_pred)
  knn_valid_acc[n]= (knn_table[1,1]+knn_table[2,2])/sum(knn_table)
  n=n+1
}
knn_valid_acc
knn_test_pred=knn(Hospitals_train_x,Hospitals_test_x,Hospitals_train_y,k=84)
knn_table=table(Hospitals_test_y,knn_test_pred)
knn_test_accuracy=(knn_table[1,1]+knn_table[2,2])/sum(knn_table)
knn_test_accuracy


#xgboost
library(xgboost)
labels_valid=as.numeric(Hospitals_valid$RETURN)-1
labels_train=as.numeric(Hospitals_train$RETURN)-1
xg_train=model.matrix( ~ .-1, Hospitals_train[1:21])
xg_valid_x<-model.matrix( ~ .-1, Hospitals_valid[1:21])
xg_test_x<-model.matrix( ~ .-1, Hospitals_test[1:21])

xg_valid_accuracy<-vector()
n=1
for (i in c(6:10)){
  bst=xgboost(data=xg_train,label=labels_train,max.depth=i,eta=0.2,nround=200,objective='binary:logistic')
  xg_valid_pred=predict(bst,xg_valid_x)
  xg_class=ifelse(xg_valid_pred>0.5,1,0)
  xg_valid_accuracy[n]= sum(ifelse(xg_class==Hospitals_valid$RETURN,1,0))/nrow(Hospitals_valid)
  n=n+1
}
xg_valid_accuracy
bst=xgboost(data=datahospital,label=labelss,max.depth=8,eta=0.1,nround=200,objective='binary:logistic')
xg_test_pred=predict(bst,xg_test_x)
xg_class=ifelse(xg_test_pred>0.5,1,0)
table(Hospitals_test$RETURN,xg_class)
sum(ifelse(xg_class==Hospitals_test$RETURN,1,0))/nrow(Hospitals_test)

