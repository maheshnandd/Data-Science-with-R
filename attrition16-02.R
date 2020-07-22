###Predicting Employee Attrition
library(caret)
library(corrplot)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(class)
library(e1071)
library(ROCR)
library(pROC)
library(data.table)
#Read Data file
path="D:/R/main project/R Project - Attrition/Attrition.csv"
attrition_data = read.csv(path,header = T)

View(attrition_data)
str(attrition_data)
colnames(attrition_data)
nrow(attrition_data)
dim(attrition_data)

                                #DATA EXPLORATION#

summary(attrition_data)
#converting categorical column  to factor
#veriable needs to convert to categorical feature
cat = list('Education', 'EnvironmentSatisfaction','WorkLifeBalance','StockOptionLevel','RelationshipSatisfaction','PerformanceRating','EmployeeCount','JobInvolvement','JobLevel','JobSatisfaction','EnvironmentSatisfaction','StandardHours')

for(c in cat)
{
  attrition_data[,c] = as.factor(attrition_data[,c])
    }
str(attrition_data)

#Deleting categorical veriable with level==1

table(attrition_data$EmployeeCount)
attrition_data$EmployeeCount = NULL
table(attrition_data$Over18)
attrition_data$Over18 = NULL
table(attrition_data$StandardHours)
attrition_data$StandardHours = NULL

#Colnames of Categorical and Numerical Veriables in data
cat_cols = colnames(attrition_data)[sapply(attrition_data,is.factor)]

num_cols = colnames(attrition_data)[sapply(attrition_data,is.numeric)]

#checking for multicollinerity across numerical veriable
corr=cor(attrition_data[,num_cols])
corrplot(corr,method = "number",type = "lower")


#checking for Nulls 
nulls = function(x) return (any(is.na(x)))
null = colnames(attrition_data)[sapply(attrition_data,nulls)]
print(null)

#OR
colSums(is.na(attrition_data))

#checking for Zeros
checkzeros=function(x) return(any(x==0))
zeros = num_cols[apply(attrition_data[,num_cols],2,checkzeros)]
print(zeros)
# No Significant Zeros found

#Distrtibution using histogram

for (c in num_cols)
{
  title=paste("histogram for",c)
  hist(unlist(attrition_data[,c]),main=title,col='yellow')
}

#checking for outliers
for(c in num_cols)
{
  msg=paste("boxplot for",c)
  boxplot(attrition_data[,c],main=msg,horizontal = T)
}
##No significant outliers

#checking classes distribution in Target Veriabel data
100*prop.table(table(attrition_data$Attrition))





                      # PLOTING DATA/VISULISATION #

#DISTRIBUTION OF NUMERICAL/CONTINOUS DATA ACROSS cLASS OF "Attrition"
#Arrenging data
plot_attrition = melt(attrition_data[,c(num_cols,'Attrition')],id.var='Attrition')
head(plot_attrition)
tail(plot_attrition)

#Plotting Data
ggplot(data = plot_attrition, aes(x= variable,y=value))+theme_bw()+ geom_boxplot(aes(fill=Attrition))+ facet_wrap(~variable,scales = 'free')


#DISTRIBUTION OF cATEGORICAL DATA ACROSS  CLASS OF ATTRITION
#plot(table(attrition_data$Attrition,attrition_data$BusinessTravel),xlab='Attrition',ylab='Business',main= 'Attrition Vs. BusinessTravel')

#Attrition rates by Gender
ggplot(attrition_data, aes(x =Gender,fill =Attrition)) + theme_bw() + geom_bar()
#Attrition rates by Business travel
ggplot(attrition_data, aes(x = BusinessTravel,fill = Attrition))+theme_bw() +geom_bar()
#Attrition rates by Department
ggplot(attrition_data,aes( x = Department,fill = Attrition))+theme_bw()+geom_bar()
##Attrition rates by Education
ggplot(attrition_data,aes(x=Education,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by EducationField
ggplot(attrition_data,aes(x= EducationField,fill= Attrition))+theme_bw()+geom_bar()
#Attrition rates by EnvironmentSatisfaction
ggplot(attrition_data,aes(x=EnvironmentSatisfaction,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by JobInvolvement
ggplot(attrition_data,aes(x=JobInvolvement,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by JobLevel
ggplot(attrition_data,aes(x=JobLevel,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by JobRole
ggplot(attrition_data,aes(x=JobRole,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by JobSatisfaction
ggplot(attrition_data,aes(x=JobSatisfaction,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by MaritalStatus
ggplot(attrition_data,aes(x=MaritalStatus,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by OverTime
ggplot(attrition_data,aes(x=OverTime,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by PerformanceRating
ggplot(attrition_data,aes(x=PerformanceRating,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by RelationshipSatisfaction
ggplot(attrition_data,aes(x=RelationshipSatisfaction,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by StockOptionLevel
ggplot(attrition_data,aes(x=StockOptionLevel,fill = Attrition))+theme_bw()+geom_bar()
#Attrition rates by WorkLifeBalance
ggplot(attrition_data,aes(x=WorkLifeBalance,fill = Attrition))+theme_bw()+geom_bar()


#shuffling data
attrition_data = attrition_data[order(sample(1:nrow(attrition_data),nrow(attrition_data))),]

#spliting data
set.seed(101)
samp=sample(seq(1:nrow(attrition_data)),0.7*nrow(attrition_data))
train=attrition_data[samp,]
test=attrition_data[-samp,]


#checking levels of class are present in train and test data
lv_attrition=levels(factor(attrition_data$Attrition))
lv_tr=length(levels(factor(train$Attrition)))
lv_ts=length(levels(factor(test$Attrition)))
if (lv_tr<lv_ts)
  print("levels are not good")else
    print("levels are good")


#checking proportion of class in train and test data
prop.table(table(attrition_data$Attrition))
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))

                                        #MODEL BUILDING#

#Algo:logistic regeration
set.seed(101)
#model1
m_log=glm(Attrition~.,data = train,binomial(link = "logit"))
summary(m_log)

#prediction for test data
baseprediction = predict(m_log1,test,type = "response")
baseprediction[1:20]

#converting baseprediction to class of 'Yes' 'No' by assuming threshold value as 0.5
pred_log_0.5=ifelse(baseprediction<=0.5,"No","Yes")

tab_log_0.5 = confusionMatrix(test$Attrition,factor(pred_log_0.5),positive = "No")
tab_log_0.5


#Identifying the best cutoff by ploting ROC 

preds_log=prediction(baseprediction,test$Attrition)


#Evaluating Accuracy for Different Cutoff
eval=performance(preds_log,"acc")
plot(eval) #ploting accuracy v/s Cutoff
max_yval=which.max(slot(eval,"y.values")[[1]])
max_acc=slot(eval,"y.values")[[1]][max_yval]
max_cutoff = slot(eval,"x.values")[[1]][max_yval]
print(paste("max accuracy",round(max_acc,3),"max cutoff",round(max_cutoff,7)))
#max accuracy 0.887 max cutoff 0.6195648
pred_log_maxacc=ifelse(baseprediction<=max_cutoff,"No","Yes")
tab_log_maxacc = confusionMatrix(test$Attrition,factor(pred_log_maxacc),positive = "No")
tab_log_maxacc
#for best Accuracy cutoff = 0.6195648, max accuracy = 0.887,Sensitivity = 0.8946,Specificity = 0.7576 
#Plot ROC
perf_log=performance(preds,"tpr","fpr")
plot(perf_log,colorize=T, main="ROC Curve",print.cutoffs.at = seq(0,1,.1), ylab = "Sensitivity", xlab = "1-Specificity")
abline(a=0,b=1)

#Best Cutoff from ROCR is 0.3
pred_log_best = ifelse(baseprediction<=0.3,'No','Yes')
tab_log_best= confusionMatrix(test$Attrition,factor(pred_log_best),positive = 'No')
tab_log_best
# From ROCR best Cutoff = 0.3, Accuracy = .8617, Sensitivity= 0.938,Specificity : 0.5542
#
#AUC
AUC_m_log=performance(preds_log,"auc")
round(unlist(slot(AUC_m_log,"y.values")),4)
#AUC= 0.8575
#AIC: 666.94
#FOR LOGISTIC REGRESSION MODEL WITH ALL VERIABLE 
#when cutoff =0.50   Accuracy : 0.8798  Sensitivity : 0.9056  Specificity : 0.6735
#when cutoff =0.61   Accuracy : 0.8798  Sensitivity : 0.9015  Specificity : 0.6889*** #for best Accuracy
#when cutoff =0.30   Accuracy : 0.8617  Sensitivity : 0.9282  Specificity : 0.5570 # from ROCR

Accuracy_mlog = 0.8798;Sensitivity_mlog = 0.9015;Specificity_mlog = 0.6889


#Selecting important Features for model based on p-value,in order to reduce the complexity of the model from summery all veriables having p.value less than 0.05 taken as important feature for the model.
m_log_summery = as.data.frame.matrix(summary(m_log)$coef)
imp_features_log= m_log1_summery[m_log1_summery$`Pr(>|z|)`<=0.05,]

imp_features_log_names=c("Age","BusinessTravel","DailyRate","DistanceFromHome","EmployeeNumber","EnvironmentSatisfaction","Gender","JobInvolvement","JobLevel","JobRole","JobSatisfaction","MaritalStatus","NumCompaniesWorked","OverTime","RelationshipSatisfaction","TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")

#subsetting train test data as important features
imp_log_train = train[,c('Attrition',imp_features_log_names)]
imp_log_test  = test[,c('Attrition',imp_features_log_names)]

#model2 Based on important features

m_log_impvar = glm(Attrition~., data = imp_log_train, binomial(link = 'logit'))
summary(m_log_impvar)

#prediction for test data
baseprediction_impvar = predict(m_log_impvar,imp_log_test,type = "response")
baseprediction_impvar[1:20]

#converting baseprediction to class of 'Yes' 'No' by assuming threshold value as 0.5
pred_impvar_0.5 = ifelse(baseprediction2 <=0.5,"No","Yes")
tab_imp_var0.5 = confusionMatrix(imp_log_test$Attrition,factor(pred2_0.5),positive = "No")
tab_imp_var0.5 


#Evaluating Accuracy for Different Cutoff
preds_log_impvar =prediction(baseprediction_impvar,imp_log_test$Attrition)
eval=performance(preds_log_impvar,"acc")
plot(eval) #ploting accuracy v/s Cutoff
max_yval=which.max(slot(eval,"y.values")[[1]])
max_acc_impvar=slot(eval,"y.values")[[1]][max_yval]
max_cutoff_impvar = slot(eval,"x.values")[[1]][max_yval]
print(paste("max accuracy",round(max_acc_impvar,3),"max cutoff",round(max_cutoff_impvar,7)))
#max accuracy 0.8889 max cutoff 0.5574558
pred_log_impvar_maxacc=ifelse(baseprediction_impvar<=max_cutoff_impvar,"No","Yes")
tab_log_impvar_maxacc = confusionMatrix(test$Attrition,factor(pred_log_impvar_maxacc),positive = "No")
tab_log_impvar_maxacc

#Identifying the best cutoff by ploting ROC 
preds_imp_var = prediction(baseprediction_impvar,imp_log_test$Attrition)
perf_impvar= performance(preds_imp_var,'tpr','fpr')
plot(perf_impvar,colorize=T, main="ROC Curve",print.cutoffs.at = seq(0,1,.1), ylab = "Sensitivity", xlab = "1-Specificity")

#from ROC best cutoff is 0.4
pred_log_impvar_best = ifelse(baseprediction2<=0.4,"No","Yes") 

tab_log_impvar_best = confusionMatrix(imp_log_test$Attrition,factor(pred_log_impvar_best),positive = 'No')
tab_log_impvar_best

# From ROCR best Cutoff = 0.3, Accuracy = .8571, Sensitivity= 0.930,Specificity : 0.5301
#
#AUC
AUC_log_impvar = performance(preds_imp_var,"auc")
round(unlist(slot(AUC_log_impvar,"y.values")),4)
#AUC= 0.8504
#AIC: 640.25
#FOR LOGISTIC REGESSTION WITH IMPORTANT VERIABLE 
#WHEN CUTOFF :0.5 Accuracy : 0.8889  Sensitivity : 0.9045 Specificity : 0.7442   
#WHEN CUTOFF :0.557 Accuracy : 0.8889  Sensitivity : 0.8985 Specificity : 0.7838 for maxximum acc
#WHEN CUTOFF :0.4   Accuracy : 0.8503 Sensitivity : 0.9103  Specificity : 0.6863***
Accuracy_mlog_impvar = 0.8503; Sensitivity_mlog_impvar = 0.9103 ; Specificity_mlog_impvar = 0.6863


#--------------------------------------------------------------------------------------#

#algo:Decision Tree
set.seed(30001)
#model1 WITH ALL VERIABLE
m_dtree = rpart(Attrition~.,data= train,method = "class")
m_dtree
table(train$Attrition)

# pictorial representation of DT model
rpart.plot(m_dtree,type = 4,box.palette = "GnBu",branch.lty=3,shadow.col = "gray",nn=T)

#OR

plot(m_dtree,margin = 0.1)
text(m_dtree,use.n = TRUE,pretty = TRUE,cex=0.8)


# Base predictions
pred1_dtree =predict(m_dtree,test,type="class")
head(pred1_dtree,20)

#confusion matrix
tab_dt1=confusionMatrix(test$Attrition,pred1_dtree)
tab_dt1

#ploting Roc curve
pred1_dtree_prob = predict(m_dtree,newdata = test,type = 'prob')
head(pred1_dtree_prob,20)
preds_dtree1= prediction(pred1_dtree_prob[,2],test$Attrition)
perf_dt= performance(preds_dtree1,'tpr','fpr')
plot(perf_dt,colorize=T, main="ROC Curve",print.cutoffs.at = seq(0,1,0.1), ylab = "Sensitivity", xlab = "1-Specificity",text.adj =c(-0.2,1.7))

#form ROCR best cutoff 0.4
pred1_dtree_0.4= ifelse(pred1_dtree_prob[,2]<=0.4,"No","Yes")
head(pred1_dtree_0.4)
tab_dt1_0.4= confusionMatrix(test$Attrition,factor(pred1_dtree_0.4))
tab_dt1_0.4
#AUC
AUC_dtree =performance(preds_dtree1,"auc")
round(unlist(slot(AUC_dtree,"y.values")),2)
#AUC= 0.7
#FOR DECISION TREE MODELS WITH ALL VERIABLE
# Accuracy = 0.8367,Sensitivity = 0.8585, Specificity = 0.4583***
Accuracy_dtree = 0.8367;Sensitivity_dtree = 0.8585; Specificity_dtree = 0.4583



#fEATURE SELECTION
#Reducing complexity on basis of feature selection of important veraible
sort(m_dtree$variable.importance,decreasing = TRUE)[1:12]
dtree_imp_vars <- names(sort(m_dtree$variable.importance,decreasing = TRUE)[1:12])

#subsetting train and test data
imp_dtree_train = train[,c('Attrition',dtree_imp_vars)]
imp_dtree_test  = test[,c('Attrition',dtree_imp_vars)]

#model2 WITH IMPORTANT VERIABLE
m_dtree_impvar = rpart(Attrition~.,data = imp_dtree_train,method = "class")
m_dtree_impvar

#prediction
pred2_dtree_impvar= predict(m_dtree_impvar,imp_dtree_test,type = "class")
head(pred2_dtree_impvar)
#Confusion Matrix
tab_dt2_impvar = confusionMatrix(imp_dtree_test$Attrition,pred2_dtree_impvar)
tab_dt2_impvar

#ploting Roc curve
pred2_dtree_impvar_prob = predict(m_dtree_impvar,newdata = imp_dtree_test,type = 'prob')
head(pred2_dtree_impvar_prob,20)

preds_dtree_impvar= prediction(pred2_dtree_impvar_prob[,2],imp_dtree_test$Attrition)

perf_dtree_impvar= performance(preds_dtree_impvar,'tpr','fpr')
plot(perf_dtree_impvar,colorize=T, main="ROC Curve",print.cutoffs.at = seq(0,1,0.1), ylab = "Sensitivity", xlab = "1-Specificity")

#form ROCR best cutoff 0.2
pred2_dtree_impvar_0.2= ifelse(pred2_dtree_impvar_prob[,2]<=0.2,"No","Yes")
head(pred2_dtree_impvar_0.2)
tab_dt2_impvar_0.2= confusionMatrix(test$Attrition,factor(pred2_dtree_impvar_0.2))
tab_dt2_impvar_0.2

#For dtree_impvar Accuracy = 0.83,Sensitivity = 0.87, Specificity = 0.4583
#AUC
AUC_dtree_impvar =performance(preds_dtree_impvar,"auc")
round(unlist(slot(AUC_dtree_impvar,"y.values")),4)
#AUC_dtree_impvar= 0.7022

#FOR DECISION TREE WITH IMPORTANT VERIABLES
#Accuracy : 0.839  Sensitivity : 0.8589 Specificity : 0.4783 
#Accuracy : 0.8367 Sensitivity : 0.8843 Specificity : 0.4808*** WHEN CUTOFF TAKEN FROM ROCR
Accuracy_dtree_impvar = 0.8367; Sensitivity_dtree_impvar = 0.8843; Specificity_dtree_impvar = 0.4808


#MODEL3 DECISION TREE PRUNED
#complexity parameter
#default value for cp=0.01
printcp(m_dtree)

plotcp(m_dtree)


#pruning the decision tree
#based on CP having the least cross-validation error
cp_value=m_dtree$cptable[which.min(m_dtree$cptable[,"xerror"])]
cp_value

#prune the tree based on cpvalue
m_dtree_prune=prune(m_dtree,cp_value)
m_dtree_prune

printcp(m_dtree_prune)
plotcp(m_dtree_prune)

pred_dtree_prune = predict(m_dtree_prune,test,type = 'class')
pred_dtree_prune

tab_dt3_prune= confusionMatrix(test$Attrition,pred_dtree_prune)
tab_dt3_prune

#ploting Roc curve
pred3_dtree_prune_prob = predict(m_dtree_prune,newdata = test,type = 'prob')
head(pred3_dtree_prune_prob,20)

preds_dtree_prune = prediction(pred3_dtree_prune_prob[,2],test$Attrition)

perf_dtree_prune= performance(preds_dtree_prune,'tpr','fpr')
plot(perf_dtree_prune,colorize=T, main="ROC Curve",print.cutoffs.at = seq(0,1,0.1), ylab = "Sensitivity", xlab = "1-Specificity")

#For dtree_prune Accuracy = 0.84,Sensitivity = 0.86, Specificity = 0.50
#AUC
AUC_dtree_prune =performance(preds_dtree_prune,"auc")
round(unlist(slot(AUC_dtree_prune,"y.values")),4)
#AUC_dtree_prune = 0.672

#FOR DECISION TREE PRUNED
Accuracy_dtree_pruned =   0.8413; Sensitivity_dtree_pruned = 0.8609;  Specificity_dtree_pruned = 0.50


#------------------------------------------------------------------------------------#

#Algo: Random Forest
#spliting data in veriable-X and target-Y form

train_x= train[,-2]
train_y= train[,2]

test_x = test[,-2]
test_y = test[,2]

#model
m_rf= randomForest(train_x,train_y)
m_rf

#predict
ped_rf= predict(m_rf,test_x)

#confusion matrix
tab_rf1= confusionMatrix(test_y,p_rf)

#finding best ntry for rf

bestmtry = tuneRF(train_x,train_y,ntreeTry = 600,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)

m2_rf_tuned = randomForest(train_x,train_y,mtry = 6, ntree = 600 )
m2_rf

p2_rf_tuned = predict(m2_rf_tuned, test_x)
p2_rf_tuned

tab2_rf_tuned= confusionMatrix(test_y,p2_rf_tuned)
tab2_rf_tuned

#ploting Roc curve
pred_rf_prob = predict(m_rf,newdata = test_x,type = 'prob')
head(pred_rf_prob,20)

preds_rf = prediction(pred_rf_prob[,2],test_y)

perf_rf= performance(preds_rf,'tpr','fpr')
plot(perf_rf,colorize=T, main="ROC Curve",print.cutoffs.at = seq(0,1,0.1), ylab = "Sensitivity", xlab = "1-Specificity")

#AUC
AUC_rf =performance(preds_rf,"auc")
round(unlist(slot(AUC_rf,"y.values")),4)
#AUC_rf = 0.8254

#Importance feature
importance(m_rf)
varImpPlot(m_rf)


#For RANDOM FOREST
Accuracy_rf = 0.86;Sensitivity_rf = 0.86 ; Specificity_rf = 0.81


                                                #MODEL SELECTION#


MODEL_SELECTION = data.frame(list("model_name" = c("Logistic Regression","LOGISTIC REGESSTION WITH IMPORTANT VERIABLE","DECISION TREE MODELS WITH ALL VERIABLE","DECISION TREE WITH IMPORTANT VERIABLES","DECISION TREE PRUNED","RANDOM FOREST"),
                "Sensitivity" = c(Sensitivity_mlog,Sensitivity_mlog_impvar,Sensitivity_dtree,Sensitivity_dtree_impvar,Sensitivity_dtree_pruned,Sensitivity_rf),
                "Specificity" = c(Specificity_mlog,Specificity_mlog_impvar,Specificity_dtree,Specificity_dtree_impvar,Specificity_dtree_pruned,Specificity_rf),
                "Accuracy" = c(Accuracy_mlog,Accuracy_mlog_impvar,Accuracy_dtree,Accuracy_dtree_impvar,Accuracy_dtree_pruned,Accuracy_rf)))
View(MODEL_SELECTION)



plot(roc(test$Attrition,baseprediction), print.auc=TRUE)
plot(roc(test$Attrition,baseprediction_impvar), print.auc = TRUE,col = "green", print.auc.y = .1, add = TRUE)
plot(roc(test$Attrition,pred1_dtree_prob[,2]), print.auc = TRUE,col = "blue", print.auc.y = .2, add = TRUE)
plot(roc(test$Attrition,pred2_dtree_impvar_prob[,2]), print.auc = TRUE,col = "red", print.auc.y = .3, add = TRUE)
plot(roc(test$Attrition,pred2_dtree_impvar_prob[,2]), print.auc = TRUE,col = "pink", print.auc.y = .4, add = TRUE)
plot(roc(test$Attrition,pred_rf_prob[,2]), print.auc = TRUE,col = "yellow", print.auc.y = .6, add = TRUE)



plot(roc(test$Attrition,))

#------------------------------------------------------
#Feature Engeenearing

#Feature Engeenearing

file="E:/Attrition1.csv"
attrition_data_FE=read.csv(file,header=T)
colnames(attrition_data_FE)
str(attrition_data_FE)
ncol(attrition_data_FE)
attrition_data_FE$SalesDept=attrition_data_FE$Department
colnames(attrition_data_FE)
table(attrition_data_FE$SalesDept)
typeof(attrition_data_FE$SalesDept)
attrition_data_FE$SalesDept=as.character(attrition_data_FE$SalesDept)
head(attrition_data_FE)
attrition_data_FE$SalesDept[attrition_data_FE$SalesDept=="Sales"]=1
attrition_data_FE$SalesDept[attrition_data_FE$SalesDept=="Human Resources"]=0
attrition_data_FE$SalesDept[attrition_data_FE$SalesDept=="Research & Development"]=0
attrition_data_FE$SalesDept=as.factor(attrition_data_FE$SalesDept)

head(attrition_data_FE)

#2

attrition_data_FE$JobInvCut=attrition_data_FE$JobInvolvement
attrition_data_FE$JobInvCut=ifelse(attrition_data_FE$JobInvCu<=2.5,"1","0")
attrition_data_FE$JobInvCut=as.factor(attrition_data_FE$JobInvCut)
head(attrition_data_FE)
colnames(attrition_data_FE)

#3
attrition_data_FE$MoovingPeople=attrition_data_FE$NumCompaniesWorked
attrition_data_FE$MoovingPeople=ifelse(attrition_data_FE$MoovingPeople>4,"1","0")
attrition_data_FE$MoovingPeople=as.factor(attrition_data_FE$MoovingPeople)
head(attrition_data_FE)
colnames(attrition_data_FE)

#4
attrition_data_FE$TotalSatisfaction_mean = (attrition_data_FE$RelationshipSatisfaction+attrition_data_FE$EnvironmentSatisfaction+attrition_data_FE$JobSatisfaction+attrition_data_FE$JobInvolvement+attrition_data_FE$WorkLifeBalance)/5
attrition_data_FE$NotSatif=attrition_data_FE$TotalSatisfaction_mean
attrition_data_FE$NotSatif=ifelse(attrition_data_FE$NotSatif<2.5,1,0)
attrition_data_FE$NotSatif=as.factor(attrition_data_FE$NotSatif)

head(attrition_data_FE)
colnames(attrition_data_FE)

#5
attrition_data_FE$LongDisJobS1=ifelse(attrition_data_FE$DistanceFromHome > 11 & attrition_data_FE$JobSatisfaction == 1 ,"1","0")
attrition_data_FE$LongDisJobS1=as.factor(attrition_data_FE$LongDisJobS1)

head(attrition_data_FE)
colnames(attrition_data_FE)

str(attrition_data_FE)
summary(attrition_data_FE)
#converting categorical column  to factor
#veriable needs to convert to categorical feature
cat = list('Education', 'EnvironmentSatisfaction','WorkLifeBalance','StockOptionLevel','RelationshipSatisfaction','PerformanceRating','EmployeeCount','JobInvolvement','JobLevel','JobSatisfaction','EnvironmentSatisfaction','StandardHours')


for(c in cat)
{
  attrition_data_FE[,c] = as.factor(attrition_data_FE[,c])
}

#Deleting categorical veriable with level==1

table(attrition_data_FE$EmployeeCount)
attrition_data_FE$EmployeeCount = NULL
table(attrition_data_FE$Over18)
attrition_data_FE$Over18 = NULL
table(attrition_data_FE$StandardHours)
attrition_data_FE$StandardHours = NULL

#Colnames of Categorical and Numerical Veriables in data
cat_cols = colnames(attrition_data_FE)[sapply(attrition_data_FE,is.factor)]

num_cols = colnames(attrition_data_FE)[sapply(attrition_data_FE,is.numeric)]

#shuffling data
attrition_data_FE = attrition_data_FE[order(sample(1:nrow(attrition_data_FE),nrow(attrition_data_FE))),]

#spliting data
set.seed(101)
samp=sample(seq(1:nrow(attrition_data_FE)),0.7*nrow(attrition_data_FE))
train=attrition_data_FE[samp,]
print(train)
test=attrition_data_FE[-samp,]
print(paste("train=",nrow(train),"test=",nrow(test)))


#checking levels of class are present in train and test data
lv_attrition=levels(factor(attrition_data_FE$Attrition))
lv_tr=length(levels(factor(train$Attrition)))
lv_ts=length(levels(factor(test$Attrition)))
if (lv_tr<lv_ts)
  print("levels are not good")else
    print("levels are good")


#checking proportion of class in train and test data
prop.table(table(attrition_data_FE$Attrition))
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))



train_x= train[,-2]
train_y= train[,2]

test_x = test[,-2]
test_y = test[,2]

#model
m_rf1= randomForest(train_x,train_y)
m_rf1

#predict
ped_rf1= predict(m_rf1,test_x)

#confusion matrix
tab_rf7= confusionMatrix(test_y,ped_rf1)
tab_rf7

#For RANDOM FOREST
Accuracy_rf1 = 0.85;Sensitivity_rf1 = 0.86 ; Specificity_rf1 = 0.63



#MODEL SELECTION#


MODEL_SELECTION = data.frame(list("model_name" = c("Logistic Regression","LOGISTIC REGESSTION WITH IMPORTANT VERIABLE","DECISION TREE MODELS WITH ALL VERIABLE","DECISION TREE WITH IMPORTANT VERIABLES","DECISION TREE PRUNED","RANDOM FOREST","RANDOM FOREST WITH FEATURE ENGEENEARING"),
                                  "Sensitivity" = c(Sensitivity_mlog,Sensitivity_mlog_impvar,Sensitivity_dtree,Sensitivity_dtree_impvar,Sensitivity_dtree_pruned,Sensitivity_rf,Sensitivity_rf1),
                                  "Specificity" = c(Specificity_mlog,Specificity_mlog_impvar,Specificity_dtree,Specificity_dtree_impvar,Specificity_dtree_pruned,Specificity_rf,Specificity_rf1),
                                  "Accuracy" = c(Accuracy_mlog,Accuracy_mlog_impvar,Accuracy_dtree,Accuracy_dtree_impvar,Accuracy_dtree_pruned,Accuracy_rf,Accuracy_rf1)))
View(MODEL_SELECTION)








