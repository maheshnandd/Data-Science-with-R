##Project 3:Churn Analysis in Telecom Industry
#Algo:Decision Tree 

#library
library(rpart)

library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(caret)
library(corrplot)

#read file
path="D:/R/Decisiontree/Dataset/churn.csv"
Tchurn=read.csv(path,header = T)

head(Tchurn)
colnames(Tchurn)
nrow(Tchurn)
View(Tchurn)
str(Tchurn)
Tchurn$customerID=NULL

#correcting data in SeniorCitizen column
levels(factor(Tchurn$SeniorCitizen))  #showing  557 levels need to convert in to categorical data of two levels

s1=Tchurn$SeniorCitizen

Tchurn$SeniorCitizen=ifelse(s1<=0.5,0,1)      #converting data to category of 0 and 1 by taking cutoff 0.5
Tchurn$SeniorCitizen=as.factor(Tchurn$SeniorCitizen)

table(Tchurn$SeniorCitizen)

num_cols=colnames(Tchurn)[unlist(lapply(Tchurn,is.numeric))]
cat_cols=colnames(Tchurn)[unlist(lapply(Tchurn,is.factor))]


#nulls
checknull=function(x) return(any(is.na(x)))
c=colnames(Tchurn)[unlist(lapply(Tchurn,checknull))]
c                                                
      #no nulls#

#outliers
for(c in num_cols)
{
  msg= paste("boxplot for",c)
  boxplot(unlist(Tchurn[c]),main = msg,horizontal = T)
}

#outliers present in "TotalAmount" which can be considered as non outlier
length(Tchurn$TotalAmount[which((Tchurn$TotalAmount)>=7500)]) 


#checking for multicollinerity
corr=cor(Tchurn[num_cols])
corrplot(corr,method = "number",type = "lower")
    #multicollinerity exist in
    #totalAmount X tenure = 0.84

#Distribution of classes
table(Tchurn$Churn)

#shuffle the data
churn=Tchurn[order(sample(1:nrow(Tchurn),nrow(Tchurn))),]

#spliting data

t=nrow(Tchurn)
samp=sample(seq(1:t),0.7*t)
train=Tchurn[samp,]
test=Tchurn[-samp,]
print(paste("train=",nrow(train),"test=",nrow(test)))

ycount_tr= length(levels(factor(train$Churn)))
ycount_ts=length(levels(factor(test$Churn)))

if(ycount_tr<ycount_ts)
  print("lavels are not good")else
  print("levels are good")



#build the base model
m1= rpart(Churn~.,data = train, method = "class")

summary(m1)
m1

#Pictorial representation of DT
rpart.plot(m1,type = 4,extra = 101,box.palette = "GnBu",branch.lty=3,shadow.col = "gray",nn=T)


#Prediction by base model
p1=predict(m1,test,type = "class")
p1
summary(p1)

#ConfusionMatrix
confusionMatrix(test$Churn,p1)

table(test$Churn)

#complexity parameter
printcp(m1)

plotcp(m1)

#Pruning the decision tree
#Based on cp having less xerror
cp_value = m1$cptable[which.min(m1$cptable[,"xerror"])]

#prune the tree based on cp value
m1_prune=prune(m1,cp_value)

#prediction based on pruned tree model
p2=predict(m1_prune,test,type = "class")

#confusionmatrix
#pruned model VS actual
confusionMatrix(test$Churn,p2)





#building Model by removing multicollinearity veriable
m2=rpart(Churn~.-TotalAmount,data = train,method = "class")
m2
summary(m2)

p1=predict(m2,test,type = "class")

confusionMatrix(test$Churn,p1)

#prune the tree based on cp
cp_value=m2$cptable[which.min(m2$cptable[,'xerror'])]
m2_prune=prune(m2,cp_value)

p2=predict(m2_prune,test,type = "class")
confusionMatrix(test$Churn,p2)


#building model based on important features only
#feature selection
m1$variable.importance

m3= rpart(Churn~MonthlyServiceCharges+tenure+Agreement+InternetConnection+DeviceProtectionService+OnlineBackup+OnlineTV+OnlineMovies,data = train,method = "class")
m3

p1=predict(m3,test,type = "class")

confusionMatrix(test$Churn,p1)


m4=rpart(Churn~MonthlyServiceCharges+tenure+Agreement+InternetConnection+DeviceProtectionService,data = train, method = "class")
m4

p1=predict(m4,test,type = "class")
confusionMatrix(test$Churn,p1)

#pruning the tree based on cp value
cp_value=m4$cptable[which.min(m4$cptable[,"xerror"])]
cp_value

m4_prune=prune(m4,cp_value)

p2=predict(m4_prune,test, type = "class")


confusionMatrix(test$Churn,p2)

##Model 4 which is with important features and pruned gives ACCURACY = 0.7714 , Pos Pred Value : 0.7858 Neg Pred Value : 0.7535 
