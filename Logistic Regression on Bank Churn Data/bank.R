#Build a classification model using logistic regression to predict the credibility of the customer, in order to minimize the risk and maximize the profit of German Credit Bank.

#Algorithm :Logistic Regeration
#dataset:Bank credit card fraud detection

library(corrplot)
library(caret)
library(ROCR)
#Read file
path="D:/R/Logistic/Dataset/BankCreditCard.csv"

bank=read.csv(path,header = T)

head(bank)
ncol(bank)
nrow(bank)

str(bank)
View(bank)
summary(bank)

#Removing unwanted feature
bank$Customer.ID=NULL

#cheaking for levels of features
for (c in colnames(bank))
{
  print(paste("levels of",c))
       print(levels(factor(bank[c])))
       print("----------------------")
}

#making features factor
bank$Gender=as.factor(bank$Gender)
bank$Marital=as.factor(bank$Marital)


bank$Age_Years=as.numeric(bank$Age_Years)
str(bank)
#making Y var as categorical data
bank$Default_Payment=as.factor(bank$Default_Payment)

str(bank)

#seperate out numerical and categorical columns
cat_cols=colnames(bank)[unlist(lapply(bank,is.factor))]

num_cols=colnames(bank)[unlist(lapply(bank,is.numeric))]

#checking data for nulls
checknulls=function(x)return(any(is.na(x)))
c=colnames(bank)[unlist(lapply(bank,checknulls))]
print(c)

#cheacking data for zeros
checkzeros=function(x) return(any(x==0))
z=colnames(num_cols)[unlist(lapply(num_cols,checkzeros))]
print(z)

#checking levels of categorical data
for(c in cat_cols)
{
  print(paste("levels of",c))
  print(levels(factor(unlist(bank[c]))))
  print("-------------------")
}

#correcting levels of Marital from category '0' which is undefined to '3'-prefer not to say
bank$Marital[bank$Marital==0]=3
levels(factor(unlist(bank$Marital)))

#checking multicolinearity
corr=cor(bank[num_cols])
corrplot(corr,method = "number",type = "lower")

#multicollerity exist in

  #Feb_Bill_Amount x Jan_Bill_Amount =0.84
  #march_Bill_Amount x Jan_Bill_Amount= 0.86
  #march_Bill_Amount x Feb_Bill_Amount = 0.85
  #April_bill_amount x Jan_Bill_Amoun = 0.82
  #April_bill_amount x Feb_Bill_Amount = 0.83
  #April_bill_amount x march_Bill_Amount = 0.91
  #May_bill_amount x Jan_Bill_Amount  = 0.76
  #May_bill_amount x Feb_Bill_Amount  =0.84
  #May_bill_amount x march_Bill_Amount =0.84
  #May_bill_amount x April_bill_amount =0.91
  #June_bill_amount x Jan_Bill_Amount =0.76
  #June_bill_amount x Feb_Bill_Amount =0.80
  #June_bill_amount x march_Bill_Amount =0.82
  #June_bill_amount x April_bill_amount =0.86
  #June_bill_amount x May_bill_amount =0.95

#checking proportion of class in Dependent veriable
prop.table(table(bank$Default_Payment))


##spliting data ##ensure that data grouping is not exist otherwise model will appear as biased
rows=nrow(bank)
s=sample(seq(1:rows),0.7*rows)
train=bank[s,]
test=bank[-s,]

print(paste("train",nrow(train),"test",nrow(test)))

##validation should be done that data classified properly
prop.table(table(bank$Default_Payment))
prop.table(table(train$Default_Payment))
prop.table(table(test$Default_Payment))

lvl_tr=length(levels(factor(train$Default_Payment)))
lvl_te=length(levels(factor(test$Default_Payment)))

if (lvl_tr>=lvl_te)
  print('levels are OK')else
    print('levels are more in Testing')

#building a base model
m1=glm(Default_Payment~.,data = train,binomial(link = "logit"))

summary(m1)

p1=predict(m1,test,type="response")

#taking cut off to 0.5
length(p1[p1<=0.5])
length(p1[p1>0.5])

#converting likelyhood to classes of  0 to 1
pred1=ifelse(p1<=0.5,0,1)

print(pred1[1:10])

cbind(p1[1:10],pred1[1:10])


confusionMatrix(test$Default_Payment,factor(pred1),positive = "0")

#ROC/AUC
#identifying optimum cutoff value by ploting ROC

predi1=prediction(p1,test$Default_Payment)
evals=performance(predi1,"acc")
plot(evals)
max_yval1=which.max(slot(evals,"y.values")[[1]])
max_yval1
max_acc1=slot(evals,"y.values")[[1]][max_yval1]
max_acc1

max_cutoff1=slot(evals,"x.values")[[1]][max_yval1]
max_cutoff1

print(paste("Best accuracy=",round(max_acc1,4),
            "Best cutoff=",round(max_cutoff1,4)))

#plot ROC
perf1=performance(predi1,"tpr","fpr")
plot(perf1,colorize=T,main="ROC",ylab="sensitivity",xlab="1-specificity")

#area under curve
auc1=performance(predi1,"auc")
round(unlist(slot(auc1,"y.values")),3)

#taking cut off 0.4316
pred2=ifelse(p1<=0.4316,0,1)

confusionMatrix(test$Default_Payment,factor(pred2),positive = "0")
prop.table(table(test$Default_Payment))

#oversampling to tackle class imbalance
library(ROSE)

row=nrow(bank)+5000

over_dp=ovun.sample(Default_Payment~.,data = bank,method = "over",N=rows,seed = 1)$data

prop.table(table(over_dp$Default_Payment))

#feature selection
step(m1)

#choosing features on basis of AIC 
#building a model
m2=glm(Default_Payment ~ Credit_Amount + Gender + Academic_Qualification + 
         Marital + Age_Years + Repayment_Status_Jan + Repayment_Status_Feb + 
         Repayment_Status_March + Repayment_Status_April + Repayment_Status_May + 
         Repayment_Status_June + Feb_Bill_Amount + April_Bill_Amount + 
         Previous_Payment_Jan + Previous_Payment_Feb + Previous_Payment_April + 
         Previous_Payment_May,data = train,binomial(link = "logit"))

summary(m2)

p2=predict(m2,test,type="response")

#taking cut off to 0.5
length(p2[p2<=0.5])
length(p2[p2>0.5])

#converting likelyhood to classes of  0 to 1
pred2=ifelse(p2<=0.5,0,1)

print(pred2[1:10])

cbind(p2[1:10],pred2[1:10])


confusionMatrix(test$Default_Payment,factor(pred2),positive = "0")

#ROC/AUC
#identifying optimum cutoff value by ploting ROC

predi2=prediction(p2,test$Default_Payment)
evals=performance(predi2,"acc")
plot(evals)
max_yval1=which.max(slot(evals,"y.values")[[1]])
max_yval1
max_acc1=slot(evals,"y.values")[[1]][max_yval1]
max_acc1

max_cutoff1=slot(evals,"x.values")[[1]][max_yval1]
max_cutoff1

print(paste("Best accuracy=",round(max_acc1,4),
            "Best cutoff=",round(max_cutoff1,4)))

#plot ROC
perf1=performance(predi1,"tpr","fpr")
plot(perf1,colorize=T,main="ROC",ylab="sensitivity",xlab="1-specificity")

#area under curve
auc1=performance(predi1,"auc")
round(unlist(slot(auc1,"y.values")),3)

#taking cut off 0.0.4731
pred2=ifelse(p2<=0.4731,0,1)

confusionMatrix(test$Default_Payment,factor(pred2),positive = "0")

##Model2 with significant features given ACCURACY=0.8273, Pos Pred Value : 0.9560 Neg Pred Value : 0.3621 
#which is close but better than base model