#Linear Discreminant analysis
#Dataset:WINE
library(ISLR)
library(corrplot)
library(caret)
library(MASS)

path= "D:/R/lda/Dataset/Wine.csv"
wine=read.csv(path,header = F)
View(wine)

#changing column names of dataframe
colnames= list("Type","Alcohol","Malicacid","Ash","Alcalinityofash","Magnesium","Totalphenols","Flavonoids","Nonflavanoidphenols","Proanthocyanins","Colorintensity","Hue","Dilution","Proline")
names(wine)=colnames
colnames(wine)

str(wine)

#making "Type" catrgorical veriable
wine$Type=as.factor(wine$Type)

#xcols
xcols=colnames(wine)[c(2:14)]

#outliers
for (c in xcols)
{
  msg= paste("boxplot for",c)
  boxplot(unlist(wine[c]),main=msg,horizontal = T)
}  
  #"Malicacid"
  #"Ash"
  #"Alcalinityofash"
  #"Magnesium"
  #"Proanthocyanins"
  #"Colorintensity"
  #"Hue"

#multicolinerity
corr=cor(wine[,xcols])
corrplot(corr,method = "number",type = "lower")
  #Flavonoids X Totalphenols=0.86

#shuffling data

wine=wine[order(sample(1:nrow(wine),nrow(wine))),]

#spliting data
rows=nrow(wine)
samp=sample(seq(1:rows),0.7*rows)
train=wine[samp,]
test=wine[-samp,]

#checking levels of data and proportion of classes
tr_class=length(levels(factor(wine$Type)))
te_class=length(levels(factor(wine$Type)))
if (tr_class<te_class)
print("levels are not good needs shuffuling data")else
print("levels are good")
 
prop.table(table(train$Type))
prop.table(table(test$Type))

#Base model building
m1=lda(Type~.,data=train)

m1

#verification of output of base model by comparing with corrosponding Group mean
mean(train$Alcohol[train$Type=="1"])
mean(train$Alcohol[train$Type=="2"])
mean(train$Alcohol[train$Type=="3"])

#prediction
p1=predict(m1,test)$class


#confusion matrix

confusionMatrix(test$Type,p1)
