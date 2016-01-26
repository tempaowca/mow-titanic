#### BIBLIOTEKI #################################################################################
install.packages("MASS")  #lda
install.packages("e1071") #naiveBayes
install.packages("rpart") #drzewa
install.packages("klaR")  #selekcja
install.packages("class") #knn
install.packages("stringr") #operacje na tekscie
install.packages("randomForest") #lasy

library(MASS)  #lda
library(e1071) #naiveBayes
library(rpart) #drzewa
library(klaR)  #selekcja
library(class) #knn
library(stringr) #operacje na tekscie
library(randomForest) #lasy

#### WCZYTANIE DANYCH ###########################################################################
TRAIN.DATA.DIR = paste(getwd(), "/data/train.csv", sep="")

trainOriginal <- read.csv(TRAIN.DATA.DIR)

#### ZIARNO #####################################################################################
set.seed(111)

#### FUNKCJE ####################################################################################

## NAPRAWA DANYCH **
newVariables <- function(data) {
  newData <- data[,c("Pclass", "Age", "Sex", "Parch", "SibSp", "Fare", "Embarked")]
  
  #BRAKUJACY WIEK ZASTAP MEDIANA WIEKU DLA PLCI
  newData$Age[is.na(newData$Age)==T & newData$Sex=="male"] <- median(newData$Age[newData$Sex=="male"],na.rm=TRUE )
  newData$Age[is.na(newData$Age)==T & newData$Sex=="female"] <- median(newData$Age[newData$Sex=="female"],na.rm=TRUE )
  
  #BRAKUJACA OPLATE ZASTAP MEDIANA DLA KLASY
  newData$Fare[newData$Fare==0 & newData$Pclass==1] <- median(newData$Fare[newData$Pclass==1 & newData$Fare>0],na.rm=T)
  newData$Fare[newData$Fare==0 & newData$Pclass==2] <- median(newData$Fare[newData$Pclass==2 & newData$Fare>0],na.rm=T)
  newData$Fare[newData$Fare==0 & newData$Pclass==3] <- median(newData$Fare[newData$Pclass==3 & newData$Fare>0],na.rm=T)
  
  newData$Embarked[newData$Embarked==""] <- "S" #najbardziej popularny port
  
  newData$Sex      <- as.factor(newData$Sex)
  newData$Embarked <- as.factor(newData$Embarked)
  
  return(newData)
}

## WARTOSCI Z TABELI POMYLEK **
# - np. step=0.01, many=T - wartosci na wielu poziomach (do krzywej roc)
# - np. step=0.5,  many=F - wartosci na jednym poziomie p-stwa (do podsumowania i porownania)
getMeasures <- function(p,y,step=0.01,many=TRUE) {
  if(many==TRUE) {
    m<-seq(0,1,by=step)
  } else {
    m<-step
  }
  n<-length(m)
  tp<-tn<-fp<-fn<-numeric(n)

  for (i in 1:n){
    tp[i]<-sum(p>=m[i] & y==1)
    fp[i]<-sum(p>=m[i] & y==0)
    fn[i]<-sum(p<m[i] & y==1)
    tn[i]<-sum(p<m[i] & y==0)
  }
return(data.frame(m=m,tp=tp,fp=fp,fn=fn,tn=tn))
}

## WSKAZNIKI **
# dokladnosc, precyzja, czulosc, specyficznosc
getIndicators <- function(m){
  acc<-(m$tp+m$tn)/(m$tp+m$fp+m$tn+m$fn)
  prec<-m$tp/(m$tp+m$fp)
  sens<-m$tp/(m$tp+m$fn)
  spec<-m$tn/(m$tn+m$fp)
  return(data.frame(acc=acc,prec=prec,sens=sens,spec=spec))
}

## KRZYWA ROC **
drawROC <- function(spec,sens){
  plot(1-spec,sens,type="s")
}

## WSKAZNIK AUC **
aucValue<-function(spec,sens){
  -sum(diff(1-spec)*sens[-length(sens)])
}


## WYLICZENIE MODELU **
getModel<-function(model,tr,lb,ts,additional=list(0)){
  if (model=="Bayes"){
    myModel<-naiveBayes(tr,lb)
    myPred<-predict(myModel,ts,type="raw")[,2]
  }
  if (model=="Tree"){
    if(is.null(additional$cp)){cpValue=0.0001}else{cpValue=additional$cp}
    myModel<-rpart(lb~.,tr, method = "class", control=rpart.control(cp=cpValue))
    myPred<-predict(myModel, ts, type="prob")[,2]
  }
  if (model=="Forest"){
    if(is.null(additional$ntree)){ntreeValue=100}else{ntreeValue=additional$ntree}
    myModel<-randomForest(tr, lb, ntree=ntreeValue)
    myPred<-predict(myModel, ts, type="prob")[,2]
  }
  if (model=="LDA"){
    myModel<-lda(lb~.,tr)
    myPred<-predict(myModel,ts)$posterior[,2]
  }
  if (model=="KNN"){
    if(is.null(additional$k)){kValue=3}else{kValue=additional$k}
    myModel<-NA
    myPred<- as.numeric(knn(tr[,c(1,2,4,5,6)],ts[,c(1,2,4,5,6)],lb,k=kValue,prob=T))-1
  }
  return(predictions<-myPred)
}

## KROSWALIDCJA **
crossValidate<-function(smp,ind,model,additional=list(0)){
  t<-numeric(0)
  t05<-numeric(0)
  for (i in 1:10){
    sub<-smp[(ind[i]+1):ind[i+1]]
    dTrain<-train[-sub,]
    dLabelTrain<-as.factor(trainOriginal$Survived[-sub])
    dTest<-train[sub,]
    dLabelTest<-as.factor(trainOriginal$Survived[sub])
    
    pModel<-getModel(model,dTrain,dLabelTrain,dTest,additional)
    
    if(length(t)==0){
      t <-getMeasures(pModel,dLabelTest)
      t05<-getMeasures(pModel,dLabelTest,0.5,F)
    }else{
      t <-t + getMeasures(pModel,dLabelTest)
      t05<-t05+getMeasures(pModel,dLabelTest,0.5,F)
    }
  }
  iModel<-getIndicators(t/10)
  i05<-getIndicators(t05/10)
  auc<-aucValue(iModel$spec,iModel$sens)
  
  return(list(model=model,basicInd=i05,auc=auc,fullSpec=iModel$spec, fullSens=iModel$sens))
  
}

#### MODELOWANIE ####################################################################################
train<-newVariables(trainOriginal)
## Zamiana parametrów nienumerycznych na numeryczne - niezbędne do wykonania algorytmu PCA
train$Sex <- ifelse(train$Sex == "male", 0, 1)
train$Embarked <- ifelse(train$Embarked == "S", 0, train$Embarked)

trainWithoutPCA <- train

## PCA
pcaTable <- princomp(train)
pcaTable <- train$scores
pcaTable <- as.data.frame(train)

smp<-sample(891,891,replace=FALSE)
ind<-c(0,seq(90,891,by=89))

## Bayes

## Bez PCA

mBayes<-crossValidate(smp,ind,"Bayes")
mBayes$basicInd
mBayes$auc
drawROC(mBayes$fullSpec, mBayes$fullSens); title("Krzywa ROC dla naiwnego Bayesa")

## PCA bez jednej kolumny

train <-pcaTable
train[[7]] <- NULL

mBayes<-crossValidate(smp,ind,"Bayes")
mBayes$basicInd
mBayes$auc
drawROC(mBayes$fullSpec, mBayes$fullSens); title("Krzywa ROC dla naiwnego Bayesa - po PCA bez jednego wymiaru")

## PCA bez dwóch kolumn

train[[6]] <- NULL

mBayes<-crossValidate(smp,ind,"Bayes")
mBayes$basicInd
mBayes$auc
drawROC(mBayes$fullSpec, mBayes$fullSens); title("Krzywa ROC dla naiwnego Bayesa - po PCA bez dwóch wymiarów")

# Drzewo decyzyjne

train <- trainWithoutPCA

# Bez PCA

mTree<-crossValidate(smp,ind,"Tree")
mTree$basicInd
mTree$auc
drawROC(mTree$fullSpec, mTree$fullSens); title("Krzywa ROC dla drzewa")

# Redukcja jednego wymiarów

train <- pcaTable
train[[7]] <- NULL

mTree<-crossValidate(smp,ind,"Tree")
mTree$basicInd
mTree$auc
drawROC(mTree$fullSpec, mTree$fullSens); title("Krzywa ROC dla drzewa - redukcja jednego wymiaru")

# Redukcja dwóch wymiarów

train <- pcaTable
train[[6]] <- NULL

mTree<-crossValidate(smp,ind,"Tree")
mTree$basicInd
mTree$auc
drawROC(mTree$fullSpec, mTree$fullSens); title("Krzywa ROC dla drzewa - redukcja dwóch wymiarów")

#Drzewo decyzyjne ze zmienionym parametrem cp

# Bez PCA

train <- trainWithoutPCA

mTree<-crossValidate(smp,ind,"Tree",list(cp=0.015))
mTree$basicInd
mTree$auc
drawROC(mTree$fullSpec, mTree$fullSens); title("Krzywa ROC dla drzewa ze zmienionym cp")

# Redukcja jednego wymiaru

train <- pcaTable
train[[7]] <- NULL

mTree<-crossValidate(smp,ind,"Tree",list(cp=0.015))
mTree$basicInd
mTree$auc
drawROC(mTree$fullSpec, mTree$fullSens); title("Krzywa ROC dla drzewa ze zmienionym cp- redukcja jednego wymiaru")

# Redukcja dwóch wymiarów

train[[6]] <- NULL

mTree<-crossValidate(smp,ind,"Tree",list(cp=0.015))
mTree$basicInd
mTree$auc
drawROC(mTree$fullSpec, mTree$fullSens); title("Krzywa ROC dla drzewa ze zmienionym cp- redukcja dwóch wymiarów")

# Lasy losowe 

# Bez PCA

train <- trainWithoutPCA

mForest<-crossValidate(smp, ind, "Forest")
mForest$basicInd
mForest$auc
drawROC(mForest$fullSpec, mForest$fullSens); title("Krzywa ROC dla lasów losowych")

# Redukcja jednego wymiaru

train <- pcaTable
train[[7]] <- NULL

mForest<-crossValidate(smp, ind, "Forest")
mForest$basicInd
mForest$auc
drawROC(mForest$fullSpec, mForest$fullSens); title("Krzywa ROC dla lasów losowych - redukcja jednego wymiaru")

# Redukcja dwóch wymiarów

train[[6]] <- NULL

mForest<-crossValidate(smp, ind, "Forest")
mForest$basicInd
mForest$auc
drawROC(mForest$fullSpec, mForest$fullSens); title("Krzywa ROC dla lasów losowych - redukcja dwóch wymiarów")

# LDA

# Bez PCA 

train <- trainWithoutPCA

mLDA<-crossValidate(smp,ind,"LDA")
mLDA$basicInd
mLDA$auc
drawROC(mLDA$fullSpec, mLDA$fullSens); title("Krzywa ROC dla LDA")

# Redukcja jednego wymiaru

train <- pcaTable
train[[7]] <- NULL

mLDA<-crossValidate(smp,ind,"LDA")
mLDA$basicInd
mLDA$auc
drawROC(mLDA$fullSpec, mLDA$fullSens); title("Krzywa ROC dla LDA - redukcja jednego wymiaru")

mKNN<-crossValidate(smp,ind,"KNN")
mKNN$basicInd

# Redukcja dwóch wymiarów

train[[6]] <- NULL

mLDA<-crossValidate(smp,ind,"LDA")
mLDA$basicInd
mLDA$auc
drawROC(mLDA$fullSpec, mLDA$fullSens); title("Krzywa ROC dla LDA - redukcja dwóch wymiarów")

#dla KNN nie rysujemy ROC i nie liczymy AUC

train <- trainWithoutPCA

mKNN<-crossValidate(smp,ind,"KNN",list(k=20)) 
mKNN$basicInd

# kalibracja KNN
accuracies <- c()
for(i in 3:50) {
  mKNN<-crossValidate(smp,ind,"KNN",list(k=i))
  print(i)
  print(mKNN$basicInd[1])
  accuracies <- append(accuracies, unlist(mKNN$basicInd[,1]))
}

plot(
  3:50,
  accuracies,
  main = "Accuracy ~ neighbours",
  ylab = "accuracy",
  xlab = "neighbours",
  ylim=c(0.6,0.8)
)

# kalibracja drzewa
myModel<-rpart(as.factor(trainOriginal$Survived)~.,data=train, method = "class", control=rpart.control(cp=0.00001))
plotcp(myModel )
myModel$cptable
