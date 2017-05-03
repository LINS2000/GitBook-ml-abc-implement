# R 實作

---

```
#----------------------------------------------------------------------------------------------
# prepare training data
#----------------------------------------------------------------------------------------------

data=read.csv('T2330-all2.csv', header=T, sep=',')
data=data[1:1742,]

closeprice=data$closeprice
lrow=nrow(data)
row.train=lrow

# compute class
class=rep(0, lrow)

for(i in 1:(lrow-1)){

  data.fun=rep(closeprice[i], lrow-i)
  data.com=closeprice[(i+1):lrow]
  data.ret=(data.com-data.fun)/data.fun

  com10=c()
  com5=c()

  com5=which(data.ret<(-0.05))
  com10=which(data.ret>=0.1)

  if((length(com5)!=0)&(length(com10)!=0)){

    if(com10[1]<com5[1]){

      class[i]=1
    } 

  }

  if((length(com5)==0)&(length(com10)!=0)){

    class[i]=1
  }
}


# delete tail data (-250 rows)
class=class[1:(lrow-250)]
data=data[1:(lrow-250), 3:ncol(data)]
data.total=data.frame(class, data)


get_variables <- function(stepresult.call){

  stepresult.call=attr(stepresult$terms, 'term.labels')
  stepresult.call=gsub('v','',stepresult.call)

  stepresult.variables=as.numeric(stepresult.call)

  return(stepresult.variables)
}

# do stepwise regression
library(MASS)

fit1=lm(class~., data=data.total)
stepresult=stepAIC(fit1, direction='both')

# select predict variables by AIC
# simplify to get stepresult's modal variables
vcol=get_variables(stepresult$call)

needdata.tsmc=data.frame(class, data[,vcol])
needdata=needdata.tsmc
#head(needdata)


#----------------------------------------------------------------------------------------------
# prepare testing data
#----------------------------------------------------------------------------------------------

data1=read.csv('T2330-all2.csv', header=T, sep=',')

closeprice=data1$closeprice

lrow=nrow(data1)
row.all=lrow

# compute class
class=rep(0, lrow)

for(i in 1:(lrow-1)){

  data.fun=rep(closeprice[i], lrow-i)
  data.com=closeprice[(i+1):lrow]
  data.ret=(data.com-data.fun)/data.fun

  com10=c()
  com5=c()

  com5=which(data.ret<(-0.05))
  com10=which(data.ret>=0.1)

  if((length(com5)!=0)&(length(com10)!=0)){

    if(com10[1]<com5[1]){

      class[i]=1
    } 

  }

  if((length(com5)==0)&(length(com10)!=0)){

    class[i]=1
  }
}


# giving real class
realmatrix=c()

for(i in (row.train+1):(row.all-120)){

  realmatrix[i-row.train]=as.matrix(class[i])
}

# compute class
a=row.train+1
b=row.all-120

sum(class[a:b]==1)
sum(class[a:b]==0)

# target(10%,-5%)
data1=data1[,3:ncol(data1)]
testdata.tsmc=data.frame(class, data1[,vcol])
testdata=testdata.tsmc

needdata$class=as.factor(needdata$class)
testdata$class=as.factor(testdata$class)

#write.csv(testdata, file = "rr.csv",row.names=FALSE)
#----------------------------------------------------------------------------------------------
# predict testing data
#----------------------------------------------------------------------------------------------

# SVM(e1071)
library(e1071)

data.resultsvm=c()
prematrixsvm=c()


for(i in (row.train+1):(row.all-120)){

  # training data by svm -- create model
  tsvm=svm(class~., data=needdata[1:(row.train-250),], type='C-classification', cost=2, kernal='radial basis', gamma=0.1, scale=TRUE)

  # predict result by model
  pretsvm=predict(tsvm, testdata[i, -1])


  data.resultsvm[i-row.train]=sum(pretsvm==testdata$class[i])/length(pretsvm)
  #prematrixsvm[i-row.train]=as.character.factor(pretsvm)
  prematrixsvm[i-row.train]=as.character.factor(pretsvm)
}

#
summary(data.resultsvm)
t=table(prematrixsvm, realmatrix)
t
sum(diag(t)/sum(t))
#data.frame(prematrixsvm,realmatrix)

# Decision Tree(C50)
library(C50)

data.resultc50=c()
prematrixc50=c()

needdata$class=as.factor(needdata$class)
testdata$class=as.factor(testdata$class)

for(i in (row.train+1):(row.all-120)){

  # training data by c50 -- create model
  trainc50=C5.0(class~., needdata[1:(row.train-250),], trial=5, control=C5.0Control(subset=FALSE, noGlobalPruning=TRUE, CF=0.25))

  # predict result by model
  predc50=predict(trainc50, testdata[i, -1], trials=5, type='class')


  data.resultc50[i-row.train]=sum(predc50==testdata$class[i])/length(predc50)
  prematrixc50[i-row.train]=as.character.factor(predc50)

}

#
summary(data.resultc50)
t=table(prematrixc50, realmatrix)
t
sum(diag(t)/sum(t))
```

> 源碼：[https://goo.gl/0sOR5k](https://goo.gl/0sOR5k)



