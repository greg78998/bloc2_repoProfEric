don <- readRDS("don.RDS")
###############################################
library(rpart)
library(randomForest)
library(glmnet)
###############################################
XX <- model.matrix(Y~.,data=don)[,-1]#,contrasts.arg=list(famhist = "contr.sum"))[,-1]
YY <- don$Y
############################################
nb <- 4
set.seed(1234)
blocs <- sample(rep(1:nb,length=nrow(don)))
PREV <- data.frame(Y=don$Y)
for(ii in 1:nb){
  print(ii)
  donA <- don[blocs!=ii,]
  donT <- don[blocs==ii,]
  XXA <- XX[blocs!=ii,]
  XXT <- XX[blocs==ii,]
  YYA <- YY[blocs!=ii]
  ###methode1
  tmp <- glm(Y~.,data=donA,family="binomial")
  PREV[blocs==ii,"log"] <- predict(tmp,donT,type="response")
  ###methode2
  #algo2 <- step(tmp,trace=0)
  #PREV[blocs==ii,"aic"] <- predict(algo2,donT,type="response")
  #####methode3
  #algo3 <- step(tmp,k=log(nrow(donA)),trace=0)
  #PREV[blocs==ii,"bic"] <- predict(algo3,donT,type="response")
  #####methode3
  tmp <- rpart(as.factor(Y)~.,data=donA)
  PREV[blocs==ii,"arbre"] <- predict(tmp,donT,type="prob")[,2]
  tmp <- randomForest(as.factor(Y)~.,data=donA)
  PREV[blocs==ii,"foret"] <- predict(tmp,donT,type="prob")[,2]
  tmp <- ranger(as.factor(Y)~.,data=donA, probability = TRUE)
  PREV[blocs==ii,"foretR"] <- predict(tmp,donT)$prediction[,2]
  #####ridge
  ridge <- cv.glmnet(XXA,YYA,alpha=0,family="binomial")
  PREV[blocs==ii,"ridmin"] <- predict(ridge,XXT,s="lambda.min",type="response")
  PREV[blocs==ii,"rid1se"] <- predict(ridge,XXT,s="lambda.1se",type="response")
  #####lasso
  lasso <- cv.glmnet(XXA,YYA,alpha=1,family="binomial")
  etape2 <- glmnet(XXA,YYA,alpha=1,family="binomial",lambda=lasso$lambda.min)
  PREV[blocs==ii,"lasmin"] <- predict(lasso,XXT,s="lambda.min",type="response")
  PREV[blocs==ii,"las1se"] <- predict(lasso,XXT,s="lambda.1se",type="response")
  #####elas
  elas <- cv.glmnet(XXA,YYA,alpha=.5,family="binomial")
  PREV[blocs==ii,"elamin"] <- predict(elas,XXT,s="lambda.min",type="response")
  PREV[blocs==ii,"ela1se"] <- predict(elas,XXT,s="lambda.1se",type="response")
}
PREV[1:4,]

