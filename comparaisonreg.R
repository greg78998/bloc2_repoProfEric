####comparaison de méthodes en regression
####programme de validation croisée par blocs
don=readRDS("don.rds")
###########################################
XX <- model.matrix(Y~.,data=don)[,-1]
YY <- don$Y
############################################
nb <- 10
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
  tmp <- lm(Y~.,data=donA)
  PREV[blocs==ii,"mco"] <- predict(tmp,donT)
  ###methode2
  algo2 <- step(tmp,trace=0)
  PREV[blocs==ii,"aic"] <- predict(algo2,donT)
  #####methode3
  algo3 <- step(tmp,k=log(nrow(donA)),trace=0)
  PREV[blocs==ii,"bic"] <- predict(algo3,donT)
  ########################################
  tmp <- rpart(Y~.,data=donA)
  PREV[blocs==ii,"arbre"] <- predict(tmp,donT)
  tmp <- randomForest(Y~.,data=donA)
  PREV[blocs==ii,"foret"] <- predict(tmp,donT)
  #####ridge
  ridge <- cv.glmnet(XXA,YYA,alpha=0)
  PREV[blocs==ii,"ridmin"] <- predict(ridge,XXT,s="lambda.min")
  PREV[blocs==ii,"rid1se"] <- predict(ridge,XXT,s="lambda.1se")
  #####lasso
  lasso <- cv.glmnet(XXA,YYA,alpha=1)
  PREV[blocs==ii,"lasmin"] <- predict(lasso,XXT,s="lambda.min")
  PREV[blocs==ii,"las1se"] <- predict(lasso,XXT,s="lambda.1se")
  #####elas
  elas <- cv.glmnet(XXA,YYA,alpha=.5)
  PREV[blocs==ii,"elamin"] <- predict(elas,XXT,s="lambda.min")
  PREV[blocs==ii,"ela1se"] <- predict(elas,XXT,s="lambda.1se")
}
erreur <- function(X,Y){mean((X-Y)^2)}
apply(PREV,2,erreur,Y=PREV$Y)

