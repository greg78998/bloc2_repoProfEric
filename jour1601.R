boston <- read.table("BostonHousing.csv",header=T,sep=",",
                     stringsAsFactors = T)
summary(boston)
library(tidyverse)
don <- rename(boston,Y=medv)
don$chas <- as.factor(don$chas)
saveRDS(don,"don.RDS")
#####################################
XX <- select(don,-Y)
type=sapply(XX,class)
XX <- XX[,type!="factor"]
XXcar <- XX^2
XXcub <- XX^3
colnames(XXcar) <- paste(colnames(XX),"car",sep="")
colnames(XXcub) <- paste(colnames(XX),"cub",sep="")
donP <- cbind(don,XXcar,XXcub)
saveRDS(donP,"donP.RDS")

library(glmnet)
don <- read.table("ozone.txt",sep=";",header=T,
                  na.strings = ".")
rownames(don) <- don[,"date"]
don$date <- NULL
don <- na.omit(don)
don <- rename(don,Y=maxO3)
don <- don[1:50,]
XX <- as.matrix(select(don,-Y))
YY <- don$Y
tmp <- glmnet(XX,YY)
names(tmp)
tmp$lambda
tmp$a0
dim(tmp$beta)
plot(tmp)
plot(tmp,xvar="lambda")
tmp <- glmnet(XX,YY,alpha=0)
plot(tmp,xvar="lambda")
######################################
XX <- as.matrix(select(don,-Y))
YY <- don$Y
tmp <- glmnet(XX,YY,alpha=1,relax=T)
tmp2 <- cv.glmnet(XX,YY,alpha=1)
plot(tmp2)

nb <- 10
set.seed(1234)
blocs <- sample(rep(1:nb,length=nrow(don)))
PREV <- data.frame(Y=don$Y)
XX <- model.matrix(Y~.,data=don)[,-1]
YY <- don$Y
Lambda <- c(0,sort(tmp2$lambda))
for(ii in 1:nb){
  print(ii)
  XXA <- XX[blocs!=ii,]
  XXT <- XX[blocs==ii,]
  YYA <- YY[blocs!=ii]
  for(jj in 1:length(Lambda)){
  tmp <- glmnet(XXA,YYA,lambda=Lambda[jj],alpha=1)
  PREV[blocs==ii,jj+1] <- predict(tmp,XXT)}
}
erreur <- function(X,Y){mean((X-Y)^2)}
choix <- apply(PREV,2,erreur,Y=PREV$Y)[-1]
plot(log(Lambda),choix)

tmp2 <- cv.glmnet(XX,YY,alpha=1)
plot(tmp2)
points(log(Lambda),choix,pch=16,col="green")

#####################################################
#####
#####################################################
artere <- read.table("artere.txt",header=T,sep=" ")
summary(artere)
plot(chd~age,data=artere,
     col=artere$chd+1,pch=16)
aggregate(artere$chd,by=list(as.factor(artere$agrp)),mean)
artere %>% group_by(as.factor(agrp)) %>% summarise(mean(chd))

reglog <- glm(chd~age,data=artere,family=binomial)
summary(reglog)
regpro <- glm(chd~age,data=artere,family=binomial(link=probit))
summary(regpro)
df <- data.frame(age=1:100)
tmp <- predict(reglog,df,type="response")
tmp
lines(df$age,tmp)




don <- read.table("SAheart.data",header=T,sep=",",
                  stringsAsFactors = T,row.names = 1)
summary(don)
don=rename(don,Y=chd)
step(glm(Y~.,data=don,family="binomial"))
