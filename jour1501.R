don <- read.table("ozone.txt",sep=";",header=T)
summary(don)
###attention Ne18
unique(don$Ne18)
###le point !!!!
don <- read.table("ozone.txt",sep=";",header=T,na.strings = ".")
summary(don)
rownames(don) <- don[,"date"]
don$date <- NULL
summary(don)
don <- na.omit(don)
reg <- lm(maxO3~.,data=don)
summary(reg)
plot(reg)
is.list(reg)
length(reg)
names(reg)
residus <- rstudent(reg)
plot(residus)
abline(h=c(-2,0,2),lty=c(2,1,2),lwd=2,col=2)
leviers <- hatvalues(reg)
sum(abs(residus)>2)/nrow(don)*100
plot(leviers,type="h")
plot(sort(leviers,dec=T),type="h")
library(tidyverse)
don <- rename(don,Y=maxO3)
saveRDS(don,"don.RDS")
don=readRDS("don.RDS")
######################################
XX <- select(don,-Y)
XXcar <- XX^2
XXcub <- XX^3
colnames(XXcar) <- paste(colnames(XX),"car",sep="")
colnames(XXcub) <- paste(colnames(XX),"cub",sep="")
donP <- cbind(don,XXcar,XXcub)
dim(donP)
saveRDS(donP,"donP.RDS")
######################################
don=readRDS("don.RDS")
XI <- model.matrix(Y~.^2,data=don)[,-1]
XI[1:4,]
donI <- data.frame(Y=don$Y,XI)
saveRDS(donI,"donI.RDS")
donPI <- cbind(donI,XXcar,XXcub)
saveRDS(donPI,"donPI.RDS")
######################################
don=readRDS("don.RDS")
XI <- model.matrix(Y~.^3,data=don)[,-1]
XI[1:4,]
donI3 <- data.frame(Y=don$Y,XI)
saveRDS(donI3,"donI3.RDS")




don=read.table("ozonequal.txt",header=T,sep=";",
               stringsAsFactors = T)
summary(don)
don <- select(don,O3,T12,vent,nebulosite)
don
library(ggplot2)
ggplot(don,aes(x=T12,y=O3,col=vent))+geom_point()
model.matrix(O3~.,data=don)
toto=lm(O3~.,data=don)
summary(toto)

don <- select(don,O3,T12,vent,nebulosite,Vx)
tmp <- lm(O3~.,data=don)
summary(tmp)
step(tmp)
library(leaps)
choix <- regsubsets(O3~.,data=don,nbest=1)
plot(choix)
toto <-model.matrix(O3~.,data=don)[,-1]
don2 <- data.frame(O3=don$O3,toto)
don2
tmp2 <- lm(O3~.,data=don2)
summary(tmp)
summary(tmp2)
