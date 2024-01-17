###feature ingeniering
don <- readRDS("don.RDS")
Varexpli <- select(don,-Y)
typevar <- sapply(Varexpli,class)
nbquali <- sum(typevar=="factor")
if(nbquali<1) XX <- Varexpli else {
  nom <- names(typevar)[typevar=="factor"]
  tmp <- NULL
  for(ii in 1:length(nom)){
    tmp <- cbind(tmp,model.matrix(Y~.-1,data=don[,c("Y",nom[ii])]))
  }
  XX <- cbind(Varexpli[,typevar!="factor"],tmp)
}
saveRDS(as.matrix(XX),"XX.RDS")

df=data.frame(x=1:10,y=factor(c(rep("A",3),rep("a",3),rep("B",4))),z=runif(10))
model.matrix(z~.,data=df)
model.matrix(z~.-1,data=df)
model.matrix(z~.,data=df,contrasts.arg = list(y="contr.sum"))
