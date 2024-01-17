library(pROC)
auctout <- roc(Y~.,data=PREV)
plot(auctout$log)
for(ii in 2:length(auctout)){
  plot(auctout[[ii]],add=T,col=ii)
}
sapply(auctout,coords,x=0.5,ret=c("threshold","accuracy",
                                  "specificity","sensitivity"
                                  ))
