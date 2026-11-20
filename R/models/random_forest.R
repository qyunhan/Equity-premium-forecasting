#####################################################################################
# random_forest.R - Random forest forecasts
#####################################################################################

source("R/models/common.R")
library(randomForest)

#Inputs: Y = data matrix (column 1 = h-month excess return, then the 122 predictors), indice = column of y (1),
#        lag = forecast horizon h

runrf=function(Y,indice,lag){

  Y=Y[1:(nrow(Y)-lag+1),] #drop the last h-1 rows: their returns are not known yet at the forecast origin
  Y2=Y
  aux=embed(Y2,nlags+lag) #create nlags predictor blocks + the forecast horizon shift
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  #predictors h months earlier

  X.out=tail(aux,1)[1:ncol(X)] #predictors at the forecast origin: used to make the forecast
  drop=seq(indice,ncol(X),by=ncol(Y2)) #the past return is the first column of every lag block
  X=X[,-drop]; X.out=X.out[-drop] #drop the past return in every lag (Mkt_RF and its lags stay in)
  colnames(X)=x_names; X.out=matrix(X.out,1,dimnames=list(NULL,colnames(X))) #keep the predictor names, so variable importance is labelled

  model=randomForest(X,y,importance = TRUE) #fit the random forest on default settings (500 trees), saving variable importance
  pred=predict(model,X.out)

  return(list("model"=model,"pred"=pred))
}

rf.rolling.window=function(Y,nprev,indice=1,lag=1){

  save.importance=list()
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    set.seed(4308+as.integer(as.Date(rownames(Y.window)[nrow(Y.window)-lag+1]))+lag) #seed depends on forecast origin and horizon
    lasso=runrf(Y.window,indice,lag)
    save.pred[(1+nprev-i),]=lasso$pred
    save.importance[[i]]=importance(lasso$model)
    cat("iteration",(1+nprev-i),"\n")
  }

  return(list("pred"=save.pred,"save.importance"=save.importance))
}

################################
#Random forest forecasts
################################

#Test period: forecasts made Dec 2005 onwards (one data matrix per horizon: Y1, Y3, Y6, Y12)
rf1c=rf.rolling.window(Y1,nprev1,1,1)
rf3c=rf.rolling.window(Y3,nprev3,1,3)
rf6c=rf.rolling.window(Y6,nprev6,1,6)
rf12c=rf.rolling.window(Y12,nprev12,1,12)

#Validation period (returns ending Jan 1995 - Dec 2005): same settings; used only for the GR ensemble weights in evaluate.R
rf1v=rf.rolling.window(Yv1,nvald1,1,1)
rf3v=rf.rolling.window(Yv3,nvald3,1,3)
rf6v=rf.rolling.window(Yv6,nvald6,1,6)
rf12v=rf.rolling.window(Yv12,nvald12,1,12)

save(rf1c,rf3c,rf6c,rf12c,rf1v,rf3v,rf6v,rf12v,file=file.path(cache_dir,"random_forest_results.RData"))
