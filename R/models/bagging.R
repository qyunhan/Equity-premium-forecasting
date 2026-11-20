#####################################################################################
# bagging.R - Bagging forecasts
#####################################################################################

source("R/models/common.R")
library(randomForest)

#Inputs: Y = data matrix (column 1 = h-month excess return, then the 122 predictors), indice = column of y (1),
#        lag = forecast horizon h

runbag=function(Y,indice,lag){

  Y=Y[1:(nrow(Y)-lag+1),] #drop the last h-1 rows which returns are not known yet at the forecast origin
  Y2=Y
  aux=embed(Y2,nlags+lag) #create nlags predictor blocks + the forecast horizon shift
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  #predictors h months earlier

  X.out=tail(aux,1)[1:ncol(X)] #predictors at the forecast origin: used to make the forecast
  drop=seq(indice,ncol(X),by=ncol(Y2)) #the past return is the first column of every lag block
  X=X[,-drop]; X.out=X.out[-drop] #drop the past return in every lag (Mkt_RF and its lags stay in)
  colnames(X)=x_names; X.out=matrix(X.out,1,dimnames=list(NULL,colnames(X)))

  model=randomForest(X,y,mtry=ncol(X)) #bagging: mtry = P, all predictors tried at every split; 500 trees
  pred=predict(model,X.out)

  return(list("model"=model,"pred"=pred))
}

bag.rolling.window=function(Y,nprev,indice=1,lag=1){

  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    set.seed(4308+as.integer(as.Date(rownames(Y.window)[nrow(Y.window)-lag+1]))+lag) #seed depends on forecast origin and horizon
    lasso=runbag(Y.window,indice,lag)
    save.pred[(1+nprev-i),]=lasso$pred
    cat("iteration",(1+nprev-i),"\n")
  }

  return(list("pred"=save.pred))
}

####################################
#Bagging forecasts
####################################

#Test period: forecasts made Dec 2005 onwards (one data matrix per horizon: Y1, Y3, Y6, Y12)
bag1c=bag.rolling.window(Y1,nprev1,1,1)
bag3c=bag.rolling.window(Y3,nprev3,1,3)
bag6c=bag.rolling.window(Y6,nprev6,1,6)
bag12c=bag.rolling.window(Y12,nprev12,1,12)

#Validation period (returns ending Jan 1995 - Dec 2005): same settings; used only for the GR ensemble weights in evaluate.R
bag1v=bag.rolling.window(Yv1,nvald1,1,1)
bag3v=bag.rolling.window(Yv3,nvald3,1,3)
bag6v=bag.rolling.window(Yv6,nvald6,1,6)
bag12v=bag.rolling.window(Yv12,nvald12,1,12)

save(bag1c,bag3c,bag6c,bag12c,bag1v,bag3v,bag6v,bag12v,file=file.path(cache_dir,"bagging_results.RData"))
