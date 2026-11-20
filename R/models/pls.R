#####################################################################################
# pls.R - Partial least squares (PLS) forecasts
#####################################################################################
#Try 1–10 components on the validation period (returns ending Jan 1995–Dec 2005).
#For each horizon, use the component count with the lowest validation RMSE throughout the test period.

source("R/models/common.R")

library(pls)

#Inputs: Y = data matrix (column 1 = h-month excess return, then the 122 predictors), indice = column of y (1),
#        lag = forecast horizon h, ncomp = number(s) of PLS components

runpls=function(Y,indice,lag,ncomp){

  Y=Y[1:(nrow(Y)-lag+1),] #drop the last h-1 rows: their returns are not known yet at the forecast origin
  Y2=Y
  aux=embed(Y2,nlags+lag) #create nlags predictor blocks + the forecast horizon shift
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]   #predictors h months earlier

  X.out=tail(aux,1)[1:ncol(X)] #predictors at the forecast origin: used to make the forecast
  drop=seq(indice,ncol(X),by=ncol(Y2)) #the past return is the first column of every lag block
  X=X[,-drop]; X.out=X.out[-drop] #drop the past return in every lag (Mkt_RF and its lags stay in)

  train=data.frame(y=y,X)
  names(train)[-1]=make.names(x_names)
  test=as.data.frame(matrix(X.out,1,dimnames=list(NULL,names(train)[-1])))

  pls.fit=plsr(y~.,data=train,ncomp=max(ncomp),scale=TRUE) #fit PLS; scale=TRUE standardizes the predictors

  pred=sapply(ncomp,function(m) as.numeric(predict(pls.fit,newdata=test,ncomp=m))) #one forecast per number of components

  return(list("model"=pls.fit,"pred"=pred))
}

pls.rolling.window=function(Y,nprev,indice=1,lag=1,ncomp=1){

  save.pred=matrix(NA,nprev,length(ncomp)) #one column per number of components
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    pls=runpls(Y.window,indice,lag,ncomp)
    save.pred[(1+nprev-i),]=pls$pred
    cat("iteration",(1+nprev-i),"\n")
  }

  #RMSE for the period (the validation RMSE chooses the number of components)
  real=Y[,indice]
  rmse=sqrt(colMeans((tail(real,nprev)-save.pred)^2)) #compute RMSE, one per number of components
  errors=cbind("rmse"=rmse)

  return(list("pred"=save.pred,"errors"=errors))
}

############################################################################
#Step 1: choose the number of components on the validation period
############################################################################

M=1:10

plsv1=pls.rolling.window(Yv1,nvald1,1,1,M) #validation forecasts for M = 1,...,10
plsv3=pls.rolling.window(Yv3,nvald3,1,3,M)
plsv6=pls.rolling.window(Yv6,nvald6,1,6,M)
plsv12=pls.rolling.window(Yv12,nvald12,1,12,M)

#chosen number of components = lowest validation RMSE, separately for each horizon
bestM1=which.min(plsv1$errors[,"rmse"])
bestM3=which.min(plsv3$errors[,"rmse"])
bestM6=which.min(plsv6$errors[,"rmse"])
bestM12=which.min(plsv12$errors[,"rmse"])

#Plot validation RMSE against the number of components
png(file.path(results_dir,"figures/pls_validation.png"),width=1400,height=1000,res=150)
par(mfrow=c(2,2))
plot(M,plsv1$errors[,"rmse"],type="b",main="PLS, 1-month horizon: validation error (1995-2005)",xlab="Number of PLS components",ylab="Validation RMSE (lower = better)")
plot(M,plsv3$errors[,"rmse"],type="b",main="PLS, 3-month horizon: validation error (1995-2005)",xlab="Number of PLS components",ylab="Validation RMSE (lower = better)")
plot(M,plsv6$errors[,"rmse"],type="b",main="PLS, 6-month horizon: validation error (1995-2005)",xlab="Number of PLS components",ylab="Validation RMSE (lower = better)")
plot(M,plsv12$errors[,"rmse"],type="b",main="PLS, 12-month horizon: validation error (1995-2005)",xlab="Number of PLS components",ylab="Validation RMSE (lower = better)")
dev.off()

############################################################################
#Step 2: PLS forecasts for the test period, with the chosen number of components
############################################################################

pls1c=pls.rolling.window(Y1,nprev1,1,1,bestM1)
pls3c=pls.rolling.window(Y3,nprev3,1,3,bestM3)
pls6c=pls.rolling.window(Y6,nprev6,1,6,bestM6)
pls12c=pls.rolling.window(Y12,nprev12,1,12,bestM12)

#Validation forecasts with the chosen number of components (used for the GR ensemble weights in evaluate.R)
pls1v=list(pred=plsv1$pred[,bestM1,drop=FALSE])
pls3v=list(pred=plsv3$pred[,bestM3,drop=FALSE])
pls6v=list(pred=plsv6$pred[,bestM6,drop=FALSE])
pls12v=list(pred=plsv12$pred[,bestM12,drop=FALSE])

save(pls1c,pls3c,pls6c,pls12c,pls1v,pls3v,pls6v,pls12v,bestM1,bestM3,bestM6,bestM12,file=file.path(cache_dir,"pls_results.RData"))
