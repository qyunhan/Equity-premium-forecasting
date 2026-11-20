#####################################################################################
# ridge.R - Ridge forecasts, lambda chosen by BIC
#####################################################################################
#Same code as lasso.R with alpha=0. ic.glmnet() then counts Ridge's effective
#degrees of freedom, since Ridge never sets a coefficient exactly to zero.

source("R/models/common.R")
library(HDeconometrics)

#Inputs: Y = data matrix (column 1 = h-month excess return, then the 122 predictors), indice = column of y (1),
#        lag = forecast horizon h, alpha = 1 LASSO / 0.5 Elastic Net / 0 Ridge, IC = criterion for lambda ("bic")

runlasso=function(Y,indice,lag,alpha=1,IC="bic"){

  Y=Y[1:(nrow(Y)-lag+1),] #drop the last h-1 rows: their returns are not known yet at the forecast origin
  #no principal components here (see pc_lasso.R)
  Y2=Y
  aux=embed(Y2,nlags+lag) #create nlags predictor blocks + the forecast horizon shift
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]   #predictors h months earlier

  X.out=tail(aux,1)[1:ncol(X)] #predictors at the forecast origin: used to make the forecast
  drop=seq(indice,ncol(X),by=ncol(Y2)) #the past return is the first column of every lag block
  X=X[,-drop]; X.out=X.out[-drop] #drop the past return in every lag (Mkt_RF and its lags stay in)

  model=ic.glmnet(X,y,crit=IC,alpha = alpha) #fit the model, lambda selected on IC (glmnet standardizes the predictors)
  pred=predict(model,X.out)

  return(list("model"=model,"pred"=pred))
}

lasso.rolling.window=function(Y,nprev,indice=1,lag=1,alpha=1,IC="bic"){

  save.coef=matrix(NA,nprev,1+length(x_names)) #intercept + 122 predictors x nlags
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    lasso=runlasso(Y.window,indice,lag,alpha,IC)
    save.coef[(1+nprev-i),]=lasso$model$coef
    save.pred[(1+nprev-i),]=lasso$pred
    cat("iteration",(1+nprev-i),"\n")
  }

  return(list("pred"=save.pred,"coef"=save.coef))
}

############################################################################
#Ridge forecasts (BIC)
############################################################################

alpha=0

#Test period: forecasts made Dec 2005 onwards (one data matrix per horizon: Y1, Y3, Y6, Y12)
ridge1c=lasso.rolling.window(Y1,nprev1,1,1,alpha,IC="bic")
ridge3c=lasso.rolling.window(Y3,nprev3,1,3,alpha,IC="bic")
ridge6c=lasso.rolling.window(Y6,nprev6,1,6,alpha,IC="bic")
ridge12c=lasso.rolling.window(Y12,nprev12,1,12,alpha,IC="bic")

#Validation period (returns ending Jan 1995 - Dec 2005): same settings; used only for the GR ensemble weights in evaluate.R
ridge1v=lasso.rolling.window(Yv1,nvald1,1,1,alpha,IC="bic")
ridge3v=lasso.rolling.window(Yv3,nvald3,1,3,alpha,IC="bic")
ridge6v=lasso.rolling.window(Yv6,nvald6,1,6,alpha,IC="bic")
ridge12v=lasso.rolling.window(Yv12,nvald12,1,12,alpha,IC="bic")

save(ridge1c,ridge3c,ridge6c,ridge12c,ridge1v,ridge3v,ridge6v,ridge12v,file=file.path(cache_dir,"ridge_results.RData"))
