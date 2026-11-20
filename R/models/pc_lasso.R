#####################################################################################
# pc_lasso.R - LASSO + 4 principal components, lambda chosen by BIC
#####################################################################################

source("R/models/common.R")
library(HDeconometrics)

#Inputs: Y = data matrix (column 1 = h-month excess return, then the 122 predictors), indice = column of y (1),
#        lag = forecast horizon h, alpha = 1 LASSO / 0.5 Elastic Net / 0 Ridge, IC = criterion for lambda ("bic")

runlasso=function(Y,indice,lag,alpha=1,IC="bic"){

  Y=Y[1:(nrow(Y)-lag+1),] #drop the last h-1 rows: their returns are not known yet at the forecast origin
  comp=princomp(scale(Y[,-indice])) #principal components of the standardized predictors (unscaled, PC1 would just copy the series with the largest variance, HWI)
  Y2=cbind(Y,comp$scores[,1:4]) #add the first 4 principal components as extra predictors
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

  save.coef=matrix(NA,nprev,1+(length(x_cols)+4)*nlags) #intercept + (122 predictors + 4 PCs) x nlags
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
#LASSO forecasts (BIC) with 4 principal components
############################################################################

alpha=1

#Test period: forecasts made Dec 2005 onwards (one data matrix per horizon: Y1, Y3, Y6, Y12)
pclasso1c=lasso.rolling.window(Y1,nprev1,1,1,alpha,IC="bic")
pclasso3c=lasso.rolling.window(Y3,nprev3,1,3,alpha,IC="bic")
pclasso6c=lasso.rolling.window(Y6,nprev6,1,6,alpha,IC="bic")
pclasso12c=lasso.rolling.window(Y12,nprev12,1,12,alpha,IC="bic")

#Validation period (returns ending Jan 1995 - Dec 2005): same settings; used only for the GR ensemble weights in evaluate.R
pclasso1v=lasso.rolling.window(Yv1,nvald1,1,1,alpha,IC="bic")
pclasso3v=lasso.rolling.window(Yv3,nvald3,1,3,alpha,IC="bic")
pclasso6v=lasso.rolling.window(Yv6,nvald6,1,6,alpha,IC="bic")
pclasso12v=lasso.rolling.window(Yv12,nvald12,1,12,alpha,IC="bic")

save(pclasso1c,pclasso3c,pclasso6c,pclasso12c,pclasso1v,pclasso3v,pclasso6v,pclasso12v,file=file.path(cache_dir,"pc_lasso_results.RData"))
