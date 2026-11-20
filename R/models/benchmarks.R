#####################################################################################
# benchmarks.R - Benchmark forecasts: Historical Mean and Full OLS
#####################################################################################
#For returns, the standard benchmark is the Historical Mean
#(average past return, Welch & Goyal 2008). Full OLS is also run on all 122 predictors (no penalty).

source("R/models/common.R")

if(nlags!=1) stop("Benchmarks use one lag. Set ERP_NLAGS=1 to rebuild them.")

#Inputs: Y = data matrix (column 1 = h-month excess return, then the 122 predictors), indice = column of y (1),
#        lag = forecast horizon h

runbench=function(Y,indice,lag){

  Y=Y[1:(nrow(Y)-lag+1),] #drop the last h-1 rows: their returns are not known yet at the forecast origin

  Y2=Y
  aux=embed(Y2,1+lag) #line up each return with the predictors h months earlier (1 lag)
  y=aux[,1]
  X=aux[,-c(1:(ncol(Y2)*lag))] #predictors h months earlier

  X.out=tail(aux,1)[1:ncol(X)] #predictors at the forecast origin: used to make the forecast
  X=X[,-indice]; X.out=X.out[-indice] #drop the past return itself (last month's market return, Mkt_RF, stays in)

  hm=mean(y) #Historical Mean forecast: average h-month excess return in the window

  model=lm(y~X) #Full OLS on all predictors
  pred=c(1,X.out)%*%coef(model)

  return(list("pred"=pred,"hm"=hm))
}

bench.rolling.window=function(Y,nprev,indice=1,lag=1){

  save.pred=matrix(NA,nprev,1)
  save.hm=matrix(NA,nprev,1)
  for(i in nprev:1){

    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    fact=runbench(Y.window,indice,lag)
    save.pred[(1+nprev-i),]=fact$pred
    save.hm[(1+nprev-i),]=fact$hm
    cat("iteration",(1+nprev-i),"\n")
  }

  return(list("pred"=save.pred,"hm"=save.hm))
}

######################################
#Benchmark forecasts: Historical Mean and Full OLS
######################################

#Test period: forecasts made Dec 2005 onwards (one data matrix per horizon: Y1, Y3, Y6, Y12)
bench1c=bench.rolling.window(Y1,nprev1,1,1)
bench3c=bench.rolling.window(Y3,nprev3,1,3)
bench6c=bench.rolling.window(Y6,nprev6,1,6)
bench12c=bench.rolling.window(Y12,nprev12,1,12)

dir.create(shared_results_dir,recursive=TRUE,showWarnings=FALSE)
save(bench1c,bench3c,bench6c,bench12c,file=benchmark_file)
