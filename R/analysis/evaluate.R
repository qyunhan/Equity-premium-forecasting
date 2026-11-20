#####################################################################################
# evaluate.R - All results, computed from the saved forecasts 
#####################################################################################

rm(list=ls())

#Root mean squared error
RMSE <- function(pred, truth){
  return(sqrt(mean((truth - pred)^2)))
}

library(sandwich) 
library(lsei)

source("R/models/common.R") #data: true values oosy1,...,oosy12 and the panel
vars=read.csv("data/variable_descriptions.csv",check.names=FALSE) #plain-English names of the 122 predictors
describe=function(code) vars$description[match(code,vars$code)]
hlab=c("1"="1-month","3"="3-month","6"="6-month","12"="12-month") #horizon labels for tables and plots

#Load results (one file per model script):
load(benchmark_file) #bench1c,...: Historical Mean ($hm) and Full OLS ($pred)
load(file.path(cache_dir,"ridge_results.RData"))
load(file.path(cache_dir,"lasso_results.RData"))
load(file.path(cache_dir,"elastic_net_results.RData"))
load(file.path(cache_dir,"pc_lasso_results.RData"))
load(file.path(cache_dir,"pls_results.RData"))
load(file.path(cache_dir,"bagging_results.RData"))
load(file.path(cache_dir,"random_forest_results.RData"))


######################################################################
#Simple average ensemble of time series forecasts
######################################################################

#Simple average of the 7 machine-learning models' forecasts:
avgpred1=(ridge1c$pred+lasso1c$pred+elasticnet1c$pred+pclasso1c$pred+pls1c$pred+bag1c$pred+rf1c$pred)/7
avgpred3=(ridge3c$pred+lasso3c$pred+elasticnet3c$pred+pclasso3c$pred+pls3c$pred+bag3c$pred+rf3c$pred)/7
avgpred6=(ridge6c$pred+lasso6c$pred+elasticnet6c$pred+pclasso6c$pred+pls6c$pred+bag6c$pred+rf6c$pred)/7
avgpred12=(ridge12c$pred+lasso12c$pred+elasticnet12c$pred+pclasso12c$pred+pls12c$pred+bag12c$pred+rf12c$pred)/7


#######################################
###Forecast combinations: Granger-Ramanathan weights
#######################################

#Using validation set (Jan 1995 - Dec 2005) to compute GR weights:

#Design matrix: the X variables are the models' forecasts on the validation set
fmatu1=cbind(ridge1v$pred,lasso1v$pred,elasticnet1v$pred,pclasso1v$pred,pls1v$pred,bag1v$pred,rf1v$pred) #one per horizon
fmatu3=cbind(ridge3v$pred,lasso3v$pred,elasticnet3v$pred,pclasso3v$pred,pls3v$pred,bag3v$pred,rf3v$pred)
fmatu6=cbind(ridge6v$pred,lasso6v$pred,elasticnet6v$pred,pclasso6v$pred,pls6v$pred,bag6v$pred,rf6v$pred)
fmatu12=cbind(ridge12v$pred,lasso12v$pred,elasticnet12v$pred,pclasso12v$pred,pls12v$pred,bag12v$pred,rf12v$pred)

#Granger-Ramanathan weights by quadratic programming: lsei(X,y,c,d,e,f) with
#X = forecasts, y = true values, c*beta=d (weights sum to one: c = vector of ones, d=1),
#e*beta>=f (weights >= 0: e = identity matrix, f = zeros). No constant.
gru1=lsei(fmatu1, oosyv1, c=rep(1,7), d=1, e=diag(7), f=rep(0,7)) 
gru3=lsei(fmatu3, oosyv3, c=rep(1,7), d=1, e=diag(7), f=rep(0,7))
gru6=lsei(fmatu6, oosyv6, c=rep(1,7), d=1, e=diag(7), f=rep(0,7))
gru12=lsei(fmatu12, oosyv12, c=rep(1,7), d=1, e=diag(7), f=rep(0,7))

#Combine the test-period forecasts with the weights:
combpredu1=cbind(ridge1c$pred,lasso1c$pred,elasticnet1c$pred,pclasso1c$pred,pls1c$pred,bag1c$pred,rf1c$pred)%*%gru1
combpredu3=cbind(ridge3c$pred,lasso3c$pred,elasticnet3c$pred,pclasso3c$pred,pls3c$pred,bag3c$pred,rf3c$pred)%*%gru3
combpredu6=cbind(ridge6c$pred,lasso6c$pred,elasticnet6c$pred,pclasso6c$pred,pls6c$pred,bag6c$pred,rf6c$pred)%*%gru6
combpredu12=cbind(ridge12c$pred,lasso12c$pred,elasticnet12c$pred,pclasso12c$pred,pls12c$pred,bag12c$pred,rf12c$pred)%*%gru12

#Save the weights: one row per horizon, one column per model
grw=rbind(gru1,gru3,gru6,gru12)
colnames(grw)=c("Ridge","LASSO","Elastic Net","PC + LASSO","PLS","Bagging","Random Forest")
write.csv(data.frame(horizon=c("1-month","3-month","6-month","12-month"),round(grw,4),check.names=FALSE),file.path(results_dir,"tables/gr_weights.csv"),row.names=FALSE)


######################################################################
#Collect all forecasts in one matrix per horizon
######################################################################
models=c("Historical Mean","Full OLS","Ridge","LASSO","Elastic Net","PC + LASSO","PLS","Bagging","Random Forest","Ensemble (average)","Ensemble (GR)")
fc1=cbind(bench1c$hm,bench1c$pred,ridge1c$pred,lasso1c$pred,elasticnet1c$pred,pclasso1c$pred,pls1c$pred,bag1c$pred,rf1c$pred,avgpred1,combpredu1)
fc3=cbind(bench3c$hm,bench3c$pred,ridge3c$pred,lasso3c$pred,elasticnet3c$pred,pclasso3c$pred,pls3c$pred,bag3c$pred,rf3c$pred,avgpred3,combpredu3)
fc6=cbind(bench6c$hm,bench6c$pred,ridge6c$pred,lasso6c$pred,elasticnet6c$pred,pclasso6c$pred,pls6c$pred,bag6c$pred,rf6c$pred,avgpred6,combpredu6)
fc12=cbind(bench12c$hm,bench12c$pred,ridge12c$pred,lasso12c$pred,elasticnet12c$pred,pclasso12c$pred,pls12c$pred,bag12c$pred,rf12c$pred,avgpred12,combpredu12)
colnames(fc1)=colnames(fc3)=colnames(fc6)=colnames(fc12)=models


######################################################################
#Accuracy: RMSE, MAE, out-of-sample R2
######################################################################
#OOS R2 = 1 - SSE(model)/SSE(Historical Mean): % reduction in squared error vs the Historical Mean (Campbell & Thompson 2008)
accuracy=function(fc,oosy,h){
  data.frame(h=h,model=colnames(fc),
             RMSE=apply(fc,2,RMSE,truth=oosy),
             MAE=apply(fc,2,function(p) mean(abs(oosy-p))),
             OOS_R2=1-apply(fc,2,function(p) sum((oosy-p)^2))/sum((oosy-fc[,"Historical Mean"])^2))
}
acc=rbind(accuracy(fc1,oosy1,1),accuracy(fc3,oosy3,3),accuracy(fc6,oosy6,6),accuracy(fc12,oosy12,12))
write.csv(data.frame(horizon=hlab[as.character(acc$h)],model=acc$model,RMSE=acc$RMSE,MAE=acc$MAE,OOS_R2_pct=100*acc$OOS_R2),
          file.path(results_dir,"tables/model_metrics.csv"),row.names=FALSE)

#Summary tables (models x horizons): OOS R2 in % (main metric) and RMSE
oosr2.table=cbind("1-month"=100*acc$OOS_R2[acc$h==1],"3-month"=100*acc$OOS_R2[acc$h==3],"6-month"=100*acc$OOS_R2[acc$h==6],"12-month"=100*acc$OOS_R2[acc$h==12])
rownames(oosr2.table)=models
rmse.table=cbind("1-month"=apply(fc1,2,RMSE,truth=oosy1),"3-month"=apply(fc3,2,RMSE,truth=oosy3),"6-month"=apply(fc6,2,RMSE,truth=oosy6),"12-month"=apply(fc12,2,RMSE,truth=oosy12))
#OOS R2 (%) is relative to the Historical Mean (benchmark)
write.csv(data.frame("Model"=sub("^Historical Mean$","Historical Mean (benchmark)",models),"1-month OOS R2 (%)"=round(oosr2.table[,1],2),
                     "3-month OOS R2 (%)"=round(oosr2.table[,2],2),"6-month OOS R2 (%)"=round(oosr2.table[,3],2),
                     "12-month OOS R2 (%)"=round(oosr2.table[,4],2),check.names=FALSE),file.path(results_dir,"tables/oos_r2_table.csv"),row.names=FALSE)
write.csv(round(rmse.table,5),file.path(results_dir,"tables/rmse_table.csv"))


################################################
##Plot ML forecasts for each horizon (.png)
##############################################

#Realised returns vs Historical Mean, LASSO and Random Forest forecasts, dated by the month the return ends
for(h in horizons){
  ml.ts=ts(cbind(get(paste0("oosy",h)),get(paste0("bench",h,"c"))$hm,get(paste0("lasso",h,"c"))$pred,get(paste0("rf",h,"c"))$pred),
           start=c(2006,h), freq=12) #the first h-month return ends in month h of 2006
  png(file.path(results_dir,"figures",paste0("forecasts_h",h,".png")),width=1400,height=700,res=150)
  plot.ts(ml.ts[,1], main=paste0(h,"-month horizon: forecasts vs realised excess return (test period)"), cex.axis=1.5, lwd=2,
          ylim=range(ml.ts)+c(0,0.3*diff(range(ml.ts))), #room for the legend
          xlab=paste0("Month in which the ",h,"-month return ends"), ylab="Excess return (0.05 = 5%)")
  points(ml.ts[,2], type="l", col="black",lty=2,lwd=2)
  points(ml.ts[,3], type="l", col="blue",lwd=2.8)
  points(ml.ts[,4], type="l", col="red",lwd=2.8)
  legend("topleft", c("Realised excess return","Historical Mean","LASSO","Random forest"), lty=c(1,2,1,1) ,col=c("black","black","blue","red"),ncol=4,bty="n",cex=0.85)
  dev.off()
}


#######################################################
##Sparsity analysis over time: LASSO, Elastic Net and PC + LASSO
#######################################################

#Horizons with fewer forecasts are padded with NA at the end to fit in one ts object
pad=function(x) c(x,rep(NA,nprev1-length(x)))

#Number of nonzero coefficients in each window, for each horizon.
#The count includes the intercept, so 1 = no predictor selected (= Historical Mean forecast)
sparsity=list("LASSO"=c("lasso","sparsity_lasso"),"Elastic Net"=c("elasticnet","sparsity_elastic_net"),"PC + LASSO"=c("pclasso","sparsity_pc_lasso"))
for(m in names(sparsity)){
  p=sparsity[[m]][1]
  coef.ts=ts(sapply(horizons,function(h) pad(rowSums(get(paste0(p,h,"c"))$coef != 0))), start=c(2005,12), freq=12) #dated by forecast origin
  colnames(coef.ts)=c("1-month","3-month","6-month","12-month")
  png(file.path(results_dir,"figures",paste0(sparsity[[m]][2],".png")),width=1400,height=1100,res=150)
  plot.ts(coef.ts, main=paste0(m,": non-zero coefficients per window (incl. intercept; 1 = historical mean)"),cex.main=1.1,
          xlab="Forecast origin",cex.axis=1.5)
  dev.off()
}

#Which predictors are selected: % of test windows in which each predictor has a nonzero coefficient
selfreq=function(coef,h,model) data.frame(horizon=hlab[as.character(h)],h=h,model=model,variable=x_names,description=describe(sub("_lag[0-9]+$","",x_names)),
                                          pct_of_windows_selected=100*colMeans(coef[,-1]!=0)) #column 1 = intercept
sel=rbind(selfreq(lasso1c$coef,1,"LASSO"),selfreq(lasso3c$coef,3,"LASSO"),selfreq(lasso6c$coef,6,"LASSO"),selfreq(lasso12c$coef,12,"LASSO"),
          selfreq(elasticnet1c$coef,1,"Elastic Net"),selfreq(elasticnet3c$coef,3,"Elastic Net"),selfreq(elasticnet6c$coef,6,"Elastic Net"),selfreq(elasticnet12c$coef,12,"Elastic Net"))
sel=sel[sel$pct_of_windows_selected>0,]
sel=sel[order(sel$model,sel$h,-sel$pct_of_windows_selected),]
sel$h=NULL
write.csv(sel,file.path(results_dir,"tables/variable_selection.csv"),row.names=FALSE)

#% of test windows with at least one predictor selected (otherwise forecast = historical mean)
anysel=function(coef) 100*mean(rowSums(coef[,-1]!=0)>0)
selshare=data.frame(model=c("LASSO","PC + LASSO","Elastic Net"),
                    h1=c(anysel(lasso1c$coef),anysel(pclasso1c$coef),anysel(elasticnet1c$coef)),
                    h3=c(anysel(lasso3c$coef),anysel(pclasso3c$coef),anysel(elasticnet3c$coef)),
                    h6=c(anysel(lasso6c$coef),anysel(pclasso6c$coef),anysel(elasticnet6c$coef)),
                    h12=c(anysel(lasso12c$coef),anysel(pclasso12c$coef),anysel(elasticnet12c$coef)))
colnames(selshare)=c("model",hlab)
write.csv(selshare,file.path(results_dir,"tables/selection_share.csv"),row.names=FALSE)


########################################################
###Random forest variable importance
########################################################
#average importance per horizon.
avgimp=function(rf) rowMeans(sapply(rf$save.importance,function(m) m[,"%IncMSE"]))
imp=rbind(data.frame(h=1,variable=x_names,mean_IncMSE=avgimp(rf1c)),data.frame(h=3,variable=x_names,mean_IncMSE=avgimp(rf3c)),
          data.frame(h=6,variable=x_names,mean_IncMSE=avgimp(rf6c)),data.frame(h=12,variable=x_names,mean_IncMSE=avgimp(rf12c)))
imp=imp[order(imp$h,-imp$mean_IncMSE),]
imp$description=describe(sub("_lag[0-9]+$","",imp$variable))
write.csv(data.frame(horizon=hlab[as.character(imp$h)],variable=imp$variable,description=imp$description,mean_IncMSE=imp$mean_IncMSE),
          file.path(results_dir,"tables/rf_variable_importance.csv"),row.names=FALSE)

png(file.path(results_dir,"figures/rf_variable_importance.png"),width=3000,height=2000,res=150) #15 most important predictors per horizon
par(mfrow=c(2,2),mar=c(4.5,26,3,1),oma=c(0,0,3,0))
for(h in horizons){
  d=head(imp[imp$h==h,],15)
  barplot(rev(d$mean_IncMSE),names.arg=rev(paste(d$variable,d$description,sep=": ")),horiz=TRUE,las=1,cex.names=0.8,
          xlab="% increase in prediction error when the predictor is shuffled",main=paste(hlab[as.character(h)],"horizon"))
}
mtext("Random forest: 15 most important predictors (%IncMSE, averaged over all test windows)",outer=TRUE,cex=1.3,font=2)
dev.off()


#Scree plot: variance explained by the principal components used in PC + LASSO (first 1-step test window)
#Preserved L1 outputs use 360 raw months. Regeneration uses 361 for L1 or 364 for L4,
#giving 360 regression rows after embedding.
comp=princomp(scale(Y1[1:(nrow(Y1)-nprev1),-1])) #predictors of the first window, standardized as in pc_lasso.R
share=comp$sdev^2/sum(comp$sdev^2)
write.csv(data.frame(PC=1:length(share),variance_explained_pct=100*share,cumulative_pct=100*cumsum(share)),file.path(results_dir,"tables/pc_explained_variance.csv"),row.names=FALSE)
png(file.path(results_dir,"figures/pc_scree.png"),width=1100,height=700,res=130)
plot(1:25,100*share[1:25],type="b",pch=19,xlab="Principal component (1 = explains the most)",ylab="% of the predictors' total variance",
     main=paste0("Scree plot: variance of the 122 standardized predictors explained by each PC\n(first window: ",nrow(Y1)-nprev1," raw months to Dec 2005; ",window_size," regression rows)"),cex.main=0.9)
abline(v=4,lty=2) #the 4 components used in PC + LASSO
legend("topright","4 PCs used in PC + LASSO",lty=2,bty="n")
dev.off()


######################################
###Diebold-Mariano (DM) tests
######################################

#DM test: regress the loss differential d_t on a constant, with Newey-West variance.
#Every model is tested against the historical mean (10 models x 4 horizons).
#Lag: 6 by the nprev^(1/3) rule (240^(1/3)=6.2). h-month forecast errors overlap for
#h-1 months, so use at least h-1 lags (11 for h=12).
dm.test=function(fc,oosy,h){
  lhm=(oosy-fc[,"Historical Mean"])^2 
  out=NULL
  for(m in colnames(fc)[-1]){
    lm1=(oosy-fc[,m])^2 
    d=lhm-lm1 #Loss differential: positive = model better than historical mean
    lag=max(6,h-1)
    if(max(abs(d))<1e-12){ out=rbind(out,data.frame(h=h,model=m,DM_stat=NA,p_value=NA,lag=lag)); next } #forecasts identical to the Historical Mean: nothing to test
    dm=lm(d~1) #regression - get estimate of mean(d_t)
    stat=dm$coefficients/sqrt(NeweyWest(dm,lag=lag)) #form the DM t-statistic
    out=rbind(out,data.frame(h=h,model=m,DM_stat=as.numeric(stat),p_value=2*pnorm(-abs(as.numeric(stat))),lag=lag)) #two-sided p-value
  }
  return(out)
}
dmres=rbind(dm.test(fc1,oosy1,1),dm.test(fc3,oosy3,3),dm.test(fc6,oosy6,6),dm.test(fc12,oosy12,12))
dmres$verdict_10pct=ifelse(is.na(dmres$p_value),"identical to Historical Mean",ifelse(dmres$p_value>=0.10,"no significant difference",
                    ifelse(dmres$DM_stat>0,"significantly MORE accurate than Historical Mean","significantly LESS accurate than Historical Mean")))
write.csv(data.frame(horizon=hlab[as.character(dmres$h)],dmres[,-1]),file.path(results_dir,"tables/dm_tests.csv"),row.names=FALSE)


########################################################
###Trading strategy: overlapping h-month positions
########################################################

#Strategy: every month a new position is opened and held for
#h months - 100% in stocks (the market) if the forecast is positive, otherwise 100% in bonds (T-bills). With h positions
#open at once, the stock weight is their average, rebalanced every month so the positions stay equally weighted.
#A 0.1% transaction cost is charged on the value traded. Trading starts once h signals exist.
strategy=function(signals,r_stock,r_bond,h,tc=0.001){

  V <- 100
  nmonths=length(signals)-h+1 #number of months traded
  account <- vector("list", nmonths)

  for (t in 1:nmonths) {

    #h signals active during month t
    active <- signals[t:(t + h - 1)]
    w <- mean(active)

    #Initial investments
    stock_start <- w * V
    bond_start <- (1 - w) * V

    #Value at end of month, before rebalancing
    stock_end <- stock_start * (1 + r_stock[t])
    bond_end <- bond_start * (1 + r_bond[t])

    V_end <- stock_end + bond_end

    #Month-end rebalancing for next month
    if (t < nmonths) {
      next_active <- signals[(t + 1):(t + h)]
      w_next <- mean(next_active)

      #Positive = buy stocks, negative = sell stocks
      stock_trade <- w_next * V_end - stock_end
    } else {
      stock_trade <- 0 #no trade after the last month
    }

    cost <- tc * abs(stock_trade) #transaction cost on the value traded

    account[[t]] <- data.frame(
      r_port = (V_end - cost) / V - 1, #return after cost
      r_bond = r_bond[t], #kept for the Sharpe ratio
      wealth_end = V_end - cost
    )

    #Carry full wealth forward
    V <- V_end - cost
  }

  account <- do.call(rbind, account)

  #Terminal wealth, Sharpe ratio (annualised) and maximum drawdown
  c(terminal_wealth = V,
    sharpe = mean(account$r_port - account$r_bond) / sd(account$r_port - account$r_bond) * sqrt(12),
    max_drawdown = max(1 - account$wealth_end / cummax(account$wealth_end)))
}

#Run the strategy for every model and horizon. Signal = 1 if the forecast is positive.
#r_stock / r_bond = market / T-bill return in the month after each forecast origin.
trade=function(fc,oosy,h){
  origin=match(as.Date(names(oosy)),panel$date)-h #rows of the forecast origins
  r_stock=panel$R_mkt[origin+1][h:length(origin)] #month t is the month after the origin of the latest active signal
  r_bond=panel$RF[origin+1][h:length(origin)]
  out=data.frame(h=h,model="Buy & Hold",t(strategy(rep(1,length(oosy)),r_stock,r_bond,h,tc=0))) #always in the market
  for(m in colnames(fc)) out=rbind(out,data.frame(h=h,model=m,t(strategy(as.numeric(fc[,m]>0),r_stock,r_bond,h))))
  return(out)
}
trading=rbind(trade(fc1,oosy1,1),trade(fc3,oosy3,3),trade(fc6,oosy6,6),trade(fc12,oosy12,12))
write.csv(data.frame(horizon=hlab[as.character(trading$h)],strategy=trading$model,final_value_of_100=trading$terminal_wealth,
                     sharpe_ratio=trading$sharpe,max_drawdown_pct=100*trading$max_drawdown),file.path(results_dir,"tables/trading_performance.csv"),row.names=FALSE)


#save all forecasts in one file
allfc=do.call(rbind,lapply(list(list(fc1,oosy1,1),list(fc3,oosy3,3),list(fc6,oosy6,6),list(fc12,oosy12,12)),function(a)
  data.frame(return_ends=rep(names(a[[2]]),ncol(a[[1]])),horizon_months=a[[3]],model=rep(colnames(a[[1]]),each=nrow(a[[1]])),forecast=as.vector(a[[1]]),actual=rep(as.numeric(a[[2]]),ncol(a[[1]])))))
write.csv(allfc,file.path(results_dir,"tables/all_forecasts.csv"),row.names=FALSE)


########################################################
###1-month switching: timing accuracy table and figure
########################################################

#A switch = a month in which the 1-month forecast is non-positive, so the strategy holds T-bills.
#A switch is right if the market excess return in that month is negative.
f1=allfc[allfc$horizon_months==1,] #1-month forecasts and realised excess returns
f1$return_ends=as.Date(f1$return_ends)
f1=f1[order(f1$model,f1$return_ends),] #sort by date within each model

#Switch precision and average size of the months caught / missed
switch.stats=function(m){
  d=f1[f1$model==m,]
  y=d$actual #realised excess return
  s=d$forecast<=0 #TRUE = switched to T-bills
  big=y<=-0.05 #excess return of -5% or worse
  data.frame(model=m,switches=sum(s),right=sum(s&y<0),precision_pct=100*mean(y[s]<0),
             avg_right_switch_pct=100*mean(y[s&y<0]),avg_wrong_switch_pct=100*mean(y[s&y>=0]),
             big_falls=sum(big),big_falls_caught=sum(big&s),
             avg_fall_caught_pct=100*mean(y[big&s]),avg_fall_missed_pct=100*mean(y[big&!s]),
             net_pp=-100*sum(y[s])) #losses avoided minus gains forgone, summed (no compounding or costs)
}
switches=do.call(rbind,lapply(c("Ensemble (average)","LASSO","PC + LASSO","PLS","Bagging","Random Forest"),switch.stats))
write.csv(switches,file.path(results_dir,"tables/switch_accuracy_1m.csv"),row.names=FALSE) #NaN = no months in that group

#Monthly value of $100 for the h = 1 strategy (same rule and cost as strategy())
dates=sort(unique(f1$return_ends)) #Jan 2006 ... Dec 2025
rs=panel$R_mkt[match(dates,panel$date)] #market return
rb=panel$RF[match(dates,panel$date)] #T-bill return
wealth.path=function(signals,tc=0.001){
  V=100
  path=numeric(length(signals))
  for(t in 1:length(signals)){
    V_end=signals[t]*V*(1+rs[t])+(1-signals[t])*V*(1+rb[t]) #stocks if signal = 1, T-bills if 0
    stock_trade=if(t<length(signals)) signals[t+1]*V_end-signals[t]*V*(1+rs[t]) else 0 #value moved into/out of stocks for next month
    V=V_end-tc*abs(stock_trade) #pay the transaction cost
    path[t]=V
  }
  return(c(100,path)) #start at $100 at the end of Dec 2005
}
signal=function(m) as.numeric(f1$forecast[f1$model==m]>0) #1 = positive forecast
wealth=cbind("Ensemble (average)"=wealth.path(signal("Ensemble (average)")),
             "LASSO / PC + LASSO"=wealth.path(signal("LASSO")),
             "Buy & Hold"=wealth.path(rep(1,length(dates)),tc=0),
             "Random Forest"=wealth.path(signal("Random Forest")),
             "T-bills only"=wealth.path(rep(0,length(dates)),tc=0))
x=c(as.Date("2005-12-01"),dates)
cols=c("blue","darkgreen","black","red","grey50")
crises=data.frame(name=c("GFC","COVID","2022"),start=as.Date(c("2007-12-01","2020-02-01","2022-01-01")),
                  end=as.Date(c("2009-06-30","2020-04-30","2022-10-31"))) #shaded periods
shade=function(lo,hi) rect(crises$start,lo,crises$end,hi,col="grey90",border=NA)

png(file.path(results_dir,"figures/switching_1m.png"),width=1400,height=1100,res=150)
par(mfrow=c(2,1),mar=c(4,5,3,1))
#Top: value of $100
plot(x,wealth[,1],type="l",col=cols[1],lwd=2,ylim=c(0,1.05*max(wealth)),
     main="1-month strategy: value of $100 (test period)",xlab="",ylab="Value of $100")
shade(-100,2*max(wealth)); text(crises$start+(crises$end-crises$start)/2,max(wealth),crises$name,col="grey40",cex=0.8)
lines(x,wealth[,1],col=cols[1],lwd=2) #redraw on top of the shading
for(j in 2:5) lines(x,wealth[,j],col=cols[j],lwd=2,lty=ifelse(j==5,2,1))
legend("left",legend=sprintf("%s ($%.0f)",colnames(wealth),wealth[nrow(wealth),]),col=cols,lty=c(1,1,1,1,2),lwd=2,bty="n",cex=0.8)
#Bottom: months with a -5% excess return, and months each model held T-bills
rows=c("LASSO","Ensemble (average)","Random Forest")
plot(range(x),c(0.5,4.5),type="n",yaxt="n",xlab="",ylab="",
     main="Months in T-bills (red = market fell, pink = market rose)")
axis(2,at=4:1,labels=c("Fell 5%+","LASSO","Ensemble","RF"),las=1,cex.axis=0.8)
shade(0,5)
big=f1[f1$model=="Historical Mean"&f1$actual<=-0.05,]
points(big$return_ends,rep(4,nrow(big)),pch="|",col="black")
for(i in 1:3){
  d=f1[f1$model==rows[i]&f1$forecast<=0,] #months the model switched to T-bills
  points(d$return_ends,rep(4-i,nrow(d)),pch="|",col=ifelse(d$actual<0,"red","pink"))
}
dev.off()

#Print the main results
options(width=150)
cat("\nOut-of-sample R2 (%), relative to the Historical Mean (benchmark)\n")
print(round(oosr2.table,2))
print(round(rmse.table,5)) 
print(dmres,digits=4,row.names=FALSE)
print(trading,digits=4,row.names=FALSE)

#Save results
save.image(file.path(cache_dir,"main_results.RData"))
