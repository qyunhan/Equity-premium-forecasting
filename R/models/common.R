#####################################################################################
# common.R - Data used by every model script
#####################################################################################

source("config.R")

panel=read.csv("data/processed/monthly_panel_1960_2025.csv",check.names=FALSE) #panel built by transform.R
panel$date=as.Date(panel$date)

#Columns of the panel:
#  Mkt_RF, RF, R_mkt   - market excess return, T-bill return and market return in month t (Fama-French)
#  Y_h1,...,Y_h12      - excess return over the NEXT h months, t+1,...,t+h (compounded market minus compounded T-bill)
#  all other columns   - 121 FRED-MD series, transformed and lagged one month (transform.R)
horizons=c(1,3,6,12) #forecast horizons in months
x_cols=c(setdiff(names(panel),c("date","Mkt_RF","RF","R_mkt",paste0("Y_h",horizons))),"Mkt_RF") #the 122 predictors: 121 FRED-MD series + this month's market excess return

window_size=360 #every model is estimated on the latest 360 months (30 years)
x_names=x_cols #first block keeps the original predictor names
if(nlags>1) for(l in 2:nlags) x_names=c(x_names,paste0(x_cols,"_lag",l))
#At the forecast origin t: x_cols is the panel at t; _lag2, _lag3, _lag4 are t-1, t-2, t-3.
#The panel's FRED-MD series are already lagged one month; Mkt_RF is contemporaneous.

#y is an h-month return, which is a different series for each horizon, so make.Y() builds one data matrix per horizon:
#  column 1      = y(t): excess return over the h months ENDING in month t
#  other columns = the 122 predictors in month t
#Inside the model functions, embed() then pairs each return with the predictors from h,...,h+nlags-1 months earlier.
#Inputs: h = horizon, last = last month whose return is forecast, nprev = number of forecasts
make.Y=function(h,last,nprev){
  yback=c(rep(NA,h),head(panel[[paste0("Y_h",h)]],-h)) #return over t-h+1,...,t
  Y=cbind(y=yback,as.matrix(panel[,x_cols])) #data matrix: y first, then the predictors
  rownames(Y)=as.character(panel$date)
  end=match(last,panel$date)
  Y=Y[(end-(nprev+window_size+2*h-1+nlags-1)+1):end,] #keep 360 observations after embedding, allowing nlags-1 extra months
  stopifnot(!anyNA(Y))
  return(Y)
}

#Test period: forecasts made at the end of Dec 2005 onwards, for returns ending by Dec 2025.
#nprev = number of out-of-sample forecasts; fewer for longer h, since the last return must end by Dec 2025
nprev1=240; nprev3=238; nprev6=235; nprev12=229
Y1=make.Y(1,as.Date("2025-12-01"),nprev1)
Y3=make.Y(3,as.Date("2025-12-01"),nprev3)
Y6=make.Y(6,as.Date("2025-12-01"),nprev6)
Y12=make.Y(12,as.Date("2025-12-01"),nprev12)

oosy1=tail(Y1[,1],nprev1)
oosy3=tail(Y3[,1],nprev3)
oosy6=tail(Y6[,1],nprev6)
oosy12=tail(Y12[,1],nprev12)

#Validation period: forecasts for returns ending Jan 1995 ... Dec 2005, built the same way.
#Used only to choose the number of PLS components (pls.R) and to estimate the GR ensemble weights (evaluate.R).
nvald1=132; nvald3=130; nvald6=127; nvald12=121
Yv1=make.Y(1,as.Date("2005-12-01"),nvald1)
Yv3=make.Y(3,as.Date("2005-12-01"),nvald3)
Yv6=make.Y(6,as.Date("2005-12-01"),nvald6)
Yv12=make.Y(12,as.Date("2005-12-01"),nvald12)

oosyv1=tail(Yv1[,1],nvald1)
oosyv3=tail(Yv3[,1],nvald3)
oosyv6=tail(Yv6[,1],nvald6)
oosyv12=tail(Yv12[,1],nvald12)

dir.create(file.path(results_dir,"tables"),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(results_dir,"figures"),recursive=TRUE,showWarnings=FALSE)

dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
