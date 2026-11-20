#####################################################################################
# transform.R - Builds the monthly data panel from the raw FRED-MD and Fama-French files
#####################################################################################

rm(list=ls())
source("config.R")

fred_path="data/raw/FRED_MD_2026_09.csv"
ff_path="data/raw/Fama_French_Factors.csv"
dir.create("data/processed",showWarnings=FALSE,recursive=TRUE)

start=as.Date("1960-01-01")
end=as.Date("2025-12-01")


########################################
# 1. Load FRED-MD data
########################################

#FRED-MD format: sasdate (MM/DD/YYYY), predictor headers, then a transformation-code row.
raw=read.csv(fred_path,check.names=FALSE,na.strings=c("","NA"))
names=colnames(raw)[-1]
tcodes=setNames(as.integer(raw[1,-1]),names)

fred=raw[-1,]
fred$date=as.Date(fred$sasdate,"%m/%d/%Y")
fred$sasdate=NULL
fred[names]=lapply(fred[names],as.numeric)
fred=fred[fred$date<=end,]

cat("\n1. FRED-MD:",nrow(fred),"months,",length(names),"predictors\n")


########################################
# 2. EDA: Missing observations
########################################

sample=fred$date>=start & fred$date<=end
train=fred$date>=start & fred$date<=as.Date("1994-12-01")

missing=data.frame(
  variable=names,
  train=colSums(is.na(fred[train,names])),
  full=colSums(is.na(fred[sample,names]))
)

missing=missing[order(-missing$full),]

cat("\n2. Predictors with missing observations (1960-2025):\n")
print(missing[missing$full>0,],row.names=FALSE)


###################################################
# 3. Exclude incomplete predictors and forward-fill
###################################################

excluded=c("ACOGNO","ANDENOx","TWEXAFEGSMTHx",
           "UMCSENTx","VIXCLSx")

keep=setdiff(names,excluded)
fred=fred[,c("date",keep)]

# Verify no retained predictor has consecutive missing months.
for(v in keep){
  x=is.na(fred[[v]][sample])
  stopifnot(!any(x[-1] & x[-length(x)]))
}

# Fill missing raw values using the previous month.
filled=data.frame(date=as.Date(character()),variable=character())

for(v in keep){
  idx=which(is.na(fred[[v]]) & sample)
  for(i in idx){
    stopifnot(i>1,!is.na(fred[[v]][i-1]))
    fred[[v]][i]=fred[[v]][i-1]
    filled=rbind(filled,data.frame(date=fred$date[i],variable=v))
  }
}

cat("\n3. Excluded predictors:\n")
print(excluded)
cat("\nForward-filled observations:\n")
print(filled,row.names=FALSE)


########################################
# 4. Apply FRED-MD transformations
########################################

lag1=function(x) c(NA,head(x,-1))
diff1=function(x) x-lag1(x)

transform=function(x,code){
  if(code==1) return(x)
  if(code==2) return(diff1(x))
  if(code==3) return(diff1(diff1(x)))
  if(code %in% 4:6){
    x[x<=0]=NA
    x=log(x)
    if(code==4) return(x)
    if(code==5) return(diff1(x))
    return(diff1(diff1(x)))
  }
  if(code==7) return(diff1(x/lag1(x)-1))
  stop("Invalid TCODE")
}

for(v in keep){
  fred[[v]]=transform(fred[[v]],tcodes[v])
  fred[[v]][!is.finite(fred[[v]])]=NA
}

cat("\n4. Transformation codes:\n")
print(table(tcodes[keep]))


########################################
# 5. Load Fama-French monthly returns
########################################

lines=readLines(ff_path)
monthly=grep("^\\s*[0-9]{6}\\s*,",lines,value=TRUE)

# Keep monthly data only, excluding annual factor observations.
annual=grep("Annual Factors",lines,fixed=TRUE)
if(length(annual)) monthly=grep("^\\s*[0-9]{6}\\s*,",
                                lines[seq_len(annual[1]-1)],value=TRUE)

#Monthly fields: YYYYMM, Mkt-RF, SMB, HML, RF; returns are percentages.
ff=read.csv(text=paste(monthly,collapse="\n"),header=FALSE)
stopifnot(ncol(ff)==5)
colnames(ff)=c("YYYYMM","Mkt_RF","SMB","HML","RF")

ff$date=as.Date(paste0(ff$YYYYMM,"01"),"%Y%m%d")
ff$Mkt_RF=ff$Mkt_RF/100
ff$RF=ff$RF/100
ff=ff[,c("date","Mkt_RF","RF")]

cat("\n5. Fama-French:",nrow(ff),"monthly observations\n")


########################################
# 6. Merge data and construct predictors
########################################

panel=merge(fred,ff,by="date")

# Macro predictors at t-1; market excess return at t.
for(v in keep) panel[[v]]=lag1(panel[[v]])

panel$R_mkt=panel$Mkt_RF+panel$RF


########################################
# 7. Construct forward excess returns
########################################

# Y_h(t) = compounded market return minus compounded RF,
# over months t+1,...,t+h.

for(h in c(1,3,6,12)){
  y=rep(NA_real_,nrow(panel))
  for(i in seq_len(nrow(panel)-h)){
    j=(i+1):(i+h)
    if(panel$date[max(j)]>end) next
    y[i]=prod(1+panel$R_mkt[j])-prod(1+panel$RF[j])
  }
  panel[[paste0("Y_h",h)]]=y
}

panel=panel[panel$date>=start & panel$date<=end,]
rownames(panel)=NULL

stopifnot(nrow(panel)==792,
          identical(panel$date,seq(start,end,by="month")))

cat("\n7. Final dataset:",nrow(panel),"months\n")
cat("FRED predictors:",length(keep),"\n")

for(h in c(1,3,6,12))
  cat("Y_h",h,":",sum(!is.na(panel[[paste0("Y_h",h)]])),
      "valid observations\n",sep="")


########################################
# 8. Export final dataset
########################################

write.csv(panel,"data/processed/monthly_panel_1960_2025.csv",
          row.names=FALSE,na="")

cat("\nData preparation complete.\n")


# Refresh the shared descriptive outputs after rebuilding the panel.
source("R/analysis/descriptive.R")
write.descriptive.outputs(panel,shared_results_dir)
