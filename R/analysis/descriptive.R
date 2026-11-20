# Shared descriptive outputs, independent of the model lag specification.
source("R/analysis/eda_correlations.R")

write.descriptive.outputs=function(panel,shared_results_dir){
  ########################################
  # Descriptive statistics and plots
  ########################################

  # Summary statistics of the targets (h-month excess returns) and of a few key predictors, 1960-2025.
  dir.create(file.path(shared_results_dir,"tables"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(shared_results_dir,"figures"),recursive=TRUE,showWarnings=FALSE)

  describe=function(x,name){
    x=x[!is.na(x)]
    data.frame(series=name,observations=length(x),mean=mean(x),sd=sd(x),
               min=min(x),max=max(x),AR1=cor(x[-1],x[-length(x)]))
  }

  desc=rbind(
    describe(panel$Y_h1,"Target: 1-month excess return"),
    describe(panel$Y_h3,"Target: 3-month excess return"),
    describe(panel$Y_h6,"Target: 6-month excess return"),
    describe(panel$Y_h12,"Target: 12-month excess return"),
    describe(panel$RF,"T-bill return (monthly)"),
    describe(panel$T1YFFM,"Spread: 1-year Treasury - fed funds"),
    describe(panel$T10YFFM,"Spread: 10-year Treasury - fed funds"),
    describe(panel$BAAFFM,"Spread: Baa corporate - fed funds"),
    describe(panel[["S&P div yield"]],"S&P 500 dividend yield (transformed)"),
    describe(panel$UNRATE,"Unemployment rate (transformed)")
  )
  desc[,3:7]=round(desc[,3:7],4)
  write.csv(desc,file.path(shared_results_dir,"tables/descriptive_statistics.csv"),row.names=FALSE)
  cat("\n9. Descriptive statistics:\n")
  print(desc,row.names=FALSE)

  # Plot: monthly and 12-month excess returns, with the validation and test periods shaded.
  # Returns are dated by the month in which they end.
  png(file.path(shared_results_dir,"figures/data_excess_returns.png"),width=1600,height=1100,res=150)
  par(mfrow=c(2,1),mar=c(4,4.5,3,1))
  for(h in c(1,12)){
    n=nrow(panel); y=panel[[paste0("Y_h",h)]][1:(n-h)]; d=panel$date[(1+h):n] # Y_h of month t ends in month t+h
    plot(d,y,type="n",ylim=range(y,na.rm=TRUE)+c(0,0.25*diff(range(y,na.rm=TRUE))),xlab="Month in which the return ends",
         ylab="Excess return (0.05 = 5%)",main=paste0(h,"-month excess return of the US market over T-bills, 1960-2025"))
    rect(as.Date("1995-01-01"),-10,as.Date("2005-12-01"),10,col="grey90",border=NA)
    rect(as.Date("2006-01-01"),-10,as.Date("2025-12-01"),10,col="grey80",border=NA)
    lines(d,y,lwd=1)
    abline(h=0,lty=2)
    abline(h=mean(y,na.rm=TRUE),col="red",lwd=1.5)
    legend("topleft",c("Validation period","Test period","Sample mean"),fill=c("grey90","grey80",NA),border=c(NA,NA,NA),
           lty=c(NA,NA,1),col=c(NA,NA,"red"),bty="n",cex=0.85,ncol=3)
  }
  dev.off()

  cat("\nDescriptive statistics and plot saved to ",shared_results_dir,"/.\n",sep="")

  write.predictor.correlations(panel,shared_results_dir)
}

if(sys.nframe()==0){
  source("config.R")
  panel=read.csv("data/processed/monthly_panel_1960_2025.csv",check.names=FALSE)
  panel$date=as.Date(panel$date)
  write.descriptive.outputs(panel,shared_results_dir)
}
