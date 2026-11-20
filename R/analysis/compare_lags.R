# Compare the fixed L4 robustness specification with the saved L1 baseline.
source("config.R")

read.lag.table=function(specification,filename){
  path=file.path(results_root,specification,"tables",filename)
  if(!file.exists(path)) stop("Missing ",path,". Restore the saved results or evaluate both lag specifications first.")
  read.csv(path,check.names=FALSE,stringsAsFactors=FALSE)
}

align.tables=function(l1,l4,keys){
  if(!all(keys %in% names(l1)) || !all(keys %in% names(l4))) stop("Missing comparison key columns.")
  if(anyNA(l1[keys]) || anyNA(l4[keys]) || anyDuplicated(l1[keys]) || anyDuplicated(l4[keys])){
    stop("Comparison keys must be complete and unique: ",paste(keys,collapse=", "))
  }
  left=l1[do.call(order,l1[keys]),,drop=FALSE]
  right=l4[do.call(order,l4[keys]),,drop=FALSE]
  rownames(left)=rownames(right)=NULL
  if(!identical(left[keys],right[keys])) stop("L1 and L4 comparison keys do not match.")
  list(l1=left,l4=right)
}

compare.metrics=function(filename,keys,metrics){
  aligned=align.tables(read.lag.table("baseline_l1",filename),read.lag.table("robustness_l4",filename),keys)
  out=aligned$l1[keys]
  for(metric in metrics){
    a=aligned$l1[[metric]];b=aligned$l4[[metric]]
    if(!is.numeric(a) || !is.numeric(b) || any(!is.finite(c(a,b)))) stop("Invalid numeric metric: ",metric)
    out[[paste0(metric,"_L1")]]=a
    out[[paste0(metric,"_L4")]]=b
    out[[paste0(metric,"_delta_L4_minus_L1")]]=b-a
  }
  # Present horizons chronologically 
  out=out[order(match(out$horizon,c("1-month","3-month","6-month","12-month"))),,drop=FALSE]
  rownames(out)=NULL
  out
}

# Compare the same forecast observations before interpreting metric differences.
forecasts=align.tables(read.lag.table("baseline_l1","all_forecasts.csv"),
                      read.lag.table("robustness_l4","all_forecasts.csv"),
                      c("horizon_months","model","return_ends"))
if(!isTRUE(all.equal(forecasts$l1$actual,forecasts$l4$actual,tolerance=1e-12))){
  stop("L1 and L4 realised returns differ.")
}
for(model in c("Historical Mean","Full OLS")){
  rows=forecasts$l1$model==model
  if(!any(rows) || !isTRUE(all.equal(forecasts$l1$forecast[rows],forecasts$l4$forecast[rows],tolerance=1e-12))){
    stop("L1 and L4 must use identical one-lag benchmarks: ",model)
  }
}
accuracy=compare.metrics("model_metrics.csv",c("horizon","model"),c("RMSE","MAE","OOS_R2_pct"))
trading=compare.metrics("trading_performance.csv",c("horizon","strategy"),
                        c("final_value_of_100","sharpe_ratio","max_drawdown_pct"))
dir.create(comparison_dir,recursive=TRUE,showWarnings=FALSE)
write.csv(accuracy,file.path(comparison_dir,"forecast_accuracy.csv"),row.names=FALSE)
write.csv(trading,file.path(comparison_dir,"trading_performance.csv"),row.names=FALSE)
cat("\nL1/L4 comparison tables saved to ",comparison_dir,"/. Deltas are L4 minus L1; percentage metric deltas are percentage points.\n",sep="")
