#####################################################################################
# eda_correlations.R - Descriptive correlations of the 122 base predictors
#####################################################################################
# Run from the project root: Rscript --vanilla R/analysis/eda_correlations.R [output_directory]
# transform.R also calls this after constructing the panel.

plot.predictor.correlation.matrix=function(correlation,dates){
  # Group strongly related series together, preserving correlation signs in the colours.
  order=hclust(as.dist(1-abs(correlation)),method="average")$order
  correlation=correlation[order,order]
  n=ncol(correlation)
  colours=colorRampPalette(c("#B35806","#F7F7F7","#2166AC"))(201)
  breaks=seq(-1,1,length.out=length(colours)+1)

  layout(matrix(c(1,2),nrow=1),widths=c(15,1.5))
  par(oma=c(3,0,4,0),mar=c(8,8,1,1))
  # Reverse the vertical coordinate so identical row and column orders start at top left.
  image(seq_len(n),seq_len(n),t(correlation[n:1,]),
        col=colours,breaks=breaks,axes=FALSE,xlab="",ylab="",asp=1,useRaster=TRUE)
  axis(1,at=seq_len(n),labels=colnames(correlation),las=2,cex.axis=0.55,tick=FALSE,gap.axis=-1)
  axis(2,at=seq_len(n),labels=rev(rownames(correlation)),las=2,cex.axis=0.55,tick=FALSE,gap.axis=-1)
  box(col="grey60")

  par(mar=c(8,1,1,5))
  plot.new()
  plot.window(xlim=c(0,1),ylim=c(-1,1),xaxs="i",yaxs="i")
  rect(0,head(breaks,-1),1,tail(breaks,-1),col=colours,border=NA)
  axis(4,at=c(-1,-0.5,0,0.5,1),las=1,cex.axis=0.85)
  box(col="grey60")
  mtext("Pearson r",side=3,line=0.3,cex=0.9)

  mtext(paste("Correlation matrix of the",n,"base predictors"),
        side=3,outer=TRUE,line=2,font=2,cex=1.4)
  mtext(sprintf("%s to %s | %d complete monthly observations",min(dates),max(dates),length(dates)),
        side=3,outer=TRUE,line=0.5,cex=1)
  mtext("Full-sample descriptive correlations | Ordered by average-linkage clustering of 1 - |r|",
        side=1,outer=TRUE,line=1,cex=0.9)
}

write.predictor.correlations=function(panel,results_dir){
  x_cols=c(setdiff(names(panel),c("date","Mkt_RF","RF","R_mkt","Y_h1","Y_h3","Y_h6","Y_h12")),"Mkt_RF")
  complete=complete.cases(panel[,x_cols])
  X=panel[complete,x_cols]
  dates=as.Date(panel$date[complete])
  stopifnot(nrow(X)>1,ncol(X)>1,all(vapply(X,sd,numeric(1))>0))

  correlation=cor(X,method="pearson")
  index=which(upper.tri(correlation),arr.ind=TRUE)
  vars=read.csv("data/variable_descriptions.csv",check.names=FALSE)
  pairs=data.frame(
    variable_1=colnames(correlation)[index[,1]],
    variable_2=colnames(correlation)[index[,2]],
    correlation=correlation[index]
  )
  pairs$description_1=vars$description[match(pairs$variable_1,vars$code)]
  pairs$description_2=vars$description[match(pairs$variable_2,vars$code)]
  pairs$absolute_correlation=abs(pairs$correlation)
  pairs=pairs[order(-pairs$absolute_correlation),
              c("variable_1","description_1","variable_2","description_2","correlation","absolute_correlation")]

  summary=data.frame(
    sample_start=min(dates),sample_end=max(dates),observations=nrow(X),
    predictors=ncol(X),unique_pairs=nrow(pairs),
    mean_absolute_correlation=mean(pairs$absolute_correlation),
    median_absolute_correlation=median(pairs$absolute_correlation),
    maximum_absolute_correlation=max(pairs$absolute_correlation)
  )
  for(threshold in c(0.5,0.8,0.9)){
    suffix=gsub("\\.","_",as.character(threshold))
    summary[[paste0("pairs_abs_r_ge_",suffix)]]=sum(pairs$absolute_correlation>=threshold)
    summary[[paste0("pct_pairs_abs_r_ge_",suffix)]]=100*mean(pairs$absolute_correlation>=threshold)
  }

  dir.create(file.path(results_dir,"tables"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(results_dir,"figures"),recursive=TRUE,showWarnings=FALSE)
  write.csv(summary,file.path(results_dir,"tables/predictor_correlation_summary.csv"),row.names=FALSE)
  write.csv(pairs,file.path(results_dir,"tables/predictor_correlation_pairs.csv"),row.names=FALSE)
  write.csv(data.frame(variable=rownames(correlation),correlation,check.names=FALSE),
            file.path(results_dir,"tables/predictor_correlation_matrix.csv"),row.names=FALSE)

  png(file.path(results_dir,"figures/predictor_correlation_heatmap.png"),
      width=4200,height=4000,res=240)
  tryCatch(plot.predictor.correlation.matrix(correlation,dates),finally=dev.off())

  cat("\nPredictor correlations (common complete sample; full-sample descriptive analysis):\n")
  print(summary,row.names=FALSE)
  cat("\nCorrelation tables and heatmap saved to ",results_dir,"/.\n",sep="")
  invisible(list(summary=summary,pairs=pairs,correlation=correlation))
}

# When sourced by transform.R, only define the function.
if(sys.nframe()==0){
  source("config.R")
  args=commandArgs(trailingOnly=TRUE)
  if(length(args)>1) stop("Usage: Rscript --vanilla R/analysis/eda_correlations.R [output_directory]")
  results_dir=if(length(args)==1) args[1] else shared_results_dir
  panel=read.csv("data/processed/monthly_panel_1960_2025.csv",check.names=FALSE)
  write.predictor.correlations(panel,results_dir)
}
