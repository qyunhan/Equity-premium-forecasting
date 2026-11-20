# L1 is the baseline; L4 is a fixed robustness check for the seven ML models.
lag_setting=Sys.getenv("ERP_NLAGS","1")
if(!lag_setting %in% c("1","4")) stop("ERP_NLAGS must be 1 or 4.")
nlags=as.integer(lag_setting)

# Override the root to evaluate a copy of the saved results, e.g. for verification.
results_root=Sys.getenv("ERP_RESULTS_ROOT","results")
results_dir=file.path(results_root,if(nlags==1) "baseline_l1" else "robustness_l4")
cache_dir=file.path(results_dir,"cache")
shared_results_dir=file.path(results_root,"shared")
benchmark_file=file.path(shared_results_dir,"benchmarks_results.RData")
comparison_dir=file.path(results_root,"comparison","tables")
