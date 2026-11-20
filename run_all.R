# Run from any working directory; supporting scripts use the project root.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_arg)) {
  setwd(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
}

args <- commandArgs(TRUE)
usage <- paste(
  "Usage: Rscript --vanilla run_all.R [--lags=1|4]",
  "[--results-only] [--rebuild-data] [--compare-lags]"
)
if (identical(args, "--help")) {
  cat(usage, "\n")
  quit(status = 0)
}
lag_args <- grep("^--lags=", args, value = TRUE)
unknown <- args[!args %in% c("--rebuild-data", "--results-only", "--compare-lags", "--lags=1", "--lags=4")]
if (length(unknown) || length(lag_args) > 1 || anyDuplicated(args)) {
  stop("Invalid or repeated argument(s). ", usage)
}
if (length(lag_args)) Sys.setenv(ERP_NLAGS = sub("^--lags=", "", lag_args))
source("config.R")
# Export the selected default as well, so every child uses the same specification.
Sys.setenv(ERP_NLAGS = as.character(nlags))
results_only <- "--results-only" %in% args
compare_lags <- "--compare-lags" %in% args

if (!file.exists(benchmark_file)) {
  stop("Missing ", benchmark_file,
       ". Restore the saved one-lag benchmarks or rebuild with ",
       "ERP_NLAGS=1 Rscript --vanilla R/models/benchmarks.R from the project root.")
}
model_names <- c("ridge", "lasso", "elastic_net", "pc_lasso", "pls", "bagging", "random_forest")
if (results_only) {
  model_files <- file.path(cache_dir, paste0(model_names, "_results.RData"))
  missing <- model_files[!file.exists(model_files)]
  if (length(missing)) {
    stop("Missing saved model forecast(s): ", paste(missing, collapse = ", "),
         ". Restore the caches or rerun without --results-only to fit the selected lag specification.")
  }
}
required_packages <- c("sandwich", "lsei")
if (!results_only) required_packages <- c(required_packages, "glmnet", "HDeconometrics", "pls", "randomForest")
missing <- required_packages[!vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) stop("Install missing package(s): ", paste(missing, collapse = ", "),
                          ". HDeconometrics is available from GitHub.")

if ("--rebuild-data" %in% args) {
  raw_files <- file.path("data", "raw", c("FRED_MD_2026_09.csv", "Fama_French_Factors.csv"))
  missing <- raw_files[!file.exists(raw_files)]
  if (length(missing)) stop("Cannot rebuild the panel; missing: ", paste(missing, collapse = ", "))
  scripts <- "R/data/transform.R"
} else {
  panel_file <- "data/processed/monthly_panel_1960_2025.csv"
  if (!file.exists(panel_file)) stop("Missing ", panel_file, ". Rerun with --rebuild-data.")
  scripts <- "R/analysis/descriptive.R"
}
if (!results_only) scripts <- c(scripts, file.path("R", "models", paste0(model_names, ".R")))
scripts <- c(scripts, "R/analysis/evaluate.R")
if (compare_lags) scripts <- c(scripts, "R/analysis/compare_lags.R")
missing <- scripts[!file.exists(scripts)]
if (length(missing)) stop("Missing pipeline script(s): ", paste(missing, collapse = ", "))

rscript <- Sys.which("Rscript")
if (!nzchar(rscript)) stop("Rscript was not found on PATH.")
pipeline_start <- Sys.time()
cat("Running L", nlags, if (nlags == 1) " baseline" else " robustness check",
    if (results_only) " from saved forecasts" else " with model fitting", ".\n", sep = "")
for (i in seq_along(scripts)) {
  cat(sprintf("\n[%d/%d] Running %s\n", i, length(scripts), scripts[i]))
  status <- system2(rscript, c("--vanilla", shQuote(normalizePath(scripts[i]))), stdout = "", stderr = "")
  if (!identical(status, 0L)) stop("Pipeline stopped: ", scripts[i], " failed with exit status ", status, ".")
}
cat(sprintf("\nCompleted in %.1f minutes. Results: %s/; shared descriptions: %s/.\n",
            as.numeric(difftime(Sys.time(), pipeline_start, units = "mins")),
            results_dir, shared_results_dir))
if (compare_lags) cat("Comparison: ", comparison_dir, "/.\n", sep = "")
