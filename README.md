# Forecasting the US Equity Premium with Machine Learning

Can a large set of macroeconomic indicators predict US stock market excess returns better than simply using the historical average, and can those forecasts be traded profitably?

This project forecasts monthly US market excess returns (Fama–French Mkt–RF) at **1, 3, 6 and 12-month** horizons using **121 FRED-MD macro-financial indicators**. It compares penalised regressions, dimension reduction, tree ensembles and forecast combinations in a **30-year rolling-window, pseudo-out-of-sample** design (test period 2006–2025). Forecasts are then evaluated as a **market / T-bill timing strategy** with transaction costs.

## Results

👉 **[View the results slides](https://qyunhan.github.io/equity-premium-forecasting/)**: methodology, findings and trading performance in 12 slides.

(Offline: open [`docs/index.html`](docs/index.html) in a browser.)

---

## Methodology

**Data**
- **Predictors:** 121 monthly macro-financial series from FRED-MD (September 2026 vintage). Series are made stationary with the FRED-MD transformation codes and lagged one month to avoid look-ahead. The current-month Mkt–RF is also included.
- **Target:** compounded h-month market excess return, built from Kenneth French's Mkt–RF and RF series.
- **Sample:** 792 months, January 1960 to December 2025.

**Forecasting design**
- **Direct h-step forecasts** for h = 1, 3, 6 and 12 months.
- **Rolling 360-month window**, re-estimated every month. Only observations with a fully realised target enter training, so there is no leakage.
- **Validation period (1995–2005):** used only to choose the number of PLS components and the ensemble weights.
- **Test period (2006–2025):** used for all reported results.

**Models**

| Group | Models | Tuning |
|---|---|---|
| Benchmarks | Historical mean, full OLS | none |
| Penalised regression | Ridge, LASSO, Elastic Net (α = 0.5) | λ chosen by **BIC** within each window |
| Dimension reduction | PC + LASSO (4 PCs re-estimated per window), PLS | BIC / validation RMSE |
| Tree ensembles | Bagging, random forest | defaults, fixed seeds |
| Combinations | Equal-weight average, constrained Granger–Ramanathan | weights fit on validation forecasts |

**Evaluation**
- **Accuracy:** OOS R², RMSE and MAE.
- **Significance:** Diebold–Mariano tests vs the historical mean, with Newey–West variance (lag 6, or 11 at 12 months, because overlapping forecasts are autocorrelated).
- **Trading:** a positive forecast means hold the market, otherwise hold T-bills. For h > 1, h overlapping positions are averaged (Jegadeesh–Titman style) and rebalanced monthly with **0.1% transaction costs**. Reported as terminal wealth, Sharpe ratio and maximum drawdown.
- **Interpretation:** how often each predictor is selected (LASSO, Elastic Net), random forest permutation importance, and sparsity over time.
- **Robustness:** every model is re-run with **4 lags** of each predictor (488 inputs). The conclusions do not change.

---

## Repository structure

```text
├── run_all.R                 # one-command pipeline (data → models → evaluation)
├── config.R                  # lag specification and results paths
├── R/
│   ├── data/transform.R      # builds the panel from raw FRED-MD + Fama–French files
│   ├── models/               # one script per model; each saves rolling-window forecasts
│   │   ├── common.R          # shared rolling-window engine and data loading
│   │   ├── benchmarks.R  ridge.R  lasso.R  elastic_net.R
│   │   └── pc_lasso.R  pls.R  bagging.R  random_forest.R
│   └── analysis/
│       ├── evaluate.R        # ensembles, accuracy, DM tests, trading, interpretation
│       ├── descriptive.R     # descriptive statistics and plots
│       ├── eda_correlations.R# 122 × 122 predictor correlation analysis
│       └── compare_lags.R    # 1-lag vs 4-lag comparison
├── data/
│   ├── raw/                  # FRED-MD and Fama–French source files
│   ├── processed/            # monthly panel, 1960–2025
│   └── variable_descriptions.csv
├── results/
│   ├── baseline_l1/          # main specification: tables, figures, cached forecasts
│   ├── robustness_l4/        # 4-lag robustness: tables and figures
│   ├── shared/               # descriptive outputs and cached benchmarks
│   └── comparison/           # 1-lag vs 4-lag tables
└── docs/                     # HTML slide deck (index.html)
```

The code separates **fitting** from **evaluation**. Model scripts only fit models and save forecasts. `evaluate.R` then regenerates every metric, test and chart from those saved forecasts, so results can be reproduced without refitting.

## How to run

```r
install.packages(c("glmnet", "pls", "randomForest", "sandwich", "lsei", "githubinstall"))
githubinstall::githubinstall("HDeconometrics")   # ic.glmnet for BIC tuning
```

```sh
# Re-evaluate from cached forecasts (fast, no refitting)
Rscript --vanilla run_all.R --lags=1 --results-only

# Full pipeline: refit all models for the baseline or the 4-lag robustness check
Rscript --vanilla run_all.R --lags=1
Rscript --vanilla run_all.R --lags=4 --compare-lags

# Rebuild the panel from the raw files first
Rscript --vanilla run_all.R --lags=1 --rebuild-data
```

Cached forecasts are included for the 1-lag baseline. The 4-lag specification needs a full refit, which is slow for the tree models.

### Main outputs (`results/baseline_l1/`)

| File | Contents |
|---|---|
| `tables/oos_r2_table.csv` | OOS R² by model and horizon (main result) |
| `tables/dm_tests.csv` | Diebold–Mariano statistics and p-values |
| `tables/trading_performance.csv` | Terminal wealth, Sharpe ratio, maximum drawdown |
| `tables/variable_selection.csv` | Share of windows in which LASSO / Elastic Net select each predictor |
| `tables/rf_variable_importance.csv` | Random forest permutation importance |
| `figures/forecasts_h*.png` | Forecasts vs realised returns |
| `figures/sparsity_*.png` | Number of selected predictors per window |
| `figures/switching_1m.png` | Timing strategy wealth and T-bill months |

## Data sources

- **FRED-MD:** McCracken & Ng, Federal Reserve Bank of St. Louis ([link](https://www.stlouisfed.org/research/economists/mccracken/fred-databases))
- **Fama–French factors:** [Kenneth French Data Library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)

## Limitations

- The trading gain depends on one episode (the 2008 financial crisis), and the data are revised rather than real-time vintages.
- The models minimise squared error, but the strategy only uses the forecast's sign. A loss function that targets direction or penalises missed downturns would fit the trading objective better.
- 12-month forecasts overlap heavily (only about 19 non-overlapping blocks in the test period), which limits statistical power.

## Team

Built with Lin Ruiyu, Liu Yiwei and Wilbert Christopher Kurniawan. The rolling-window forecasting functions follow Medeiros et al. (2019).
