# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **Common Task Framework (CTF)** competition repository for the paper "The Power of the Common Task Framework" by Hellum, Jensen, Kelly, and Pedersen (2025). The goal is to find the portfolio with the highest Sharpe ratio. Models are submitted to [https://jkpfactors.com/ctf/submit](https://jkpfactors.com/ctf/submit).

## Package Management

Uses **uv** as the Python package manager with `pyproject.toml` and `uv.lock`. Python >=3.13 required.

```bash
uv sync              # Install dependencies
uv add <package>     # Add a new dependency
uv run <script>      # Run a script in the virtual environment
```

## Running Models

There is no formal test suite or build system. Models are run as standalone scripts:

```bash
uv run python models_python/one_over_n/one_over_N.py
uv run python scripts/data_prep.py
```

R models `source()` shared utilities from `utils/R/` and are run directly in R. For HPC submission, `scripts/build_model.R` inlines all `source()` calls into a single `*_standalone.R` file. The SLURM scripts handle this automatically:

```bash
# Build standalone (inlines source() calls) then run
Rscript scripts/build_model.R models_R/markowitz-ml/markowitz_ml.R
Rscript -e 'source("models_R/markowitz-ml/markowitz_ml_standalone.R"); ...'
```

## Architecture

### Data Flow

```
data/raw/ (parquet)  →  Data Prep (utils/data_prep.py)  →  Model  →  data/processed/{model}/ (CSV)
```

Input data consists of three parquet files in `data/raw/`:
- `ctff_chars.parquet` — stock characteristics
- `ctff_features.parquet` — feature column names
- `ctff_daily_ret.parquet` — daily returns

### Model Contract

Every model (Python or R) must expose a `main()` function that takes `chars`, `features`, and `daily_ret` as inputs and returns a DataFrame with columns: **id**, **eom** (end-of-month date), **w** (portfolio weights). This is the format required for submission to the competition website.

**Python:**
```python
def main(chars: pd.DataFrame, features: pd.DataFrame, daily_ret: pd.DataFrame) -> pd.DataFrame:
```

**R:**
```r
main <- function(chars, features, daily_ret) { ... }  # returns data.frame/data.table/tibble
```

### Key Data Columns

- `id` — stock identifier
- `eom` — end-of-month date (primary grouping key)
- `ctff_test` — test set indicator (filter to `== 1` for submission)
- `ret_exc_lead1m` — one-month-ahead excess return
- `me` — market equity

### Adding a New Model

1. Save the script in a folder under `models_python/` or `models_R/`
2. For R models, `source()` shared utilities from `utils/R/` as needed (e.g., `data_prep.R`, `factor_model_utils.R`, `xgb_utils.R`)
3. Add a `*_testing.R` file that sources `utils/R/local_testing.R` and calls `run_toy_tests()` / `validate_portfolio()`
4. Add a SLURM script that calls `scripts/build_model.R` to generate a standalone file before running the model
5. Save CSV output under `data/processed/{model_name}/`
6. Save documentation under `documentation/{model_name}/`

### Key Libraries

- **Polars** is the primary DataFrame library for data processing (preferred over pandas for new code)
- **Pandas** is used at the model interface boundary (input/output of `main()`)
- `utils/data_prep.py` provides `impute_and_rank()` for percentile ranking and missing value imputation
- `private/settings.py` uses Pydantic `BaseSettings` for configuration (env var prefixes: `COV_`, `APP_`)
- **data.table** is the primary R DataFrame library for data manipulation
- **xgboost** is used for gradient-boosted tree models in R
- **arrow** is used for reading parquet files in R
- `utils/R/data_prep.R` provides `impute_and_rank()` for R models
- `utils/R/factor_model_utils.R` provides Barra factor model helpers (regressions, covariance estimation)
- `utils/R/xgb_utils.R` provides XGBoost hyperparameter tuning and training helpers
- `utils/R/local_testing.R` provides `run_toy_tests()` and `validate_portfolio()` for model validation
- `scripts/build_model.R` inlines `source()` calls to produce standalone R files for HPC submission

### Directories Not in Git

`data/` (raw, interim, processed, wrds), `output/` (figures, tables), and `private/` are gitignored.

### Python Environment
- Python installation: `c:\\Users\\tij2\\Dropbox\\Research\\Active projects\\CTF-github\\.venv\\Scripts\\python.exe`
- Virtual environment: `.venv/`
- Package manager: uv

### R Environment
- R installation: `C:/Program Files/R/R-4.5.1/bin/x64/R.exe`
- Renv library: `renv/library/`

### Preferred R coding patterns
- Always use the new pipe (`|>`) operator for chaining commands. Never the old pipe (`%>%`) operator.
- For joins, use data.table's `X[Y, on = ...]` syntax instead of `merge()`. For a left join on `Y`, write `X[Y, on = .(key1, key2)]` rather than `merge(Y, X, by = c("key1", "key2"), all.x = TRUE)`.
