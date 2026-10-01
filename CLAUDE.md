# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **Common Task Framework (CTF)** competition repository for the paper "The Power of the Common Task Framework" by Hellum, Jensen, Kelly, and Pedersen (2025). The goal is to find the portfolio with the highest Sharpe ratio. Models are submitted to [https://jkpfactors.com/ctf/submit](https://jkpfactors.com/ctf/submit).

## CTF Competition Rules (must follow)

The full rules are in [`docs/ctf_rules.md`](docs/ctf_rules.md) (a copy of https://jkpfactors.com/ctf/rules.md, updated 2026-09-29; re-download it if the site's `updated_at` changes). Every model in this repo is a potential submission, so all model code must satisfy them. In practice:

**Output (Rules 5, 11, 12).**
- `main(chars, features, daily_ret)` returns exactly `id` (integer), `eom` (Date), `w` (double), with no missing values, no duplicated `(id, eom)`, and a row for every `ctff_test` observation. The output must be under 50 MB.
- Return the weights only from `main()`: no file input/output inside `main()`.
- In R, end `main()` with `finalize_output()` (`utils/R/output_utils.R`), which enforces this and logs a summary.

**No lookahead (Rule 1).**
- Weights at `t` may use only data available at `t`. The CTF reruns each model on data with later months removed and flags any change in earlier weights.
- Don't let full-sample quantities, such as sample-wide ranks, normalizations or test-period statistics, leak into earlier months. (Including a factor that only has exposures in later months is harmless: the toy lookahead test covers this, with weights equal to machine precision.)
- `ctff_test` defines the test set; never hard-code dates.

**Determinism (Rule 18).**
- Call `set.seed()` at the start of `main()`. Results must be identical across runs and across thread counts (tolerance: relative 1e-5, absolute 1e-8).
- Results must not depend on how the data is fed in: the order of rows, of the feature list, or of columns. XGBoost samples rows and columns by position, and glmnet's coordinate descent visits columns in order (feature order moved Minimum Variance weights by ~1e-4). Every R model starts `main()` by passing each input it uses through the matching helper in `utils/R/data_prep.R`: `canonical_chars()`, `canonical_features()`, and `canonical_daily_ret()` if it uses daily returns (Factor-ML doesn't). They sort rows by `id` and date, columns by name, and the feature list alphabetically (radix sort, the same in every locale). The toy tests rerun each model on fully shuffled input.

**Security (Rules 10, 15).**
- No network access, `system()`/`system2()`/`shell()`, `eval()`/`parse()`, `source()`, or `Sys.setenv()`.
- No string-built formulas (`as.formula(paste(...))`) either: the scanner treats them as dynamic code. Build design matrices directly, e.g. `as.matrix(dt[, x_vars, with = FALSE])`.

**Dependencies (Rules 4, 8, 16).**
- The runtime is R 4.4.2 with `arrow`, `data.table`, `dplyr` and `tidyr` pre-installed. Every other package must be a CRAN package pinned in the model's own `models_R/<model>/renv.lock`, and must work on R 4.4.2.
- **Never load `library(tidyverse)`.** Attach only the packages used (e.g. `dplyr`, `tidyr`, `purrr`, `lubridate`, `tibble`). The meta-package pulls in packages that need system libraries missing from the CTF container (`ragg`) and network packages (`httr`, `curl`, `rvest`, …) that fail the security scan.

**Small validation run (Rule 14).**
- Before the full run, the CTF runs each model on a ~4 MB, 123-month subset with few stocks.
- Code must not assume every industry, stock or date is present, or that long histories exist. For example, `x_vars_fun(..., present = names(factor_chars))` drops industries with no stocks in the sample.

**Limits (Rules 9, 13).**
- 32 CPU cores (cap thread counts at 32), 300 GB RAM, 24 hours.
- Source files must be under 1 MB and UTF-8.

**What went wrong before (Sept 2026).** The CTF admins could not run our submissions because of `library(tidyverse)`, a string-built regression formula, and a crash on the validation data when an industry had no stocks. The checks below now catch all three.

**What went wrong before (Oct 2026).** The CTF pipeline got lower Sharpe ratios than our runs for the two XGBoost models (Factor-ML 0.74 vs 0.82, Markowitz-ML 2.31 vs 2.49) with identical environments; the models depended on the order of the input rows and features. All models now put their inputs in a canonical order, and the toy tests check fully shuffled input.

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

R models `source()` shared utilities from `utils/R/` and are run directly in R. For HPC runs and submission, `scripts/build_model.R` inlines all `source()` calls into a single `*_standalone.R` file and writes the model's own `renv.lock`. The SLURM scripts run the build automatically:

```bash
# Build standalone (inlines source() calls) and the model's renv.lock, then run
Rscript scripts/build_model.R models_R/markowitz-ml/markowitz_ml.R
Rscript -e 'source("models_R/markowitz-ml/markowitz_ml_standalone.R"); ...'

# Check the submission files against the CTF rules
Rscript scripts/check_submission.R models_R/markowitz-ml
```

The files to submit for a model are `models_R/<model>/<model>_standalone.R`, `data/processed/<model>.csv`, `models_R/<model>/renv.lock`, and `documentation/<model>/<model>.pdf`.

## Pull Request Workflow

Every change goes to `main` through a pull request, following the `/pr-review-cycle` skill (`.claude/skills/pr-review-cycle/SKILL.md`):
1. Open the PR and **wait for Copilot's review** before merging: `scripts/copilot_review.sh <pr>`. Copilot reviews once, 1.5–4 minutes after the PR opens, and doesn't re-review later pushes.
2. Verify each comment against the code or data. Fix valid ones on the same branch and rerun the relevant tests.
3. Reply under every comment ("Fixed in <sha>: …" or "Not changed: <reason, evidence>").
4. Squash-merge through the REST API (`gh pr merge` can fail in gh 2.45), then sync local `main` and, if code changed, the HPC.

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
main <- function(chars, features, daily_ret) { ... }  # returns finalize_output(weights, start_time)
```

See "CTF Competition Rules" above for the full output contract.

### Key Data Columns

- `id` — stock identifier
- `eom` — end-of-month date (primary grouping key)
- `ctff_test` — test set indicator (filter to `== 1` for submission; authoritative for the test period)
- `ret_exc_lead1m` — one-month-ahead excess return
- `me` — market equity

### Adding a New Model

1. Save the script in a folder under `models_python/` or `models_R/`, following "CTF Competition Rules" above: explicit packages (no tidyverse), `set.seed()` and `start_time <- Sys.time()` at the start of `main()`, `return(finalize_output(weights, start_time))` at the end
2. For R models, `source()` shared utilities from `utils/R/` as needed (e.g., `data_prep.R`, `factor_model_utils.R`, `xgb_utils.R`, `output_utils.R`)
3. Add a `*_testing.R` file that sources `utils/R/local_testing.R` and calls `run_toy_tests()` on the `*_standalone.R` file (and `validate_portfolio()` on the full output)
4. Add a SLURM script that calls `scripts/build_model.R` to generate the standalone file and `renv.lock` before running the model
5. Save CSV output as `data/processed/{model_name}.csv`
6. Save documentation under `documentation/{model_name}/`, including a Performance section that includes `performance_stats.md` and `cumulative_returns.pdf` (add the model to `scripts/performance_stats.R` and run it to generate them)

**Pre-submission checklist** (all must pass before anything is sent to the CTF):
1. `Rscript scripts/build_model.R models_R/<model>/<model>.R`: standalone file plus `renv.lock`; the build stops if forbidden packages are loaded
2. `source("utils/toy_data.R")`, then `Rscript models_R/<model>/<model>_testing.R`. The toy data mimics the 123-month validation run (few stocks, one industry missing, one industry appearing only in the last test month). The tests check the output contract, determinism (two runs), invariance to shuffled input (rows, feature list, columns), and lookahead (a run with the last test month removed must leave earlier weights unchanged)
3. Full run on the HPC (SLURM), then `validate_portfolio()` and the documentation's performance statistics
4. `Rscript scripts/check_submission.R models_R/<model>`: the rule checks (size, UTF-8, `main` signature, prohibited code, lock file coverage, R 4.4.2 compatibility of the locked versions, output schema and coverage; a missing weights CSV fails unless `--static` is given)
5. After merging to `main`: tag the commit (`git tag -a ctf-submission-YYYY-MM-DD`, then `git push origin <tag>`), run `Rscript scripts/code_version.R <tag>` to write each model's `code_version.md` (repo, tag, commit, SHA-256 of the submitted script), and re-render the documentation, which includes it in its "Code and Reproducibility" section

### Key Libraries

- **Polars** is the primary DataFrame library for data processing (preferred over pandas for new code)
- **Pandas** is used at the model interface boundary (input/output of `main()`)
- `utils/data_prep.py` provides `impute_and_rank()` for percentile ranking and missing value imputation
- `private/settings.py` uses Pydantic `BaseSettings` for configuration (env var prefixes: `COV_`, `APP_`)
- **data.table** is the primary R DataFrame library for data manipulation
- **xgboost** is used for gradient-boosted tree models in R
- **arrow** is used for reading parquet files in R
- `utils/R/data_prep.R` provides `canonical_chars()`, `canonical_daily_ret()`, `canonical_features()` (fixed input order) and `prepare_pred_data()` for R models
- `utils/R/factor_model_utils.R` provides Barra factor model helpers (regressions, covariance estimation)
- `utils/R/xgb_utils.R` provides XGBoost hyperparameter tuning and training helpers
- `utils/R/local_testing.R` provides `run_toy_tests()` (output contract, determinism and lookahead tests on validation-like toy data) and `validate_portfolio()` for model validation
- `scripts/build_model.R` inlines `source()` calls to produce standalone R files for HPC submission, and writes a per-model `renv.lock` (only that model's packages)
- `scripts/check_submission.R` checks a model's submission files against the CTF rules; `scripts/submission_utils.R` holds the shared package lists (pre-installed, forbidden)
- `scripts/copilot_review.sh <pr>` waits for Copilot's review of a PR and prints its verdict and inline comments with ids
- `scripts/code_version.R` writes each model's documentation section recording the public repo, tag, commit and script checksum of a submission
- `utils/R/output_utils.R` provides `finalize_output()`, which enforces the output contract at the end of `main()`
- `utils/R/performance_stats.R` provides `perf_stats()` (mean, SD, Sharpe ratio, gross leverage, turnover, maximum drawdown); `scripts/performance_stats.R` writes each model's documentation table and cumulative-return figure

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
