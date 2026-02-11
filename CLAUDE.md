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

R models (e.g., `models_R/KNS/kns_pc.R`) are run directly in R.

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

Every Python model must expose a `main()` function with this signature:

```python
def main(chars: pd.DataFrame, features: pd.DataFrame, daily_ret: pd.DataFrame) -> pd.DataFrame:
```

The returned DataFrame must have columns: **id**, **eom** (end-of-month date), **w** (portfolio weights). This is the format required for submission to the competition website.

### Key Data Columns

- `id` — stock identifier
- `eom` — end-of-month date (primary grouping key)
- `ctff_test` — test set indicator (filter to `== 1` for submission)
- `ret_exc_lead1m` — one-month-ahead excess return
- `me` — market equity

### Adding a New Model

1. Save the script in a folder under `models_python/` or `models_R/`
2. Save CSV output under `data/processed/{model_name}/`
3. Save documentation under `documentation/{model_name}/`

### Key Libraries

- **Polars** is the primary DataFrame library for data processing (preferred over pandas for new code)
- **Pandas** is used at the model interface boundary (input/output of `main()`)
- `utils/data_prep.py` provides `impute_and_rank()` for percentile ranking and missing value imputation
- `private/settings.py` uses Pydantic `BaseSettings` for configuration (env var prefixes: `COV_`, `APP_`)

### Directories Not in Git

`data/` (raw, interim, processed, wrds), `output/` (figures, tables), and `private/` are gitignored.
