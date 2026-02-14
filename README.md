## Overview

Repository for the Common Task Framework (CTF) competition associated with the paper ["The Power of the Common Task Framework"](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5242901) by Hellum, Jensen, Kelly, and Pedersen (2025), where the goal is to find the stochastic discount factor, or more precisely, the portfolio with the highest Sharpe ratio. The goal of this repo is to contain all benchmark models from the CTF paper. The repo is still work-in-progress.

- **Submit a model:** [jkpfactors.com/ctf/submit](https://jkpfactors.com/ctf/submit)
- **Competition rules:** [jkpfactors.com/ctf/rules](https://jkpfactors.com/ctf/rules)
- **Dataset access:** [jkpfactors.com/ctf/dataset-access](https://jkpfactors.com/ctf/dataset-access)
- **Leaderboard:** [jkpfactors.com/ctf/leaderboard](https://jkpfactors.com/ctf/leaderboard)

## Model structure

Each model lives in its own folder under `models_python/` (Python) or `models_R/` (R). A model folder should contain:

1. **Main script** (required) — Named after the model (e.g., `minimum_variance.R`, `factor_ml.R`). This is the script submitted to the competition website and must follow the competition's required structure.
2. **Testing script** (optional) — For local validation before submission (e.g., `minimum_variance_testing.R`).
3. **Slurm script** (optional) — For running the model on a cluster (e.g., `minimum_variance.slurm`).

Example structure:

```
models_R/
├── minimum-variance/
│   ├── minimum_variance.R              # Main model script
│   ├── minimum_variance_testing.R      # Local testing
│   └── minimum_variance.slurm          # Cluster submission
├── factor-ml/
│   ├── factor_ml.R
│   ├── factor_ml_testing.R
│   └── factor_ml.slurm
```

Additionally:
- Save the CSV output in a separate folder under `data/processed/` with a descriptive name (e.g., `data/processed/minimum_variance.csv`).
- Save documentation in a separate folder under `documentation/` (e.g., `documentation/minimum_variance/minimum_variance.pdf`).
