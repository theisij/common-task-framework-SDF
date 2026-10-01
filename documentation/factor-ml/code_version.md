The code is public at <https://github.com/theisij/common-task-framework-SDF>. The submitted weights were produced by the version tagged `ctf-submission-2026-10-01-v2` (commit `c29881e`, 2026-10-01): <https://github.com/theisij/common-task-framework-SDF/tree/ctf-submission-2026-10-01-v2>.

- **Model script** (the submitted file): `models_R/factor-ml/factor_ml_standalone.R`
- **Dependencies:** `models_R/factor-ml/renv.lock`, for the CTF runtime (R 4.4.2)
- **Commit:** `c29881e5a85f28c844d1eb4359e02c31533b4833`
- **SHA-256 of the model script:**

```
fc3f50c293b0ae740dd227b4fbad762618c1daf27533414462dc555a1274e60f
```

To reproduce, check out the tag, place the CTF data in `data/raw/`, run `Rscript scripts/build_model.R models_R/factor-ml/factor_ml.R`, and call `main()` in the resulting standalone script (or submit `models_R/factor-ml/factor_ml.slurm` on a SLURM cluster).

The weights are deterministic and do not depend on how the data is passed to the model. Random seeds are fixed, and `main()` first puts its inputs in a canonical order: the stock characteristics are sorted by stock identifier and month, the list of features alphabetically, and the columns by name. This matters because some estimation steps depend on the order of the data, for example XGBoost's random sampling of rows and columns.
