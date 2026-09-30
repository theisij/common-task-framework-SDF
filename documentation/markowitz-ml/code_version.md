The code is public at <https://github.com/theisij/common-task-framework-SDF>. The submitted weights were produced by the version tagged `ctf-submission-2026-09-30` (commit `a51f013`, 2026-09-30): <https://github.com/theisij/common-task-framework-SDF/tree/ctf-submission-2026-09-30>.

- **Model script** (the submitted file): `models_R/markowitz-ml/markowitz_ml_standalone.R`
- **Dependencies:** `models_R/markowitz-ml/renv.lock`, for the CTF runtime (R 4.4.2)
- **Commit:** `a51f01384378cd00c74aaba45ee171ed04c6d59d`
- **SHA-256 of the model script:**

```
a2c3a78483383c309798b8648b3ca7affe2b8675233556014224689d86365e76
```

To reproduce, check out the tag, place the CTF data in `data/raw/`, run `Rscript scripts/build_model.R models_R/markowitz-ml/markowitz_ml.R`, and call `main()` in the resulting standalone script (or submit `models_R/markowitz-ml/markowitz_ml.slurm` on a SLURM cluster).
