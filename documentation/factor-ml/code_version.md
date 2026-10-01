The code is public at <https://github.com/theisij/common-task-framework-SDF>. The submitted weights were produced by the version tagged `ctf-submission-2026-09-30` (commit `a51f013`, 2026-09-30): <https://github.com/theisij/common-task-framework-SDF/tree/ctf-submission-2026-09-30>.

- **Model script** (the submitted file): `models_R/factor-ml/factor_ml_standalone.R`
- **Dependencies:** `models_R/factor-ml/renv.lock`, for the CTF runtime (R 4.4.2)
- **Commit:** `a51f01384378cd00c74aaba45ee171ed04c6d59d`
- **SHA-256 of the model script:**

```
f810c60c4aec417f5fac946efa774c44748de5136a2144558c61cbd554da1fd4
```

To reproduce, check out the tag, place the CTF data in `data/raw/`, run `Rscript scripts/build_model.R models_R/factor-ml/factor_ml.R`, and call `main()` in the resulting standalone script (or submit `models_R/factor-ml/factor_ml.slurm` on a SLURM cluster).
