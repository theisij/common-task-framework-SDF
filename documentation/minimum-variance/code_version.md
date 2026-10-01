The code is public at <https://github.com/theisij/common-task-framework-SDF>. The submitted weights were produced by the version tagged `ctf-submission-2026-10-01` (commit `f04c4e4`, 2026-10-01): <https://github.com/theisij/common-task-framework-SDF/tree/ctf-submission-2026-10-01>.

- **Model script** (the submitted file): `models_R/minimum-variance/minimum_variance_standalone.R`
- **Dependencies:** `models_R/minimum-variance/renv.lock`, for the CTF runtime (R 4.4.2)
- **Commit:** `f04c4e4662bb7da053099bea783e1d419c86c76d`
- **SHA-256 of the model script:**

```
f9edcf0c723848c1c5772037adca4ae3fbdb7a9c28397a8b3b18e1a3bce215eb
```

To reproduce, check out the tag, place the CTF data in `data/raw/`, run `Rscript scripts/build_model.R models_R/minimum-variance/minimum_variance.R`, and call `main()` in the resulting standalone script (or submit `models_R/minimum-variance/minimum_variance.slurm` on a SLURM cluster).
