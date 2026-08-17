# Locked package environment

`renv.lock` is the dependency-selection authority for R 4.5.1. The complete
recursive closure contains 120 exact CRAN records. The companion audit view,
`reproducibility/r-package-lock.csv`, records the immutable official source URL
and SHA-256 of the exact staged source archive for every record.

## Direct runtime packages

| Package | Version |
|---|---:|
| renv | 1.2.0 |
| flexsurv | 2.3.2 |
| IPDfromKM | 0.1.10 |
| dampack | 1.0.2.1000 |
| MASS | 7.3-65 |
| ggplot2 | 4.0.2 |
| scales | 1.4.0 |
| dplyr | 1.2.1 |
| tidyr | 1.3.2 |
| survival | 3.8-3 |
| flextable | 0.9.11 |
| officer | 0.7.3 |
| jsonlite | 2.0.0 |
| cowplot | 1.2.0 |
| kableExtra | 1.4.0 |
| mgcv | 1.9-3 |
| showtext | 0.9-8 |
| sysfonts | 0.8.9 |
| stringr | 1.6.0 |
| testthat | 3.3.2 |
| png | 0.1-9 |
| zip | 2.3.3 |

`png` provides deterministic pixel-preserving re-encoding of the expected-loss
PNG. `zip` provides normalized DOCX containers with fixed member timestamps.

Deferred or considered packages (`hesim`, `assertHE`, `here`, `survminer`, and
`survextrap`) are not active dependencies and are absent from the direct set.

Use `Rscript reproducibility/bootstrap-renv.R verify ARCHIVE_CACHE` to verify
the complete frozen archive closure. Clean restoration is permitted only after
that pre-install verification succeeds.
