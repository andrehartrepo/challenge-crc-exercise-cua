# Cost-Utility Analysis of a Structured Exercise Programme in Colon Cancer

This repository holds the R model behind a master's thesis in health economics: a decision-analytic cost-utility analysis comparing a structured exercise programme with standard care for colon cancer survivors after adjuvant chemotherapy, in the Norwegian setting.

## What this repository is

The complete analysis code, the locked analytical inputs, and the generated tables the thesis reports. All of it is desk research on published sources: trial publications, national registry statistics, public life tables, and published unit costs and tariffs. It contains no individual patient data.

Parameter values are annotated inline with `# SOURCE:` comments naming their origin, and modelling choices with `# GUIDELINE:` comments naming the guideline followed. The code is meant to be read as much as run.

## Model at a glance

- Structure: 3-state partitioned survival model (disease-free, progressed or recurred, dead)
- Population: stage III or high-risk stage II colon cancer survivors, cohort entry age 61
- Comparison: a structured exercise programme versus standard care
- Effectiveness source: the CHALLENGE trial (Courneya et al. 2025), hazard ratio 0.72 for disease-free survival (95% CI 0.55 to 0.94) and 0.63 for overall survival (95% CI 0.43 to 0.94)
- Perspective: extended healthcare perspective as the base case (Meld. St. 21, 2024-2025, Section 4.3.8)
- Time horizon: 40 years
- Cycle length: one month (1/12 year), 480 cycles
- Discounting: the stepped 4/3/2 percent schedule of Rundskriv R-109 as the base case, with a flat 4 percent schedule as sensitivity analysis
- Currency: NOK
- Probabilistic analysis: 10,000 draws in the base-case PSA, and 500 draws per scenario in the probabilistic one-way analysis across 8 parameters at a low and a high bound

## Repository layout

- `main.R` is the entry point and runs the whole analysis in order
- `R/` holds the twelve numbered analysis scripts, `00-parameters.R` through `10-extreme-value-tests.R`
- `R/functions/` holds two shared helpers: the canonical object hasher and the lock writer
- `data/raw/` holds the shipped life tables, the EQ-5D-5L norms, and the model version history
- `data/processed/` is where the run writes its reconstructed patient-level data and intermediate objects; it ships empty
- `km-digitisation/` holds the four digitised Kaplan-Meier coordinate files
- `reproducibility/` holds the input lock, the runtime lock, the package lock, the preflight checks, and the output registry
- `tests/` holds the test suite
- `output/` also holds three files at its root: `model-results.tex`, `model-results-manifest.csv` and `model-results-provenance.csv`
- `output/tables/` holds the 30 generated LaTeX tables, `manifest.json`, the styled appendix workbook `appendix-styled-tables.xlsx`, and a model selection analysis note `model-selection-analysis.md`
- `output/validation/` holds the extreme-value harness results
- `output/figures/` is where the run writes figures; it ships empty
- `reports/` is an empty placeholder directory
- `renv.lock` and `PACKAGES.md` describe the locked package environment

## Requirements

R 4.5.1. The package environment is locked: `renv.lock` carries 120 exact CRAN records for the full recursive closure, `reproducibility/r-package-lock.csv` records the official source URL and SHA-256 of every one of those archives, and `PACKAGES.md` lists 22 direct runtime packages with their versions.

One direct dependency is not in the lock. `ggsurvfit` is used by the survival figures in `R/07-visualization.R` but has no record in `renv.lock` or `reproducibility/r-package-lock.csv`, so `renv::restore()` will not install it. Install it separately; the author's library has version 1.2.0.

```r
install.packages("ggsurvfit")
```

Expect a long run. The base-case PSA draws 10,000 samples and the probabilistic one-way analysis runs 500 draws for each of 16 parameter-bound scenarios, so a full `main.R` takes considerably longer than a deterministic pass.

`R/07-visualization.R` loads the Charter typeface from a hard-coded macOS system path, `/System/Library/Fonts/Supplemental/Charter.ttc`, when the `showtext` package is available.

## How to run

Set the working directory to this directory. Everything below is relative to it.

```sh
Rscript reproducibility/check-inputs.R
Rscript reproducibility/check-runtime.R
Rscript main.R
Rscript reproducibility/verify-required-outputs.R
```

The first two are fail-closed preflight checks and `main.R` runs them again itself before doing any work. Their expected output is `INPUT PREFLIGHT PASS: 8 exact analytical inputs` and `R RUNTIME PREFLIGHT PASS: 120 locked packages`. The final check reports `REQUIRED OUTPUT VERIFICATION PASS: 130 generated paths`.

For a clean package library, `reproducibility/bootstrap-renv.R` verifies every source archive against `reproducibility/r-package-lock.csv` before installing anything, then performs a strict `renv::restore()`.

```sh
Rscript reproducibility/bootstrap-renv.R verify ARCHIVE_CACHE
Rscript reproducibility/bootstrap-renv.R restore ARCHIVE_CACHE
```

## Reproducibility and its scope

Two things are locked, and they lock different amounts.

The inputs are locked absolutely. `reproducibility/check-inputs.R` reads `reproducibility/input-lock.csv` and refuses to proceed unless all 8 analytical input files are present with their exact recorded SHA-256. This works on any machine.

The runtime is locked to one machine. `reproducibility/check-runtime.R` compares the running R against `reproducibility/r-runtime-spec.csv`, which pins R version 4.5.1 (2025-06-13) on platform aarch64-apple-darwin20, the random number generator kinds, and the file paths and SHA-256 of the BLAS and LAPACK libraries of that specific R installation. Any other machine fails this check and `main.R` stops.

That is deliberate, and it sets the honest boundary of what this package offers. The code is fully readable and auditable anywhere, but `main.R` refuses to run on any other machine: it runs `check-runtime.R` as a fail-closed preflight before doing any work.

## Tests

Eight test files sit in `tests/testthat/`.

`reproducibility/run-gate-tests.R` runs four of them: cost component ownership, endpoint dependence, utility uncertainty, and the structural cure point. These four source the model themselves, so they need no helper. The script also verifies the package archive closure first and includes a tamper fixture that proves a one-byte change to a source archive is detected before installation, which means it needs the archive cache that `reproducibility/bootstrap-renv.R` populates.

The other four (CEAC and CEAF, core functions, discount boundaries, patient time) rely on `tests/testthat/helper-source-model.R` and run through `tests/testthat.R`.

Seven individual tests are skipped on purpose, each with a `QUARANTINE` marker naming its reason: six in `test_core_functions.R` target a retired cost-function interface the model no longer implements, and one in `test_endpoint_dependence.R` covers a nested-fixture hash pin that diverged in the current R environment while the production frame pins stayed intact.

## Outputs

`main.R` writes figures to `output/figures/`, tables to `output/tables/`, text snippets to `output/text-snippets/`, and its intermediate objects to `data/processed/`.

Some generated files ship with the repository rather than being left to the run: the 30 thesis tables in `output/tables/`, `manifest.json` (the registry of the run's 14 DOCX table renders, which do not ship), and the LaTeX command set in `output/model-results.tex` with its manifest and provenance files. `reproducibility/required-outputs.txt` is the registry of all 130 generated paths the run must produce, and `reproducibility/verify-required-outputs.R` checks them. The `git_commit` field in `manifest.json` records the state of the private development repository at generation time; that identifier is not resolvable here.

## Data provenance

Eight files are the exact analytical inputs, each SHA-256 pinned in `reproducibility/input-lock.csv`.

- `km-digitisation/dfs-control-wpd.csv`, `dfs-exercise-wpd.csv`, `os-control-wpd.csv`, `os-exercise-wpd.csv` are coordinates read off Figure 2 of the CHALLENGE trial publication (Courneya et al., NEJM 2025;393:13-25), digitised manually with WebPlotDigitizer and combined with the published numbers at risk. `R/01-digitize-km.R` reconstructs patient-level data from them using the Guyot et al. (2012) algorithm via the `IPDfromKM` package. The published figures are not part of this repository; only the digitised coordinates are.
- `data/raw/norway_life_table.csv` and `norway_life_table_sex_specific.csv` hold general-population mortality
- `data/raw/dmp-eq5d5l-norms-current.csv` holds the EQ-5D-5L population norms used for the severity calculation
- `data/raw/version-history.csv` records the model version history

## Extreme-value harness

`R/10-extreme-value-tests.R` is a standalone harness. `main.R` never calls it. It runs the model at extreme parameter values in child R processes and writes `output/validation/extreme-value-results.csv`.

```sh
Rscript R/10-extreme-value-tests.R
```

That CSV ships with the repository because `R/09-export-latex-commands.R` reads it as an input and stops if it is missing or empty. It also asserts that the harness result still matches the live deterministic base case, so a model change without a harness rerun fails loudly rather than publishing stale numbers.

## Known limitations of this code package

- The runtime lock ties bit-exact reproduction to one machine, as described above.
- `ggsurvfit` is a direct dependency outside the lock and must be installed separately.
- The Charter font path in `R/07-visualization.R` is macOS-specific.
- Seven tests are quarantined and skipped.
- The scripts are a sourced analysis pipeline, not an R package: there is no `DESCRIPTION`, no namespace, and objects are shared through the global environment.

## Author and citation

- André Álcega Hartmann
- Eu-HEM, European Master in Health Economics and Management
- Programme cohort 2024-2026

Please cite the thesis when using this model or its code.

## License

The code is released under the [MIT License](LICENSE). Documentation, text, and generated tables are released under [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

You are free to share and adapt this material with appropriate attribution.
