#!/usr/bin/env Rscript

archive_cache <- Sys.getenv(
  "G2_PACKAGE_ARCHIVE_CACHE", path.expand("~/Library/Caches/g2-package-archives"))

run_checked <- function(command, args, label) {
  status <- system2(command, args)
  if (!identical(status, 0L)) stop(label, " failed")
}

run_checked(file.path(R.home("bin"), "Rscript"),
            "R/functions/canonical-hash-v1.R", "canonical hash self-check")
run_checked(file.path(R.home("bin"), "Rscript"),
            "R/functions/lock-writer-v1.R", "lock writer self-check")
run_checked(file.path(R.home("bin"), "Rscript"),
            c("reproducibility/bootstrap-renv.R", "verify", archive_cache),
            "package archive closure preflight")

source("R/functions/lock-writer-v1.R")
package_lock <- read.csv(file.path("reproducibility", "r-package-lock.csv"),
                         stringsAsFactors = FALSE, check.names = FALSE,
                         colClasses = "character")
probe <- package_lock[package_lock$package == "renv", ]
if (nrow(probe) != 1L) stop("tamper fixture requires one renv archive row")
archive <- file.path(archive_cache,
                     paste0(probe$package, "_", probe$version, ".tar.gz"))
tampered <- tempfile("one-byte-tamper-", fileext = ".tar.gz")
on.exit(unlink(tampered), add = TRUE)
if (!file.copy(archive, tampered, overwrite = TRUE)) stop("tamper copy failed")
local({
  con <- file(tampered, open = "r+b")
  on.exit(close(con))
  byte <- readBin(con, what = "raw", n = 1L)
  seek(con, where = 0L, origin = "start")
  writeBin(as.raw(bitwXor(as.integer(byte), 1L)), con)
})
if (identical(lock_writer_sha256_file_v1(tampered), probe$archive_sha256)) {
  stop("one-byte archive tamper was not detected")
}
cat("ARCHIVE TAMPER FIXTURE PASS: mismatch detected before installation\n")

files <- c(
  "tests/testthat/test_cost_component_ownership.R",
  "tests/testthat/test_endpoint_dependence.R",
  "tests/testthat/test_utility_uncertainty.R",
  "tests/testthat/test_structural_cure_point.R")
for (path in files) {
  testthat::test_file(path, load_helpers = FALSE, stop_on_failure = TRUE)
}
cat("GATE TEST RUNNER PASS: exact analytical test manifest\n")
