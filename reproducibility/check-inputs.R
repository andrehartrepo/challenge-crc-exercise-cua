#!/usr/bin/env Rscript

source("R/functions/lock-writer-v1.R")
lock_path <- file.path("reproducibility", "input-lock.csv")
if (!file.exists(lock_path)) stop("input-lock.csv is missing")
lock <- read.csv(lock_path, stringsAsFactors = FALSE, check.names = FALSE,
                 colClasses = "character")
expected_columns <- c("path", "sha256", "role")
if (!identical(names(lock), expected_columns) || nrow(lock) != 8L ||
    anyNA(lock) || any(lock == "") || anyDuplicated(lock$path) ||
    anyDuplicated(lock$role)) {
  stop("input-lock.csv schema/cardinality/identity failure")
}
if (any(!grepl("^[0-9a-f]{64}$", lock$sha256, perl = TRUE))) {
  stop("input-lock.csv contains an invalid SHA-256")
}
for (i in seq_len(nrow(lock))) {
  path <- lock$path[[i]]
  if (!file.exists(path) || dir.exists(path)) stop("required input missing: ", path)
  actual <- lock_writer_sha256_file_v1(path)
  if (!identical(actual, lock$sha256[[i]])) {
    stop("required input SHA-256 mismatch: ", path)
  }
}
cat("INPUT PREFLIGHT PASS: 8 exact analytical inputs\n")
