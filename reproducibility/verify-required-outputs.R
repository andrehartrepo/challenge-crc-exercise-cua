#!/usr/bin/env Rscript

source("R/functions/lock-writer-v1.R")
required_path <- file.path("reproducibility", "required-outputs.txt")
classification_path <- file.path(
  "reproducibility", "output-path-classification.csv")
if (!file.exists(required_path) || !file.exists(classification_path)) {
  stop("required-output registry artifacts are missing")
}
required <- readLines(required_path, warn = FALSE, encoding = "UTF-8")
if (length(required) != 130L || anyDuplicated(required) ||
    sum(startsWith(required, "output/")) != 110L ||
    sum(startsWith(required, "data/processed/")) != 20L) {
  stop("required-outputs.txt identity/cardinality failure")
}
# Caution: the schema, the class vocabulary and the writer-disjointness rule have
#   one executable home in R/functions/lock-writer-v1.R, not a copy here, so a
#   frozen writer-owned row is refused at LOAD rather than mid-gate after the
#   model has already run.
classification <- lock_writer_read_classification_v1(classification_path)
canonical <- classification$path[
  classification$class == "canonical_generated"]
if (!identical(sort(canonical, method = "radix"),
               sort(required, method = "radix"))) {
  stop("required-output and classification registries differ")
}

missing <- required[!file.exists(required)]
empty <- required[file.exists(required) & file.info(required)$size <= 0]
if (length(missing)) stop("required generated output missing: ", missing[[1L]])
if (length(empty)) stop("required generated output empty: ", empty[[1L]])

actual <- c(
  list.files("data/processed", recursive = TRUE, full.names = TRUE,
             all.files = TRUE, no.. = TRUE),
  list.files("output", recursive = TRUE, full.names = TRUE,
             all.files = TRUE, no.. = TRUE))
actual <- actual[file.info(actual)$isdir %in% FALSE]
unregistered <- setdiff(actual, classification$path)
if (length(unregistered)) {
  stop("new or modified generated path is unregistered: ", unregistered[[1L]])
}

frozen <- classification[
  classification$class %in% lock_writer_classification_frozen_v1(), ]
for (i in seq_len(nrow(frozen))) {
  path <- frozen$path[[i]]
  if (!file.exists(path)) next
  if (!grepl("^[0-9a-f]{64}$", frozen$baseline_sha256[[i]])) {
    stop("frozen path lacks a baseline hash: ", path)
  }
  if (!identical(lock_writer_sha256_file_v1(path),
                 frozen$baseline_sha256[[i]])) {
    stop("frozen excluded/source path changed: ", path)
  }
}

# The recorder file is a path-only compiled-input graph and avoids reading any
# reserved chapter container. When it exists, every recorded model table input
# must be present and registered.
fls <- file.path("..", "latex", "thesis.fls")
if (file.exists(fls)) {
  inputs <- sub("^INPUT ", "", grep("^INPUT ", readLines(fls, warn = FALSE),
                                     value = TRUE))
  table_inputs <- inputs[grepl("model-r/output/tables/thesis-table-[0-9]+\\.tex$",
                               inputs, perl = TRUE)]
  targets <- unique(sub("^.*model-r/", "", table_inputs))
  if (any(!targets %in% required) || any(!file.exists(targets))) {
    stop("compiled model table input is absent or unregistered")
  }
}
cat("REQUIRED OUTPUT VERIFICATION PASS: 130 generated paths\n")
