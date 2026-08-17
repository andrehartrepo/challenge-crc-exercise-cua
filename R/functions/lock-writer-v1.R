# Canonical lock and generated-path writers, version 1.
#
# This file is the single executable definition of the byte encodings for
# r-runtime-spec.csv, r-package-lock.csv, tex-runtime-lock.csv, font-lock.csv,
# required-outputs.txt, and the model/visual source-lock JSON files. It is also
# the single executable definition of the output-path-classification schema, its
# class vocabulary, and the frozen/writer disjointness rule. It uses base R and
# the system shasum executable; source() defines functions only.

lock_writer_schema_v1 <- function(kind) {
  schemas <- list(
    r_runtime = c(
      schema_version = "integer", r_version = "character",
      platform = "character", rng_kind = "character",
      normal_kind = "character", sample_kind = "character",
      blas_path = "character", blas_sha256 = "character",
      lapack_path = "character", lapack_sha256 = "character",
      repository_option = "character"
    ),
    r_package = c(
      schema_version = "integer", package = "character",
      version = "character", source = "character",
      repository = "character", remote_sha = "character",
      archive_sha256 = "character", immutable_source_url = "character"
    ),
    tex_runtime = c(
      schema_version = "integer", record_type = "character",
      name = "character", version = "character",
      local_revision = "character", catalogue_version = "character",
      catalogue_date = "character", source_tex_root = "character"
    ),
    font = c(
      schema_version = "integer", family = "character",
      face = "character", system_collection_path = "character",
      sha256 = "character"
    )
  )
  if (length(kind) != 1L || is.na(kind) || !kind %in% names(schemas))
    stop("unknown lock-writer schema")
  schemas[[kind]]
}

lock_writer_validate_schema_v1 <- function(x, schema, artifact) {
  if (!is.data.frame(x)) stop(artifact, " input must be a data frame")
  if (!identical(names(x), names(schema)))
    stop(artifact, " schema names/order drift")
  for (i in seq_along(schema)) {
    actual <- typeof(x[[i]])
    expected <- unname(schema[[i]])
    if (!identical(actual, expected))
      stop(artifact, " storage type drift for field '", names(schema)[[i]],
           "': expected ", expected, ", got ", actual)
  }
  if (nrow(x) < 1L) stop(artifact, " must contain at least one row")
  if (anyNA(x)) stop(artifact, " may not contain NA values")
  if (any(x$schema_version != 1L))
    stop(artifact, " schema_version must be integer 1")
  invisible(TRUE)
}

lock_writer_utf8_v1 <- function(x, artifact) {
  if (length(x) != 1L || is.na(x)) stop(artifact, " field must be scalar")
  x <- enc2utf8(as.character(x))
  bytes <- as.integer(charToRaw(x))
  if (any(bytes == 0L)) stop(artifact, " field contains NUL")
  if (any(bytes == 13L)) stop(artifact, " field contains CR")
  x
}

lock_writer_csv_field_v1 <- function(x, artifact) {
  x <- lock_writer_utf8_v1(x, artifact)
  if (grepl('[,"\n]', x, perl = TRUE))
    paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
  else x
}

lock_writer_scalar_text_v1 <- function(x, type, artifact) {
  if (length(x) != 1L || is.na(x)) stop(artifact, " field must be scalar")
  if (type == "character") return(lock_writer_utf8_v1(x, artifact))
  if (type == "integer") return(as.character(x))
  if (type == "logical") return(if (x) "TRUE" else "FALSE")
  if (type == "double") return(sprintf("%.17g", x))
  stop(artifact, " unsupported field type")
}

lock_writer_csv_bytes_v1 <- function(x, schema, artifact) {
  lock_writer_validate_schema_v1(x, schema, artifact)
  header <- paste(vapply(names(schema), lock_writer_csv_field_v1, "",
                         artifact = artifact), collapse = ",")
  rows <- vapply(seq_len(nrow(x)), function(i) {
    fields <- vapply(seq_along(schema), function(j) {
      value <- lock_writer_scalar_text_v1(x[[j]][[i]], schema[[j]], artifact)
      lock_writer_csv_field_v1(value, artifact)
    }, "")
    paste(fields, collapse = ",")
  }, "")
  paste0(paste(c(header, rows), collapse = "\n"), "\n")
}

lock_writer_sort_key_v1 <- function(x) {
  vapply(enc2utf8(x), function(value) {
    paste(sprintf("%02X", as.integer(charToRaw(value))), collapse = "")
  }, "")
}

lock_writer_assert_nonempty_v1 <- function(x, fields, artifact) {
  for (field in fields) {
    if (any(nchar(x[[field]], type = "bytes") == 0L))
      stop(artifact, " empty value in field '", field, "'")
  }
  invisible(TRUE)
}

lock_writer_assert_sha256_v1 <- function(x, fields, artifact) {
  for (field in fields) {
    if (any(!grepl("^[0-9a-f]{64}$", x[[field]], perl = TRUE)))
      stop(artifact, " invalid SHA-256 in field '", field, "'")
  }
  invisible(TRUE)
}

lock_writer_emit_v1 <- function(bytes, path) {
  if (length(path) != 1L || is.na(path) || path == "")
    stop("output path must be one non-empty string")
  if (!dir.exists(dirname(path))) stop("output directory does not exist")
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(bytes)), con)
  invisible(path)
}

write_r_runtime_spec_v1 <- function(x, path) {
  artifact <- "r-runtime-spec.csv"
  schema <- lock_writer_schema_v1("r_runtime")
  lock_writer_validate_schema_v1(x, schema, artifact)
  if (nrow(x) != 1L) stop(artifact, " must contain exactly one row")
  lock_writer_assert_nonempty_v1(
    x, setdiff(names(schema), "schema_version"), artifact
  )
  lock_writer_assert_sha256_v1(x, c("blas_sha256", "lapack_sha256"), artifact)
  bytes <- lock_writer_csv_bytes_v1(x, schema, artifact)
  lock_writer_emit_v1(bytes, path)
}

write_r_package_lock_v1 <- function(x, path) {
  artifact <- "r-package-lock.csv"
  schema <- lock_writer_schema_v1("r_package")
  lock_writer_validate_schema_v1(x, schema, artifact)
  lock_writer_assert_nonempty_v1(
    x, c("package", "version", "source", "repository",
         "archive_sha256", "immutable_source_url"), artifact
  )
  lock_writer_assert_sha256_v1(x, "archive_sha256", artifact)
  key <- paste(x$package, x$version, sep = "\034")
  if (anyDuplicated(key)) stop(artifact, " duplicate package/version row")
  ord <- order(lock_writer_sort_key_v1(x$package),
               lock_writer_sort_key_v1(x$version), method = "radix")
  bytes <- lock_writer_csv_bytes_v1(x[ord, , drop = FALSE], schema, artifact)
  lock_writer_emit_v1(bytes, path)
}

write_tex_runtime_lock_v1 <- function(x, path) {
  artifact <- "tex-runtime-lock.csv"
  schema <- lock_writer_schema_v1("tex_runtime")
  lock_writer_validate_schema_v1(x, schema, artifact)
  if (any(!x$record_type %in% c("tool", "package")))
    stop(artifact, " record_type must be tool or package")
  lock_writer_assert_nonempty_v1(x, c("record_type", "name", "version"), artifact)
  package_rows <- x$record_type == "package"
  if (any(package_rows & (x$local_revision == "" | x$source_tex_root == "")))
    stop(artifact, " package rows require local_revision and source_tex_root")
  key <- paste(x$record_type, x$name, sep = "\034")
  if (anyDuplicated(key)) stop(artifact, " duplicate record_type/name row")
  ord <- order(lock_writer_sort_key_v1(x$record_type),
               lock_writer_sort_key_v1(x$name), method = "radix")
  bytes <- lock_writer_csv_bytes_v1(x[ord, , drop = FALSE], schema, artifact)
  lock_writer_emit_v1(bytes, path)
}

write_font_lock_v1 <- function(x, path) {
  artifact <- "font-lock.csv"
  schema <- lock_writer_schema_v1("font")
  lock_writer_validate_schema_v1(x, schema, artifact)
  lock_writer_assert_nonempty_v1(
    x, c("family", "face", "system_collection_path", "sha256"), artifact
  )
  lock_writer_assert_sha256_v1(x, "sha256", artifact)
  if (any(!grepl("^/(System|Library)/", x$system_collection_path, perl = TRUE)))
    stop(artifact, " collection path must be under /System or /Library")
  key <- paste(x$family, x$face, sep = "\034")
  if (anyDuplicated(key)) stop(artifact, " duplicate family/face row")
  ord <- order(lock_writer_sort_key_v1(x$family),
               lock_writer_sort_key_v1(x$face), method = "radix")
  bytes <- lock_writer_csv_bytes_v1(x[ord, , drop = FALSE], schema, artifact)
  lock_writer_emit_v1(bytes, path)
}

write_required_outputs_v1 <- function(paths, path) {
  artifact <- "required-outputs.txt"
  if (typeof(paths) != "character" || is.object(paths))
    stop(artifact, " input must be a character vector")
  if (length(paths) < 1L || anyNA(paths))
    stop(artifact, " requires at least one non-missing path")
  paths <- enc2utf8(paths)
  if (any(paths == "")) stop(artifact, " contains an empty path")
  if (any(grepl("[\r\n]", paths, perl = TRUE)))
    stop(artifact, " path contains CR or LF")
  if (any(grepl("\\\\", paths, perl = TRUE)))
    stop(artifact, " paths must use forward slashes")
  if (any(!grepl("^(data/processed|output)/", paths, perl = TRUE)))
    stop(artifact, " path is outside data/processed or output")
  segments <- strsplit(paths, "/", fixed = TRUE)
  if (any(vapply(segments, function(x) any(x %in% c("", ".", "..")), FALSE)))
    stop(artifact, " path contains an empty, dot, or dot-dot segment")
  if (anyDuplicated(paths)) stop(artifact, " contains a duplicate path")
  ord <- order(lock_writer_sort_key_v1(paths), method = "radix")
  bytes <- paste0(paste(paths[ord], collapse = "\n"), "\n")
  lock_writer_emit_v1(bytes, path)
}

lock_writer_source_lock_schema_v1 <- function() {
  c(
    schema_version = "integer",
    lock_id = "character",
    root_commit = "character",
    nested_commit = "character",
    model_source_lock_sha256 = "character_or_null",
    input_lock_sha256 = "character",
    r_runtime_lock_sha256 = "character",
    r_package_lock_sha256 = "character",
    tex_runtime_lock_sha256 = "character",
    font_lock_sha256 = "character",
    model_results_manifest_sha256 = "character",
    model_results_provenance_sha256 = "character",
    required_output_inventory_sha256 = "character",
    output_hash_evidence_manifest_sha256 = "character",
    thesis_pdf_render_evidence_sha256 = "character",
    gate3_reports_manifest_sha256 = "character",
    clean_room_reports_manifest_sha256 = "character",
    visual_manifest_sha256 = "character_or_null",
    an_015_evidence_sha256 = "character_or_null",
    an_016_evidence_sha256 = "character_or_null",
    final_pdf_sha256 = "character_or_null"
  )
}

lock_writer_validate_source_lock_v1 <- function(x) {
  artifact <- "source-lock.json"
  schema <- lock_writer_source_lock_schema_v1()
  if (!is.list(x) || is.data.frame(x) || is.object(x))
    stop(artifact, " input must be an unclassed named list")
  if (!identical(names(x), names(schema)))
    stop(artifact, " schema names/order drift")
  for (i in seq_along(schema)) {
    value <- x[[i]]
    expected <- unname(schema[[i]])
    if (expected == "character_or_null" && is.null(value)) next
    actual <- typeof(value)
    expected_type <- if (expected == "character_or_null") "character" else expected
    if (length(value) != 1L || is.na(value) || !identical(actual, expected_type))
      stop(artifact, " storage type drift for field '", names(schema)[[i]],
           "': expected ", expected, ", got ", actual)
  }
  if (!identical(x$schema_version, 1L))
    stop(artifact, " schema_version must be integer 1")
  model_id <- "whole-thesis-round1-model-repro-source-lock-2026-07-13.json"
  visual_id <- "whole-thesis-round1-visual-final-source-lock-2026-07-13.json"
  if (!x$lock_id %in% c(model_id, visual_id))
    stop(artifact, " lock_id is not a supported source-lock identity")
  if (!grepl("^[0-9a-f]{40}$", x$root_commit, perl = TRUE) ||
      !grepl("^[0-9a-f]{40}$", x$nested_commit, perl = TRUE))
    stop(artifact, " root_commit and nested_commit must be lowercase 40-hex commits")
  sha_fields <- setdiff(names(schema),
                        c("schema_version", "lock_id", "root_commit", "nested_commit"))
  for (field in sha_fields) {
    value <- x[[field]]
    if (!is.null(value) && !grepl("^[0-9a-f]{64}$", value, perl = TRUE))
      stop(artifact, " invalid SHA-256 in field '", field, "'")
  }
  visual_fields <- c("visual_manifest_sha256", "an_015_evidence_sha256",
                     "an_016_evidence_sha256", "final_pdf_sha256")
  if (identical(x$lock_id, model_id)) {
    if (!is.null(x$model_source_lock_sha256) ||
        any(!vapply(x[visual_fields], is.null, FALSE)))
      stop(artifact, " model lock requires null predecessor and visual-only fields")
  } else {
    required_successor <- c("model_source_lock_sha256", visual_fields)
    if (any(vapply(x[required_successor], is.null, FALSE)))
      stop(artifact, " visual successor requires predecessor and visual evidence hashes")
  }
  invisible(TRUE)
}

lock_writer_json_escape_v1 <- function(x, artifact = "source-lock.json") {
  if (typeof(x) != "character" || length(x) != 1L || is.na(x))
    stop(artifact, " JSON string must be one non-missing character scalar")
  x <- enc2utf8(x)
  if (is.na(iconv(x, from = "UTF-8", to = "UTF-8", sub = NA_character_)))
    stop(artifact, " JSON string is not valid UTF-8")
  bytes <- as.integer(charToRaw(x))
  escaped <- unlist(lapply(bytes, function(byte) {
    if (byte == 34L) return(as.integer(charToRaw('\\"')))
    if (byte == 92L) return(as.integer(charToRaw('\\\\')))
    if (byte == 8L) return(as.integer(charToRaw('\\b')))
    if (byte == 9L) return(as.integer(charToRaw('\\t')))
    if (byte == 10L) return(as.integer(charToRaw('\\n')))
    if (byte == 12L) return(as.integer(charToRaw('\\f')))
    if (byte == 13L) return(as.integer(charToRaw('\\r')))
    if (byte < 32L)
      return(as.integer(charToRaw(sprintf("\\u%04X", byte))))
    byte
  }), use.names = FALSE)
  out <- rawToChar(as.raw(escaped))
  Encoding(out) <- "UTF-8"
  out
}

lock_writer_source_lock_bytes_v1 <- function(x) {
  lock_writer_validate_source_lock_v1(x)
  schema <- lock_writer_source_lock_schema_v1()
  fields <- vapply(seq_along(schema), function(i) {
    key <- names(schema)[[i]]
    value <- x[[i]]
    encoded <- if (is.null(value)) {
      "null"
    } else if (identical(unname(schema[[i]]), "integer")) {
      as.character(value)
    } else {
      paste0('"', lock_writer_json_escape_v1(value), '"')
    }
    paste0('"', key, '":', encoded)
  }, "")
  paste0("{", paste(fields, collapse = ","), "}\n")
}

write_source_lock_v1 <- function(x, path) {
  bytes <- lock_writer_source_lock_bytes_v1(x)
  lock_writer_emit_v1(bytes, path)
}

lock_writer_source_lock_fixture_v1 <- function() {
  h <- paste(rep("a", 64L), collapse = "")
  list(
    schema_version = 1L,
    lock_id = "whole-thesis-round1-model-repro-source-lock-2026-07-13.json",
    root_commit = paste(rep("b", 40L), collapse = ""),
    nested_commit = paste(rep("c", 40L), collapse = ""),
    model_source_lock_sha256 = NULL,
    input_lock_sha256 = h,
    r_runtime_lock_sha256 = h,
    r_package_lock_sha256 = h,
    tex_runtime_lock_sha256 = h,
    font_lock_sha256 = h,
    model_results_manifest_sha256 = h,
    model_results_provenance_sha256 = h,
    required_output_inventory_sha256 = h,
    output_hash_evidence_manifest_sha256 = h,
    thesis_pdf_render_evidence_sha256 = h,
    gate3_reports_manifest_sha256 = h,
    clean_room_reports_manifest_sha256 = h,
    visual_manifest_sha256 = NULL,
    an_015_evidence_sha256 = NULL,
    an_016_evidence_sha256 = NULL,
    final_pdf_sha256 = NULL
  )
}

lock_writer_sha256_file_v1 <- function(path) {
  shasum <- Sys.which("shasum")
  if (identical(unname(shasum), "")) stop("shasum executable not found")
  out <- system2(unname(shasum), c("-a", "256", shQuote(path)),
                 stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) stop("shasum failed")
  hash <- sub("[[:space:]].*$", "", out[[1L]])
  if (!grepl("^[0-9a-f]{64}$", hash, perl = TRUE))
    stop("invalid shasum output")
  hash
}

# The output-path classification registry declares, for every generated path,
# its class and which in-repo script writes it. Its only consumer is
# reproducibility/verify-required-outputs.R.
#
# Caution: a byte-frozen baseline on a path an in-repo process writes is a category
#   error. Every legitimate parameter change breaks the pin INSIDE the gate that
#   wrote it, mid-run, after the model has already executed. Measured 2026-07-31:
#   R/10-extreme-value-tests.R rewrote the frozen row
#   output/validation/extreme-value-results.csv and halted both the test run
#   under tests/ and the thesis build. The disjointness check below refuses
#   that combination at LOAD, before any hashing, so it cannot be discovered
#   mid-gate again.
# Caution: "unknown" is not "none". An undeclared writer cannot enter a frozen
#   class; declaring it is the cure and the default fails closed.
lock_writer_classification_schema_v1 <- function() {
  c("path", "class", "baseline_sha256", "reason", "writer")
}

lock_writer_classification_classes_v1 <- function() {
  c("canonical_generated", "tracked_source_input", "tracked_placeholder",
    "historical_cache_legacy_excluded", "regenerated_validation_output")
}

lock_writer_classification_frozen_v1 <- function() {
  c("tracked_source_input", "historical_cache_legacy_excluded")
}

lock_writer_validate_classification_v1 <- function(x, artifact) {
  if (!is.data.frame(x)) stop(artifact, " input must be a data frame")
  if (!identical(names(x), lock_writer_classification_schema_v1()))
    stop(artifact, " schema names/order drift")
  if (nrow(x) < 1L) stop(artifact, " must contain at least one row")
  if (anyNA(x)) stop(artifact, " may not contain NA values")
  if (anyDuplicated(x$path)) stop(artifact, " duplicate path")
  if (any(!x$class %in% lock_writer_classification_classes_v1()))
    stop(artifact, " unknown class")
  lock_writer_assert_nonempty_v1(
    x, c("path", "class", "reason", "writer"), artifact)
  lock_writer_assert_sha256_v1(x, "baseline_sha256", artifact)
  frozen <- x$class %in% lock_writer_classification_frozen_v1()
  owned <- which(frozen & x$writer != "none")
  if (length(owned))
    stop(artifact, " frozen path is writer-owned; reclassify it out of the ",
         "frozen classes: ", x$path[[owned[[1L]]]], " <- ",
         x$writer[[owned[[1L]]]])
  invisible(TRUE)
}

lock_writer_read_classification_v1 <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path))
    stop("output-path-classification.csv is missing")
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character")
  lock_writer_validate_classification_v1(x, "output-path-classification.csv")
  x
}

lock_writer_v1_self_check <- function() {
  td <- tempfile("lock-writer-v1-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  h <- paste(rep("a", 64L), collapse = "")
  fixtures <- list(
    r_runtime = data.frame(
      schema_version = 1L, r_version = "R 4.5.1", platform = "test-platform",
      rng_kind = "Mersenne-Twister", normal_kind = "Inversion",
      sample_kind = "Rejection", blas_path = "/Library/test/blas",
      blas_sha256 = h, lapack_path = "/Library/test/lapack",
      lapack_sha256 = h, repository_option = "@CRAN@",
      stringsAsFactors = FALSE, check.names = FALSE
    ),
    r_package = data.frame(
      schema_version = c(1L, 1L), package = c("zeta", "alpha"),
      version = c("2.0", "1.0"), source = c("CRAN", "CRAN"),
      repository = c("CRAN", "CRAN"), remote_sha = c("", ""),
      archive_sha256 = c(h, h),
      immutable_source_url = c("https://example/zeta.tgz", "https://example/alpha.tgz"),
      stringsAsFactors = FALSE, check.names = FALSE
    ),
    tex_runtime = data.frame(
      schema_version = c(1L, 1L), record_type = c("tool", "package"),
      name = c("latexmk", "article"), version = c("4.87", "2026-01-01"),
      local_revision = c("", "70000"), catalogue_version = c("", "1.4n"),
      catalogue_date = c("", "2025-01-22"),
      source_tex_root = c("", "/opt/homebrew/texlive"),
      stringsAsFactors = FALSE, check.names = FALSE
    ),
    font = data.frame(
      schema_version = c(1L, 1L), family = c("Menlo", "Charter"),
      face = c("regular", "bold"),
      system_collection_path = c("/System/Library/Fonts/Menlo.ttc",
                                 "/System/Library/Fonts/Charter.ttc"),
      sha256 = c(h, h), stringsAsFactors = FALSE, check.names = FALSE
    ),
    required_outputs = c("output/z.csv", "data/processed/a.rds"),
    source_lock = lock_writer_source_lock_fixture_v1()
  )
  writers <- list(
    r_runtime = write_r_runtime_spec_v1,
    r_package = write_r_package_lock_v1,
    tex_runtime = write_tex_runtime_lock_v1,
    font = write_font_lock_v1,
    required_outputs = write_required_outputs_v1,
    source_lock = write_source_lock_v1
  )
  actual <- vapply(names(writers), function(name) {
    target <- file.path(td, paste0(name, ".fixture"))
    writers[[name]](fixtures[[name]], target)
    lock_writer_sha256_file_v1(target)
  }, "")
  expected <- c(
    r_runtime = "d715977b5764762d43fe11471d7a3add885b592515c0556e06a2bb7cbd0257de",
    r_package = "702397ba9dc27b705cf7efebf00df85e4a33364b4e07a74949a29399818f442d",
    tex_runtime = "d05165f87046f39424eca9e1c75f3a6ce100324ef376f3c0a1dc2640cac4a324",
    font = "e0b3f27b0941e38170bb9c01f27e3c1896eb5af5f584f9023d5b187c8afdbe20",
    required_outputs = "527b36557919f8fc2bad4539f91c54cefe297115afd42baed8447a44bf8a69d5",
    source_lock = "09c91da314547bf1d4ead7450b63cab9149f8ac98cb54a4ab8fa63eede1c056f"
  )
  for (name in names(actual))
    cat("FIXTURE ", name, " ", actual[[name]], "\n", sep = "")
  if (!identical(actual, expected)) {
    mismatch <- names(expected)[actual != expected][[1L]]
    stop("fixture hash mismatch: ", mismatch, " expected ", expected[[mismatch]],
         " got ", actual[[mismatch]])
  }

  expect_error <- function(label, target, thunk, pattern) {
    message <- tryCatch({ thunk(); NA_character_ },
                        error = function(e) conditionMessage(e))
    if (is.na(message)) stop("negative fixture did not error: ", label)
    if (!grepl(pattern, message, fixed = TRUE))
      stop("negative fixture wrong error: ", label, ": ", message)
    if (file.exists(target)) stop("negative fixture emitted bytes: ", label)
    cat("NEGATIVE ", label, ": ", message, "; NO_OUTPUT\n", sep = "")
  }
  target <- file.path(td, "bad-runtime")
  expect_error("r-runtime-extra-field", target, function() {
    bad <- fixtures$r_runtime
    bad$extra <- "drift"
    write_r_runtime_spec_v1(bad, target)
  }, "schema names/order drift")
  target <- file.path(td, "bad-package")
  expect_error("r-package-mistyped-version", target, function() {
    bad <- fixtures$r_package
    bad$version <- c(2L, 1L)
    write_r_package_lock_v1(bad, target)
  }, "expected character, got integer")
  target <- file.path(td, "bad-tex")
  expect_error("tex-runtime-missing-field", target, function() {
    bad <- fixtures$tex_runtime[, -8L, drop = FALSE]
    write_tex_runtime_lock_v1(bad, target)
  }, "schema names/order drift")
  target <- file.path(td, "bad-font")
  expect_error("font-extra-field", target, function() {
    bad <- fixtures$font
    bad$extra <- "drift"
    write_font_lock_v1(bad, target)
  }, "schema names/order drift")
  target <- file.path(td, "bad-required-outputs")
  expect_error("required-outputs-mistyped", target, function() {
    write_required_outputs_v1(as.list(fixtures$required_outputs), target)
  }, "input must be a character vector")
  target <- file.path(td, "bad-source-lock-missing")
  expect_error("source-lock-missing-field", target, function() {
    bad <- fixtures$source_lock[-6L]
    write_source_lock_v1(bad, target)
  }, "schema names/order drift")
  target <- file.path(td, "bad-source-lock-extra")
  expect_error("source-lock-extra-field", target, function() {
    bad <- fixtures$source_lock
    bad$extra <- "drift"
    write_source_lock_v1(bad, target)
  }, "schema names/order drift")
  target <- file.path(td, "bad-source-lock-misnamed")
  expect_error("source-lock-misnamed-field", target, function() {
    bad <- fixtures$source_lock
    names(bad)[[6L]] <- "input_lock_hash"
    write_source_lock_v1(bad, target)
  }, "schema names/order drift")
  target <- file.path(td, "bad-source-lock-mistyped")
  expect_error("source-lock-mistyped-field", target, function() {
    bad <- fixtures$source_lock
    bad$schema_version <- 1
    write_source_lock_v1(bad, target)
  }, "expected integer, got double")
  target <- file.path(td, "bad-source-lock-reordered")
  expect_error("source-lock-reordered-field", target, function() {
    bad <- fixtures$source_lock[c(2L, 1L, 3:length(fixtures$source_lock))]
    write_source_lock_v1(bad, target)
  }, "schema names/order drift")
  h64 <- paste(rep("b", 64L), collapse = "")
  classification <- data.frame(
    path = c("output/a.csv", "output/b.csv", "output/c.csv"),
    class = c("canonical_generated", "historical_cache_legacy_excluded",
              "regenerated_validation_output"),
    baseline_sha256 = c(h64, h64, h64),
    reason = c("canonical_current_run_product",
               "excluded_pre_Gate_cache_or_legacy_product",
               "regenerated_by_make_extreme_value"),
    writer = c("unknown", "none", "R/10-extreme-value-tests.R"),
    stringsAsFactors = FALSE, check.names = FALSE)
  lock_writer_validate_classification_v1(classification, "classification")
  cat("FIXTURE classification writer-owned non-frozen row accepted\n")
  target <- file.path(td, "never-written")
  expect_error("classification-frozen-writer-owned", target, function() {
    bad <- classification
    bad$class[[3L]] <- "historical_cache_legacy_excluded"
    lock_writer_validate_classification_v1(bad, "classification")
  }, "frozen path is writer-owned")
  expect_error("classification-frozen-writer-unknown", target, function() {
    bad <- classification
    bad$writer[[2L]] <- "unknown"
    lock_writer_validate_classification_v1(bad, "classification")
  }, "frozen path is writer-owned")
  expect_error("classification-missing-writer-column", target, function() {
    lock_writer_validate_classification_v1(
      classification[, -5L, drop = FALSE], "classification")
  }, "schema names/order drift")
  expect_error("classification-empty-writer", target, function() {
    bad <- classification
    bad$writer[[1L]] <- ""
    lock_writer_validate_classification_v1(bad, "classification")
  }, "empty value in field 'writer'")
  expect_error("classification-unknown-class", target, function() {
    bad <- classification
    bad$class[[1L]] <- "invented_class"
    lock_writer_validate_classification_v1(bad, "classification")
  }, "unknown class")
  expect_error("classification-bad-baseline", target, function() {
    bad <- classification
    bad$baseline_sha256[[2L]] <- "short"
    lock_writer_validate_classification_v1(bad, "classification")
  }, "invalid SHA-256 in field 'baseline_sha256'")
  cat("SELF-CHECK PASS\n")
  invisible(actual)
}

if (sys.nframe() == 0L) lock_writer_v1_self_check()
