#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1L) args[[1L]] else "verify"
cache_dir <- if (length(args) >= 2L) args[[2L]] else
  file.path("reproducibility", "package-archives")
if (!mode %in% c("capture", "verify", "restore")) {
  stop("usage: bootstrap-renv.R capture|verify|restore [archive-cache]")
}

source("R/functions/lock-writer-v1.R")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("bootstrap-renv.R requires jsonlite before model restoration")
}

sha256 <- lock_writer_sha256_file_v1
lock <- jsonlite::read_json("renv.lock", simplifyVector = FALSE)
records <- lock$Packages
if (!is.list(records) || length(records) == 0L || anyDuplicated(names(records))) {
  stop("renv.lock has an invalid package registry")
}
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

record_value <- function(record, field) {
  value <- record[[field]]
  if (is.null(value) || length(value) != 1L || is.na(value) || value == "") {
    stop("renv.lock package record missing ", field)
  }
  as.character(value)
}

archive_name <- function(package, version) {
  paste0(package, "_", version, ".tar.gz")
}

official_urls <- function(package, version) {
  filename <- archive_name(package, version)
  c(paste0("https://cran.r-project.org/src/contrib/", filename),
    paste0("https://cran.r-project.org/src/contrib/Archive/", package,
           "/", filename))
}

validate_archive <- function(path, package, version) {
  td <- tempfile("archive-description-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  member <- paste0(package, "/DESCRIPTION")
  listed <- utils::untar(path, list = TRUE)
  if (!member %in% listed) stop("archive lacks exact DESCRIPTION path: ", path)
  utils::untar(path, files = member, exdir = td)
  dcf <- read.dcf(file.path(td, member), fields = c("Package", "Version", "Repository"))
  if (!identical(unname(dcf[1L, "Package"]), package) ||
      !identical(unname(dcf[1L, "Version"]), version) ||
      !identical(unname(dcf[1L, "Repository"]), "CRAN")) {
    stop("archive identity mismatch: ", path)
  }
  invisible(TRUE)
}

download_exact <- function(url, destination) {
  curl <- Sys.which("curl")
  if (identical(unname(curl), "")) stop("curl executable not found")
  status <- system2(unname(curl),
                    c("--location", "--fail", "--silent", "--show-error",
                      url, "--output", destination))
  identical(status, 0L)
}

capture_row <- function(name) {
  record <- records[[name]]
  package <- record_value(record, "Package")
  version <- record_value(record, "Version")
  if (!identical(package, name) ||
      !identical(record_value(record, "Source"), "Repository") ||
      !identical(record_value(record, "Repository"), "CRAN")) {
    stop("only exact CRAN Repository records are permitted: ", name)
  }
  destination <- file.path(cache_dir, archive_name(package, version))
  selected_url <- NA_character_
  for (url in official_urls(package, version)) {
    candidate <- paste0(destination, ".download")
    if (file.exists(candidate)) unlink(candidate)
    if (download_exact(url, candidate)) {
      if (file.exists(destination)) unlink(destination)
      if (!file.rename(candidate, destination)) stop("cannot stage archive")
      selected_url <- url
      break
    }
    if (file.exists(candidate)) unlink(candidate)
  }
  if (is.na(selected_url)) stop("official CRAN source archive unavailable: ", name)
  validate_archive(destination, package, version)
  data.frame(
    schema_version = 1L, package = package, version = version,
    source = "CRAN", repository = "CRAN", remote_sha = "",
    archive_sha256 = sha256(destination),
    immutable_source_url = selected_url,
    stringsAsFactors = FALSE, check.names = FALSE)
}

read_audit_lock <- function() {
  path <- file.path("reproducibility", "r-package-lock.csv")
  if (!file.exists(path)) stop("r-package-lock.csv is missing")
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character")
  x$schema_version <- as.integer(x$schema_version)
  lock_writer_validate_schema_v1(
    x, lock_writer_schema_v1("r_package"), "r-package-lock.csv")
  x
}

verify_cache <- function(audit) {
  expected <- sort(names(records), method = "radix")
  observed <- sort(audit$package, method = "radix")
  if (!identical(observed, expected) || anyDuplicated(audit$package)) {
    stop("r-package-lock.csv closure row-set mismatch")
  }
  for (i in seq_len(nrow(audit))) {
    row <- audit[i, ]
    record <- records[[row$package]]
    if (!identical(row$version, record_value(record, "Version")) ||
        !identical(row$source, "CRAN") || !identical(row$repository, "CRAN") ||
        row$remote_sha != "" ||
        !grepl(paste0("^https://cran\\.r-project\\.org/src/contrib/(Archive/",
                      row$package, "/)?", row$package, "_",
                      gsub("([.])", "\\\\\\1", row$version),
                      "\\.tar\\.gz$"), row$immutable_source_url, perl = TRUE) ||
        !grepl("^[0-9a-f]{64}$", row$archive_sha256)) {
      stop("invalid immutable package-lock row: ", row$package)
    }
    archive <- file.path(cache_dir, archive_name(row$package, row$version))
    if (!file.exists(archive)) stop("verified archive is missing: ", archive)
    if (!identical(sha256(archive), row$archive_sha256)) {
      stop("verified archive SHA-256 mismatch: ", row$package)
    }
    validate_archive(archive, row$package, row$version)
  }
  invisible(TRUE)
}

if (identical(mode, "capture")) {
  rows <- lapply(sort(names(records), method = "radix"), capture_row)
  audit <- do.call(rbind, rows)
  write_r_package_lock_v1(
    audit, file.path("reproducibility", "r-package-lock.csv"))
  cat("CAPTURE PASS:", nrow(audit), "verified source archives\n")
  quit(status = 0L)
}

audit <- read_audit_lock()
verify_cache(audit)
cat("ARCHIVE PREFLIGHT PASS:", nrow(audit), "verified source archives\n")
if (identical(mode, "verify")) quit(status = 0L)

bootstrap_lib <- file.path("renv", "bootstrap")
renv_archive <- file.path(cache_dir, "renv_1.2.0.tar.gz")
if (!requireNamespace("renv", quietly = TRUE, lib.loc = bootstrap_lib)) {
  status <- system2(file.path(R.home("bin"), "R"),
                    c("CMD", "INSTALL", "--library", bootstrap_lib,
                      renv_archive))
  if (!identical(status, 0L)) stop("verified renv bootstrap install failed")
}
.libPaths(c(normalizePath(bootstrap_lib), .libPaths()))
Sys.setenv(RENV_CONFIG_CACHE_ENABLED = "FALSE")
options(repos = c(CRAN = "@CRAN@"))
renv::restore(project = ".", lockfile = "renv.lock", clean = TRUE,
              prompt = FALSE, strict = TRUE)
cat("STRICT RESTORE PASS\n")
