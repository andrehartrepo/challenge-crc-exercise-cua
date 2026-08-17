# Canonical in-memory provenance hashing, version 1 public API.
#
# This version retains the versioned path and public function names while
# strengthening finite-double encoding to bit-exact binary64 hexadecimal.
#
# This file is the single executable definition of the canonical object-to-row
# normalisation and byte encoding used by the model/reproducibility
# specification. Declared SCHEMA field names must match
# ^[A-Za-z][A-Za-z0-9_]*$ and declared storage types are enforced with
# typeof() before encoding. That grammar applies only to declared schema field
# names. Hostile OBJECT names (including '/', ':', commas, and delimiters) are
# legal input data and are length-prefixed into node_path values.

canonical_schema_v1 <- function() {
  c(
    input_spec_id = "character", node_path = "character",
    node_kind = "character", position = "integer",
    value_type = "character", character_value = "character",
    integer_value = "integer", double_value = "double",
    logical_value = "logical"
  )
}

utf8_key_v1 <- function(x) {
  if (length(x) != 1L || is.na(x)) stop("name must be one non-missing string")
  paste(sprintf("%02X", as.integer(charToRaw(enc2utf8(x)))), collapse = "")
}

name_token_v1 <- function(x) {
  if (length(x) == 0L || is.na(x)) return("N")
  x <- enc2utf8(x)
  paste0("S", nchar(x, type = "bytes"), ":", x)
}

path_segment_v1 <- function(tag, position, name = NA_character_) {
  if (!tag %in% c("E", "A", "V") || length(position) != 1L ||
      is.na(position) || position < 1L) stop("invalid path segment")
  paste0("/", tag, as.integer(position), ":", name_token_v1(name))
}

container_type_v1 <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.data.frame(x)) return("DATA_FRAME")
  if (is.pairlist(x)) stop("pairlists are unsupported")
  if (is.list(x)) return("LIST")
  t <- typeof(x)
  if (!t %in% c("character", "integer", "double", "logical"))
    stop("unsupported R type: ", t)
  stem <- switch(t, character = "CHARACTER", integer = "INTEGER",
                 double = "DOUBLE", logical = "LOGICAL")
  if (is.matrix(x)) return(paste0(stem, "_MATRIX"))
  if (is.array(x)) return(paste0(stem, "_ARRAY"))
  paste0(stem, "_VECTOR")
}

attribute_order_v1 <- function(x) {
  a <- attributes(x)
  if (is.null(a)) return(character())
  n <- names(a)
  if (is.null(n) || anyNA(n) || any(n == "") || anyDuplicated(n))
    stop("attributes must have unique non-empty names")
  lead <- c("names", "class", "dim", "dimnames")
  fixed <- lead[lead %in% n]
  extra <- setdiff(n, lead)
  if (length(extra)) {
    keys <- vapply(extra, utf8_key_v1, "")
    extra <- extra[order(keys, method = "radix")]
  }
  c(fixed, extra)
}

canonical_object_rows_v1 <- function(x, input_spec_id) {
  if (length(input_spec_id) != 1L || is.na(input_spec_id) || input_spec_id == "")
    stop("input_spec_id must be one non-empty string")
  input_spec_id <- enc2utf8(input_spec_id)
  rows <- list()
  add_row <- function(path, kind, position, value_type,
                      cv = NA_character_, iv = NA_integer_,
                      dv = NA_real_, lv = NA) {
    rows[[length(rows) + 1L]] <<- data.frame(
      input_spec_id = input_spec_id, node_path = enc2utf8(path),
      node_kind = kind, position = as.integer(position),
      value_type = value_type, character_value = cv,
      integer_value = iv, double_value = dv, logical_value = lv,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  walk <- function(value, path, role, position) {
    if (is.object(value) && isS4(value)) stop("S4 objects are unsupported")
    kind <- paste0(role, "_CONTAINER")
    add_row(path, kind, position, container_type_v1(value))
    if (is.list(value)) {
      nm <- names(value)
      for (i in seq_along(value)) {
        label <- if (is.null(nm)) NA_character_ else nm[[i]]
        walk(value[[i]], paste0(path, path_segment_v1("E", i, label)),
             "ELEMENT", i)
      }
    } else if (!is.null(value)) {
      nm <- names(value)
      t <- typeof(value)
      scalar_type <- switch(t, character = "CHARACTER", integer = "INTEGER",
                            double = "DOUBLE", logical = "LOGICAL")
      for (i in seq_along(value)) {
        label <- if (is.null(nm)) NA_character_ else nm[[i]]
        scalar_path <- paste0(path, path_segment_v1("V", i, label))
        if (t == "character") {
          v <- value[[i]]
          add_row(scalar_path, "VALUE_SCALAR", i, scalar_type,
                  cv = if (is.na(v)) NA_character_ else enc2utf8(v))
        } else if (t == "integer") {
          add_row(scalar_path, "VALUE_SCALAR", i, scalar_type, iv = value[[i]])
        } else if (t == "double") {
          add_row(scalar_path, "VALUE_SCALAR", i, scalar_type, dv = value[[i]])
        } else {
          add_row(scalar_path, "VALUE_SCALAR", i, scalar_type, lv = value[[i]])
        }
      }
    }
    a <- attributes(value)
    ao <- attribute_order_v1(value)
    for (j in seq_along(ao)) {
      an <- ao[[j]]
      walk(a[[an]], paste0(path, path_segment_v1("A", j, an)),
           "ATTRIBUTE", j)
    }
    invisible(NULL)
  }
  walk(x, "$", "ROOT", 0L)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

percent_encode_v1 <- function(x) {
  b <- as.integer(charToRaw(enc2utf8(x)))
  allowed <- (b >= 65L & b <= 90L) | (b >= 97L & b <= 122L) |
    (b >= 48L & b <= 57L) | b %in% c(46L, 95L, 126L, 47L, 45L)
  paste(ifelse(allowed, rawToChar(as.raw(b), multiple = TRUE),
               sprintf("%%%02X", b)), collapse = "")
}

raw_to_hex_v2 <- function(x) {
  if (typeof(x) != "raw") stop("raw input required")
  paste(sprintf("%02x", as.integer(x)), collapse = "")
}

hex_to_raw_v2 <- function(x) {
  if (typeof(x) != "character" || length(x) != 1L || is.na(x) ||
      !grepl("^[0-9a-f]{16}$", x, perl = TRUE))
    stop("binary64 hex must be exactly 16 lowercase hexadecimal characters")
  pairs <- substring(x, seq.int(1L, 15L, by = 2L),
                     seq.int(2L, 16L, by = 2L))
  as.raw(strtoi(pairs, base = 16L))
}

encode_double_v1 <- function(x) {
  if (is.na(x) && !is.nan(x)) return("NA")
  if (is.nan(x)) return("NaN")
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  before <- writeBin(as.double(x), raw(), size = 8L, endian = "big")
  out <- raw_to_hex_v2(before)
  after <- hex_to_raw_v2(out)
  if (!identical(before, after)) stop("double raw bytes do not round-trip")
  out
}

validate_finite_declared_v1 <- function(rows, fields) {
  if (!is.data.frame(rows) || typeof(fields) != "character" ||
      anyNA(fields) || anyDuplicated(fields) || any(!fields %in% names(rows)))
    stop("invalid finite-field declaration")
  for (field in fields) {
    if (typeof(rows[[field]]) != "double")
      stop("finite-declared field is not double: ", field)
    if (any(!is.finite(rows[[field]])))
      stop("non-finite value in finite-declared field: ", field)
  }
  invisible(TRUE)
}

validate_canonical_schema_v1 <- function(rows, schema) {
  if (typeof(schema) != "character" || is.null(names(schema)))
    stop("schema must be a named character vector")
  if (anyNA(names(schema)) || any(names(schema) == "") ||
      anyDuplicated(names(schema))) stop("schema field names must be unique")
  identifier_regex <- "^[A-Za-z][A-Za-z0-9_]*$"
  invalid_names <- !grepl(identifier_regex, names(schema), perl = TRUE)
  if (any(invalid_names))
    stop("invalid schema field name: ", names(schema)[which(invalid_names)[1L]])
  supported <- c("character", "integer", "double", "logical")
  if (anyNA(schema) || any(!schema %in% supported))
    stop("unsupported declared schema type")
  if (!is.data.frame(rows)) stop("rows must be a data frame")
  if (!identical(names(rows), names(schema))) stop("schema names/order drift")
  for (j in seq_along(schema)) {
    actual <- typeof(rows[[j]])
    expected <- unname(schema[[j]])
    if (!identical(actual, expected))
      stop("storage type drift for field '", names(schema)[[j]],
           "': expected ", expected, ", got ", actual)
  }
  invisible(TRUE)
}

canonical_bytes_v1 <- function(rows, schema = canonical_schema_v1()) {
  validate_canonical_schema_v1(rows, schema)
  encode_field <- function(x, type) {
    if (length(x) != 1L) stop("field is not scalar")
    if (type == "character")
      return(if (is.na(x)) "NA" else paste0("s:", percent_encode_v1(x)))
    if (type == "integer") return(if (is.na(x)) "NA" else as.character(x))
    if (type == "logical") return(if (is.na(x)) "NA" else if (x) "TRUE" else "FALSE")
    if (type == "double") return(encode_double_v1(x))
    stop("unsupported schema type")
  }
  header <- paste(names(schema), collapse = ",")
  lines <- vapply(seq_len(nrow(rows)), function(i) {
    paste(vapply(seq_along(schema), function(j) {
      encode_field(rows[[j]][[i]], schema[[j]])
    }, ""), collapse = ",")
  }, "")
  paste0(paste(c(header, lines), collapse = "\n"), "\n")
}

sha256_text_v1 <- function(bytes) {
  f <- tempfile("canonical-v1-")
  on.exit(unlink(f), add = TRUE)
  writeBin(charToRaw(bytes), f)
  unname(tools::sha256sum(f))
}

canonical_hash_v1 <- function(rows, schema = canonical_schema_v1()) {
  sha256_text_v1(canonical_bytes_v1(rows, schema))
}

canonical_object_hash_v1 <- function(x, input_spec_id) {
  canonical_hash_v1(canonical_object_rows_v1(x, input_spec_id))
}

canonical_hash_v1_self_check <- function() {
  fixture_nested_input_spec <- list(
    "a/b" = c("alpha", NA_character_),
    "c:d" = c(1L, NA_integer_),
    "norsk:ø" = "blå/å",
    empty = list(),
    mixed = c(0, -as.double(FALSE), NA_real_, NaN, Inf, -Inf),
    flags = c(TRUE, NA),
    matrix = matrix(c(1, 2, 3, 4), nrow = 2L)
  )
  attr(fixture_nested_input_spec, "meta:source") <- "x/y"

  fixture_primary_reference_frame <- data.frame(
    "a/b" = c(0.25, NA_real_),
    "c:d" = c(2L, 3L),
    flag = c(TRUE, FALSE),
    check.names = FALSE
  )
  attr(fixture_primary_reference_frame, "surv_coef_dfs") <-
    matrix(c(0.1, 0.2), nrow = 1L,
           dimnames = list("r/1", c("p:1", "p/2")))
  attr(fixture_primary_reference_frame, "empty") <- list()

  fixture_endpoint_block <- data.frame(
    row_id = c(2L, 1L), log_HR = c(-as.double(FALSE), 0.125),
    check.names = FALSE
  )
  fixture_endpoint_schema <- c(row_id = "integer", log_HR = "double")

  fixture_u_prog_uniform <- data.frame(
    position = 1:3, u_prog_uniform = c(0, 0.5, 1), check.names = FALSE
  )
  fixture_u_prog_schema <- c(position = "integer", u_prog_uniform = "double")

  actual <- c(
    fixture_nested_input_spec = canonical_object_hash_v1(
      fixture_nested_input_spec, "fixture_nested_input_spec"),
    fixture_primary_reference_frame = canonical_object_hash_v1(
      fixture_primary_reference_frame, "primary_reference_frame"),
    fixture_endpoint_block = canonical_hash_v1(
      fixture_endpoint_block, fixture_endpoint_schema),
    fixture_u_prog_uniform = canonical_hash_v1(
      fixture_u_prog_uniform, fixture_u_prog_schema)
  )
  expected <- c(
    fixture_nested_input_spec =
      "f022e375d885de85b12118f0dcf728e77f7a0980a1e767883847e672903034c9",
    fixture_primary_reference_frame =
      "9df3805dd71ae24d995ccd1181db58d4be52a3b09c536278deb1d0046dca7d20",
    fixture_endpoint_block =
      "ee48a0a0f895b6f521d031ccb8a22484202d7e3c9e605173d45753e218235c05",
    fixture_u_prog_uniform =
      "ab90abc8f7c5e06b1bee049fd5c8971e0e995d821360f1dc769f28b288bef8ee"
  )
  for (nm in names(expected)) {
    if (!identical(unname(actual[[nm]]), unname(expected[[nm]])))
      stop("positive fixture hash mismatch: ", nm,
           " expected ", expected[[nm]], " got ", actual[[nm]])
    cat("POSITIVE ", nm, " ", actual[[nm]], "\n", sep = "")
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) prior_seed <- get(".Random.seed", envir = .GlobalEnv,
                                  inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", prior_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(42L)
  witness <- rnorm(5L)[5L]
  witness_bytes <- writeBin(witness, raw(), size = 8L, endian = "big")
  witness_hex <- encode_double_v1(witness)
  if (!identical(hex_to_raw_v2(witness_hex), witness_bytes))
    stop("witness draw raw-byte round-trip failed")
  cat("POSITIVE fixture_witness_draw ", witness_hex, "\n", sep = "")

  positive_zero <- encode_double_v1(0)
  negative_zero <- encode_double_v1(-as.double(FALSE))
  if (!identical(positive_zero, "0000000000000000") ||
      !identical(negative_zero, "8000000000000000") ||
      identical(positive_zero, negative_zero))
    stop("signed-zero bit patterns are not distinct")
  cat("POSITIVE fixture_signed_zero ", positive_zero, " ", negative_zero,
      "\n", sep = "")

  adjacent_one <- readBin(hex_to_raw_v2("3ff0000000000000"), "double",
                          n = 1L, size = 8L, endian = "big")
  adjacent_next <- readBin(hex_to_raw_v2("3ff0000000000001"), "double",
                           n = 1L, size = 8L, endian = "big")
  if (identical(adjacent_one, adjacent_next) ||
      identical(encode_double_v1(adjacent_one),
                encode_double_v1(adjacent_next)))
    stop("adjacent binary64 values do not encode distinctly")
  cat("POSITIVE fixture_adjacent_binary64 ",
      encode_double_v1(adjacent_one), " ", encode_double_v1(adjacent_next),
      "\n", sep = "")

  expect_error <- function(label, thunk, pattern) {
    message <- tryCatch({
      thunk()
      NA_character_
    }, error = function(e) conditionMessage(e))
    if (is.na(message)) stop("negative fixture did not error: ", label)
    if (!grepl(pattern, message, fixed = TRUE))
      stop("negative fixture wrong error: ", label, ": ", message)
    cat("NEGATIVE ", label, ": ", message, "\n", sep = "")
  }

  expect_error("double-as-integer", function() {
    bad <- fixture_endpoint_block
    bad$row_id <- as.double(bad$row_id)
    canonical_hash_v1(bad, fixture_endpoint_schema)
  }, "expected integer, got double")
  expect_error("integer-as-double", function() {
    bad <- fixture_endpoint_block
    bad$log_HR <- c(0L, 1L)
    canonical_hash_v1(bad, fixture_endpoint_schema)
  }, "expected double, got integer")
  expect_error("numeric-as-logical", function() {
    bad <- data.frame(flag = c(0, 1), check.names = FALSE)
    canonical_hash_v1(bad, c(flag = "logical"))
  }, "expected logical, got double")
  expect_error("delimiter-bearing-schema-field", function() {
    bad <- data.frame("a,b" = 1L, check.names = FALSE)
    canonical_hash_v1(bad, c("a,b" = "integer"))
  }, "invalid schema field name: a,b")
  expect_error("non-finite-in-finite-declared-field", function() {
    bad <- data.frame(value = c(0, Inf), check.names = FALSE)
    validate_finite_declared_v1(bad, "value")
  }, "non-finite value in finite-declared field: value")

  cat("SELF-CHECK PASS\n")
  invisible(actual)
}

if (sys.nframe() == 0L) canonical_hash_v1_self_check()
