# ==============================================================================
# 08-export-tables.R
# Standardized formatting function and centralized DOCX export
# Adapted from HEVAL5200 course material
#
# Input:  PSA results, structural SA results, model outputs from upstream code
# Output: 14 formatted .docx tables in output/tables/
#
# REFERENCE CODE PROVENANCE:
#   format_thesis_table() adapted from HEVAL5200 format_standard_table()
#     (HEVAL5200 course material)
#   add_thesis_footnotes() adapted from HEVAL5200 add_table_footnotes()
#     (HEVAL5200 course material)
#   Centralized export structure adapted from HEVAL5200 course material
#     (centralized DOCX export pattern)
#
# PSA is the base-case analysis method
# Structural SA required
# GUIDELINE: CHEERS-VOI reporting standard (Kunst et al. 2024)
# GUIDELINE: ISPOR Good Practices for transparency in modelling outputs
# ==============================================================================

# --- Package dependencies (loaded in main.R) ----------------------------------
# flextable: table construction and formatting
# officer: fp_border, fp_text, fp_par for Word-compatible styling

if (!requireNamespace("flextable", quietly = TRUE)) {
  stop("Package 'flextable' required for table export. Install with: ",
       "install.packages('flextable')")
}
if (!requireNamespace("officer", quietly = TRUE)) {
  stop("Package 'officer' required for table export. Install with: ",
       "install.packages('officer')")
}

# --- Constants ----------------------------------------------------------------
# Adapted from HEVAL5200 course material with thesis-specific parameterization
# GUIDELINE: Typography standards (Butterick, Practical Typography)

THESIS_TABLE_FONT <- "Charter"       # Thesis body font (academic serif)
# JOURNAL_TABLE_FONT removed. Unused constant; journal-specific font
# handling will be added if/when a target journal is selected that
# requires a Times New Roman exception to the project typography standard.
#
BODY_FONT_SIZE <- 10
HEADER_FONT_SIZE <- 10
CAPTION_FONT_SIZE <- 10
FOOTNOTE_FONT_SIZE <- 8

# Output directory
TABLE_OUTPUT_DIR <- "output/tables"

# SOURCE: price basis adjudicated in build_deflator_table() (Table A.16 caption).
# Caution: shared by tables whose printed money is composed from the face-value
# inputs; tables printing no cost carry no price-basis note at all.
PRICE_BASIS_NOTE <- "Costs are in 2024 NOK, except the July-2025 Helfo tariffs, the 2026 Pasientreiser mileage rate, and values composed from them, at face value."

# Fixed values for deterministic DOCX containers. The dates are provenance-
# neutral constants; the actual run time belongs only in the volatile run log.
DOCX_FIXED_CORE_TIMESTAMP_V1 <- "2000-01-01T00:00:00Z"
DOCX_FIXED_MEMBER_TIME_V1 <- as.POSIXct(
  "2000-01-01 00:00:00", tz = "UTC")


# --- Deterministic DOCX export -----------------------------------------------

normalize_docx_container_v1 <- function(source_path, output_path) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("Package 'zip' is required for deterministic DOCX export.")
  }
  if (!file.exists(source_path)) {
    stop("DOCX source container does not exist: ", source_path)
  }

  source_path <- normalizePath(source_path, mustWork = TRUE)
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_path <- file.path(
    normalizePath(output_dir, mustWork = TRUE), basename(output_path))

  staging_dir <- tempfile(pattern = "docx-normalize-")
  dir.create(staging_dir)
  on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(source_path, exdir = staging_dir)

  core_path <- file.path(staging_dir, "docProps", "core.xml")
  if (!file.exists(core_path)) {
    stop("DOCX container is missing docProps/core.xml: ", source_path)
  }
  core_xml <- paste(readLines(core_path, warn = FALSE), collapse = "\n")
  for (property in c("created", "modified")) {
    pattern <- paste0(
      "(?<=<dcterms:", property,
      " xsi:type=\"dcterms:W3CDTF\">)[^<]*(?=</dcterms:",
      property, ">)")
    if (!grepl(pattern, core_xml, perl = TRUE)) {
      stop("DOCX core property is missing or malformed: dcterms:", property)
    }
    core_xml <- sub(
      pattern, DOCX_FIXED_CORE_TIMESTAMP_V1, core_xml, perl = TRUE)
  }
  writeLines(core_xml, core_path, useBytes = TRUE)

  members <- sort(
    list.files(staging_dir, recursive = TRUE, full.names = TRUE,
               all.files = TRUE, no.. = TRUE),
    method = "radix")
  if (length(members) == 0L) {
    stop("DOCX container has no file members: ", source_path)
  }
  relative_members <- substring(members, nchar(staging_dir) + 2L)

  required_members <- c(
    "[Content_Types].xml", "_rels/.rels", "word/document.xml")
  if (!all(required_members %in% relative_members)) {
    stop("DOCX container is missing required OOXML package members.")
  }
  root_relationships <- paste(
    readLines(file.path(staging_dir, "_rels", ".rels"), warn = FALSE),
    collapse = "\n")
  relationship_tags <- regmatches(
    root_relationships,
    gregexpr("<Relationship\\b[^>]*>", root_relationships, perl = TRUE)
  )[[1L]]
  office_type <- paste0(
    "Type=\"",
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument",
    "\"")
  office_relationships <- relationship_tags[
    grepl(office_type, relationship_tags, fixed = TRUE)]
  if (length(office_relationships) != 1L ||
      !grepl("Target=\"word/document.xml\"", office_relationships,
             fixed = TRUE) ||
      grepl("TargetMode=", office_relationships, fixed = TRUE)) {
    stop("DOCX root relationships do not resolve word/document.xml.")
  }

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required for DOCX relationship validation.")
  }
  relationship_document <- tryCatch(
    xml2::read_xml(root_relationships, options = "NONET"),
    error = function(e) {
      stop("DOCX root relationships do not resolve word/document.xml.")
    })
  relationship_namespace <- paste0(
    "http://schemas.openxmlformats.org/package/2006/",
    "relationships")
  relationship_root <- xml2::xml_find_all(
    relationship_document,
    paste0("/*[local-name()='Relationships' and namespace-uri()='",
           relationship_namespace, "']"))
  relationship_nodes <- xml2::xml_find_all(
    relationship_document,
    paste0("/*[local-name()='Relationships' and namespace-uri()='",
           relationship_namespace,
           "']/*[local-name()='Relationship' and namespace-uri()='",
           relationship_namespace, "']"))
  get_relationship_attribute <- function(node, attribute) {
    attributes <- xml2::xml_attrs(node, ns = xml2::xml_ns(node))
    if (!attribute %in% names(attributes)) return(NA_character_)
    unname(attributes[[attribute]])
  }
  relationship_types <- vapply(
    relationship_nodes, get_relationship_attribute, character(1),
    attribute = "Type")
  office_relationship_nodes <- relationship_nodes[
    !is.na(relationship_types) & relationship_types == paste0(
      "http://schemas.openxmlformats.org/officeDocument/2006/relationships/",
      "officeDocument")]
  office_target <- if (length(office_relationship_nodes) == 1L) {
    get_relationship_attribute(office_relationship_nodes[[1L]], "Target")
  } else {
    NA_character_
  }
  office_target_mode <- if (length(office_relationship_nodes) == 1L) {
    get_relationship_attribute(
      office_relationship_nodes[[1L]], "TargetMode")
  } else {
    NA_character_
  }
  if (length(relationship_root) != 1L ||
      length(office_relationship_nodes) != 1L ||
      !identical(office_target, "word/document.xml") ||
      !is.na(office_target_mode)) {
    stop("DOCX root relationships do not resolve word/document.xml.")
  }

  prior_tz <- Sys.getenv("TZ", unset = NA_character_)
  Sys.setenv(TZ = "UTC")
  on.exit({
    if (is.na(prior_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = prior_tz)
  }, add = TRUE)
  timestamp_set <- vapply(
    members, Sys.setFileTime, logical(1), time = DOCX_FIXED_MEMBER_TIME_V1)
  if (!all(timestamp_set)) {
    stop("Could not set fixed DOCX member timestamps.")
  }

  if (file.exists(output_path)) unlink(output_path)
  zip::zip(
    zipfile = output_path,
    files = relative_members,
    recurse = FALSE,
    compression_level = 9,
    include_directories = FALSE,
    root = staging_dir,
    mode = "mirror")

  archive <- utils::unzip(output_path, list = TRUE)
  if (!identical(as.character(archive$Name), relative_members)) {
    stop("Normalized DOCX member ordering is not canonical.")
  }
  if (!all(format(archive$Date, "%Y-%m-%d %H:%M:%S", tz = "UTC") ==
           "2000-01-01 00:00:00")) {
    stop("Normalized DOCX member timestamps are not fixed.")
  }
  invisible(output_path)
}


save_as_deterministic_docx_v1 <- function(ft, path) {
  raw_path <- tempfile(pattern = "docx-raw-", fileext = ".docx")
  on.exit(unlink(raw_path), add = TRUE)
  flextable::save_as_docx(ft, path = raw_path)
  normalize_docx_container_v1(raw_path, path)
}


assert_deterministic_docx_export_v1 <- function() {
  fixture <- flextable::flextable(data.frame(
    fixture = "deterministic-docx-v1", value = 1L,
    stringsAsFactors = FALSE))
  first <- tempfile(pattern = "docx-fixture-a-", fileext = ".docx")
  second <- tempfile(pattern = "docx-fixture-b-", fileext = ".docx")
  on.exit(unlink(c(first, second)), add = TRUE)

  save_as_deterministic_docx_v1(fixture, first)
  Sys.sleep(1.1)
  save_as_deterministic_docx_v1(fixture, second)
  first_bytes <- readBin(first, what = "raw", n = file.info(first)$size)
  second_bytes <- readBin(second, what = "raw", n = file.info(second)$size)
  if (!identical(first_bytes, second_bytes)) {
    stop("Deterministic DOCX fixture produced non-identical bytes.")
  }
  invisible(TRUE)
}


# --- format_thesis_table() ----------------------------------------------------
# Adapted from HEVAL5200 format_standard_table()
# Adaptations: parameterized font family, header_rows for gtsummary support,
# target dispatch (thesis vs journal)

format_thesis_table <- function(ft,
                                caption_text = NULL,
                                table_number = NULL,
                                add_row_shading = TRUE,
                                font_family = THESIS_TABLE_FONT,
                                header_rows = 1) {

  n_rows <- flextable::nrow_part(ft, part = "body")

  ft <- ft |>
    flextable::theme_booktabs() |>
    # Font settings
    flextable::font(fontname = font_family, part = "all") |>
    flextable::fontsize(size = BODY_FONT_SIZE, part = "body") |>
    flextable::fontsize(size = HEADER_FONT_SIZE, part = "header") |>
    flextable::bold(part = "header") |>
    # Alignment
    flextable::align(align = "center", part = "header") |>
    flextable::align(align = "left", part = "body") |>
    flextable::valign(valign = "center", part = "all") |>
    # Spacing and padding
    flextable::padding(padding.top = 4, padding.bottom = 4,
                       padding.left = 5, padding.right = 5, part = "all") |>
    flextable::line_spacing(space = 1, part = "all") |>
    # Borders (booktabs style)
    flextable::hline_top(
      part = "header",
      border = officer::fp_border(color = "black", width = 1.5)
    ) |>
    flextable::hline_bottom(
      part = "header",
      border = officer::fp_border(color = "black", width = 0.75)
    ) |>
    flextable::hline_bottom(
      part = "body",
      border = officer::fp_border(color = "black", width = 1.5)
    ) |>
    flextable::set_table_properties(width = 1, layout = "autofit")

  # Alternating row shading (readability aid for wide tables)
  # header_rows parameter offsets the shading start for gtsummary multi-row headers
  if (add_row_shading && n_rows > 2) {
    shade_start <- if (header_rows > 1) 1 else 2
    even_rows <- seq(shade_start, n_rows, by = 2)
    ft <- ft |>
      flextable::bg(i = even_rows, bg = "#F5F5F5", part = "body")
  }

  # Caption formatting (journal style: above table)
  if (!is.null(caption_text)) {
    if (!is.null(table_number)) {
      # Two-chunk caption: bold "Table N." prefix + regular descriptive text
      ft <- ft |>
        flextable::set_caption(
          caption = flextable::as_paragraph(
            flextable::as_chunk(
              paste0("Table ", table_number, ". "),
              props = officer::fp_text(font.family = font_family,
                                       font.size = CAPTION_FONT_SIZE,
                                       bold = TRUE,
                                       italic = FALSE)
            ),
            flextable::as_chunk(
              caption_text,
              props = officer::fp_text(font.family = font_family,
                                       font.size = CAPTION_FONT_SIZE,
                                       bold = FALSE,
                                       italic = FALSE)
            )
          ),
          fp_p = officer::fp_par(text.align = "left", padding.bottom = 6)
        )
    } else {
      # Original single-chunk caption (backward compatible)
      ft <- ft |>
        flextable::set_caption(
          caption = flextable::as_paragraph(
            flextable::as_chunk(
              caption_text,
              props = officer::fp_text(font.family = font_family,
                                       font.size = CAPTION_FONT_SIZE,
                                       bold = FALSE,
                                       italic = FALSE)
            )
          ),
          fp_p = officer::fp_par(text.align = "left", padding.bottom = 6)
        )
    }
  }

  ft
}


# --- add_thesis_footnotes() ---------------------------------------------------
# Adapted from HEVAL5200 add_table_footnotes()
# Adaptation: accepts named list for structured footnotes (abbreviations,
# source, notes) per CHEERS-VOI reporting standard

add_thesis_footnotes <- function(ft, notes_list) {
  # notes_list: named list, e.g. list(abbrev = "...", source = "...", notes = "...")
  # Each non-NULL element becomes a separate footer line
  for (key in names(notes_list)) {
    if (!is.null(notes_list[[key]]) && nchar(notes_list[[key]]) > 0) {
      ft <- ft |>
        flextable::add_footer_lines(values = notes_list[[key]])
    }
  }

  ft <- ft |>
    flextable::fontsize(size = FOOTNOTE_FONT_SIZE, part = "footer") |>
    flextable::font(fontname = THESIS_TABLE_FONT, part = "footer") |>
    flextable::italic(part = "footer") |>
    flextable::color(color = "#555555", part = "footer") |>
    flextable::padding(padding.top = 4, padding.bottom = 2,
                       part = "footer") |>
    flextable::hline_top(
      part = "footer",
      border = officer::fp_border(color = "#CCCCCC", width = 0.5)
    )

  ft
}


# --- LaTeX export via kableExtra -----------------------------------------------
# Produces booktabs-style .tex files for \input{} in the thesis.
# Each file contains one complete LaTeX table environment: table,
# sidewaystable, or the page-breaking longtable path, as classified below.
# Captions and labels remain generator-owned.

save_as_latex_table <- function(df, table_number, caption_text,
                                caption_short = NULL,
                                caption_continued = NULL,
                                col_align = NULL,
                                footnotes = NULL,
                                output_dir = TABLE_OUTPUT_DIR,
                                fontsize_override = NULL,
                                makebox_width_override = NULL,
                                tabular_colspec_override = NULL,
                                latex_column_labels = NULL,
                                latex_header_above = NULL,
                                page_layout = "standard") {

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    warning("kableExtra not available. Skipping LaTeX export for table ",
            table_number, ".", call. = FALSE)
    return(invisible(NULL))
  }

  # Page geometry is generator-owned. Genuine one-page wide
  # tables emit direct sideways floats; generated output is never hand-patched
  # Multipage tables remain longtable at their LaTeX call sites.
  page_layout <- if (is.null(page_layout)) "standard" else page_layout
  if (!page_layout %in% c("standard", "sideways-single")) {
    stop("Unsupported page_layout: ", page_layout, call. = FALSE)
  }
  use_sideways <- identical(page_layout, "sideways-single")

  label <- paste0("table", sprintf("%02d", table_number))
  tex_filename <- paste0("thesis-table-", sprintf("%02d", table_number), ".tex")
  tex_path <- file.path(output_dir, tex_filename)

  # Default alignment: left for first column, right for rest
  if (is.null(col_align)) {
    n_cols <- ncol(df)
    col_align <- c("l", rep("r", n_cols - 1))
  }

  # Escape percent signs in captions for LaTeX
  safe_caption <- gsub("%", "\\\\%", caption_text)
  safe_caption_short <- if (is.null(caption_short)) {
    NULL
  } else {
    gsub("%", "\\\\%", caption_short)
  }

  # Uniform font size across all 14 results tables
  # 9pt = \small. Keeps 8-column tables (01, 13) within margins when
  # centered via \makebox. Consistent appearance for the examiner.
  # Per-table override via fontsize_override (e.g. structural SA needs 8/10).
  uniform_font_size <- 9

  # DECISION: EVERY table the document renders
  # carries the house style. The prior scope of ten main-paper tables left the
  # eighteen generated APPENDIX tables in the old look, which reads as a
  # document that changes style halfway through. The nine hand-written
  # latex/tables/ members already carry it, so this one predicate closes the
  # class. Gate G30 is the standing check.
  house_style <- TRUE

  # Tables with 20+ rows use longtable to allow page breaks
  use_longtable <- (nrow(df) >= 20)

  if (use_sideways && use_longtable) {
    stop("sideways-single cannot contain a longtable; use lscape at the LaTeX call site",
         call. = FALSE)
  }

  table_env <- if (use_sideways) "sidewaystable" else "table"
  table_position <- if (use_sideways) "p" else ""

  latex_df <- df
  if (!is.null(latex_column_labels)) {
    if (length(latex_column_labels) != ncol(latex_df)) {
      stop("latex_column_labels must contain one label per table column.",
           call. = FALSE)
    }
    names(latex_df) <- as.character(latex_column_labels)
  }
  if (!is.null(latex_header_above)) {
    if (!is.numeric(latex_header_above) || is.null(names(latex_header_above)) ||
        anyNA(latex_header_above) || any(latex_header_above <= 0) ||
        sum(latex_header_above) != ncol(latex_df)) {
      stop("latex_header_above must be named positive widths summing to ncol(df).",
           call. = FALSE)
    }
  }

  tex <- kableExtra::kbl(
    latex_df, format = "latex", booktabs = TRUE,
    caption = safe_caption,
    label = label,
    align = col_align,
    escape = TRUE,
    linesep = "",
    longtable = use_longtable,
    table.envir = table_env,
    position = table_position
  )

  if (!is.null(latex_header_above)) {
    tex <- if (house_style) {
      kableExtra::add_header_above(
        tex, header = latex_header_above, escape = TRUE,
        bold = TRUE, color = "white", background = "#1B2A4A")
    } else {
      kableExtra::add_header_above(
        tex, header = latex_header_above, escape = TRUE)
    }
  }

  if (use_longtable) {
    tex <- tex |>
      kableExtra::kable_styling(
        position = "center", font_size = uniform_font_size,
        latex_options = c("repeat_header")
      )
  } else if (use_sideways) {
    tex <- tex |>
      kableExtra::kable_styling(
        position = "center", font_size = uniform_font_size
      )
  } else {
    tex <- tex |>
      kableExtra::kable_styling(
        position = "center", font_size = uniform_font_size,
        latex_options = c("HOLD_position")
      )
  }

  # House style: navy header band with white bold text, alternating body tint,
  # body ink matching the hand-written appendix corpus.
  # kableExtra emits \cellcolor per cell; measured band geometry is IDENTICAL
  # to the corpus \rowcolor band (adjacent cells' \tabcolsep padding abuts),
  # so no post-processing of the emitted string is needed.
  if (house_style) {
    tex <- kableExtra::row_spec(
      tex, 0, bold = TRUE, color = "white", background = "#1B2A4A")
    tint_rows  <- seq(1, nrow(latex_df), by = 2)
    plain_rows <- setdiff(seq_len(nrow(latex_df)), tint_rows)
    if (length(tint_rows) > 0) {
      tex <- kableExtra::row_spec(tex, tint_rows,
                                  color = "#2D3748", background = "#F7F9FC")
    }
    if (length(plain_rows) > 0) {
      tex <- kableExtra::row_spec(tex, plain_rows, color = "#2D3748")
    }
  }

  if (!is.null(footnotes) && length(footnotes) > 0) {
    # Collapse named list into single footnote string
    # Order: abbreviations, notes, source (matching DOCX footer order)
    fn_parts <- character(0)
    # emit an optional rounding disclosure as its own final note
    # so rounded visible operands are not implied to reproduce unrounded ratios.
    for (key in c("abbrev", "pricebasis", "notes", "source", "rounding")) {
      if (!is.null(footnotes[[key]]) && nchar(footnotes[[key]]) > 0) {
        fn_parts <- c(fn_parts, footnotes[[key]])
      }
    }
    if (length(fn_parts) > 0) {
      tex <- tex |>
        kableExtra::footnote(general = fn_parts,
                              general_title = "",
                              threeparttable = TRUE,
                              escape = TRUE)
    }
  }

  # Post-process centering:
  # Wrap the ENTIRE threeparttable (table + footnotes) in \makebox so
  # both the tabular and footnotes are centered together symmetrically.
  # For tables without footnotes, wrap just the tabular.
  # Longtable: skip \makebox (global \LTleft/\LTright centers them).
  #
  # Per-table makebox_width_override (e.g. "0.95\\textwidth" for structural
  # SA) lets a single wide table narrow the \makebox to pull the tabular
  # further inside the page margins without changing the global default.
  tex_str <- as.character(tex)
  # Optional caption controls the List of Tables only; the body caption stays complete.
  if (!is.null(safe_caption_short) && nzchar(safe_caption_short)) {
    tex_str <- sub(
      "\\caption{",
      paste0("\\caption[", safe_caption_short, "]{"),
      tex_str,
      fixed = TRUE
    )
  }
  # The longtable continuation head repeats the whole caption, so a long caption
  # costs a page of its own at every break. caption_continued replaces the repeat
  # only, leaving the first head and the List of Tables entry byte-identical.
  if (!is.null(caption_continued) && nzchar(caption_continued)) {
    tex_str <- sub(
      paste0("\\caption[]{", safe_caption, " \\textit{(continued)}}"),
      paste0("\\caption[]{", gsub("%", "\\\\%", caption_continued),
             " \\textit{(continued)}}"),
      tex_str,
      fixed = TRUE
    )
  }
  # Caution: kbl() and footnote() run with escape = TRUE, which mangles the live
  # LaTeX constructs that notes and cells may carry. Two are sanctioned: the
  # resolving cross-reference to the specification-sensitivity table, and the
  # cite macro carrying a note's source attribution. Un-escape exactly those, so
  # neither the displayed number nor the printed year label can go stale
  # (no-stale-references rule). kbl() passes the caption argument through
  # unescaped, so a cite macro in a caption needs no repair here. Any future
  # embedded macro in a cell or note needs the same treatment.
  tex_str <- gsub("Appendix Table\\textasciitilde{}\\textbackslash{}ref\\{tab:table30\\}",
                  "Appendix Table~\\ref{tab:table30}",
                  tex_str, fixed = TRUE)
  tex_str <- gsub("Table\\textasciitilde{}\\textbackslash{}ref\\{tab:table04\\}",
                  "Table~\\ref{tab:table04}",
                  tex_str, fixed = TRUE)
  tex_str <- gsub("\\textbackslash{}parencite\\{latimer2013\\}",
                  "\\parencite{latimer2013}",
                  tex_str, fixed = TRUE)
  makebox_width <- if (!is.null(makebox_width_override) &&
                       nzchar(makebox_width_override)) {
    makebox_width_override
  } else if (use_sideways) {
    "\\linewidth"
  } else {
    "\\textwidth"
  }
  # Build the \makebox[<width>][c]{% prefix as a literal replacement.
  # Two backslash levels survive into the rendered .tex:
  # (1) R string escaping, (2) sub() backreference-safe construction.
  # We use sprintf() so the caller supplies "\\textwidth" or
  # "0.95\\textwidth" as-is (single backslash in the R string literal).
  makebox_prefix_literal <- paste0("\\makebox[", makebox_width, "][c]{%\n")
  # Escape for use inside sub() replacement string: double every backslash.
  makebox_prefix_sub_repl <- gsub("\\\\", "\\\\\\\\",
                                  makebox_prefix_literal, fixed = FALSE)
  if (!use_longtable) {
    has_tpt <- grepl("\\\\begin\\{threeparttable\\}", tex_str)
    if (has_tpt) {
      # Wrap threeparttable (includes both tabular and tablenotes)
      tex_str <- sub("(\\\\begin\\{threeparttable\\})",
                     paste0(makebox_prefix_sub_repl, "\\1"), tex_str)
      tex_str <- sub("(\\\\end\\{threeparttable\\})",
                     "\\1\n}", tex_str)
    } else {
      # No footnotes: wrap just the tabular
      tex_str <- sub("(\\\\begin\\{tabular\\})",
                     paste0(makebox_prefix_sub_repl, "\\1"), tex_str)
      tex_str <- sub("(\\\\end\\{tabular\\})",
                     "\\1\n}", tex_str)
    }
  }

  # Per-table fontsize override. Replace the single `\fontsize{9}{11}` line
  # emitted by kableExtra::kable_styling(font_size = 9) with an arbitrary
  # override (e.g. "8}{10" produces \fontsize{8}{10}). The override value
  # is a string matching the two-argument body of \fontsize{A}{B}.
  if (!is.null(fontsize_override) && nzchar(fontsize_override)) {
    tex_str <- sub("\\\\fontsize\\{[^}]+\\}\\{[^}]+\\}",
                   paste0("\\\\fontsize{", fontsize_override, "}"),
                   tex_str)
  }

  # Per-table tabular column-spec override. kableExtra's align argument
  # accepts only single-character column specs (l/c/r), so p{...} strings
  # cannot pass through kbl(align=...). Instead, the builder supplies a
  # full column-spec string (e.g. "p{4.5cm}p{4cm}p{8cm}p{3cm}p{3cm}") and
  # we substitute it into the first \begin{tabular}[t]{<spec>} occurrence.
  if (!is.null(tabular_colspec_override) && nzchar(tabular_colspec_override)) {
    # Longtables need the override too: table-04 (21 rows -> longtable path)
    # rendered 287pt off-page because this sub() only matched tabular.
    # Caution: sub() replacement strings consume literal backslashes; protect
    # LaTeX commands in an override before concatenating with the \1 backref.
    tabular_colspec_sub_repl <- gsub("\\", "\\\\",
                                     tabular_colspec_override,
                                     fixed = TRUE)
    tex_str <- sub("(\\\\begin\\{(tabular|longtable)\\}\\[t\\]\\{)[^}]+\\}",
                   paste0("\\1", tabular_colspec_sub_repl, "}"),
                   tex_str)
  }

  if (use_sideways &&
      !grepl("^\\\\begin\\{sidewaystable\\}\\[p\\]", tex_str)) {
    stop("sideways-single did not emit sidewaystable[p]", call. = FALSE)
  }

  writeLines(tex_str, tex_path)
  cat("LaTeX saved:", tex_filename, "\n")
  invisible(tex_path)
}


# --- Helper: format NOK values ------------------------------------------------
# Defensive parameter accessors with warnings (anti-hardcoding)
get_n_psa <- function() {
  if (exists("n_psa", envir = globalenv())) {
    get("n_psa", envir = globalenv())
  } else {
    warning("n_psa not in global environment; using fallback 10000. ",
            "Run model first to populate.", call. = FALSE)
    10000
  }
}

get_discount_rate <- function() {
  if (exists("discount_rate_flat", envir = globalenv())) {
    get("discount_rate_flat", envir = globalenv())
  } else {
    warning("discount_rate_flat not in global environment; using fallback 0.04. ",
            "Run 00-parameters.R first.", call. = FALSE)
    0.04
  }
}

format_nok <- function(x, digits = 0) {
  # DECISION: true minus U+2212 for rendered negatives.
  # Caution: format() pads a vector to a common width, so the sign is not always
  # the first character; anchor the substitution after the pad, not at 1.
  sub("^( *)-", "\\1−", format(round(x, digits), big.mark = ","))
}


# ==============================================================================
# TABLE BUILDERS
# Each returns list(ft = flextable, path = "filename.docx", caption = "...")
# ==============================================================================


# --- Deterministic base-case table -------------------------------------------
build_deterministic_table <- function() {
  if (!exists("base_case_results")) {
    message("08-export: Deterministic results not in memory. Skipping.")
    return(NULL)
  }

  caption <- "Supplementary deterministic results."

  # Includes: discounted QALYs, discounted LYs, discounted costs, ICER, cost/LYG.
  # GUIDELINE: DMP Section 12.3 requires cost per LYG alongside ICER
  has_ly <- "total_ly_disc" %in% names(base_case_results)

  df <- data.frame(
    Arm = c("Standard Care", "Exercise", "Incremental"),
    # DECISION: display-consistency rule. The three displayed QALY cells must
    # reproduce by hand (Exercise - Standard Care = Incremental) and agree
    # with the disaggregated table's component sums (Standard Care
    # 8.8095 + 1.4403 = 10.2498). Independent raw rounding lands the Standard
    # Care total 1 ulp low (10.2497), breaking both identities, so the
    # Standard Care cell derives from the rounded Exercise and Incremental
    # values. Ceiling: holds while Exercise raw rounding equals its component
    # sum; revisit if a future re-anchor breaks that.
    `Total QALYs (disc.)` = {
      q <- round(base_case_results$total_qalys, 4)
      q[1] <- round(q[2] - q[3], 4)
      q
    },
    `Life-Years (disc.)` = if (has_ly) {
      round(base_case_results$total_ly_disc, 2)
    } else { rep("-", 3) },
    `Total Costs (NOK, disc.)` = format_nok(base_case_results$total_costs_nok),
    `Total QALYs (undisc.)` = round(base_case_results$total_qalys_raw, 3),
    `Total Costs (NOK, undisc.)` = format_nok(base_case_results$total_costs_raw),
    `ICER (NOK/QALY)` = ifelse(
      is.na(base_case_results$icer_nok_qaly), "-",
      format_nok(base_case_results$icer_nok_qaly)
    ),
    `Cost/LYG (NOK/LY)` = if (has_ly) {
      ifelse(is.na(base_case_results$cost_per_ly), "-",
             format_nok(base_case_results$cost_per_ly))
    } else { rep("-", 3) },
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  fn <- list(
    abbrev = paste0(
      "QALY = quality-adjusted life-year; LY = life-year; ",
      "LYG = life-years gained; ICER = incremental cost-effectiveness ",
      "ratio; NOK = Norwegian kroner; disc. = discounted; ",
      "undisc. = undiscounted."
    ),
    pricebasis = PRICE_BASIS_NOTE,
    notes = paste0(
      "Cost/LYG follows DMP Section 12.3. ",
      "Ratios use unrounded operands; rounding may prevent exact reproduction."
    ),
    source = "Source: Deterministic analysis."
  )

  ft <- flextable::flextable(df) |>
    format_thesis_table(caption_text = caption, table_number = 1) |>
    flextable::align(j = 2:8, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-01-deterministic.docx",
       caption = caption, df = df, table_number = 1, footnotes = fn,
       # Caution: the 1.65cm cap predates the bold header. Bold `(NOK/QALY)` has no
       # break point and does not fit it; only the ICER column widens.
       tabular_colspec_override = paste0(
         "@{}>{\\raggedright\\arraybackslash}p{1.70cm}",
         ">{\\raggedleft\\arraybackslash}p{1.19cm}",
         ">{\\raggedleft\\arraybackslash}p{0.97cm}",
         ">{\\raggedleft\\arraybackslash}p{1.19cm}",
         ">{\\raggedleft\\arraybackslash}p{1.35cm}",
         ">{\\raggedleft\\arraybackslash}p{1.24cm}",
         ">{\\raggedleft\\arraybackslash}p{1.90cm}",
         ">{\\raggedleft\\arraybackslash}p{1.46cm}@{}"))
}


# --- PSA summary table --------------------------------------------------------
build_psa_summary_table <- function() {
  if (!exists("psa_summary")) {
    message("08-export: PSA summary not in memory. Skipping.")
    return(NULL)
  }

  caption <- paste0(
    "Probabilistic main-analysis results by strategy. ",
    "NMB, INMB and P(cost-effective) evaluated at the applicable ",
    "cost-effectiveness threshold of \\wtpThreshold{} NOK per QALY."
  )

  inb_ci <- psa_summary$inb_95ci
  df <- data.frame(
    Metric = c(
      "Expected QALYs",
      "QALYs, 95% CrI",
      "Surveillance costs (NOK)",
      "Exercise programme costs (NOK)",
      "Progressed-disease costs (NOK)",
      "Terminal-care costs (NOK)",
      "Total costs (NOK)",
      "Total costs, 95% CrI (NOK)",
      "NMB at applicable threshold (NOK)",
      "ICER (NOK/QALY)",
      "INMB 95% credible interval (NOK)",
      "P(cost-effective) at applicable threshold"
    ),
    `Standard Care` = c(
      sprintf("%.4f", psa_summary$expected_qalys_ctrl),
      paste0(sprintf("%.4f", psa_summary$qalys_ctrl_95ci[1]), " to ",
             sprintf("%.4f", psa_summary$qalys_ctrl_95ci[2])),
      format_nok(psa_summary$expected_cost_surveillance_ctrl),
      format_nok(psa_summary$expected_cost_exercise_ctrl),
      format_nok(psa_summary$expected_cost_progressed_ctrl),
      format_nok(psa_summary$expected_cost_terminal_ctrl),
      format_nok(psa_summary$expected_costs_ctrl),
      paste0(format_nok(psa_summary$costs_ctrl_95ci[1]), " to ",
             format_nok(psa_summary$costs_ctrl_95ci[2])),
      format_nok(psa_summary$expected_nmb_ctrl),
      "-", "-", "-"
    ),
    Exercise = c(
      sprintf("%.4f", psa_summary$expected_qalys_int),
      paste0(sprintf("%.4f", psa_summary$qalys_int_95ci[1]), " to ",
             sprintf("%.4f", psa_summary$qalys_int_95ci[2])),
      format_nok(psa_summary$expected_cost_surveillance_int),
      format_nok(psa_summary$expected_cost_exercise_int),
      format_nok(psa_summary$expected_cost_progressed_int),
      format_nok(psa_summary$expected_cost_terminal_int),
      format_nok(psa_summary$expected_costs_int),
      paste0(format_nok(psa_summary$costs_int_95ci[1]), " to ",
             format_nok(psa_summary$costs_int_95ci[2])),
      format_nok(psa_summary$expected_nmb_int),
      "-", "-", "-"
    ),
    Incremental = c(
      sprintf("%.4f", psa_summary$expected_inc_qaly),
      paste0(sprintf("%.4f", psa_summary$inc_qalys_95ci[1]), " to ",
             sprintf("%.4f", psa_summary$inc_qalys_95ci[2])),
      format_nok(psa_summary$expected_cost_surveillance_int -
                   psa_summary$expected_cost_surveillance_ctrl),
      format_nok(psa_summary$expected_cost_exercise_int -
                   psa_summary$expected_cost_exercise_ctrl),
      format_nok(psa_summary$expected_cost_progressed_int -
                   psa_summary$expected_cost_progressed_ctrl),
      format_nok(psa_summary$expected_cost_terminal_int -
                   psa_summary$expected_cost_terminal_ctrl),
      format_nok(psa_summary$expected_inc_cost),
      paste0(format_nok(psa_summary$inc_costs_95ci[1]), " to ",
             format_nok(psa_summary$inc_costs_95ci[2])),
      format_nok(psa_summary$expected_inb),
      format_nok(psa_summary$expected_icer),
      paste0(format_nok(inb_ci[1]), " to ", format_nok(inb_ci[2])),
      paste0(round(psa_summary$prob_ce * 100, 1), "%")
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  fn <- list(
    abbrev = paste0(
      "CrI = credible interval (2.5th to 97.5th percentile of the PA ",
      "draws); INMB = incremental NMB; NMB = net monetary benefit; ",
      "ICER = incremental cost-effectiveness ratio; ",
      "QALY = quality-adjusted life-year; ",
      "NOK = Norwegian kroner."
    ),
    pricebasis = PRICE_BASIS_NOTE,
    notes = paste0(
      "Cost components, cost totals, ICER and NMB/INMB use unrounded means; ",
      "rounding may prevent exact reproduction, including of the displayed ",
      "totals from the displayed components."
    ),
    source = "Source: Probabilistic analysis."
  )

  ft <- flextable::flextable(df) |>
    format_thesis_table(caption_text = caption, table_number = 2) |>
    flextable::align(j = 2:4, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-02-psa-summary.docx", caption = caption, df = df, table_number = 2, footnotes = fn)
}


# --- Convergence table --------------------------------------------------------
# GUIDELINE: Hatswell et al. (2018), PharmacoEconomics 36(12):1421-1426
# Primary: INMB CI method (Key Points, p.1421). Secondary: ICER stability.
# INMB CI is the primary convergence diagnostic (Hatswell et al. 2018)
table03_stability_relation <- function(difference_pct, threshold_pct) {
  stopifnot(
    is.numeric(difference_pct), length(difference_pct) == 1L,
    is.finite(difference_pct), difference_pct >= 0,
    is.numeric(threshold_pct), length(threshold_pct) == 1L,
    is.finite(threshold_pct), threshold_pct > 0
  )

  if (difference_pct < threshold_pct) {
    "below"
  } else if (difference_pct > threshold_pct) {
    "exceeding"
  } else {
    "within"
  }
}

# Amendment 4 fixtures: exercise both comparison branches and pin the live
# 0.14%-versus-1% case to truthful below-threshold wording.
local({
  stopifnot(
    identical(table03_stability_relation(0.5, 1), "below"),
    identical(table03_stability_relation(1.5, 1), "exceeding"),
    identical(table03_stability_relation(0.14, 1), "below")
  )
})

build_convergence_table <- function() {
  if (!exists("convergence") || !exists("n_psa")) {
    message("08-export: Convergence data or n_psa not in memory. Skipping.")
    return(NULL)
  }

  # --- Panel A: INMB CI convergence (primary method) ---
  inmb_tbl <- convergence$inmb_table
  first_conv <- convergence$inmb_first_converged
  wtp_used <- if (!is.null(attr(inmb_tbl, "wtp"))) {
    attr(inmb_tbl, "wtp")
  } else {
    wtp_threshold
  }

  if (!is.null(inmb_tbl)) {
    panel_a <- data.frame(
      Metric = paste0("INMB at n=", format(inmb_tbl$n, big.mark = ",")),
      Value = paste0(
        "Mean: ", format(round(inmb_tbl$mean_inb), big.mark = ","),
        " (95% CI: ", format(round(inmb_tbl$ci_lower), big.mark = ","),
        " to ", format(round(inmb_tbl$ci_upper), big.mark = ","), ")",
        ifelse(inmb_tbl$ci_excludes_zero, " *", "")
      ),
      stringsAsFactors = FALSE
    )
  } else {
    panel_a <- data.frame(
      Metric = "INMB CI convergence",
      Value = "Not computed (inmb_table missing from convergence object)",
      stringsAsFactors = FALSE
    )
  }

  # --- Panel B: ICER stability (secondary check) ---
  panel_b <- data.frame(
    Metric = c("ICER at 1,000 iterations (NOK/QALY)",
               paste0("ICER at ", format(n_psa, big.mark = ","),
                      " iterations (NOK/QALY)"),
               "Percentage difference",
               "Within 1% threshold"),
    Value = c(format_nok(convergence$icer_1k),
              format_nok(convergence$icer_10k),
              paste0(round(convergence$pct_diff, 2), "%"),
              if (convergence$within_1pct) "Yes" else "No"),
    stringsAsFactors = FALSE
  )

  # --- Combined table ---
  separator_a <- data.frame(
    Metric = "Panel A: INMB Confidence Interval Method (Primary)",
    Value = "", stringsAsFactors = FALSE)
  separator_b <- data.frame(
    Metric = "Panel B: ICER Stability Check (Secondary)",
    Value = "", stringsAsFactors = FALSE)

  df <- rbind(separator_a, panel_a, separator_b, panel_b)

  # --- Programmatic caption ---
  conv_sentence <- if (!is.null(first_conv) && !is.infinite(first_conv)) {
    paste0("The INMB 95% CI excluded zero from n=",
           format(first_conv, big.mark = ","), " iterations onward.")
  } else {
    "The INMB 95% CI did not exclude zero at any tested sample size."
  }

  stability_threshold_pct <- 1
  stability_relation <- table03_stability_relation(
    convergence$pct_diff,
    stability_threshold_pct
  )

  caption <- paste0(
    "PA Convergence Diagnostics. ",
    "Primary method: incremental net monetary benefit (INMB) confidence ",
    "interval. ",
    conv_sentence, " ",
    "Secondary check: cumulative mean ICER difference ",
    stability_relation, " the ", stability_threshold_pct,
    "% stability threshold; the primary INMB criterion governs. ",
    "Cost-effectiveness threshold: NOK ", format(wtp_used, big.mark = ","), "/QALY. ",
    "Extended healthcare perspective; 4% discount rate (Rundskriv R-109)."
  )

  fn <- list(
    notes = paste0(
      "* = 95% CI excludes zero (convergence confirmed). ",
      "ICER = incremental cost-effectiveness ratio; NOK = Norwegian kroner; ",
      "PA = probabilistic analysis; QALY = quality-adjusted life year; ",
      "CI = confidence interval. ",
      "INMB = incremental net monetary benefit = ",
      "inc. QALYs x threshold - inc. costs."
    ),
    pricebasis = PRICE_BASIS_NOTE,
    source = paste0(
      "Source: PA convergence diagnostics (R code, supplementary material). ",
      "Hatswell et al. (2018), PharmacoEconomics 36(12):1421-1426."
    )
  )

  ft <- flextable::flextable(df) |>
    format_thesis_table(caption_text = caption, table_number = 3) |>
    flextable::align(j = 2, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-03-convergence.docx", caption = caption, caption_short = "PA convergence diagnostics", df = df, table_number = 3, footnotes = fn)
}


# --- Structural SA table ------------------------------------------------------
build_structural_sa_table <- function() {
  if (file.exists(file.path(TABLE_OUTPUT_DIR, "structural_sa_results.csv"))) {
    ssa <- read.csv(file.path(TABLE_OUTPUT_DIR, "structural_sa_results.csv"),
                    stringsAsFactors = FALSE)
  } else if (exists("ssa_results")) {
    ssa <- ssa_results
  } else {
    message("08-export: Structural SA results not available. Skipping.")
    return(NULL)
  }

  caption <- "Structural sensitivity analysis."

  # Format for display (includes CE verdict per DMP WTP threshold)
  # DECISION: the CE verdict and the printed threshold share one
  # basis, each scenario's own severity-weighted threshold (sev_reference),
  # matching the ssaNcostEffective macro (F2 fork). NA guarded loudly.
  ce_flag <- ifelse(is.na(ssa$icer), "-",
                    ifelse(!is.na(ssa$sev_reference) &
                             ssa$icer <= ssa$sev_reference, "Yes", "No"))

  display_df <- data.frame(
    Scenario = ssa$description,
    `Inc. Costs (NOK)` = ifelse(is.na(ssa$inc_costs), "-",
                                 format_nok(ssa$inc_costs)),
    `Inc. QALYs` = ifelse(is.na(ssa$inc_qalys), "-",
                           sprintf("%.4f", ssa$inc_qalys)),
    `ICER (NOK/QALY)` = ifelse(is.na(ssa$icer), "-",
                                 format_nok(ssa$icer)),
    `Threshold (NOK/QALY)` = ifelse(is.na(ssa$sev_reference), "-",
                                 format_nok(ssa$sev_reference)),
    `Cost-effective` = ce_flag,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  fn <- list(
    abbrev = paste0(
      "ICER = incremental cost-effectiveness ratio; QALY = quality-adjusted life-year; ",
      "HR = hazard ratio; DFS = disease-free survival; OS = overall survival; ",
      "NOK = Norwegian kroner; BIC = Bayesian information criterion; ",
      "EQ-5D-5L = EuroQol 5-dimension 5-level; PD = progressed disease; ",
      "SSB = Statistisk sentralbyrå; OUS = Oslo University Hospital."
    ),
    pricebasis = paste0(PRICE_BASIS_NOTE, " The session-cost scenario anchors are entered at face value."),
    notes = paste0(
      "Yes = cost-effective; '-' = no reportable value, including the ",
      "mortality-floor-removal scenario, which returned an invalid result. ",
      "Ratios and INMB values use unrounded operands; the rounded operands ",
      "shown may not reproduce them exactly."),
    source = "Source: Structural sensitivity analysis."
  )

  ft <- flextable::flextable(display_df) |>
    format_thesis_table(caption_text = caption, table_number = 4) |>
    flextable::align(j = 2:6, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  # Structural SA is the widest results table (5 columns with long scenario
  # descriptions in column 1). The default 9/11 fontsize and full-textwidth
  # \makebox leave the long descriptions slightly overfull. These overrides
  # are emitted natively by save_as_latex_table() so reruns are idempotent
  # (no post-processing required).
  list(ft = ft, path = "thesis-table-04-structural-sa.docx",
       caption = caption, df = display_df, table_number = 4, footnotes = fn,
       # DECISION: NO per-table font size. The
       # 8/10 override that used to sit here made this the one main-paper table
       # set below the uniform 9. Every column below is capped, so the table's
       # WIDTH no longer depends on the font at all and the uniform size costs
       # nothing. G31 is the standing check.
       # Caution: content-driven r columns are sized by their own header strings, so
       # the house style's bold header widened this tabular past \textwidth (five
       # 8.01747pt alignment overfulls, +6.935bp past the prose bound on the
       # continuation page). Capped p columns let the headers wrap, so header
       # WEIGHT can no longer drive layout.
       # DECISION: six columns; p-sum 11.45 cm fits \textwidth
       # 398pt (13.99 cm) minus 12 x 6pt \tabcolsep (2.53 cm) = 11.46 cm.
       # Caution: a correct ROW SUM does not prove a CELL fits. The unbreakable
       # header token "(NOK/QALY)" needs 51.894pt in this document (1.40 cm =
       # 39.834pt plus the 12.06017pt overfull it produced there) and 53.38477pt
       # by standalone \settowidth at Charter bold 9pt. Threshold takes 1.90 cm
       # (54.06pt), which clears both readings; the width comes off Scenario,
       # raggedright prose whose longest token is far narrower than 4.05 cm.
       tabular_colspec_override = paste0(
         ">{\\raggedright\\arraybackslash}p{4.05cm}",
         ">{\\raggedleft\\arraybackslash}p{1.45cm}",
         ">{\\raggedleft\\arraybackslash}p{1.00cm}",
         ">{\\raggedleft\\arraybackslash}p{1.85cm}",
         ">{\\raggedleft\\arraybackslash}p{1.90cm}",
         ">{\\raggedleft\\arraybackslash}p{1.20cm}"))
}


# --- POWSA table --------------------------------------------------------------
build_owpsa_table <- function() {
  if (exists("all_results")) {
    owpsa <- all_results
  } else if (file.exists(file.path(TABLE_OUTPUT_DIR, "owpsa_v7_results.csv"))) {
    message("08-export: OWPSA loaded from CSV (may be stale if parameters changed). ",
            "Re-run main.R (step 8b) to refresh.")
    owpsa <- read.csv(file.path(TABLE_OUTPUT_DIR, "owpsa_v7_results.csv"),
                      stringsAsFactors = FALSE)
  } else {
    message("08-export: OWPSA results not available. Skipping.")
    return(NULL)
  }

  # SOURCE: INMB and P(CE) are both threshold-defined, and NOK is price-year
  # defined; the macro keeps the printed threshold tied to the model run rather
  # than freezing a literal the next run could invalidate.
  caption <- paste0(
    "Probabilistic one-way sensitivity analysis. ",
    "INMB and P(CE) evaluated at the applicable cost-effectiveness threshold ",
    "of \\wtpThreshold{} NOK per QALY; costs in 2024 NOK."
  )

  label_stems <- c(
    HR_DFS = "DFS HR",
    HR_OS = "OS HR",
    c_exercise_annual = "Exercise cost",
    u_dfs = "Disease-free utility",
    c_progressed_annual = "Progressed-disease cost",
    u_prog = "Progressed utility",
    c_terminal = "End-of-life cost",
    c_surveillance_early = "Surveillance cost"
  )
  input_params <- as.character(owpsa$param_name)
  if (anyNA(input_params) || anyDuplicated(input_params) ||
      !setequal(input_params, names(label_stems)) ||
      length(input_params) != length(label_stems)) {
    stop("build_owpsa_table: parameter display map must match each input exactly once.")
  }
  decimal_params <- input_params %in% c("HR_DFS", "HR_OS", "u_dfs", "u_prog")
  display_ranges <- character(length(input_params))
  display_ranges[decimal_params] <- paste0(
    formatC(as.numeric(owpsa$value_low[decimal_params]),
            format = "f", digits = 2),
    "-",
    formatC(as.numeric(owpsa$value_high[decimal_params]),
            format = "f", digits = 2)
  )
  cost_matches <- regmatches(
    owpsa$label[!decimal_params],
    regexec("\\(([0-9.]+%-[0-9.]+%)\\)", owpsa$label[!decimal_params])
  )
  if (any(lengths(cost_matches) != 2L)) {
    stop("build_owpsa_table: cost labels must contain a registered percentage range.")
  }
  display_ranges[!decimal_params] <- vapply(
    cost_matches, function(x) x[[2L]], character(1L))
  display_labels <- paste0(
    unname(label_stems[input_params]), " (", display_ranges, ")")
  if (anyDuplicated(display_labels)) {
    stop("build_owpsa_table: duplicate display labels are not allowed.")
  }

  display_df <- data.frame(
    Parameter = display_labels,
    `INMB Low (NOK)` = format_nok(owpsa$inmb_low),
    `INMB High (NOK)` = format_nok(owpsa$inmb_high),
    `Range (NOK)` = format_nok(owpsa$range),
    # DECISION: fixed one-decimal formatting so integer probabilities render
    # "100.0%" / "99.0%" consistent with the one-decimal cells in the same
    # columns (round() alone drops trailing zeros and mixes formats).
    `P(CE) Low` = paste0(
      formatC(round(owpsa$prob_ce_low * 100, 1), format = "f", digits = 1), "%"),
    `P(CE) High` = paste0(
      formatC(round(owpsa$prob_ce_high * 100, 1), format = "f", digits = 1), "%"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  fn <- list(
    abbrev = "INMB = incremental net monetary benefit; P(CE) = probability cost-effective; NOK = Norwegian kroner; QALY = quality-adjusted life year; DFS = disease-free survival; OS = overall survival; HR = hazard ratio.",
    notes = "Derived values use unrounded operands; rounding may prevent exact reproduction.",
    source = "Source: POWSA (main.R step 8b)."
  )

  ft <- flextable::flextable(display_df) |>
    flextable::set_header_labels(values = c(
      Parameter = "Parameter",
      `INMB Low (NOK)` = "Low",
      `INMB High (NOK)` = "High",
      `Range (NOK)` = "Range",
      `P(CE) Low` = "Low",
      `P(CE) High` = "High"
    )) |>
    flextable::add_header_row(
      values = c("", "INMB (NOK)", "P(CE)"),
      colwidths = c(1, 3, 2), top = TRUE) |>
    format_thesis_table(caption_text = caption, table_number = 5,
                        header_rows = 2) |>
    flextable::align(j = 2:6, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(
    ft = ft, path = "thesis-table-05-powsa.docx", caption = caption,
    df = display_df, table_number = 5, footnotes = fn,
    latex_column_labels = c("Parameter", "Low", "High", "Range", "Low", "High"),
    latex_header_above = c(" " = 1, "INMB (NOK)" = 3, "P(CE)" = 2)
  )
}


# --- EVPI table ---------------------------------------------------------------
build_evpi_table <- function() {
  evpi_path <- "data/processed/evpi_results.rds"
  if (!file.exists(evpi_path)) {
    message("08-export: EVPI results not available. Skipping.")
    return(NULL)
  }

  evpi <- readRDS(evpi_path)

  ref_wtp <- if (exists("wtp_threshold")) wtp_threshold else opportunity_cost_nok
  nearest_idx <- which.min(abs(evpi$wtp - ref_wtp))
  nearest_wtp <- evpi$wtp[nearest_idx]
  evpi_at_ref <- evpi$evpi[nearest_idx]

  caption <- paste0(
    "Expected value of perfect information at selected ",
    "cost-effectiveness thresholds. ",
    "Panel A names its own threshold on every row; Panel B is evaluated at ",
    "the applicable cost-effectiveness threshold of \\wtpThreshold{} NOK ",
    "per QALY."
  )

  # The 275,000 NOK/QALY row sits alongside the existing thresholds. 275,000 lies
  #   on the EVPI compute grid seq(50000, 825000, 25000) built in R/06-psa.R, so it
  #   maps to an exact grid point and its EVPI is recomputed from evpi_results.rds
  #   (never hardcoded).
  # The vector below is the selected set of EVPI display thresholds.
  display_wtps <- c(100000, 200000, 275000, 300000, 495000, 600000, 800000)

  grid_wtps <- sapply(display_wtps, function(w) evpi$wtp[which.min(abs(evpi$wtp - w))])
  grid_evpis <- sapply(display_wtps, function(w) evpi$evpi[which.min(abs(evpi$wtp - w))])

  # --- Panel A: Per-Patient EVPI at WTP Thresholds ---
  panel_a_header <- data.frame(
    Metric = "Panel A: Per-patient EVPI at selected cost-effectiveness thresholds",
    Value = "", stringsAsFactors = FALSE)
  panel_a <- data.frame(
    Metric = paste0("EVPI at threshold = NOK ", format_nok(grid_wtps), "/QALY"),
    Value = paste0("NOK ", format_nok(grid_evpis)),
    stringsAsFactors = FALSE
  )

  # --- Panel B: Population EVPI (Discounted Technology Horizon) ---
  # GUIDELINE: Briggs, Claxton, Sculpher (2006), p.176, Section 6.2.1
  # GUIDELINE: Fenwick et al. (2020), p.144-145, GPR 3, Box 4
  # Modelled annual operated stage-mix cohort size = 845. Technology horizon T = 10 years (base case).
  annual_patients <- ANNUAL_OPERATED_STAGE_MIX_COHORT
  r_disc <- get_discount_rate()
  # GUIDELINE: Justify the technology horizon and test alternatives (Fenwick et al. 2020)
  horizons <- c(5, 10, 20)
  # GUIDELINE: Discount future eligible cohorts in population EVPI (Briggs et al. 2006)
  annuity_factors <- (1 - (1 + r_disc)^(-horizons)) / r_disc
  disp_evpi <- round(evpi_at_ref)
  disp_af <- round(annuity_factors, 4)
  # Derive reader-facing population totals from the rounded displayed operands.
  pop_evpis <- disp_evpi * annual_patients * disp_af
  single_cohort <- disp_evpi * annual_patients
  # population totals derive from the rounded displayed operands so a reader
  # reproduces them by hand from the printed per-patient EVPI, cohort, and annuity factor.

  panel_b_header <- data.frame(
    Metric = "Panel B: Population EVPI",
    Value = "", stringsAsFactors = FALSE)
  panel_b <- data.frame(
    Metric = c(
      "Modelled annual operated stage-mix cohort size",
      "Per-patient EVPI at applicable threshold",
      "Single annual cohort (undiscounted)",
      paste0("T=", horizons, " years (discounted at ", r_disc * 100, "%)")
    ),
    Value = c(
      format(annual_patients, big.mark = ","),
      paste0("NOK ", format_nok(evpi_at_ref)),
      paste0("NOK ", format(round(single_cohort), big.mark = ",")),
      paste0("NOK ", format(round(pop_evpis), big.mark = ","),
             " (annuity factor ", round(annuity_factors, 4), ")")
    ),
    stringsAsFactors = FALSE
  )

  display_df <- rbind(panel_a_header, panel_a, panel_b_header, panel_b)

  fn <- list(
    abbrev = paste0(
      "EVPI = expected value of perfect information; NMB = net monetary benefit; ",
      "QALY = quality-adjusted life-year; NOK = Norwegian kroner; ",
      "T = decision horizon."
    ),
    pricebasis = PRICE_BASIS_NOTE,
    notes = "Population EVPI = per-patient EVPI × modelled annual cohort × discounted annuity factor.",
    source = "Source: Value-of-information analysis."
  )

  ft <- flextable::flextable(display_df) |>
    format_thesis_table(caption_text = caption, table_number = 6) |>
    flextable::align(j = 2, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-06-evpi.docx", caption = caption, caption_short = "EVPI by threshold", df = display_df, table_number = 6, footnotes = fn,
       tabular_colspec_override = ">{\\raggedright\\arraybackslash}p{6.7cm}r")
}


# --- EVPPI table --------------------------------------------------------------
build_evppi_table <- function() {
  evppi_path <- "data/processed/evppi_results.rds"
  if (!file.exists(evppi_path)) {
    message("08-export: EVPPI results not available. Skipping.")
    return(NULL)
  }

  evppi <- readRDS(evppi_path)

  caption <- paste0(
    "Expected value of partial perfect information by parameter. ",
    "EVPPI and EVPI evaluated at the applicable cost-effectiveness threshold ",
    "of \\wtpThreshold{} NOK per QALY."
  )

  display_evppi <- evppi[round(evppi$evppi) != 0, , drop = FALSE]

  # WHY (display-consistency rule): percent-of-EVPI cells are computed FROM
  # THE ROUNDED DISPLAYED OPERANDS (the EVPPI and EVPI integers as printed),
  # so a reader can reproduce each cell by hand from the printed numbers;
  # the stored pct_evpi column uses unrounded operands and can differ by 0.1.
  display_df <- data.frame(
    Parameter = ifelse(display_evppi$parameter %in% names(param_display_labels),
                       unname(param_display_labels[display_evppi$parameter]),
                       display_evppi$parameter),
    `EVPPI (NOK)` = format_nok(display_evppi$evppi),
    `% of EVPI` = paste0(
      # Caution: round() drops a trailing zero, so a whole-tenth share printed as
      #   "59%" beside its sibling "45.3%" and against the prose macro "59.0%".
      #   formatC holds one decimal for every cell in this column.
      formatC(round(100 * round(display_evppi$evppi) / round(attr(evppi, "evpi")), 1),
              format = "f", digits = 1), "%"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  fn <- list(
    abbrev = "EVPPI = expected value of partial perfect information; EVPI = expected value of perfect information; NOK = Norwegian kroner; DFS = disease-free survival; OS = overall survival; QALY = quality-adjusted life year.",
    pricebasis = PRICE_BASIS_NOTE,
    notes = "The joint HR row is a secondary combination of the individual HR estimands.",
    source = "Source: Probabilistic value-of-information analysis."
  )

  ft <- flextable::flextable(display_df) |>
    format_thesis_table(caption_text = caption, table_number = 7) |>
    flextable::align(j = 2:3, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-07-evppi.docx", caption = caption, df = display_df, table_number = 7, footnotes = fn)
}


# --- Validation table ---------------------------------------------------------
# Internal validation: trace properties, endpoint reproduction,
# and pseudo-IPD HR reconstruction.
# GUIDELINE: AdViSHE C3 (trace testing), D4 (empirical validation)
build_validation_table <- function() {
  val_path <- "data/processed/validation_checks.rds"
  if (!file.exists(val_path)) {
    message("08-export: Validation checks not available. Skipping.")
    return(NULL)
  }

  val <- readRDS(val_path)

  # --- Panel A: PSM Trace Properties ---
  panel_a_header <- data.frame(
    Check = "Panel A: PSM Trace Properties (AdViSHE C3)",
    Result = "", stringsAsFactors = FALSE)
  panel_a <- data.frame(
    Check = c(
      "DFS monotonic (Standard Care)",
      "OS monotonic (Standard Care)",
      "Dead monotonic (Standard Care)",
      "DFS monotonic (Exercise)",
      "OS monotonic (Exercise)",
      "Dead monotonic (Exercise)",
      "State sum range (should be ~1)",
      "Minimum PD proportion"
    ),
    Result = c(
      val$dfs_monotonic_standard_care,
      val$os_monotonic_standard_care,
      val$dead_monotonic_standard_care,
      val$dfs_monotonic_exercise,
      val$os_monotonic_exercise,
      val$dead_monotonic_exercise,
      paste0("[", round(val$state_sum_range[1], 6), ", ",
             round(val$state_sum_range[2], 6), "]"),
      round(val$min_pd, 6)
    ),
    stringsAsFactors = FALSE
  )

  # --- Panel B: 5-Year DFS Endpoint Reproduction (AdViSHE D4) ---
  panel_b_header <- data.frame(
    Check = "Panel B: 5-Year DFS Endpoint Reproduction (AdViSHE D4)",
    Result = "", stringsAsFactors = FALSE)

  has_5yr <- !is.null(val$dfs_5yr_standard_care)
  if (has_5yr) {
    gap_ctrl <- round(val$dfs_5yr_standard_care - val$dfs_5yr_published_standard_care, 1)
    gap_int  <- round(val$dfs_5yr_exercise - val$dfs_5yr_published_exercise, 1)
    panel_b <- data.frame(
      Check = c(
        "5yr DFS Standard Care (model)",
        "5yr DFS Standard Care (published, Courneya 2025)",
        "Gap (percentage points)",
        "5yr DFS Exercise (model)",
        "5yr DFS Exercise (published, Courneya 2025)",
        "Gap (percentage points)"
      ),
      Result = c(
        paste0(val$dfs_5yr_standard_care, "%"),
        paste0(val$dfs_5yr_published_standard_care, "%"),
        paste0(gap_ctrl, " pp"),
        paste0(val$dfs_5yr_exercise, "%"),
        paste0(val$dfs_5yr_published_exercise, "%"),
        paste0(gap_int, " pp")
      ),
      stringsAsFactors = FALSE
    )
  } else {
    panel_b <- data.frame(
      Check = "5yr DFS predictions",
      Result = "Not available (requires PSM trace with cycle_length)",
      stringsAsFactors = FALSE
    )
  }

  # --- Panel C: Pseudo-IPD HR Reconstruction (AdViSHE D4) ---
  panel_c_header <- data.frame(
    Check = "Panel C: Pseudo-IPD HR Reconstruction (AdViSHE D4)",
    Result = "", stringsAsFactors = FALSE)

  hr_csv_path <- "output/tables/ipd_reconstruction_validation.csv"
  if (file.exists(hr_csv_path)) {
    hr_val <- read.csv(hr_csv_path, stringsAsFactors = FALSE)
    panel_c_rows <- do.call(rbind, lapply(seq_len(nrow(hr_val)), function(i) {
      r <- hr_val[i, ]
      data.frame(
        Check = c(
          paste0(r$endpoint, " HR (published)"),
          paste0(r$endpoint, " HR (reconstructed)"),
          paste0(r$endpoint, " 95% CI"),
          paste0(r$endpoint, " absolute difference"),
          paste0(r$endpoint, " within +/-0.05")
        ),
        Result = c(
          as.character(r$published_hr),
          as.character(r$reconstructed_hr),
          paste0("[", r$ci_lower, ", ", r$ci_upper, "]"),
          as.character(r$abs_difference),
          if (r$within_005) "Yes" else "No"
        ),
        stringsAsFactors = FALSE
      )
    }))
    panel_c <- panel_c_rows
  } else {
    panel_c <- data.frame(
      Check = "HR reconstruction validation",
      Result = "Not available (run 01-digitize-km.R first)",
      stringsAsFactors = FALSE
    )
  }

  # --- Panel D: Model Selection (AIC/BIC Comparison) ---
  panel_d_header <- data.frame(
    Check = "Panel D: Survival Distribution Selection (AdViSHE B2)",
    Result = "", stringsAsFactors = FALSE)

  aic_bic_path <- "output/tables/aic_bic_comparison.csv"
  if (file.exists(aic_bic_path)) {
    aic_bic <- read.csv(aic_bic_path, stringsAsFactors = FALSE)
    # DECISION: the CSV arrives AIC-ordered but the panel prints
    # only BIC; sort each endpoint-arm block ascending on the printed BIC so
    # the visible order matches the visible column. Block order preserved.
    blk <- match(paste(aic_bic$endpoint, aic_bic$arm),
                 unique(paste(aic_bic$endpoint, aic_bic$arm)))
    aic_bic <- aic_bic[order(blk, aic_bic$BIC), ]
    # Show BIC for all endpoint x arm fits (DFS / Standard Care is the primary selection criterion)
    panel_d <- data.frame(
      Check = paste0(aic_bic$endpoint, ", ", aic_bic$arm, ": ", aic_bic$distribution, " (BIC)"),
      Result = format(round(aic_bic$BIC), big.mark = ","),
      stringsAsFactors = FALSE
    )
  } else {
    panel_d <- data.frame(
      Check = "AIC/BIC comparison",
      Result = "Not available (run 02-fit-survival.R first)",
      stringsAsFactors = FALSE
    )
  }

  # --- Combined table ---
  df <- rbind(panel_a_header, panel_a,
              panel_b_header, panel_b,
              panel_c_header, panel_c,
              panel_d_header, panel_d)

  # --- Programmatic caption ---
  caption_parts <- c(
    "Internal Validation Summary. ",
    "Panel A: PSM trace properties (state occupancy summation, monotonicity). "
  )
  if (has_5yr) {
    caption_parts <- c(caption_parts,
      "Panel B: 5-year DFS endpoint reproduction, model versus published. ")
  }
  caption_parts <- c(caption_parts,
    "Panel C: Pseudo-IPD hazard ratio reconstruction via Cox PH. ",
    "Panel D: Parametric survival model fit by BIC.")
  caption <- paste0(caption_parts, collapse = "")

  fn <- list(
    abbrev = paste0(
      "DFS = disease-free survival; OS = overall survival; ",
      "PD = progressed disease; PSM = partitioned survival model; ",
      "HR = hazard ratio; CI = confidence interval; ",
      "pp = percentage points; AdViSHE = Assessment of the Validation Status of ",
      "Health-Economic decision models; BIC = Bayesian information criterion; ",
      "PH = proportional hazards."
    ),
    notes = paste0(
      "Monotonicity: TRUE = state proportion never increases ",
      "(DFS, OS) or never decreases (Dead) over time. ",
      "5-year DFS gaps within 1 percentage point, the tolerance adopted ",
      "here for parametric extrapolation of reconstructed pseudo-IPD ",
      "(Guyot et al., 2012)."
    ),
    source = paste0(
      "Source: PSM trace validation and HR reconstruction ",
      "(R code, supplementary material); Courneya et al. (2025)."
    )
  )

  ft <- flextable::flextable(df) |>
    format_thesis_table(caption_text = caption, table_number = 8) |>
    flextable::align(j = 2, align = "center", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-08-validation.docx", caption = caption, df = df, table_number = 8, footnotes = fn,
       caption_continued = "Internal Validation Summary.")
}


# --- Consolidated CEAC + CEAF by WTP table ------------------------------------
build_ceac_ceaf_wtp_table <- function() {
  # Consolidated CEAC/CEAF-by-WTP strategy table; it replaces the
  #   margin-overflowing CEAF figure area with a table. Not yet referenced by the
  #   thesis LaTeX; produced here as thesis-table-29.tex.
  if (!exists("psa_results")) {
    message("08-export: PSA results not in memory for CEAC/CEAF table. Skipping.")
    return(NULL)
  }

  # SOURCE: the six Magnussen severity thresholds at R/00-parameters.R
  #   reference_nok = c(275000, 385000, 495000, 605000, 715000, 825000).
  # DECISION: emit ALL SIX rungs exactly, so the Results claim of
  #   cost-effectiveness "across all six Norwegian severity categories" is
  #   demonstrated by the table that sentence cites. The round grid values
  #   600,000 and 800,000 previously stood in for categories 4 and 5 and are
  #   dropped: no prose under latex/chapters/ cites either.
  # Caution: 100,000 / 200,000 / 300,000 are display-grid context, not severity
  #   rungs; the labelled frontier still resolves on the true 385,000 row.
  # SOURCE: Magnussen et al. 2015, Table 3, p. 48; display-grid context
  wtp_display <- c(100000, 200000, 275000, 300000, 385000, 495000, 605000,
                   715000, 825000)

  cc <- compute_ceac_ceaf_by_wtp(psa_results$inc_costs, psa_results$inc_qalys,
                                 wtp_display)

  wtp_ref <- if (exists("wtp_threshold")) wtp_threshold else opportunity_cost_nok
  frontier_at_ref <- cc$frontier[which.min(abs(cc$wtp - wtp_ref))]

  caption <- "Cost-effectiveness acceptability curve and frontier by threshold."

  display_df <- data.frame(
    `Threshold (NOK/QALY)` = format_nok(cc$wtp),
    `P(CE), exercise`     = paste0(round(cc$prob_ce * 100, 1), "%"),
    `Frontier strategy`   = cc$frontier,
    `Expected INMB (NOK)` = format_nok(round(cc$expected_inmb)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  fn <- list(
    abbrev = paste0(
      "QALY = quality-adjusted life-year; NOK = Norwegian kroner; ",
      "CEAC = cost-effectiveness acceptability curve; ",
      "CEAF = cost-effectiveness acceptability frontier; ",
      "P(CE) = probability cost-effective; ",
      "INMB = incremental net monetary benefit."
    ),
    pricebasis = PRICE_BASIS_NOTE,
    source = "Source: Probabilistic analysis."
  )

  list(df = display_df, table_number = 29, caption = caption,
       footnotes = fn, col_align = c("r", "r", "l", "r"))
}


# --- Table 31: Selected survival-model coefficients ---------------------------
build_survival_coefficients_table <- function(
    dfs_path = "data/processed/survival_fits_dfs.rds",
    os_path = "data/processed/survival_fits_os.rds") {
  if (!file.exists(dfs_path) || !file.exists(os_path)) {
    stop("build_survival_coefficients_table: required survival-fit RDS is missing")
  }

  extract_lnorm <- function(path, endpoint) {
    fit_bundle <- readRDS(path)
    if (!is.list(fit_bundle) ||
        !identical(names(fit_bundle), c("control", "intervention"))) {
      stop(endpoint, ": survival-fit RDS must contain control and intervention")
    }
    if (!is.list(fit_bundle$control) ||
        !"lnorm" %in% names(fit_bundle$control)) {
      stop(endpoint, ": control log-normal fit is missing")
    }
    res_t <- fit_bundle$control$lnorm$res.t
    res_n <- fit_bundle$control$lnorm$res
    if (!is.matrix(res_t) || !is.matrix(res_n) ||
        !all(c("est", "se") %in% colnames(res_t)) ||
        !all(c("est", "L95%", "U95%") %in% colnames(res_n)) ||
        !identical(rownames(res_t), c("meanlog", "sdlog")) ||
        !identical(rownames(res_n), c("meanlog", "sdlog"))) {
      stop(endpoint, ": log-normal res.t and res must contain matched meanlog/sdlog estimates")
    }
    if (anyNA(res_t[, c("est", "se"), drop = FALSE]) ||
        any(!is.finite(res_t[, c("est", "se"), drop = FALSE])) ||
        anyNA(res_n[, c("est", "L95%", "U95%"), drop = FALSE]) ||
        any(!is.finite(res_n[, c("est", "L95%", "U95%"), drop = FALSE]))) {
      stop(endpoint, ": log-normal coefficient estimates or SEs are invalid")
    }

    data.frame(
      Endpoint = endpoint,
      Distribution = "Log-normal",
      Parameter = rownames(res_t),
      `Estimate (SE)` = sprintf("%.4f (%.4f)",
                                unname(res_t[, "est"]), unname(res_t[, "se"])),
      `Fitted value (95% CI)` = sprintf("%.4f (%.4f to %.4f)",
                                        unname(res_n[, "est"]),
                                        unname(res_n[, "L95%"]),
                                        unname(res_n[, "U95%"])),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  df <- rbind(
    extract_lnorm(dfs_path, "DFS"),
    extract_lnorm(os_path, "OS")
  )

  caption <- paste0(
    "Control-arm log-normal survival coefficients, on the flexsurv ",
    "estimation scale and as fitted parameter values."
  )
  fn <- list(
    abbrev = paste0(
      "DFS = disease-free survival; OS = overall survival; ",
      "SE = standard error; CI = confidence interval."
    ),
    notes = paste0(
      "The log-normal is estimated with two parameters, meanlog and sdlog, ",
      "the mean and standard deviation of log survival time; it has no ",
      "separate shape parameter, and sdlog is its scale. Estimate (SE) is ",
      "reported on the flexsurv estimation scale, on which sdlog is ",
      "log-transformed; Fitted value (95% CI) is the same parameter on its ",
      "own scale. Values are rounded to four decimal places."
    ),
    source = paste0(
      "Source: Fitted control-arm survival objects generated by ",
      "R/02-fit-survival.R."
    )
  )

  list(
    df = df,
    table_number = 31,
    caption = caption,
    footnotes = fn,
    col_align = c("l", "l", "l", "r", "r")
  )
}


# --- Table 30: PSA specification sensitivity --------------------------------
build_psa_specification_sensitivity_table <- function() {
  spec_path <- "data/processed/psa_specification_sensitivity.rds"
  spec <- if (exists("psa_specification_sensitivity")) {
    psa_specification_sensitivity
  } else if (file.exists(spec_path)) {
    stored <- readRDS(spec_path)
    if (is.list(stored) && !is.null(stored$scenario_summaries)) {
      stored$scenario_summaries
    } else {
      stored
    }
  } else {
    stop("build_psa_specification_sensitivity_table: required RDS is missing")
  }

  expected_ids <- c(
    "reference_rho0_sampling_only", "dependence_rho03",
    "dependence_rho05", "dependence_rho07", "dependence_rho09",
    "mapping_rmse_quadrature_rho0")
  required <- c(
    "scenario_id", "scenario_label", "rho_endpoint_latent",
    "mean_incremental_cost", "mean_incremental_qaly",
    "icer_ratio_of_means", "probability_cost_effective",
    "sd_incremental_cost", "sd_incremental_qaly",
    "cor_incremental_cost_qaly", "primary_convergence_n",
    "evpi_per_patient", "evppi_joint_hr")
  if (!all(required %in% names(spec)) ||
      !identical(as.character(spec$scenario_id), expected_ids)) {
    stop("build_psa_specification_sensitivity_table: invalid registry or schema")
  }

  panel_a <- data.frame(
    Scenario = spec$scenario_label,
    `rho / mean delta cost` = format(spec$rho_endpoint_latent, nsmall = 1),
    `ICER / mean delta QALY` = format_nok(spec$icer_ratio_of_means),
    `P(CE) / SD delta cost` = paste0(
      formatC(round(spec$probability_cost_effective * 100, 1),
              format = "f", digits = 1), "%"),
    `EVPI / SD delta QALY` = format_nok(spec$evpi_per_patient),
    `Joint-HR EVPPI / cost-QALY correlation` =
      format_nok(spec$evppi_joint_hr),
    `Primary convergence N` = format(spec$primary_convergence_n,
                                     big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE)
  panel_b <- data.frame(
    Scenario = spec$scenario_label,
    `rho / mean delta cost` = format_nok(spec$mean_incremental_cost),
    `ICER / mean delta QALY` = sprintf("%.4f", spec$mean_incremental_qaly),
    `P(CE) / SD delta cost` = format_nok(spec$sd_incremental_cost),
    `EVPI / SD delta QALY` = sprintf("%.4f", spec$sd_incremental_qaly),
    `Joint-HR EVPPI / cost-QALY correlation` =
      sprintf("%.3f", spec$cor_incremental_cost_qaly),
    `Primary convergence N` = format(spec$primary_convergence_n,
                                     big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE)
  panel_header <- function(label, columns) {
    row <- as.data.frame(as.list(rep("", 7L)), stringsAsFactors = FALSE)
    names(row) <- columns
    row[[1]] <- label
    row
  }
  display_df <- rbind(
    panel_header("Panel A: decision and value-of-information results",
                 names(panel_a)),
    panel_a,
    panel_header("Panel B: incremental-outcome moments, correlation, and convergence",
                 names(panel_b)),
    panel_b)

  ssa_path <- file.path(TABLE_OUTPUT_DIR, "structural_sa_results.csv")
  if (!file.exists(ssa_path)) {
    stop("build_psa_specification_sensitivity_table: structural_sa_results.csv is missing")
  }
  n_structural_nonbase <- sum(
    read.csv(ssa_path, stringsAsFactors = FALSE)$scenario != "base_case")
  if (!is.finite(n_structural_nonbase) || n_structural_nonbase < 1L) {
    stop("build_psa_specification_sensitivity_table: non-base scenario count is invalid")
  }

  caption <- paste0(
    "PA specification sensitivity (one reference plus five sensitivity ",
    # Caution: this count is DERIVED from the non-base rows of
    # structural_sa_results.csv at generation time. A frozen literal here read 23
    # against a measured 24 and against the thesis's own two other statements of
    # the same count. It is caption text, excluded from the counted BODY metric.
    "specifications; separate from the ", n_structural_nonbase,
    " deterministic structural scenarios). ",
    "Panel A reports decision and value-of-information results; Panel B ",
    "reports incremental-outcome moments, correlation, and convergence. ",
    "P(CE), EVPI and Joint-HR EVPPI evaluated at the applicable ",
    "cost-effectiveness threshold of \\wtpThreshold{} NOK per QALY.")
  fn <- list(
    abbrev = paste0(
      "PA = probabilistic analysis; ICER = incremental ",
      "cost-effectiveness ratio; P(CE) = probability cost-effective; ",
      "EVPI = expected value of perfect information; EVPPI = expected value ",
      "of partial perfect information; QALY = quality-adjusted life year; ",
      "RMSE = root mean squared error; SD = standard deviation; ",
      "NOK = Norwegian kroner."),
    pricebasis = PRICE_BASIS_NOTE,
    notes = paste0(
      "Reference rho=0 denotes the explicit DFS-OS cross-endpoint independence ",
      "assumption. Positive rho values apply Gaussian-copula rank coupling. ",
      "All rows use common random numbers. ",
      "Delta denotes exercise minus standard care."),
    source = "Source: PA specification sensitivity (R code, supplementary material).")

  ft <- flextable::flextable(display_df) |>
    format_thesis_table(caption_text = caption, table_number = 30) |>
    flextable::align(j = 2:7, align = "right", part = "body") |>
    flextable::bold(i = c(1, nrow(panel_a) + 2L), part = "body") |>
    add_thesis_footnotes(fn)

  list(
    ft = ft,
    path = "thesis-table-30-psa-specification-sensitivity.docx",
    caption = caption, df = display_df, table_number = 30,
    footnotes = fn, col_align = c("l", rep("r", 6L)),
    # DECISION: no per-table font size. This
    # sideways table sets inside \textheight, so the uniform 9 fits its
    # existing capped columns unchanged.
    tabular_colspec_override = paste0(
      ">{\\raggedright\\arraybackslash}p{4.0cm}",
      ">{\\raggedleft\\arraybackslash}p{2.0cm}",
      ">{\\raggedleft\\arraybackslash}p{2.0cm}",
      ">{\\raggedleft\\arraybackslash}p{2.0cm}",
      ">{\\raggedleft\\arraybackslash}p{2.0cm}",
      ">{\\raggedleft\\arraybackslash}p{2.3cm}",
      ">{\\raggedleft\\arraybackslash}p{1.8cm}"),
    page_layout = "sideways-single")
}


# --- Subgroup SA table --------------------------------------------------------
build_subgroup_sa_table <- function() {
  # Subgroup scenarios are embedded in structural SA (Dimensions A-E)
  if (file.exists(file.path(TABLE_OUTPUT_DIR, "structural_sa_results.csv"))) {
    ssa <- read.csv(file.path(TABLE_OUTPUT_DIR, "structural_sa_results.csv"),
                    stringsAsFactors = FALSE)
  } else if (exists("ssa_results")) {
    ssa <- ssa_results
  } else {
    message("08-export: Structural SA results not available for subgroup table. Skipping.")
    return(NULL)
  }

  subgroup_names <- c("stage_ii_only", "stage_iii_only", "age_50", "age_74",
                      "male_only", "female_only", "full_adherence",
                      "low_adherence", "utility_devlin")
  sub <- ssa[ssa$scenario %in% subgroup_names, ]

  if (nrow(sub) == 0) {
    message("08-export: No subgroup scenarios found in structural SA. Skipping.")
    return(NULL)
  }

  n_executed <- sum(!is.na(sub$icer))
  caption <- "Subgroup sensitivity analysis."

  display_df <- data.frame(
    Subgroup = sub$description,
    `Inc. Costs (NOK)` = ifelse(is.na(sub$inc_costs), "-",
                                 format_nok(sub$inc_costs)),
    `Inc. QALYs` = ifelse(is.na(sub$inc_qalys), "-",
                           sprintf("%.4f", sub$inc_qalys)),
    `ICER (NOK/QALY)` = ifelse(is.na(sub$icer), "-",
                                 format_nok(sub$icer)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  fn <- list(
    abbrev = "ICER = incremental cost-effectiveness ratio; QALY = quality-adjusted life year; NOK = Norwegian kroner.",
    notes = "Subgroups defined per thesis research question 3. '-' = scenario did not execute.",
    source = "Source: Structural sensitivity analysis subgroup dimensions A-E (R code, supplementary material).",
    # disclose unrounded ICER operands after the existing notes
    # so readers do not expect the rounded visible inputs to reproduce ratios.
    rounding = "ICERs are computed from unrounded incremental costs and QALYs; the rounded operands shown may not reproduce the ratio exactly."
  )

  ft <- flextable::flextable(display_df) |>
    format_thesis_table(caption_text = caption, table_number = 10) |>
    flextable::align(j = 2:4, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-10-subgroup-sa.docx", caption = caption, df = display_df, table_number = 10, footnotes = fn)
}


# --- Monotonicity table -------------------------------------------------------
build_internal_validation_table <- function() {
  val_path <- "data/processed/validation_checks.rds"
  if (!file.exists(val_path)) {
    message("08-export: Validation checks not available for monotonicity. Skipping.")
    return(NULL)
  }

  val <- readRDS(val_path)

  # Write-order guard: validation cache must not be older than the
  # trace it represents. Best-effort mtime check; not a content-freshness
  # proof. R/03-build-psm.R currently saves psm_trace.rds before
  # validation_checks.rds in the same main.R run; this guard depends on
  # that write order.
  trace_path <- "data/processed/psm_trace.rds"
  if (!file.exists(trace_path)) {
    message("08-export: psm_trace.rds not available. ",
            "Skipping monotonicity table.")
    return(NULL)
  }
  if (file.info(trace_path)$mtime >= file.info(val_path)$mtime) {
    message("08-export: validation_checks.rds (mtime ",
            format(file.info(val_path)$mtime, "%Y-%m-%d %H:%M:%S"),
            ") is older than psm_trace.rds (mtime ",
            format(file.info(trace_path)$mtime, "%Y-%m-%d %H:%M:%S"),
            "). This indicates the validation cache was not refreshed ",
            "after the most recent trace write. Rerun R/main.R end-to-end ",
            "so both caches are produced in the same session, or run ",
            "R/03-build-psm.R standalone before R/08-export-tables.R. ",
            "Skipping monotonicity table.")
    return(NULL)
  }

  # Arm-vector guard. Arm vocabulary is read from validation_checks.rds
  # (populated by R/03-build-psm.R) so the table builder does not
  # redeclare the arm names. The result-field extraction below is
  # coupled to exactly two arms, so the guard requires length 2.
  if (is.null(val$monotonicity_arms) ||
      !is.character(val$monotonicity_arms) ||
      length(val$monotonicity_arms) != 2L ||
      anyNA(val$monotonicity_arms)) {
    message("08-export: monotonicity_arms missing or not a character ",
            "vector of length 2 in validation_checks.rds. ",
            "Skipping monotonicity table.")
    return(NULL)
  }
  required_arms <- val$monotonicity_arms

  # Ordering-invariant guard. The result-field extraction below depends
  # on both the set of arm names AND their order. If a future upstream
  # refactor reorders monotonicity_arms, row labels would drift from
  # their verdicts silently. This guard fails fast and loud instead.
  expected_arms <- c("Standard Care", "Exercise")
  if (!identical(required_arms, expected_arms)) {
    message("08-export: monotonicity_arms order or content mismatch. ",
            "Expected c(\"Standard Care\", \"Exercise\"); got c(\"",
            paste(required_arms, collapse = "\", \""),
            "\"). Skipping monotonicity table.")
    return(NULL)
  }

  # Precondition guard: each arm in psm_trace must have at least 2 rows.
  # Rationale: all(numeric(0) <= tol) returns TRUE (vacuous truth), so a
  # degenerate arm would silently produce a Satisfied verdict.
  psm_trace <- readRDS(trace_path)
  if (!"arm" %in% names(psm_trace)) {
    message("08-export: psm_trace.rds missing arm column. ",
            "Skipping monotonicity table.")
    return(NULL)
  }
  rows_per_arm <- table(psm_trace$arm)
  missing_arms <- setdiff(required_arms, names(rows_per_arm))
  if (length(missing_arms) > 0L) {
    message("08-export: psm_trace.rds missing required arms: ",
            paste(missing_arms, collapse = ", "),
            ". Skipping monotonicity table.")
    return(NULL)
  }
  if (any(rows_per_arm[required_arms] < 2L)) {
    message("08-export: psm_trace arm row counts insufficient. ",
            "Required: both arms with >= 2 rows. Found: ",
            paste(names(rows_per_arm), rows_per_arm, sep = "=",
                  collapse = ", "),
            ". Skipping monotonicity table.")
    return(NULL)
  }

  # Tolerance guard. Tolerance is read from the upstream
  # validation_checks.rds, populated by R/03-build-psm.R from
  # R/00-parameters.R (defined once there).
  if (is.null(val$monotonicity_tolerance) ||
      !is.numeric(val$monotonicity_tolerance) ||
      length(val$monotonicity_tolerance) != 1L ||
      is.na(val$monotonicity_tolerance) ||
      val$monotonicity_tolerance <= 0) {
    message("08-export: monotonicity_tolerance missing or invalid in ",
            "validation_checks.rds. Skipping monotonicity table.")
    return(NULL)
  }
  tol <- val$monotonicity_tolerance

  result_values <- c(val$dfs_monotonic_standard_care,
                     val$dfs_monotonic_exercise,
                     val$dead_monotonic_standard_care,
                     val$dead_monotonic_exercise,
                     val$state_sum_all_arms,
                     val$non_negative_all_arms)

  if (!is.logical(result_values) ||
      length(result_values) != 6L ||
      anyNA(result_values)) {
    message("08-export: Internal validation fields invalid in ",
            "validation_checks.rds. Expected logical vector of length 6 ",
            "with no NA values. Observed class = ", class(result_values)[1],
            ", length = ", length(result_values),
            ", anyNA = ", anyNA(result_values),
            ". Skipping internal validation table.")
    return(NULL)
  }

  # Programmatic content assembly. All strings below are built from
  # variables. No inline prose literals describing the tests appear in
  # the rendered caption or footnote; descriptive prose is hoisted
  # into variables reused across row labels, caption, and
  # footnote to prevent drift.
  n_arms <- length(required_arms)
  arm_text <- paste(required_arms, collapse = " and ")
  n_checks <- length(result_values)
  # Platform-portable scientific-notation formatter for the tolerance.
  # formatC()/sprintf() emit a 2-digit exponent on macOS/Linux ("1e-10")
  # but a 3-digit exponent on some Windows R builds ("1e-010"). The
  # post-processing step strips leading zeros from the exponent so the
  # rendered token is identical across platforms. An assertion pins the
  # value when tol equals 1e-10 (the value defined in 00-parameters.R).
  tol_text <- formatC(tol, format = "e", digits = 0)
  tol_text <- sub("e([+-])0+([0-9])", "e\\1\\2", tol_text)
  if (identical(tol, 1e-10)) {
    stopifnot(identical(tol_text, "1e-10"))
  }

  verdict_pass <- "Satisfied"
  verdict_fail <- "Violated"

  test_labels <- c(
    dfs          = "DFS probability non-increasing",
    dead         = "Death probability non-decreasing",
    state_sum    = "State occupancy sums to 1",
    non_negative = "Progressed-disease probability non-negative"
  )
  n_test_types <- length(test_labels)

  # Caution: two of the four properties are evaluated PER ARM and two are pooled
  #   over all arms, so the rendered row count is 2 x n_arms + 2, never
  #   n_test_types x n_arms. The note below states that composition.
  arm_specific_labels <- test_labels[c("dfs", "dead")]
  pooled_labels       <- test_labels[c("state_sum", "non_negative")]

  check_descriptions <- c(
    paste0(test_labels[["dfs"]], " (", required_arms, ")"),
    paste0(test_labels[["dead"]], " (", required_arms, ")"),
    test_labels[["state_sum"]],
    test_labels[["non_negative"]]
  )

  # Oxford-style comma list for 3+ items ("a, b, c, and d"); "a and b" for 2.
  oxford_list <- function(x) {
    if (length(x) <= 2L) {
      paste(x, collapse = " and ")
    } else {
      paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
    }
  }
  arm_test_prose    <- oxford_list(arm_specific_labels)
  pooled_test_prose <- oxford_list(pooled_labels)

  # Caption carries only the table title (keeps List of Tables compact).
  # All methodology prose (test count, arm count, tolerance, verdict
  # semantics) is hoisted into fn$notes where it renders beneath the
  # table itself. All strings are still assembled via paste0() from
  # variables, so the programmatic-caption rule is preserved.
  # The caption splices n_arms (a variable, not a literal) so that it
  # is built from at least one programmatic variable rather than being
  # a paste0() wrapper around a single literal string (a technical
  # dodge of that rule). n_arms is a pure count derived from
  # the cache; it does not introduce numeric-looking tolerance content.
  caption <- paste0("Internal validation of PSM trace across ",
                    n_arms, " arms (", arm_text, "): per-cycle verdicts.")

  # Note for future maintainers (not rendered): upstream validation
  # computes six monotonicity values per main.R run (dfs_monotonic_*,
  # os_monotonic_*, dead_monotonic_* for both arms). The os_monotonic_*
  # check written in R/03-build-psm.R (validation_checks[["os_monotonic_<arm>"]],
  # built on `s_alive <- 1 - arm_data$p_dead`) is computed on 1 - p_dead, which
  # is the same expression as the dead_monotonic_* check with opposite
  # sign. The two tests are arithmetically identical, so this table
  # renders only the four independent monotonicity checks (DFS+Death x
  # 2 arms). State-sum and non-negativity are rendered as two global
  # aggregate verdicts (all arms, all cycles). Total rows: 4 + 2 = 6.

  df <- data.frame(
    Check = check_descriptions,
    Verdict = ifelse(result_values, verdict_pass, verdict_fail),
    stringsAsFactors = FALSE
  )

  fn <- list(
    abbrev = paste0("DFS = disease-free survival; ",
                    "PSM = partitioned survival model."),
    notes = paste0("Each verdict evaluates its stated condition at every ",
                   "model cycle and reduces the sequence to a single ",
                   verdict_pass, "/", verdict_fail,
                   " result within a floating-point noise tolerance of ",
                   tol_text, " per cycle."),
    source = "Source: PSM trace validation (R code, supplementary material)."
  )

  ft <- flextable::flextable(df) |>
    format_thesis_table(caption_text = caption, table_number = 11) |>
    flextable::align(j = 2, align = "center", part = "all") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  # Column alignment aligned with thesis companion style. Majority of
  # results/appendix tables use "l" for the label column and "r" for the
  # value column (see table 01-02, 06, 08-14). Table 08 (Model Validation
  # (the direct companion to this A.1 Internal Validation table) uses
  # "lr" specifically for its Check/Result two-column layout; A.1 now
  # mirrors that convention.
  list(ft = ft, path = "thesis-table-11-internal-validation.docx",
       caption = caption, df = df, table_number = 11, footnotes = fn,
       col_align = c("l", "r"))
}


# --- Quadrant analysis table ---------------------------------------------------
build_quadrant_table <- function() {
  # Requires psa_results in memory
  if (!exists("psa_results")) {
    message("08-export: PSA results not in memory for quadrant table. Skipping.")
    return(NULL)
  }

  wtp_ref <- if (exists("wtp_threshold")) wtp_threshold else opportunity_cost_nok

  # SOURCE: Drummond et al. (2015), Black (1990), HEVAL5200 course material
  # wtp_range from globalenv for crossover WTP computation
  qa <- compute_quadrant_analysis(psa_results,
                                   wtp_range = if (exists("wtp_range")) wtp_range else NULL)

  # Decision uncertainty from summarise_psa()
  psa_summ <- summarise_psa(psa_results)
  dec_unc <- min(psa_summ$prob_ce, 1 - psa_summ$prob_ce)

  # --- Programmatic caption ---
  # GUIDELINE: CHEERS-VOI Item 20 (characterizing uncertainty)
  caption <- "Cost-effectiveness plane quadrant analysis."

  display_df <- data.frame(
    Quadrant = c(
      "NE (more costly, more effective)",
      "SE (dominant: less costly, more effective)",
      "NW (dominated: more costly, less effective)",
      "SW (less costly, less effective)"
    ),
    Count = format(qa$counts, big.mark = ","),
    # DECISION: display-consistency rule. Fixed one-decimal formatting so a
    # zero quadrant renders "0.0%" and every cell matches the one-decimal
    # prose macros (SE stays 38.5%); values unchanged, formatting only.
    Percentage = paste0(
      formatC(round(qa$percentages, 1), format = "f", digits = 1), "%"),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    # DECISION: qa$counts is a NAMED vector, so data.frame() inherits NE/SE/NW/SW
    #   as ROW NAMES and kbl() emits them as an unheaded leading column ({llrr}).
    #   Suppressing them heads every real column.
    # Caution: display_df has exactly THREE columns (Quadrant, Count, Percentage).
    #   The leading quadrant-code column in the built .tex is the row-name column,
    #   NOT a data column: dropping a data column here would delete Quadrant and
    #   leave the row names in place.
    row.names = NULL
  )

  fn <- list(
    abbrev = "NE = north-east; SE = south-east; NW = north-west; SW = south-west; CE = cost-effectiveness.",
    source = "Source: Probabilistic analysis quadrant data."
  )

  ft <- flextable::flextable(display_df) |>
    format_thesis_table(caption_text = caption, table_number = 12) |>
    flextable::align(j = 2:3, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-12-quadrant-analysis.docx", caption = caption, df = display_df, table_number = 12, footnotes = fn)
}


# --- Disaggregated results table (A.10.7) ------------------------------------
# Per-arm, per-state breakdown of life-years and QALYs.
# GUIDELINE: disaggregated outcome reporting. NOT CHEERS-VOI Item 10, which
# governs discount-rate reporting (Kunst et al. 2023, Table 2, printed p.1464).
build_disaggregated_table <- function() {
  cq_path <- "data/processed/costs_qalys.rds"
  if (!file.exists(cq_path)) {
    message("08-export: costs_qalys.rds not available. Skipping disaggregated table.")
    return(NULL)
  }

  cq <- readRDS(cq_path)

  # State occupancies: eff_p_dfs, eff_p_prog (half-cycle corrected)
  # Life-years: ly_raw (undiscounted), ly_disc (discounted)
  # Per-state discounted QALYs: qalys_dfs_disc, qalys_pd_disc
  # These columns were added to 04-costs-qalys.R so the decomposition
  # exactly matches total qalys_disc and includes age-adjustment and
  # exercise decrement without needing utility globals at export time.
  cycle_len <- if (exists("cycle_length")) cycle_length else 1/12

  have_split <- all(c("qalys_dfs_disc", "qalys_pd_disc") %in% names(cq))
  if (!have_split) {
    warning("build_disaggregated_table: costs_qalys.rds lacks per-state QALY columns ",
            "(qalys_dfs_disc, qalys_pd_disc). Rerun main.R to regenerate. ",
            "Reporting NA for DFS/PD QALY split.")
  }

  results_by_arm <- lapply(c("Standard Care", "Exercise"), function(arm_name) {
    arm_data <- cq[cq$arm == arm_name, ]
    # Per-state LYs: state occupancy x cycle_length (undiscounted)
    dfs_ly <- sum(arm_data$eff_p_dfs) * cycle_len
    pd_ly  <- sum(arm_data$eff_p_prog) * cycle_len
    # Per-state QALYs: read directly from stored decomposition columns.
    # qalys_dfs_disc + qalys_pd_disc = qalys_disc exactly (u_dead = 0).
    if (have_split) {
      dfs_qaly <- sum(arm_data$qalys_dfs_disc)
      pd_qaly  <- sum(arm_data$qalys_pd_disc)
    } else {
      dfs_qaly <- NA_real_
      pd_qaly  <- NA_real_
    }
    data.frame(
      Arm = arm_name,
      `DFS LYs (undisc.)` = round(dfs_ly, 2),
      `PD LYs (undisc.)` = round(pd_ly, 2),
      `Total LYs (undisc.)` = round(sum(arm_data$ly_raw), 2),
      `DFS QALYs (disc.)` = round(dfs_qaly, 4),
      `PD QALYs (disc.)` = round(pd_qaly, 4),
      # DECISION: display-consistency rule. The displayed per-arm QALY total
      # is the sum of its displayed rounded components (e.g. Standard Care
      # 8.8095 + 1.4403 = 10.2498), not an independently rounded raw total,
      # so every row and the arm-difference column reproduce by hand from
      # the printed cells. The outer round() only removes floating-point noise.
      `Total QALYs (disc.)` = round(round(dfs_qaly, 4) + round(pd_qaly, 4), 4),
      `Total Costs (NOK)` = format_nok(sum(arm_data$costs_disc)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, results_by_arm)

  inc_row <- data.frame(
    Arm = "Incremental",
    `DFS LYs (undisc.)` = round(
      as.numeric(df[2, 2]) - as.numeric(df[1, 2]), 2),
    `PD LYs (undisc.)` = round(
      as.numeric(df[2, 3]) - as.numeric(df[1, 3]), 2),
    `Total LYs (undisc.)` = round(
      as.numeric(df[2, 4]) - as.numeric(df[1, 4]), 2),
    `DFS QALYs (disc.)` = round(
      as.numeric(df[2, 5]) - as.numeric(df[1, 5]), 4),
    `PD QALYs (disc.)` = round(
      as.numeric(df[2, 6]) - as.numeric(df[1, 6]), 4),
    # DECISION: display-consistency rule. The displayed incremental QALY
    # total is the sum of its displayed rounded components
    # (0.9783 - 0.0681 = 0.9102), not an independently rounded raw total;
    # the outer round() only removes floating-point noise.
    `Total QALYs (disc.)` = round(
      round(as.numeric(df[2, 5]) - as.numeric(df[1, 5]), 4) +
        round(as.numeric(df[2, 6]) - as.numeric(df[1, 6]), 4), 4),
    `Total Costs (NOK)` = format_nok(
      sum(cq$costs_disc[cq$arm == "Exercise"]) -
        sum(cq$costs_disc[cq$arm == "Standard Care"])),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  df <- rbind(df, inc_row)

  # Caption made placement-neutral for main-paper placement
  caption <- "Disaggregated outcomes by health state."

  fn <- list(
    abbrev = paste0(
      "DFS = disease-free survival; PD = progressed disease; LY = life-year; ",
      "QALY = quality-adjusted life-year; NOK = Norwegian kroner; ",
      "undisc. = undiscounted; disc. = discounted."
    ),
    pricebasis = PRICE_BASIS_NOTE,
    source = "Source: Cost and QALY computation (R model, supplementary material)."
  )

  ft <- flextable::flextable(df) |>
    format_thesis_table(caption_text = caption, table_number = 13) |>
    flextable::align(j = 2:8, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-13-disaggregated.docx",
       caption = caption, df = df, table_number = 13, footnotes = fn,
       page_layout = "sideways-single")
}


# --- Perspective comparison table (Results body) -----------------------------
# Extended perspective = base case (Meld. St. 21, 2024-2025, Section 4.3.8)
# GUIDELINE: Second Panel Recommendation 1 on multi-perspective reporting
#   (Sanders et al. 2016)
# SOURCE: perspective_delta.rds from R/04-costs-qalys.R
build_perspective_table <- function() {
  pd_path <- "data/processed/perspective_delta.rds"
  if (!file.exists(pd_path)) {
    message("08-export: perspective_delta.rds not available. ",
            "Skipping perspective table.")
    return(NULL)
  }

  pd <- readRDS(pd_path)

  # Caution: Compute delta from ROUNDED ICERs to avoid compound
  # rounding (for internal consistency with displayed values); differencing the
  # unrounded ICERs can land one unit away from the displayed operands.
  icer_ext <- round(pd$icer[1])
  icer_std <- round(pd$icer[2])
  # Caution: Derive reader-facing deltas from rounded displayed operands to prevent one-unit drift.
  icer_delta <- icer_ext - icer_std

  inc_cost_ext <- round(pd$inc_cost[1])
  inc_cost_std <- round(pd$inc_cost[2])
  # Caution: Derive reader-facing deltas from rounded displayed operands to prevent one-unit drift.
  cost_delta <- inc_cost_ext - inc_cost_std

  inc_qaly_ext <- round(pd$inc_qaly[1], 3)
  inc_qaly_std <- round(pd$inc_qaly[2], 3)
  # Caution: Derive reader-facing deltas from rounded displayed operands to prevent display drift.
  qaly_delta <- round(inc_qaly_ext - inc_qaly_std, 3)

  # Caution: Derive reader-facing percentages from the displayed delta and denominator.
  pct <- paste0("+", round(icer_delta / icer_std * 100), "%")

  df <- data.frame(
    Perspective = c(
      "Extended healthcare (base case)",
      "Standard healthcare (SA)",
      "Delta (extended minus standard)"
    ),
    `Inc. Cost (NOK)` = c(
      format_nok(inc_cost_ext),
      format_nok(inc_cost_std),
      format_nok(cost_delta)
    ),
    `Inc. QALY` = c(
      sprintf("%.3f", inc_qaly_ext),
      sprintf("%.3f", inc_qaly_std),
      sprintf("%.3f", qaly_delta)
    ),
    `ICER (NOK/QALY)` = c(
      format_nok(icer_ext),
      format_nok(icer_std),
      paste0(format_nok(icer_delta), " (", pct, ")")
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  caption <- "Perspective scenario analysis (deterministic base case)."

  fn <- list(
    abbrev = paste0(
      "ICER = incremental cost-effectiveness ratio; ",
      "QALY = quality-adjusted life year; ",
      "NOK = Norwegian kroner (2024 prices); ",
      "SA = sensitivity analysis."
    ),
    rounding = paste0(
      "ICERs are computed from unrounded incremental costs and QALYs; the ",
      "rounded operands shown may not reproduce the ratio exactly."),
    notes = paste0(
      "Extended healthcare includes patient travel and time costs; standard ",
      "healthcare excludes them. Delta row reports extended minus standard ",
      "using rounded displayed ICERs. Incremental QALYs are identical because ",
      "the perspective scenario changes costs only."
    ),
    source = "Source: Perspective scenario analysis."
  )

  ft <- flextable::flextable(df) |>
    format_thesis_table(caption_text = caption, table_number = 14) |>
    flextable::align(j = 2:4, align = "right", part = "body") |>
    flextable::bold(i = 3, part = "body") |>
    add_thesis_footnotes(fn)

  # Also write CSV for programmatic reproducibility
  csv_path <- file.path(TABLE_OUTPUT_DIR, "perspective_comparison.csv")
  write.csv(df, csv_path, row.names = FALSE)
  cat("Saved CSV:", csv_path, "\n")

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(ft = ft, path = "thesis-table-14-perspective.docx", caption = caption, df = df, table_number = 14, footnotes = fn)
}


# --- Input parameters table (Appendix A.7) ------------------------------------
# Programmatic extraction of all model input parameters from R globals.
# Replaces the xlsx-generated input-parameters table with a single-source
# R-generated version. Every value traces to R/00-parameters.R.
# GUIDELINE: CHEERS 2022 Item 14 (model parameters)
build_input_parameters_table <- function() {

  # Helper: format with commas
  fnok <- function(x) format(round(x), big.mark = ",")

  # --- Section 1: Clinical Efficacy ---
  sec1_header <- data.frame(
    Parameter = "CLINICAL EFFICACY AND MODEL SETTINGS", `Base Case` = "",
    `SE/SD` = "",
    `PA Distribution` = "", Source = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )

  sec1 <- data.frame(
    Parameter = c(
      "HR DFS (exercise vs control)",
      "HR OS (exercise vs control)",
      "Cross-endpoint reference dependence (rho)",
      "Starting age",
      "Time horizon",
      "Cycle length"
    ),
    `Base Case` = c(
      as.character(HR_DFS),
      as.character(HR_OS),
      as.character(rho_endpoint_latent_reference),
      paste0(cohort_age, " years"),
      paste0(time_horizon, " years"),
      paste0(round(cycle_length * 12), " month")
    ),
    `SE/SD` = c(
      paste0("SE=", round(se_log_HR_DFS, 3)),
      paste0("SE=", round(se_log_HR_OS, 3)),
      "-", "-", "-", "-"
    ),
    `PA Distribution` = c(
      "Lognormal", "Lognormal", "Fixed assumption", "Fixed", "Fixed", "Fixed"
    ),
    Source = c(
      "Courneya et al., 2025",
      "Courneya et al., 2025",
      "Assumption: joint DFS-OS covariance not identifiable from published trial data; specification sensitivity in Appendix Table~\\ref{tab:table30}",
      "CHALLENGE trial median",
      "DMP guidelines",
      "Standard practice"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  # --- Section 2: HRQoL Utility Weights ---
  sec2_header <- data.frame(
    Parameter = "HRQoL UTILITY WEIGHTS", `Base Case` = "", `SE/SD` = "",
    `PA Distribution` = "", Source = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )

  sec2 <- data.frame(
    Parameter = c(
      "Disease-free (u_DF)",
      "Progressed disease (u_PD)",
      "Dead",
      "Exercise HRQoL decrement"
    ),
    `Base Case` = c(
      as.character(round(u_dfs_mean, 4)),
      formatC(u_prog_mean, format = "f", digits = 3),
      as.character(round(u_dead, 4)),
      as.character(round(u_exercise_decrement, 4))
    ),
    `SE/SD` = c(
      paste0("SE=", round(u_dfs_se, 4)),
      paste0("SE=", round(u_prog_sampling_se, 4)),
      "-",
      "-"
    ),
    `PA Distribution` = c("Beta", "Beta", "Fixed", "Fixed (not sampled)"),
    Source = c(
      "Mulder et al., 2022",
      paste0("Färkkilä et al., 2014 (mapped to 5L via Torkilseng et al., ",
             "2025; unrounded mapped mean ",
             formatC(u_prog_mean, format = "f", digits = 6), ")"),
      "By definition",
      "No evidence of disutility"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  # --- Section 3: Cost Parameters ---
  sec3_header <- data.frame(
    Parameter = "COST PARAMETERS", `Base Case` = "", `SE/SD` = "",
    `PA Distribution` = "", Source = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )

  sec3 <- data.frame(
    Parameter = c(
      "Exercise session fee (Helfo A3, DMP-adjusted)",
      "Initial assessment (Helfo A1d)",
      "Patient travel per session",
      "Patient time, exercise",
      "Patient time, travel",
      "Session total (extended)",
      "Session total (healthcare-services-only perspective)",
      "Behavioural-support contact (A3a + 1 x A3b, 30 min)",
      "Shared surveillance annual (years 0-5)",
      "Shared progressed-disease annual",
      "End-of-life (one-time)"
    ),
    `Base Case` = c(
      fnok(c_session_healthcare),
      fnok(c_intervention_setup),
      formatC(c_travel_per_session, format = "f", digits = 2, big.mark = ","),
      fnok(c_patient_time_rate),
      paste0("approx ", fnok(round(c_patient_time_per_session - c_patient_time_rate))),
      paste0("approx ", fnok(round(c_session_extended))),
      fnok(c_session_standard),
      fnok(c_behaviour_contact),
      fnok(c_surveillance_early),
      fnok(c_progressed_annual),
      fnok(c_terminal)
    ),
    `SE/SD` = c(
      "-", paste0("SE=", fnok(c_intervention_setup_se)), "-", "-", "-",
      "composite",
      "-",
      "-",
      paste0("SE=", fnok(c_surveillance_early_se)),
      paste0("SE=", fnok(c_progressed_annual_se)),
      paste0("SE=", fnok(c_terminal_se))
    ),
    `PA Distribution` = c(
      "(via total)", "Gamma", "(via total)", "(via total)", "(via total)",
      "Gamma", "-", "(via total)", "Gamma", "Gamma", "Gamma"
    ),
    Source = c(
      "2 x (Helfo A3a + 4xA3b); DMP v1.6 1.2 Forutsetninger row 27",
      "Helfo A1d",
      "Pasientreiser 3.20 NOK/km (pasientreiseforskriften Section 21)",
      "DMP v1.6 Fritid rate",
      "DMP v1.6 Fritid rate",
      "Composite",
      "DMP-adjusted Helfo A3 only",
      "Helfo A3a + 1 x A3b; lower bound of the stated 30-60 min per topic",
      "Helsedir + ISF DRG 906A + DMP v1.6",
      "Joranger 2020 OR1 T1; SSB CPI to 2024",
      "Bjørnelv 2020 CPI-adj"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  # --- Combine ---
  df <- rbind(sec1_header, sec1, sec2_header, sec2, sec3_header, sec3)

  # --- Programmatic caption ---
  caption <- paste0(
    "Model Input Parameters. ",
    nrow(sec1) + nrow(sec2) + nrow(sec3), " parameters across ",
    "clinical efficacy and model settings, HRQoL utility weights, and cost ",
    "components. ",
    "All values implemented in the R model (supplementary material). ",
    "Extended healthcare perspective. ",
    "Effective discount rate 4% per Rundskriv R-109 stepped schedule."
  )

  fn <- list(
    abbrev = paste0(
      "HR = hazard ratio; DFS = disease-free survival; ",
      "OS = overall survival; SE = standard error; SD = standard deviation; ",
      "QALY = quality-adjusted life year; NOK = Norwegian kroner; ",
      "PA = probabilistic analysis; ",
      "CPI = consumer price index; CHALLENGE = Colon Health and Lifelong ",
      "Exercise Change (trial); DF = disease-free; PD = progressed disease; ",
      "DMP = Norwegian Medical Products Agency; DRG = diagnosis-related group; ",
      "Helfo = Helseøkonomiforvaltningen (Norwegian Health Economics ",
      "Administration); HRQoL = health-related quality of life; ",
      "ISF = Innsatsstyrt finansiering (activity-based financing); ",
      "SSB = Statistisk sentralbyrå."
    ),
    pricebasis = PRICE_BASIS_NOTE,
    source = paste0(
      "Source: Model input parameters (R code, supplementary material). ",
      "CHEERS 2022 Item 14."
    )
  )

  ft <- flextable::flextable(df) |>
    format_thesis_table(caption_text = caption, table_number = 15) |>
    flextable::align(j = 2:5, align = "left", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  # LAYOUT WL-04: the landscape longtable (appendix thesisrotatedlongtable) sat
  # 67.21pt over its rotated line width in all 5 chunks (overfull boxes). Fixed
  # p-widths rehearsal-proven 0 boxes; col3 fits the unbreakable SE token (>3.63cm).
  list(ft = ft, path = "thesis-table-15-input-parameters.docx", caption = caption, df = df, table_number = 15, footnotes = fn, col_align = c("l", "r", "l", "l", "l"),
       caption_continued = "Model Input Parameters.",
       tabular_colspec_override = "lr>{\\raggedright\\arraybackslash}p{3.7cm}>{\\raggedright\\arraybackslash}p{3.6cm}>{\\raggedright\\arraybackslash}p{5.2cm}")
}


# ==============================================================================
# APPENDIX TABLES (Tables 16-25)
# LaTeX only. All tables generated directly from R.
# All values from R/00-parameters.R (the primary reference file).
# ==============================================================================

# --- Table 16: Exercise Intervention Costs ------------------------------------
build_exercise_costs_table <- function() {
  df <- data.frame(
    Component = c(
      "Helfo A3a (start tariff, individual PT, 20 min)",
      paste0("Helfo A3b (time tariff, 10 min, x", n_a3b_units, ")"),
      "Session subtotal (healthcare, DMP-adjusted)",
      paste0("Patient travel (", travel_distance_km, " km x ",
             format(mileage_rate_nok, nsmall = 2), " NOK/km x 2)"),
      paste0("Patient time, exercise (", exercise_time_hr, " hr x ",
             format_nok(c_patient_time_rate), " NOK/hr)"),
      paste0("Patient time, travel (", round(travel_time_min), " min x ",
             format_nok(c_patient_time_rate), " NOK/hr)"),
      "Session total (extended perspective)",
      "Session total (healthcare-services-only perspective)",
      "Initial assessment Helfo A1d (one-time)",
      # DECISION: the fifth cost component belongs in this table. The appendix
      #   states the exercise cost components are itemised here, so omitting it
      #   would make that sentence false. No travel and no patient time attach
      #   to this row.
      "Behavioural-support contact (A3a + 1 x A3b, 30 min)"
    ),
    `Base case (NOK)` = c(
      format_nok(c_helfo_a3a),
      paste0(n_a3b_units, " x ", format_nok(c_helfo_a3b), " = ",
             format_nok(n_a3b_units * c_helfo_a3b)),
      paste0(format(dmp_takst_adjustment), " x ", format_nok(c_session_tariff_bare),
             " = ", format_nok(c_session_healthcare)),
      format_nok(c_travel_per_session),
      format_nok(c_patient_time_rate),
      paste0("approx ", format_nok(round(c_patient_time_per_session - c_patient_time_rate))),
      paste0("approx ", format_nok(round(c_session_extended))),
      format_nok(c_session_standard),
      format_nok(c_intervention_setup),
      format_nok(c_behaviour_contact)
    ),
    Source = c(
      "Helfo tariff (see note)",
      "Helfo tariff (see note)",
      paste0("(A3a + ", n_a3b_units, " x A3b) x ", format(dmp_takst_adjustment),
             "; DMP v1.6 1.2 Forutsetninger row 27"),
      "Pasientreiser (pasientreiseforskriften Section 21); Meld. St. 21 Section 4.3.8",
      "DMP enhetskostnader v1.6 Fritid rate",
      "DMP enhetskostnader v1.6 Fritid rate",
      "Composite of above",
      "DMP-adjusted Helfo A3 only",
      "Helfo tariff (see note)",
      "Helfo A3a + 1 x A3b; lower bound of the stated 30-60 min per topic"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- "Exercise intervention cost components."

  fn <- list(
    abbrev = paste0(
      "PT = physiotherapy; NOK = Norwegian kroner; ",
      "Helfo = Helse\u00f8konomiforvaltningen (Norwegian Health Economics Administration); ",
      "DMP = Norwegian Medical Products Agency; ",
      "CHEERS = Consolidated Health Economic Evaluation Reporting Standards."
    ),
    notes = "Helfo tariffs are July 2025 rates entered undeflated, as is the Pasientreiser mileage rate in force from 1 January 2026; other costs are 2024 NOK.",
    source = "Source: Model inputs; CHEERS Item 14."
  )

  list(df = df, table_number = 16, caption = caption,
       footnotes = fn, col_align = c("l", "r", "l"),
       # Rotated-content fallback:
       # the upright render spans the full A4 width (gs bbox 595.28 of
       # 595.28pt) and clips on any physical printer margin.
       page_layout = "sideways-single")
}


# --- Table 17: Adherence Schedule ---------------------------------------------
build_adherence_table <- function() {
  df <- data.frame(
    Phase = c(
      "Phase 1 (months 1-6)",
      paste0("Phase 2 (months 7-",
             adherence_phase1_months + adherence_phase2_months, ")"),
      paste0("Phase 3 (months ",
             adherence_phase1_months + adherence_phase2_months + 1, "-",
             adherence_phase1_months + adherence_phase2_months + adherence_phase3_months, ")"),
      "Total"
    ),
    Duration = c(
      paste0(adherence_phase1_months, " months"),
      paste0(adherence_phase2_months, " months"),
      paste0(adherence_phase3_months, " months"),
      paste0(adherence_phase1_months + adherence_phase2_months + adherence_phase3_months,
             " months")
    ),
    `Adherence-adjusted contacts` = c(
      format(adj_sessions_phase1, nsmall = 2),
      format(adj_sessions_phase2, nsmall = 2),
      format(adj_sessions_phase3, nsmall = 2),
      format(adj_sessions_total_gross, nsmall = 2)
    ),
    `Total sessions (phase, gross)` = c(
      as.character(gross_sessions_phase1),
      as.character(gross_sessions_phase2),
      as.character(gross_sessions_phase3),
      as.character(gross_sessions_total)
    ),
    `Adherence rate` = c(
      # Caution: one decimal, not zero. At zero decimals phase 1 printed 50% for a
      #   factor of 49.5% and the total printed 48% for 48.2%, so neither row
      #   reproduced its own printed adherence-adjusted contacts.
      paste0(format(round(adherence_phase1_rate * 100, 1), nsmall = 1), "%"),
      paste0(format(round(adherence_phase2_rate * 100, 1), nsmall = 1), "%"),
      paste0(format(round(adherence_phase3_rate * 100, 1), nsmall = 1), "%"),
      paste0(format(round(adherence_weighted_rate * 100, 1), nsmall = 1),
             "% (weighted)")
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  # Caution: the total is printed ONCE. `adj_sessions_total_gross` and
  #   `n_sessions_adherence` are tied by the stopifnot at 00-parameters.R:906,
  #   so printing both produced the degenerate "reduces total from 28.92 to
  #   28.92" sentence the superseded caption and note carried. There is no
  #   second-stage refinement between the schedule sum and the costed count.
  caption <- paste0(
    "Exercise Programme Adherence Schedule. ",
    "Adherence-weighted supervised exercise sessions: ",
    format(adj_sessions_total_gross, nsmall = 2), " of ",
    gross_sessions_total, " scheduled sessions over ",
    intervention_duration_years * 12, " months. ",
    "The three phase totals below sum to that figure, which is the session ",
    "count costed in the R model (see Section A.7.1 text)."
  )

  # DECISION: the note states WHY the costed count is exercise-only.
  #   The source also prints behavioural-support attendance, so a reader who
  #   does not see this scoping rule cannot reconstruct the total and cannot
  #   tell why the larger blended figure was retired. The superseded note
  #   instead described the blend as the basis of the rates, which is the
  #   double count that was retired.
  # Caution: the displayed phase-1 rate is the mean of the two printed phase-1
  #   rates over one schedule, not a percentage the source prints. Phases 2
  #   and 3 each carry a single printed rate (54% and the pooled 44%).
  fn <- list(
    abbrev = "",
    notes = paste0(
      "Rates are the printed attendance percentages for supervised exercise ",
      "sessions only; behavioural-support contacts are costed separately on ",
      "their own printed rates. The phase-1 rate is the mean of two printed ",
      "rates over one schedule; phases 2 and 3 each use a single printed rate."
    ),
    source = paste0(
      "Source: Courneya et al. (2025), Table 2 and Results narrative; ",
      "session counts constructed, not transcribed."
    )
  )

  list(df = df, table_number = 17, caption = caption,
       tabular_colspec_override = paste0(
         ">{\\raggedright\\arraybackslash}p{3.0cm}",
         ">{\\raggedright\\arraybackslash}p{1.9cm}",
         ">{\\raggedleft\\arraybackslash}p{2.9cm}",
         ">{\\raggedleft\\arraybackslash}p{3.0cm}",
         ">{\\raggedleft\\arraybackslash}p{2.3cm}"),
       footnotes = fn, col_align = c("l", "l", "r", "r", "r"))
}


# --- Table 18: SA Session Cost Anchors ----------------------------------------
# 7 structural-SA anchors checked against the July-2025 Helfo amendment
# and separately dated primary Norwegian sources.
build_sa_anchors_table <- function() {
  df <- data.frame(
    `SA anchor` = c(
      "Helfo C22a group physiotherapy (lower bound)",
      "Helfo A3 individual PT, bare tariff (tested alternative)",
      "Helfo A7 specialist PT",
      "Thorsen OUS labour cost",
      "Helfo A8 manual therapy",
      "Helfo A3 + driftstilskudd, municipal subsidy (tested alternative)",
      "DMP-adjusted tariff, 2 x Helfo A3 (base case)"
    ),
    `Value (NOK)` = c(
      format_nok(sa_session_c22a_group),
      format_nok(sa_session_a3_base),
      format_nok(sa_session_a7_specialist),
      format_nok(sa_session_thorsen_ous),
      format_nok(sa_session_a8_manual),
      format_nok(sa_session_driftstilskudd),
      format_nok(sa_session_extreme)
    ),
    `Source description` = c(
      paste0("C22a (303/10) + C22b (112) = 142 NOK/patient; ",
             "FOR-2025-06-23-1196 (from 1 July 2025)"),
      paste0("A3a (", format_nok(c_helfo_a3a), ") + ", n_a3b_units,
             " x A3b (", format_nok(c_helfo_a3b),
             "); FOR-2025-06-23-1196 (from 1 July 2025)"),
      "A7a (275) + 4 x A7b (109); FOR-2025-06-23-1196 (from 1 July 2025)",
      "Thorsen OUS: 504 NOK/hr x 1.5 hr (L. Thorsen, personal communication, April 11, 2026)",
      "A8a (340) + 4 x A8b (141); FOR-2025-06-23-1196 (from 1 July 2025)",
      paste0("A3 (", format_nok(sa_session_a3_base),
             "; July-2025 tariff) + municipal driftstilskudd ",
             "amortised per session (2024 rate, FOR-2024-06-17-1184)"),
      paste0("2 x ", format_nok(sa_session_a3_base),
             "; doubling rule from DMP v1.6 sheet 1.2 Forutsetninger row 27 ",
             "(Justering av takst = 2; SLV 2018 s. 27)")
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- paste0(
    "Prespecified Exercise Session Cost Anchors. ",
    nrow(df), " prespecified session-cost anchors (inputs) spanning ",
    format_nok(sa_session_c22a_group), " to ",
    format_nok(sa_session_extreme), " NOK per session. ",
    "Base case: ", format_nok(c_session_healthcare),
    " NOK, the Helfo A3 tariff with the DMP subsidy adjustment."
  )

  fn <- list(
    abbrev = paste0(
      "PT = physiotherapy; OUS = Oslo University Hospital; ",
      "NOK = Norwegian kroner; ",
      "DMP = Norwegian Medical Products Agency; ",
      "Helfo = Helseøkonomiforvaltningen (Norwegian Health Economics ",
      "Administration); SA = sensitivity analysis."
    ),
    pricebasis = "The July-2025 Helfo tariffs, values composed from them, and the Thorsen OUS labour anchor are entered at face value; the driftstilskudd component is in 2024 NOK.",
    notes = paste0(
      "Helfo C22, A3, A7 and A8 anchors are the regulated rates in force ",
      "from 1 July 2025. The base case is the DMP-adjusted tariff."
    ),
    source = paste0(
      "Source: FOR-2025-06-23-1196; FOR-2024-06-17-1184; ",
      "DMP enhetskostnader v1.6; ",
      "L. Thorsen (personal communication, April 11, 2026)."
    )
  )

  list(df = df, table_number = 18, caption = caption,
       footnotes = fn, col_align = c("l", "r", "l"),
       tabular_colspec_override = ">{\\raggedright\\arraybackslash}p{4.2cm}>{\\raggedleft\\arraybackslash}p{1.6cm}>{\\raggedright\\arraybackslash}p{5.4cm}")
}


# --- Table 19: Disease-Free State Costs ---------------------------------------
build_df_costs_table <- function() {
  # Guideline-faithful 5-year surveillance schedule.
  # Amortises to c_surveillance_early (params.R).
  five_year_total <- n_cea_5yr * c_cea_unit + n_ct_5yr * c_ct_session_unit +
    n_colonoscopy_5yr * c_colonoscopy_unit + n_specialist_5yr * c_specialist_unit
  df <- data.frame(
    Component = c(
      "CEA blood test",
      "CT (lungs/liver/abdomen)",
      "Colonoscopy",
      "Specialist consultation",
      "Five-year total",
      "Amortised annual"
    ),
    `Count (5 yr)` = c(
      as.character(n_cea_5yr),
      paste0(n_ct_5yr, " (yr 1-3)"),
      paste0(n_colonoscopy_5yr, " (yr 5)"),
      as.character(n_specialist_5yr),
      "",
      ""
    ),
    `Unit cost (2024 NOK)` = c(
      format_nok(c_cea_unit),
      format_nok(c_ct_session_unit),
      format_nok(c_colonoscopy_unit),
      format_nok(c_specialist_unit),
      "",
      ""
    ),
    `5-year cost (NOK)` = c(
      format_nok(n_cea_5yr * c_cea_unit),
      format_nok(n_ct_5yr * c_ct_session_unit),
      format_nok(n_colonoscopy_5yr * c_colonoscopy_unit),
      format_nok(n_specialist_5yr * c_specialist_unit),
      format_nok(five_year_total),
      format_nok(c_surveillance_early)
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- paste0(
    "Disease-Free State Surveillance Costs. ",
    "Guideline-faithful 5-year post-resection surveillance schedule; ",
    "five-year total ", format_nok(five_year_total), " NOK amortised to ",
    format_nok(c_surveillance_early), " NOK/year (2024 NOK), applied during years 0-",
    surveillance_cutoff_years,
    " per the Helsedirektoratet CRC follow-up programme."
  )

  fn <- list(
    abbrev = paste0(
      "CEA = carcinoembryonic antigen; CT = computed tomography; ",
      "NOK = Norwegian kroner; DMP = Norwegian Medical Products Agency; ",
      "CRC = colorectal cancer. The CEA test is costed at the generic ",
      "sample-taking tariff (takst 701a, DMP unit-cost database), applied ",
      "to CEA by assumption."
    ),
    notes = paste0(
      "Schedule per the Norwegian national CRC follow-up programme: CEA at ",
      "1/6/12/18/24/30/36/60 months (8 tests); CT lungs/liver/abdomen at ",
      "12/24/36 months (years 1 to 3); colonoscopy at 60 months (year 5); ",
      "2 specialist consultations. Follow-up costs set to zero after year ",
      surveillance_cutoff_years, ". ",
      "Derived values use unrounded operands; rounding may prevent exact reproduction."
    ),
    source = paste0(
      "Source: Helsedirektoratet CRC follow-up programme; ",
      "DMP enhetskostnader v1.6; ISF DRG 906A and 710O; ",
      "Kenseth et al. (2024) Table 1."
    )
  )

  list(df = df, table_number = 19, caption = caption,
       footnotes = fn, col_align = c("l", "r", "r", "r"))
}


# --- Table 20: Progressed Disease Costs ---------------------------------------
build_pd_costs_table <- function() {
  # SOURCE: Joranger et al. 2020 Online Resource 1 Table 1, stage-IV column.
  # the component rows are PRINTED from stage4_components
  #   (R/00-parameters.R), never restated here, so the table and the model
  #   cannot drift apart.
  df <- data.frame(
    Resource = c(
      paste0(stage4_components$row_label,
             " (row ", stage4_components$joranger_row, ")"),
      "Progressed-disease cost, frequency-weighted total",
      "End-of-life (one-time, last month of life)",
      "Drug discount (SA)"
    ),
    `Cost basis` = c(
      paste0("EUR ", format(stage4_components$unit_cost_eur_2011,
                            big.mark = ",", trim = TRUE),
             " x ", sprintf("%.3f", stage4_components$frequency_stage4)),
      paste0(format_nok(c_progressed_annual), "/year"),
      format_nok(c_terminal),
      paste0(round(drug_discount_rate * 100), "% applied")
    ),
    Notes = c(
      rep("2011 EUR unit cost x stage-IV frequency", nrow(stage4_components)),
      "Converted at 7.79 NOK/EUR; SSB CPI 2011 to 2024",
      "Bjørnelv 2020, CPI-adjusted",
      "Structural SA scenario"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- paste0(
    "Progressed Disease State Costs. ",
    "Metastatic treatment: ", format_nok(c_progressed_annual), " NOK/year. ",
    "End-of-life: ", format_nok(c_terminal), " NOK (one-time). ",
    "All values in 2024 NOK unless stated."
  )

  fn <- list(
    abbrev = paste0(
      "SA = sensitivity analysis; CPI = consumer price index; ",
      "NOK = Norwegian kroner; EUR = euro; SSB = Statistisk sentralbyrå."
    ),
    notes = paste0(
      "Drug-component discount (", round(drug_discount_rate * 100),
      "% of the drug share, which is ", round(drug_cost_share_pd * 100),
      "% of PD costs) in structural SA, yielding ",
      round(drug_discount_factor * 100), "% of base-case PD cost, a ",
      round((1 - drug_discount_factor) * 100),
      "% reduction in the total progressed-disease annual cost. ",
      "Same cost applies to both arms (exercise programme ended in PD state)."
    ),
    source = paste0(
      "Source: Joranger et al. (2020) Online Resource 1 Table 1; ",
      "Bjørnelv et al. (2020)."
    )
  )

  list(df = df, table_number = 20, caption = caption,
       caption_short = "Progressed-disease costs",
       tabular_colspec_override = paste0(
         ">{\\raggedright\\arraybackslash}p{6.2cm}",
         ">{\\raggedleft\\arraybackslash}p{3.1cm}",
         ">{\\raggedright\\arraybackslash}p{5.4cm}"),
       footnotes = fn, col_align = c("l", "r", "l"))
}


# --- Table 21: Discount Rate Schedule ----------------------------------------
build_discount_schedule_table <- function() {
  brackets <- formals(create_discount_weights)$brackets
  if (is.call(brackets)) brackets <- eval(brackets)

  df <- data.frame(
    # Public labels are explicit metadata: internal endpoints are elapsed-time
    # accumulator boundaries and must not be formatted as printed year bands.
    `Time horizon` = brackets$display_label,
    `Annual discount rate` = paste0(brackets$rate * 100, "%"),
    Source = c(
      "Rundskriv R-109 (Ministry of Finance, 2021)",
      "Rundskriv R-109",
      "Rundskriv R-109"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- paste0(
    "Norwegian Stepped Discount Rate Schedule. ",
    "Applied to both costs and QALYs per Rundskriv R-109 (2021). ",
    "Flat ", round(discount_rate_flat * 100), "% tested as sensitivity analysis."
  )

  fn <- list(
    abbrev = "QALY = quality-adjusted life year.",
    notes = paste0(
      "Stepped schedule mandated by Norwegian Ministry of Finance (NOU 2012:16). ",
      "Discount weights evaluated at cycle midpoint per Briggs et al. (2006) Ch. 2. ",
      "Flat ", round(discount_rate_flat * 100),
      "% rate retained as sensitivity scenario."
    ),
    source = paste0(
      "Source: Rundskriv R-109 (2021); NOU 2012:16; ",
      "DMP submission guidelines (2026) Section 12.8."
    )
  )

  list(df = df, table_number = 21, caption = caption,
       footnotes = fn, col_align = c("l", "r", "l"))
}


# --- Shared deflator constant -------------------------------------------------
# SOURCE: SSB Table 08981 annual averages (2015 = 100), 2013 = 95.9, 2024 = 133.6.
# DECISION: the end-of-life cost keeps Bjørnelv's original 2013 NOK basis. Kenseth
#   2024 Table 1 reprints 118,215 untransformed (identical at all six significant
#   figures to Bjørnelv Additional file 1 Table S1), so no 2021 indexation is
#   applied to it. Matches c_terminal in 00-parameters.R (118,215 x 133.6 / 95.9).
# DECISION: file scope, not local to one builder: tables 22 and 23 both print this
#   factor, and a second representation is what let them disagree.
cpi_2013_to_2024 <- 133.6 / 95.9

# DECISION: file scope, not local to one builder: tables 7 and 22 both print these
#   parameter labels and Figure 4.4 prints the same ones, and a second
#   representation is what let a table and its own figure disagree.
# SOURCE: the strings are byte-identical to the `param_labels` map inside
#   plot_evppi_bar() in R/07-visualization.R for every key both carry; G21 gates it.
param_display_labels <- c(
  HR_OS                = "Hazard ratio, OS",
  HR_DFS               = "Hazard ratio, DFS",
  u_dfs                = "Utility, disease-free",
  u_prog               = "Utility, progressed",
  c_progressed_annual  = "Shared progressed-disease annual cost",
  c_terminal           = "Terminal care cost",
  c_surveillance_early = "Shared surveillance annual cost",
  c_exercise_annual    = "Exercise-programme annual cost",
  c_intervention_setup = "Exercise programme setup cost",
  c_surveillance_late  = "Late surveillance annual cost",
  "HR_OS + HR_DFS (joint)" = "Hazard ratios, OS + DFS (joint)"
)

# --- Table 22: PSA Distributions ---------------------------------------------
build_psa_dists_table <- function() {
  df <- data.frame(
    Parameter = unname(param_display_labels[c(
      "c_surveillance_early",
      "c_surveillance_late",
      "c_exercise_annual",
      "c_progressed_annual",
      "c_terminal",
      "c_intervention_setup"
    )]),
    Mean = c(
      format_nok(c_surveillance_early),
      format_nok(c_surveillance_late),
      format_nok(c_exercise_annual),
      format_nok(c_progressed_annual),
      format_nok(c_terminal),
      format_nok(c_intervention_setup)
    ),
    SE = c(
      format_nok(c_surveillance_early_se),
      "-",
      format_nok(c_exercise_annual_se),
      format_nok(c_progressed_annual_se),
      format_nok(c_terminal_se),
      format_nok(c_intervention_setup_se)
    ),
    Distribution = c("Gamma", "Fixed zero", rep("Gamma", 4)),
    `Uncertainty basis` = c(
      "Assumed SE: 20% of mean",
      "Fixed zero: not sampled",
      "Assumed SE: 20% of mean",
      "Assumed SE: 20% of mean",
      # SOURCE: Bjornelv 2020 supplement Table S1: last-month SD 88,704,
      #   n 7,695 (2013 NOK); SE of mean via SD/sqrt(n).
      "Empirical SE: published SD/sqrt(n)",
      "Assumed SE: 20% of mean"
    ),
    Rationale = c(
      "Assumed SE = 20% of mean; Gamma fitted by moment matching per Briggs et al. 2006",
      "Active zero after surveillance cutoff",
      "Assumed SE = 20% of mean; Gamma fitted by moment matching per Briggs et al. 2006",
      "SE = 20% of mean",
      "SE = SD/sqrt(n), Bjørnelv 2020 Table S1",
      "SE = 20% of mean"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- "Cost-parameter distributions used in the probabilistic analysis."

  fn <- list(
    abbrev = "SE = standard error; SD = standard deviation; NOK = Norwegian kroner.",
    pricebasis = "Mean and SE are in 2024 NOK, except the July-2025 Helfo tariffs, the 2026 Pasientreiser mileage rate, and values composed from them, at face value.",
    notes = paste0(
      "All sampled cost distributions are Gamma; late surveillance is fixed ",
      "at zero. Gamma shape = (mean/SE)^2; rate = mean/SE^2."
    ),
    source = "Source: Model inputs; CHEERS 2022 Item 14."
  )

  list(df = df, table_number = 22, caption = caption,
       tabular_colspec_override = paste0(
         ">{\\raggedright\\arraybackslash}p{2.45cm}",
         ">{\\raggedleft\\arraybackslash}p{1.15cm}",
         ">{\\raggedleft\\arraybackslash}p{1.05cm}",
         ">{\\raggedright\\arraybackslash}p{1.40cm}",
         ">{\\raggedright\\arraybackslash}p{2.40cm}",
         ">{\\raggedright\\arraybackslash}p{2.80cm}"),
       footnotes = fn, col_align = c("l", "r", "r", "l", "l", "l"))
}


# --- Table 23: Deflator Methodology ------------------------------------------
build_deflator_table <- function() {
  df <- data.frame(
    Source = c(
      "Helfo A3/A1d tariffs",
      "Thorsen OUS labour anchor",
      "DMP enhetskostnader v1.6",
      "Pasientreiser rate",
      "Kenseth CT unit costs",
      "Joranger PD costs",
      "Bjørnelv 2020 EOL cost",
      "Mulder 2022 utilities",
      "Färkkilä 2014 utilities",
      "Garratt 2022 norms",
      "Courneya 2025 HRs"
    ),
    `Price-date status` = c(
      "July 2025 tariff (current)",
      "No price year stated",
      "2024 NOK (current)",
      "2026 rate (current)",
      "2021 NOK",
      "2011 EUR",
      "2013 NOK",
      "Not applicable",
      "Not applicable",
      "Not applicable",
      "Not applicable"
    ),
    `Deflator applied to 2024 NOK` = c(
      "None needed",
      "None applied",
      "None needed",
      "None needed",
      paste0("CPI 2021-2024 (", format(cpi_2021_to_2024, nsmall = 4), ")"),
      paste0("EUR x 7.79; CPI 2011-2024 (",
             format(cpi_2011_to_2024, nsmall = 4), ")"),
      paste0("CPI 2013-2024 (",
             format(round(cpi_2013_to_2024, 5), nsmall = 4), ")"),
      "None (utility, not cost)",
      "None (utility, not cost)",
      "None (utility, not cost)",
      "None (clinical, not cost)"
    ),
    Rationale = c(
      "Regulated rates in force from 1 July 2025, entered at face value (see Methods)",
      paste0("Hourly labour rate from the Oslo University Hospital cost ",
             "estimate for Nye metoder ID2025_092 (L. Thorsen, personal ",
             "communication, April 11, 2026); the estimate states no price ",
             "year, and the anchor is entered at face value in structural ",
             "sensitivity analysis only"),
      "Unit cost database published in 2024 NOK",
      "Regulated standardsats in force from 1 January 2026, entered at face value (see Methods)",
      "Published in 2021 prices; CPI from SSB Table 08981",
      "Joranger OR1 first-year cost; source conversion 7.79 NOK/EUR",
      paste0("Kenseth 2024 states all costs were indexed to 2021, but its Table 1 ",
             "last-month-of-life value equals Bjørnelv's 2013 NOK figure exactly; ",
             "the 2013 basis is therefore used"),
      "Utilities are dimensionless",
      "Utilities are dimensionless",
      "Population norms, not costs",
      "Hazard ratios are dimensionless"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- paste0(
    "Deflator Methodology by Source. ",
    "Per-source deflator classification for every cost input that is not ",
    "already in 2024 NOK, with the CPI factors applied to each."
  )

  fn <- list(
    abbrev = paste0(
      "CPI = consumer price index; NOK = Norwegian kroner; ",
      "SSB = Statistisk sentralbyrå; DMP = Norwegian Medical Products Agency; ",
      "HR = hazard ratio; CT = computed tomography; EOL = end of life; ",
      "EUR = euro; Helfo = Helseøkonomiforvaltningen (Norwegian Health ",
      "Economics Administration); PD = progressed disease; ",
      "OUS = Oslo University Hospital."
    ),
    pricebasis = paste0(
      "Cost inputs are in 2024 NOK, except the July-2025 Helfo tariffs, the ",
      "2026 Pasientreiser mileage rate and the Thorsen OUS labour anchor, ",
      "at face value."
    ),
    notes = paste0(
      "CPI ratios computed from SSB Table 08981 annual averages (2015 = 100): ",
      "2011 index 93.3, 2013 index 95.9, 2021 index 116.1, 2024 index 133.6. ",
      "The Bjørnelv end-of-life cost is indexed from its 2013 NOK basis to 2024; ",
      "Kenseth et al. (2024) Table 1 reprints the same 118,215 NOK value."
    ),
    source = paste0(
      "Source: SSB Table 08981; Joranger et al. (2020); ",
      "Kenseth et al. (2024); Bjørnelv et al. (2020); ",
      "L. Thorsen (personal communication, April 11, 2026)."
    )
  )

  list(df = df, table_number = 23, caption = caption,
       footnotes = fn, col_align = c("l", "l", "l", "l"),
       # Rotated width must fit the 237mm typeblock height. Unconstrained l
       # columns rendered ~840pt wide and bled glyphs off both sheet edges.
       tabular_colspec_override = paste0(
         ">{\\raggedright\\arraybackslash}p{4.3cm}",
         ">{\\raggedright\\arraybackslash}p{2.6cm}",
         ">{\\raggedright\\arraybackslash}p{3.6cm}",
         ">{\\raggedright\\arraybackslash}p{9.5cm}"),
       page_layout = "sideways-single")
}


# --- Table 24: Waning Parameters ----------------------------------------------
build_waning_table <- function() {
  df <- data.frame(
    Parameter = c(
      "Waning type",
      "Step waning year",
      "Linear waning start",
      "Linear waning end"
    ),
    `Base case` = c(
      "None (constant HR)",
      "N/A",
      "N/A",
      "N/A"
    ),
    `Scenario values` = c(
      "Step; Linear",
      paste0("Year ", waning_step_year, " (HR resets to 1.0)"),
      paste0("Year ", waning_linear_start),
      paste0("Year ", waning_linear_end, " (HR reaches 1.0)")
    ),
    Source = c(
      "Taylor et al., 2024; Jennings et al., 2024",
      "Modelling assumption",
      "Modelling assumption",
      "Modelling assumption"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- paste0(
    "Treatment Effect Waning Parameters. ",
    "Base case: no waning (constant HR). ",
    "Step waning at year ", waning_step_year,
    "; linear waning from year ", waning_linear_start,
    " to year ", waning_linear_end, " tested in structural SA."
  )

  fn <- list(
    abbrev = paste0(
      "HR = hazard ratio; SA = sensitivity analysis."
    ),
    notes = paste0(
      "Waning framework follows Taylor et al. (2024) 6-step approach. ",
      "Step waning: HR instantaneously resets to 1.0. ",
      "Linear waning: the log hazard ratio interpolates linearly from the ",
      "treatment effect to 0, so the hazard ratio approaches 1.0 geometrically. ",
      # Direction at CURRENT model values (supersedes the historical note below):
      # both waning scenarios are cost-saving with reduced QALY gains -- dominant,
      # negative ICERs per the Table 4.7 convention, both cost-effective "Yes".
      # Constant HR remains the favourable assumption in net-benefit terms
      # (larger QALY gain dominates the NMB).
      # HISTORY: an earlier verification recorded positive waning ICERs
      # (base 107,832 era); those values were retired by the later cost
      # revisions. # SOURCE: thesis-table-04 rows 26-27 (structural-scenarios CSV).
      "Base case (no waning) maintains the trial hazard ratios over the lifetime ",
      "horizon and may overestimate long-term benefit; the step- and linear-waning ",
      "scenarios are cost-saving with reduced QALY gains ",
      "(dominant; see Table~\\ref{tab:table04}). ",
      "CHALLENGE median follow-up: 7.9 years."
    ),
    source = paste0(
      "Source: Taylor et al. (2024); Jennings et al. (2024)."
    )
  )

  list(df = df, table_number = 24, caption = caption,
       footnotes = fn, col_align = c("l", "l", "l", "l"))
}


# --- Table 25: Perspective Cost Breakdown -------------------------------------
build_perspective_costs_table <- function() {
  travel_time_cost <- round(c_patient_time_per_session - c_patient_time_rate)

  df <- data.frame(
    `Cost Component` = c(
      "Helfo A3 physiotherapy session fee, DMP-adjusted",
      "Helfo A1d initial assessment (one-time)",
      paste0("Pasientreiser travel (", format(mileage_rate_nok, nsmall = 2),
             " NOK/km x ", travel_distance_km, " km x 2)"),
      paste0("Patient time, exercise (", format_nok(c_patient_time_rate),
             " NOK/hr x ", exercise_time_hr, " hr)"),
      paste0("Patient time, travel (", format_nok(c_patient_time_rate),
             " NOK/hr x ~", round(travel_time_min / 60, 3), " hr)"),
      # SOURCE: Helfo A3a + 1 x A3b (FOR-2025-06-23-1196, from 1 July 2025);
      #   contact count from the trial's printed phase attendance rates.
      # DECISION: the priced behavioural-support component belongs in this
      #   table, as in tables 15, 16, 17 and 28.
      "Behavioural-support contact (A3a + 1 x A3b, 30 min)"
    ),
    `Base-Case Value (unit per row)` = c(
      paste0(format(dmp_takst_adjustment), " x ", format_nok(c_session_tariff_bare),
             " = ", format_nok(c_session_healthcare), " NOK"),
      paste0(format_nok(c_intervention_setup), " NOK (applied once)"),
      paste0(format_nok(c_travel_per_session), " NOK"),
      paste0(format_nok(c_patient_time_rate), " NOK"),
      paste0("approx ", format_nok(travel_time_cost), " NOK"),
      paste0(format_nok(c_behaviour_contact), " NOK per contact")
    ),
    Source = c(
      paste0("(Helfo A3a + ", n_a3b_units, " x A3b) x ",
             format(dmp_takst_adjustment),
             " (FOR-2025-06-23-1196, from 1 July 2025; ",
             "DMP v1.6 1.2 Forutsetninger row 27)"),
      "Helfo A1d tariff (FOR-2025-06-23-1196; from 1 July 2025)",
      "Pasientreiser (pasientreiseforskriften Section 21); Meld. St. 21 Section 4.3.8; DMP 2026 Section 12.5",
      "DMP enhetskostnader v1.6 Fritid rate (R-109; DMP 2026 Section 12.7.1)",
      "DMP enhetskostnader v1.6 Fritid rate (R-109; DMP 2026 Section 12.7.1)",
      "Helfo A3a + 1 x A3b (FOR-2025-06-23-1196; from 1 July 2025)"
    ),
    `Healthcare-Services-Only Perspective?` = c(
      "Yes", "Yes", "No (excluded)", "No (excluded)", "No (excluded)", "Yes"
    ),
    `Extended Perspective?` = c(
      "Yes", "Yes", "Yes (included)", "Yes (included)", "Yes (included)", "Yes"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- paste0(
    "Cost Component Breakdown: Extended vs Healthcare-Services-Only Perspective. ",
    "Supervised-session subtotals, extended perspective: approx ",
    format_nok(round(c_session_extended)),
    " NOK; healthcare-services-only perspective: ",
    format_nok(c_session_standard),
    " NOK. The behavioural-support contact is priced on its own schedule and ",
    "is not inside either subtotal."
  )

  fn <- list(
    abbrev = paste0(
      "NOK = Norwegian kroner; DMP = Norwegian Medical Products Agency; ",
      "Helfo = Helse\u00f8konomiforvaltningen (Norwegian Health Economics Administration)."
    ),
    pricebasis = PRICE_BASIS_NOTE,
    notes = paste0(
      "The extended perspective is the base case; the healthcare-services-only ",
      "perspective excludes patient travel and patient time. ",
      "Units are stated per row: the first five rows are per supervised exercise ",
      "session, except the initial assessment, which is one-time; the ",
      "behavioural-support contact is priced per contact on its own ",
      "adherence-adjusted schedule of ",
      format(n_behaviour_adherence, nsmall = 2), " contacts over 36 months."
    ),
    source = paste0(
      "Source: Meld. St. 21 (2024--2025); DMP submission guidelines (2026); ",
      "Rundskriv R-109."
    )
  )

  # Perspective costs table has long source strings in column 3 that wrap
  # awkwardly at the default l-column width. Use fixed p{...} widths for
  # all 5 columns so each column wraps cleanly. kableExtra's align arg
  # rejects p{...} specs; save_as_latex_table() substitutes the column
  # spec post-generation via tabular_colspec_override (emitted natively,
  # no post-processing required on rerun).
  list(df = df, table_number = 25, caption = caption,
       footnotes = fn, col_align = c("l", "l", "l", "l", "l"),
       tabular_colspec_override = paste0(
         ">{\\raggedright\\arraybackslash}p{4.5cm}",
         ">{\\raggedright\\arraybackslash}p{4cm}",
         ">{\\raggedright\\arraybackslash}p{8cm}",
         ">{\\raggedright\\arraybackslash}p{3cm}",
         ">{\\raggedright\\arraybackslash}p{3cm}"),
       page_layout = "sideways-single")
}


# --- Table 26: Parametric Distribution Selection (DFS, OS) -------------------
# Compares six parametric distributions fitted to the control arm
# Kaplan-Meier data by AIC and BIC, following the Latimer (2013) NICE
# Technical Support Document 14 approach.
build_distribution_comparison_table <- function() {
  csv_path <- file.path("output", "tables", "aic_bic_comparison.csv")
  if (!file.exists(csv_path)) {
    message("08-export: AIC/BIC comparison CSV not found at ", csv_path,
            ". Skipping.")
    return(NULL)
  }

  raw <- read.csv(csv_path, stringsAsFactors = FALSE)
  primary <- raw[raw$arm == "Standard Care (PRIMARY)", ]

  dist_label <- c(
    exp       = "Exponential",
    weibull   = "Weibull",
    lnorm     = "Log-normal",
    llogis    = "Log-logistic",
    gompertz  = "Gompertz",
    gengamma  = "Generalised gamma"
  )
  dist_order <- c("exp", "weibull", "lnorm", "llogis", "gompertz", "gengamma")

  dfs <- primary[primary$endpoint == "DFS", ]
  os  <- primary[primary$endpoint == "OS",  ]
  dfs <- dfs[match(dist_order, dfs$distribution), ]
  os  <- os[match(dist_order, os$distribution), ]
  rownames(dfs) <- NULL
  rownames(os)  <- NULL

  dfs$bic_rank <- rank(dfs$BIC, ties.method = "min")
  os$bic_rank  <- rank(os$BIC,  ties.method = "min")

  df <- data.frame(
    Distribution = unname(dist_label[dist_order]),
    `DFS AIC`    = format(round(dfs$AIC, 1), nsmall = 1, big.mark = ","),
    `DFS BIC`    = format(round(dfs$BIC, 1), nsmall = 1, big.mark = ","),
    `DFS rank`   = dfs$bic_rank,
    `OS AIC`     = format(round(os$AIC, 1), nsmall = 1, big.mark = ","),
    `OS BIC`     = format(round(os$BIC, 1), nsmall = 1, big.mark = ","),
    `OS rank`    = os$bic_rank,
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
  )

  best_dfs <- unname(dist_label[dfs$distribution[which.min(dfs$BIC)]])
  best_os  <- unname(dist_label[os$distribution[which.min(os$BIC)]])

  # SOURCE: label generated by biblatex from the cite key, never hard-typed --
  # references.bib holds one Latimer 2013 entry, so a typed "2013a" suffix
  # points at a bibliography label that is never printed.
  caption <- paste0(
    "Parametric Distribution Selection by AIC and BIC. ",
    "Six candidate distributions fitted to the standard-care arm for DFS ",
    "and OS endpoints. ",
    "Lowest BIC: ", best_dfs, " for DFS, ", best_os, " for OS. ",
    "Log-normal applied to both endpoints on grounds of parsimony, ",
    "biological plausibility, and visual fit to the Kaplan-Meier curves."
  )

  fn <- list(
    abbrev = paste0(
      "AIC = Akaike information criterion; ",
      "BIC = Bayesian information criterion; ",
      "DFS = disease-free survival; OS = overall survival."
    ),
    notes = paste0(
      "Distributions fitted using maximum likelihood estimation via the ",
      "flexsurv R package. BIC preferred over AIC as the primary selection ",
      "criterion (penalises complexity more heavily). Differences within 2 ",
      "BIC points are considered negligible evidence of a superior fit."
    ),
    source = paste0(
      "Source: Parametric survival model fitting (R code, supplementary material); ",
      "distribution selection following NICE DSU Technical Support Document 14 ",
      "\\parencite{latimer2013}."
    )
  )

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(df = df, table_number = 26, caption = caption,
       footnotes = fn,
       col_align = c("l", "r", "r", "r", "r", "r", "r"))
}


# --- Table 27: Discounting and Analytical Framework --------------------------
# SOURCE: R/00-parameters.R named parameters + R/06-psa.R technology horizons
# All values read from R globals (zero hardcoding)
# GUIDELINE: Rundskriv R-109 (2021); Magnussen et al. (2015); DMP 2026
build_framework_table <- function() {
  # Read the executable schedule and its separate public labels from the one
  # default definition; do not duplicate internal endpoints in this generator.
  disc_brackets <- formals(create_discount_weights)$brackets
  if (is.call(disc_brackets)) disc_brackets <- eval(disc_brackets)

  # Technology decision horizons from compute_pop_evpi_horizon
  # SOURCE: the `horizons` default argument of compute_pop_evpi_horizon() in
  #   R/06-psa.R (Briggs et al. 2006; Fenwick et al. 2020)
  tech_horizons <- c(5, 10, 20)
  tech_base <- 10

  df <- data.frame(
    Parameter = c(
      sub("^Years ", "Discount rate years ", disc_brackets$display_label),
      "Perspective",
      "Severity category",
      "Cost-effectiveness threshold",
      "PA iterations",
      "Technology decision horizon",
      "EQ-5D instrument",
      "Value set"
    ),
    Value = c(
      paste0(disc_brackets$rate[1] * 100, "%"),
      paste0(disc_brackets$rate[2] * 100, "%"),
      paste0(disc_brackets$rate[3] * 100, "%"),
      "Extended healthcare (utvidet helsetjenesteperspektiv)",
      # GUIDELINE: severity class computed from absolute shortfall (DMP; Magnussen et al. 2015)
      sprintf("Category %d (%.2f QALY shortfall)", severity$group, severity$absolute_shortfall),
      paste0(format(wtp_threshold, big.mark = ","), " NOK/QALY"),
      format(n_psa, big.mark = ","),
      paste0(tech_base, " years (base); ",
             paste(tech_horizons[tech_horizons != tech_base],
                   collapse = ", "),
             " years (sensitivity)"),
      "EQ-5D-5L",
      "Dutch 5L (DFS), Danish 5L (PD), Norwegian 3L crosswalk (age norms)"
    ),
    Source = c(
      "Rundskriv R-109",
      "Rundskriv R-109",
      "Rundskriv R-109",
      "Meld. St. 21 (2024--2025) Section 4.3.8; DMP 2026 Sections 12.5, 12.7.1; Appendix A.11",
      "Magnussen et al., 2015",
      "Magnussen et al., 2015",
      "Hatswell et al., 2018",
      "Briggs et al. 2006; Fenwick et al. 2020",
      "DMP submission guidelines (July 2026)",
      "Mulder 2022; Torkilseng 2025; Garratt 2022"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  caption <- paste0(
    "Discounting and Analytical Framework. ",
    "Stepped discount schedule per Rundskriv R-109. ",
    "Extended healthcare perspective as base case. ",
    "Cost-effectiveness threshold ", format(wtp_threshold, big.mark = ","),
    " NOK/QALY (severity Category ", severity$group, "). ",
    format(n_psa, big.mark = ","), " PA iterations."
  )

  fn <- list(
    abbrev = paste0(
      "QALY = quality-adjusted life year; ",
      "NOK = Norwegian kroner; PA = probabilistic analysis; ",
      "DMP = Norwegian Medical Products Agency; ",
      "EQ-5D-5L = EuroQol 5-dimension 5-level; ",
      "EQ-5D = EuroQol five-dimension questionnaire; ",
      "DFS = disease-free survival; PD = progressed disease."
    ),
    notes = paste0(
      "Severity class derived from absolute shortfall method (Magnussen et al., 2015). ",
      "Extended perspective includes patient travel and patient time at leisure-time ",
      "wage rate per Rundskriv R-109."
    ),
    source = paste0(
      "Source: Rundskriv R-109 (2021); Meld. St. 21 (2024--2025); ",
      "DMP submission guidelines (2026); Magnussen et al. (2015)."
    )
  )

  ft <- flextable::flextable(df) |>
    format_thesis_table(caption_text = caption, table_number = 27) |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(df = df, table_number = 27, caption = caption,
       footnotes = fn, col_align = c("l", "l", "l"),
       tabular_colspec_override = ">{\\raggedright\\arraybackslash}p{3.2cm}>{\\raggedright\\arraybackslash}p{3.2cm}>{\\raggedright\\arraybackslash}p{5.2cm}")
}


# --- Table 28: Model Development Iterations (Version History) ----------------
# DATA-DRIVEN: historical versions read from data/raw/version-history.csv.
# Current version appended programmatically from R model output.
# To add a new version: freeze the previous version's values in the CSV,
# then update current_version below. The R code never needs editing for
# historical values.
build_version_history_table <- function() {
  if (!exists("base_case_results") || !exists("psa_summary")) {
    message("08-export: base_case_results or psa_summary not in memory. ",
            "Skipping version history table.")
    return(NULL)
  }

  # --- Current version metadata ---
  current_version <- "Current"  # Timestamped in code: 2026-04-15
  current_key_change <- paste0(
    "Reference sampling uses endpoint-specific independence; positive ",
    "complete-row copula dependence is a separate specification sensitivity. ",
    "Surveillance, exercise-programme, and progressed-disease costs and ",
    "progressed-state utility uncertainty use corrected component ownership. ",
    "The programme cost now also prices the trial's behavioural-support ",
    "contacts, which were previously unpriced.")

  # --- Current version values from R model outputs (programmatic) ---
  current_icer <- format_nok(base_case_results$icer_nok_qaly[3])
  current_psa  <- format_nok(round(psa_summary$expected_icer))
  current_pce  <- paste0(round(psa_summary$prob_ce * 100, 1), "%")

  # --- Historical records from data file (frozen, single source of truth) ---
  history_path <- file.path("data", "raw", "version-history.csv")
  if (!file.exists(history_path)) {
    stop("build_version_history_table: required version-history.csv is missing")
  }
  history <- read.csv(history_path, stringsAsFactors = FALSE)
  required_history_columns <- c("version", "key_change", "icer", "psa_mean", "pce")
  if (!identical(names(history), required_history_columns) ||
      nrow(history) == 0L || anyNA(history)) {
    stop("build_version_history_table: invalid version-history.csv schema or rows")
  }

  # Format historical ICER/PSA values with thousand separators
  format_hist <- function(x) {
    num <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x)))
    ifelse(is.na(num), x, format_nok(num))
  }
  history$icer_fmt <- sapply(history$icer, function(v) {
    if (grepl("dominant", v, ignore.case = TRUE)) {
      num <- as.numeric(gsub("[^0-9.-]", "", v))
      paste0(format_nok(num), " (dominant)")
    } else {
      format_nok(as.numeric(v))
    }
  })
  history$psa_fmt <- sapply(history$psa_mean, function(v) {
    num <- as.numeric(gsub("[^0-9.-]", "", v))
    format_nok(num)
  })

  versions <- data.frame(
    Version = c(history$version, current_version),
    `Key Change` = c(history$key_change, current_key_change),
    `ICER (NOK/QALY)` = c(history$icer_fmt, current_icer),
    `PA Mean` = c(history$psa_fmt, current_psa),
    # Header must not name a threshold: historical rows were recorded under
    # the then-applicable 495k reference, since superseded, and the current
    # row is computed at the live wtp_threshold, the severity-informed 385k
    # (275k opportunity-cost benchmark x Magnussen severity weight 1.4).
    `P(CE) at prevailing threshold` = c(history$pce, current_pce),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  n_versions <- nrow(versions)
  caption <- paste0(
    "Summary of Model Calibration and Development.")

  fn <- list(
    abbrev = paste0(
      "ICER = incremental cost-effectiveness ratio; ",
      "NOK = Norwegian kroner; QALY = quality-adjusted life year; ",
      "PA = probabilistic analysis; ",
      "CE = cost-effective; HR = hazard ratio; ",
      "SE = standard error; SD = standard deviation; ",
      "SA = sensitivity analysis; SSB = Statistisk sentralbyrå; ",
      "CEAF = cost-effectiveness acceptability frontier; ",
      "CHEERS = Consolidated Health Economic Evaluation Reporting Standards; ",
      "EQ-5D = EuroQol five-dimension questionnaire; ",
      "Helfo = Helseøkonomiforvaltningen (Norwegian Health Economics ",
      "Administration)."
    ),
    pricebasis = PRICE_BASIS_NOTE,
    notes = paste0(
      "Dominant = lower cost and higher QALYs. ",
      "P(CE) is evaluated at the threshold applying to each version: historical ",
      "rows against the then-applicable 495,000 NOK/QALY reference, the current ",
      "version at ", format(wtp_threshold, big.mark = ","),
      " NOK/QALY (severity Category ", severity$group, "). ",
      "Each row's printed results belong to its own version and are superseded ",
      "by the current version."
    ),
    source = paste0(
      "Source: R model output (current version); model development records."
    )
  )

  ft <- flextable::flextable(versions) |>
    format_thesis_table(caption_text = caption, table_number = 28) |>
    flextable::align(j = 3:5, align = "right", part = "body") |>
    add_thesis_footnotes(fn)

  cat("PROGRAMMATIC CAPTION:\n")
  cat(caption, "\n\n")

  list(df = versions, table_number = 28, caption = caption,
       footnotes = fn, col_align = c("l", "l", "r", "r", "r"),
       # DECISION: no per-table TYPE size. Every
       # column below is capped, so the uniform 9 fits them unchanged.
       # Caution: the LEADING alone steps 11 -> 10.5 here. At 11 this table plus
       # its four notes overran the page by 22.07pt (Overfull \vbox on folio
       # 75; the note block printed over the folio). At 10.5 the block clears
       # the typeblock by 18.71pt with the type size, the column widths and
       # every content byte unchanged. Do not restore 11 without re-measuring.
       fontsize_override = "9}{10.5",
       tabular_colspec_override = paste0(
         ">{\\raggedright\\arraybackslash}p{1.2cm}",
         ">{\\raggedright\\arraybackslash}p{7.0cm}",
         ">{\\raggedright\\arraybackslash}p{2.5cm}",
         ">{\\raggedright\\arraybackslash}p{2.0cm}",
         ">{\\raggedright\\arraybackslash}p{1.5cm}"))
}


# ==============================================================================
# MASTER EXPORT FUNCTION
# ==============================================================================

build_all_tables <- function(output_dir = TABLE_OUTPUT_DIR) {

  cat("=== GENERATING THESIS TABLES ===\n\n")

  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  assert_deterministic_docx_export_v1()
  cat("Deterministic DOCX fixture: PASS\n")

  builders <- list(
    build_deterministic_table,
    build_psa_summary_table,
    build_convergence_table,
    build_structural_sa_table,
    build_owpsa_table,
    build_evpi_table,
    build_evppi_table,
    build_validation_table,
    build_subgroup_sa_table,
    build_internal_validation_table,
    build_quadrant_table,
    build_disaggregated_table,
    build_perspective_table,
    build_psa_specification_sensitivity_table
  )

  manifest <- list()
  saved_count <- 0

  for (builder in builders) {
    result <- tryCatch(builder(), error = function(e) {
      warning("Table builder failed: ", e$message, call. = FALSE)
      NULL
    })

    if (!is.null(result)) {
      save_path <- file.path(output_dir, result$path)
      save_as_deterministic_docx_v1(result$ft, path = save_path)
      cat("Saved:", result$path, "\n")
      saved_count <- saved_count + 1

      # LaTeX export (alongside DOCX)
      if (!is.null(result$df) && !is.null(result$table_number)) {
        tryCatch(
          save_as_latex_table(
            df = result$df,
            table_number = result$table_number,
            caption_text = result$caption,
            caption_short = result$caption_short,
            caption_continued = result$caption_continued,
            footnotes = result$footnotes,
            col_align = result$col_align,
            fontsize_override = result$fontsize_override,
            makebox_width_override = result$makebox_width_override,
            tabular_colspec_override = result$tabular_colspec_override,
            latex_column_labels = result$latex_column_labels,
            latex_header_above = result$latex_header_above,
            page_layout = result$page_layout
          ),
          error = function(e) {
            warning("LaTeX export failed for table ", result$table_number,
                    ": ", e$message, call. = FALSE)
          }
        )
      }

      git_hash <- tryCatch({
        git_output <- system("git rev-parse HEAD", intern = TRUE,
                             ignore.stderr = TRUE)
        if (length(git_output) == 0 || nchar(trimws(git_output)) == 0)
          "not-a-git-repo"
        else trimws(git_output)
      },
        warning = function(w) "not-a-git-repo",
        error = function(e) "not-a-git-repo"
      )
      manifest[[result$path]] <- list(
        path = save_path,
        caption = result$caption,
        git_commit = git_hash,
        cheers_compliant = TRUE
      )
    }
  }

  # Input parameters table (LaTeX only, no DOCX)
  ip_result <- tryCatch(build_input_parameters_table(), error = function(e) {
    warning("Input parameters table failed: ", e$message, call. = FALSE)
    NULL
  })
  if (!is.null(ip_result) && !is.null(ip_result$df)) {
    save_as_latex_table(
      df = ip_result$df,
      table_number = ip_result$table_number,
      caption_text = ip_result$caption,
      caption_continued = ip_result$caption_continued,
      footnotes = ip_result$footnotes,
      col_align = ip_result$col_align,
      fontsize_override = ip_result$fontsize_override,
      makebox_width_override = ip_result$makebox_width_override,
      tabular_colspec_override = ip_result$tabular_colspec_override,
      latex_column_labels = ip_result$latex_column_labels,
      latex_header_above = ip_result$latex_header_above,
      page_layout = ip_result$page_layout
    )
    saved_count <- saved_count + 1
  }

  # Appendix tables 16-31 (LaTeX only, no DOCX)
  appendix_builders <- list(
    build_exercise_costs_table,      # Table 16
    build_adherence_table,           # Table 17
    build_sa_anchors_table,          # Table 18
    build_df_costs_table,            # Table 19
    build_pd_costs_table,            # Table 20
    build_discount_schedule_table,   # Table 21
    build_psa_dists_table,           # Table 22
    build_deflator_table,            # Table 23
    build_waning_table,              # Table 24
    build_perspective_costs_table,   # Table 25
    build_distribution_comparison_table, # Table 26
    build_framework_table,            # Table 27
    build_version_history_table,      # Table 28
    build_ceac_ceaf_wtp_table,        # Table 29 (consolidated CEAC/CEAF-by-WTP)
    build_survival_coefficients_table # Table 31
  )

  for (builder in appendix_builders) {
    ap_result <- tryCatch(builder(), error = function(e) {
      warning("Appendix table builder failed: ", e$message, call. = FALSE)
      NULL
    })
    if (!is.null(ap_result) && !is.null(ap_result$df)) {
      save_as_latex_table(
        df = ap_result$df,
        table_number = ap_result$table_number,
        caption_text = ap_result$caption,
        caption_short = ap_result$caption_short,
        footnotes = ap_result$footnotes,
        col_align = ap_result$col_align,
        fontsize_override = ap_result$fontsize_override,
        makebox_width_override = ap_result$makebox_width_override,
        tabular_colspec_override = ap_result$tabular_colspec_override,
        latex_column_labels = ap_result$latex_column_labels,
        latex_header_above = ap_result$latex_header_above,
        page_layout = ap_result$page_layout
      )
      saved_count <- saved_count + 1
    }
  }

  manifest_path <- file.path(output_dir, "manifest.json")
  writeLines(
    jsonlite::toJSON(manifest, pretty = TRUE, auto_unbox = TRUE),
    manifest_path
  )
  cat("\nManifest:", manifest_path, "\n")

  cat("\n=== ALL THESIS TABLES GENERATED ===\n")
  cat("Total:", saved_count, "tables saved to", output_dir, "\n")

  invisible(manifest)
}


# ==============================================================================
# TEXT SNIPPET GENERATORS
# Programmatic interpretation strings from R objects
#
# REFERENCE CODE PROVENANCE:
#   Dominance statement pattern: adapted from HEVAL5200 course material
#   Model selection pattern: adapted from HEVAL5200 course material
#   Significance patterns noted for reference from HEVAL5200 course material
#     but not adapted here (thesis PSA outputs do not include p-value testing)
#   All adapted to thesis context (CRC exercise intervention, Norwegian perspective)
#
# Output: one .txt file per claim in output/text-snippets/
# ==============================================================================

SNIPPET_OUTPUT_DIR <- "output/text-snippets"

# ANNUAL_OPERATED_STAGE_MIX_COHORT is defined in R/00-parameters.R as the single
# source of truth. This file does not redefine it. The constant is loaded
# via the main.R source chain (00-parameters.R sources before
# 08-export-tables.R), so all references below resolve to the
# parameter-file value.
# Modelled annual cohort size: all 762 operated stage III colon cancer cases
# (CRC Quality Registry 2024, Figure 2.1, p.12) divided by the 90.2 percent
# stage III share from CHALLENGE Table 1 gives 845. No adjuvant-treatment
# filter is applied. The cohort-matched analogue based on recorded receipt or
# start of adjuvant chemotherapy is approximately 472, a receipt-based
# upper-bound proxy affected by the case-to-patient limitation; the completion
# count is unknown because the registry does not report it.
stopifnot(exists("ANNUAL_OPERATED_STAGE_MIX_COHORT"))


# --- Snippet 1: ICER interpretation ------------------------------------------
# Adapted from HEVAL5200 dominance statement pattern + CE verdict
# SOURCE: psa_summary (R/06-psa.R), wtp_threshold (R/00-parameters.R)
# PSA is the base-case analysis method
# GUIDELINE: DMP severity-weighted WTP (Magnussen et al. 2015)
build_snippet_icer_interpretation <- function() {
  if (!exists("psa_summary") || !exists("wtp_threshold")) {
    message("snippet: PSA summary or cost-effectiveness threshold not in memory. Skipping.")
    return(NULL)
  }

  icer <- psa_summary$expected_icer
  wtp <- wtp_threshold
  inc_cost <- psa_summary$expected_inc_cost
  inc_qaly <- psa_summary$expected_inc_qaly
  prob_ce <- psa_summary$prob_ce

  if (inc_cost < 0 && inc_qaly > 0) {
    verdict <- "dominant (lower costs and higher QALYs)"
  } else if (inc_cost > 0 && inc_qaly < 0) {
    verdict <- "dominated (higher costs and lower QALYs)"
  } else if (inc_cost > 0 && inc_qaly > 0) {
    if (icer <= wtp) {
      verdict <- paste0(
        "cost-effective (ICER of NOK ", format_nok(icer),
        "/QALY below the cost-effectiveness threshold of NOK ", format_nok(wtp), "/QALY)")
    } else {
      verdict <- paste0(
        "not cost-effective (ICER of NOK ", format_nok(icer),
        "/QALY exceeds the cost-effectiveness threshold of NOK ", format_nok(wtp), "/QALY)")
    }
  } else {
    verdict <- paste0(
      "cost-saving but with fewer QALYs (ICER of NOK ", format_nok(icer), "/QALY)")
  }

  snippet <- paste0(
    "The exercise intervention is ", verdict,
    " compared with standard care from the Norwegian extended healthcare ",
    "perspective. The expected (probabilistic) ICER is NOK ",
    format_nok(icer), " per QALY gained (N = ",
    format(psa_summary$n_iterations, big.mark = ","),
    " PSA iterations, effective discount rate 4% per Rundskriv R-109 ",
    "stepped schedule). At the applicable Norwegian cost-effectiveness threshold of NOK ",
    format_nok(wtp), " per QALY (severity Category ", severity$group, ", Magnussen et al. 2015), ",
    "the probability of cost-effectiveness is ",
    round(prob_ce * 100, 1), "%.")

  cat("TEXT SNIPPET: results-icer-interpretation.txt\n")
  list(text = snippet, path = "results-icer-interpretation.txt")
}


# --- Snippet 2: PSA verdict at multiple WTP thresholds -----------------------
# SOURCE: psa_results (R/06-psa.R), wtp_threshold (R/00-parameters.R)
# GUIDELINE: DMP severity-weighted WTP categories (Magnussen et al. 2015)
build_snippet_psa_verdict <- function() {
  if (!exists("psa_results") || !exists("wtp_threshold")) {
    message("snippet: PSA results or cost-effectiveness threshold not in memory. Skipping.")
    return(NULL)
  }

  wtp_categories <- c(275000, 385000, 495000, 605000, 715000, 825000)
  cat_names <- c("Category 1", "Category 2", "Category 3",
                 "Category 4", "Category 5", "Category 6")

  probs <- sapply(wtp_categories, function(w) {
    inb <- psa_results$inc_qalys * w - psa_results$inc_costs
    mean(inb[!is.na(inb)] >= 0)
  })

  prob_lines <- paste0(
    "  ", cat_names, " (NOK ", format(wtp_categories, big.mark = ","),
    "/QALY): ", round(probs * 100, 1), "%")

  wtp_ref <- if (exists("wtp_threshold")) wtp_threshold else opportunity_cost_nok
  ref_idx <- which.min(abs(wtp_categories - wtp_ref))

  snippet <- paste0(
    "Probability of cost-effectiveness by Norwegian severity category ",
    "(N = ", format(nrow(psa_results), big.mark = ","),
    " PSA iterations, extended healthcare perspective, ",
    "effective discount rate 4% per Rundskriv R-109 stepped schedule):\n\n",
    paste(prob_lines, collapse = "\n"), "\n\n",
    "At the applicable cost-effectiveness threshold of NOK ",
    format(wtp_threshold, big.mark = ","),
    " per QALY (severity Category ", severity$group, "), P(CE) = ",
    round(probs[ref_idx] * 100, 1), "%.")

  cat("TEXT SNIPPET: results-psa-verdict.txt\n")
  list(text = snippet, path = "results-psa-verdict.txt")
}


# --- Snippet 3: Dominance statement ------------------------------------------
# Adapted from HEVAL5200 four-branch dominance pattern
# SOURCE: psa_summary (R/06-psa.R)
# PSA is the base-case analysis method
build_snippet_dominance_statement <- function() {
  if (!exists("psa_summary")) {
    message("snippet: PSA summary not in memory. Skipping.")
    return(NULL)
  }

  inc_cost <- psa_summary$expected_inc_cost
  inc_qaly <- psa_summary$expected_inc_qaly
  icer <- psa_summary$expected_icer

  if (inc_cost < 0 && inc_qaly > 0) {
    snippet <- paste0(
      "The exercise intervention dominates standard care (lower expected ",
      "costs by NOK ", format_nok(abs(inc_cost)),
      " and higher expected QALYs by ", round(inc_qaly, 4), ").")
  } else if (inc_cost > 0 && inc_qaly < 0) {
    snippet <- paste0(
      "The exercise intervention is dominated by standard care (higher ",
      "expected costs by NOK ", format_nok(inc_cost),
      " and lower expected QALYs by ", round(abs(inc_qaly), 4), ").")
  } else if (inc_cost > 0 && inc_qaly > 0) {
    snippet <- paste0(
      "The exercise intervention costs more (incremental cost NOK ",
      format_nok(inc_cost), ") but provides additional QALYs (",
      round(inc_qaly, 4), "); ICER of NOK ",
      format_nok(icer), " per QALY gained.")
  } else {
    snippet <- paste0(
      "The exercise intervention costs less (saving NOK ",
      format_nok(abs(inc_cost)), ") but provides fewer QALYs (",
      round(abs(inc_qaly), 4), " fewer); ICER of NOK ",
      format_nok(icer), " per QALY gained.")
  }

  cat("TEXT SNIPPET: results-dominance-statement.txt\n")
  list(text = snippet, path = "results-dominance-statement.txt")
}


# --- Snippet 4: OWPSA top drivers -------------------------------------------
# SOURCE: all_results (main.R step 8b OWPSA output)
# PSA is the base-case analysis method
build_snippet_dsa_drivers <- function() {
  if (!exists("all_results")) {
    if (file.exists(file.path(TABLE_OUTPUT_DIR, "owpsa_v7_results.csv"))) {
      owpsa <- read.csv(file.path(TABLE_OUTPUT_DIR, "owpsa_v7_results.csv"),
                        stringsAsFactors = FALSE)
    } else {
      message("snippet: OWPSA results not available. Skipping.")
      return(NULL)
    }
  } else {
    owpsa <- all_results
  }

  # Sort by range descending (most influential first)
  owpsa <- owpsa[order(-owpsa$range), ]
  top3 <- head(owpsa, 3)

  driver_lines <- paste0(
    "  ", seq_len(nrow(top3)), ". ", top3$label,
    " (INMB range: NOK ", format(round(top3$range), big.mark = ","), ")")

  snippet <- paste0(
    "Key drivers of cost-effectiveness (OWPSA, top 3 by INMB range):\n\n",
    paste(driver_lines, collapse = "\n"), "\n\n",
    "All ", nrow(owpsa), " parameters tested produced positive expected INMB ",
    "across both bounds, indicating robust cost-effectiveness.")

  # Verify all-positive claim
  if (any(owpsa$inmb_low < 0) || any(owpsa$inmb_high < 0)) {
    snippet <- sub(
      "All .* indicating robust cost-effectiveness\\.",
      paste0(sum(owpsa$inmb_low < 0 | owpsa$inmb_high < 0),
             " parameter bound(s) produced negative expected INMB, ",
             "indicating sensitivity to these parameters."),
      snippet)
  }

  cat("TEXT SNIPPET: results-dsa-drivers.txt\n")
  list(text = snippet, path = "results-dsa-drivers.txt")
}


# --- Snippet 5: Model selection statement ------------------------------------
# Adapted from HEVAL5200 programmatic model selection pattern
# SOURCE: best_dist_dfs, best_dist_os (R/00-parameters.R)
# GUIDELINE: NICE TSD 14 (supplementary, DMP/NOMA silent on fitting methodology)
build_snippet_model_selection <- function() {
  if (!exists("best_dist_dfs") || !exists("best_dist_os")) {
    message("snippet: Survival distribution selections not in memory. Skipping.")
    return(NULL)
  }

  dist_names <- c(
    exp = "exponential", weibull = "Weibull", lnorm = "log-normal",
    llogis = "log-logistic", gompertz = "Gompertz", gengamma = "generalised gamma")

  dfs_name <- if (best_dist_dfs %in% names(dist_names)) {
    dist_names[[best_dist_dfs]]
  } else best_dist_dfs

  os_name <- if (best_dist_os %in% names(dist_names)) {
    dist_names[[best_dist_os]]
  } else best_dist_os

  same_dist <- (best_dist_dfs == best_dist_os)

  aic_snippet <- ""
  aic_path <- file.path(TABLE_OUTPUT_DIR, "aic_bic_comparison.csv")
  if (file.exists(aic_path)) {
    aic_data <- read.csv(aic_path, stringsAsFactors = FALSE)
    dfs_ctrl <- aic_data[aic_data$endpoint == "DFS" &
                          grepl("Standard Care", aic_data$arm), ]
    os_ctrl <- aic_data[aic_data$endpoint == "OS" &
                         grepl("Standard Care", aic_data$arm), ]
    if (nrow(dfs_ctrl) > 0 && nrow(os_ctrl) > 0) {
      dfs_best_aic <- dfs_ctrl$distribution[which.min(dfs_ctrl$AIC)]
      dfs_best_bic <- dfs_ctrl$distribution[which.min(dfs_ctrl$BIC)]
      os_best_aic <- os_ctrl$distribution[which.min(os_ctrl$AIC)]
      os_best_bic <- os_ctrl$distribution[which.min(os_ctrl$BIC)]
      # Report AIC/BIC evidence for the selected distributions
      dfs_best_aic_name <- if (dfs_best_aic %in% names(dist_names))
        dist_names[[dfs_best_aic]] else dfs_best_aic
      dfs_best_bic_name <- if (dfs_best_bic %in% names(dist_names))
        dist_names[[dfs_best_bic]] else dfs_best_bic
      os_best_aic_name <- if (os_best_aic %in% names(dist_names))
        dist_names[[os_best_aic]] else os_best_aic
      os_best_bic_name <- if (os_best_bic %in% names(dist_names))
        dist_names[[os_best_bic]] else os_best_bic
      aic_snippet <- paste0(
        " Lowest BIC: ", dfs_best_bic_name, " (DFS), ",
        os_best_bic_name, " (OS). ",
        "Lowest AIC: ", dfs_best_aic_name, " (DFS), ",
        os_best_aic_name, " (OS). ",
        "Selection criterion: BIC among 2-parameter distributions, ",
        "clinical plausibility of long-term extrapolation, and parsimony ",
        "(NICE TSD 14, supplementary).")
    }
  }

  # Survival distribution selection based on BIC,
  # parsimony, and clinical plausibility (not AIC alone)
  if (same_dist) {
    snippet <- paste0(
      "The ", dfs_name, " distribution was selected for both DFS and OS ",
      "extrapolation based on BIC, parsimony, and clinical plausibility ",
      "of the long-term extrapolation (NICE TSD 14, supplementary).",
      aic_snippet)
  } else {
    snippet <- paste0(
      "The ", dfs_name, " distribution was selected for DFS and the ",
      os_name, " distribution for OS extrapolation, based on BIC, ",
      "parsimony, and clinical plausibility (NICE TSD 14, supplementary).",
      aic_snippet)
  }

  cat("TEXT SNIPPET: results-model-selection.txt\n")
  list(text = snippet, path = "results-model-selection.txt")
}


# --- Snippet 6: Severity narrative -------------------------------------------
# SOURCE: wtp_threshold (R/00-parameters.R)
# GUIDELINE: DMP absolute shortfall method (Magnussen et al. 2015)
build_snippet_severity_narrative <- function() {
  if (!exists("wtp_threshold")) {
    message("snippet: Cost-effectiveness threshold not in memory. Skipping.")
    return(NULL)
  }

  # GUIDELINE: applicable WTP maps to a Magnussen severity category via the
  #   reference ladder below (DMP; Magnussen et al. 2015). The category is looked
  #   up from the computed wtp_threshold (not hardcoded); the ladder enumerates
  #   all six bands and is the reference, not a claim about this indication.
  severity_map <- data.frame(
    # SOURCE: Magnussen et al. 2015, Table 3, p. 48, current six-group severity mapping
    category = 1:6,
    wtp = c(275000, 385000, 495000, 605000, 715000, 825000),
    # SOURCE: Magnussen et al. 2015, Table 3, p. 48, absolute-shortfall bands
    shortfall = c("<4 QALYs", "4-7.9 QALYs", "8-11.9 QALYs",
                  "12-15.9 QALYs", "16-19.9 QALYs", ">20 QALYs"),
    stringsAsFactors = FALSE
  )

  matched <- severity_map[severity_map$wtp == wtp_threshold, ]
  if (nrow(matched) == 1) {
    # NOU 2014:12 is a four-class historical precursor, not the owner of the
    # current six-group Magnussen/DMP mapping.
    snippet <- paste0(
      "The applicable cost-effectiveness threshold is NOK ",
      format(wtp_threshold, big.mark = ","),
      " per QALY, corresponding to severity category ", matched$category,
      " (absolute shortfall ", matched$shortfall,
      ") under the Norwegian priority-setting ",
      "framework (Magnussen et al. 2015). ",
      "This severity classification reflects the expected health loss for ",
      "stage III colorectal cancer patients relative to the general population.")
  } else {
    snippet <- paste0(
      "The applicable cost-effectiveness threshold is NOK ",
      format(wtp_threshold, big.mark = ","),
      " per QALY under the Norwegian priority-setting framework ",
      "(Magnussen et al. 2015, absolute shortfall method).")
  }

  cat("TEXT SNIPPET: results-severity-narrative.txt\n")
  list(text = snippet, path = "results-severity-narrative.txt")
}


# --- Snippet 7: EVPI narrative -----------------------------------------------
# SOURCE: evpi_results.rds, wtp_threshold (R/00-parameters.R)
# GUIDELINE: CHEERS-VOI (Kunst et al. 2024)
build_snippet_evpi_narrative <- function() {
  evpi_path <- "data/processed/evpi_results.rds"
  if (!file.exists(evpi_path) || !exists("wtp_threshold")) {
    message("snippet: EVPI results or cost-effectiveness threshold not available. Skipping.")
    return(NULL)
  }

  evpi <- readRDS(evpi_path)
  wtp <- wtp_threshold

  nearest_idx <- which.min(abs(evpi$wtp - wtp))
  evpi_at_wtp <- evpi$evpi[nearest_idx]
  nearest_wtp <- evpi$wtp[nearest_idx]

  # Population EVPI calculation (uses file-level ANNUAL_OPERATED_STAGE_MIX_COHORT)
  annual_patients <- ANNUAL_OPERATED_STAGE_MIX_COHORT
  # GUIDELINE: Scale per-patient EVPI to the eligible population (Briggs et al. 2006)
  pop_evpi <- evpi_at_wtp * annual_patients

  if (exists("psa_summary") && abs(psa_summary$expected_inb) >= 1e-6) {
    inb <- psa_summary$expected_inb
    evpi_pct_inb <- evpi_at_wtp / abs(inb) * 100
    inb_context <- paste0(
      " The per-patient EVPI represents ",
      round(evpi_pct_inb, 1), "% of the expected net benefit (NOK ",
      format_nok(inb), "), indicating ",
      if (evpi_pct_inb < 5) "low" else if (evpi_pct_inb < 15) "moderate"
      else "substantial",
      " decision uncertainty relative to the expected gains.")
  } else {
    inb_context <- ""
  }

  # Discounted population EVPI (Briggs 2006 formula)
  # GUIDELINE: Briggs, Claxton, Sculpher (2006), p.176, Section 6.2.1
  # GUIDELINE: Fenwick et al. (2020), p.144-145, GPR 3, Box 4
  r_pop <- get_discount_rate()
  pop_evpi_10yr <- evpi_at_wtp * annual_patients *
    (1 - (1 + r_pop)^(-10)) / r_pop
  # GUIDELINE: Discount future eligible cohorts in population EVPI (Briggs et al. 2006)
  pop_evpi_5yr <- evpi_at_wtp * annual_patients *
    (1 - (1 + r_pop)^(-5)) / r_pop
  pop_evpi_20yr <- evpi_at_wtp * annual_patients *
    (1 - (1 + r_pop)^(-20)) / r_pop

  # Convergence INMB values for inline text (P9)
  conv_sentence <- ""
  if (exists("convergence") && !is.null(convergence$inmb_first_converged)) {
    fc <- convergence$inmb_first_converged
    if (!is.infinite(fc)) {
      conv_sentence <- paste0(
        "PSA convergence was confirmed at n=",
        format(fc, big.mark = ","),
        " iterations (95% CI for INMB excluded zero; ",
        "Hatswell et al. 2018). ")
    }
  }
  icer_stability_sentence <- ""
  if (exists("convergence") && !is.null(convergence$pct_diff)) {
    icer_stability_sentence <- paste0(
      "Secondary ICER stability check: ",
      round(convergence$pct_diff, 2), "% difference between ",
      "1,000 and ", format(get_n_psa(), big.mark = ","),
      " iterations. ")
  }

  snippet <- paste0(
    conv_sentence,
    icer_stability_sentence,
    "The per-patient EVPI at the applicable cost-effectiveness threshold of NOK ",
    format(nearest_wtp, big.mark = ","), " per QALY is NOK ",
    format_nok(evpi_at_wtp), ". ",
    "The modelled annual operated stage-mix cohort size is ",
    format(annual_patients, big.mark = ","),
    ", derived from all operated stage III colon cancer cases in Norway ",
    "scaled to the CHALLENGE stage mix without an adjuvant-treatment filter ",
    "(Courneya et al. 2025; CRC Quality Registry, 2024). ",
    "The cohort-matched analogue based on recorded receipt or start of adjuvant ",
    "chemotherapy was approximately 472, a receipt-based upper-bound proxy ",
    "affected by the case-to-patient limitation; the registry did not report completion. ",
    "The undiscounted population ",
    "EVPI for a single annual cohort is NOK ",
    format(round(pop_evpi), big.mark = ","), ". ",
    "Over a 10-year technology decision horizon with 4% discounting per ",
    "Rundskriv R-109 (Briggs, Claxton and Sculpher 2006, Section 6.2.1; ",
    "Fenwick et al. 2020, GPR 3), the discounted population EVPI is NOK ",
    format(round(pop_evpi_10yr), big.mark = ","),
    " (sensitivity: NOK ", format(round(pop_evpi_5yr), big.mark = ","),
    " at 5 years, NOK ", format(round(pop_evpi_20yr), big.mark = ","),
    " at 20 years).",
    inb_context,
    if (pop_evpi_10yr < 50e6) {
      " The modest population EVPI suggests that the expected cost of a wrong adoption decision is small relative to the cost of additional research, and that further data collection to reduce parameter uncertainty may not be justified at this threshold."
    } else {
      " The population EVPI suggests that further research to reduce parameter uncertainty may be justified, as the expected cost of a wrong adoption decision is substantial."
    })

  cat("TEXT SNIPPET: results-evpi-narrative.txt\n")
  list(text = snippet, path = "results-evpi-narrative.txt")
}


# --- Snippet 8: Software statement -------------------------------------------
# For methods chapter software subsection
build_snippet_software_statement <- function() {
  r_version <- paste0(R.version$major, ".", R.version$minor)

  pkg_versions <- sapply(
    c("flexsurv", "dampack", "mgcv", "flextable", "officer", "ggplot2",
      "IPDfromKM", "survival", "MASS", "dplyr", "scales",
      "tidyr", "jsonlite"),
    function(p) {
      tryCatch(as.character(packageVersion(p)),
               error = function(e) "not installed")
    }
  )

  pkg_lines <- paste0("  ", names(pkg_versions), " ", pkg_versions)

  snippet <- paste0(
    "All analyses were conducted in R version ", r_version,
    " (R Core Team, ", format(Sys.Date(), "%Y"), "). ",
    "The partitioned survival model was implemented using flexsurv ",
    pkg_versions[["flexsurv"]],
    " for parametric survival fitting, with individual patient data ",
    "reconstructed from published Kaplan-Meier curves using IPDfromKM ",
    pkg_versions[["IPDfromKM"]],
    ". MASS::mvrnorm supplied endpoint-specific baseline-survival coefficient ",
    "MVN draws; marginal HRs were sampled separately, and positive ",
    "cross-endpoint complete-row coupling was sensitivity-only. Expected loss ",
    "curves used dampack ",
    pkg_versions[["dampack"]],
    ". EVPI was computed via custom INB formula; EVPPI was computed via GAM ",
    "regression (Strong, Oakley, and Brennan, 2014) using mgcv ",
    pkg_versions[["mgcv"]],
    ". Output tables were formatted using flextable ",
    pkg_versions[["flextable"]],
    " and exported to DOCX format. All model code is available in the ",
    "accompanying repository.\n\n",
    "Package versions:\n", paste(pkg_lines, collapse = "\n"))

  cat("TEXT SNIPPET: methods-software-statement.txt\n")
  list(text = snippet, path = "methods-software-statement.txt")
}


# ==============================================================================
# TEXT SNIPPET RUNNER
# ==============================================================================

build_all_snippets <- function(output_dir = SNIPPET_OUTPUT_DIR) {

  cat("\n=== GENERATING TEXT SNIPPETS ===\n\n")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  builders <- list(
    build_snippet_icer_interpretation,
    build_snippet_psa_verdict,
    build_snippet_dominance_statement,
    build_snippet_dsa_drivers,
    build_snippet_model_selection,
    build_snippet_severity_narrative,
    build_snippet_evpi_narrative,
    build_snippet_software_statement
  )

  saved_count <- 0

  for (builder in builders) {
    result <- tryCatch(builder(), error = function(e) {
      warning("Snippet builder failed: ", e$message, call. = FALSE)
      NULL
    })

    if (!is.null(result)) {
      save_path <- file.path(output_dir, result$path)
      writeLines(result$text, save_path)
      cat("Saved:", result$path, "\n")
      saved_count <- saved_count + 1
    }
  }

  cat("\n=== ALL TEXT SNIPPETS GENERATED ===\n")
  cat("Total:", saved_count, "snippets saved to", output_dir, "\n")

  invisible(saved_count)
}


# --- Script loaded message ----------------------------------------------------
message("Table export and text snippet code loaded (R/08-export-tables.R). ",
        "Call build_all_tables() for DOCX output, build_all_snippets() for ",
        "text snippets.")
