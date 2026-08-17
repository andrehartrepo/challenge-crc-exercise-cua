# =============================================================================
# 07-visualization.R
# All model visualisations
#
# Produces:
#   - Cost-effectiveness plane (CE plane)  --  from PSA base case
#   - Cost-effectiveness acceptability curve (CEAC)
#   - Tornado diagram (from OWPSA results, probabilistic)
#   - Expected loss curves (ADOPT A4, via dampack)
#   - PSA convergence diagnostic (ADOPT A5)
#   - PSM state occupancy plot
#   - Parametric survival fit overlaid on KM
#
# Input:  data/processed/psa_results.rds
#         data/processed/psm_trace.rds
#         data/processed/costs_qalys.rds
#
# Output: output/figures/*.pdf
#
# Packages: ggplot2, dampack, scales, tidyr
# =============================================================================
#
# REFERENCE CODE PROVENANCE:
#   Cost-effectiveness plane scatter: Black WC (1990). "The CE plane: a
#     graphic representation of cost-effectiveness." Medical Decision Making
#     10(3):212-214. DOI 10.1177/0272989X9001000308. PMID 2115096. The
#     scatter of PSA iterations on the (incremental QALY, incremental cost)
#     plane with WTP threshold reference line is the canonical
#     representation of joint parameter uncertainty.
#   Cost-effectiveness acceptability curve (CEAC): Fenwick E, Claxton K,
#     Sculpher M (2001). "Representing uncertainty: the role of
#     cost-effectiveness acceptability curves." Health Economics
#     10(8):779-787. DOI 10.1002/hec.635. PMID 11747057. CEAC plots the
#     proportion of PSA iterations cost-effective at each WTP threshold.
#     The cost-effectiveness acceptability frontier (CEAF), reporting the
#     probability that the optimal strategy is cost-effective at each
#     WTP, is the ISPOR-recommended companion plot for two-strategy
#     comparisons.
#   Tornado diagram from OWPSA: McCabe C, Paulden M, Awotwe I, Sutton A,
#     Hall P (2020). "One-way sensitivity analysis for probabilistic
#     cost-effectiveness analysis: conditional expected incremental net
#     benefit." PharmacoEconomics 38(2):135-141.
#     DOI 10.1007/s40273-019-00869-3. PMID 31802379. Defines the
#     conditional expected INB (cINMB) tornado approach used here:
#     each parameter is fixed at low/high while all others remain sampled.
# SOURCE: dampack::calc_exp_loss() is provided by Alarid-Escudero F,
# Knowlton G, Easterly CW, Enns E (2024), dampack 1.0.2.1000,
# DOI 10.32614/CRAN.package.dampack.
# SOURCE: Expected-loss-curve interpretation follows Alarid-Escudero F,
# Enns EA, Kuntz KM, Michaud TL, Jalal H (2019), Value in Health
# 22(5):611-618, DOI 10.1016/j.jval.2019.02.008.
#   PSA convergence visualisation: Hatswell AJ et al. (2018) PharmacoEconomics
#     36(12):1421-1426. DOI 10.1007/s40273-018-0697-3. PMID 30051221.
#     Plot of INMB CI versus iteration count is the recommended visual
#     companion to the table from compute_convergence_inmb() in 06-psa.R.
#   PSM state occupancy plot: standard Markov-trace visualisation
#     (Drummond et al. 2015, Ch. 9); not associated with a specific
#     published implementation.
#
# Adaptations:
#   1. ggplot2 wrappers around dampack outputs (rather than calling
#      dampack's plot methods directly) ensure consistent fonts, colour
#      palette, and aspect ratios across all model figures.
#   2. plot_ceac() emits the appendix acceptability-curve figure; plot_ceaf()
#      is a retained utility whose saved figure output stays retired (the
#      frontier is reported in the WTP table in 08-export-tables.R and in the
#      Results prose, so a frontier overlay would repeat it).
#   3. Tornado diagram colour-codes parameters by category (HRs, costs,
#      utilities) to support quick interpretation in the thesis appendix.
#
# Academic citations:
#   Black WC (1990). DOI 10.1177/0272989X9001000308. PMID 2115096.
#   Fenwick E, Claxton K, Sculpher M (2001).
#     DOI 10.1002/hec.635. PMID 11747057.
#   McCabe C, Paulden M, Awotwe I, Sutton A, Hall P (2020).
#     DOI 10.1007/s40273-019-00869-3. PMID 31802379.
#   Alarid-Escudero F, Enns EA, Kuntz KM, Michaud TL, Jalal H (2019).
#     Value in Health 22(5):611-618. DOI 10.1016/j.jval.2019.02.008.
#   Hatswell AJ et al. (2018).
#     DOI 10.1007/s40273-018-0697-3. PMID 30051221.
# =============================================================================

# --- Font setup (showtext for Charter in PDF output) -------------------------
# Charter is installed on macOS but R's default PDF device cannot render it.
# showtext enables system font rendering in all graphics devices.
if (requireNamespace("showtext", quietly = TRUE)) {
  sysfonts::font_add("Charter",
    regular    = "/System/Library/Fonts/Supplemental/Charter.ttc",
    bold       = "/System/Library/Fonts/Supplemental/Charter.ttc",
    italic     = "/System/Library/Fonts/Supplemental/Charter.ttc",
    bolditalic = "/System/Library/Fonts/Supplemental/Charter.ttc")
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)
}

# Colour palette (accessible, consistent with Norwegian academic style)
# pal is defined at file scope (global env when sourced).
# Acceptable for a script-based workflow; would need refactoring for a package.
# If sourced twice, pal is overwritten with same values (idempotent).
pal <- list(
  intervention = "#0072B2",   # Blue
  control      = "#E69F00",   # Orange
  neutral      = "#999999",   # Grey
  highlight    = "#D55E00",   # Red-orange for emphasis
  wtp_line     = "#CC79A7"    # Pink for WTP reference line
)

# --- Distribution colour palette (survival figures) -------------------------
# GUIDELINE: Selected distribution highlighting standard
# Base is ColorBrewer Set1, with three substitutions, all made for measured
# contrast against a white background (contrast ratio computed by the WCAG 2.1
# relative-luminance formula; benchmark 3.0:1 for non-text graphical objects,
# SC 1.4.11):
#   Gen. Gamma  #A65628  Set1 position 7 (brown), replacing yellow #FFFF33.
#   Log-normal  #1B7837  ColorBrewer PRGn index 10 of 11, colorblind = TRUE.
#   Gompertz    #B35806  ColorBrewer PuOr index 2 of 11, colorblind = TRUE.
# DECISION: Log-normal and Gompertz were substituted because their Set1 hues
# could not reach 3.0:1 at ANY opacity (ceilings 2.78 and 2.53 at alpha 1.0),
# and Log-normal is the selected distribution for both endpoints. The four
# surviving Set1 hues clear the benchmark at the alpha set below.
# Not rated as colour-blind safe for 6+ categories (ColorBrewer flags Set1
# colorblind = FALSE). Mitigation is two-channel: the selected distribution
# uses linewidth 1.7/alpha 1.0 against 0.9/0.85, preserving the primary visual
# distinction in grayscale, AND on the two reconstructed-KM families every
# series carries a direct text label, so colour is not the sole channel
# carrying series identity there. The hazard and log-cumulative-hazard
# families still rely on the legend alone; that residual is recorded, not
# cured, here.
# Caution: deuteranopia separation improves under the perceptual deltaE measure in
# BOTH simulation models (Vienot 1999: 2 of 15 pairs below deltaE 10 -> 0;
# Machado 2009: 4 of 15 -> 2). It does NOT improve under the luminance-only
# WCAG measure in Machado 2009 (13 of 15 -> 13 of 15). The claim therefore
# rests on the change of metric, never on both sets sharing one method.
# Standardized across all survival figures.
# Distinct from strategy palette (pal) which uses blue/orange for arms.
dist_colours <- c(
  "Exponential"   = "#e41a1c",
  "Weibull"       = "#377eb8",
  "Log-normal"    = "#1B7837",
  "Log-logistic"  = "#984ea3",
  "Gompertz"      = "#B35806",
  "Gen. Gamma"    = "#a65628"
)

# Internal name to display label mapping
dist_labels <- c(
  exp      = "Exponential",
  weibull  = "Weibull",
  lnorm    = "Log-normal",
  llogis   = "Log-logistic",
  gompertz = "Gompertz",
  gengamma = "Gen. Gamma"
)

# --- Shared thesis theme (Charter font, consistent styling) -----------------
# GUIDELINE: Charter font, thesis-standard sizing
# Charter matches thesis table font (flextable). Academic serif.
# Eight-inch figures scale to about 69% in the 400 pt text block.
# A 15 pt base therefore renders at about 10.4 pt; rel(0.8) axis and
# subtitle text renders at about 8.3 pt on the page.
theme_thesis <- function(base_size = 15) {
  ggplot2::theme_minimal(
    base_size   = base_size,
    base_family = "Charter"
  ) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(face = "bold"),
    legend.position = "bottom",
    # Readable legend text with compressed key spacing to prevent clipping
    legend.text      = ggplot2::element_text(size = ggplot2::rel(0.75)),
    legend.key.width = ggplot2::unit(2.0, "lines"),
    legend.spacing.x = ggplot2::unit(0.3, "lines"),
    plot.subtitle    = ggplot2::element_text(size = ggplot2::rel(0.8)),
    # Lighter grid, no minor grid lines
    panel.grid.major = ggplot2::element_line(colour = "#E8E8E8",
                                              linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank()
  )
}

# --- Centralized annotation and highlighting constants -----------
# GUIDELINE: Standardized annotation tier system
# Anti-hardcoding: all annotation sizes defined once, referenced everywhere.
ann_size_secondary <- 4.1  # mm: tornado INMB, model structure arrows, EL
ann_size_reference <- 4.3  # mm: WTP/INMB ref annotations (CEAC, CEAF, EL, SSA)
ann_size_standard  <- 4.5  # mm: P(CE)/ICER, end-of-data, KM, EVPPI, convergence
ann_size_bold      <- 5.1  # mm: model structure state labels, pop EVPI bar values

# Tighter annotation padding
ann_label_padding <- ggplot2::unit(0.15, "lines")

# Selected distribution highlighting (standardized)
# Applied uniformly to ALL survival figures (overlay, extrapolation,
# hazard, validation). Hazard previously used 2.0/0.6/0.35; now standardized.
# Caution: alpha is a LEGIBILITY FLOOR, not the highlight channel. At the former
# other_alpha 0.4 every non-selected series composited to under 2.0:1 against
# white, below the 3.0:1 benchmark. Raising it to 0.85 puts all six series in
# the 3.37 to 4.13 band. The selected-versus-other highlight moves entirely
# into linewidth, and is STRONGER after the change: ratio 1.89x (1.7/0.9)
# against the former 1.71x (1.2/0.7).
sel_linewidth   <- 1.7   # selected distribution linewidth
other_linewidth <- 0.9   # non-selected distribution linewidth
sel_alpha       <- 1.0   # selected distribution opacity
other_alpha     <- 0.85  # non-selected distribution opacity

# --- Direct series labelling (redundant non-colour channel) -----------------
# GUIDELINE: colour must not be the sole channel carrying meaning (WCAG 2.1
# SC 1.4.1 principle; project visual-excellence research candidate R3).
# Caution: linetype is NOT available as that channel. On plot_extrapolation() it
# already encodes ARM, and merging six distribution levels into a scale that
# also carries reference-rule keys needs 7 values, which hard-errors in
# scale_linetype_manual (measured). Direct labelling is the conformant route.
# Caution: ggrepel places labels by a randomised repulsion; `seed` is mandatory or
# the saved PNG changes between identical runs and breaks byte determinism.
series_end_labels <- function(df, x_col, group_col) {
  parts <- split(df, df[[group_col]])
  do.call(rbind, lapply(parts, function(d) d[which.max(d[[x_col]]), ]))
}


# --- 1. CE Plane -------------------------------------------------------------

#' Cost-Effectiveness Plane (Scatter of PSA Iterations)
#'
#' Plots incremental costs vs incremental QALYs from PSA.
#' WTP threshold line at the applicable severity-informed WTP reference.
#' Points below the line are cost-effective.
#'
#' @param psa_results Data frame from run_psa() with inc_qalys, inc_costs.
#' @param wtp Numeric. WTP threshold for reference line.
#' @return ggplot object.
plot_ce_plane <- function(psa_results, wtp = NULL) {
  # NULL default with lazy resolution, consistent with 06-psa.R
  if (is.null(wtp)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("plot_ce_plane: wtp must be provided or wtp_threshold must exist.")
    }
  }
  # Type guard: wtp must be a positive numeric
  wtp <- as.numeric(wtp)
  if (!is.finite(wtp) || wtp <= 0) {
    stop("plot_ce_plane: wtp_threshold must be a positive numeric.")
  }

  # GUIDELINE: Key result metrics (CHEERS 2022 base-case and uncertainty results)
  inb <- psa_results$inc_qalys * wtp - psa_results$inc_costs
  p_ce <- mean(inb[!is.na(inb)] >= 0)
  # SOURCE: ISPOR PSA good-practice: ratio of PSA means (Briggs et al. 2006)
  icer_rom <- mean(psa_results$inc_costs, na.rm = TRUE) /
              mean(psa_results$inc_qalys, na.rm = TRUE)

  # Compact subtitle: headline results only (CHEERS 2022 reporting)
  plot_subtitle <- paste0(
    "ICER = ",
    format(round(icer_rom), big.mark = ","),
    " NOK/QALY | P(CE) = ", round(p_ce * 100, 1), "%"
  )

  # Two-column caption: each line is a short complete sentence fitting on ONE
  # physical line. Both columns LEFT-aligned (hjust=0). Sentences kept under
  # ~33 chars so longest line fits within its column's width at 13 pt Charter.
  col1_text <- paste(
    paste0("Diagonal: threshold ", format(wtp, big.mark = ","), "\nNOK/QALY."),
    "Below diagonal: cost-effective.",
    "Dashed: zero cost, zero QALYs.",
    sep = "\n"
  )
  col2_text <- paste(
    "Each point: one PA draw.",
    "Exercise minus Standard Care.",
    "NE quadrant: trade-off\nversus the threshold.",
    sep = "\n"
  )
  col3_text <- paste(
    paste0("n = ", format(nrow(psa_results), big.mark = ","),
           "; ratio of means."),
    paste0("Diamond: mean ICER,\n",
           format(round(icer_rom), big.mark = ","), " NOK/QALY."),
    # SOURCE: Rundskriv R-109 is the Finansdepartementet (Ministry of Finance)
    #   circular that sets the 4% rate; DMP is not its issuer, and the paper's
    #   abstract and Methods both attribute it to Finansdepartementet.
    # GUIDELINE: Norwegian discount rate 4% per Rundskriv R-109
    #   (Finansdepartementet 2021), Norwegian hierarchy tier 1.
    # DECISION: name the base-case perspective in full. Every sibling
    #   annotation in this file says "extended healthcare perspective", and
    #   the paper turns on extended versus healthcare-services-only.
    "Extended healthcare perspective;\n4% (Rundskriv R-109).",
    sep = "\n"
  )

  main_plot <- ggplot2::ggplot(psa_results,
                  ggplot2::aes(x = inc_qalys, y = inc_costs)) +
    ggplot2::geom_point(alpha = 0.3, colour = pal$intervention, size = 1) +
    # Mean incremental cost and QALY, plotted as a single high-contrast marker.
    # SOURCE: ISPOR PSA good-practice, ratio of PSA means (Briggs et al. 2006);
    #   the same two means that build icer_rom above.
    # DECISION: the corpus CE-plane exemplar plots and keys its mean; the plane
    #   otherwise gives the reader no anchor for the headline ICER.
    ggplot2::annotate(
      "point",
      x      = mean(psa_results$inc_qalys, na.rm = TRUE),
      y      = mean(psa_results$inc_costs, na.rm = TRUE),
      shape  = 23, size = 3, stroke = 0.9,
      fill   = pal$wtp_line, colour = "grey15"
    ) +
    # Fixed aesthetics for reference lines
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                         colour = "grey50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                         colour = "grey50") +
    ggplot2::geom_abline(slope = wtp, intercept = 0,
                          linetype = "dotted", colour = pal$wtp_line,
                          linewidth = 0.8) +
    # QALY axis: 2 decimal places
    ggplot2::scale_x_continuous(
      name   = "Incremental QALYs",
      labels = scales::label_number(accuracy = 0.01)
    ) +
    # NOK axis: comma-separated thousands
    ggplot2::scale_y_continuous(
      name   = "Incremental Costs (NOK)",
      labels = scales::label_comma()
    ) +
    ggplot2::labs(
      title    = NULL,
      subtitle = NULL
    ) +
    theme_thesis()

  # Composite main plot (top 78%) and two-column caption (bottom 22%) via
  # ggdraw. BOTH columns LEFT-aligned (hjust=0): each line starts from its
  # column's left edge. x positions place col 1 at the plot panel's left edge
  # and col 2 just right of a visible separator, so the block reads left-to-
  # right across the full plot panel width.
  p <- cowplot::ggdraw() +
    cowplot::draw_plot(main_plot,
                        x = 0, y = 0.28, width = 1, height = 0.72) +
    cowplot::draw_label(col1_text,
                         x = 0.12, y = 0.14,
                         hjust = 0, vjust = 0.5, size = 11,
                         colour = "#333333", fontfamily = "Charter",
                         lineheight = 1.1) +
    cowplot::draw_label(col2_text,
                         x = 0.42, y = 0.14,
                         hjust = 0, vjust = 0.5, size = 11,
                         colour = "#333333", fontfamily = "Charter",
                         lineheight = 1.1) +
    cowplot::draw_label(col3_text,
                         x = 0.72, y = 0.14,
                         hjust = 0, vjust = 0.5, size = 11,
                         colour = "#333333", fontfamily = "Charter",
                         lineheight = 1.1) +
    cowplot::draw_line(x = c(0.405, 0.405), y = c(0.03, 0.25),
                        size = 0.5, colour = "#999999") +
    cowplot::draw_line(x = c(0.705, 0.705), y = c(0.03, 0.25),
                        size = 0.5, colour = "#999999") +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  ce_caption <- paste0(col1_text, "\n\n", col2_text, "\n\n", col3_text)
  cat("PROGRAMMATIC CAPTION:\n")
  cat(ce_caption, "\n\n")
  attr(p, "caption") <- ce_caption
  p
}


# --- 2. CEAC -----------------------------------------------------------------

#' Cost-Effectiveness Acceptability Curve (CEAC)
#'
#' Shows the probability that exercise is cost-effective
#' as a function of the WTP threshold.
#' Vertical reference lines at the applicable severity-informed WTP reference
#' and optionally at other scenario thresholds.
#'
#' @param psa_results Data frame from run_psa().
#' @param wtp_range Numeric vector. WTP values for x-axis.
#' @param wtp_ref Numeric. Primary WTP reference line (applicable severity-informed WTP reference).
#' @return ggplot object.
plot_ceac <- function(psa_results, wtp_range, wtp_ref = NULL) {
  # NULL default with lazy resolution
  if (is.null(wtp_ref)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp_ref <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("plot_ceac: wtp_ref must be provided or wtp_threshold must exist.")
    }
  }
  # Check for NAs upfront instead of silently dropping
  # via na.rm = TRUE. Consistent with M9 principle (fail loudly on NAs).
  # PSA tryCatch fallback should prevent NAs, but if they occur, the CEAC
  # must surface the problem rather than silently exclude iterations.
  na_count <- sum(is.na(psa_results$inc_qalys) | is.na(psa_results$inc_costs))
  if (na_count > 0) {
    warning("plot_ceac: ", na_count, " PSA iterations have NA inc_qalys or ",
            "inc_costs. These are excluded from the CEAC. ",
            "Investigate upstream PSA failures.", call. = FALSE)
  }
  ceac_data <- data.frame(
    wtp  = wtp_range,
    prob = sapply(wtp_range, function(w) {
      # GUIDELINE: Compute INB as incremental QALYs times WTP minus incremental costs (Briggs et al. 2006)
      inb <- psa_results$inc_qalys * w - psa_results$inc_costs
      valid <- inb[!is.na(inb)]
      # GUIDELINE: Estimate the CEAC as the share of PSA draws that are cost-effective (Alarid-Escudero et al. 2019)
      mean(valid >= 0)
    })
  )

  p_at_ref <- ceac_data$prob[which.min(abs(ceac_data$wtp - wtp_ref))]

  # --- Programmatic caption for CEAC figure ---
  # SOURCE: ceac_data and p_at_ref computed above in this function
  # PSA base-case with CEAC visualization
  ceac_caption <- paste0(
    "Cost-Effectiveness Acceptability Curve. ",
    "Probability of cost-effectiveness at threshold = NOK ",
    format(wtp_ref, big.mark = ","), "/QALY: ",
    round(p_at_ref * 100, 1), "%. ",
    "Based on ", format(nrow(psa_results), big.mark = ","),
    " PSA iterations (extended healthcare perspective, ",
    "effective discount rate 4% per Rundskriv R-109 stepped schedule)."
  )
  cat("PROGRAMMATIC CAPTION:\n")
  cat(ceac_caption, "\n\n")

  # GUIDELINE: Two mutually exclusive strategy CEAC probabilities sum to one (Fenwick et al. 2001)
  ceac_data$prob_ctrl <- 1 - ceac_data$prob

  p <- ggplot2::ggplot(ceac_data, ggplot2::aes(x = wtp / 1000)) +
    ggplot2::geom_line(ggplot2::aes(y = prob, colour = "Exercise"),
                       linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = prob_ctrl, colour = "Standard Care"),
                       linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                         colour = pal$neutral, linewidth = 0.5) +
    # P(CE) result stays inline (WTP in legend)
    ggplot2::annotate("label",
                       x = wtp_ref / 1000 + 20,
                       y = p_at_ref,
                       label = paste0("P(CE) = ",
                                      round(p_at_ref * 100, 1), "%"),
                       colour = "#333333", hjust = 0,
                       size = ann_size_reference,
                       fill = alpha("white", 0.85), linewidth = 0,
                       label.padding = ann_label_padding,
                       family = "Charter") +
    # WTP reference line (mapped to linetype for legend entry)
    ggplot2::geom_vline(ggplot2::aes(xintercept = wtp_ref / 1000,
                                      linetype = "Cost-effectiveness threshold"),
                        colour = pal$wtp_line, linewidth = 0.8,
                        key_glyph = "path") +
    ggplot2::scale_colour_manual(
      values = c("Exercise"      = pal$intervention,
                 "Standard Care" = pal$control),
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Cost-effectiveness threshold" = "dotted"),
      name = NULL
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        order = 1,
        override.aes = list(linewidth = 1.5, alpha = 1, shape = NA)
      ),
      linetype = ggplot2::guide_legend(
        order = 2,
        override.aes = list(colour = pal$wtp_line, linewidth = 1.0)
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(),
                                 limits = c(0, 1)) +
    ggplot2::labs(
      title    = NULL,
      subtitle = paste0("P(CE) at threshold = ",
                        format(wtp_ref, big.mark = ","),
                        " NOK/QALY: ", round(p_at_ref * 100, 1), "%"),
      x        = "Cost-effectiveness threshold (1,000 NOK per QALY)",
      y        = "Probability Cost-Effective"
    ) +
    theme_thesis()

  attr(p, "caption") <- ceac_caption
  p
}


# --- 2b. CEAF ----------------------------------------------------------------

#' Cost-Effectiveness Acceptability Frontier (CEAF)
#'
#' Plots both strategy CEACs and the CEAF frontier (the maximum probability
#' of cost-effectiveness across strategies at each WTP). For a 2-strategy
#' comparison, CEAF = max(P, 1-P) >= 0.5 by construction since
#' P(control) = 1 - P(exercise) (Fenwick et al. 2001, note d, p.786).
#'
#' Also computes crossover WTP (where P(CE) = 50%) and decision uncertainty
#' (1 - CEAF at the reference WTP), returned as ggplot attributes.
#'
#' @param psa_results Data frame from run_psa() with inc_qalys, inc_costs.
#' @param wtp_range Numeric vector. WTP values for x-axis.
#' @param wtp_ref Numeric. Primary WTP reference line (applicable severity-informed WTP reference).
#' @return ggplot object with attributes: crossover_wtp, decision_uncertainty,
#'   ceaf_at_ref, caption.
plot_ceaf <- function(psa_results, wtp_range, wtp_ref = NULL) {
  # NULL default with lazy resolution
  if (is.null(wtp_ref)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp_ref <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("plot_ceaf: wtp_ref must be provided or wtp_threshold must exist.")
    }
  }

  # P(CE) for exercise at each WTP; P(control) = 1 - P(exercise) for 2 strategies
  # GUIDELINE: Fenwick, Claxton, and Sculpher (2001)
  na_count <- sum(is.na(psa_results$inc_qalys) | is.na(psa_results$inc_costs))
  if (na_count > 0) {
    warning("plot_ceaf: ", na_count, " PSA iterations have NA values. ",
            "Excluded from CEAF.", call. = FALSE)
  }
  prob_exercise <- sapply(wtp_range, function(w) {
    # GUIDELINE: Compute INB as incremental QALYs times WTP minus incremental costs (Briggs et al. 2006)
    inb <- psa_results$inc_qalys * w - psa_results$inc_costs
    # GUIDELINE: Estimate the CEAC as the share of PSA draws that are cost-effective (Fenwick et al. 2001)
    mean(inb[!is.na(inb)] >= 0)
  })
  # GUIDELINE: Two mutually exclusive strategy CEAC probabilities sum to one (Fenwick et al. 2001)
  prob_control <- 1 - prob_exercise
  # GUIDELINE: Define the CEAF by the strategy with maximum expected net benefit (Fenwick et al. 2001)
  ceaf_values <- pmax(prob_exercise, prob_control)

  ceaf_data <- data.frame(
    wtp           = wtp_range,
    prob_exercise = prob_exercise,
    prob_control  = prob_control,
    ceaf          = ceaf_values
  )

  # Values at reference WTP
  # Evaluate decision uncertainty at the programmatic severity-informed reference.
  ref_idx <- which.min(abs(ceaf_data$wtp - wtp_ref))
  p_exercise_ref <- ceaf_data$prob_exercise[ref_idx]
  ceaf_at_ref <- ceaf_data$ceaf[ref_idx]
  # Decision uncertainty = 1 - CEAF at reference WTP
  # Equivalent to the probability of making the wrong decision
  # SOURCE: complement of CEAF per Fenwick et al. (2001)
  decision_uncertainty <- 1 - ceaf_at_ref

  # Crossover WTP: where P(exercise optimal) crosses 50%
  # METHOD: minimum-distance per spec (which.min(abs(prob_ce - 0.5))).
  # METHOD: minimum-distance approach (which.min(abs(prob_ce - 0.5))).
  # Course reference code uses sign-change detection (diff(sign(...))),
  # Caution: Select the nearest 50% CEAC point before applying the reporting guard.
  crossover_idx <- which.min(abs(prob_exercise - 0.5))
  # Guard: only report crossover if actually near 50% (within 5 percentage points)
  # Caution: Do not report a crossover unless the closest CEAC point is within 5 percentage points of 50%.
  crossover_wtp <- if (abs(prob_exercise[crossover_idx] - 0.5) < 0.05) {
    wtp_range[crossover_idx]
  } else {
    NA_real_
  }

  # --- Programmatic caption ---
  # SOURCE: ceaf_data computed above
  # GUIDELINE: CHEERS-VOI Item 20 (characterizing uncertainty)
  ceaf_caption <- paste0(
    "Cost-Effectiveness Acceptability Frontier (CEAF). ",
    "For a 2-strategy comparison, CEAF = max(P, 1-P) >= 50%% by construction ",
    "since P(exercise) + P(control) = 1 (Fenwick et al. 2001, note d). ",
    "At threshold = NOK ", format(wtp_ref, big.mark = ","), "/QALY: ",
    "P(exercise optimal) = ", round(p_exercise_ref * 100, 1), "%, ",
    "CEAF = ", round(ceaf_at_ref * 100, 1), "%, ",
    "decision uncertainty = ", round(decision_uncertainty * 100, 1), "%. ",
    if (!is.na(crossover_wtp)) {
      paste0("Crossover threshold (P(CE) = 50%): NOK ",
             format(crossover_wtp, big.mark = ","), "/QALY. ")
    } else {
      "No crossover observed in the threshold range. "
    },
    "Based on ", format(nrow(psa_results), big.mark = ","),
    " PSA iterations (extended healthcare perspective, ",
    "effective discount rate 4% per Rundskriv R-109 stepped schedule)."
  )
  cat("PROGRAMMATIC CAPTION:\n")
  cat(ceaf_caption, "\n\n")

  p <- ggplot2::ggplot(ceaf_data, ggplot2::aes(x = wtp / 1000)) +
    # Individual strategy CEACs
    ggplot2::geom_line(ggplot2::aes(y = prob_exercise, colour = "Exercise"),
                       linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = prob_control, colour = "Standard Care"),
                       linewidth = 0.9) +
    # CEAF frontier (mapped to linetype for legend entry)
    ggplot2::geom_line(ggplot2::aes(y = ceaf, linetype = "CEAF frontier"),
                       linewidth = 1.3, colour = "black") +
    # 50% reference line
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                        colour = pal$neutral, linewidth = 0.5) +
    # WTP reference line (mapped to linetype for legend entry)
    ggplot2::geom_vline(ggplot2::aes(xintercept = wtp_ref / 1000,
                                      linetype = "Cost-effectiveness threshold"),
                        colour = pal$wtp_line, linewidth = 0.8,
                        key_glyph = "path") +
    ggplot2::scale_colour_manual(
      values = c("Exercise" = pal$intervention,
                 "Standard Care" = pal$control),
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = c("CEAF frontier" = "dashed",
                 "Cost-effectiveness threshold" = "dotted"),
      name = NULL
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        order = 1,
        override.aes = list(linewidth = 1.5, alpha = 1, shape = NA)
      ),
      linetype = ggplot2::guide_legend(
        order = 2,
        override.aes = list(
          colour = c("black", pal$wtp_line),
          linewidth = 1.0
        )
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(),
                                limits = c(0, 1)) +
    ggplot2::labs(
      title    = NULL,
      subtitle = paste0("CEAF at threshold = ",
                        format(wtp_ref, big.mark = ","),
                        " NOK/QALY: ", round(ceaf_at_ref * 100, 1),
                        "% | Decision uncertainty: ",
                        round(decision_uncertainty * 100, 1), "%"),
      x        = "Cost-effectiveness threshold (1,000 NOK per QALY)",
      y        = "Probability Cost-Effective"
    ) +
    theme_thesis()

  attr(p, "caption") <- ceaf_caption
  attr(p, "crossover_wtp") <- crossover_wtp
  attr(p, "decision_uncertainty") <- decision_uncertainty
  attr(p, "ceaf_at_ref") <- ceaf_at_ref
  p
}


# --- 3. Tornado Diagram (OWPSA) ------------------------------------------------

#' Tornado Diagram from One-Way Probabilistic SA
#'
#' Visualises the impact of each parameter on expected incremental net
#' monetary benefit (INMB) per McCabe et al. (2020). Each bar shows
#' the conditional expected INMB when one parameter is fixed at its
#' low or high bound while all others are sampled probabilistically.
#'
#' @param owpsa_results Data frame with columns: param_name, label,
#'   inmb_low, inmb_high, range. Produced by main.R POWSA block.
#' @param base_icer Numeric. Base case expected ICER for subtitle context.
#' @param wtp Numeric. WTP threshold for subtitle label.
#' @return ggplot object.
plot_tornado <- function(owpsa_results, base_icer, wtp = NULL) {
  if (is.null(wtp)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("plot_tornado: wtp must be provided or wtp_threshold must exist.")
    }
  }
  if (is.null(owpsa_results) || nrow(owpsa_results) == 0) {
    message("Tornado plot: no OWPSA results available.")
    return(NULL)
  }

  # Data already contains inmb_low, inmb_high, range per parameter
  # Sort by range (ascending so widest bar is at top in coord_flip-free layout)
  tornado_data <- owpsa_results |>
    dplyr::arrange(range)

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
  input_params <- as.character(tornado_data$param_name)
  if (anyNA(input_params) || anyDuplicated(input_params) ||
      !setequal(input_params, names(label_stems)) ||
      length(input_params) != length(label_stems)) {
    stop("plot_tornado: parameter display map must match each input exactly once.")
  }

  decimal_params <- input_params %in% c("HR_DFS", "HR_OS", "u_dfs", "u_prog")
  display_ranges <- character(length(input_params))
  display_ranges[decimal_params] <- paste0(
    formatC(as.numeric(tornado_data$value_low[decimal_params]),
            format = "f", digits = 2),
    "-",
    formatC(as.numeric(tornado_data$value_high[decimal_params]),
            format = "f", digits = 2)
  )
  cost_matches <- regmatches(
    tornado_data$label[!decimal_params],
    regexec("\\(([0-9.]+%-[0-9.]+%)\\)", tornado_data$label[!decimal_params])
  )
  if (any(lengths(cost_matches) != 2L)) {
    stop("plot_tornado: cost labels must contain a registered percentage range.")
  }
  display_ranges[!decimal_params] <- vapply(
    cost_matches, function(x) x[[2L]], character(1L))
  tornado_data$label <- paste0(
    unname(label_stems[input_params]), " (", display_ranges, ")")
  if (anyDuplicated(tornado_data$label)) {
    stop("plot_tornado: duplicate display labels are not allowed.")
  }

  tornado_data$label <- factor(tornado_data$label,
                                levels = tornado_data$label)

  top_param <- tornado_data$label[nrow(tornado_data)]
  top_range <- tornado_data$range[nrow(tornado_data)]

  p <- ggplot2::ggplot(tornado_data) +
    ggplot2::geom_segment(
      ggplot2::aes(x = inmb_low, xend = inmb_high,
                    y = label, yend = label),
      linewidth = 6, colour = pal$intervention, alpha = 0.7
    ) +
    ggplot2::geom_vline(xintercept = 0,
                         linetype = "dashed", colour = pal$highlight,
                         linewidth = 0.8) +
    # INMB = 0 corresponds to ICER = WTP (not base-case ICER)
    # At x = 0, incremental NMB is zero, meaning ICER exactly equals WTP threshold.
    # Base-case ICER is already reported in the subtitle.
    # GUIDELINE: McCabe et al. (2020): decision threshold annotation
    # Caution: at the default upper expansion the outermost thousands-formatted
    # tick label runs past the device edge and prints clipped mid-glyph. The wider
    # upper expansion moves the last break inboard far enough to seat the whole
    # label inside the canvas; the break set is unchanged because the expanded
    # upper limit stays below the next candidate break.
    ggplot2::scale_x_continuous(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0.05, 0.10))
    ) +
    ggplot2::labs(
      title    = NULL,
      subtitle = NULL,
      x        = "Conditional Expected INMB (NOK)",
      y        = "Parameter"
    ) +
    theme_thesis() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 11),
      axis.text.y = ggplot2::element_text(size = 12)
    )

  tornado_caption <- paste0(
    "Tornado Diagram (Probabilistic One-Way SA). ",
    nrow(tornado_data), " parameters. ",
    "Most influential: ", top_param,
    " (INMB range: NOK ", format(round(top_range), big.mark = ","), "). ",
    "Threshold = NOK ", format(wtp, big.mark = ","), "/QALY. ",
    "Base case ICER = NOK ", format(round(base_icer), big.mark = ","),
    "/QALY. McCabe et al. (2020) conditional expected INMB method. ",
    "(Extended healthcare perspective, ",
    "effective discount rate 4% per Rundskriv R-109 stepped schedule)."
  )
  cat("PROGRAMMATIC CAPTION:\n")
  cat(tornado_caption, "\n\n")
  attr(p, "caption") <- tornado_caption
  p
}


# --- 4. Expected Loss Curves (ADOPT A4) -------------------------------------

#' Expected Loss Plot (ggplot2)
#'
#' Plots the expected opportunity loss for each strategy as a function
#' of WTP. Complements the CEAC by showing the magnitude of loss from
#' choosing the wrong strategy. Fenwick et al. (2020) recommend reporting
#' expected loss alongside CEACs.
#' SOURCE: Alarid-Escudero et al. (2019), Value in Health 22(5):611-618,
#'   DOI 10.1016/j.jval.2019.02.008 (expected-loss interpretation).
#'
#' @param exp_loss Output from calculate_expected_loss() (dampack object).
#'   Coerced via as.data.frame() to columns: WTP, Strategy, Expected_Loss, On_Frontier.
#' @param wtp Numeric. WTP threshold for reference line.
#' @return ggplot object.
plot_expected_loss <- function(exp_loss, wtp = NULL) {
  if (is.null(wtp)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("plot_expected_loss: wtp must be provided or wtp_threshold must exist.")
    }
  }
  if (is.null(exp_loss)) {
    message("Expected loss plot: no data available.")
    return(NULL)
  }

  el_df <- as.data.frame(exp_loss)

  # Rename dampack strategy labels to thesis terminology
  # dampack::make_psa_obj converts "Standard Care" to "Standard.Care" via make.names
  el_df$Strategy <- gsub("Standard\\.Care", "Standard Care", el_df$Strategy)

  # Display range extends 25% beyond WTP for annotation legibility
  el_df <- el_df[el_df$WTP <= wtp * 1.25, ]

  # Expected loss at WTP for key result annotation
  el_at_wtp <- el_df[el_df$WTP == wtp, ]
  el_exercise_at_wtp <- if (nrow(el_at_wtp) > 0) {
    el_at_wtp$Expected_Loss[el_at_wtp$Strategy == "Exercise"]
  } else NA_real_

  p <- ggplot2::ggplot(el_df,
                  ggplot2::aes(x = WTP / 1000, y = Expected_Loss,
                               colour = Strategy)) +
    ggplot2::geom_line(linewidth = 1) +
    # WTP reference line (mapped to linetype for legend entry)
    ggplot2::geom_vline(ggplot2::aes(xintercept = wtp / 1000,
                                      linetype = "Cost-effectiveness threshold"),
                        colour = pal$wtp_line, linewidth = 0.8,
                        key_glyph = "path") +
    # Expected loss annotation (repositioned left of WTP to prevent clipping)
    # Caution: the annotation reports the EXERCISE arm's expected loss but was
    # positioned at 70 percent of the maximum over BOTH series, which the
    # Standard Care curve sets. That put a label reading a few hundred NOK deep
    # in the Standard Care curve's neighbourhood, inviting the reader to
    # attribute the value to the wrong strategy. The anchor is now the value's
    # own series, and the text names the strategy it belongs to.
    # geom_label_repel keeps it off the curve.
    {if (!is.na(el_exercise_at_wtp)) ggrepel::geom_label_repel(
                       data = data.frame(x = wtp / 1000,
                                         y = el_exercise_at_wtp),
                       ggplot2::aes(x = x, y = y),
                       inherit.aes = FALSE,
                       label = paste0("Exercise (optimal strategy): ",
                                      format(round(el_exercise_at_wtp),
                                             big.mark = ","),
                                      " NOK at threshold"),
                       colour = "#333333", hjust = 1,
                       size = ann_size_secondary,
                       fill = alpha("white", 0.85), label.size = 0,
                       label.padding = ann_label_padding,
                       family = "Charter",
                       direction = "y", nudge_y = 1.1, nudge_x = -40,
                       segment.size = 0.3, min.segment.length = 0,
                       seed = 20260803)
    } +
    ggplot2::scale_x_continuous(
      labels = scales::label_comma(),
      breaks = seq(0, floor(wtp / 1000 * 1.25 / 100) * 100, by = 100)
    ) +
    # GUIDELINE: log y axis follows dampack::plot.exp_loss, which defaults
    # log_y = TRUE for this exact figure (DARTH-lineage reference
    # implementation; dampack 1.0.2.1000).
    # Caution: the two series span several orders of magnitude over the WTP
    # grid. On a linear axis the optimal strategy's expected loss, which IS the
    # per-patient EVPI and the entire point of the figure, flattens onto the
    # axis and cannot be read. log10 is safe here only because every expected
    # loss on the grid is strictly positive; a zero or negative value would
    # drop silently from the panel.
    ggplot2::scale_y_continuous(
      transform = "log10",
      labels = scales::label_comma()
    ) +
    ggplot2::scale_colour_manual(
      values = c("Exercise"      = pal$intervention,
                 "Standard Care" = pal$control),
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Cost-effectiveness threshold" = "dotted"),
      name = NULL
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        order = 1,
        override.aes = list(linewidth = 1.5, alpha = 1)
      ),
      linetype = ggplot2::guide_legend(
        order = 2,
        override.aes = list(colour = pal$wtp_line, linewidth = 1.0)
      )
    ) +
    ggplot2::labs(
      title    = NULL,
      # EVPI context in subtitle
      subtitle = paste0("Expected opportunity loss per strategy ",
                        "(expected loss at optimal = per-patient EVPI) |\n",
                        "Threshold = ", format(wtp, big.mark = ","),
                        " NOK/QALY"),
      x        = "Cost-effectiveness threshold (1,000 NOK per QALY)",
      y        = "Expected Loss (NOK)"
    ) +
    theme_thesis()

  # Programmatic caption
  el_caption <- paste0(
    "Expected Loss Curves. ",
    "Expected opportunity loss per strategy across threshold values. ",
    if (!is.na(el_exercise_at_wtp)) {
      paste0("At threshold = NOK ", format(wtp, big.mark = ","),
             "/QALY: Exercise expected loss = NOK ",
             format(round(el_exercise_at_wtp), big.mark = ","), ". ")
    } else "",
    "Based on ", format(nrow(el_df), big.mark = ","),
    " threshold values (extended healthcare perspective, ",
    "effective discount rate 4% per Rundskriv R-109 stepped schedule)."
  )
  cat("PROGRAMMATIC CAPTION:\n")
  cat(el_caption, "\n\n")
  attr(p, "caption") <- el_caption
  p
}


# --- 5. PSM State Occupancy --------------------------------------------------

#' PSM State Occupancy Plot (Stacked Area)
#'
#' Shows the proportion of the cohort in each health state over time.
#' Visualises the PSM trace for one or both arms.
#'
#' @param psm_trace Data frame from build_psm_trace().
#' @return ggplot object.
plot_psm_trace <- function(psm_trace) {
  long_trace <- tidyr::pivot_longer(
    psm_trace,
    cols      = c(p_dfs, p_prog, p_dead),
    names_to  = "state",
    values_to = "probability"
  )
  # Single factor call with reversed levels instead of
  # creating the factor then immediately reversing it (double allocation).
  # Reversed so DFS is at bottom (largest area first) in stacked area chart.
  # GUIDELINE: Woods et al. 2017, NICE DSU TSD 19  --  standard PSM visualisation
  long_trace$state <- factor(long_trace$state,
                              levels = c("p_dead", "p_prog", "p_dfs"),
                              labels = c("Dead", "Progressed",
                                         "Disease-Free"))

  n_arms <- length(unique(long_trace$arm))
  max_time <- max(long_trace$time, na.rm = TRUE)

  p <- ggplot2::ggplot(long_trace,
                  ggplot2::aes(x = time, y = probability,
                               fill = state)) +
    ggplot2::geom_area(position = "stack", alpha = 0.85) +
    ggplot2::facet_wrap(~ arm) +
    ggplot2::scale_fill_manual(
      # Health-state colours distinct from strategy colours (Exercise/Std Care)
      values = c("Disease-Free" = "#009E73",
                 "Progressed"   = "#CC6677",
                 "Dead"         = pal$neutral)
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(),
                                 limits = c(0, 1)) +
    ggplot2::labs(
      title    = NULL,
      subtitle = NULL,
      x        = "Time (years)",
      y        = "Proportion in State",
      fill     = "Health State"
    ) +
    theme_thesis()

  psm_caption <- paste0(
    "PSM State Occupancy. 3-state partitioned survival model ",
    "(Disease-Free, Progressed, Dead) over ",
    round(max_time), " years. ",
    n_arms, " arms (Exercise, Standard Care). ",
    "(Extended healthcare perspective, ",
    "effective discount rate 4% per Rundskriv R-109 stepped schedule)."
  )
  cat("PROGRAMMATIC CAPTION:\n")
  cat(psm_caption, "\n\n")
  attr(p, "caption") <- psm_caption
  p
}


# --- 6. PSA Convergence Diagnostic (ADOPT A5) --------------------------------
# Note: plot_psa_convergence() is defined in 06-psa.R


# =============================================================================
# SURVIVAL FIGURES (overlay, extrapolation, hazard)
# GUIDELINE: NICE DSU TSD 14, Sections 4.3-4.6 (Latimer 2013)
# These figures visualise the parametric survival fitting results from
# 02-fit-survival.R.
#
# REFERENCE CODE PROVENANCE:
#   Survival fit overlay on KM: standard practice per NICE DSU TSD 14
#     (Latimer 2013, Section 4.3). Visual inspection of parametric fits
#     against reconstructed KM data.
#   KM reconstruction method: Guyot P, Ades AE, Ouwens MJ, Welton NJ (2012).
#     "Enhanced secondary analysis of survival data: reconstructing the data
#     from published Kaplan-Meier survival curves." BMC Medical Research
#     Methodology 12:9. DOI 10.1186/1471-2288-12-9. PMID 22297116.
#   Hazard function inspection: NICE DSU TSD 14, Section 4.4. Clinical
#     plausibility of long-term hazard behaviour guides distribution selection.
#   General population mortality overlay: NICE DSU TSD 21 (Rutherford et al.
#     2020). Extrapolated survival must not exceed general population.
#   (NICE TSDs used as supplementary references; DMP/NOMA has no equivalent
#     survival visualisation or extrapolation methodology guidance.)
# =============================================================================


#' Survival Overlay: KM + Parametric Fits
#'
#' Overlays all candidate parametric survival curves on the reconstructed
#' Kaplan-Meier curve for visual goodness-of-fit assessment. Selected
#' distribution is highlighted per the selected distribution colour standard.
#'
#' @param fits Named list of flexsurvreg objects (from fit_distributions()).
#' @param ipd_ctrl Data frame with 'time' and 'status' columns (control arm).
#' @param endpoint_label Character. "DFS" or "OS".
#' @param selected_dist Character. Distribution name (e.g., "lnorm").
#' @return ggplot object with programmatic caption in attr(, "caption").
plot_survival_overlay <- function(fits, ipd_ctrl, endpoint_label,
                                  selected_dist = NULL) {
  if (is.null(selected_dist)) {
    stop("plot_survival_overlay: selected_dist must be provided.")
  }
  # KM from reconstructed IPD (Guyot et al., 2012)
  km_fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = ipd_ctrl)
  km_df <- data.frame(
    time = c(0, km_fit$time),
    surv = c(1, km_fit$surv)
  )

  max_time <- max(ipd_ctrl$time, na.rm = TRUE)
  t_grid <- seq(0.01, max_time, length.out = 300)

  # Parametric fit predictions within observed data period
  surv_list <- lapply(names(fits), function(d) {
    pred <- summary(fits[[d]], t = t_grid, type = "survival")[[1]]
    data.frame(
      time       = pred$time,
      surv       = pred$est,
      dist_name  = d,
      dist_label = dist_labels[d],
      stringsAsFactors = FALSE
    )
  })
  surv_df <- do.call(rbind, surv_list)

  # Highlight flag for selected distribution
  surv_df$highlight <- ifelse(surv_df$dist_name == selected_dist,
                               "Selected", "Other")

  # Caution: over a full 0-1 axis the fitted curves and the KM sit in a single
  # band at the top of the panel, so a reader cannot separate the distributions
  # or judge how far apart they run, which is the only thing this figure is for.
  # The window below starts at the round 0.05 step at or below the lowest
  # plotted survival less a 0.02 margin, so every plotted point stays inside the
  # panel and only empty space is removed. Ticks are derived from the same
  # window: the default 0/0.25/0.5/0.75/1 grid leaves one or two labelled ticks
  # inside it, and a truncated probability axis whose level cannot be read off
  # the axis is worse than the blank space it replaces. The union keeps 1.00
  # labelled where a 0.1 step from an odd floor would otherwise stop at 0.95.
  y_zoom_lo <- max(0, floor((min(c(surv_df$surv, km_fit$surv),
                                 na.rm = TRUE) - 0.02) * 20) / 20)
  y_zoom_breaks <- union(seq(y_zoom_lo, 1,
                             by = if (1 - y_zoom_lo > 0.6) 0.1 else 0.05), 1)

  sel_label <- dist_labels[selected_dist]

  # Add KM to the colour aesthetic so it appears in the legend
  km_df$dist_label <- "Reconstructed Kaplan-Meier"

  # Colour map: KM (black) + all converged distributions
  overlay_colours <- c(
    "Reconstructed Kaplan-Meier" = "#000000",
    dist_colours[dist_labels[names(fits)]]
  )

  # Dynamic "(selected)" suffix in legend
  local_labels <- dist_labels
  local_labels[selected_dist] <- paste0(local_labels[selected_dist],
                                         " (selected)")
  # Rebuild colour map with "(selected)" label
  overlay_colours <- c(
    "Reconstructed Kaplan-Meier" = "#000000",
    setNames(dist_colours[dist_labels[names(fits)]],
             local_labels[names(fits)])
  )
  surv_df$dist_label <- local_labels[surv_df$dist_name]

  # GUIDELINE: NICE DSU TSD 14 recommendation 4 requires numbers-at-risk data
  # on diagrams of Kaplan-Meier curves where visual inspection is used (NICE
  # TSD 14 used as supplementary reference; no equivalent DMP/NOMA or ISPOR
  # guidance identified).
  # Caution: add_risktable() is guarded and refuses any object that is not a
  # ggsurvfit, so the KM base layer has to be built by ggsurvfit() from the
  # survfit object rather than by geom_step() from a data frame. The cheap
  # hand-rolled alternative (annotate() below the panel) was measured and
  # REJECTED: the existing y-scale limits clip every annotation and the figure
  # renders with the risk numbers silently ABSENT.
  # The parametric layers below carry inherit.aes = FALSE, so they are
  # unaffected by the base layer's own mapping.
  p <- ggsurvfit::ggsurvfit(km_fit, linewidth = 0.8) +
    ggplot2::aes(colour = "Reconstructed Kaplan-Meier") +
    # Non-selected distributions (de-emphasised)
    # Standardized linewidth/alpha via constants
    ggplot2::geom_line(
      data = surv_df[surv_df$highlight == "Other", ],
      ggplot2::aes(x = time, y = surv, colour = dist_label),
      inherit.aes = FALSE,
      linewidth = other_linewidth, alpha = other_alpha
    ) +
    # Selected distribution (highlighted)
    ggplot2::geom_line(
      data = surv_df[surv_df$highlight == "Selected", ],
      ggplot2::aes(x = time, y = surv, colour = dist_label),
      inherit.aes = FALSE,
      linewidth = sel_linewidth, alpha = sel_alpha
    ) +
    ggrepel::geom_text_repel(
      data = series_end_labels(
        rbind(surv_df[, c("time", "surv", "dist_label")],
              data.frame(time = max(km_fit$time),
                         surv = min(km_fit$surv),
                         dist_label = "Reconstructed Kaplan-Meier")),
        "time", "dist_label"),
      ggplot2::aes(x = time, y = surv, colour = dist_label,
                   label = dist_label),
      inherit.aes = FALSE, show.legend = FALSE,
      direction = "y", hjust = 0, nudge_x = 0.35,
      segment.size = 0.3, min.segment.length = 0, size = 3.4,
      family = "Charter", box.padding = 0.18, seed = 20260803
    ) +
    # End of observed data (mapped to linetype for legend entry)
    ggplot2::geom_vline(ggplot2::aes(xintercept = max_time,
                                      linetype = "End of observed data"),
                        colour = "#666666", linewidth = 0.7,
                        key_glyph = "path") +
    ggplot2::scale_colour_manual(
      values = overlay_colours,
      name   = "",
      guide  = "none"
    ) +
    ggplot2::scale_linetype_manual(
      values = c("End of observed data" = "dashed"),
      name = NULL
    ) +
    # Caution: a guides() entry OVERRIDES the scale's own guide argument, so the
    # colour entry must be removed here as well as suppressed on the scale, or
    # the retired distribution legend renders anyway.
    ggplot2::guides(
      linetype = ggplot2::guide_legend(order = 2,
        override.aes = list(colour = "#666666", linewidth = 1.0)
      )
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = y_zoom_breaks,
      labels = scales::label_number(accuracy = 0.01)
    ) +
    # coord_cartesian ZOOMS: the scale limits stay 0-1, so no plotted point is
    # dropped and no series is pinned flat along a limit.
    ggplot2::coord_cartesian(ylim = c(y_zoom_lo, 1)) +
    # Caution: the end-of-observed-data rule landed between labelled ticks, so its
    # value could not be read off the axis. Adding it as its own break puts the
    # number on the axis, and the padded upper limit reserves the band the
    # right-edge series labels occupy.
    ggplot2::scale_x_continuous(
      limits = c(0, max_time * 1.30),
      # Caution: a pretty break falling within a hair of max_time collides with
      # the end-of-observation label and makes add_risktable() emit an extra
      # column past the last follow-up time. Drop those, then add the rule.
      breaks = local({
        b <- scales::breaks_pretty()(c(0, max_time))
        sort(unique(c(b[b < max_time * 0.95], round(max_time, 1))))
      })
    ) +
    ggplot2::labs(
      title    = NULL,
      subtitle = NULL,
      x        = "Time (years)",
      y        = "Survival Probability",
      colour   = "Distribution"
    ) +
    theme_thesis() +
    ggsurvfit::add_risktable(
      risktable_stats = "n.risk",
      stats_label = list(n.risk = "Number at risk")
    )

  cap <- paste0(
    endpoint_label, " survival overlay ",
    "(per NICE DSU TSD 14 Section 4.3; supplementary; no DMP/NOMA equivalent). ",
    length(fits), " parametric distributions fitted to Standard Care arm ",
    "(", nrow(ipd_ctrl), " patients). ",
    "Selected: ", sel_label, " (highlighted). ",
    "KM reconstructed from published curves (Guyot et al., 2012). ",
    "End of observed data: ", round(max_time, 1), " years."
  )
  cat("PROGRAMMATIC CAPTION:\n", cap, "\n\n")
  attr(p, "caption") <- cap
  p
}


#' Survival Extrapolation: Long-Term Parametric Predictions
#'
#' Extrapolates all candidate distributions beyond observed data to the
#' model time horizon. Includes end-of-data marker and optional general
#' population mortality overlay (OS only, per NICE DSU TSD 21).
#'
#' @param fits_ctrl Named list of control-arm flexsurvreg objects.
#' @param fits_int Named list of intervention-arm flexsurvreg objects.
#' @param ipd_ctrl Control-arm data frame with 'time' and 'status' columns.
#' @param ipd_int Intervention-arm data frame with 'time' and 'status' columns.
#' @param endpoint_label Character. "DFS" or "OS".
#' @param selected_dist Character. Distribution name.
#' @param n_years Numeric. Extrapolation horizon in years.
#' @param s_genpop Numeric vector. General population survival (NULL = skip).
#' @param cycle_length Numeric. Model cycle length in years (for genpop time).
#' @return ggplot object.
plot_extrapolation <- function(fits_ctrl, fits_int, ipd_ctrl, ipd_int,
                                endpoint_label, selected_dist = NULL,
                                show_legend = TRUE,
                                n_years = NULL, s_genpop = NULL,
                                cycle_length = NULL) {
  if (is.null(selected_dist)) {
    stop("plot_extrapolation: selected_dist must be provided.")
  }
  expected_fit_names <- names(dist_labels)
  if (!is.list(fits_ctrl) || !is.list(fits_int) ||
      !identical(names(fits_ctrl), expected_fit_names) ||
      !identical(names(fits_int), expected_fit_names)) {
    stop("plot_extrapolation: both arms must contain the six named fit objects.")
  }
  if (!selected_dist %in% expected_fit_names) {
    stop("plot_extrapolation: selected_dist is absent from the arm-specific fits.")
  }
  required_ipd_columns <- c("time", "status")
  if (!is.data.frame(ipd_ctrl) || !is.data.frame(ipd_int) ||
      !all(required_ipd_columns %in% names(ipd_ctrl)) ||
      !all(required_ipd_columns %in% names(ipd_int))) {
    stop("plot_extrapolation: both IPD inputs must contain time and status.")
  }
  if (is.null(n_years)) {
    if (exists("time_horizon", envir = globalenv())) {
      n_years <- get("time_horizon", envir = globalenv())
    } else {
      stop("plot_extrapolation: n_years must be provided or time_horizon must exist.")
    }
  }
  max_obs <- max(c(ipd_ctrl$time, ipd_int$time), na.rm = TRUE)
  t_grid <- seq(0.01, n_years, length.out = 500)

  # KM from reconstructed IPD for both arms in the observed-data region
  # GUIDELINE: NICE DSU TSD 14 Section 4.5 recommends showing observed data
  # alongside extrapolated curves (supplementary; no DMP/NOMA equivalent)
  build_km_df <- function(ipd, arm_label) {
    km_fit <- survival::survfit(
      survival::Surv(time, status) ~ 1, data = ipd)
    data.frame(
      time = c(0, km_fit$time),
      surv = c(1, km_fit$surv),
      arm = arm_label,
      stringsAsFactors = FALSE
    )
  }
  km_df <- rbind(
    build_km_df(ipd_ctrl, "Standard care"),
    build_km_df(ipd_int, "Exercise")
  )

  # Parametric predictions to the full horizon for both arms
  build_surv_df <- function(fits, arm_label) {
    surv_list <- lapply(names(fits), function(d) {
      pred <- summary(fits[[d]], t = t_grid, type = "survival")[[1]]
      data.frame(
        time = pred$time,
        surv = pred$est,
        dist_name = d,
        dist_label = unname(dist_labels[[d]]),
        arm = arm_label,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, surv_list)
  }
  surv_df <- rbind(
    build_surv_df(fits_ctrl, "Standard care"),
    build_surv_df(fits_int, "Exercise")
  )
  surv_df$highlight <- ifelse(
    surv_df$dist_name == selected_dist, "Selected", "Other")

  sel_label <- unname(dist_labels[[selected_dist]])

  # Add KM to the colour legend; arm is shown through linetype
  km_df$dist_label <- "Reconstructed Kaplan-Meier"

  # Dynamic "(selected)" suffix in legend
  local_labels <- dist_labels
  local_labels[selected_dist] <- paste0(
    local_labels[selected_dist], " (selected)")
  extrap_dist_colours <- c(
    "Reconstructed Kaplan-Meier" = "#000000",
    setNames(
      dist_colours[dist_labels[names(fits_ctrl)]],
      local_labels[names(fits_ctrl)])
  )
  surv_df$dist_label <- local_labels[surv_df$dist_name]

  p <- ggplot2::ggplot() +
    # KM step functions for both arms
    ggplot2::geom_step(
      data = km_df,
      ggplot2::aes(
        x = time, y = surv, colour = dist_label, linetype = arm),
      linewidth = 0.8
    ) +
    # Non-selected distributions
    ggplot2::geom_line(
      data = surv_df[surv_df$highlight == "Other", ],
      ggplot2::aes(
        x = time, y = surv, colour = dist_label, linetype = arm),
      linewidth = other_linewidth, alpha = other_alpha
    ) +
    # Selected distribution
    ggplot2::geom_line(
      data = surv_df[surv_df$highlight == "Selected", ],
      ggplot2::aes(
        x = time, y = surv, colour = dist_label, linetype = arm),
      linewidth = sel_linewidth, alpha = sel_alpha
    ) +
    # End of observed data
    ggplot2::geom_vline(
      ggplot2::aes(
        xintercept = max_obs, linetype = "End of observed data"),
      colour = "#666666", linewidth = 0.7, key_glyph = "path"
    )

  # General population survival overlay
  # GUIDELINE: NICE DSU TSD 14 Section 4.5 (supplementary; DMP/NOMA has no
  # equivalent). Applied to both OS (mandatory) and DFS (recommended) to
  # allow plausibility assessment of all extrapolated curves.
  # GUIDELINE: NICE DSU TSD 21 (Rutherford et al. 2020)
  has_genpop <- !is.null(s_genpop) && !is.null(cycle_length)
  if (has_genpop) {
    t_genpop <- (seq_along(s_genpop) - 1) * cycle_length
    gp_df <- data.frame(time = t_genpop, surv = s_genpop)
    gp_df <- gp_df[gp_df$time <= n_years, ]
    keep_idx <- seq(1, nrow(gp_df), length.out = min(500, nrow(gp_df)))
    gp_df <- gp_df[round(keep_idx), ]

    p <- p +
      ggplot2::geom_line(
        data = gp_df,
        ggplot2::aes(
          x = time, y = surv, linetype = "General population (SSB)"),
        colour = "#000000", linewidth = 0.7
      )
  }

  # Arm and reference-line linetypes share one compact legend
  lt_values <- c(
    "Standard care" = "solid",
    "Exercise" = "dashed",
    "End of observed data" = "longdash"
  )
  lt_override_colours <- c("#333333", "#333333", "#666666")
  if (has_genpop) {
    lt_values <- c(lt_values, "General population (SSB)" = "dotted")
    lt_override_colours <- c(lt_override_colours, "#000000")
  }

  p <- p +
    ggplot2::scale_colour_manual(
      values = extrap_dist_colours,
      name = ""
    ) +
    ggplot2::scale_linetype_manual(
      values = lt_values,
      # Caution: this ONE key mixes two kinds of entry, arm identity (solid,
      # dashed) and reference series (longdash, dotted). Unnamed, a reader
      # cannot tell that dashed means the exercise arm while dashed-grey means
      # end of observation. The name states what the whole key encodes.
      name = "Arm / reference series"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        order = 1, ncol = 3,
        override.aes = list(linewidth = 1.5, linetype = "solid", shape = NA)),
      # Caution: the key title goes ON TOP, never inline. At ncol = 4 this row
      # already fills the canvas, so an inline title pushed both ends past the
      # edge and printed "1 / reference series" with the last key cut to "S".
      linetype = ggplot2::guide_legend(
        order = 2, ncol = 4, title.position = "top",
        override.aes = list(
          colour = lt_override_colours, linewidth = 1.0, shape = NA)),
      shape = ggplot2::guide_legend(
        order = 3, ncol = 1,
        override.aes = list(
          colour = pal$highlight, alpha = 1, linetype = 0))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, n_years, by = 10)
    ) +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      x = "Time (years)",
      y = "Survival Probability"
    ) +
    theme_thesis(base_size = 18) +
    ggplot2::theme(
      # Caution: Figure 3.2 stacks the DFS and OS panels of this function in one
      # float. Each PNG carries its own legend, so the composite printed the
      # identical legend block twice. show_legend = FALSE on the top panel
      # leaves exactly one legend at the foot of the composite, where it reads
      # as governing both panels. A.1 and A.5 are standalone appendix figures
      # and keep the default TRUE.
      legend.position = if (isTRUE(show_legend)) "bottom" else "none",
      legend.box = "vertical",
      legend.direction = "horizontal",
      # Caution: printed size = canvas size / downscale. V-004 includes this
      # 10 in canvas at 0.9\textwidth (downscale 2.016), so 9 pt set here
      # printed at 4.46 pt. 15 pt prints at 7.44 pt, clearing the 7 pt floor.
      legend.text = ggplot2::element_text(size = 15),
      legend.key.width = grid::unit(1.2, "cm"),
      legend.spacing = grid::unit(0, "cm"),
      legend.box.spacing = grid::unit(0, "cm")
    )

  # Genpop crossing annotation for the selected standard-care distribution
  crossing_note <- ""
  if (has_genpop) {
    sel_pred <- summary(
      fits_ctrl[[selected_dist]], t = t_grid, type = "survival")[[1]]$est
    t_genpop_grid <- (seq_along(s_genpop) - 1) * cycle_length
    gp_interp <- stats::approx(t_genpop_grid, s_genpop, xout = t_grid)$y
    cross_idx <- which(sel_pred > gp_interp & t_grid > max_obs)
    if (length(cross_idx) > 0) {
      cross_time <- t_grid[cross_idx[1]]
      cross_surv <- sel_pred[cross_idx[1]]
      cross_df <- data.frame(x = cross_time, y = cross_surv)
      p <- p +
        ggplot2::geom_point(
          data = cross_df,
          ggplot2::aes(x = x, y = y, shape = "Exceeds general-population survival"),
          size = 3, colour = pal$highlight, show.legend = TRUE
        ) +
        ggplot2::scale_shape_manual(
          values = c("Exceeds general-population survival" = 17),
          name = ""
        )
      crossing_note <- paste0(
        " Standard-care selected distribution (", sel_label,
        ") exceeds general population survival at ",
        round(cross_time, 1), " years; hazard-max mortality constraint ",
        "(NICE DSU TSD 21) applied in the PSM.")
    } else {
      crossing_note <- paste0(
        " Standard-care selected distribution (", sel_label,
        ") does not exceed general population survival ",
        "over the extrapolation horizon.")
    }
  }

  gp_note <- if (has_genpop) {
    paste0(" General population mortality overlay (SSB).", crossing_note)
  } else {
    ""
  }
  cap <- paste0(
    endpoint_label, " survival extrapolation ",
    "(per NICE DSU TSD 14 Section 4.5; supplementary, no DMP/NOMA equivalent). ",
    length(fits_ctrl), " parametric distributions fitted separately to ",
    "Standard Care (", nrow(ipd_ctrl), " reconstructed patients) and Exercise (",
    nrow(ipd_int), " reconstructed patients) arms, extrapolated to ",
    n_years, " years. End of observed data: ", round(max_obs, 1),
    " years. Selected: ", sel_label, ".", gp_note
  )
  cat("PROGRAMMATIC CAPTION:\n", cap, "\n\n")
  attr(p, "caption") <- cap
  attr(p, "arm_labels") <- sort(unique(surv_df$arm))
  attr(p, "has_genpop_overlay") <- has_genpop
  p
}


#' Hazard Functions: Clinical Plausibility Assessment
#'
#' Plots the hazard function for each candidate distribution to assess
#' clinical plausibility of long-term behaviour. Selected distribution
#' highlighted. Per NICE DSU TSD 14 Section 4.4.
#'
#' @param fits Named list of flexsurvreg objects.
#' @param ipd_ctrl Data frame with 'time' and 'status' columns.
#' @param endpoint_label Character. "DFS" or "OS".
#' @param selected_dist Character. Distribution name.
#' @param n_years Numeric. Hazard display horizon.
#' @param s_genpop Numeric vector. General population survival (NULL = skip).
#' @param cycle_length Numeric. Model cycle length in years (for genpop time).
#' @param log_scale Logical. If TRUE, use log10 y-axis (appendix supplement).
#' @return ggplot object.
plot_hazard <- function(fits, ipd_ctrl, endpoint_label,
                         selected_dist = NULL,
                         n_years = NULL,
                         s_genpop = NULL,
                         cycle_length = NULL,
                         log_scale = FALSE) {
  if (is.null(selected_dist)) {
    stop("plot_hazard: selected_dist must be provided.")
  }
  if (is.null(n_years)) {
    if (exists("time_horizon", envir = globalenv())) {
      n_years <- get("time_horizon", envir = globalenv())
    } else {
      stop("plot_hazard: n_years must be provided or time_horizon must exist.")
    }
  }
  max_obs <- max(ipd_ctrl$time, na.rm = TRUE)
  t_grid <- seq(0.01, n_years, length.out = 500)

  # Hazard predictions
  haz_list <- lapply(names(fits), function(d) {
    # GUIDELINE: Present fitted and extrapolated hazard functions for survival-model assessment (Rutherford et al. 2020)
    pred <- summary(fits[[d]], t = t_grid, type = "hazard")[[1]]
    data.frame(
      time       = pred$time,
      hazard     = pred$est,
      dist_name  = d,
      dist_label = dist_labels[d],
      stringsAsFactors = FALSE
    )
  })
  haz_df <- do.call(rbind, haz_list)
  haz_df$highlight <- ifelse(haz_df$dist_name == selected_dist,
                              "Selected", "Other")

  sel_label <- dist_labels[selected_dist]

  # Cap extreme hazard values for readable y-axis (linear scale only)
  # Caution: Using a global percentile (e.g., 99th) fails when one distribution
  # (Gompertz OS) has a hazard explosion that squashes all others near y=0.
  # Instead, cap at 2x the selected distribution's maximum hazard, ensuring
  # the clinically relevant distributions remain readable.
  sel_max_haz <- max(haz_df$hazard[haz_df$dist_name == selected_dist],
                     na.rm = TRUE)
  y_cap <- max(sel_max_haz * 2,
               quantile(haz_df$hazard, 0.75, na.rm = TRUE) * 1.5)

  # Annotation y-position depends on scale
  # Label position: log scale uses bottom (below curves), linear uses top
  if (log_scale) {
    sel_haz_vals_tmp <- haz_df$hazard[haz_df$dist_name == selected_dist]
    log_y_min_tmp <- max(min(sel_haz_vals_tmp, na.rm = TRUE) * 0.1, 1e-4)
    ann_y <- log_y_min_tmp * 5  # Above bottom boundary, below curves
  } else {
    ann_y <- y_cap * 0.90  # Top of linear plot, above curves
  }

  # Dynamic "(selected)" suffix in legend
  local_labels <- dist_labels
  local_labels[selected_dist] <- paste0(local_labels[selected_dist],
                                         " (selected)")
  haz_df$dist_label <- local_labels[haz_df$dist_name]
  haz_colours <- setNames(dist_colours[dist_labels[names(fits)]],
                           local_labels[names(fits)])

  p <- ggplot2::ggplot() +
    # Non-selected (standardized constants)
    ggplot2::geom_line(
      data = haz_df[haz_df$highlight == "Other", ],
      ggplot2::aes(x = time, y = hazard, colour = dist_label),
      linewidth = other_linewidth, alpha = other_alpha
    ) +
    # Selected (standardized constants)
    ggplot2::geom_line(
      data = haz_df[haz_df$highlight == "Selected", ],
      ggplot2::aes(x = time, y = hazard, colour = dist_label),
      linewidth = sel_linewidth, alpha = sel_alpha
    ) +
    # End of observed data (mapped to linetype for legend entry)
    ggplot2::geom_vline(ggplot2::aes(xintercept = max_obs,
                                      linetype = "End of observed data"),
                        colour = "#666666", linewidth = 0.7,
                        key_glyph = "path") +
    ggplot2::scale_colour_manual(
      values = haz_colours,
      name   = ""
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, n_years, by = 10)
    )

  # General population hazard overlay
  # GUIDELINE: NICE DSU TSD 14 Section 4.4 (supplementary; DMP/NOMA has no
  # equivalent). Allows assessment of whether parametric hazards converge
  # toward background mortality at long horizons.
  gp_note <- ""
  has_genpop_haz <- !is.null(s_genpop) && !is.null(cycle_length)
  if (has_genpop_haz) {
    n_gp <- length(s_genpop) - 1
    h_genpop <- numeric(n_gp)
    for (i in seq_len(n_gp)) {
      ratio <- s_genpop[i + 1] / max(s_genpop[i], 1e-15)
      h_genpop[i] <- -log(max(ratio, 1e-15)) / cycle_length
    }
    t_genpop <- (seq_len(n_gp) - 1) * cycle_length
    # Sub-sample to 500 points for smooth rendering
    # (consistent with plot_extrapolation() sub-sampling)
    gp_haz_df <- data.frame(time = t_genpop, hazard = h_genpop)
    gp_haz_df <- gp_haz_df[gp_haz_df$time <= n_years, ]
    if (nrow(gp_haz_df) > 500) {
      interp <- stats::approx(gp_haz_df$time, gp_haz_df$hazard, n = 500)
      gp_haz_df <- data.frame(time = interp$x, hazard = interp$y)
    }

    p <- p +
      # General population (mapped to linetype for legend entry)
      ggplot2::geom_line(
        data = gp_haz_df,
        ggplot2::aes(x = time, y = hazard,
                     linetype = "General population (SSB)"),
        colour = "#000000", linewidth = 0.7
      )
    gp_note <- " General population hazard overlay (SSB life table)."
  }

  haz_lt_values <- c("End of observed data" = "dashed")
  haz_lt_colours <- "#666666"
  if (has_genpop_haz) {
    haz_lt_values <- c(haz_lt_values, "General population (SSB)" = "dotted")
    haz_lt_colours <- c(haz_lt_colours, "#000000")
  }

  p <- p +
    ggplot2::scale_linetype_manual(
      values = haz_lt_values,
      name = NULL
    ) +
    ggplot2::guides(
      colour   = ggplot2::guide_legend(order = 1, nrow = 3,
                   override.aes = list(linewidth = 1.5, shape = NA)),
      linetype = ggplot2::guide_legend(order = 2,
                   override.aes = list(colour = haz_lt_colours,
                                        linewidth = 1.0, shape = NA))
    )

  # Y-axis scale: log or linear
  if (log_scale) {
    # DECISION: the log panel's window is set from EVERY plotted series, not from
    #   the selected distribution alone. 1e-6 is the fail-closed floor for a
    #   non-positive or empty minimum.
    log_y_min <- max(min(haz_df$hazard[haz_df$hazard > 0], na.rm = TRUE), 1e-6)
    # DECISION: the ceiling mirrors the floor above and spans EVERY plotted
    #   series, the general-population overlay included. A ceiling from the
    #   selected distribution alone let the Gompertz OS hazard leave the panel
    #   through its top, so the panel stopped short of the horizon end.
    log_y_max <- max(haz_df$hazard,
                     if (has_genpop_haz) gp_haz_df$hazard else NULL,
                     na.rm = TRUE)
    # Caution: scale limits + oob = squish PIN every out-of-range series to the
    # limit, drawing a horizontal line where the fitted hazard keeps moving.
    # Measured on Figures A.3 and A.7: Gompertz and the SSB life-table overlay
    # both ran flat along a limit. coord_cartesian ZOOMS instead, clipping the
    # series at the panel edge exactly as the linear branch below already does.
    p <- p + ggplot2::scale_y_log10(
      labels = scales::label_number()
    ) + ggplot2::coord_cartesian(ylim = c(log_y_min, log_y_max))
  } else {
    p <- p + ggplot2::coord_cartesian(ylim = c(0, y_cap))
  }

  # Selected distribution annotation in legend only
  p <- p +
    ggplot2::labs(
      title    = NULL,
      subtitle = NULL,
      x        = "Time (years)",
      y        = "Hazard Rate (per person-year)"
    ) +
    theme_thesis() +
    # Legend inherits the house "bottom" position from theme_thesis(); the
    # "right" override squeezed the panel to roughly two thirds of the figure
    # width. Caution: the foot box must STACK the colour and linetype keys.
    # Side by side they overran the canvas and cut "(SSB)" off the
    # general-population key on the log branch (gate G32 measures this).
    ggplot2::theme(
      legend.box           = "vertical",
      legend.spacing       = ggplot2::unit(0, "cm"),
      legend.box.spacing   = ggplot2::unit(0, "cm")
    )

  scale_note <- if (log_scale) " Log-scale y-axis shows full dynamic range." else ""
  cap <- paste0(
    endpoint_label, " hazard functions ",
    "(per NICE DSU TSD 14 Section 4.4; supplementary, no DMP/NOMA equivalent). ",
    length(fits), " candidate distributions fitted to Standard Care arm ",
    "(", nrow(ipd_ctrl), " patients, KM reconstructed via Guyot et al., 2012). ",
    "End of observed data: ", round(max_obs, 1), " years. ",
    "Selected: ", sel_label, ".",
    gp_note, scale_note
  )
  cat("PROGRAMMATIC CAPTION:\n", cap, "\n\n")
  attr(p, "caption") <- cap
  p
}


# =============================================================================
# ADDITIONAL ANALYSIS FIGURES
# 6 plot functions for CHEERS 2022, ISPOR, NICE DSU TSD 14 compliance
# =============================================================================


# --- Model Structure Diagram ------------------------------------------
#' 3-State PSM Model Structure Diagram
#'
#' Static diagram showing the partitioned survival model structure:
#' Disease-Free (DF) -> Progressed Disease (PD) -> Dead.
#' GUIDELINE: CHEERS Item 13, ISPOR Roberts et al. 2012.
#'
#' @return ggplot object.
plot_model_structure <- function() {
  # State positions (x, y coordinates for bubble placement)
  states <- data.frame(
    name  = c("Disease-Free\n(DF)", "Progressed\nDisease (PD)", "Dead"),
    x     = c(1, 3, 2),
    y     = c(2, 2, 0.15),
    # Health-state colours distinct from strategy colours (Exercise/Std Care)
    # to avoid semantic confusion with the blue=Exercise, orange=Std Care convention
    color = c("#009E73", "#CC6677", pal$neutral),
    stringsAsFactors = FALSE
  )

  # Transition arrows (from -> to)
  # All arrows sized to float between circles with clear visual gaps
  # (~0.55-0.60 data units from circle centers, matching circle radius)
  arrows <- data.frame(
    x    = c(1.55, 1.25, 2.75),
    xend = c(2.45, 1.75, 2.25),
    y    = c(2.0,  1.45, 1.45),
    yend = c(2.0,  0.65, 0.65),
    label = c(
      "Disease-free to progressed",
      "Disease-free to dead",
      "Progressed to dead"
    ),
    stringsAsFactors = FALSE
  )

  # Staying arrows for the two living states
  self_loops <- data.frame(
    x = c(0.72, 2.72),
    xend = c(1.28, 3.28),
    y = c(2.42, 2.42),
    yend = c(2.42, 2.42),
    label_x = c(1, 3),
    label_y = c(3.08, 3.08),
    label = c("Remain\ndisease-free", "Remain\nprogressed"),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() +
    # State bubbles
    ggplot2::geom_point(
      data = states,
      ggplot2::aes(x = x, y = y),
      size = 40, colour = states$color, fill = states$color,
      shape = 21, alpha = 0.3
    ) +
    ggplot2::geom_point(
      data = states,
      ggplot2::aes(x = x, y = y),
      size = 40, colour = states$color, fill = NA,
      shape = 21, stroke = 1.5
    ) +
    # State labels (bold annotation tier)
    ggplot2::geom_text(
      data = states,
      ggplot2::aes(x = x, y = y, label = name),
      size = ann_size_bold, fontface = "bold", family = "Charter"
    ) +
    # Transition arrows
    ggplot2::geom_segment(
      data = arrows,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed"),
      linewidth = 0.8, colour = "#333333"
    ) +
    # Self-loop arrows show remaining in each living state within a cycle
    ggplot2::geom_curve(
      data = self_loops,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      curvature = -1.15,
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed"),
      linewidth = 0.8, colour = "#333333"
    ) +
    ggplot2::geom_text(
      data = self_loops,
      ggplot2::aes(x = label_x, y = label_y, label = label),
      size = ann_size_secondary, family = "Charter", colour = "#333333"
    ) +
    # Arrow labels
    ggplot2::annotate(
      "label", x = 2.0, y = 2.15,
      label = "Disease-free to progressed", size = ann_size_secondary,
      family = "Charter", colour = "#333333",
      fill = scales::alpha("white", 0.85), linewidth = 0,
      label.padding = ann_label_padding
    ) +
    ggplot2::annotate(
      "label", x = 1.20, y = 1.05,
      label = "Disease-free\nto dead", size = ann_size_secondary,
      family = "Charter", colour = "#333333",
      fill = scales::alpha("white", 0.85), linewidth = 0,
      label.padding = ann_label_padding
    ) +
    ggplot2::annotate(
      "label", x = 2.80, y = 1.05,
      label = "Progressed\nto dead", size = ann_size_secondary,
      family = "Charter", colour = "#333333",
      fill = scales::alpha("white", 0.85), linewidth = 0,
      label.padding = ann_label_padding
    ) +
    # State equations are reported once in Methods and are intentionally
    # omitted from the schematic.
    ggplot2::coord_cartesian(xlim = c(0, 4), ylim = c(-0.3, 3.35)) +
    ggplot2::labs(
      title    = NULL,
      subtitle = NULL
    ) +
    theme_thesis() +
    ggplot2::theme(
      axis.text        = ggplot2::element_blank(),
      axis.title       = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  cap <- paste0(
    "Model structure. Three-state partitioned survival model ",
    "(Disease-Free, Progressed Disease, Dead). ",
    "State occupancy equations are reported in Methods. ",
    "Applied to CHALLENGE trial data (Courneya et al., NEJM 2025)."
  )
  cat("PROGRAMMATIC CAPTION:\n", cap, "\n\n")
  attr(p, "caption") <- cap
  p
}


# --- EVPPI Bar Chart -------------------------------------------------
#' EVPPI Bar Chart
#'
#' Horizontal bar chart of Expected Value of Partial Perfect Information
#' per parameter. GUIDELINE: CHEERS-VOI Item S3, Fenwick et al. 2020 GPR.
#'
#' @param evppi_results Data frame with columns: parameter, evppi, pct_evpi.
#'   Attribute "evpi" contains per-patient EVPI. Attribute "wtp" contains WTP.
#' @return ggplot object.
plot_evppi_bar <- function(evppi_results) {
  evpi_val <- attr(evppi_results, "evpi")
  wtp_val  <- attr(evppi_results, "wtp")
  if (is.null(evpi_val)) stop("plot_evppi_bar: evppi_results must have attr 'evpi'.")
  if (is.null(wtp_val))  stop("plot_evppi_bar: evppi_results must have attr 'wtp'.")

  # Parameter label mapping
  # SOURCE: the `params_to_test` default block inside compute_evppi() in 06-psa.R
  # and sample_psa_parameters() output columns
  param_labels <- c(
    HR_OS                = "Hazard ratio, OS",
    HR_DFS               = "Hazard ratio, DFS",
    u_dfs                = "Utility, disease-free",
    u_prog               = "Utility, progressed",
    c_progressed_annual  = "Shared progressed-disease annual cost",
    c_terminal           = "Terminal care cost",
    c_surveillance_early = "Shared surveillance annual cost",
    c_exercise_annual    = "Exercise-programme annual cost",
    c_intervention_setup = "Exercise programme setup cost",
    "surv_coef_dfs (grouped)" = "DFS baseline coefficients (grouped)",
    "surv_coef_os (grouped)" = "OS baseline coefficients (grouped)",
    "HR_OS + HR_DFS (joint)" = "Hazard ratios, OS + DFS (joint)",
    c_terminal           = "Terminal care cost"
  )
  evppi_results$display_label <- ifelse(
    evppi_results$parameter %in% names(param_labels),
    param_labels[evppi_results$parameter],
    evppi_results$parameter
  )

  # Order by EVPPI value (highest at top)
  evppi_results$display_label <- factor(
    evppi_results$display_label,
    levels = evppi_results$display_label[order(evppi_results$evppi)]
  )

  # Percentage labels for bar ends
  # DECISION: display-consistency rule. Displayed percentages are derived from
  # the rounded displayed operands, pct = round(100 * round(EVPPI NOK) /
  # round(EVPI NOK), 1), so every bar agrees with Table 4.6 and the Abstract
  # rather than with the upstream unrounded pct_evpi.
  # Caution: round() drops a trailing zero, so a whole-tenth value printed as
  #   "59%" against the prose macro's "59.0%". sprintf holds one decimal for
  #   every bar, so precision does not vary within a single figure.
  evppi_results$pct_display <- round(
    100 * round(evppi_results$evppi) / round(evpi_val), 1
  )
  evppi_results$pct_label <- paste0(sprintf("%.1f", evppi_results$pct_display), "%")

  n_params <- nrow(evppi_results)

  p <- ggplot2::ggplot(evppi_results,
                        ggplot2::aes(x = evppi, y = display_label)) +
    ggplot2::geom_col(fill = pal$intervention, alpha = 0.85, width = 0.6) +
    # EVPI reference line
    ggplot2::geom_vline(
      xintercept = evpi_val,
      linetype   = "dashed",
      colour     = pal$highlight,
      linewidth  = 0.8
    ) +
    # EVPI annotation (vertically centred): RESULT annotation, stays inline
    # Percentage labels at bar ends (standard annotation tier)
    ggplot2::geom_text(
      ggplot2::aes(label = pct_label),
      hjust = -0.2, size = ann_size_standard, family = "Charter"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(
      title    = NULL,
      subtitle = NULL,
      x        = "EVPPI (NOK per patient)",
      y        = "Parameter"
    ) +
    theme_thesis()

  cap <- paste0(
    "Expected Value of Partial Perfect Information (EVPPI). ",
    nrow(evppi_results), " parameters assessed via GAM regression ",
    "(Strong, Oakley, and Brennan 2014). ",
    evppi_results$parameter[which.max(evppi_results$evppi)],
    " dominates (",
    evppi_results$pct_display[which.max(evppi_results$evppi)],
    "% of EVPI). Per-patient EVPI reference line at ",
    format(evpi_val, big.mark = ","), " NOK. ",
    "Threshold = ", format(wtp_val, big.mark = ","), " NOK/QALY. ",
    "Based on PSA iterations with costs and QALYs discounted at ",
    "effective rate 4% per Rundskriv R-109 stepped schedule ",
    "(extended healthcare perspective)."
  )
  cat("PROGRAMMATIC CAPTION:\n", cap, "\n\n")
  attr(p, "caption") <- cap
  p
}


# --- Structural SA INMB Forest Plot -----------------------------------
#' Structural Sensitivity Analysis INMB Forest Plot
#'
#' One row per executed scenario; x = incremental net monetary benefit
#' (INMB) at the scenario's own severity-weighted cost-effectiveness
#' threshold: INMB_s = sev_reference_s x inc_qalys_s - inc_costs_s.
#' DECISION: INMB replaces the ICER axis. Eight scenarios are south-east-
#' quadrant dominant; negative ICERs are not meaningful on a continuous
#' axis (Briggs et al. 2012, ISPOR-SMDM Task Force-6, p729: reporting
#' negative ICERs "should be avoided"; Briggs, Claxton & Sculpher 2006
#' pp127-129; McCabe et al. 2020). Full ICER detail stays in the
#' structural-SA results table (LaTeX label tab:table04).
#' GUIDELINE: CHEERS 2022 Item 20 (characterizing uncertainty), DMP standard.
#' SOURCE: structural_sa_results.csv columns inc_costs, inc_qalys,
#'   sev_group, sev_reference (written by R/06b-structural-sa.R).
#'
#' @param ssa_results Data frame with scenario, description, inc_costs,
#'   inc_qalys, icer, sev_group, sev_reference columns.
#' @return ggplot object.
plot_structural_sa <- function(ssa_results) {
  scenario_labels <- c(
    base_case = "Base case",
    waning_step_5yr = "Step waning",
    waning_linear_3_10 = "Linear waning",
    no_mortality_cap = "Mortality-floor removal",
    flat_discount_4pct = "Discounting: flat 4%",
    horizon_10yr = "Horizon, 10 years",
    horizon_20yr = "Horizon, 20 years",
    mortality_pmin = "Mortality implementation",
    # DECISION: scenario display registration
    cure_point_10yr = "Cure point, 10 years",
    drug_discount_40pct = "Drug discount",
    session_cost_bare_tariff = "Bare tariff session cost",
    session_cost_driftstilskudd = "Driftstilskudd session cost",
    session_cost_thorsen_salary = "Salary-based session cost",
    patient_time_working_age_weighted = "Working-age patient time",
    stage_ii_only = "Stage II only",
    stage_iii_only = "Stage III only",
    age_50 = "Age 50",
    age_74 = "Age 74",
    norwegian_registry_age = "Registry age 73",
    norwegian_registry_age_women_76 = "Registry age 76",
    male_only = "Male only",
    female_only = "Female only",
    full_adherence = "Full adherence",
    low_adherence = "Low adherence",
    utility_devlin = "Utility source",
    dfs_gengamma = "DFS distribution"
  )
  scenario_ids <- as.character(ssa_results$scenario)
  if (anyNA(scenario_ids) || anyDuplicated(scenario_ids) ||
      !setequal(scenario_ids, names(scenario_labels)) ||
      length(scenario_ids) != length(scenario_labels)) {
    stop("plot_structural_sa: scenario display map must match source IDs exactly.")
  }
  if (anyDuplicated(unname(scenario_labels)) ||
      any(nchar(unname(scenario_labels)) > 45L)) {
    stop("plot_structural_sa: display labels must be unique and at most 45 characters.")
  }
  ssa_results$display_label <- unname(scenario_labels[scenario_ids])

  # Filter the invalid-model-state scenario (all-NA row; no_mortality_cap).
  ssa_clean <- ssa_results[!is.na(ssa_results$icer), ]
  # Caution: INMB needs every input column present and finite; fail loudly.
  needed <- c("inc_costs", "inc_qalys", "sev_group", "sev_reference")
  if (!all(needed %in% names(ssa_clean)) ||
      any(!is.finite(ssa_clean$inc_costs)) ||
      any(!is.finite(ssa_clean$inc_qalys)) ||
      any(!is.finite(ssa_clean$sev_reference))) {
    stop("plot_structural_sa: inc_costs, inc_qalys, sev_group and sev_reference must be present and finite for all executed scenarios.")
  }

  # DECISION: lambda_s = the scenario's own severity-weighted threshold
  # (CSV sev_reference), matching tab:table04's per-scenario basis and the
  # GUIDELINE: Results claim "at their severity-weighted thresholds" (DMP 2026 p39;
  # GUIDELINE: Magnussen absolute shortfall).
  ssa_clean$inmb <- ssa_clean$sev_reference * ssa_clean$inc_qalys -
    ssa_clean$inc_costs

  base_row <- ssa_clean[ssa_clean$scenario == "base_case", ]
  if (nrow(base_row) != 1L) {
    stop("plot_structural_sa: expected exactly one base_case row.")
  }
  base_inmb <- base_row$inmb

  # Row order: smallest decision margin at the TOP (first row read).
  ssa_clean$display_label <- factor(
    ssa_clean$display_label,
    levels = ssa_clean$display_label[order(-ssa_clean$inmb)]
  )

  # Severity group encoded by lightness steps of the document blue
  # (greyscale-safe); legend glosses each group's threshold from the
  # data, never a typed literal.
  sev_cols <- c(`1` = "#8FBFDE", `2` = "#4E94C4", `3` = "#0072B2",
                `4` = "#004E7A")
  grp <- sort(unique(ssa_clean$sev_group))
  refs <- vapply(grp, function(g) {
    unique(ssa_clean$sev_reference[ssa_clean$sev_group == g])
  }, numeric(1))
  grp_labels <- paste0("Group ", grp, " (",
                       format(refs, big.mark = ",", trim = TRUE), ")")
  names(grp_labels) <- as.character(grp)
  ssa_clean$sev_group_f <- factor(as.character(ssa_clean$sev_group),
                                  levels = as.character(grp))

  # Cost-saving marker: open circles for negative incremental costs.
  ssa_clean$cost_saving <- ssa_clean$inc_costs < 0

  ssa_clean$row_idx <- as.numeric(ssa_clean$display_label)
  shade_rows <- ssa_clean[ssa_clean$row_idx %% 2 == 0, ]

  p <- ggplot2::ggplot(ssa_clean,
                       ggplot2::aes(x = inmb, y = display_label)) +
    {if (nrow(shade_rows) > 0)
      ggplot2::geom_rect(
        data = shade_rows,
        ggplot2::aes(ymin = row_idx - 0.5, ymax = row_idx + 0.5),
        xmin = -Inf, xmax = Inf, fill = "#E0EAF5", inherit.aes = FALSE
      )
    } +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = inmb,
                   y = display_label, yend = display_label,
                   colour = sev_group_f),
      alpha = 0.35, linewidth = 0.6, show.legend = FALSE
    ) +
    # INMB = 0 is the decision boundary: cost-effective at lambda_s iff INMB > 0.
    ggplot2::geom_vline(xintercept = 0, colour = "#2D3748",
                        linetype = "solid", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = base_inmb, colour = pal$highlight,
                        linetype = "dashed", linewidth = 0.8) +
    ggplot2::geom_point(
      ggplot2::aes(colour = sev_group_f, shape = cost_saving),
      size = 4, stroke = 1.4, fill = "white"
    ) +
    ggplot2::scale_colour_manual(
      values = sev_cols[as.character(grp)],
      labels = grp_labels,
      name = "Severity group (threshold, NOK/QALY)"
    ) +
    ggplot2::scale_shape_manual(
      values = c(`FALSE` = 16, `TRUE` = 21),
      labels = c(`FALSE` = "Cost-increasing", `TRUE` = "Cost-saving"),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_comma(),
      breaks = scales::breaks_pretty(n = 6),
      expand = ggplot2::expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::labs(
      title = NULL, subtitle = NULL,
      x = "Incremental net monetary benefit (NOK per patient)",
      y = NULL, caption = NULL
    ) +
    theme_thesis(base_size = 18) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title.position = "top",
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    ) +
    ggplot2::guides(
      # Caution: four group keys plus the title on one bottom row exceed the
      # 10 in canvas and the legend clips at both ends, dropping Group 4.
      colour = ggplot2::guide_legend(order = 1, nrow = 2,
                                     override.aes = list(shape = 16, size = 4)),
      shape = ggplot2::guide_legend(order = 2,
                                    override.aes = list(colour = "#2D3748",
                                                        size = 4))
    )

  p
}


# --- Log-Cumulative Hazard Plot ----------------------------------------
#' Log-Cumulative Hazard Plot
#'
#' Plots log(-log(S(t))) vs log(t) from the Kaplan-Meier estimator,
#' with parametric distribution fits overlaid for visual distribution
#' selection assessment. A linear trend indicates Weibull suitability;
#' curvature suggests alternatives may fit better.
#' GUIDELINE: NICE DSU TSD 14 Section 4.3.2 (supplementary; no DMP/NOMA equiv).
#'
#' @param ipd_ctrl Data frame with 'time' and 'status' columns (control arm).
#' @param endpoint_label Character. "DFS" or "OS".
#' @param fits Named list of flexsurvreg objects (NULL = KM only).
#' @param selected_dist Character. Distribution name for highlighting (NULL = none).
#' @return ggplot object.
plot_log_cumhaz <- function(ipd_ctrl, endpoint_label,
                             fits = NULL, selected_dist = NULL) {
  # GUIDELINE: Use Kaplan-Meier and log-cumulative-hazard plots in parametric model selection (Latimer 2013)
  km_fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = ipd_ctrl)

  # Exclude S(t) = 1 (log(0) undefined) and S(t) = 0 (log(-Inf))
  # Caution: Log-cumulative hazard is undefined when survival equals zero or one.
  valid <- km_fit$surv > 0 & km_fit$surv < 1
  lch_df <- data.frame(
    # GUIDELINE: Diagnose parametric form on the log-time and log-cumulative-hazard scale (Latimer 2013)
    log_time    = log(km_fit$time[valid]),
    log_cum_haz = log(-log(km_fit$surv[valid]))
  )
  # Tag KM for colour legend
  lch_df$dist_label <- "Reconstructed Kaplan-Meier"

  max_obs <- max(ipd_ctrl$time, na.rm = TRUE)
  # GUIDELINE: NICE DSU TSD 14 Section 3.2 scopes log-cumulative hazard plots
  # to the OBSERVED hazards (supplementary; no DMP/NOMA equivalent identified).
  # Caution: evaluating the fitted curves from t = 0.01, far below the first
  # observed event, drove the y axis to about -31 and compressed the KM data,
  # which is the only diagnostic content of the figure, into under a tenth of
  # the panel. The grid now starts at the first observed event, so the axis is
  # set by the data the figure actually diagnoses. This is a domain
  # restriction, not an axis clip: no plotted series is cut.
  # Caution: Start the fitted grid at the first observed event so pre-event extrapolation cannot compress the diagnostic.
  t_first_event <- min(km_fit$time[km_fit$n.event > 0], na.rm = TRUE)
  # Caution: Restrict the diagnostic grid to observed event time through maximum follow-up.
  t_grid <- seq(t_first_event, max_obs, length.out = 300)

  # GUIDELINE: NICE DSU TSD 14 Section 4.3.2: parametric fits alongside KM
  # for distribution selection assessment
  has_fits <- !is.null(fits) && length(fits) > 0
  if (has_fits) {
    param_list <- lapply(names(fits), function(d) {
      # GUIDELINE: Overlay parametric survival fits with Kaplan-Meier diagnostics (Latimer 2013)
      pred <- summary(fits[[d]], t = t_grid, type = "survival")[[1]]
      s_vals <- pred$est
      # Filter: log(-log(S)) requires 0 < S < 1
      ok <- s_vals > 0 & s_vals < 1
      data.frame(
        # GUIDELINE: Diagnose parametric form on the log-time and log-cumulative-hazard scale (Latimer 2013)
        log_time    = log(t_grid[ok]),
        log_cum_haz = log(-log(s_vals[ok])),
        dist_name   = d,
        dist_label  = dist_labels[d],
        stringsAsFactors = FALSE
      )
    })
    param_df <- do.call(rbind, param_list)
    param_df$highlight <- ifelse(param_df$dist_name == selected_dist,
                                  "Selected", "Other")

    # Dynamic "(selected)" suffix in legend
    local_labels <- dist_labels
    if (!is.null(selected_dist)) {
      local_labels[selected_dist] <- paste0(local_labels[selected_dist],
                                             " (selected)")
    }
    param_df$dist_label <- local_labels[param_df$dist_name]

    # Colour map: KM (black) + parametric distributions
    lch_colours <- c(
      "Reconstructed Kaplan-Meier" = "#000000",
      setNames(dist_colours[dist_labels[names(fits)]],
               local_labels[names(fits)])
    )
  } else {
    lch_colours <- c(
      "Reconstructed Kaplan-Meier" = "#000000"
    )
  }

  p <- ggplot2::ggplot() +
    # KM log-cumulative hazard (step function)
    ggplot2::geom_step(
      data = lch_df,
      ggplot2::aes(x = log_time, y = log_cum_haz, colour = dist_label),
      linewidth = 0.8
    )

  if (has_fits) {
    p <- p +
      # Non-selected distributions (de-emphasised)
      ggplot2::geom_line(
        data = param_df[param_df$highlight == "Other", ],
        ggplot2::aes(x = log_time, y = log_cum_haz, colour = dist_label),
        linewidth = other_linewidth, alpha = other_alpha
      ) +
      # Selected distribution (highlighted)
      ggplot2::geom_line(
        data = param_df[param_df$highlight == "Selected", ],
        ggplot2::aes(x = log_time, y = log_cum_haz, colour = dist_label),
        linewidth = sel_linewidth, alpha = sel_alpha
      )
  }

  p <- p +
    # End of observed data (mapped to linetype for legend entry)
    ggplot2::geom_vline(ggplot2::aes(xintercept = log(max_obs),
                                      linetype = "End of observed data"),
                        colour = "#666666", linewidth = 0.7,
                        key_glyph = "path") +
    ggplot2::scale_colour_manual(
      values = lch_colours,
      name   = ""
    ) +
    ggplot2::scale_linetype_manual(
      values = c("End of observed data" = "dashed"),
      name = NULL
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1, ncol = 1,
                 override.aes = list(linewidth = 1.5, shape = NA)),
      linetype = ggplot2::guide_legend(order = 2,
                   override.aes = list(colour = "#666666", linewidth = 1.0))
    ) +
    ggplot2::labs(
      title    = NULL,
      subtitle = "Standard Care arm",
      # Caution: the plotted values are dimensionless; the unit belongs INSIDE the
      # transform, not as a parenthetical on the axis, which elsewhere in this
      # document (Figure A.2, "Time (years)") means the ticks are in that unit.
      x        = "log(Time in years)",
      y        = "log(-log(S(t)))"
    ) +
    theme_thesis() +
    # Legend inherits the house "bottom" position from theme_thesis(); see the
    # matching note in plot_hazard(). On this tall-aspect (6x8) figure the side
    # legend cost the most panel width of any figure in the family.
    ggplot2::theme(
      legend.box           = "horizontal",
      legend.spacing        = ggplot2::unit(0, "cm"),
      legend.box.spacing    = ggplot2::unit(0, "cm")
    )

  sel_note <- if (!is.null(selected_dist)) {
    paste0("Selected: ", dist_labels[selected_dist], ". ")
  } else ""
  n_dists <- if (has_fits) length(fits) else 0
  cap <- paste0(
    endpoint_label, " log-cumulative hazard plot ",
    "(per NICE DSU TSD 14 Section 4.3.2; supplementary, no DMP/NOMA equivalent). ",
    "Standard Care arm only (parametric distributions fitted to control arm; ",
    "intervention derived via HR). ",
    if (n_dists > 0) paste0(n_dists, " parametric distributions overlaid. ") else "",
    sel_note,
    "Reconstructed KM (Guyot et al., 2012). ",
    nrow(ipd_ctrl), " patients. ",
    "End of observed data: ", round(max_obs, 1), " years. ",
    "Linear trend in log(-log(S(t))) vs log(t) indicates Weibull ",
    "appropriateness; curvature suggests alternative distributions ",
    "may fit better."
  )
  cat("PROGRAMMATIC CAPTION:\n", cap, "\n\n")
  attr(p, "caption") <- cap
  p
}


# --- Intervention Arm Validation Overlay ------------------------------
#' Intervention Arm Validation: HR-Derived vs Directly Fitted
#'
#' Overlays HR-derived intervention curves (S_ctrl^HR), directly fitted
#' intervention parametric curves, and the observed intervention KM.
#' Validates the proportional hazards assumption used in the PSM.
#' GUIDELINE: NICE DSU TSD 14 Section 4.4 (supplementary; no DMP/NOMA equiv).
#'
#' @param fits_ctrl Named list of flexsurvreg objects (control arm).
#' @param fits_int Named list of flexsurvreg objects (intervention validation).
#' @param ipd_int Data frame with 'time' and 'status' columns (intervention).
#' @param endpoint_label Character. "DFS" or "OS".
#' @param selected_dist Character. Distribution name (e.g., "lnorm").
#' @param hr_value Numeric. Hazard ratio (e.g., 0.72 for DFS, 0.63 for OS).
#' @return ggplot object.
plot_int_validation <- function(fits_ctrl, fits_int, ipd_int, endpoint_label,
                                 selected_dist = NULL,
                                 hr_value) {
  if (is.null(selected_dist)) {
    stop("plot_int_validation: selected_dist must be provided.")
  }
  # Intervention KM from reconstructed IPD (Guyot et al., 2012)
  km_fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = ipd_int)
  km_df <- data.frame(
    time = c(0, km_fit$time),
    surv = c(1, km_fit$surv)
  )
  max_obs <- max(ipd_int$time, na.rm = TRUE)
  t_grid <- seq(0.01, max_obs, length.out = 300)

  sel_label <- dist_labels[selected_dist]

  # Dynamic "(selected)" suffix in legend
  local_labels <- dist_labels
  local_labels[selected_dist] <- paste0(local_labels[selected_dist],
                                         " (selected)")

  # HR-derived intervention curves: S_int(t) = S_ctrl(t)^HR for each dist
  hr_list <- lapply(names(fits_ctrl), function(d) {
    s_ctrl <- summary(fits_ctrl[[d]], t = t_grid, type = "survival")[[1]]$est
    s_int_hr <- s_ctrl ^ hr_value
    data.frame(
      time       = t_grid,
      surv       = s_int_hr,
      dist_label = local_labels[d],
      curve_type = "HR-derived",
      highlight  = ifelse(d == selected_dist, "Selected", "Other"),
      stringsAsFactors = FALSE
    )
  })
  hr_df <- do.call(rbind, hr_list)

  # Directly fitted intervention curve (selected distribution only)
  sel_int_pred <- summary(fits_int[[selected_dist]], t = t_grid,
                           type = "survival")[[1]]$est
  direct_df <- data.frame(
    time       = t_grid,
    surv       = sel_int_pred,
    dist_label = paste0(sel_label, " (direct fit)"),
    curve_type = "Direct fit",
    highlight  = "Validation",
    stringsAsFactors = FALSE
  )

  # KM for legend mapping
  km_df$dist_label <- "Reconstructed Kaplan-Meier"

  # Caution: this figure's whole content is how closely the HR-derived curves,
  # the direct fit and the observed KM track each other, and over a full 0-1
  # axis they collapse into one band at the top of the panel. The window below
  # starts at the round 0.05 step at or below the lowest plotted survival less a
  # 0.02 margin, so every plotted point stays inside the panel and only empty
  # space is removed, with ticks derived from the same window so the level can
  # still be read off a truncated axis. The union keeps 1.00 labelled where a
  # 0.1 step from an odd floor would otherwise stop at 0.95.
  y_zoom_lo <- max(0, floor((min(c(hr_df$surv, direct_df$surv, km_fit$surv),
                                 na.rm = TRUE) - 0.02) * 20) / 20)
  y_zoom_breaks <- union(seq(y_zoom_lo, 1,
                             by = if (1 - y_zoom_lo > 0.6) 0.1 else 0.05), 1)

  val_colours <- c(
    "Reconstructed Kaplan-Meier" = "#000000",
    setNames(dist_colours[dist_labels[names(fits_ctrl)]],
             local_labels[names(fits_ctrl)]),
    # Direct fit in dark red-brown for visual distinction from KM (black)
    setNames("#8B0000", paste0(sel_label, " (direct fit)"))
  )

  # GUIDELINE: NICE DSU TSD 14 recommendation 4, numbers at risk on KM
  # diagrams. See the matching note in plot_survival_overlay() for why the base
  # layer must be a ggsurvfit object.
  # Caution: the fixed dashed linetype on the direct-fit curve is SEMANTIC here.
  # Dashed means "directly fitted", against the HR-derived curves, and that
  # contrast is the entire point of an internal-validation figure. It must not
  # be remapped to a linetype scale.
  p <- ggsurvfit::ggsurvfit(km_fit, linewidth = 0.8) +
    ggplot2::aes(colour = "Reconstructed Kaplan-Meier") +
    # HR-derived: non-selected (standardized constants)
    ggplot2::geom_line(
      data = hr_df[hr_df$highlight == "Other", ],
      ggplot2::aes(x = time, y = surv, colour = dist_label),
      inherit.aes = FALSE,
      linewidth = other_linewidth, alpha = other_alpha
    ) +
    # HR-derived: selected (standardized constants)
    ggplot2::geom_line(
      data = hr_df[hr_df$highlight == "Selected", ],
      ggplot2::aes(x = time, y = surv, colour = dist_label),
      inherit.aes = FALSE,
      linewidth = sel_linewidth, alpha = sel_alpha
    ) +
    # Directly fitted validation curve (dashed, fixed linetype)
    ggplot2::geom_line(
      data = direct_df,
      ggplot2::aes(x = time, y = surv, colour = dist_label),
      inherit.aes = FALSE,
      linewidth = 1.0, linetype = "dashed"
    ) +
    # End of observed data, keyed in the legend rather than explained in the
    # subtitle, so the rule is keyed the same way on every figure that draws it.
    ggplot2::geom_vline(ggplot2::aes(xintercept = max_obs,
                                      linetype = "End of observed data"),
                        colour = "#666666", linewidth = 0.7,
                        key_glyph = "path") +
    ggplot2::scale_linetype_manual(
      values = c("End of observed data" = "dashed"),
      name = NULL
    ) +
    ggrepel::geom_text_repel(
      data = series_end_labels(
        rbind(hr_df[, c("time", "surv", "dist_label")],
              direct_df[, c("time", "surv", "dist_label")],
              data.frame(time = max(km_fit$time),
                         surv = min(km_fit$surv),
                         dist_label = "Reconstructed Kaplan-Meier")),
        "time", "dist_label"),
      ggplot2::aes(x = time, y = surv, colour = dist_label,
                   label = dist_label),
      inherit.aes = FALSE, show.legend = FALSE,
      direction = "y", hjust = 0, nudge_x = 0.35,
      segment.size = 0.3, min.segment.length = 0, size = 3.4,
      family = "Charter", box.padding = 0.18, seed = 20260803
    ) +
    ggplot2::scale_colour_manual(
      values = val_colours,
      name   = "",
      guide  = "none"
    ) +
    # No override -- direct fit distinguished by dark red colour;
    # dashed linetype visible on the actual plot line

    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = y_zoom_breaks,
      labels = scales::label_number(accuracy = 0.01)
    ) +
    # coord_cartesian ZOOMS: the scale limits stay 0-1, so no plotted point is
    # dropped and no series is pinned flat along a limit.
    ggplot2::coord_cartesian(ylim = c(y_zoom_lo, 1)) +
    ggplot2::scale_x_continuous(
      limits = c(0, max_obs * 1.30),
      # Caution: see plot_survival_overlay(); drop pretty breaks that collide with
      # the end-of-observation break before adding it.
      breaks = local({
        b <- scales::breaks_pretty()(c(0, max_obs))
        sort(unique(c(b[b < max_obs * 0.95], round(max_obs, 1))))
      })
    ) +
    ggplot2::labs(
      title    = NULL,
      subtitle = paste0("Exercise arm (HR = ", hr_value, ")"),
      x        = "Time (years)",
      y        = "Survival Probability"
    ) +
    theme_thesis() +
    ggsurvfit::add_risktable(
      risktable_stats = "n.risk",
      stats_label = list(n.risk = "Number at risk")
    )

  cap <- paste0(
    endpoint_label, " exercise arm validation ",
    "(per NICE DSU TSD 14 Section 4.4; supplementary, no DMP/NOMA equivalent). ",
    "HR-derived exercise survival (S_ctrl^", hr_value,
    ") for selected ", sel_label,
    " distribution overlaid with directly fitted exercise parametric ",
    "curve and reconstructed exercise KM (Guyot et al., 2012). ",
    "End of observed data: ", round(max_obs, 1), " years."
  )
  cat("PROGRAMMATIC CAPTION:\n", cap, "\n\n")
  attr(p, "caption") <- cap
  p
}


# --- Population EVPI Over Technology Horizon -------------------------
#' Population EVPI Bar Chart Over Technology Horizon
#'
#' Bar chart showing population EVPI at different technology horizons.
#' GUIDELINE: Fenwick et al. 2020 GPR 3-4 (per-patient EVPI * discounted cohort).
#'
#' @param pop_evpi_data Data frame with horizon_years, pop_evpi columns.
#'   Attributes: evpi_per_patient, annual_cohort, discount_rate.
#' @return ggplot object.
plot_pop_evpi_horizon <- function(pop_evpi_data) {
  evpi_pp   <- attr(pop_evpi_data, "evpi_per_patient")
  cohort    <- attr(pop_evpi_data, "annual_cohort")
  disc_rate <- attr(pop_evpi_data, "discount_rate")
  if (is.null(evpi_pp))   stop("plot_pop_evpi_horizon: pop_evpi_data must have attr 'evpi_per_patient'.")
  if (is.null(cohort))    stop("plot_pop_evpi_horizon: pop_evpi_data must have attr 'annual_cohort'.")
  if (is.null(disc_rate)) stop("plot_pop_evpi_horizon: pop_evpi_data must have attr 'discount_rate'.")
  wtp_val <- attr(pop_evpi_data, "wtp")
  if (is.null(wtp_val)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp_val <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("plot_pop_evpi_horizon: wtp must be available as attr or in globalenv.")
    }
  }

  # Format horizon as factor for categorical bars
  pop_evpi_data$horizon_label <- paste0("T = ", pop_evpi_data$horizon_years,
                                         " years")
  pop_evpi_data$horizon_label <- factor(
    pop_evpi_data$horizon_label,
    levels = pop_evpi_data$horizon_label
  )

  # Value labels (millions, 1 decimal)
  # Caution: round() drops a trailing zero, so 1.0 printed as "1M NOK" against
  #   the prose macro's "1.0M". sprintf holds the decimal the prose carries.
  pop_evpi_data$value_label <- paste0(
    sprintf("%.1f", pop_evpi_data$pop_evpi / 1e6), "M NOK"
  )

  p <- ggplot2::ggplot(pop_evpi_data,
                        ggplot2::aes(x = horizon_label, y = pop_evpi)) +
    ggplot2::geom_col(fill = pal$intervention, alpha = 0.85, width = 0.5) +
    # Value annotations above bars (bold annotation tier)
    ggplot2::geom_text(
      ggplot2::aes(label = value_label),
      vjust = -0.5, size = ann_size_bold, family = "Charter",
      fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(sprintf("%.1f", x / 1e6), "M"),
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(
      title    = NULL,
      subtitle = NULL,
      x        = "Technology Horizon",
      y        = "Population EVPI (NOK, discounted)"
    ) +
    theme_thesis()

  cap <- paste0(
    "Population Expected Value of Perfect Information over technology horizon. ",
    "Per-patient EVPI = ", format(evpi_pp, big.mark = ","),
    " NOK multiplied by the discounted modelled annual operated stage-mix cohort size of ",
    format(cohort, big.mark = ","),
    " over ",
    paste(pop_evpi_data$horizon_years, collapse = ", "),
    " years (annuity factor at ", round(disc_rate * 100),
    "% discount rate per Rundskriv R-109). ",
    "Threshold = ", format(wtp_val, big.mark = ","), " NOK/QALY. (Extended healthcare perspective)."
  )
  cat("PROGRAMMATIC CAPTION:\n", cap, "\n\n")
  attr(p, "caption") <- cap
  p
}


# --- Helper: save PDF + deterministic 300 DPI PNG ----------------------------
# GUIDELINE: Dual-format output (PDF + 300 DPI PNG)
# Reproducibility: every raster export uses the locked ragg 1.5.2
# AGG device with explicit geometry, resolution, and background. This avoids
# platform-device antialiasing jitter while leaving the plotted data unchanged.
save_png_deterministic_v1 <- function(plot, path, width, height) {
  if (!requireNamespace("ragg", quietly = TRUE)) {
    stop("Package 'ragg' is required for deterministic raster export.")
  }
  ragg::agg_png(
    filename = path,
    width = width,
    height = height,
    units = "in",
    res = 300,
    background = "white",
    scaling = 1
  )
  png_device <- grDevices::dev.cur()
  on.exit({
    if (png_device %in% grDevices::dev.list()) {
      grDevices::dev.off(png_device)
    }
  }, add = TRUE)
  print(plot)
  grDevices::dev.off(png_device)
  invisible(path)
}
#
# expected_loss.png is re-encoded from its decoded pixel matrix because the
# graphics device can emit different, but pixel-equivalent, IDAT streams across
# otherwise identical runs. The png 0.1-9 encoder produces a canonical stream
# while retaining the exact pixels and explicit 300-DPI physical resolution.
normalize_expected_loss_png_v1 <- function(path) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required for deterministic expected-loss export.")
  }
  if (!file.exists(path)) {
    stop("Expected-loss PNG was not created: ", path)
  }

  pixels <- png::readPNG(path, native = FALSE)
  normalized <- tempfile(
    pattern = "expected-loss-normalized-",
    tmpdir = dirname(path), fileext = ".png")
  on.exit(unlink(normalized), add = TRUE)
  png::writePNG(pixels, target = normalized, dpi = 300)

  round_trip <- png::readPNG(normalized, native = FALSE)
  if (!identical(dim(pixels), dim(round_trip)) ||
      !isTRUE(all(pixels == round_trip))) {
    stop("Deterministic expected-loss PNG encoding changed decoded pixels.")
  }
  if (!file.rename(normalized, path)) {
    stop("Could not install normalized expected-loss PNG: ", path)
  }
  invisible(path)
}

save_figure <- function(plot, name, width, height) {
  pdf_path <- paste0("output/figures/", name, ".pdf")
  png_path <- paste0("output/figures/", name, ".png")
  ggplot2::ggsave(pdf_path, plot, width = width, height = height)
  save_png_deterministic_v1(plot, png_path, width, height)
  if (identical(name, "expected_loss")) {
    normalize_expected_loss_png_v1(png_path)
  }
}

# DECISION: ONE canvas for Figure 3.2 and every
# appendix survival/extrapolation line figure, so the class prints at ONE size.
# Caution: the canvas is NOT the printed size. Every one of these figures is
# included at width=\textwidth (398.0pt = 5.5071 in), so a 10 in canvas is
# downscaled by 0.5507 and theme_thesis(base_size = 18) prints at 9.91 pt.
# Shrinking the canvas toward the printed width does NOT sharpen anything: it
# only inflates the printed type past the 12 pt body, and below 9.79 in it
# CLIPS the legend, whose guide box has a fixed natural width of 9.78139 in
# and does not reflow with the device.
# The height is the page: \textheight 679.0pt less the longest caption in the
# class (83.95625pt, Figure 3.2's; that box already contains
# \abovecaptionskip, so it is subtracted once) leaves 595.04375pt, i.e.
# 10 x 595.04375 / 398.0 = 14.95085 in. 14.70 sits inside that, so the LONGEST
# caption still fits its page and every shorter-captioned member fills at
# least 89 per cent of the block.
FIG_FULLPAGE_W <- 10
FIG_FULLPAGE_H <- 14.70


# --- Save all figures --------------------------------------------------------

#' Save All Model Figures to output/figures/
#'
#' Master function that generates and saves all visualisation outputs.
#' Called from main.R as the final step.
#'
#' Note: all paths are relative to the repository root.
#' main.R ensures the working directory is set correctly and that
#' output directories exist before this function is called.
#'
#' @param psa_results Data frame from run_psa().
#' @param psm_trace Data frame from build_psm_trace().
#' @param owpsa_results Data frame from run_owpsa() (optional).
#' @param exp_loss Output from calculate_expected_loss() (optional).
#' @param base_icer Numeric. Base case expected ICER (optional).
#' @param wtp Numeric. WTP threshold. Resolved from globalenv() if NULL.
#' @param fits_dfs List with $control and $intervention DFS fit lists (optional).
#' @param fits_os List with $control and $intervention OS fit lists (optional).
#' @param ipd_dfs List with $control and $intervention data.frames (optional).
#' @param ipd_os List with $control and $intervention data.frames (optional).
#' @param s_genpop Numeric vector. General population survival (optional).
save_figures <- function(psa_results = NULL, psm_trace = NULL,
                          owpsa_results = NULL, exp_loss = NULL,
                          base_icer = NULL, wtp = NULL,
                          fits_dfs = NULL, fits_os = NULL,
                          ipd_dfs = NULL, ipd_os = NULL,
                          s_genpop = NULL) {
  # wtp parameter with NULL-default lazy resolution
  if (is.null(wtp)) {
    if (exists("wtp_threshold", envir = globalenv())) {
      wtp <- get("wtp_threshold", envir = globalenv())
    } else {
      stop("wtp_threshold not found in global environment. ",
           "Source 00-parameters.R before generating figures.")
    }
  }

  if (!exists("wtp_range", envir = globalenv())) {
    stop("save_figures: wtp_range must exist in globalenv. Source 00-parameters.R.")
  }
  wtp_range <- get("wtp_range", envir = globalenv())

  # CE Plane and CEAC (from PSA base case)
  if (!is.null(psa_results)) {
    ce_plane <- plot_ce_plane(psa_results, wtp = wtp)
    save_figure(ce_plane, "ce_plane", 8, 6)

    ceac_plot <- plot_ceac(psa_results, wtp_range, wtp_ref = wtp)
    save_figure(ceac_plot, "ceac", 8, 6)

    # PSA convergence (function from 06-psa.R)
    if (exists("plot_psa_convergence")) {
      conv_plot <- plot_psa_convergence(psa_results)
      save_figure(conv_plot, "psa_convergence", 8, 6)
    }

    message("Saved: CE plane, CEAC, PSA convergence")
  } else {
    message("TODO: PSA results not available. Run 06-psa.R first.")
  }

  # Tornado diagram (from OWPSA)
  if (!is.null(owpsa_results) && !is.null(base_icer)) {
    tornado <- plot_tornado(owpsa_results, base_icer, wtp)
    if (!is.null(tornado)) {
      save_figure(tornado, "tornado_owpsa", 8, 6)
      message("Saved: Tornado diagram (OWPSA)")
    }
  } else {
    message("TODO: OWPSA results not available. Run run_owpsa() first.")
  }

  # Expected loss curves
  if (!is.null(exp_loss)) {
    el_plot <- plot_expected_loss(exp_loss, wtp)
    if (!is.null(el_plot)) {
      save_figure(el_plot, "expected_loss", 8, 6)
      message("Saved: Expected loss curves")
    }
  } else {
    message("TODO: Expected loss not calculated. Run calculate_expected_loss().")
  }

  # PSM state occupancy
  if (!is.null(psm_trace)) {
    trace_plot <- plot_psm_trace(psm_trace)
    # Reduced width from 10 to 8 so text is proportionally larger
    save_figure(trace_plot, "psm_state_occupancy", 8, 6)
    message("Saved: PSM state occupancy plot")
  } else {
    message("TODO: PSM trace not available. Run 03-build-psm.R first.")
  }

  # Survival figures (overlay, extrapolation, hazard)
  # GUIDELINE: NICE DSU TSD 14 (Latimer 2013), Sections 4.3-4.6
  # (supplementary; DMP/NOMA has no equivalent survival visualisation guidance)
  if (!is.null(fits_dfs) && !is.null(fits_os) &&
      !is.null(ipd_dfs) && !is.null(ipd_os)) {
    # Selected distributions from global env (default to lnorm)
    sel_dfs <- if (exists("best_dist_dfs", envir = globalenv()))
      get("best_dist_dfs", envir = globalenv()) else
      stop("save_figures: best_dist_dfs must exist in globalenv. Source 00-parameters.R.")
    sel_os <- if (exists("best_dist_os", envir = globalenv()))
      get("best_dist_os", envir = globalenv()) else
      stop("save_figures: best_dist_os must exist in globalenv. Source 00-parameters.R.")

    # Cycle length for genpop time axis
    cl <- if (exists("cycle_length", envir = globalenv()))
      get("cycle_length", envir = globalenv()) else
      stop("save_figures: cycle_length must exist in globalenv. Source 00-parameters.R.")

    fig32_panels <- list()
    fig32_legend_source <- NULL
    for (ep in c("dfs", "os")) {
      fits_ep <- if (ep == "dfs") fits_dfs else fits_os
      fits_ctrl <- fits_ep$control
      fits_int <- fits_ep$intervention
      ipd_ctrl  <- if (ep == "dfs") ipd_dfs$control else ipd_os$control
      ipd_int   <- if (ep == "dfs") ipd_dfs$intervention else ipd_os$intervention
      ep_label  <- toupper(ep)
      sel_dist  <- if (ep == "dfs") sel_dfs else sel_os

      # 1. Overlay
      overlay <- plot_survival_overlay(fits_ctrl, ipd_ctrl, ep_label,
                                        selected_dist = sel_dist)
      save_figure(overlay, paste0(ep, "_ctrl_survival_overlay"), FIG_FULLPAGE_W, FIG_FULLPAGE_H)

      # 2. Extrapolation (both endpoints get genpop overlay for plausibility)
      # GUIDELINE: NICE DSU TSD 14 Section 4.5 (supplementary) recommends
      # checking ALL extrapolated curves against background mortality
      extrap <- plot_extrapolation(
        fits_ctrl = fits_ctrl,
        fits_int = fits_int,
        ipd_ctrl = ipd_ctrl,
        ipd_int = ipd_int,
        endpoint_label = ep_label,
        selected_dist = sel_dist,
        s_genpop = s_genpop,
        cycle_length = cl
      )
      save_figure(extrap, paste0(ep, "_ctrl_extrapolation"), FIG_FULLPAGE_W, FIG_FULLPAGE_H)

      # Legend-free DFS panel, used ONLY by Figure 3.2's top panel so the
      # composite carries one legend instead of two. The legend-bearing asset
      # above is unchanged and remains the one Figure A.1 prints.
      extrap_nolegend <- plot_extrapolation(
        fits_ctrl = fits_ctrl,
        fits_int = fits_int,
        ipd_ctrl = ipd_ctrl,
        ipd_int = ipd_int,
        endpoint_label = ep_label,
        selected_dist = sel_dist,
        show_legend = FALSE,
        s_genpop = s_genpop,
        cycle_length = cl
      )
      fig32_panels[[ep]] <- extrap_nolegend
      if (identical(ep, "dfs")) {
        fig32_legend_source <- extrap
      }

      # 3. Hazard (with general population mortality overlay)
      haz <- plot_hazard(fits_ctrl, ipd_ctrl, ep_label,
                          selected_dist = sel_dist,
                          s_genpop = s_genpop, cycle_length = cl)
      save_figure(haz, paste0(ep, "_ctrl_hazard"), FIG_FULLPAGE_W, FIG_FULLPAGE_H)
    }
    message("Saved: 6 survival figures (overlay, extrapolation, hazard x DFS, OS)")

    # Figure 3.2 composite. Both panels are legend-free and are laid out as
    # two rows of one plot_grid, so their plot areas are structurally equal;
    # the shared legend occupies its OWN row and cannot starve a panel. Each
    # endpoint is named by a panel title INSIDE its own panel, so there is no
    # outer label column that can drift out of alignment with the panel it
    # names, and the full row width goes to the data region.
    # Caution: rel_heights is RELATIVE, so a guessed legend share silently draws the
    # guide box back over the panel above it and can hide whole guides. The legend
    # row is MEASURED from the guide-box grob under the target device: with figure
    # height H and legend height L, the two panels share H - L, so the legend's
    # relative height is L / ((H - L) / 2). Never a constant.
    fig32_legend <- cowplot::get_plot_component(fig32_legend_source,
                                                "guide-box-bottom")
    fig32_legend_in <- grid::convertHeight(
      grid::grobHeight(fig32_legend), "in", valueOnly = TRUE)
    if (!is.finite(fig32_legend_in) || fig32_legend_in <= 0 ||
        fig32_legend_in >= FIG_FULLPAGE_H) {
      stop("Figure 3.2: measured legend height outside the figure.", call. = FALSE)
    }
    fig32 <- cowplot::plot_grid(
      fig32_panels[["dfs"]] + ggplot2::ggtitle("Disease-free survival"),
      fig32_panels[["os"]] + ggplot2::ggtitle("Overall survival"),
      fig32_legend,
      ncol = 1,
      rel_heights = c(1, 1,
                      fig32_legend_in / ((FIG_FULLPAGE_H - fig32_legend_in) / 2))
    )
    save_figure(fig32, "methods_extrapolation_composite",
                FIG_FULLPAGE_W, FIG_FULLPAGE_H)
    message("Saved: Figure 3.2 composite")

    # --- Log-cumulative hazard plots ----------------------
    # Pass fits and selected_dist for parametric overlays
    lch_dfs <- plot_log_cumhaz(ipd_dfs$control, "DFS",
                                fits = fits_dfs$control,
                                selected_dist = sel_dfs)
    save_figure(lch_dfs, "log_cumhaz_dfs", FIG_FULLPAGE_W, FIG_FULLPAGE_H)
    lch_os <- plot_log_cumhaz(ipd_os$control, "OS",
                               fits = fits_os$control,
                               selected_dist = sel_os)
    save_figure(lch_os, "log_cumhaz_os", FIG_FULLPAGE_W, FIG_FULLPAGE_H)
    message("Saved: Log-cumulative hazard plots (DFS, OS)")

    # --- Intervention arm validation overlay ---------------
    fits_dfs_int_viz <- if (file.exists("data/processed/survival_fits_dfs_int_validation.rds"))
      readRDS("data/processed/survival_fits_dfs_int_validation.rds") else NULL
    fits_os_int_viz <- if (file.exists("data/processed/survival_fits_os_int_validation.rds"))
      readRDS("data/processed/survival_fits_os_int_validation.rds") else NULL

    if (!is.null(fits_dfs_int_viz)) {
      hr_dfs_val <- if (exists("HR_DFS", envir = globalenv()))
        get("HR_DFS", envir = globalenv()) else
        stop("save_figures: HR_DFS must exist in globalenv. Source 00-parameters.R.")
      val_dfs <- plot_int_validation(fits_dfs$control, fits_dfs_int_viz,
                                      ipd_dfs$intervention, "DFS",
                                      selected_dist = sel_dfs,
                                      hr_value = hr_dfs_val)
      save_figure(val_dfs, "int_validation_dfs", FIG_FULLPAGE_W, FIG_FULLPAGE_H)
      message("Saved: Intervention arm validation (DFS)")
    }
    if (!is.null(fits_os_int_viz)) {
      hr_os_val <- if (exists("HR_OS", envir = globalenv()))
        get("HR_OS", envir = globalenv()) else
        stop("save_figures: HR_OS must exist in globalenv. Source 00-parameters.R.")
      val_os <- plot_int_validation(fits_os$control, fits_os_int_viz,
                                     ipd_os$intervention, "OS",
                                     selected_dist = sel_os,
                                     hr_value = hr_os_val)
      save_figure(val_os, "int_validation_os", FIG_FULLPAGE_W, FIG_FULLPAGE_H)
      message("Saved: Intervention arm validation (OS)")
    }

    # --- Log-scale hazard plots (appendix supplement) -------
    haz_log_dfs <- plot_hazard(fits_dfs$control, ipd_dfs$control, "DFS",
                                selected_dist = sel_dfs, log_scale = TRUE,
                                s_genpop = s_genpop, cycle_length = cl)
    save_figure(haz_log_dfs, "dfs_ctrl_hazard_log", FIG_FULLPAGE_W, FIG_FULLPAGE_H)
    haz_log_os <- plot_hazard(fits_os$control, ipd_os$control, "OS",
                               selected_dist = sel_os, log_scale = TRUE,
                               s_genpop = s_genpop, cycle_length = cl)
    save_figure(haz_log_os, "os_ctrl_hazard_log", FIG_FULLPAGE_W, FIG_FULLPAGE_H)
    message("Saved: Log-scale hazard plots (DFS, OS)")

  } else {
    message("TODO: Survival fits/IPD not available. ",
            "Run 02-fit-survival.R first for survival figures.")
  }

  # --- Model structure diagram (static) --------------------
  model_struct <- plot_model_structure()
  save_figure(model_struct, "model_structure", 8, 6)
  message("Saved: Model structure diagram")

  # --- EVPPI bar chart ------------------------------------
  evppi_data <- if (file.exists("data/processed/evppi_results.rds"))
    readRDS("data/processed/evppi_results.rds") else NULL
  if (!is.null(evppi_data)) {
    evppi_bar <- plot_evppi_bar(evppi_data)
    save_figure(evppi_bar, "evppi_bar", 8, 6)
    message("Saved: EVPPI bar chart")
  }

  # --- Structural SA forest plot --------------------------
  ssa_file <- "output/tables/structural_sa_results.csv"
  if (file.exists(ssa_file)) {
    ssa_data <- read.csv(ssa_file, stringsAsFactors = FALSE)
    ssa_forest <- plot_structural_sa(ssa_data)
    save_figure(ssa_forest, "structural_sa_forest", FIG_FULLPAGE_W, FIG_FULLPAGE_H)
    message("Saved: Structural SA forest plot")
  }

  # --- Population EVPI horizon ----------------------------
  pop_evpi_data <- if (file.exists("data/processed/pop_evpi_horizon.rds"))
    readRDS("data/processed/pop_evpi_horizon.rds") else NULL
  if (!is.null(pop_evpi_data)) {
    pop_evpi_bar <- plot_pop_evpi_horizon(pop_evpi_data)
    save_figure(pop_evpi_bar, "pop_evpi_horizon", 8, 6)
    message("Saved: Population EVPI horizon")
  }
}


# --- Placeholder execution ---------------------------------------------------
if (!isTRUE(getOption("thesis.suppress_figure_autorun", FALSE))) {
  psa_results <- if (file.exists("data/processed/psa_results.rds"))
    readRDS("data/processed/psa_results.rds") else NULL

  psm_trace <- if (file.exists("data/processed/psm_trace.rds"))
    readRDS("data/processed/psm_trace.rds") else NULL

  # Both-arm survival data for overlay, extrapolation, and hazard figures
  fits_dfs_viz_path <- "data/processed/survival_fits_dfs.rds"
  fits_os_viz_path <- "data/processed/survival_fits_os.rds"
  if (!file.exists(fits_dfs_viz_path) || !file.exists(fits_os_viz_path)) {
    stop("Both-arm survival-fit RDS files are required for visualisation.")
  }
  fits_dfs_viz <- readRDS(fits_dfs_viz_path)
  fits_os_viz <- readRDS(fits_os_viz_path)

  expected_arms <- c("control", "intervention")
  expected_dist_keys <- names(dist_labels)
  assert_both_arm_fits <- function(x, endpoint) {
    if (!is.list(x) || !identical(names(x), expected_arms)) {
      stop(endpoint, " fits must contain control and intervention arms.")
    }
    for (arm in expected_arms) {
      if (!is.list(x[[arm]]) ||
          !identical(names(x[[arm]]), expected_dist_keys)) {
        stop(endpoint, " ", arm, " fits have unexpected distribution keys.")
      }
    }
    invisible(TRUE)
  }
  assert_both_arm_fits(fits_dfs_viz, "DFS")
  assert_both_arm_fits(fits_os_viz, "OS")

  ipd_dfs_viz <- if (file.exists("data/processed/ipd_dfs.rds"))
    readRDS("data/processed/ipd_dfs.rds") else NULL
  ipd_os_viz <- if (file.exists("data/processed/ipd_os.rds"))
    readRDS("data/processed/ipd_os.rds") else NULL
  assert_both_arm_ipd <- function(x, endpoint) {
    if (!is.list(x) ||
        !identical(sort(names(x)), sort(expected_arms))) {
      stop(endpoint, " IPD must contain control and intervention arms.")
    }
    for (arm in expected_arms) {
      if (!is.data.frame(x[[arm]]) ||
          !all(c("time", "status") %in% names(x[[arm]]))) {
        stop(endpoint, " ", arm, " IPD must contain time and status.")
      }
    }
    invisible(TRUE)
  }
  assert_both_arm_ipd(ipd_dfs_viz, "DFS")
  assert_both_arm_ipd(ipd_os_viz, "OS")

  # General population survival for both extrapolation figures
  s_genpop_viz <- if (exists("psa_s_genpop", envir = globalenv())) {
    get("psa_s_genpop", envir = globalenv())
  } else if (exists("load_genpop_survival") &&
             exists("life_table_path", envir = globalenv()) &&
             file.exists(life_table_path)) {
    load_genpop_survival(
      life_table_path = life_table_path,
      entry_age = cohort_age,
      n_cycles = n_cycles,
      cycle_length = cycle_length
    )
  } else {
    stop("General-population survival is required for extrapolation figures.")
  }
  if (!is.numeric(s_genpop_viz) ||
      length(s_genpop_viz) != n_cycles + 1L ||
      !identical(s_genpop_viz[[1]], 1) ||
      anyNA(s_genpop_viz) || any(!is.finite(s_genpop_viz))) {
    stop("General-population survival failed visualisation preflight.")
  }

  save_figures(
    psa_results    = psa_results,
    psm_trace      = psm_trace,
    owpsa_results  = if (exists("owpsa_results", envir = globalenv()))
      get("owpsa_results", envir = globalenv()) else NULL,
    exp_loss       = if (exists("exp_loss", envir = globalenv()))
      get("exp_loss", envir = globalenv()) else NULL,
    base_icer      = if (exists("icer_val", envir = globalenv()))
      get("icer_val", envir = globalenv()) else NULL,
    fits_dfs       = fits_dfs_viz,
    fits_os        = fits_os_viz,
    ipd_dfs        = ipd_dfs_viz,
    ipd_os         = ipd_os_viz,
    s_genpop       = s_genpop_viz
  )
  message("Visualisation script complete.")
}
