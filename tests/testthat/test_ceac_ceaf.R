# =============================================================================
# test_ceac_ceaf.R
# Unit tests for compute_ceac_ceaf_by_wtp() - consolidated
# CEAC + CEAF by-WTP table helper.
# =============================================================================

test_that("compute_ceac_ceaf_by_wtp computes CEAC probability and CEAF frontier", {
  # inc_qalys = 1, inc_costs = 300000 for all iterations.
  # INMB(wtp) = wtp*1 - 300000. Exercise on frontier iff mean INMB >= 0 iff wtp >= 300000.
  inc_qalys <- rep(1, 4)
  inc_costs <- rep(300000, 4)
  res <- compute_ceac_ceaf_by_wtp(inc_costs, inc_qalys, c(200000, 400000))

  # WTP 200,000: INMB = -100,000 < 0 -> P(CE) = 0, frontier = Standard Care
  expect_equal(res$prob_ce[res$wtp == 200000], 0)
  expect_equal(res$frontier[res$wtp == 200000], "Standard Care")
  # WTP 400,000: INMB = +100,000 >= 0 -> P(CE) = 1, frontier = Exercise
  expect_equal(res$prob_ce[res$wtp == 400000], 1)
  expect_equal(res$frontier[res$wtp == 400000], "Exercise")
})

test_that("compute_ceac_ceaf_by_wtp gives P(CE) = 0.5 at a straddling crossover", {
  # Two iterations straddling zero at WTP 300,000:
  #   iter1: qaly 1, cost 200000 -> INMB = +100000
  #   iter2: qaly 1, cost 400000 -> INMB = -100000
  res <- compute_ceac_ceaf_by_wtp(c(200000, 400000), c(1, 1), 300000)
  expect_equal(res$prob_ce, 0.5)
})

test_that("compute_ceac_ceaf_by_wtp drops NA INB without na.rm masking", {
  # One NA iteration must be dropped, not silently coerced.
  res <- compute_ceac_ceaf_by_wtp(c(200000, NA), c(1, 1), 400000)
  # Only the valid iteration (INMB = +200000) counts -> P(CE) = 1
  expect_equal(res$prob_ce, 1)
})

test_that("compute_ceac_ceaf_by_wtp validates inputs", {
  expect_error(compute_ceac_ceaf_by_wtp(c(1, 2), c(1), 300000))          # length mismatch
  expect_error(compute_ceac_ceaf_by_wtp(numeric(0), numeric(0), 300000)) # empty inputs
  expect_error(compute_ceac_ceaf_by_wtp(c(1), c(1), -5))                 # negative WTP
})
