# Regression guard for Norwegian stepped-discount boundaries.
# Source truth. Internal endpoints follow the function's one-based
# completed-interval accumulator; printed labels remain 0-39/40-74/75+.

expected_norwegian_discount_weight <- function(t) {
  1 / (
    1.04^pmin(t, 40) *
      1.03^pmin(pmax(t - 40, 0), 35) *
      1.02^pmax(t - 75, 0)
  )
}

test_that("default schedule separates internal endpoints from public labels", {
  brackets <- eval(formals(create_discount_weights)$brackets)

  expect_equal(brackets$year_end, c(40, 75, Inf))
  expect_equal(brackets$rate, c(0.04, 0.03, 0.02))
  expect_identical(
    brackets$display_label,
    c("Years 0-39", "Years 40-74", "Years 75+")
  )

  interval_rate <- function(y) {
    brackets$rate[which(y <= brackets$year_end)[1]]
  }
  expect_equal(
    vapply(c(39, 40, 41, 74, 75, 76), interval_rate, numeric(1)),
    c(0.04, 0.04, 0.03, 0.03, 0.03, 0.02)
  )
})

test_that("stepped weights match an independent closed form around 40 and 75", {
  n_cycles <- 960
  cycle_length <- 1 / 12
  weights <- create_discount_weights(
    n_cycles,
    cycle_length,
    schedule = "stepped"
  )
  cycle <- 0:n_cycles
  elapsed <- ifelse(cycle == 0, 0, (cycle - 0.5) * cycle_length)
  oracle <- expected_norwegian_discount_weight(elapsed)
  boundary_cycles <- c(0, 468, 469, 480, 481, 888, 889, 900, 901, 960)

  expect_equal(
    weights[boundary_cycles + 1],
    oracle[boundary_cycles + 1],
    tolerance = 1e-13
  )
  expect_equal(weights, oracle, tolerance = 1e-13)
})

test_that("40-year stepped base is analytically the flat 4 percent schedule", {
  stepped <- create_discount_weights(480, 1 / 12, schedule = "stepped")
  flat <- create_discount_weights(
    480,
    1 / 12,
    schedule = "flat",
    flat_rate = 0.04
  )

  expect_equal(stepped, flat, tolerance = 1e-13)
})

test_that("custom two-column brackets remain supported", {
  custom <- data.frame(year_end = Inf, rate = 0.05)
  stepped <- create_discount_weights(
    12,
    1 / 12,
    schedule = "stepped",
    brackets = custom
  )
  flat <- create_discount_weights(
    12,
    1 / 12,
    schedule = "flat",
    flat_rate = 0.05
  )

  expect_equal(stepped, flat, tolerance = 1e-13)
})
