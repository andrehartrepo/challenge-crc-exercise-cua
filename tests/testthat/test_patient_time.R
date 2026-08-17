# =============================================================================
# test_patient_time.R
# Unit tests for weighted_patient_time_rate() - sensitivity scenario helper
# (Rundskriv R-109 section 6.1.4 labour/leisure blend; base case leisure = 328 NOK/hr)
# =============================================================================

test_that("weighted_patient_time_rate computes the labour/leisure blend", {
  # working_share 0.5, gross 500, leisure 328 -> 0.5*500 + 0.5*328 = 414
  expect_equal(weighted_patient_time_rate(0.5, 500, 328), 414)
})

test_that("weighted_patient_time_rate collapses to the bounds correctly", {
  expect_equal(weighted_patient_time_rate(0, 500, 328), 328)  # no working share -> leisure
  expect_equal(weighted_patient_time_rate(1, 500, 328), 500)  # full working share -> gross
})

test_that("weighted_patient_time_rate stops on NA (BLOCKED pending external data)", {
  expect_error(weighted_patient_time_rate(NA, 500, 328), "must not be NA")
  expect_error(weighted_patient_time_rate(0.5, NA, 328), "must not be NA")
  expect_error(weighted_patient_time_rate(0.5, 500, NA), "must not be NA")
})

test_that("weighted_patient_time_rate validates input ranges", {
  expect_error(weighted_patient_time_rate(1.5, 500, 328))   # share > 1
  expect_error(weighted_patient_time_rate(-0.1, 500, 328))  # share < 0
  expect_error(weighted_patient_time_rate(0.5, 0, 328))     # gross not > 0
  expect_error(weighted_patient_time_rate(0.5, 500, 0))     # leisure not > 0
})

test_that("patient-time inputs hold the SSB-derived values", {
  # Guardrail: pins the patient-time inputs sourced from
  # SSB Table 06161: register-based
  # employment at exact age 61 (49,522/66,898 = 0.740 share) and the
  # SSB-derived gross hourly rate. Deferred-NA premise superseded; a change
  # here without a new spec is drift.
  expect_identical(patient_time_working_share, 0.740)
  expect_equal(c_patient_time_rate_gross, 620.4822067386087)
  # Base case leisure rate stays intact at 328 NOK/hr (DMP section 12.7.1).
  expect_equal(c_patient_time_rate, 328)
})
