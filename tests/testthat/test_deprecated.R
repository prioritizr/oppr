test_that("defunct functions", {
  expect_error(add_max_richness_objective(), "add_max_wtd_sum_objective()")
})
