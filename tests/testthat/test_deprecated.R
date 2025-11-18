test_that("defunct functions", {
  expect_error(add_max_richness_objective(), "add_max_wtd_sum_objective")
  expect_error(plot_phylo_persistence(), "plot_solution_phylogram")
  expect_error(plot_feature_persistence(), "plot_solution_barplot")
})
