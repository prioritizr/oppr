test_that("defunct functions", {
  expect_error(plot_phylo_persistence(), "plot_solution_phylogram")
  expect_error(plot_feature_persistence(), "plot_solution_barplot")
})
