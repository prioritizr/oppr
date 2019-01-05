context("solvers")

test_that("add_default_solver", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # make data
  data(sim_projects, sim_actions, sim_features)
  p <- problem(sim_projects, sim_actions, sim_features,
               "name", "success", "name", "cost", "name") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.5) %>%
       add_binary_decisions() %>%
       add_default_solver(time_limit = 5)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "tbl_df"))
})

test_that("add_rsymphony_solver", {
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # make data
  data(sim_projects, sim_actions, sim_features)
  p <- problem(sim_projects, sim_actions, sim_features,
               "name", "success", "name", "cost", "name") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.5) %>%
       add_binary_decisions() %>%
       add_rsymphony_solver(time_limit = 5)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "tbl_df"))
  expect_equal(nrow(s), 1)
  expect_gt(ncol(s), 0)
})

test_that("add_lpsymphony_solver", {
  skip_on_cran()
  skip_if_not_installed("lpsymphony")
  skip_on_os("linux") # lpsymphony package crashes unpredictably on Ubuntu 16.04
  # make data
  data(sim_projects, sim_actions, sim_features)
  p <- problem(sim_projects, sim_actions, sim_features,
               "name", "success", "name", "cost", "name") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.5) %>%
       add_binary_decisions() %>%
       add_lpsymphony_solver(time_limit = 5)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "tbl_df"))
  expect_equal(nrow(s), 1)
  expect_gt(ncol(s), 0)
})

test_that("add_gurobi_solver", {
  skip_on_cran()
  skip_if_not_installed("gurobi")
  # make data
  data(sim_projects, sim_actions, sim_features)
  p <- problem(sim_projects, sim_actions, sim_features,
               "name", "success", "name", "cost", "name") %>%
       add_min_set_objective() %>%
       add_relative_targets(0.5) %>%
       add_binary_decisions() %>%
       add_gurobi_solver(time_limit = 5)
  s <- solve(p)
  # check that solution has correct properties
  expect_true(inherits(s, "tbl_df"))
  expect_equal(nrow(s), 1)
  expect_gt(ncol(s), 0)
})
