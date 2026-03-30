test_that("format", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.5) %>%
    add_binary_decisions() %>%
    add_rsymphony_solver()
  # solve problem
  s <- solve(p)
  # run tests
  expect_true(inherits(s, "tbl_df"))
  expect_equal(nrow(s), 1)
  expect_gt(ncol(s), 0)
})

test_that("linear objective", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("Rsymphony")
  # create data
  projects <- tibble::tibble(
    name = c("P1", "P2", "P3", "P4"),
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(0.91, 0.00, 0.80, 0.10),
    F2 = c(0.00, 0.92, 0.80, 0.10),
    F3 = c(0.00, 0.00, 0.00, 0.10),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(FALSE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(0.10, 0.10, 0.15, 0)
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name"
    ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
    add_binary_decisions() %>%
    add_rsymphony_solver()
  # solve problem
  s <- solve(p)
  # solve problem
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
})
