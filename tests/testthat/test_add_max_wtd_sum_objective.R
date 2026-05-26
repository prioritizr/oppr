test_that("compile (no weights)", {
  # create data
  projects <- tibble::tibble(
    name = c("P1", "P2", "P3", "P4"),
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(91, 0.00, 80, 10),
    F2 = c(0.00, 92, 80, 10),
    F3 = c(0.00, 0.00, 0.00, 10),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(FALSE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(0.10, 0.10, 0.15, 0),
    locked_in = FALSE,
    locked_out = FALSE
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # create problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
    add_binary_decisions()
  # create optimization problem
  o1 <- compile(p)
  o2 <- max_wtd_sum_mip_formulation(projects, actions, features, 0.16)
  # run tests
  expect_equal(o1$obj(), o2$obj)
  expect_equal(o1$vtype(), o2$vtype)
  expect_equal(o1$lb(), o2$lb)
  expect_equal(o1$ub(), o2$ub)
  expect_equal(o1$sense(), o2$sense)
  expect_equal(o1$rhs(), o2$rhs)
  expect_true(all(o1$A() == o2$A))
})

test_that("compile (weights)", {
  # create data
  projects <- tibble::tibble(
    name = c("P1", "P2", "P3", "P4"),
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(91, 0.00, 80, 10),
    F2 = c(0.00, 92, 80, 10),
    F3 = c(0.00, 0.00, 0.00, 10),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(FALSE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(0.10, 0.10, 0.15, 0),
    locked_in = FALSE,
    locked_out = FALSE
  )
  features <- tibble::tibble(
    name = c("F1", "F2", "F3"),
    weight = seq_len(3) * 90
  )
  # create problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
    add_feature_weights(weight = features$weight) %>%
    add_binary_decisions()
  # create optimization problem
  o1 <- compile(p)
  o2 <- max_wtd_sum_mip_formulation(projects, actions, features, 0.16)
  # run tests
  expect_equal(o1$obj(), o2$obj)
  expect_equal(o1$vtype(), o2$vtype)
  expect_equal(o1$lb(), o2$lb)
  expect_equal(o1$ub(), o2$ub)
  expect_equal(o1$sense(), o2$sense)
  expect_equal(o1$rhs(), o2$rhs)
  expect_true(all(o1$A() == o2$A))
})

test_that("solve (single solution)", {
  # define skips
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  projects <- tibble::tibble(
    name = c("P1", "P2", "P3", "P4"),
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(91, 0.00, 80, 10),
    F2 = c(0.00, 92, 80, 10),
    F3 = c(0.00, 0.00, 0.00, 10),
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
  # build problems
  p1 <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
    add_binary_decisions()
  p2 <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.26) %>%
    add_binary_decisions()
  # solve problem
  s1 <- solve(p1)
  s2 <- solve(p2)
  # run tests
  ## s1
  expect_s3_class(s1, "tbl_df")
  expect_equal(nrow(s1), 1L)
  expect_equal(s1$solution, 1L)
  expect_true(is_optimal_solver_status(s1$status))
  expect_equal(s1$obj, s1$F1 + s1$F2 + s1$F3)
  expect_equal(s1$cost, 0.15)
  expect_equal(s1$A1, FALSE)
  expect_equal(s1$A2, FALSE)
  expect_equal(s1$A3, TRUE)
  expect_equal(s1$A4, TRUE)
  expect_equal(s1$F1, 94 * 0.8)
  expect_equal(s1$F2, 94 * 0.8)
  expect_equal(s1$F3, 10 * 1)
  ## s2
  expect_s3_class(s2, "tbl_df")
  expect_equal(nrow(s2), 1L)
  expect_equal(s2$solution, 1L)
  expect_true(is_optimal_solver_status(s2$status))
  expect_equal(s2$obj, s2$F1 + s2$F2 + s2$F3)
  expect_equal(s2$cost, 0.2)
  expect_equal(s2$A1, TRUE)
  expect_equal(s2$A2, TRUE)
  expect_equal(s2$A3, FALSE)
  expect_equal(s2$A4, TRUE)
  expect_equal(s2$F1, 95 * 0.91)
  expect_equal(s2$F2, 96 * 0.92)
  expect_equal(s2$F3, 10 * 1)
})

test_that("solve (zeros)", {
  # define skips
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  projects <- tibble::tibble(
    name = c("P1", "P2", "P3", "P4"),
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(91, 0.00, 80, 0),
    F2 = c(0.00, 92, 80, 0),
    F3 = c(0.00, 0.00, 32.00, 0),
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
  # build problems
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0) %>%
    add_binary_decisions()
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1L)
  expect_equal(s$solution, 1L)
  expect_true(is_optimal_solver_status(s$status))
  expect_equal(s$obj, 0)
  expect_equal(s$cost, 0)
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$F1, 0)
  expect_equal(s$F2, 0)
  expect_equal(s$F3, 0)
})

test_that("invalid arguments", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    )
  # run tests
  expect_error({
    add_max_wtd_sum_objective(p, NA_real_)
  })
  expect_error({
    add_max_wtd_sum_objective(p, c(1, 1))
  })
  expect_error({
    add_max_wtd_sum_objective(p, "a")
  })
  expect_error({
    add_max_wtd_sum_objective(p, TRUE)
  })
})
