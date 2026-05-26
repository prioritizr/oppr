test_that("compile (no weights)", {
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
  features <- tibble::tibble(
    name = c("F1", "F2", "F3"),
    target = c(0.1, 0.2, 0.3)
  )
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_targets_met_objective(budget = 0.16) %>%
    add_absolute_targets("target") %>%
    add_binary_decisions()
  # create optimization problem
  o1 <- compile(p)
  o2 <- max_targets_mip_formulation(projects, actions, features, 0.16)
  # run tests
  expect_equal(o1$obj(), o2$obj)
  expect_equal(o1$vtype(), o2$vtype)
  expect_equal(o1$lb(), o2$lb)
  expect_equal(o1$ub(), o2$ub)
  expect_equal(o1$sense(), o2$sense)
  expect_equal(o1$rhs(), o2$rhs)
  expect_equal(o1$pwlobj(), list())
  expect_true(all(o1$A() == o2$A))
})

test_that("compile (weights)", {
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
  features <- tibble::tibble(
    name = c("F1", "F2", "F3"),
    target = c(0.1, 0.2, 0.3),
    weight = seq_len(3) * 4
  )
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_targets_met_objective(budget = 0.16) %>%
    add_absolute_targets("target") %>%
    add_feature_weights(weight = features$weight) %>%
    add_binary_decisions()
  # create optimization problem
  o1 <- compile(p)
  o2 <- max_targets_mip_formulation(projects, actions, features, 0.16)
  # run tests
  expect_equal(o1$obj(), o2$obj)
  expect_equal(o1$vtype(), o2$vtype)
  expect_equal(o1$lb(), o2$lb)
  expect_equal(o1$ub(), o2$ub)
  expect_equal(o1$sense(), o2$sense)
  expect_equal(o1$rhs(), o2$rhs)
  expect_equal(o1$pwlobj(), list())
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
  features <- tibble::tibble(
    name = c("F1", "F2", "F3"),
    target1 = c(0.11, 0.9, 0.05),
    target2 = c(0.9, 0.15, 0.05),
    target3 = c(0.7, 0.7, 0.05)
  )
  # build problems
  p1 <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_targets_met_objective(budget = 0.11) %>%
    add_absolute_targets("target1") %>%
    add_binary_decisions()
  p2 <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_targets_met_objective(budget = 0.11) %>%
    add_absolute_targets("target2") %>%
    add_binary_decisions()
  p3 <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_targets_met_objective(budget = 0.16) %>%
    add_absolute_targets("target3") %>%
    add_binary_decisions()
  # solve problems
  s1 <- solve(p1)
  s2 <- solve(p2)
  s3 <- solve(p3)
  # run tests
  ## s1
  expect_s3_class(s1, "tbl_df")
  expect_equal(nrow(s1), 1)
  expect_equal(s1$solution, 1L)
  expect_true(is_optimal_solver_status(s1$status))
  expect_equal(s1$cost, 0.1)
  expect_equal(s1$obj, 2)
  expect_equal(s1$A1, TRUE)
  expect_equal(s1$A2, FALSE)
  expect_equal(s1$A3, FALSE)
  expect_equal(s1$A4, TRUE)
  expect_equal(s1$F1, 0.95 * 0.91)
  expect_equal(s1$F2, 1 * 0.1)
  expect_equal(s1$F3, 1 * 0.1)
  ## s2
  expect_s3_class(s2, "tbl_df")
  expect_equal(nrow(s2), 1)
  expect_equal(s2$solution, 1L)
  expect_true(is_optimal_solver_status(s2$status))
  expect_equal(s2$obj, 2)
  expect_equal(s2$cost, 0.1)
  expect_equal(s2$A1, FALSE)
  expect_equal(s2$A2, TRUE)
  expect_equal(s2$A3, FALSE)
  expect_equal(s2$A4, TRUE)
  expect_equal(s2$F1, 1 * 0.1)
  expect_equal(s2$F2, 0.96 * 0.92)
  expect_equal(s2$F3, 1 * 0.1)
  ## s3
  expect_s3_class(s3, "tbl_df")
  expect_equal(nrow(s3), 1)
  expect_equal(s3$solution, 1L)
  expect_true(is_optimal_solver_status(s3$status))
  expect_equal(s3$obj, 3)
  expect_equal(s3$cost, 0.15)
  expect_equal(s3$A1, FALSE)
  expect_equal(s3$A2, FALSE)
  expect_equal(s3$A3, TRUE)
  expect_equal(s3$A4, TRUE)
  expect_equal(s3$F1, 0.94 * 0.8)
  expect_equal(s3$F2, 0.94 * 0.8)
  expect_equal(s3$F3, 1 * 0.1)
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
    add_max_targets_met_objective(p, NA_real_)
  })
  expect_error({
    add_max_targets_met_objective(p, c(1, 1))
  })
  expect_error({
    add_max_targets_met_objective(p, "a")
  })
  expect_error({
    add_max_targets_met_objective(p, TRUE)
  })
})
