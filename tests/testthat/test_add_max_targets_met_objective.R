context("add_max_targets_met_objective")

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
  # create problem
  p <- problem(
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
  # create problem
  p <- problem(
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

test_that("exact solver (simple problem, single solution)", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # make data
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
  # create problem
  p1 <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_max_targets_met_objective(budget = 0.11) %>%
    add_absolute_targets("target1") %>%
    add_binary_decisions()
  p2 <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_max_targets_met_objective(budget = 0.11) %>%
    add_absolute_targets("target2") %>%
    add_binary_decisions()
  p3 <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_max_targets_met_objective(budget = 0.16) %>%
    add_absolute_targets("target3") %>%
    add_binary_decisions()
  # solve problem
  s1 <- solve(p1)
  s2 <- solve(p2)
  s3 <- solve(p3)
  # tests
  ## s1
  expect_is(s1, "tbl_df")
  expect_equal(nrow(s1), 1)
  expect_equal(s1$solution, 1L)
  expect_true(s1$status %in% c(
    "OPTIMAL", "TM_OPTIMAL_SOLUTION_FOUND",
    "optimal solution found"
  ))
  expect_equal(s1$cost, 0.1)
  expect_equal(s1$obj, 2)
  expect_equal(s1$A1, 1)
  expect_equal(s1$A2, 0)
  expect_equal(s1$A3, 0)
  expect_equal(s1$A4, 1)
  expect_equal(s1$F1, 0.95 * 0.91)
  expect_equal(s1$F2, 1 * 0.1)
  expect_equal(s1$F3, 1 * 0.1)
  ## s2
  expect_is(s2, "tbl_df")
  expect_equal(nrow(s2), 1)
  expect_equal(s2$solution, 1L)
  expect_true(s2$status %in% c(
    "OPTIMAL", "TM_OPTIMAL_SOLUTION_FOUND",
    "optimal solution found"
  ))
  expect_equal(s2$obj, 2)
  expect_equal(s2$cost, 0.1)
  expect_equal(s2$A1, 0)
  expect_equal(s2$A2, 1)
  expect_equal(s2$A3, 0)
  expect_equal(s2$A4, 1)
  expect_equal(s2$F1, 1 * 0.1)
  expect_equal(s2$F2, 0.96 * 0.92)
  expect_equal(s2$F3, 1 * 0.1)
  ## s3
  expect_is(s3, "tbl_df")
  expect_equal(nrow(s3), 1)
  expect_equal(s3$solution, 1L)
  expect_true(s3$status %in% c(
    "OPTIMAL", "TM_OPTIMAL_SOLUTION_FOUND",
    "optimal solution found"
  ))
  expect_equal(s3$obj, 3)
  expect_equal(s3$cost, 0.15)
  expect_equal(s3$A1, 0)
  expect_equal(s3$A2, 0)
  expect_equal(s3$A3, 1)
  expect_equal(s3$A4, 1)
  expect_equal(s3$F1, 0.94 * 0.8)
  expect_equal(s3$F2, 0.94 * 0.8)
  expect_equal(s3$F3, 1 * 0.1)
})

test_that("exact solver (simple problem, multiple solutions)", {
  skip_on_cran()
  skip_if_not_installed("gurobi", "8.0.0")
  # make data
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
    target = c(0.9, 0.05, 0.05)
  )
  # create problem
  p <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_max_targets_met_objective(budget = 0.11) %>%
    add_absolute_targets("target") %>%
    add_binary_decisions() %>%
    add_gurobi_solver(number_solutions = 100)
  # solve problem
  s <- solve(p)
  # tests
  expect_is(s, "tbl_df")
  expect_gt(nrow(s), 1)
  expect_equal(s$solution, seq_len(nrow(s)))
  expect_equal(s$status, ifelse(abs(s$obj - max(s$obj)) < 1e-10,
    "OPTIMAL", "SUBOPTIMAL"
  ))
  expect_equal(s$obj, (s$F1 >= features$target[1]) +
    (s$F2 >= features$target[2]) +
    (s$F3 >= features$target[3]))
  expect_equal(s$cost, (s$A1 * actions$cost[1]) +
    (s$A2 * actions$cost[2]) +
    (s$A3 * actions$cost[3]) +
    (s$A4 * actions$cost[4]))
  expect_is(s$F1, "numeric")
  expect_is(s$F2, "numeric")
  expect_is(s$F3, "numeric")
  expect_is(s$A1, "numeric")
  expect_is(s$A2, "numeric")
  expect_is(s$A3, "numeric")
  expect_is(s$A4, "numeric")
})

test_that("exact solver (locked constraints, multiple solutions)", {
  skip_on_cran()
  skip_if_not_installed("gurobi", "8.0.0")
  # make data
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
    target = c(0.9, 0.05, 0.05)
  )
  # create problem
  p <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_max_targets_met_objective(budget = 100) %>%
    add_absolute_targets("target") %>%
    add_locked_in_constraints(1) %>%
    add_locked_out_constraints(2) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(number_solutions = 100)
  # solve problem
  s <- solve(p)
  # tests
  expect_is(s, "tbl_df")
  expect_gt(nrow(s), 1)
  expect_equal(s$solution, seq_len(nrow(s)))
  expect_equal(s$status, ifelse(abs(s$obj - max(s$obj)) < 1e-10,
    "OPTIMAL", "SUBOPTIMAL"
  ))
  expect_equal(s$obj, (s$F1 >= features$target[1]) +
    (s$F2 >= features$target[2]) +
    (s$F3 >= features$target[3]))
  expect_equal(s$cost, (s$A1 * actions$cost[1]) +
    (s$A2 * actions$cost[2]) +
    (s$A3 * actions$cost[3]) +
    (s$A4 * actions$cost[4]))
  expect_is(s$F1, "numeric")
  expect_is(s$F2, "numeric")
  expect_is(s$F3, "numeric")
  expect_is(s$A1, "numeric")
  expect_true(all(s$A1 > 0.5))
  expect_is(s$A2, "numeric")
  expect_true(all(s$A2 < 0.5))
  expect_is(s$A3, "numeric")
  expect_is(s$A4, "numeric")
})

test_that("invalid arguments", {
  data(sim_projects, sim_actions, sim_features)
  p <- problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name", FALSE
  )
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

test_that("solution_statistics", {
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
    weight = c(100, 4, 9),
    target = c(0.7, 0.7, 0.05)
  )
  # create problem
  p <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_max_targets_met_objective(budget = 0.16) %>%
    add_absolute_targets("target") %>%
    add_feature_weights("weight") %>%
    add_binary_decisions()
  # create solutions
  s <- data.frame(
    A1 = c(1, 0, 0, 1),
    A2 = c(1, 1, 0, 1),
    A3 = c(0, 0, 1, 1),
    A4 = c(1, 1, 1, 1)
  )
  # evaluate solutions
  ss <- solution_statistics(p, s)
  # tests
  expect_equal(ss$cost, c(
    0.1 + 0.1 + 0,
    0.1 + 0,
    0.15 + 0,
    0.1 + 0.1 + 0.15 + 0
  ))
  expect_equal(ss$obj, c(
    100 + 4 + 9,
    4 + 9,
    100 + 4 + 9,
    100 + 4 + 9
  ))
  expect_equal(ss$F1, c(
    0.95 * 0.91,
    0.1 * 1,
    0.94 * 0.8,
    0.95 * 0.91
  ))
  expect_equal(ss$F2, c(
    0.96 * 0.92,
    0.96 * 0.92,
    0.94 * 0.8,
    0.96 * 0.92
  ))
  expect_equal(ss$F3, c(
    0.1 * 1,
    0.1 * 1,
    0.1 * 1,
    0.1 * 1
  ))
})
