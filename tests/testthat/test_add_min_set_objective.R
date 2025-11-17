context("add_min_set_objective")

test_that("compile", {
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
    target = c(0.7, 0.7, 0.09)
  )
  # create problem
  p <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets("target") %>%
    add_binary_decisions()
  # create optimization problem
  o1 <- compile(p)
  o2 <- min_set_mip_formulation(projects, actions, features)
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
    target = c(0.7, 0.7, 0.09)
  )
  # create problem
  p <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets("target") %>%
    add_binary_decisions()
  # solve problem
  s <- solve(p)
  # tests
  expect_is(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_equal(s$solution, 1L)
  expect_true(s$status %in% c(
    "OPTIMAL", "TM_OPTIMAL_SOLUTION_FOUND",
    "optimal solution found"
  ))
  expect_equal(s$cost, 0.15)
  expect_equal(s$obj, 0.15)
  expect_equal(s$A1, 0)
  expect_equal(s$A2, 0)
  expect_equal(s$A3, 1)
  expect_equal(s$A4, 1)
  expect_equal(s$P1, 0)
  expect_equal(s$P2, 0)
  expect_equal(s$P3, 1)
  expect_equal(s$P4, 1)
  expect_equal(s$F1, 0.94 * 0.8)
  expect_equal(s$F2, 0.94 * 0.8)
  expect_equal(s$F3, 1 * 0.1)
})

test_that("exact solver (simple problem, multiple solutions)", {
  skip_on_cran()
  skip_if_not_installed("gurobi", "8.0.0")
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
    target = c(0.7, 0.7, 0.09)
  )
  # create problem
  p <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets("target") %>%
    add_binary_decisions() %>%
    add_gurobi_solver(number_solutions = 100)
  # solve problem
  s <- solve(p)
  # solve problem
  expect_is(s, "tbl_df")
  expect_gt(nrow(s), 1)
  expect_equal(s$solution, seq_len(nrow(s)))
  expect_equal(s$status, ifelse(abs(s$cost - min(s$cost)) < 1e-10,
    "OPTIMAL", "SUBOPTIMAL"
  ))
  expect_equal(s$cost, (s$A1 * actions$cost[1]) +
    (s$A2 * actions$cost[2]) +
    (s$A3 * actions$cost[3]) +
    (s$A4 * actions$cost[4]))
  expect_true(all(s$F1 >= 0.7))
  expect_true(all(s$F2 >= 0.7))
  expect_true(all(s$F3 >= 0.09))
  expect_is(s$A1, "numeric")
  expect_is(s$A2, "numeric")
  expect_is(s$A3, "numeric")
  expect_is(s$A4, "numeric")
  expect_is(s$P1, "numeric")
  expect_is(s$P2, "numeric")
  expect_is(s$P3, "numeric")
  expect_is(s$P4, "numeric")
  expect_true(all((s$A1 + s$A2 + s$A3) >= 1))
})

test_that("exact solver (locked constraints, multiple solutions)", {
  skip_on_cran()
  skip_if_not_installed("gurobi", "8.0.0")
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
    target = c(0.7, 0.09, 0.09)
  )
  # create problem
  p <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets("target") %>%
    add_locked_in_constraints(1) %>%
    add_locked_out_constraints(2) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(number_solutions = 100)
  # solve problem
  s <- solve(p)
  # solve problem
  expect_is(s, "tbl_df")
  expect_gt(nrow(s), 1)
  expect_equal(s$solution, seq_len(nrow(s)))
  expect_equal(s$status, ifelse(abs(s$cost - min(s$cost)) < 1e-10,
    "OPTIMAL", "SUBOPTIMAL"
  ))
  expect_equal(s$cost, (s$A1 * actions$cost[1]) +
    (s$A2 * actions$cost[2]) +
    (s$A3 * actions$cost[3]) +
    (s$A4 * actions$cost[4]))
  expect_true(all(s$F1 >= 0.7))
  expect_true(all(s$F2 >= 0.09))
  expect_true(all(s$F3 >= 0.09))
  expect_is(s$A1, "numeric")
  expect_true(all(s$A1 > 0.5))
  expect_is(s$P1, "numeric")
  expect_true(all(s$P1 > 0.5))
  expect_is(s$A2, "numeric")
  expect_true(all(s$P2 < 0.5))
  expect_is(s$P2, "numeric")
  expect_true(all(s$A2 < 0.5))
  expect_is(s$A3, "numeric")
  expect_is(s$A4, "numeric")
  expect_is(s$P3, "numeric")
  expect_is(s$P4, "numeric")
  expect_true(all((s$A1 + s$A2 + s$A3) >= 1))
  expect_true(all((s$P1 + s$P2 + s$P3) >= 1))
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
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # create problem
  p <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_min_set_objective() %>%
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
  expect_equal(ss$obj, ss$cost)
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
