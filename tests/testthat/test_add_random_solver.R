context("add_random_solver")

test_that("minimum set objective (1 solution)", {
  # create data
  projects <- tibble::tibble(name = c("P1", "P2", "P3", "P4"),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             F1 =       c(0.91, 0.00, 0.80, 0.10),
                             F2 =       c(0.00, 0.92, 0.80, 0.10),
                             F3 =       c(0.00, 0.00, 0.00, 0.10),
                             A1 =       c(TRUE, FALSE, FALSE, FALSE),
                             A2 =       c(FALSE, TRUE, FALSE, FALSE),
                             A3 =       c(FALSE, FALSE, TRUE, FALSE),
                             A4 =       c(FALSE, FALSE, FALSE, TRUE))
  actions <- tibble::tibble(name =      c("A1", "A2", "A3", "A4"),
                            cost =      c(0.10, 0.10, 0.15, 0))
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # create problem
  p <- problem(projects, actions, features, "name", "success", "name", "cost",
               "name") %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(0.7, 0.7, 0.05)) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(1) %>%
       add_locked_out_constraints(2) %>%
       add_random_solver(1, verbose = FALSE)
  # generate solution
  s <- solve(p)
  # tests
  expect_is(s, "tbl_df")
  expect_equal(s$solution, 1L)
  expect_equal(s$status, NA_character_)
  expect_equal(s$obj, 0.25)
  expect_equal(s$cost, 0.25)
  expect_equal(s$A1, TRUE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$F1, 0.95 * 0.91)
  expect_equal(s$F2, 0.94 * 0.8)
  expect_equal(s$F3, 1 * 0.1)
})

test_that("minimum set objective (100 solutions)", {
  # create data
  projects <- tibble::tibble(name = c("P1", "P2", "P3", "P4"),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             F1 =       c(0.91, 0.00, 0.80, 0.10),
                             F2 =       c(0.00, 0.92, 0.80, 0.10),
                             F3 =       c(0.00, 0.00, 0.00, 0.10),
                             A1 =       c(TRUE, FALSE, FALSE, FALSE),
                             A2 =       c(FALSE, TRUE, FALSE, FALSE),
                             A3 =       c(FALSE, FALSE, TRUE, FALSE),
                             A4 =       c(FALSE, FALSE, FALSE, TRUE))
  actions <- tibble::tibble(name =      c("A1", "A2", "A3", "A4"),
                            cost =      c(0.10, 0.10, 0.15, 0))
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # create problem
  p <- problem(projects, actions, features, "name", "success", "name", "cost",
               "name") %>%
       add_min_set_objective() %>%
       add_absolute_targets(c(0.7, 0.7, 0.05)) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(1) %>%
       add_random_solver(100, verbose = FALSE)
  # generate solution
  s <- solve(p)
  # tests
  expect_is(s, "tbl_df")
  expect_equal(s$solution, seq_len(100))
  expect_equal(s$status, rep(NA_character_, 100))
  expect_true(all(s$obj %in% c(0.2, 0.25)))
  expect_equal(s$obj, s$cost)
  expect_equal(s$cost, (0.1 * s$A1) +
                       (0.1 * s$A2) +
                       (0.15 * s$A3) +
                       (0.0 * s$A4))
  expect_equal(s$A1, rep(TRUE, 100))
  expect_equal(s$A2 + s$A3, rep(1, 100))
  expect_equal(s$A4, rep(TRUE, 100))
  expect_true(all(s$F1 > 0.7))
  expect_true(all(s$F2 > 0.7))
  expect_true(all(s$F3 > 0.05))
})

test_that("maximum benefit objective (1 solution)", {
  # create data
  projects <- tibble::tibble(name = c("P1", "P2", "P3", "P4"),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             F1 =       c(0.91, 0.00, 0.80, 0.10),
                             F2 =       c(0.00, 0.92, 0.80, 0.10),
                             F3 =       c(0.00, 0.00, 0.00, 0.10),
                             A1 =       c(TRUE, FALSE, FALSE, FALSE),
                             A2 =       c(FALSE, TRUE, FALSE, FALSE),
                             A3 =       c(FALSE, FALSE, TRUE, FALSE),
                             A4 =       c(FALSE, FALSE, FALSE, TRUE))
  actions <- tibble::tibble(name =      c("A1", "A2", "A3", "A4"),
                            cost =      c(0.10, 0.10, 0.15, 0))
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # create problem
  p <- problem(projects, actions, features, "name", "success", "name", "cost",
               "name") %>%
       add_max_sum_persistence_objective(budget = 0.15) %>%
       add_binary_decisions() %>%
       add_locked_in_constraints(1) %>%
       add_locked_out_constraints(2) %>%
       add_random_solver(1, verbose = FALSE)
  # generate solution
  s <- solve(p)
  # tests
  expect_is(s, "tbl_df")
  expect_equal(s$solution, 1L)
  expect_equal(s$status, NA_character_)
  expect_equal(s$obj, s$F1 + s$F2 + s$F3)
  expect_equal(s$cost, (0.1 * s$A1) +
                       (0.1 * s$A2) +
                       (0.15 * s$A3) +
                       (0.0 * s$A4))
  expect_equal(s$A1, TRUE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$F1, 0.95 * 0.91)
  expect_equal(s$F2, 1 * 0.1)
  expect_equal(s$F3, 1 * 0.1)
})

test_that("maximum benefit objective (100 solutions)", {
  # create data
  projects <- tibble::tibble(name = c("P1", "P2", "P3", "P4"),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             F1 =       c(0.91, 0.00, 0.80, 0.10),
                             F2 =       c(0.00, 0.92, 0.80, 0.10),
                             F3 =       c(0.00, 0.00, 0.00, 0.10),
                             A1 =       c(TRUE, FALSE, FALSE, FALSE),
                             A2 =       c(FALSE, TRUE, FALSE, FALSE),
                             A3 =       c(FALSE, FALSE, TRUE, FALSE),
                             A4 =       c(FALSE, FALSE, FALSE, TRUE))
  actions <- tibble::tibble(name =      c("A1", "A2", "A3", "A4"),
                            cost =      c(0.10, 0.10, 0.15, 0))
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # create problem
  p <- problem(projects, actions, features, "name", "success", "name", "cost",
               "name") %>%
       add_max_sum_persistence_objective(budget = 0.15) %>%
       add_binary_decisions() %>%
       add_locked_out_constraints(2) %>%
       add_random_solver(100, verbose = FALSE)
  # generate solution
  s <- solve(p)
  # tests
  expect_is(s, "tbl_df")
  expect_equal(s$solution, seq_len(100))
  expect_equal(s$status, rep(NA_character_, 100))
  expect_equal(s$obj, s$F1 + s$F2 + s$F3)
  expect_true(all(s$cost <= 0.15))
  expect_equal(s$A1 + s$A3, rep(1, 100))
  expect_equal(s$A2, rep(FALSE, 100))
  expect_equal(s$A4, rep(TRUE, 100))
})

test_that("invalid arguments", {
  data(sim_projects, sim_actions, sim_features)
  p <- problem(sim_projects, sim_actions, sim_features,
               "name", "success", "name", "cost", "name")
  # number_solutions
  expect_error({
    add_random_solver(p, number_solutions = NA_integer_)
  })
  expect_error({
    add_random_solver(p, number_solutions = c(1, 1))
  })
  expect_error({
    add_random_solver(p, number_solutions = "a")
  })
  expect_error({
    add_random_solver(p, number_solutions = 0)
  })
  expect_error({
    add_random_solver(p, number_solutions = TRUE)
  })
  # verbose
  expect_error({
    add_random_solver(p, verbose = NA)
  })
  expect_error({
    add_random_solver(p, verbose = 1)
  })
  expect_error({
    add_random_solver(p, verbose = "a")
  })
})
