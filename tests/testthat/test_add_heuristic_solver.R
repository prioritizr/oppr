test_that("add_phylo_div_objective", {
  # create data
  projects <- tibble::tibble(
    name = letters[1:4],
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
  tree <- ape::read.tree(text = "((F1,F2),F3);")
  tree$edge.length <- c(100, 5, 5, 5)
  # make problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_phylo_div_objective(0.16, tree) %>%
    add_binary_decisions() %>%
    add_heuristic_solver()
  # solve problem
  s <- solve(p)
  # run tests
  expect_equal(s$solution, 1L)
  expect_equal(s$status, NA_character_)
  expect_equal(s$cost, 0.1)
  expect_equal(
    s$obj,
    (5 * s$F1) +
      (5 * s$F2) +
      (5 * s$F3) +
      (100 * (1 - ((1 - (s$F1)) * (1 - (s$F2)))))
  )
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, TRUE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$F1, 0.1 * 1)
  expect_equal(s$F2, 0.96 * 0.92)
  expect_equal(s$F3, 0.1 * 1)
})

test_that("add_max_targets_met", {
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
    target = c(0.05, 0.9, 0.05)
  )
  # create problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_targets_met_objective(budget = 0.11) %>%
    add_absolute_targets("target") %>%
    add_binary_decisions() %>%
    add_heuristic_solver()
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_equal(s$solution, 1L)
  expect_equal(s$status, NA_character_)
  expect_equal(s$cost, 0.1)
  expect_equal(s$obj, 2)
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, TRUE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$F1, 1 * 0.1)
  expect_equal(s$F2, 0.96 * 0.92)
  expect_equal(s$F3, 1 * 0.1)
})

test_that("add_min_set_objective", {
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
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets("target") %>%
    add_binary_decisions() %>%
    add_heuristic_solver()
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_equal(s$solution, 1L)
  expect_equal(s$status, NA_character_)
  expect_equal(s$cost, 0.2)
  expect_equal(s$obj, 0.2)
  expect_equal(s$A1, TRUE)
  expect_equal(s$A2, TRUE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$P1, TRUE)
  expect_equal(s$P2, TRUE)
  expect_equal(s$P3, FALSE)
  expect_equal(s$P4, TRUE)
  expect_equal(s$F1, 0.95 * 0.91)
  expect_equal(s$F2, 0.96 * 0.92)
  expect_equal(s$F3, 1 * 0.1)
})

test_that("add_max_wtd_sum_objective", {
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
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
    add_binary_decisions() %>%
    add_heuristic_solver(verbose = FALSE)
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1L)
  expect_equal(s$solution, 1L)
  expect_equal(s$status, NA_character_)
  expect_equal(s$obj, s$F1 + s$F2 + s$F3)
  expect_equal(s$cost, 0.1)
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, TRUE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$F1, 1 * 0.1)
  expect_equal(s$F2, 0.96 * 0.92)
  expect_equal(s$F3, 1 * 0.1)
})

test_that("shared actions", {
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
    A4 = c(FALSE, TRUE, TRUE, FALSE),
    A5 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4", "A5"),
    cost = c(0.10, 0.10, 0.15, 0.05, 0)
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # create problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.26) %>%
    add_binary_decisions() %>%
    add_heuristic_solver(verbose = FALSE, number_solutions = 100)
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 3L)
  expect_equal(s$solution, seq_len(3))
  expect_equal(s$status, rep(NA_character_, 3))
  expect_equal(s$obj, s$F1 + s$F2 + s$F3)
  expect_equal(s$cost, c(0.25, 0.1, 0))
  expect_equal(s$A1, c(TRUE, TRUE, FALSE))
  expect_equal(s$A2, c(TRUE, FALSE, FALSE))
  expect_equal(s$A3, c(FALSE, FALSE, FALSE))
  expect_equal(s$A4, c(TRUE, FALSE, FALSE))
  expect_equal(s$A5, c(TRUE, TRUE, TRUE))
})

test_that("zero budget", {
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
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0) %>%
    add_binary_decisions() %>%
    add_heuristic_solver(number_solutions = 100)
  # solve problem
  s <- solve(p)
  # tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_equal(s$obj, 0.3)
  expect_equal(s$cost, 0)
  expect_equal(s$status, rep(NA_character_, nrow(s)))
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
})

test_that("large problem)", {
  # create data
  set.seed(1000)
  sim_data <- simulate_ptm_data(
    number_projects = 70, number_actions = 30,
    number_features = 40
  )
  projects <- sim_data$projects
  actions <- sim_data$actions
  features <- sim_data$features
  features$weight <- exp(runif(nrow(features), 1, 15))
  # main processing
  for (p in seq(0, 1, length.out = 5)) {
    # generate solution
    b <- sum(actions$cost) * p
    s <-
      problem(
        projects = projects, actions = actions, features = features,
        "name", "success", "name", "cost", "name"
      ) %>%
      add_max_wtd_sum_objective(budget = b) %>%
      add_feature_weights("weight") %>%
      add_binary_decisions() %>%
      add_heuristic_solver(verbose = FALSE, number_solutions = 100) %>%
      solve()
    # run tests
    expect_s3_class(s, "tbl_df")
    expect_gte(nrow(s), 1)
    expect_equal(s$status, rep(NA_character_, nrow(s)))
    expect_true(all(s$cost <= b))
  }
})

test_that("multiple solutions (min set obj)", {
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
    target = c(0.09, 0.7, 0.09)
  )
  # create problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets("target") %>%
    add_binary_decisions() %>%
    add_heuristic_solver(number_solutions = 100)
  # solve problem
  s <- solve(p)
  # tests
  expect_s3_class(s, "tbl_df")
  expect_gt(nrow(s), 1)
  expect_equal(s$solution, seq_len(nrow(s)))
  expect_equal(s$status, rep(NA_character_, nrow(s)))
  expect_equal(s$obj, s$cost)
  expect_equal(
    s$cost,
    (s$A1 * actions$cost[1]) +
      (s$A2 * actions$cost[2]) +
      (s$A3 * actions$cost[3]) +
      (s$A4 * actions$cost[4])
  )
  expect_true(all(s$F1 >= 0.09))
  expect_true(all(s$F2 >= 0.7))
  expect_true(all(s$F3 >= 0.09))
  expect_type(s$A1, "logical")
  expect_type(s$A2, "logical")
  expect_type(s$A3, "logical")
  expect_type(s$A4, "logical")
  expect_true(all((s$A1 + s$A2 + s$A3) >= 1))
})

test_that("heuristic solver (shared actions, multiple solutions)", {
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
    A4 = c(FALSE, TRUE, TRUE, FALSE),
    A5 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4", "A5"),
    cost = c(0.10, 0.10, 0.15, 0.05, 0)
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # create problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.26) %>%
    add_binary_decisions() %>%
    add_heuristic_solver(verbose = FALSE, number_solutions = 100)
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 3L)
  expect_equal(s$solution, seq_len(3))
  expect_equal(s$status, rep(NA_character_, 3))
  expect_equal(s$obj, s$F1 + s$F2 + s$F3)
  expect_equal(s$cost, c(0.25, 0.1, 0))
  expect_equal(s$A1, c(TRUE, TRUE, FALSE))
  expect_equal(s$A2, c(TRUE, FALSE, FALSE))
  expect_equal(s$A3, c(FALSE, FALSE, FALSE))
  expect_equal(s$A4, c(TRUE, FALSE, FALSE))
  expect_equal(s$A5, c(TRUE, TRUE, TRUE))
})

test_that("single feasible solution", {
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
    target = c(0.9, 0.05, 0.05)
  )
  # create problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_targets_met_objective(budget = 0.11) %>%
    add_absolute_targets("target") %>%
    add_binary_decisions() %>%
    add_heuristic_solver(number_solutions = 100)
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_gt(nrow(s), 1)
  expect_equal(s$solution, seq_len(nrow(s)))
  expect_equal(s$status, rep(NA_character_, nrow(s)))
  expect_equal(
    s$obj,
    (s$F1 >= features$target[1]) +
      (s$F2 >= features$target[2]) +
      (s$F3 >= features$target[3])
  )
  expect_equal(
    s$cost,
    (s$A1 * actions$cost[1]) +
      (s$A2 * actions$cost[2]) +
      (s$A3 * actions$cost[3]) +
      (s$A4 * actions$cost[4])
  )
  expect_type(s$F1, "double")
  expect_type(s$F2, "double")
  expect_type(s$F3, "double")
  expect_type(s$A1, "logical")
  expect_type(s$A2, "logical")
  expect_type(s$A3, "logical")
  expect_type(s$A4, "logical")
})

test_that("locked constraints (max benefit obj)", {
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
    target = c(0.9, 0.05, 0.05)
  )
  # create problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_targets_met_objective(budget = 0.11) %>%
    add_absolute_targets("target") %>%
    add_locked_in_action_constraints(1) %>%
    add_locked_out_action_constraints(2) %>%
    add_binary_decisions() %>%
    add_heuristic_solver(number_solutions = 100)
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_equal(s$solution, seq_len(nrow(s)))
  expect_equal(s$status, rep(NA_character_, nrow(s)))
  expect_equal(
    s$obj,
    (s$F1 >= features$target[1]) +
      (s$F2 >= features$target[2]) +
      (s$F3 >= features$target[3])
  )
  expect_equal(
    s$cost,
    (s$A1 * actions$cost[1]) +
      (s$A2 * actions$cost[2]) +
      (s$A3 * actions$cost[3]) +
      (s$A4 * actions$cost[4])
  )
  expect_type(s$F1, "double")
  expect_type(s$F2, "double")
  expect_type(s$F3, "double")
  expect_type(s$A1, "logical")
  expect_true(all(s$A1 > 0.5))
  expect_type(s$A2, "logical")
  expect_true(all(s$A2 < 0.5))
  expect_type(s$A3, "logical")
  expect_type(s$A4, "logical")
})

test_that("locked constraints (min set obj)", {
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
    target = c(0.09, 0.7, 0.09)
  )
  # create problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets("target") %>%
    add_locked_in_action_constraints(1) %>%
    add_locked_out_action_constraints(2) %>%
    add_binary_decisions() %>%
    add_heuristic_solver(number_solutions = 100)
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_equal(s$solution, seq_len(nrow(s)))
  expect_equal(s$status, rep(NA_character_, nrow(s)))
  expect_equal(s$obj, s$cost)
  expect_equal(
    s$cost,
    (s$A1 * actions$cost[1]) +
      (s$A2 * actions$cost[2]) +
      (s$A3 * actions$cost[3]) +
      (s$A4 * actions$cost[4])
  )
  expect_true(all(s$F1 >= 0.09))
  expect_true(all(s$F2 >= 0.7))
  expect_true(all(s$F3 >= 0.09))
  expect_type(s$A1, "logical")
  expect_true(all(s$A1 > 0.5))
  expect_type(s$A2, "logical")
  expect_true(all(s$A2 < 0.5))
  expect_type(s$A3, "logical")
  expect_type(s$A4, "logical")
  expect_true(all((s$A1 + s$A2 + s$A3) >= 1))
})

test_that("no sweep", {
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
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # create problem
  p1 <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
    add_binary_decisions() %>%
    add_heuristic_solver(initial_sweep = FALSE, verbose = FALSE)
  p2 <- problem(
    projects, actions, features, "name", "success", "name", "cost",
    "name", FALSE
  ) %>%
    add_max_wtd_sum_objective(budget = 0.26) %>%
    add_binary_decisions() %>%
    add_heuristic_solver(initial_sweep = FALSE, verbose = FALSE)
  # solve problem
  s1 <- solve(p1)
  s2 <- solve(p2)
  # tests
  ## s1
  expect_s3_class(s1, "tbl_df")
  expect_equal(nrow(s1), 1L)
  expect_equal(s1$solution, 1L)
  expect_equal(s1$status, NA_character_)
  expect_equal(s1$obj, s1$F1 + s1$F2 + s1$F3)
  expect_equal(s1$cost, 0.1)
  expect_equal(s1$A1, FALSE)
  expect_equal(s1$A2, TRUE)
  expect_equal(s1$A3, FALSE)
  expect_equal(s1$A4, TRUE)
  expect_equal(s1$F1, 1 * 0.1)
  expect_equal(s1$F2, 0.96 * 0.92)
  expect_equal(s1$F3, 1 * 0.1)
  ## s2
  expect_s3_class(s2, "tbl_df")
  expect_equal(nrow(s2), 1L)
  expect_equal(s2$solution, 1L)
  expect_equal(s2$status, NA_character_)
  expect_equal(s2$obj, s2$F1 + s2$F2 + s2$F3)
  expect_equal(s2$cost, 0.2)
  expect_equal(s2$A1, TRUE)
  expect_equal(s2$A2, TRUE)
  expect_equal(s2$A3, FALSE)
  expect_equal(s2$A4, TRUE)
  expect_equal(s2$F1, 0.95 * 0.91)
  expect_equal(s2$F2, 0.96 * 0.92)
  expect_equal(s2$F3, 1 * 0.1)
})

test_that("large problem (no sweep)", {
  # make data
  set.seed(1000)
  sim_data <- simulate_ptm_data(
    number_projects = 70, number_actions = 30,
    number_features = 40
  )
  projects <- sim_data$projects
  actions <- sim_data$actions
  features <- sim_data$features
  features$weight <- exp(runif(nrow(features), 1, 15))
  b <- sum(actions$cost) * 0.6
  # generate solutions
  s <- problem(
    projects = projects, actions = actions, features = features,
    "name", "success", "name", "cost", "name"
  ) %>%
    add_max_wtd_sum_objective(budget = b) %>%
    add_feature_weights("weight") %>%
    add_locked_in_action_constraints(c(1, 2, 3)) %>%
    add_locked_out_action_constraints(c(4, 5)) %>%
    add_binary_decisions() %>%
    add_heuristic_solver(
      initial_sweep = FALSE, verbose = FALSE,
      number_solutions = 100
    ) %>%
    solve()
  # tests
  expect_s3_class(s, "tbl_df")
  expect_gt(nrow(s), 1)
  expect_equal(s$status, rep(NA_character_, nrow(s)))
  expect_true(all(s$action_1 == 1))
  expect_true(all(s$action_2 == 1))
  expect_true(all(s$action_3 == 1))
  expect_true(all(s$action_4 == 0))
  expect_true(all(s$action_5 == 0))
  expect_true(all(s$cost <= b))
})
