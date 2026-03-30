test_that("minimum set objective (1 solution)", {
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
    add_min_set_objective() %>%
    add_absolute_targets(c(0.7, 0.7, 0.05)) %>%
    add_binary_decisions() %>%
    add_locked_in_action_constraints(1) %>%
    add_locked_out_action_constraints(2) %>%
    add_random_solver(1, verbose = FALSE)
  # generate solution
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
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

test_that("minimum set objective (100 solutions, locked in)", {
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
    add_min_set_objective() %>%
    add_absolute_targets(c(0.7, 0.7, 0.05)) %>%
    add_binary_decisions() %>%
    add_locked_in_action_constraints(1) %>%
    add_random_solver(100, verbose = FALSE)
  # generate solution
  s <- solve(p)
  # tests
  expect_s3_class(s, "tbl_df")
  expect_equal(s$solution, seq_len(100))
  expect_equal(s$status, rep(NA_character_, 100))
  expect_true(all(s$obj %in% c(0.2, 0.25)))
  expect_equal(s$obj, s$cost)
  expect_equal(
    s$cost,
    (0.1 * s$A1) +
      (0.1 * s$A2) +
      (0.15 * s$A3) +
      (0.0 * s$A4)
  )
  expect_equal(s$A1, rep(TRUE, 100))
  expect_equal(s$A2 + s$A3, rep(1, 100))
  expect_equal(s$A4, rep(TRUE, 100))
  expect_true(all(s$F1 > 0.7))
  expect_true(all(s$F2 > 0.7))
  expect_true(all(s$F3 > 0.05))
})

test_that("minimum set objective (100 solutions, locked out)", {
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
      "name", FALSE
    ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(c(0.7, 0.7, 0.05)) %>%
    add_binary_decisions() %>%
    add_locked_out_action_constraints(1) %>%
    add_random_solver(100, verbose = FALSE)
  # generate solution
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(s$solution, seq_len(100))
  expect_equal(s$status, rep(NA_character_, 100))
  expect_true(all(s$obj %in% c(0.15, 0.25)))
  expect_equal(s$obj, s$cost)
  expect_equal(
    s$cost,
      (0.1 * s$A1) +
      (0.1 * s$A2) +
      (0.15 * s$A3) +
      (0.0 * s$A4)
  )
  expect_equal(s$A1, rep(FALSE, 100))
  expect_true(all(s$F1 > 0.7))
  expect_true(all(s$F2 > 0.7))
  expect_true(all(s$F3 > 0.05))
})

test_that("maximum benefit objective (1 solution)", {
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
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.15) %>%
    add_binary_decisions() %>%
    add_locked_in_action_constraints(1) %>%
    add_locked_out_action_constraints(2) %>%
    add_random_solver(1, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(s$solution, 1L)
  expect_equal(s$status, NA_character_)
  expect_equal(s$obj, s$F1 + s$F2 + s$F3)
  expect_equal(
    s$cost,
    (0.1 * s$A1) +
      (0.1 * s$A2) +
      (0.15 * s$A3) +
      (0.0 * s$A4)
  )
  expect_equal(s$A1, TRUE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$F1, 0.95 * 0.91)
  expect_equal(s$F2, 1 * 0.1)
  expect_equal(s$F3, 1 * 0.1)
})

test_that("maximum benefit objective (100 solutions, locked in)", {
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
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
    add_binary_decisions() %>%
    add_locked_in_action_constraints(2) %>%
    add_random_solver(100, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(s$solution, seq_len(100))
  expect_equal(s$status, rep(NA_character_, 100))
  expect_equal(s$obj, s$F1 + s$F2 + s$F3)
  expect_true(all(s$cost == 0.1))
  expect_equal(s$A1 + s$A3, rep(0, 100))
  expect_equal(s$A2, rep(TRUE, 100))
  expect_equal(s$A4, rep(TRUE, 100))
})

test_that("maximum benefit objective (100 solutions, locked out)", {
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
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.15) %>%
    add_binary_decisions() %>%
    add_locked_out_action_constraints(2) %>%
    add_random_solver(100, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_s3_class(s, "tbl_df")
  expect_equal(s$solution, seq_len(100))
  expect_equal(s$status, rep(NA_character_, 100))
  expect_equal(s$obj, s$F1 + s$F2 + s$F3)
  expect_true(all(s$cost <= 0.15))
  expect_equal(s$A1 + s$A3, rep(1, 100))
  expect_equal(s$A2, rep(FALSE, 100))
  expect_equal(s$A4, rep(TRUE, 100))
})

test_that("maximum benefit objective (zero cost project locked out)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features, "name", "success",
      "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.15) %>%
    add_binary_decisions() %>%
    add_locked_out_action_constraints(which(sim_actions$cost == 0)) %>%
    add_random_solver(100, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # tests
  expect_s3_class(s, "tbl_df")
  expect_equal(s$solution, seq_len(100))
  expect_equal(s$status, rep(NA_character_, 100))
  expect_equal(s$obj, s$F1 + s$F2 + s$F3)
  expect_true(all(s$cost <= 0.15))
  expect_equal(s$baseline_action, rep(FALSE, 100))
})

test_that("maximum benefit (large problem, inc budgets)", {
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
  # generate solutions and run tests
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
      add_random_solver(verbose = FALSE, number_solutions = 5) %>%
      solve()
    # run tests
    expect_s3_class(s, "tbl_df")
    expect_equal(nrow(s), 5)
    expect_equal(s$status, rep(NA_character_, nrow(s)))
    expect_true(all(s$cost <= b))
  }
})

test_that("maximum benefit (large problem, inc budgets, locked constraints)", {
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
  # generate solutions and run tests
  for (p in seq(0.15, 1, length.out = 5)) {
    # generate solution
    b <- sum(actions$cost) * p
    s <-
      problem(
        projects = projects, actions = actions, features = features,
        "name", "success", "name", "cost", "name"
      ) %>%
      add_max_wtd_sum_objective(budget = b) %>%
      add_feature_weights("weight") %>%
      add_locked_in_action_constraints(c(1, 2, 3)) %>%
      add_locked_out_action_constraints(c(4, 5)) %>%
      add_binary_decisions() %>%
      add_random_solver(verbose = FALSE, number_solutions = 5) %>%
      solve()
    # run tests
    expect_s3_class(s, "tbl_df")
    expect_equal(nrow(s), 5)
    expect_equal(s$status, rep(NA_character_, nrow(s)))
    expect_true(all(s$action_1 == TRUE))
    expect_true(all(s$action_2 == TRUE))
    expect_true(all(s$action_3 == TRUE))
    expect_true(all(s$action_4 == FALSE))
    expect_true(all(s$action_5 == FALSE))
    expect_true(all(s$cost <= b))
  }
})

test_that("invalid arguments", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <- problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name", FALSE
  )
  # run tests
  ## load out all actions
  expect_warning({
    expect_error({
      p %>%
      add_max_wtd_sum_objective(budget = 0.15) %>%
      add_binary_decisions() %>%
      add_locked_out_action_constraints(seq_len(nrow(sim_actions))) %>%
      add_random_solver(1, verbose = FALSE) %>%
      solve()
    })
  })
  ## locked in actions exceed budget
  expect_warning({
    expect_error({
      p %>%
      add_max_wtd_sum_objective(budget = 0.15) %>%
      add_binary_decisions() %>%
      add_locked_in_action_constraints(seq_len(nrow(sim_actions))) %>%
      add_random_solver(1, verbose = FALSE) %>%
      solve()
    })
  })
  ## locked out actions make targets impossible to meet
  expect_warning({
    expect_error({
      p %>%
      add_min_set_objective() %>%
      add_absolute_targets(0.999) %>%
      add_binary_decisions() %>%
      add_locked_out_action_constraints(seq_len(nrow(sim_actions))) %>%
      add_random_solver(1, verbose = FALSE) %>%
      solve()
    })
  })
  ## number_solutions
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
  ## verbose
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
