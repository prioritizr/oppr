test_that("problem() (budgets)", {
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
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # generate solution
  s <- solve(p)
  # run incremental rank procedure
  budgets <- seq(0, 0.16, length.out = 4)[-1]
  y <- rank_importance(p, s, n = 1, ranks = NULL, budgets = budgets)
  # tests
  expect_s3_class(y, "data.frame")
  expect_named(y, c("project", "rank", "score"))
  expect_equal(y$project, p$project_names())
  expect_equal(y$rank, c(NA_real_, NA_real_, 3, 1))
  expect_equal(y$score, c(NA_real_, NA_real_, 0, 1))
})

test_that("problem() (ranks)", {
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
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # generate solution
  s <- solve(p)
  # run incremental rank procedure
  y <- rank_importance(p, s, n = 1, ranks = 3, budgets = NULL)
  # tests
  expect_s3_class(y, "data.frame")
  expect_named(y, c("project", "rank", "score"))
  expect_equal(y$project, p$project_names())
  expect_equal(y$rank, c(NA_real_, NA_real_, 3, 1))
  expect_equal(y$score, c(NA_real_, NA_real_, 0, 1))
})

test_that("multi_problem() (budgets)", {
  # define skips
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  projects <- list(
    tibble::tibble(
      name = c("O1P1", "O1P2", "O1P3"),
      success = c(0.95, 0.96, 1.00),
      O1F1 = c(50, 0.00, 10),
      O1F2 = c(0.00, 92, 10),
      A1 = c(TRUE, FALSE, FALSE),
      A2 = c(FALSE, TRUE, FALSE),
      A3 = c(FALSE, FALSE, TRUE),
      A4 = c(FALSE, FALSE, FALSE),
      A5 = c(FALSE, FALSE, FALSE),
      A6 = c(FALSE, FALSE, FALSE)
    ),
    tibble::tibble(
      name = c("O2P1", "O2P2", "O2P3"),
      success = c(0.9, 0.4, 1.00),
      O2F1 = c(0.91, 0.00, 0.1),
      O2F2 = c(0.00, 0.92, 0.1),
      A1 = c(FALSE, FALSE, FALSE),
      A2 = c(FALSE, FALSE, FALSE),
      A3 = c(FALSE, FALSE, TRUE),
      A4 = c(TRUE, FALSE, FALSE),
      A5 = c(FALSE, TRUE, FALSE),
      A6 = c(FALSE, FALSE, TRUE)
    )
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4", "A5", "A6"),
    cost = c(0.10, 0.11, 0, 0.09, 0.12, 0),
    locked_in = FALSE,
    locked_out = FALSE
  )
  features <- list(
    tibble::tibble(name = c("O1F1", "O1F2")),
    tibble::tibble(name = c("O2F1", "O2F2"))
  )
  # build problem
  p <-
    multi_problem(
      obj1 = problem(
        projects[[1]], actions, features[[1]],
        "name", "success", "name", "cost", "name", FALSE,
        baseline_project_name = "O1P3"
      ) %>%
      add_max_wtd_sum_objective(budget = 0.25) %>%
      add_binary_decisions(),
      obj2 = problem(
        projects[[2]], actions, features[[2]],
        "name", "success", "name", "cost", "name", FALSE,
        baseline_project_name = "O2P3"
      ) %>%
      add_max_wtd_sum_objective(budget = 0.25) %>%
      add_binary_decisions()
    ) %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    add_wtd_goal_approach(weights = c(1, 1), goals = c(10, 10))
  # generate solution
  s <- solve(p)
  # run incremental rank procedure
  budgets <- seq(0, 0.25, length.out = 4)[-1]
  y <- rank_importance(p, s, n = 1, ranks = NULL, budgets = budgets)
  # run tests
  expect_s3_class(y, "data.frame")
  expect_named(y, c("project", "rank", "score"))
  expect_equal(
    y$project,
    c(p$problems[[1]]$project_names(), p$problems[[2]]$project_names())
  )
  expect_equal(
    y$rank,
    c(NA_real_, NA_real_, 1, 2, 3, 1)
  )
  expect_equal(
    y$score,
    c(NA_real_, NA_real_, 1, 0.5, 0, 1)
  )
})

test_that("multi_problem() (budgets)", {
  # define skips
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create data
  projects <- list(
    tibble::tibble(
      name = c("O1P1", "O1P2", "O1P3"),
      success = c(0.95, 0.96, 1.00),
      O1F1 = c(50, 0.00, 10),
      O1F2 = c(0.00, 92, 10),
      A1 = c(TRUE, FALSE, FALSE),
      A2 = c(FALSE, TRUE, FALSE),
      A3 = c(FALSE, FALSE, TRUE),
      A4 = c(FALSE, FALSE, FALSE),
      A5 = c(FALSE, FALSE, FALSE),
      A6 = c(FALSE, FALSE, FALSE)
    ),
    tibble::tibble(
      name = c("O2P1", "O2P2", "O2P3"),
      success = c(0.9, 0.4, 1.00),
      O2F1 = c(0.91, 0.00, 0.1),
      O2F2 = c(0.00, 0.92, 0.1),
      A1 = c(FALSE, FALSE, FALSE),
      A2 = c(FALSE, FALSE, FALSE),
      A3 = c(FALSE, FALSE, TRUE),
      A4 = c(TRUE, FALSE, FALSE),
      A5 = c(FALSE, TRUE, FALSE),
      A6 = c(FALSE, FALSE, TRUE)
    )
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4", "A5", "A6"),
    cost = c(0.10, 0.11, 0, 0.09, 0.12, 0),
    locked_in = FALSE,
    locked_out = FALSE
  )
  features <- list(
    tibble::tibble(name = c("O1F1", "O1F2")),
    tibble::tibble(name = c("O2F1", "O2F2"))
  )
  # build problem
  p <-
    multi_problem(
      obj1 = problem(
        projects[[1]], actions, features[[1]],
        "name", "success", "name", "cost", "name", FALSE,
        baseline_project_name = "O1P3"
      ) %>%
      add_max_wtd_sum_objective(budget = 0.25) %>%
      add_binary_decisions(),
      obj2 = problem(
        projects[[2]], actions, features[[2]],
        "name", "success", "name", "cost", "name", FALSE,
        baseline_project_name = "O2P3"
      ) %>%
      add_max_wtd_sum_objective(budget = 0.25) %>%
      add_binary_decisions()
    ) %>%
    add_default_solver(gap = 0, verbose = FALSE) %>%
    add_wtd_goal_approach(weights = c(1, 1), goals = c(10, 10))
  # generate solution
  s <- solve(p)
  # run incremental rank procedure
  y <- rank_importance(p, s, n = 1, ranks = 3, budgets = NULL)
  # run tests
  expect_s3_class(y, "data.frame")
  expect_named(y, c("project", "rank", "score"))
  expect_equal(
    y$project,
    c(p$problems[[1]]$project_names(), p$problems[[2]]$project_names())
  )
  expect_equal(
    y$rank,
    c(NA_real_, NA_real_, 1, 2, 3, 1)
  )
  expect_equal(
    y$score,
    c(NA_real_, NA_real_, 1, 0.5, 0, 1)
  )
})
