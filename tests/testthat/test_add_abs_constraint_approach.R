test_that("all feasible solutions", {
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
    add_default_solver(gap = 0)
  # solve problems
  s1 <-
    p %>%
    add_abs_constraint_approach(goals = c(NA, 0.8)) %>%
    solve()
  s2 <-
    p %>%
    add_abs_constraint_approach(goals = c(80, 0.8)) %>%
    solve()
  # run tests
  ## s1
  expect_equal(s1$A1, FALSE)
  expect_equal(s1$A2, TRUE)
  expect_equal(s1$A3, TRUE)
  expect_equal(s1$A4, TRUE)
  expect_equal(s1$A5, FALSE)
  expect_equal(s1$A6, TRUE)
  expect_equal(s1$O1P1, FALSE)
  expect_equal(s1$O1P2, TRUE)
  expect_equal(s1$O1P3, TRUE)
  expect_equal(s1$O2P1, TRUE)
  expect_equal(s1$O2P2, FALSE)
  expect_equal(s1$O2P3, TRUE)
  ## s2
  expect_equal(s2$A1, FALSE)
  expect_equal(s2$A2, TRUE)
  expect_equal(s2$A3, TRUE)
  expect_equal(s2$A4, TRUE)
  expect_equal(s2$A5, FALSE)
  expect_equal(s2$A6, TRUE)
  expect_equal(s2$O1P1, FALSE)
  expect_equal(s2$O1P2, TRUE)
  expect_equal(s2$O1P3, TRUE)
  expect_equal(s2$O2P1, TRUE)
  expect_equal(s2$O2P2, FALSE)
  expect_equal(s2$O2P3, TRUE)
})

test_that("some feasible solutions", {
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
    add_abs_constraint_approach(
      goals = matrix(c(NA, 9999, NA, 0.8), ncol = 2, byrow = TRUE)
    ) %>%
    add_default_solver(gap = 0)
  # solve problem
  s <- solve(p)
  # run tests
  expect_true(is.numeric(s$solution))
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, TRUE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$A5, FALSE)
  expect_equal(s$A6, TRUE)
  expect_equal(s$O1P1, FALSE)
  expect_equal(s$O1P2, TRUE)
  expect_equal(s$O1P3, TRUE)
  expect_equal(s$O2P1, TRUE)
  expect_equal(s$O2P2, FALSE)
  expect_equal(s$O2P3, TRUE)
})

test_that("no feasible solutions", {
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
    add_abs_constraint_approach(
      goals = matrix(c(NA, 9999, 9999, 9999), ncol = 2, byrow = TRUE)
    ) %>%
    add_default_solver(gap = 0)
  # run tests
  expect_error(solve(p))
})
