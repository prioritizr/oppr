test_that("max modelsense", {
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
    add_default_solver(gap = 0)
  # solve problems
  s1 <-
    p %>%
    add_ref_point_approach(weights = c(1, 0), goals = c(200, 2)) %>%
    solve()
  s2 <-
    p %>%
    add_ref_point_approach(weights = c(0, 1), goals = c(200, 2)) %>%
    solve()
  s3 <-
    p %>%
    add_ref_point_approach(weights = c(0.5, 0.5), goals = c(200, 2)) %>%
    solve()
  # run tests
  ## s1
  expect_equal(s1$A1, TRUE)
  expect_equal(s1$A2, TRUE)
  expect_equal(s1$A4, FALSE)
  expect_equal(s1$A5, FALSE)
  expect_equal(s1$A6, TRUE)
  expect_equal(s1$O1P1, TRUE)
  expect_equal(s1$O1P2, TRUE)
  expect_equal(s1$O2P1, FALSE)
  expect_equal(s1$O2P2, FALSE)
  expect_equal(s1$O2P3, TRUE)
  ## s2
  expect_equal(s2$A1, FALSE)
  expect_equal(s2$A2, FALSE)
  expect_equal(s2$A3, TRUE)
  expect_equal(s2$A4, TRUE)
  expect_equal(s2$A5, TRUE)
  expect_equal(s2$O1P1, FALSE)
  expect_equal(s2$O1P2, FALSE)
  expect_equal(s2$O1P3, TRUE)
  expect_equal(s2$O2P1, TRUE)
  expect_equal(s2$O2P2, TRUE)
  ## s3
  expect_equal(s3$A1, FALSE)
  expect_equal(s3$A2, TRUE)
  expect_equal(s3$A3, TRUE)
  expect_equal(s3$A4, TRUE)
  expect_equal(s3$A5, FALSE)
  expect_equal(s3$A6, TRUE)
  expect_equal(s3$O1P1, FALSE)
  expect_equal(s3$O1P2, TRUE)
  expect_equal(s3$O1P3, TRUE)
  expect_equal(s3$O2P1, TRUE)
  expect_equal(s3$O2P2, FALSE)
  expect_equal(s3$O2P3, TRUE)
})

test_that("manually specified best and worst", {
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
      O2F3 = c(0.00, 0.5, 0.01),
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
      add_max_wtd_sum_objective(budget = 1000) %>%
      add_binary_decisions(),
      obj2 = problem(
        projects[[2]], actions, features[[2]],
        "name", "success", "name", "cost", "name", FALSE,
        baseline_project_name = "O2P3"
      ) %>%
      add_max_wtd_sum_objective(budget = 1000) %>%
      add_binary_decisions()
    ) %>%
    add_ref_point_approach(
      weights = c(1, 0),
      goals = c(10, 2),
      best = c(10, 1),
      worst = c(0, 0)
    ) %>%
    add_default_solver(gap = 0)
  # solve problems
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(s$O1P1, TRUE)
  expect_equal(s$O1P2, TRUE)
})

test_that("zero budget", {
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
      O2F3 = c(0.00, 0.5, 0.01),
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
    tibble::tibble(name = c("O2F1", "O2F2", "O2F3"))
  )
  # define tree
  tree <- ape::read.tree(text = "((O2F1:5,O2F2:4):2,O2F3:8);")
  # build problem
  p <-
    multi_problem(
      obj1 = problem(
        projects[[1]], actions, features[[1]],
        "name", "success", "name", "cost", "name", FALSE,
        baseline_project_name = "O1P3"
      ) %>%
      add_max_wtd_sum_objective(budget = 0) %>%
      add_binary_decisions(),
      obj2 = problem(
        projects[[2]], actions, features[[2]],
        "name", "success", "name", "cost", "name", FALSE,
        baseline_project_name = "O2P3"
      ) %>%
      add_max_phylo_div_objective(budget = 0, tree = tree) %>%
      add_binary_decisions()
    ) %>%
    add_default_solver(gap = 0)
  # solve problems
  s1 <-
    p %>%
    add_ref_point_approach(weights = c(1, 0), goals = c(200, 2)) %>%
    solve()
  s2 <-
    p %>%
    add_ref_point_approach(weights = c(0, 1), goals = c(200, 2)) %>%
    solve()
  s3 <-
    p %>%
    add_ref_point_approach(weights = c(0.5, 0.5), goals = c(200, 2)) %>%
    solve()
  # run tests
  ## s1
  expect_equal(s1$A1, FALSE)
  expect_equal(s1$A2, FALSE)
  expect_equal(s1$A3, TRUE)
  expect_equal(s1$A5, FALSE)
  expect_equal(s1$A6, TRUE)
  expect_equal(s1$O1P1, FALSE)
  expect_equal(s1$O1P2, FALSE)
  expect_equal(s1$O1P3, TRUE)
  expect_equal(s1$O2P1, FALSE)
  expect_equal(s1$O2P2, FALSE)
  expect_equal(s1$O2P3, TRUE)
  ## s2
  expect_equal(s2$A1, FALSE)
  expect_equal(s2$A2, FALSE)
  expect_equal(s2$A3, TRUE)
  expect_equal(s2$A5, FALSE)
  expect_equal(s2$A6, TRUE)
  expect_equal(s2$O1P1, FALSE)
  expect_equal(s2$O1P2, FALSE)
  expect_equal(s2$O1P3, TRUE)
  expect_equal(s2$O2P1, FALSE)
  expect_equal(s2$O2P2, FALSE)
  expect_equal(s2$O2P3, TRUE)
  ## s3
  expect_equal(s3$A1, FALSE)
  expect_equal(s3$A2, FALSE)
  expect_equal(s3$A3, TRUE)
  expect_equal(s3$A5, FALSE)
  expect_equal(s3$A6, TRUE)
  expect_equal(s3$O1P1, FALSE)
  expect_equal(s3$O1P2, FALSE)
  expect_equal(s3$O1P3, TRUE)
  expect_equal(s3$O2P1, FALSE)
  expect_equal(s3$O2P2, FALSE)
  expect_equal(s3$O2P3, TRUE)
})

test_that("invalid inputs", {
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
    tibble::tibble(
      name = c("O1F1", "O1F2")
    ),
    tibble::tibble(
      name = c("O2F1", "O2F2"),
      target = c(0.2, 0.001)
    )
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
      add_min_set_objective() %>%
      add_absolute_targets("target") %>%
      add_binary_decisions()
    ) %>%
    add_default_solver(gap = 0)
  # run tests
  expect_error(
    p %>% add_ref_point_approach(weights = c(0.5, 0.5), goals = c(200, 2)),
    "max"
  )
})
