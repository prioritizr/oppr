test_that("format", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("gurobi", "13.0.0")
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.5) %>%
    add_binary_decisions() %>%
    add_gurobi_solver()
  # solve problem
  s <- solve(p)
  # check that solution has correct properties
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_gt(ncol(s), 0)
})

test_that("linear objective", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("gurobi", "13.0.0")
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
      "name"
    ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(start = c(FALSE, NA, NA, NA))
  # solve problem
  s <- solve(p)
  # run tests
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
})

test_that("multiple solutions (single objective)", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("gurobi", "13.0.0")
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
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name"
    ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0.5, number_solutions = 2)
  # solve problem
  s <- solve(p)
  # run tests
  expect_equal(nrow(s), 2)
})

test_that("multiple solutions (multiple objectives)", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("gurobi", "13.0.0")
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
  tree <- ape::read.tree(text = "((O1_F1,O1_F2),O1_F3);")
  tree$edge.length <- c(100, 5, 5, 5)
  # build problem
  p <-
    multi_problem(
      obj1 =
        problem(
          projects %>%
            gsub_column("name", "P", "O1_P") %>%
            gsub_names("F", "O1_F"),
          actions,
          features %>% gsub_column("name", "F", "O1_F"),
          "name", "success", "name", "cost", "name", FALSE
        ) %>%
        add_max_phylo_div_objective(budget = 0.16, tree) %>%
        add_feature_weights("weight") %>%
        add_binary_decisions(),
      obj2 =
        problem(
          projects %>%
            gsub_column("name", "P", "O2_P") %>%
            gsub_names("F", "O2_F"),
          actions,
          features %>% gsub_column("name", "F", "O2_F"),
          "name", "success", "name", "cost", "name", FALSE
        ) %>%
        add_max_wtd_sum_objective(budget = 0.16) %>%
        add_feature_weights("weight") %>%
        add_binary_decisions(),
      obj3 =
        problem(
          projects %>%
            gsub_column("name", "P", "O3_P") %>%
            gsub_names("F", "O3_F"),
          actions,
          features %>% gsub_column("name", "F", "O3_F"),
          "name", "success", "name", "cost", "name", FALSE
        ) %>%
        add_max_targets_met_objective(budget = 0.16) %>%
        add_absolute_targets("target") %>%
        add_binary_decisions()
    ) %>%
    add_wtd_goal_approach(weights = c(11, 12, 13), goals = c(4, 5, 6)) %>%
    add_gurobi_solver(gap = 0.5, number_solutions = 3)
  # solve problem
  s <- solve(p)
  # run tests
  expect_equal(nrow(s), 3)
})
