test_that("compile", {
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
    cost = c(0.10, 0.10, 0.15, 0),
    locked_in = FALSE,
    locked_out = FALSE
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
    add_binary_decisions()
  # create optimization problem
  o1 <- compile(p, n_approx = 10)
  o2 <- max_phylo_div_mip_formulation(projects, actions, tree, 0.16, 10)
  # run tests
  expect_equal(o1$obj(), o2$obj)
  expect_equal(o1$vtype(), o2$vtype)
  expect_equal(o1$lb(), o2$lb)
  expect_equal(o1$ub(), o2$ub)
  expect_equal(o1$sense(), o2$sense)
  expect_equal(o1$rhs(), o2$rhs)
  expect_equal(o1$pwlobj(), o2$pwlobj)
  expect_true(all(o1$A() == o2$A))
})

test_that("solve (single solution)", {
  skip_on_cran()
  skip_if_not_installed("gurobi", "8.0.0")
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
  tree2 <- tree
  tree2$edge.length <- c(5, 100, 5, 5)
  # build problems
  p1 <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_phylo_div_objective(0.16, tree) %>%
    add_binary_decisions()
  p2 <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_phylo_div_objective(0.21, tree) %>%
    add_binary_decisions()
  p3 <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_phylo_div_objective(0.11, tree2) %>%
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
  expect_equal(s1$cost, 0.15)
  expect_equal(
    s1$obj,
    (5 * s1$F1) +
      (5 * s1$F2) +
      (5 * s1$F3) +
      (100 * (1 - ((1 - (s1$F1)) * (1 - (s1$F2)))))
  )
  expect_equal(s1$A1, FALSE)
  expect_equal(s1$A2, FALSE)
  expect_equal(s1$A3, TRUE)
  expect_equal(s1$A4, TRUE)
  expect_equal(s1$F1, 0.94 * 0.8)
  expect_equal(s1$F2, 0.94 * 0.8)
  expect_equal(s1$F3, 0.1 * 1)
  ## s2
  expect_s3_class(s2, "tbl_df")
  expect_equal(nrow(s2), 1)
  expect_equal(s2$solution, 1L)
  expect_true(is_optimal_solver_status(s2$status))
  expect_equal(
    s2$obj,
    (5 * s2$F1) +
      (5 * s2$F2) +
      (5 * s2$F3) +
      (100 * (1 - ((1 - (s2$F1)) * (1 - (s2$F2)))))
  )
  expect_equal(s2$cost, 0.2)
  expect_equal(s2$A1, TRUE)
  expect_equal(s2$A2, TRUE)
  expect_equal(s2$A3, FALSE)
  expect_equal(s2$A4, TRUE)
  expect_equal(s2$F1, 0.95 * 0.91)
  expect_equal(s2$F2, 0.96 * 0.92)
  expect_equal(s2$F3, 0.1 * 1)
  ## s3
  expect_s3_class(s3, "tbl_df")
  expect_equal(nrow(s3), 1)
  expect_equal(s3$solution, 1L)
  expect_true(is_optimal_solver_status(s3$status))
  expect_equal(
    s3$obj,
    (100 * s3$F1) +
      (5 * s3$F2) +
      (5 * s3$F3) +
      (5 * (1 - ((1 - (s3$F1)) * (1 - (s3$F2)))))
  )
  expect_equal(s3$cost, 0.1)
  expect_equal(s3$A1, TRUE)
  expect_equal(s3$A2, FALSE)
  expect_equal(s3$A3, FALSE)
  expect_equal(s3$A4, TRUE)
  expect_equal(s3$F1, 0.95 * 0.91)
  expect_equal(s3$F2, 0.1 * 1)
  expect_equal(s3$F3, 0.1 * 1)
})

test_that("solve (random order)", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("gurobi", "8.0.0")
  # create data
  projects <- tibble::tibble(
    name = letters[1:4],
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(0.91, 0.00, 0.80, 0.10),
    F3 = c(0.00, 0.00, 0.00, 0.80),
    F4 = c(0.00, 0.00, 0.00, 0.80),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(FALSE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(0.10, 0.10, 0.15, 0)
  )
  features <- tibble::tibble(name = c("F1", "F3", "F4"))
  tree <- ape::read.tree(text = "((F3, F4), F1);")
  tree$edge.length <- rep(5, nrow(tree$edge))
  # make problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_phylo_div_objective(0.16, tree) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # solve problem
  s <- solve(p)
  # solve problem
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_equal(s$solution, 1L)
  expect_true(is_optimal_solver_status(s$status))
  expect_equal(
    s$obj,
    (5 * 0.95 * 0.91) +
      (5 * 1 * 0.80) +
      (5 * 1 * 0.80) +
      (5 * (1 - ((1 - (0.8)) * (1 - (0.8)))))
  )
  expect_equal(s$cost, 0.1)
  expect_equal(s$A1, TRUE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$F1, 0.95 * 0.91)
  expect_equal(s$F3, 0.8 * 1)
  expect_equal(s$F4, 0.8 * 1)
})

test_that("solve (weights)", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("gurobi", "8.0.0")
  # create data
  projects <- tibble::tibble(
    name = letters[1:4],
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(0.91, 0.00, 0.00, 0.10),
    F2 = c(0.00, 0.00, 0.05, 0.01),
    F3 = c(0.00, 0.00, 0.00, 0.80),
    F4 = c(0.00, 0.00, 0.00, 0.80),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(FALSE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(0.10, 0.10, 0.15, 0)
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3", "F4"))
  tree <- ape::read.tree(text = "(((F3, F4), F1), F2);")
  tree$edge.length <- rep(5, nrow(tree$edge))
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_phylo_div_objective(0.16, tree) %>%
    add_feature_weights(c(4, 1000, 2, 4)) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # solve problem
  s <- solve(p)
  # solve problem
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_equal(s$solution, 1L)
  expect_true(is_optimal_solver_status(s$status))
  expect_equal(
    s$obj,
    (5 * 0.1 * 1) + (4 * 0.1 * 1) +
      (5 * 0.05 * 0.94) + (1000 * 0.05 * 0.94) +
      (5 * 1 * 0.80) + (2 * 0.8 * 1) +
      (5 * 1 * 0.80) + (4 * 0.8 * 1) +
      (5 * (1 - ((1 - (0.8)) * (1 - (0.8))))) +
      (5 * (1 - ((1 - (0.8)) * (1 - (0.8)) * (1 - (0.1 * 1)))))
  )
  expect_equal(s$cost, 0.15)
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
  expect_equal(s$F1, 0.1 * 1)
  expect_equal(s$F2, 0.05 * 0.94)
  expect_equal(s$F3, 0.8 * 1)
  expect_equal(s$F4, 0.8 * 1)
})

test_that("solve (constant branch probabilities)", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("gurobi", "8.0.0")
  # create data
  projects <- tibble::tibble(
    name = letters[1:4],
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(0.91, 0.00, 0.80, 0.10),
    F2 = c(0.00, 0.92, 0.80, 0.10),
    F3 = c(0.00, 0.00, 0.00, 1.0),
    F4 = c(0.00, 0.00, 0.00, 1.0),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(FALSE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(0.10, 0.10, 0.15, 0)
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3", "F4"))
  tree <- ape::read.tree(text = "((F1,F2),(F3,F4));")
  tree$edge.length <- c(5, 5, 5, 5, 5, 5, 5)
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_phylo_div_objective(0.16, tree) %>%
    add_binary_decisions()
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1)
  expect_equal(s$solution, 1L)
  expect_true(is_optimal_solver_status(s$status))
  expect_equal(
    s$obj,
    (0.752 * 5) +
      (0.752 * 5) +
      (1 * 5) +
      (1 * 5) +
      ((1 - ((1 - 0.752) * (1 - 0.752))) * 5) +
      ((1 - ((1 - 1) * (1 - 1))) * 5)
  )
  expect_equal(s$cost, 0.15)
  expect_equal(s$F1, 0.94 * 0.8)
  expect_equal(s$F2, 0.94 * 0.8)
  expect_equal(s$F3, 1.0)
  expect_equal(s$F4, 1.0)
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
})

test_that("invalid arguments", {
  # load data
  data(sim_projects, sim_actions, sim_features, sim_tree)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    )
  # run tests
  ## budgets
  expect_error({
    add_max_phylo_div_objective(p, NA_real_, sim_tree)
  })
  expect_error({
    add_max_phylo_div_objective(p, c(1, 1), sim_tree)
  })
  expect_error({
    add_max_phylo_div_objective(p, "a", sim_tree)
  })
  expect_error({
    add_max_phylo_div_objective(p, TRUE, sim_tree)
  })
  ## tree
  expect_error({
    add_max_phylo_div_objective(p, 1e+5, 1)
  })
  expect_error({
    sim_tree2 <- sim_tree
    sim_tree2$Nnode <- 1
    add_max_phylo_div_objective(p, 1e+5, sim_tree2)
  })
  expect_error({
    sim_tree2 <- ape::drop.tip(sim_tree, "F1")
    add_max_phylo_div_objective(p, 1e+5, sim_tree2)
  })
  skip_if_not_installed("gurobi", "8.0.0")
  expect_warning({
    p %>%
      add_max_phylo_div_objective(
        1e+5,
        replace(sim_tree, "edge.length", NULL)
      ) %>%
      solve()
  })
})

test_that("invalid arguments (zeros)", {
  # create data
  projects <- tibble::tibble(
    name = letters[1:4],
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(0.91, 0.00, 0.80, 0.0),
    F2 = c(0.00, 0.92, 0.80, 0.0),
    F3 = c(0.00, 0.00, 0.00, 0.0),
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
  tree <- ape::read.tree(text = "((F1,F2),F3);")
  tree$edge.length <- c(100, 5, 5, 5)
  # run test
  expect_error(
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_phylo_div_objective(0.16, tree),
    "non-zero"
  )
})
