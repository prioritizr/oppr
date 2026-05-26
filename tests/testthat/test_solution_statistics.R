test_that("add_max_phylo_div_objective", {
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
    weight = c(100, 4, 9)
  )
  tree <- ape::read.tree(text = "((F1,F2),F3);")
  tree$edge.length <- c(100, 5, 5, 5)
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_phylo_div_objective(budget = 0.16, tree) %>%
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
  # run tests
  expect_equal(
    ss$cost,
    c(
      0.1 + 0.1 + 0,
      0.1 + 0,
      0.15 + 0,
      0.1 + 0.1 + 0.15 + 0
    )
  )
  expect_equal(
    ss$obj,
    (ss$F1 * 100) + (ss$F2 * 4) + (ss$F3[1] * 9) +
      (ss$F1 * 5) + (ss$F2 * 5) + (ss$F3 * 5) +
      ((1 - (1 - ss$F1) * (1 - ss$F2)) * 100)
  )
  expect_equal(
    ss$P1,
    c(TRUE, FALSE, FALSE, TRUE)
  )
  expect_equal(
    ss$P2,
    c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(
    ss$P3,
    c(FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    ss$P4,
    rep(TRUE, 4)
  )
  expect_equal(
    ss$F1,
    c(
      0.95 * 0.91,
      0.1 * 1,
      0.94 * 0.8,
      0.95 * 0.91
    )
  )
  expect_equal(
    ss$F2,
    c(
      0.96 * 0.92,
      0.96 * 0.92,
      0.94 * 0.8,
      0.96 * 0.92
    )
  )
  expect_equal(
    ss$F3,
    c(
      0.1 * 1,
      0.1 * 1,
      0.1 * 1,
      0.1 * 1
    )
  )
})

test_that("add_max_targets_met_objective", {
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
  # build problem
  p <-
    problem(
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
  # run tests
  expect_equal(
    ss$cost,
    c(
      0.1 + 0.1 + 0,
      0.1 + 0,
      0.15 + 0,
      0.1 + 0.1 + 0.15 + 0
    )
  )
  expect_equal(
    ss$obj,
    c(
      100 + 4 + 9,
      4 + 9,
      100 + 4 + 9,
      100 + 4 + 9
    )
  )
  expect_equal(
    ss$P1,
    c(TRUE, FALSE, FALSE, TRUE)
  )
  expect_equal(
    ss$P2,
    c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(
    ss$P3,
    c(FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    ss$P4,
    rep(TRUE, 4)
  )
  expect_equal(
    ss$F1,
    c(
      0.95 * 0.91,
      0.1 * 1,
      0.94 * 0.8,
      0.95 * 0.91
    )
  )
  expect_equal(
    ss$F2,
    c(
      0.96 * 0.92,
      0.96 * 0.92,
      0.94 * 0.8,
      0.96 * 0.92
    )
  )
  expect_equal(
    ss$F3,
    c(
      0.1 * 1,
      0.1 * 1,
      0.1 * 1,
      0.1 * 1
    )
  )
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
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # build problem
  p <-
    problem(
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
  # run tests
  expect_equal(
    ss$cost,
    c(
      0.1 + 0.1 + 0,
      0.1 + 0,
      0.15 + 0,
      0.1 + 0.1 + 0.15 + 0
    )
  )
  expect_equal(ss$obj, ss$cost)
  expect_equal(
    ss$P1,
    c(TRUE, FALSE, FALSE, TRUE)
  )
  expect_equal(
    ss$P2,
    c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(
    ss$P3,
    c(FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    ss$P4,
    rep(TRUE, 4)
  )
  expect_equal(
    ss$F1,
    c(
      0.95 * 0.91,
      0.1 * 1,
      0.94 * 0.8,
      0.95 * 0.91
    )
  )
  expect_equal(
    ss$F2,
    c(
      0.96 * 0.92,
      0.96 * 0.92,
      0.94 * 0.8,
      0.96 * 0.92
    )
  )
  expect_equal(
    ss$F3,
    c(
      0.1 * 1,
      0.1 * 1,
      0.1 * 1,
      0.1 * 1
    )
  )
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
  features <- tibble::tibble(
    name = c("F1", "F2", "F3"),
    weight = c(100, 4, 9)
  )
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(budget = 0.16) %>%
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
  # run tests
  expect_equal(
    ss$cost,
    c(
      0.1 + 0.1 + 0,
      0.1 + 0,
      0.15 + 0,
      0.1 + 0.1 + 0.15 + 0
    )
  )
  expect_equal(ss$obj, (ss$F1 * 100) + (ss$F2 * 4) + (ss$F3 * 9))
  expect_equal(
    ss$P1,
    c(TRUE, FALSE, FALSE, TRUE)
  )
  expect_equal(
    ss$P2,
    c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(
    ss$P3,
    c(FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    ss$P4,
    rep(TRUE, 4)
  )
  expect_equal(
    ss$F1,
    c(
      0.95 * 0.91,
      0.1 * 1,
      0.94 * 0.8,
      0.95 * 0.91
    )
  )
  expect_equal(
    ss$F2,
    c(
      0.96 * 0.92,
      0.96 * 0.92,
      0.94 * 0.8,
      0.96 * 0.92
    )
  )
  expect_equal(
    ss$F3,
    c(
      0.1 * 1,
      0.1 * 1,
      0.1 * 1,
      0.1 * 1
    )
  )
})

test_that("multi_problem()", {
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
    )
  # create solutions
  s <- data.frame(
    A1 = c(1, 0, 0, 1),
    A2 = c(1, 1, 0, 1),
    A3 = c(0, 0, 1, 1),
    A4 = c(1, 1, 1, 1)
  )
  # evaluate solutions
  ss <- solution_statistics(p, s)
  ss2 <- lapply(p$problems, solution_statistics, s)
  # run tests
  for (i in seq_along(ss2)) {
    names(ss2[[i]])[[2]] <- p$problem_names()[[i]]
    for (j in names(ss2[[i]])) {
      expect_equal(ss[[j]], ss2[[i]][[j]])
    }
  }
})
