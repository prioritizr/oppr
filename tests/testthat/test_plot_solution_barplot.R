test_that("some projects funded", {
  # create data
  projects <- tibble::tibble(
    name = letters[1:4],
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(0.91, 0.00, 0.80, 0.10),
    F2 = c(0.00, 0.92, 0.80, 0.10),
    F3 = c(0.00, 0.00, 0.00, 0.10),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(TRUE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(0.10, 0.10, 0.15, 0),
    locked_in = FALSE,
    locked_out = FALSE
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  solution <- tibble::tibble(A1 = TRUE, A2 = TRUE, A3 = FALSE, A4 = TRUE)
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name"
    ) %>%
    add_max_wtd_sum_objective(0.16) %>%
    add_binary_decisions()
  # make plot
  g <- plot_solution_barplot(p, solution)
  # run tests
  expect_s3_class(g, "ggplot")
  expect_true({
    f <- tempfile(fileext = ".png")
    png(f)
    print(g)
    dev.off()
    unlink(f)
    TRUE
  })
})

test_that("all projects funded", {
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
  solution <- tibble::tibble(A1 = TRUE, A2 = TRUE, A3 = TRUE, A4 = TRUE)
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name"
    ) %>%
    add_max_wtd_sum_objective(0.16) %>%
    add_binary_decisions()
  # make plot
  g <- plot_solution_barplot(p, solution)
  # run tests
  expect_s3_class(g, "ggplot")
  expect_true({
    f <- tempfile(fileext = ".png")
    png(f)
    print(g)
    dev.off()
    unlink(f)
    TRUE
  })
})

test_that("no projects funded", {
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
  solution <- tibble::tibble(A1 = FALSE, A2 = FALSE, A3 = FALSE, A4 = FALSE)
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name"
    ) %>%
    add_max_wtd_sum_objective(0.16) %>%
    add_binary_decisions()
  # make plot
  g <- plot_solution_barplot(p, solution)
  # run tests
  expect_s3_class(g, "ggplot")
  expect_true({
    f <- tempfile(fileext = ".png")
    png(f)
    print(g)
    dev.off()
    unlink(f)
    TRUE
  })
})

test_that("non-probability outcome values for a feature", {
  # create data
  projects <- tibble::tibble(
    name = letters[1:4],
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(91, 0.00, 80, 10),
    F2 = c(0.00, 0.92, 0.80, 0.10),
    F3 = c(0.00, 0.00, 0.00, 0.10),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(TRUE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(0.10, 0.10, 0.15, 0),
    locked_in = FALSE,
    locked_out = FALSE
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  solution <- tibble::tibble(A1 = TRUE, A2 = TRUE, A3 = FALSE, A4 = TRUE)
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name"
    ) %>%
    add_max_wtd_sum_objective(0.16) %>%
    add_binary_decisions()
  # make plot
  g <- plot_solution_barplot(p, solution)
  # run tests
  expect_s3_class(g, "ggplot")
  expect_true({
    f <- tempfile(fileext = ".png")
    png(f)
    print(g)
    dev.off()
    unlink(f)
    TRUE
  })
})

test_that("invalid arguments", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features, "name", "success",
      "name", "cost", "name"
    ) %>%
    add_max_wtd_sum_objective(0.16) %>%
    add_binary_decisions()
  # create solution
  solution <- as.data.frame(
    matrix(
      rep(TRUE, p$number_of_actions()),
      nrow = 1,
      dimnames = list(NULL, p$action_names())
    )
  )
  # run tests
  ## verify that test data yields plot
  expect_s3_class(plot_solution_barplot(p, solution), "ggplot")
  ## invalid problem
  expect_error({
    plot_solution_barplot(
      problem(
        sim_projects, sim_actions, sim_features, "name", "success",
        "name", "cost", "name"
      ),
      solution
    )
  })
  ## invalid solution
  expect_error({
    plot_solution_barplot(p, as.matrix(solution))
  })
  expect_error({
    s <- solution
    s[[1]] <- NA_real_
    plot_solution_barplot(p, s)
  })
  expect_error({
    s <- solution
    s[[1]] <- "a"
    plot_solution_barplot(p, s)
  })
  expect_error({
    s <- solution
    plot_solution_barplot(p, solution[, -1, drop = FALSE])
  })
  ## invalid n
  expect_error({
    plot_solution_barplot(p, solution, NA_integer_)
  })
  expect_error({
    plot_solution_barplot(p, solution, "a")
  })
  expect_error({
    plot_solution_barplot(p, solution, TRUE)
  })
  ## invalid hjust
  expect_error({
    plot_solution_barplot(p, solution, 1, NA_real_)
  })
  expect_error({
    plot_solution_barplot(p, solution, 1, "a")
  })
  expect_error({
    plot_solution_barplot(p, solution, 1, TRUE)
  })
})
