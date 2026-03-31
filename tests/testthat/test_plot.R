test_that("no phylogenetic data", {
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
  g <- plot(p, solution)
  d <- plot(p, solution, return_data = TRUE)
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
  expect_s3_class(d, "tbl_df")
})

test_that("phylogenetic data", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("ggtree")
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
  solution <- tibble::tibble(A1 = TRUE, A2 = TRUE, A3 = FALSE, A4 = TRUE)
  # build problem
  p <-
    problem(
      projects, actions, features, "name", "success", "name", "cost",
      "name"
    ) %>%
    add_max_phylo_div_objective(0.16, tree) %>%
    add_binary_decisions()
  # make plot
  g <- plot(p, solution)
  d <- plot(p, solution, return_data = TRUE)
  # run tests
  expect_s3_class(g, "ggtree")
  expect_true({
    f <- tempfile(fileext = ".png")
    png(f)
    print(g)
    dev.off()
    unlink(f)
    TRUE
  })
  expect_s4_class(d, "treedata")
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
  expect_s3_class(plot(p, solution), "ggplot")
  ## invalid problem
  expect_error({
    plot(
      problem(
        sim_projects, sim_actions, sim_features, "name", "success",
        "name", "cost", "name"
      ),
      solution
    )
  })
  ## additional arguments
  expect_error({
    plot(p, s, extra_argument = NULL)
  })
})
