test_that("integer (compile)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(1) %>%
    add_locked_out_project_constraints(which(sim_projects$locked_out))
  # compile problem
  o <- compile(p)
  # run tests
  expect_equal(
    o$ub(),
    replace(o$ub(), nrow(sim_actions) + which(sim_projects$locked_out), 0)
  )
})

test_that("integer (solve)", {
  # define skips
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
  # create problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(1e+5) %>%
    add_locked_out_project_constraints(which(sim_projects$locked_out))
  # solve problem
  s <- solve(p)
  # run tests
  for (i in sim_projects$name[sim_projects$locked_out]) {
    expect_equal(s[[i]], FALSE)
  }
})

test_that("integer (invalid arguments)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(1)
  # run tests
  expect_error({
    add_locked_out_project_constraints(p, -1)
  })
  expect_error({
    add_locked_out_project_constraints(p, 0)
  })
  expect_error({
    add_locked_out_project_constraints(p, nrow(sim_actions) + 1)
  })
  expect_error({
    add_locked_out_project_constraints(p, NA_real_)
  })
})

test_that("logical (compile)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(1) %>%
    add_locked_out_project_constraints(sim_projects$locked_out)
  # compile problem
  o <- compile(p)
  # run tests
  expect_equal(
    o$ub(),
    replace(o$ub(), nrow(sim_actions) + which(sim_projects$locked_out), 0)
  )
})

test_that("logical (invalid arguments)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(1)
  # run tests
  expect_error({
    add_locked_out_project_constraints(p, FALSE)
  })
  expect_error({
    add_locked_out_project_constraints(p, c(TRUE, TRUE))
  })
  expect_error({
    add_locked_out_project_constraints(p, NA)
  })
})

test_that("logical (solve)", {
  # define skips
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(1e+5) %>%
    add_locked_out_project_constraints(sim_projects$locked_out)
  # solve problem
  s <- solve(p)
  # run tests
  for (i in sim_projects$name[sim_projects$locked_out]) {
    expect_equal(s[[i]], FALSE)
  }
})

test_that("character (compile)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(1) %>%
    add_locked_out_project_constraints("locked_out")
  # compile problem
  o <- compile(p)
  # run tests
  expect_equal(
    o$ub(),
    replace(o$ub(), nrow(sim_actions) + which(sim_projects$locked_out), 0)
  )
})

test_that("logical (invalid arguments)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(1)
  # run tests
  expect_error({
    add_locked_out_project_constraints(p, "name")
  })
  expect_error({
    add_locked_out_project_constraints(p, "column_that_doesnt_exist")
  })
  expect_error({
    add_locked_out_project_constraints(p, NA_character_)
  })
})

test_that("character (solve)", {
  # define skips
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_out <- c(TRUE, rep(FALSE, nrow(sim_projects) - 1))
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(1e+5) %>%
    add_locked_out_project_constraints("locked_out")
  # solve problem
  s <- solve(p)
  # run tests
  for (i in sim_projects$name[sim_projects$locked_out]) {
    expect_equal(s[[i]], FALSE)
  }
})
