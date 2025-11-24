test_that("compile", {
  # create problem
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_in <- FALSE
  sim_projects$locked_out <- FALSE
  sim_projects$locked_in[1:2] <- TRUE
  sim_projects$locked_out[3] <- TRUE
  # create locked data
  d <- data.frame(
    project = c(
      sim_projects$name[sim_projects$locked_in],
      sim_projects$name[sim_projects$locked_out]
    ),
    status = c(
      rep(1, sum(sim_projects$locked_in)),
      rep(0, sum(sim_projects$locked_out))
    )
  )
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(100) %>%
    add_manual_locked_project_constraints(d)
  # compile problem
  o <- compile(p)
  # run tests
  expect_equal(
    o$lb(),
    replace(o$lb(), nrow(sim_actions) + which(sim_projects$locked_in), 1)
  )
  expect_equal(
    o$ub(),
    replace(o$ub(), nrow(sim_actions) + which(sim_projects$locked_out), 0)
  )
})

test_that("solve", {
  # define skips
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_projects, sim_actions, sim_features)
  sim_projects$locked_in <- FALSE
  sim_projects$locked_out <- FALSE
  sim_projects$locked_in[1:2] <- TRUE
  sim_projects$locked_out[3] <- TRUE
  # create locked data
  d <- data.frame(
    project = c(
      sim_projects$name[sim_projects$locked_in],
      sim_projects$name[sim_projects$locked_out]
    ),
    status = c(
      rep(1, sum(sim_projects$locked_in)),
      rep(0, sum(sim_projects$locked_out))
    )
  )
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(10000) %>%
    add_manual_locked_project_constraints(d)
  # solve problem
  s <- solve(p)
  # run tests
  for (i in sim_projects$name[sim_projects$locked_in]) {
    expect_equal(s[[i]], TRUE)
  }
  for (i in sim_projects$name[sim_projects$locked_out]) {
    expect_equal(s[[i]], FALSE)
  }
})
