test_that("compile", {
  # create problem
  data(sim_projects, sim_actions, sim_features)
  # create locked data
  d <- data.frame(
    action = c(
      sim_actions$name[sim_actions$locked_in],
      sim_actions$name[sim_actions$locked_out]
    ),
    status = c(
      rep(1, sum(sim_actions$locked_in)),
      rep(0, sum(sim_actions$locked_out))
    )
  )
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(100) %>%
    add_manual_locked_action_constraints(d)
  # compile problem
  o <- compile(p)
  # run tests
  expect_equal(
    o$lb(),
    replace(o$lb(), which(sim_actions$locked_in), 1)
  )
  expect_equal(
    o$ub(),
    replace(o$ub(), which(sim_actions$locked_out), 0)
  )
})

test_that("solve", {
  # define skips
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # create problem
  data(sim_projects, sim_actions, sim_features)
  # create locked data
  d <- data.frame(
    action = c(
      sim_actions$name[sim_actions$locked_in],
      sim_actions$name[sim_actions$locked_out]
    ),
    status = c(
      rep(1, sum(sim_actions$locked_in)),
      rep(0, sum(sim_actions$locked_out))
    )
  )
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_max_wtd_sum_objective(10000) %>%
    add_manual_locked_action_constraints(d)
  # solve problem
  s <- solve(p)
  # run tests
  for (i in sim_actions$name[sim_actions$locked_in]) {
    expect_equal(s[[i]], TRUE)
  }
  for (i in sim_actions$name[sim_actions$locked_out]) {
    expect_equal(s[[i]], FALSE)
  }
})
