test_that("compile", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.5) %>%
    add_binary_decisions()
  # compile problem
  o <- compile(p)
  # run tests
  expect_true(all(o$lb() == 0))
  expect_true(all(o$ub() == 1))
  expect_true(all(o$vtype() == "B"))
})

test_that("solve", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # generate solution
  p <- problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  ) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.5) %>%
    add_binary_decisions()
  s1 <- solve(p)
  s2 <- solve(p)
  # check that solutions have correct decisions
  expect_true(all(as.matrix(s1[, sim_actions$name]) %in% c(0, 1)))
  expect_equal(
    as.matrix(s1[, sim_actions$name]),
    as.matrix(s2[, sim_actions$name])
  )
})
