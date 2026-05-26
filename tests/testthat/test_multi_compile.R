test_that("works", {
  # load data
  data(sim_multi_projects)
  data(sim_multi_features)
  data(sim_multi_actions)
  data(sim_multi_tree)
  # build problem
  p <-
    multi_problem(
      obj1 =
        problem(
          sim_multi_projects[[1]], sim_multi_actions, sim_multi_features[[1]],
          "name", "success", "name", "cost", "name", FALSE,
          baseline_project_name = "baseline_project_obj1"
        ) %>%
        add_max_wtd_sum_objective(budget = 200) %>%
        add_feature_weights("weight") %>%
        add_binary_decisions(),
      obj2 =
        problem(
          sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
          "name", "success", "name", "cost", "name", FALSE,
          baseline_project_name = "baseline_project_obj2"
        ) %>%
        add_max_wtd_sum_objective(budget = 200) %>%
        add_feature_weights("weight") %>%
        add_binary_decisions()
    )
  # compile problem
  o1 <- multi_compile(p)
  o2 <- dual_wtd_sum_multi_compile(
    sim_multi_projects[1:2], sim_multi_actions, sim_multi_features[1:2],
    budget = 200
  )
  ## reorder constraint matrix from R implementation to follow C++ order
  o2$A <- reorder_matrix(o2$A, o1$opt$A())
  # run tests
  ## structure
  expect_type(o1, "list")
  expect_type(o1$modelsense, "character")
  expect_type(o1$obj, "double")
  expect_true(is.matrix(o1$obj))
  expect_s3_class(o1$opt, "OptimizationProblem")
  ## obj
  expect_equal(nrow(o1$obj), 2)
  expect_equal(o1$obj, o2$obj, ignore_attr = TRUE)
  ## modelsense
  expect_equal(o1$modelsense, c("max", "max"))
  ## optimization problem
  expect_equal(o1$opt$lb(), o2$lb)
  expect_equal(o1$opt$ub(), o2$ub)
  expect_equal(o1$opt$vtype(), o2$vtype)
  expect_equal(o1$opt$sense(), o2$sense)
  expect_equal(o1$opt$rhs(), o2$rhs)
  expect_true(all(o1$opt$A() == o2$A))
})

test_that("locked constraints", {
  # load data
  data(sim_multi_projects)
  data(sim_multi_features)
  data(sim_multi_actions)
  data(sim_multi_tree)
  # build problem
  p <-
    multi_problem(
      obj1 =
        problem(
          sim_multi_projects[[1]], sim_multi_actions, sim_multi_features[[1]],
          "name", "success", "name", "cost", "name", FALSE,
          baseline_project_name = "baseline_project_obj1"
        ) %>%
        add_max_wtd_sum_objective(budget = 200) %>%
        add_locked_in_action_constraints("locked_in") %>%
        add_binary_decisions(),
      obj2 =
        problem(
          sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
          "name", "success", "name", "cost", "name", FALSE,
          baseline_project_name = "baseline_project_obj2"
        ) %>%
        add_max_wtd_sum_objective(budget = 200) %>%
        add_locked_out_action_constraints("locked_out") %>%
        add_binary_decisions()
    )
  # compile problem
  o <- multi_compile(p)
  # run tests
  expect_equal(
    o$opt$lb()[seq_len(nrow(sim_multi_actions))],
    replace(
      rep(0, nrow(sim_multi_actions)),
      which(sim_multi_actions$locked_in),
      1
    )
  )
  expect_equal(
    o$opt$ub()[seq_len(nrow(sim_multi_actions))],
    replace(
      rep(1, nrow(sim_multi_actions)),
      which(sim_multi_actions$locked_out),
      0
    )
  )
})
