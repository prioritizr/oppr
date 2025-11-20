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
          "name", "success", "name", "cost", "name",
          baseline_project_name = "baseline_project_obj1"
        ) %>%
        add_max_phylo_div_objective(
          budget = 200, tree = sim_multi_tree[[1]]
        ) %>%
        add_binary_decisions(),
     obj2 =
       problem(
         sim_multi_projects[[2]], sim_multi_actions, sim_multi_features[[2]],
         "name", "success", "name", "cost", "name",
         baseline_project_name = "baseline_project_obj2"
       ) %>%
       add_max_richness_objective(budget = 200) %>%
       add_binary_decisions(),
     obj3 =
       problem(
         sim_multi_projects[[3]], sim_multi_actions, sim_multi_features[[3]],
         "name", "success", "name", "cost", "name",
         baseline_project_name = "baseline_project_obj3"
       ) %>%
       add_max_wtd_sum_objective(budget = 200) %>%
       add_binary_decisions()
   )
  # run tests
  ## display methods
  expect_type(print(p), "logical")
  expect_type(show(p), "logical")
  expect_equal(p$repr(), "MultiObjProjectProblem object")
  ## getters
  expect_equal(
    number_of_projects(p),
    sum(vapply(sim_multi_projects, nrow, integer(1)))
  )
  expect_equal(
    number_of_actions(p),
    nrow(sim_multi_actions)
  )
  expect_equal(
    number_of_features(p),
    sum(vapply(sim_multi_features, nrow, integer(1)))
  )
  expect_equal(
    number_of_problems(p),
    3
  )
  expect_equal(
    problem_names(p),
    c("obj1", "obj2", "obj3")
  )
  expect_equal(
    project_names(p),
    setNames(
      lapply(sim_multi_projects, `[[`, "name"),
      p$problem_names()
    )
  )
  expect_equal(
    action_names(p),
    sim_multi_actions$name
  )
  expect_equal(
    feature_names(p),
    setNames(
      lapply(sim_multi_features, `[[`, "name"),
      p$problem_names()
    )
  )
})
