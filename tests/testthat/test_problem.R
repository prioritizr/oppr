context("problem")

test_that("valid arguments", {
  # data
  data(sim_projects, sim_actions, sim_features)
  p <- problem(sim_projects, sim_actions, sim_features,
               "name", "success", "name", "cost", "name")
  # tests
  ## display methods
  expect_is(print(p), "NULL")
  expect_is(show(p), "NULL")
  expect_equal(p$repr(), "ProjectProblem object")
  ## getters
  expect_equal(p$get_data("projects"), sim_projects)
  expect_equal(p$get_data("actions"), sim_actions)
  expect_equal(p$get_data("features"), sim_features)
  expect_equal(p$get_data("project_name_column"), "name")
  expect_equal(p$get_data("project_success_column"), "success")
  expect_equal(p$get_data("action_name_column"), "name")
  expect_equal(p$get_data("action_cost_column"), "cost")
  expect_equal(p$get_data("feature_name_column"), "name")
  expect_equal(number_of_projects(p), nrow(sim_projects))
  expect_equal(number_of_actions(p), nrow(sim_actions))
  expect_equal(number_of_features(p), nrow(sim_features))
  expect_equal(project_names(p), sim_projects$name)
  expect_equal(action_names(p), sim_actions$name)
  expect_equal(feature_names(p), sim_features$name)
  expect_equal(p$action_costs(), setNames(sim_actions$cost, sim_actions$name))
  expect_equal(p$project_success_probabilities(),
               setNames(sim_projects$success, sim_projects$name))
  expect_true(all(p$pf_matrix() ==
                  as(as.matrix(sim_projects[, sim_features$name]),
                     "dgCMatrix")))
  expect_equal(rownames(p$pf_matrix()), sim_projects$name)
  expect_equal(colnames(p$pf_matrix()), sim_features$name)
  expect_true(all(p$pa_matrix() ==
                  as(as.matrix(sim_projects[, sim_actions$name]), "dgCMatrix")))
  expect_equal(rownames(p$pa_matrix()), sim_projects$name)
  expect_equal(colnames(p$pa_matrix()), sim_actions$name)
  expect_error(p$feature_targets())
  ## setters
  p$set_data("feature_name_column", "test")
  expect_equal(p$get_data("feature_name_column"), "test")
  p$set_data("feature_name_column", "name")
})

test_that("invalid arguments", {
  data(sim_projects, sim_actions, sim_features)


})

test_that("inheritance", {
  data(sim_projects, sim_actions, sim_features)
})
