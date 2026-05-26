test_that("sim_projects", {
  # load data
  data(sim_projects)
  # run tests
  ## data.frame properties
  expect_s3_class(sim_projects, "tbl_df")
  expect_equal(ncol(sim_projects), 13)
  expect_equal(nrow(sim_projects), 6)
  ## name column
  expect_type(sim_projects$name, "character")
  expect_equal(anyDuplicated(sim_projects$name), 0)
  ## success column
  expect_type(sim_projects$success, "double")
  expect_true(assertthat::noNA(sim_projects$success))
  expect_true(all(sim_projects$success >= 0))
  expect_true(all(sim_projects$success <= 1))
  ## species persistence probability columns
  for (s in paste0("F", seq_len(5))) {
    expect_type(sim_projects[[s]], "double")
    expect_true(all(sim_projects[[s]] >= 0, na.rm = TRUE))
    expect_true(all(sim_projects[[s]] <= 1, na.rm = TRUE))
    expect_true(sum(is.na(sim_projects[[s]])) == (nrow(sim_projects) - 2))
  }
  ## action columns
  for (s in paste0("F", seq_len(5), "_action")) {
    expect_type(sim_projects[[s]], "logical")
    expect_equal(sum(sim_projects[[s]]), 1)
    expect_true(assertthat::noNA(sim_projects[[s]]))
  }
})

test_that("sim_actions", {
  # load data
  data(sim_actions)
  # run tests
  ## data.frame properties
  expect_s3_class(sim_actions, "tbl_df")
  expect_equal(ncol(sim_actions), 4)
  expect_equal(nrow(sim_actions), 6)
  ## name column
  expect_type(sim_actions$name, "character")
  expect_equal(anyDuplicated(sim_actions$name), 0)
  expect_true(assertthat::noNA(sim_actions$name))
  ## cost column
  expect_type(sim_actions$cost, "double")
  expect_true(all(sim_actions$cost >= 0))
  expect_true(assertthat::noNA(sim_actions$cost))
  ## locked in column
  expect_type(sim_actions$locked_in, "logical")
  expect_true(sum(sim_actions$locked_in) == 1)
  expect_true(assertthat::noNA(sim_actions$locked_in))
  ## locked out column
  expect_type(sim_actions$locked_out, "logical")
  expect_true(sum(sim_actions$locked_out) == 1)
  expect_true(assertthat::noNA(sim_actions$locked_out))
  expect_equal(max(sim_actions$locked_in + sim_actions$locked_out), 1)
})

test_that("sim_features", {
  # load data
  data(sim_features)
  # run tests
  ## data.frame properties
  expect_s3_class(sim_features, "tbl_df")
  expect_equal(ncol(sim_features), 2)
  expect_equal(nrow(sim_features), 5)
  ## name column
  expect_type(sim_features$name, "character")
  expect_equal(anyDuplicated(sim_features$name), 0)
  expect_true(assertthat::noNA(sim_features$name))
  ## weight column
  expect_type(sim_features$weight, "double")
  expect_true(all(sim_features$weight >= 0))
  expect_true(assertthat::noNA(sim_features$weight))
})

test_that("sim_tree", {
  # load data
  data(sim_tree)
  # run tests
  expect_s3_class(sim_tree, "phylo")
  expect_true(is.null(suppressMessages(ape::checkValidPhylo(sim_tree))))
  expect_equal(sort(sim_tree$tip.label), sort(paste0("F", seq_len(5))))
})
