test_that("works (include_baseline = FALSE)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <- problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name", FALSE
  )
  # run tests
  ## display methods
  expect_type(print(p), "logical")
  expect_type(show(p), "logical")
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
  expect_equal(
    p$project_success_probabilities(),
    setNames(sim_projects$success, sim_projects$name)
  )
  expect_true(
    all(
      p$of_matrix() ==
        as_Matrix(as.matrix(sim_projects[, sim_features$name]), "dgCMatrix"),
      na.rm = TRUE
    )
  )
  expect_equal(rownames(p$of_matrix()), sim_projects$name)
  expect_equal(colnames(p$of_matrix()), sim_features$name)
  expect_true(
    all(
      p$eof_matrix() ==
        as_Matrix(
          as.matrix(sim_projects[, sim_features$name]) *
            matrix(p$project_success_probabilities(),
              ncol = p$number_of_features(),
              nrow = p$number_of_projects()
            ),
          "dgCMatrix"
        ),
      na.rm = TRUE
    )
  )
  expect_equal(rownames(p$eof_matrix()), sim_projects$name)
  expect_equal(colnames(p$eof_matrix()), sim_features$name)
  expect_true(
    all(
      p$pa_matrix() ==
        as_Matrix(as.matrix(sim_projects[, sim_actions$name]), "dgCMatrix")
    )
  )
  expect_equal(rownames(p$pa_matrix()), sim_projects$name)
  expect_equal(colnames(p$pa_matrix()), sim_actions$name)
  expect_error(p$feature_targets())
  ## setters
  p$set_data("feature_name_column", "test")
  expect_equal(p$get_data("feature_name_column"), "test")
  p$set_data("feature_name_column", "name")
})

test_that("works (include_baseline = TRUE)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", TRUE
    )
  # run tests
  ## display methods
  expect_type(print(p), "logical")
  expect_type(show(p), "logical")
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
  expect_equal(
    p$project_success_probabilities(),
    setNames(sim_projects$success, sim_projects$name)
  )
  expect_true(
    all(
      p$of_matrix() ==
        as_Matrix(
          as.matrix(sim_projects[, sim_features$name]),
          "dgCMatrix"
        ),
      na.rm = TRUE
    )
  )
  expect_equal(rownames(p$of_matrix()), sim_projects$name)
  expect_equal(colnames(p$of_matrix()), sim_features$name)
  sim_eof_matrix <- as_Matrix(
    as.matrix(sim_projects[, sim_features$name]) *
      matrix(p$project_success_probabilities(),
        ncol = p$number_of_features(),
        nrow = p$number_of_projects()
      ),
    "dgCMatrix"
  )
  for (i in seq_len(ncol(sim_eof_matrix))) {
    j <- which(sim_eof_matrix[-nrow(sim_eof_matrix), i] > 1e-10)
    curr_p <- sim_eof_matrix[j, i]
    curr_bp <- sim_eof_matrix[nrow(sim_eof_matrix), i]
    curr_p <- curr_p + ((1 - curr_p) * curr_bp)
    sim_eof_matrix[j, i] <- curr_p
  }
  expect_true(all(p$eof_matrix() == sim_eof_matrix, na.rm = TRUE))
  expect_equal(rownames(p$eof_matrix()), sim_projects$name)
  expect_equal(colnames(p$eof_matrix()), sim_features$name)
  expect_true(
    all(
      p$pa_matrix() ==
        as_Matrix(as.matrix(sim_projects[, sim_actions$name]), "dgCMatrix")
    )
  )
  expect_equal(rownames(p$pa_matrix()), sim_projects$name)
  expect_equal(colnames(p$pa_matrix()), sim_actions$name)
  expect_error(p$feature_targets())
  ## setters
  p$set_data("feature_name_column", "test")
  expect_equal(p$get_data("feature_name_column"), "test")
  p$set_data("feature_name_column", "name")
})

test_that("works (non-probability outcome data)", {
  # create data
  projects <- tibble::tibble(
    name = c("P1", "P2", "P3", "P4"),
    success = c(0.95, 0.96, 0.94, 1.00),
    F1 = c(91, 0.00, 80, 10),
    F2 = c(0.00, 92, 80, 10),
    F3 = c(0.00, 0.00, 0.00, 10),
    A1 = c(TRUE, FALSE, FALSE, FALSE),
    A2 = c(FALSE, TRUE, FALSE, FALSE),
    A3 = c(FALSE, FALSE, TRUE, FALSE),
    A4 = c(FALSE, FALSE, FALSE, TRUE)
  )
  actions <- tibble::tibble(
    name = c("A1", "A2", "A3", "A4"),
    cost = c(10, 10, 15, 0),
    locked_in = FALSE,
    locked_out = FALSE
  )
  features <- tibble::tibble(name = c("F1", "F2", "F3"))
  # build problem
  p <-
    problem(
      projects, actions, features,
      "name", "success", "name", "cost", "name", TRUE
    )
  # run tests
  ## display methods
  expect_type(print(p), "logical")
  expect_type(show(p), "logical")
  expect_equal(p$repr(), "ProjectProblem object")
  ## getters
  expect_equal(p$get_data("projects"), projects)
  expect_equal(p$get_data("actions"), actions)
  expect_equal(p$get_data("features"), features)
  expect_equal(p$get_data("project_name_column"), "name")
  expect_equal(p$get_data("project_success_column"), "success")
  expect_equal(p$get_data("action_name_column"), "name")
  expect_equal(p$get_data("action_cost_column"), "cost")
  expect_equal(p$get_data("feature_name_column"), "name")
  expect_equal(number_of_projects(p), nrow(projects))
  expect_equal(number_of_actions(p), nrow(actions))
  expect_equal(number_of_features(p), nrow(features))
  expect_equal(project_names(p), projects$name)
  expect_equal(action_names(p), actions$name)
  expect_equal(feature_names(p), features$name)
  expect_equal(p$action_costs(), setNames(actions$cost, actions$name))
  expect_equal(
    p$project_success_probabilities(),
    setNames(projects$success, projects$name)
  )
  expect_true(
    all(
      p$of_matrix() ==
        as_Matrix(
          as.matrix(projects[, features$name]),
          "dgCMatrix"
        ),
      na.rm = TRUE
    )
  )
  expect_equal(rownames(p$of_matrix()), projects$name)
  expect_equal(colnames(p$of_matrix()), features$name)
  eof_matrix <- as_Matrix(
    as.matrix(projects[, features$name]) *
      matrix(p$project_success_probabilities(),
        ncol = p$number_of_features(),
        nrow = p$number_of_projects()
      ),
    "dgCMatrix"
  )
  for (i in seq_len(ncol(eof_matrix))) {
    j <- which(eof_matrix[-nrow(eof_matrix), i] > 1e-10)
    curr_p <- eof_matrix[j, i]
    curr_bp <- eof_matrix[nrow(eof_matrix), i]
    curr_p <- curr_p + ((1 - curr_p) * curr_bp)
    eof_matrix[j, i] <- curr_p
  }
  expect_true(all(p$eof_matrix() == eof_matrix, na.rm = TRUE))
  expect_equal(rownames(p$eof_matrix()), projects$name)
  expect_equal(colnames(p$eof_matrix()), features$name)
  expect_true(
    all(
      p$pa_matrix() ==
        as_Matrix(as.matrix(projects[, actions$name]), "dgCMatrix")
    )
  )
  expect_equal(rownames(p$pa_matrix()), projects$name)
  expect_equal(colnames(p$pa_matrix()), actions$name)
})

test_that("invalid arguments", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # verify works with build in dataset
  expect_s3_class(
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    ),
    "ProjectProblem"
  )
  # run tests
  ## invalid names
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    problem(
      sim_projects, sim_actions, sim_features,
      "name1", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success1", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name1", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost1", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name1"
    )
  })
  ## invalid success
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects$success[1] <- NA_real_
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects$success[1] <- -1
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects$success[1] <- 2
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects$success <- as.character(sim_projects$success)
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  ## invalid costs
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_actions$cost[1] <- NA_real_
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_actions$cost[1] <- -5
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_actions$cost <- "2"
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  ## invalid species probabilities
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects$F1[1] <- NA_real_
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects$F1[1] <- -1
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects$F1[1] <- 2
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects$F1 <- as.character(sim_projects$F1)
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    problem(
      sim_projects[, -3], sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    problem(
      sim_projects[, -8], sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  ## feature columns
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_features$name[1] <- NA_character_
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_features$name <- 5
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_features$name <- TRUE
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects[nrow(sim_projects), sim_features$name[1]] <- 1e-12
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
  expect_error({
    data(sim_projects, sim_actions, sim_features)
    sim_projects[nrow(sim_projects), sim_features$name[1]] <- NA_real_
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name"
    )
  })
})
