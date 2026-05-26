test_that("numeric(1)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_relative_targets(0.5)
  # calculate relative targets
  mp <- unname(rep(apply(p$eof_matrix(), 2, max, na.rm = TRUE)))
  targets <- p$targets$output()
  # run tests
  expect_s3_class(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "sense", "value")))
  expect_type(targets$feature, "integer")
  expect_type(targets$value, "double")
  expect_type(targets$sense, "character")
  expect_equal(targets$feature, seq_len(nrow(sim_features)))
  expect_equal(targets$value, mp * 0.5)
  expect_equal(targets$sense, rep(">=", nrow(sim_features)))
})

test_that("numeric(4)", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_relative_targets(seq_len(5) * 0.1)
  # calculate relative targets
  targets <- p$targets$output()
  mp <- unname(rep(apply(p$eof_matrix(), 2, max, na.rm = TRUE)))
  # run tests
  expect_s3_class(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "sense", "value")))
  expect_type(targets$feature, "integer")
  expect_type(targets$value, "double")
  expect_type(targets$sense, "character")
  expect_equal(targets$feature, seq_len(nrow(sim_features)))
  expect_equal(targets$value, mp * seq_len(5) * 0.1)
  expect_equal(targets$sense, rep(">=", nrow(sim_features)))
})

test_that("character", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  sim_features$target <- seq_len(5) * 0.1
  # build problem
  p <-
    problem(
      sim_projects, sim_actions, sim_features,
      "name", "success", "name", "cost", "name", FALSE
    ) %>%
    add_relative_targets("target")
  # calculate relative targets
  mp <- unname(rep(apply(p$eof_matrix(), 2, max, na.rm = TRUE)))
  targets <- p$targets$output()
  # run tests
  expect_s3_class(targets, "tbl_df")
  expect_true(all(names(targets) == c("feature", "sense", "value")))
  expect_type(targets$feature, "integer")
  expect_type(targets$value, "double")
  expect_type(targets$sense, "character")
  expect_equal(targets$feature, seq_len(nrow(sim_features)))
  expect_equal(targets$value, mp * sim_features$target)
  expect_equal(targets$sense, rep(">=", nrow(sim_features)))
})

test_that("invalid arguments", {
  # load data
  data(sim_projects, sim_actions, sim_features)
  # build problem
  p <- problem(
    sim_projects, sim_actions, sim_features,
    "name", "success", "name", "cost", "name"
  )
  # run tests
  ## single numeric values
  expect_error({
    add_relative_targets(p, 2)
  })
  expect_error({
    add_relative_targets(p, -1)
  })
  expect_error({
    add_relative_targets(p, NA_real_)
  })
  ## multiple numeric values
  expect_error({
    add_relative_targets(p, rep(0.1, nrow(sim_features) - 1))
  })
  expect_error({
    add_relative_targets(p, c(2, rep(0.1, nrow(sim_features) - 1)))
  })
  expect_error({
    add_relative_targets(p, c(-1, rep(0.1, nrow(sim_features) - 1)))
  })
  expect_error({
    add_relative_targets(p, c(NA_real_, rep(0.1, nrow(sim_features) - 1)))
  })
  ## character values
  expect_error({
    add_relative_targets(p, NA_character_)
  })
  expect_error({
    add_relative_targets(p, "a")
  })
})
