test_that("works", {
  # create data
  d <- simulate_multi_ppp_data(
    number_objectives = 3,
    number_features = 7,
    number_actions = 5
  )
  # run tests
  ## classes
  expect_type(d, "list")
  expect_type(d$projects, "list")
  expect_s3_class(d$actions, "tbl_df")
  expect_type(d$features, "list")
  expect_type(d$tree, "list")
  ## projects
  expect_length(d$projects, 3)
  for (i in seq_along(d$projects)) {
    ## class
    expect_s3_class(d$projects[[i]], "tbl_df")
    ## name
    expect_true(assertthat::has_name(d$projects[[i]], "name"))
    expect_type(d$projects[[i]]$name, "character")
    ## success
    expect_true(assertthat::has_name(d$projects[[i]], "success"))
    expect_type(d$projects[[i]]$success, "double")
    expect_true(all(d$projects[[i]]$success >= 0))
    expect_true(all(d$projects[[i]]$success <= 1))
    ## actions
    expect_true(assertthat::has_name(d$projects[[i]], d$actions$name))
    expect_true(
      all(
        vapply(
          d$projects[[i]][, d$actions$name, drop = FALSE],
          inherits, logical(1), "logical"
        )
      )
    )
    ## features
    expect_true(assertthat::has_name(d$projects[[i]], d$features[[i]]$name))
    expect_true(
      all(
        vapply(
          d$projects[[i]][, d$features$name, drop = FALSE],
          inherits, logical(1), "numeric"
        )
      )
    )
  }
  ## actions
  expect_s3_class(d$actions, "tbl_df")
  expect_type(d$actions$name, "character")
  expect_type(d$actions$cost, "double")
  expect_type(d$actions$locked_in, "logical")
  expect_type(d$actions$locked_out, "logical")
  ## features
  expect_length(d$features, 3)
  for (i in seq_along(d$features)) {
    ## class
    expect_s3_class(d$features[[i]], "tbl_df")
    ## name
    expect_true(assertthat::has_name(d$features[[i]], "name"))
    expect_type(d$features[[i]]$name, "character")
    ## weight
    expect_true(assertthat::has_name(d$features[[i]], "weight"))
    expect_type(d$features[[i]]$weight, "double")
  }
  ## tree
  expect_length(d$tree, 3)
  for (i in seq_along(d$tree)) {
    ## class
    expect_s3_class(d$tree[[i]], "phylo")
    ## tip.labels
    expect_equal(
      sort(d$features[[i]]$name),
      sort(d$tree[[i]]$tip.label)
    )
  }
})
