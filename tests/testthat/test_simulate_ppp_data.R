context("simulate_ppp_data")

test_that("valid arguments", {
  s <- simulate_ppp_data(5, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01, 0.4, 0.01,
                         0.01)
  # verify object structure
  expect_is(s, "list")
  expect_is(s$projects, "tbl_df")
  expect_is(s$actions, "tbl_df")
  expect_is(s$features, "tbl_df")
  expect_is(s$tree, "phylo")
  # project data
  ## dimensions
  expect_equal(nrow(s$projects), 6)
  expect_equal(ncol(s$projects), 13)
  ## name column
  expect_is(s$projects$name, "character")
  expect_equal(anyDuplicated(s$projects$name), 0)
  expect_equal(s$projects$name[nrow(s$projects)], "baseline_project")
  expect_equal(s$projects$name[-nrow(s$projects)],
               paste0("F", seq_len(5), "_project"))
  ## success column
  expect_is(s$projects$success, "numeric")
  expect_true(all(s$projects$success >= 0.7))
  expect_true(all(s$projects$success[-nrow(s$actions)] <= 0.99))
  expect_equal(s$projects$success[nrow(s$actions)], 1)
  expect_true(all(is.finite(s$projects$success)))
  ## species persistence columns
  expect_equal(unique(vapply(s$projects[, paste0("F", seq_len(5)),
                                        drop = FALSE], class,
                                        character(1))), "numeric")
  expect_equal(rowSums(as.matrix(s$projects[, paste0("F", seq_len(5)),
                                            drop = FALSE]) > 0, na.rm = TRUE),
                                            c(rep(1, 5), 5))
  expect_gte(
    min(
      as_Matrix(
        as.matrix(s$projects[-6, paste0("F", seq_len(5)),drop = FALSE]),
        "dgCMatrix")@x,
      na.rm = TRUE
    ),
    0.5
  )
  expect_lte(max(as.matrix(s$projects[-6, paste0("F", seq_len(5)),
                                      drop = FALSE]), na.rm = TRUE), 0.9)
  expect_gte(min(as.matrix(s$projects[6, paste0("F", seq_len(5)),
                                      drop = FALSE])), 0.01)
  expect_lte(max(as.matrix(s$projects[6, paste0("F", seq_len(5)),
                                      drop = FALSE])), 0.4)
  ## organization data
  expect_true(all(vapply(
    s$projects[, grepl("action", names(s$projects))], inherits,
    logical(1), "logical")))
  expect_true(all(vapply(
    s$projects[, grepl("action", names(s$projects))], sum,
    numeric(1)) == 1))
  # action data
  ## dimensions
  expect_equal(ncol(s$actions), 4)
  expect_equal(nrow(s$actions), 6)
  ## name column
  expect_is(s$actions$name, "character")
  expect_equal(anyDuplicated(s$actions$name), 0)
  expect_equal(s$actions$name, c(paste0("F", seq_len(5), "_action"),
                                     "baseline_action"))
  ## locked in column
  expect_is(s$actions$locked_in, "logical")
  expect_equal(sum(s$actions$locked_in), 1L)
  expect_true(assertthat::noNA(s$actions$locked_in))
  ## locked out column
  expect_is(s$actions$locked_out, "logical")
  expect_equal(sum(s$actions$locked_out), 1L)
  expect_true(assertthat::noNA(s$actions$locked_out))
  ## cost column
  expect_is(s$actions$cost, "numeric")
  expect_true(all(s$actions$cost >= 0))
  expect_true(all(is.finite(s$actions$cost)))
  # species
  ## structure
  expect_equal(ncol(s$features), 2)
  expect_equal(nrow(s$features), 5)
  ## name column
  expect_is(s$features$name, "character")
  expect_equal(anyDuplicated(s$features$name), 0)
  expect_equal(s$features$name, paste0("F", seq_len(5)))
  # tree
  ## structure
  expect_equal(length(s$tree$tip.label), nrow(s$projects) - 1)
})

test_that("invalid arguments", {
  # number species
  expect_error(simulate_ppp_data(-1, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(0.5, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(NA_integer_, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data("a", 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # cost mean
  expect_error(simulate_ppp_data(100, -1, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, NA_real_, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, "a", 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # cost sd
  expect_error(simulate_ppp_data(100, 100, -1, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, NA_real_, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, "a", 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # success_min_probability
  expect_error(simulate_ppp_data(100, 100, 5, -0.1, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 1.1, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, NA_real_, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, "a", 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # success_max_probability
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 1.1, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, -0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, "a", 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, NA_real_, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.6, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # funded_min_persistence_probability
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, -0.1, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 1.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, "a", 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, NA_real_, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # funded_max_persistence_probability
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, -0.9, 0.01, 0.4,
                                 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 1.9,
                                 0.01, 100, 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, "a", 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, NA_real_, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.3, 0.01,
                                 0.4, 0.01, 0.01))
  # not_funded_min_persistence_probability
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, -0.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 1.01,
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, "a",
                                 0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, NA_real_,
                                 0.4, 0.01, 0.01))
  # not_funded_max_persistence_probability
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 -0.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 1.4, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 NA_real_, 0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.004, 0.01, 0.01))
  # locked_in_proportion
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 1.1, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, -0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, NA_real_, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, "a", 0.01))
  # locked_out_proportion
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 1.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, -0.01, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, NA_real_, 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, "a", 0.01))
  expect_error(simulate_ppp_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.5, 0.6))
})
