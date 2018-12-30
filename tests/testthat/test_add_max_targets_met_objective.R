context("add_max_targets_met_objective")

test_that("compile", {
  # create data
  projects <- tibble::tibble(name = c("P1", "P2", "P3", "P4"),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             F1 =       c(0.91, 0.00, 0.80, 0.10),
                             F2 =       c(0.00, 0.92, 0.80, 0.10),
                             F3 =       c(0.00, 0.00, 0.00, 0.10),
                             A1 =       c(TRUE, FALSE, FALSE, FALSE),
                             A2 =       c(FALSE, TRUE, FALSE, FALSE),
                             A3 =       c(FALSE, FALSE, TRUE, FALSE),
                             A4 =       c(FALSE, FALSE, FALSE, TRUE))
  actions <- tibble::tibble(name =      c("A1", "A2", "A3", "A4"),
                            cost =      c(0.10, 0.10, 0.15, 0))
  features <- tibble::tibble(name = c("F1", "F2", "F3"),
                             target = c(0.1, 0.2, 0.3))
  # create problem
  p <- problem(projects, actions, features, "name", "success", "name", "cost",
               "name") %>%
       add_max_targets_met_objective(budget = 0.16) %>%
       add_absolute_targets("target") %>%
       add_binary_decisions()
  # create optimization problem
  o <- compile(p)
  # tests
  expect_equal(o$modelsense(), "max")
  expect_equal(o$obj(), c(rep(0, ncol(o$A()) - nrow(features)),
                          rep(1, nrow(features))))
  expect_equal(o$ub(), rep(1, ncol(o$A())))
  expect_equal(o$lb(), rep(0, ncol(o$A())))
  expect_equal(o$vtype(), c(rep("B", nrow(actions) + nrow(projects) +
                                     nrow(projects) * nrow(features)),
                            rep("B", nrow(features))))
  expect_equal(o$pwlobj(), list())
  expect_equal(o$col_ids(), c(rep("i", nrow(actions)), rep("j", nrow(projects)),
                              rep("fj", nrow(projects) * nrow(features)),
                              rep("f", nrow(features))))
  expect_equal(o$rhs(), c(rep(0, sum(p$pa_matrix() > 0)),
                          rep(0, nrow(features) * nrow(projects)),
                          rep(1, nrow(features)),
                          rep(0, nrow(features)),
                          0.16))
  expect_equal(o$sense(), c(rep(">=", sum(p$pa_matrix() > 0)),
                            rep(">=", nrow(features) * nrow(projects)),
                            rep("=", nrow(features)),
                            rep(">=", nrow(features)),
                            "<="))
  expect_equal(o$row_ids(), c(rep("c1", sum(p$pa_matrix() > 0)),
                            rep("c2", nrow(features) * nrow(projects)),
                            rep("c3", nrow(features)),
                            rep("c4", nrow(features)),
                            "m"))
  A <- Matrix::sparseMatrix(i = 1, j = 1, x = 0,
    dims = c(sum(p$pa_matrix() > 0) + (nrow(features) * nrow(projects)) +
             nrow(features) + nrow(features) + 1,
             nrow(actions) + nrow(projects) +
             (nrow(projects) * nrow(features)) + nrow(features)),
    dimnames = list(NULL, c(paste0("X_", seq_len(nrow(actions))),
                            paste0("Y_", seq_len(nrow(projects))),
                            paste0("Z_", outer(seq_len(nrow(projects)),
                                               seq_len(nrow(features)),
                                               paste0)),
                            paste0("F_", seq_len(nrow(features))))))
  A <- Matrix::drop0(A)
  curr_row <- 0
  for (j in seq_len(nrow(projects))) {
    for (i in seq_len(nrow(actions))) {
      if (projects[[actions$name[i]]][j]) {
        curr_row <- curr_row + 1
        A[curr_row, paste0("X_", i)] <- 1
        A[curr_row, paste0("Y_", j)] <- -1
      }
    }
  }
  for (f in seq_len(nrow(features))) {
    for (j in seq_len(nrow(projects))) {
      curr_row <- curr_row + 1
      A[curr_row, paste0("Y_", j)] <- 1
      A[curr_row, paste0("Z_", j, f)] <- -1
    }
  }
  for (f in seq_len(nrow(features))) {
    curr_row <- curr_row + 1
    A[curr_row, paste0("Z_", seq_len(nrow(projects)), f)] <- 1
  }
  for (f in seq_len(nrow(features))) {
    curr_row <- curr_row + 1
    curr_projects_for_f <- which(projects[[f]] > 0)
    A[curr_row, paste0("Z_", curr_projects_for_f, f)] <-
      projects[[features$name[f]]][curr_projects_for_f] *
      projects$success[curr_projects_for_f]
    A[curr_row, paste0("F_", f)] <- -1 * features$target[f]
  }
  curr_row <- curr_row + 1
  A[curr_row, seq_len(nrow(actions))] <- actions$cost
  expect_true(all(o$A() == A))
})

test_that("solve", {
  skip_on_cran()
  skip_if_not(any_solvers_installed())
  # make data
  projects <- tibble::tibble(name = c("P1", "P2", "P3", "P4"),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             F1 =       c(0.91, 0.00, 0.80, 0.10),
                             F2 =       c(0.00, 0.92, 0.80, 0.10),
                             F3 =       c(0.00, 0.00, 0.00, 0.10),
                             A1 =       c(TRUE, FALSE, FALSE, FALSE),
                             A2 =       c(FALSE, TRUE, FALSE, FALSE),
                             A3 =       c(FALSE, FALSE, TRUE, FALSE),
                             A4 =       c(FALSE, FALSE, FALSE, TRUE))
  actions <- tibble::tibble(name =      c("A1", "A2", "A3", "A4"),
                            cost =      c(0.10, 0.10, 0.15, 0))
  features <- tibble::tibble(name = c("F1", "F2", "F3"),
                             target1 = c(0.05, 0.9, 0.05),
                             target2 = c(0.9, 0.05, 0.05),
                             target3 = c(0.7, 0.7, 0.05))
  # create problem
  p1 <- problem(projects, actions, features, "name", "success", "name", "cost",
                "name") %>%
       add_max_targets_met_objective(budget = 0.11) %>%
       add_absolute_targets("target1") %>%
       add_binary_decisions()
  p2 <- problem(projects, actions, features, "name", "success", "name", "cost",
                "name") %>%
       add_max_targets_met_objective(budget = 0.11) %>%
       add_absolute_targets("target2") %>%
       add_binary_decisions()
  p3 <- problem(projects, actions, features, "name", "success", "name", "cost",
                "name") %>%
       add_max_targets_met_objective(budget = 0.16) %>%
       add_absolute_targets("target3") %>%
       add_binary_decisions()
  # solve problem
  s1 <- solve(p1)
  s2 <- solve(p2)
  s3 <- solve(p3)
  # tests
  ## s1
  expect_equal(s1$solution, 1L)
  expect_equal(s1$status, "OPTIMAL")
  expect_equal(s1$cost, 0.1)
  expect_equal(s1$A1, 1)
  expect_equal(s1$A2, 0)
  expect_equal(s1$A3, 0)
  expect_equal(s1$A4, 1)
  ## s2
  expect_equal(s2$solution, 1L)
  expect_equal(s2$status, "OPTIMAL")
  expect_equal(s2$cost, 0.1)
  expect_equal(s2$A1, 1)
  expect_equal(s2$A2, 0)
  expect_equal(s2$A3, 0)
  expect_equal(s2$A4, 1)
  ## s3
  expect_equal(s3$solution, 1L)
  expect_equal(s3$status, "OPTIMAL")
  expect_equal(s3$cost, 0.15)
  expect_equal(s3$A1, 0)
  expect_equal(s3$A2, 0)
  expect_equal(s3$A3, 1)
  expect_equal(s3$A4, 1)
})
