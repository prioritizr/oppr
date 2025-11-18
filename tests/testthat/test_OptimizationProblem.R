test_that("new problem", {
  # create data
  x <- new_optimization_problem()
  # run tests
  expect_equal(x$ncell(), 0)
})

test_that("get methods", {
  # data
  l <- list(
    modelsense = "min",
    A_i = c(0L, 1L, 1L),
    A_j = c(0L, 1L, 2L),
    A_x = c(7, 8, 9),
    obj = c(9, 10, 11),
    pwlobj = list(1),
    lb = c(12, 13, 14),
    ub = c(15, 16, 17),
    rhs = c(18, 19),
    number_of_projects = 2,
    number_of_actions = 3,
    number_of_features = 1,
    number_of_branches = 10,
    number_of_allocations = 5,
    sense = c("=", "="),
    vtype = c("B", "S", "C"),
    row_ids = c("a", "b"),
    col_ids = c("d", "e", "f")
  )
  x <- predefined_optimization_problem(l, list(i = 4))
  # run tests
  expect_equal(x$nrow(), 2)
  expect_equal(x$ncol(), 3)
  expect_equal(x$ncell(), length(l$A_x))
  expect_equal(
    x$A(),
    Matrix::sparseMatrix(i = l$A_i, j = l$A_j, x = l$A_x, index1 = FALSE)
  )
  expect_equal(x$modelsense(), l$modelsense)
  expect_equal(x$obj(), l$obj)
  expect_equal(x$pwlobj(), l$pwlobj)
  expect_equal(x$rhs(), l$rhs)
  expect_equal(x$sense(), l$sense)
  expect_equal(x$lb(), l$lb)
  expect_equal(x$ub(), l$ub)
  expect_equal(x$number_of_projects(), l$number_of_projects)
  expect_equal(x$number_of_actions(), l$number_of_actions)
  expect_equal(x$number_of_features(), l$number_of_features)
  expect_equal(x$number_of_branches(), l$number_of_branches)
  expect_equal(x$number_of_allocations(), l$number_of_allocations)
  expect_equal(x$col_ids(), l$col_ids)
  expect_equal(x$row_ids(), l$row_ids)
  expect_equal(x$get_data(), list(i = 4))
})

test_that("as.list", {
  # data
  l <- list(
    modelsense = "min",
    A_i = c(0L, 1L, 1L),
    A_j = c(0L, 1L, 2L),
    A_x = c(7, 8, 9),
    obj = c(9, 10, 11),
    pwlobj = list(500),
    lb = c(12, 13, 14),
    ub = c(15, 16, 17),
    rhs = c(18, 19),
    number_of_projects = 2,
    number_of_actions = 3,
    number_of_features = 1,
    number_of_branches = 10,
    number_of_allocations = 5,
    sense = c("=", "="),
    vtype = c("B", "S", "C"),
    row_ids = c("a", "b"),
    col_ids = c("d", "e", "f")
  )
  l2 <- as.list(predefined_optimization_problem(l))
  # tests
  expect_equal(l$modelsense, l2$modelsense)
  expect_equal(l$A_i, l2$A_i)
  expect_equal(l$A_j, l2$A_j)
  expect_equal(l$A_x, l2$A_x)
  expect_equal(l$obj, l2$obj)
  expect_equal(l$pwlobj, l2$pwlobj)
  expect_equal(l$lb, l2$lb)
  expect_equal(l$ub, l2$ub)
  expect_equal(l$rhs, l2$rhs)
  expect_equal(l$number_of_projects, l2$number_of_projects)
  expect_equal(l$number_of_actions, l2$number_of_actions)
  expect_equal(l$number_of_features, l2$number_of_features)
  expect_equal(l$number_of_branches, l2$number_of_branches)
  expect_equal(l$number_of_allocations, l2$number_of_allocations)
  expect_equal(l$sense, l2$sense)
  expect_equal(l$vtype, l2$vtype)
  expect_equal(l$row_ids, l2$row_ids)
  expect_equal(l$col_ids, l2$col_ids)
})
