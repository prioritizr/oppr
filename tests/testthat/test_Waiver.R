test_that("works", {
  # constructor
  i <- new_waiver()
  # methods
  i
  print(i)
  # run tests
  expect_true(inherits(i, "Waiver"))
  expect_true(is.Waiver(i))
})
