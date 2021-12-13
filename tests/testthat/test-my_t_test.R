test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("error occurs with invalid alternative",{
  expect_error(my_t_test(rnorm(20), "one.sided", 0))
})

test_that("length of output is 4",{
  expect_equal(length(my_t_test(rnorm(20), "two.sided", 0)),4)
})

test_that("length of output is 4",{
  expect_equal(length(my_t_test(rnorm(20), "less", 0)),4)
})

test_that("length of output is 4",{
  expect_equal(length(my_t_test(rnorm(20), "greater", 0)),4)
})