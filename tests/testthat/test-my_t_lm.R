test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("output is a model", {
  expect_equal(ncol(my_lm(bill_length_mm~bill_depth_mm,my_penguins)), 4)
})
