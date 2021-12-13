test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
my_penguins <- my_penguins %>% na.omit
test_that("output has length 2", {
  expect_equal(2, length(my_knn_cv(my_penguins[3:6], my_penguins$species, 1, 5)))
})