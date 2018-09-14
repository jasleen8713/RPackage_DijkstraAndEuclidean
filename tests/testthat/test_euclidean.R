context("euclidean")


test_that("Wrong input throws an error.", {
  expect_error(euclidean("100", 1000))  
  expect_error(euclidean(100, "1000"))
  expect_error(euclidean(TRUE, "1000"))  
})