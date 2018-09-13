# tests the dijkstra function
context("dijkstra")

test_that("User Input Validation", {
  expect_error(dijkstra("a","b"))
})
