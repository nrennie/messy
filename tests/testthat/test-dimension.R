test_that("Dimensions unchanged", {
  expect_equal(dim(mtcars), dim(messy(mtcars)))
})
