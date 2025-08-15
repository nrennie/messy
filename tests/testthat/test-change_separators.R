test_that("change_separators works ", {
  # Test data
  test_df <- data.frame(
    X1 = c("a b", "c_d", "e f", "g_h", "i j"),
    X2 = c("k l", "m n", "o_p", "q r", "s_t"),
    X3 = 1:5,
    stringsAsFactors = FALSE
  )

  # when cols argument is used
  result_X1 <- change_separators(test_df, cols = "X1", messiness = 1)
  expect_identical(result_X1$X2, test_df$X2)
  expect_identical(result_X1$X3, test_df$X3)

  # messiness must be a value between 0-1
  expect_error(change_separators(test_df, messiness = -0.01))
  expect_error(change_separators(test_df, messiness = 1.5))

  # invalid column names for cols
  expect_error(change_separators(test_df, cols = "test_col3"))

  # when strings remain unchanged
  test_df_noseparators <- data.frame(X1 = c("ab", "cd", "ef"))
  result_noseparators <- change_separators(test_df_noseparators, messiness = 1)
  expect_identical(result_noseparators, test_df_noseparators)
})
