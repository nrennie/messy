test_that("alter_separators works ", {
  # Test data
  test_df <- data.frame(
    X1 = c("a b", "c_d", "e f", "g_h", "i j"),
    X2 = c("k l", "m n", "o_p", "q r", "s_t"),
    X3 = 1:5,
    stringsAsFactors = FALSE)

  # strings get altered
  result_df <- alter_separators(test_df, messiness = 1)
  expect_true(all(result_df$X1 != test_df$X1 | result_df$X2 != test_df$X2))
  expect_identical(result_df$X3, test_df$X3)

  # messiness determines % of rows altered
  result_low <- alter_separators(test_df, messiness = 0.2)
  altered_count_low <- sum(result_low$X1 != test_df$X1) + sum(result_low$X2 != test_df$X2)
  expect_true(altered_count_low <= 2)  # 2  or 20% of 10 values for both cols

  result_high <- alter_separators(test_df, messiness = 0.8)
  altered_count_high <- sum(result_high$X1 != test_df$X1) + sum(result_high$X2 != test_df$X2)
  expect_true(altered_count_high >= 6)  # 80% of 10 values

  # when cols argument is used
  result_X1 <- alter_separators(test_df, cols = "X1", messiness = 1)
  expect_true(all(result_X1$X1 != test_df$X1))
  expect_identical(result_X1$X2, test_df$X2)
  expect_identical(result_X1$X3, test_df$X3)

  # messiness must be a value between 0-1
  expect_error(alter_separators(test_df, messiness = -0.01))
  expect_error(alter_separators(test_df, messiness = 1.5))

  # invalid column names for cols
  expect_error(alter_separators(test_df, cols = "test_col3"))

  # alterations applied?
  result <- alter_separators(test_df, messiness = 1)
  for (i in 1:nrow(test_df)) {
    # were spaced duplicated or replaced, or were underscores replaced with " "
    expect_true(
      grepl("  ", result$X1[i]) ||
        (grepl("_", result$X1[i]) & !grepl("_", test_df$X1[i])) ||
        (!grepl("_", result$X1[i]) & grepl("_", test_df$X1[i]))
    )
    expect_true(
      grepl("  ", result$X2[i]) ||
        (grepl("_", result$X2[i]) && !grepl("_", test_df$X2[i])) ||
        (!grepl("_", result$X2[i]) && grepl("_", test_df$X2[i]))
    )
  }

  # when strings remain unchanged
  test_df_noseparators <- data.frame(X1 = c("ab", "cd", "ef"))
  result_noseparators <- alter_separators(test_df_noseparators, messiness = 1)
  expect_identical(result_noseparators, test_df_noseparators)

})
