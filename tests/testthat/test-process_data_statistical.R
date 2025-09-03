test_that("process_data_statistical adds significance when applicable", {
  skip_if_not(exists("process_data_statistical", mode = "function"))
  in_df <- data.frame(from = c("A","C"), to = c("C","B"), p = c(0.01, 0.2))
  out <- process_data_statistical(in_df, thresh = 0.05)
  expect_true("significance" %in% names(out))
})
