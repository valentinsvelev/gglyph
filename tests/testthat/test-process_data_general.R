test_that("process_data_general outputs required columns", {
  skip_if_not(exists("process_data_general", mode = "function"))
  in_df <- data.frame(from = c("A","C"), to = c("C","B"))
  out <- process_data_general(in_df)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("type","x","y","x.from","y.from","x.to","y.to","label","angle") %in% names(out)))
})

test_that("process_data_general errors on invalid input", {
  expect_error(process_data_general(NULL))
})
