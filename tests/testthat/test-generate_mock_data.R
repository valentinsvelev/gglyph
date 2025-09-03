test_that("generate_mock_data returns a data.frame with expected cols", {
  skip_if_not(exists("generate_mock_data", mode = "function"))
  set.seed(42)
  df <- generate_mock_data(n_edges = 2L)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("type","x","y","x.from","y.from","x.to","y.to","label","angle") %in% names(df)))
  expect_gt(nrow(df), 0)
})
