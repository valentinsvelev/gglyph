test_that("generate_mock_data returns a data.frame with expected cols - simple version", {
  skip_if_not(exists("generate_mock_data", mode = "function"))
  set.seed(42)
  df <- generate_mock_data(n_edges = 2)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("type","x","y","x.from","y.from","x.to","y.to","label","angle") %in% names(df)))
  expect_gt(nrow(df), 0)
})

test_that("generate_mock_data returns a data.frame with expected cols - full version", {
  skip_if_not(exists("generate_mock_data", mode = "function"))
  set.seed(42)
  df <- generate_mock_data(n_edges = 10, n_nodes = 10, n_groups = 5, statistical = TRUE, p_threshold = 0.05)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("type","x","y","x.from","y.from","x.to","y.to","label","angle", "group", "significance") %in% names(df)))
  expect_gt(nrow(df), 0)
})

test_that("generate_mock_data errors on invalid inputs", {
  expect_error(generate_mock_data(n_nodes = "error"), regexp = "correct input types")
  expect_error(generate_mock_data(n_edges = "error"), regexp = "correct input types")
  expect_error(generate_mock_data(n_groups = "error"), regexp = "correct input types")
  expect_error(generate_mock_data(statistical = "error"), regexp = "correct input types")
  expect_error(generate_mock_data(p_threshold = "error"), regexp = "correct input types")
})
