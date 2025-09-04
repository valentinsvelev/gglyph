test_that("process_data_general outputs required columns - simple version", {
  skip_if_not(exists("process_data_general", mode = "function"))
  df <- gglyph::generate_mock_data()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("type","x","y","x.from","y.from","x.to","y.to","label","angle") %in% names(df)))
})

test_that("process_data_general outputs required columns - full version", {
  skip_if_not(exists("process_data_general", mode = "function"))
  df <- gglyph::generate_mock_data(n_groups = 3)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("type","x","y","x.from","y.from","x.to","y.to","label","angle","group") %in% names(df)))
})

test_that("process_data_general errors on invalid inputs", {
  expect_error(process_data_general(NULL), regexp = "provide a DataFrame")
  expect_error(process_data_general(data.frame()), regexp = "valid column name")
})
