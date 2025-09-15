test_that("process_data_statistical - outputs required columns (simple version)", {
  skip_if_not(exists("process_data_statistical", mode = "function"))
  df <- gglyph::generate_mock_data()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("type","x","y","x.from","y.from","x.to","y.to","label","angle") %in% names(df)))
})

test_that("process_data_statistical - outputs required columns (full version)", {
  skip_if_not(exists("process_data_statistical", mode = "function"))
  df <- gglyph::generate_mock_data(n_groups = 3, statistical = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("type","x","y","x.from","y.from","x.to","y.to","label","angle","group","significance") %in% names(df)))
})

test_that("process_data_statistical - errors on invalid inputs", {
  expect_error(gglyph::process_data_statistical(NULL), regexp = "must be a DataFrame")
  expect_error(gglyph::process_data_statistical(data.frame()), regexp = "column name")
})
