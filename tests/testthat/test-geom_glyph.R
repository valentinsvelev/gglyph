test_that("geom_glyph builds a plot without error", {
  skip_if_not_installed("ggplot2")
  df <- generate_mock_data()
  p <- ggplot2::ggplot(df) + gglyph::geom_glyph(data = df)
  b <- ggplot2::ggplot_build(p)
  expect_s3_class(b, "ggplot_built")
})

test_that("geom_glyph errors on invalid inputs", {
  # < 3 nodes should error
  df_bad <- generate_mock_data(n_nodes = 2)
  expect_error(gglyph::geom_glyph(data = df_bad), regexp = "at least 3 nodes")
})
