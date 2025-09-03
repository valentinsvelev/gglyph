test_that("geom_glyph builds a plot without error", {
  skip_if_not_installed("ggplot2")
  df <- gglyph_test_data
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + gglyph::geom_glyph(data = df)
  b <- ggplot2::ggplot_build(p)
  expect_s3_class(b, "ggplot_built")
})

test_that("geom_glyph validates inputs", {
  # < 3 nodes should error (based on your checks)
  small_nodes <- subset(gglyph_test_data, type == "node")[1:2, ]
  df_bad <- rbind(small_nodes, subset(gglyph_test_data, type == "edge"))
  expect_error(gglyph::geom_glyph(data = df_bad), "at least 3 nodes", ignore.case = TRUE)
})
