test_that("geom_glyph - builds a simple plot without error", {
  skip_if_not_installed("ggplot2")
  df <- gglyph::generate_mock_data()
  p <- ggplot2::ggplot(df) + gglyph::geom_glyph(data = df)
  b <- ggplot2::ggplot_build(p)
  expect_s3_class(b, "ggplot_built")
})

test_that("geom_glyph - builds a complex plot without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  df <- gglyph::generate_mock_data(n_groups = 6)
  p <- ggplot2::ggplot(df) +
    gglyph::geom_glyph(data = df, node_colour = viridis::viridis) +
    ggplot2::labs(title = "TEST") +
    ggplot2::scale_colour_manual(values = c("A" = viridis::viridis(1), "B" = viridis::viridis(2), "C" = viridis::viridis(3), "D" = viridis::viridis(4), "E" = viridis::viridis(5), "F" = viridis::viridis(6))) +
    ggplot2::scale_fill_manual(values = c("A" = viridis::viridis(1), "B" = viridis::viridis(2), "C" = viridis::viridis(3), "D" = viridis::viridis(4), "E" = viridis::viridis(5), "F" = viridis::viridis(6))) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.5))
  b <- ggplot2::ggplot_build(p)
  expect_s3_class(b, "ggplot_built")
})

test_that("geom_glyph - edge_fill defaults to edge_colour when edge_fill is NULL", {
  skip_if_not_installed("ggplot2")
  df <- gglyph::generate_mock_data()
  layers <- geom_glyph(data = df, edge_colour = "red")
  lyr_edge <- Filter(function(x) inherits(x, "LayerInstance"), layers)[[1]]
  edge_dat <- lyr_edge$data

  expect_true(all(edge_dat$color == "red"))
  expect_true(all(edge_dat$fill_internal == "red"))
})

test_that("geom_glyph - errors and warnings on invalid inputs", {
  # Setup
  skip_if_not_installed("ggplot2")
  df <- gglyph::generate_mock_data(n_nodes = 2)
  df_grouped <- gglyph::generate_mock_data(n_groups = 3)

  # < 3 nodes should error
  expect_error(ggplot2::ggplot(data = df) + gglyph::geom_glyph(), regexp = "at least 3 nodes")

  # Input DataFrame is empty or missing necessary columns
  expect_error(ggplot2::ggplot(data = data.frame()) + gglyph::geom_glyph(), regexp = "non-empty DataFrame")
  expect_error(ggplot2::ggplot(data = df %>% select(-from)) + gglyph::geom_glyph(), regexp = "'to' or 'from' columns")
  expect_error(ggplot2::ggplot(data = df %>% select(-angle)) + gglyph::geom_glyph(), regexp = "'angle' column")

  # Color warnings
  expect_warning(ggplot2::ggplot(data = df_grouped) + gglyph::geom_glyph(node_colour = c("red", "blue")), regexp = "fewer colors than groups")
  expect_warning(ggplot2::ggplot(data = df_grouped) + gglyph::geom_glyph(node_colour = c("red", "blue", "green", "black")), regexp = "more colors than groups")

  # Numeric arguments are numeric
  expect_error(ggplot2::ggplot(data = df) + geom_glyph(node_size = "big"), regexp = "are numeric")
})
