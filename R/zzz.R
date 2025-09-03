#' @importFrom dplyr filter select group_by mutate ungroup case_when n where bind_rows rename select_if
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggproto Geom aes layer coord_fixed theme_void theme
#' @importFrom ggplot2 element_text margin guides guide_legend geom_blank labs
#' @importFrom ggplot2 last_plot ggplot_build
#' @importFrom grid grobTree polygonGrob pointsGrob textGrob unit gpar nullGrob gList
#' @importFrom ggtext element_markdown
#' @importFrom rlang .data `%||%` sym
#' @importFrom stats setNames runif complete.cases
#' @importFrom utils combn
NULL

# Optional package env for internal caches (if you need one)
.gglyph <- new.env(parent = emptyenv())

# Called when the namespace is loaded (good for setting default options)
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.gglyph <- list(
    gglyph.verbose = FALSE
  )
  toset <- !(names(op.gglyph) %in% names(op))
  if (any(toset)) options(op.gglyph[toset])
  invisible()
}

# Silence R CMD check notes from tidy-eval / column names
utils::globalVariables(c(
  ".", "type", "group", "x", "y", "x.from", "y.from", "x.to", "y.to",
  "color", "start_width", "end_width", "fill_internal", "label", "angle",
  "node_colour_processed", "node_fill_processed", "node_shape_processed",
  "hjust", "vjust", "significance"
))
