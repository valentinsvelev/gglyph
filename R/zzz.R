#' @importFrom rlang .data
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
