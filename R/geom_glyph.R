# ------------------------
# --- Helper functions ---
# ------------------------

#' Generate edge
#'
#' @noRd
.generate_edge <- function(
    x1, y1, x2, y2,
    start_width = 0.05,
    end_width = 0.01,
    n = 10
) {
  t <- seq(0, 1, length.out = n)
  x_vals <- (1 - t) * x1 + t * x2
  y_vals <- (1 - t) * y1 + t * y2
  width_vals <- (1 - t) * start_width + t * end_width
  perp_angle <- atan2(y2 - y1, x2 - x1) + pi / 2
  x_upper <- x_vals + cos(perp_angle) * width_vals
  y_upper <- y_vals + sin(perp_angle) * width_vals
  x_lower <- x_vals - cos(perp_angle) * width_vals
  y_lower <- y_vals - sin(perp_angle) * width_vals
  df <- data.frame(x = c(x_upper, rev(x_lower)), y = c(y_upper, rev(y_lower)))
  return(df)
}

#' Custom logic for colour and fill in geom_glyph
#'
#' @noRd
.process_colour_arg <- function(df, colour_arg, all_groups, aesthetic_name) {
  is_grouped <- "group" %in% names(df) && !is.null(all_groups)

  if (is_grouped) { # Grouped data
    if (is.function(colour_arg)) {
      groups_sorted <- sort(all_groups, decreasing = TRUE)
      num_groups <- length(groups_sorted)
      colour_vector <- colour_arg(num_groups)
      colour_map <- setNames(colour_vector, groups_sorted)
      processed_colours <- colour_map[as.character(df$group)]
    } else if (length(colour_arg) > 1) {
      colour_vec <- rep(colour_arg, length.out = length(all_groups))
      colour_map <- setNames(colour_vec, all_groups)
      processed_colours <- colour_map[as.character(df$group)]
    } else {
      processed_colours <- rep(colour_arg, nrow(df))
    }
  } else { # Ungrouped data
    if (is.function(colour_arg)) {
      stop(paste0("Using a color function for '", aesthetic_name, "' is only supported for grouped data."), call. = FALSE)
    } else if (length(colour_arg) > 1) {
      stop(paste0("Using a color vector for '", aesthetic_name, "' is only supported for grouped data."), call. = FALSE)
    } else {
      processed_colours <- colour_arg
    }
  }
  return(processed_colours)
}

#' Custom logic for node shape in geom_glyph
#'
#' @noRd
.process_shape_arg <- function(df, shape_arg, all_groups, aesthetic_name) {
  is_grouped <- "group" %in% names(df) && !is.null(all_groups)
  if (is_grouped) {
    if (!is.null(names(shape_arg))) {
      custom_shapes <- shape_arg
      default_groups <- setdiff(all_groups, names(custom_shapes))
      default_shape_values <- c(21, 16, 17, 15, 18, 19, 20, 0)
      if (length(default_groups) > length(default_shape_values)) {
        warning("More groups than available default shapes. Shapes will be recycled.", call. = FALSE)
      }
      default_shapes_map <- setNames(rep_len(default_shape_values, length.out = length(default_groups)), default_groups)
      final_palette <- c(custom_shapes, default_shapes_map)
      processed_shapes <- final_palette[as.character(df$group)]
    } else if (is.function(shape_arg)) {
      num_groups <- length(all_groups)
      shape_vector <- shape_arg(num_groups)
      shape_map <- setNames(shape_vector, all_groups)
      processed_shapes <- shape_map[as.character(df$group)]
    } else {
      shape_vec <- rep(shape_arg, length.out = length(all_groups))
      shape_map <- setNames(shape_vec, all_groups)
      processed_shapes <- shape_map[as.character(df$group)]
    }
  } else {
    processed_shapes <- rep(shape_arg, nrow(df))
  }
  return(processed_shapes)
}


# -----------------------
# --- GGProto Objects ---
# -----------------------

#' GeomGlyphEdge ggproto object
#'
#' A custom ggproto object for drawing edges with variable widths at start and end points.
#'
#' @format A ggproto object for custom edge rendering in ggplot2.
#' @noRd
GeomGlyphEdge <- ggplot2::ggproto("GeomGlyphEdge", Geom,
                                  required_aes = c("x", "y", "xend", "yend", "fill_internal"),
                                  default_aes  = aes(
                                    colour = "grey20", fill = "grey", alpha = 1,
                                    start_width = 0.02, end_width = 0.001,
                                    shape = NA, size = 0.5
                                  ),
                                  extra_params = c(
                                    "na.rm", "key_type", "node_alpha", "counter",
                                    "node_colours", "node_fills", "node_shapes", "edge_colour", "edge_fill",
                                    "key_node_colour", "key_node_fill", "key_node_shape",
                                    "force_default_legend", "is_grouped_legend"
                                  ),

                                  # Create legend
                                  draw_key = function(data, params, size) {
                                    if (!is.null(params$counter)) {

                                      # Create the correct length of the edge(s)
                                      if ((length(unique(params$node_colours)) > 1) || (params$is_grouped_legend)) {
                                        df <- if (params$force_default_legend) {.generate_edge(0.2, 0.6, 3.5, 0.6, start_width = 0.2, end_width = 0.01)}
                                        else {.generate_edge(0.12, 0.5, 1.5, 0.5, start_width = 0.18, end_width = 0.04)}
                                      }
                                      else {
                                        df <- .generate_edge(0.2, 0.6, 3.5, 0.6, start_width = 0.2, end_width = 0.01)
                                      }

                                      # Increment the counter (needed for cases with multiple colours to iterate over them)
                                      params$counter$i <- params$counter$i + 1

                                      # If the default legend should be used (black nodes and grey edge)
                                      if (!is.null(params$force_default_legend) && params$force_default_legend) {
                                        current_node_colour <- "black"
                                        current_node_fill <- "black"
                                        current_edge_colour <- "grey"
                                        current_edge_fill <- "grey"
                                      }
                                      # If a fancier legend should be created
                                      else {
                                        # If there is only one colour used
                                        if (length(unique(params$node_colours)) == 1 || length(unique(params$node_fills)) == 1) {
                                          current_node_colour <- params$node_colours[1]
                                          current_node_fill <- params$node_fills[1]
                                          current_node_shape  <- params$node_shapes[1]

                                          current_edge_colour <- if (identical(data$colour, "grey20")) {
                                            data$fill_internal # Fallback to internal fill
                                          }
                                          else {
                                            data$colour # Use the mapped color
                                          }

                                          current_edge_fill <- if (identical(data$fill, "grey")) {
                                            data$fill_internal # Fallback to internal fill
                                          }
                                          else {
                                            data$fill # Use the mapped fill
                                          }

                                          current_edge_colour <- if (is.null(current_edge_colour)) {params$edge_colour[1]} else current_edge_colour
                                          current_edge_fill <- if (is.null(current_edge_fill)) {params$edge_fill[1]} else current_edge_fill
                                        }
                                        else {
                                          current_node_colour <- params$node_colours[params$counter$i]
                                          current_node_fill <- params$node_fills[params$counter$i]
                                          current_node_shape  <- params$node_shapes[params$counter$i]

                                          current_edge_colour <- if (identical(data$colour, "grey20")) {
                                            data$fill_internal # Fallback to internal fill
                                          }
                                          else {
                                            data$colour # Use the mapped color
                                          }

                                          current_edge_fill <- if (identical(data$fill, "grey")) {
                                            data$fill_internal # Fallback to internal fill
                                          }
                                          else {
                                            data$fill # Use the mapped fill
                                          }

                                          # Check if edge colours are NULL and apply (this is mainly for colour functions)
                                          current_edge_colour <- if (is.null(current_edge_colour)) {params$edge_colour[params$counter$i]} else current_edge_colour
                                          current_edge_fill <- if (is.null(current_edge_fill)) {params$edge_fill[params$counter$i]} else current_edge_fill
                                        }
                                      }
                                    }
                                    else {
                                      df <- .generate_edge(0.2, 0.6, 3.5, 0.6, start_width = 0.2, end_width = 0.01)
                                      current_node_colour <- params$key_node_colour
                                      current_node_fill   <- params$key_node_fill
                                      current_node_shape  <- params$key_node_shape

                                      current_edge_colour <- params$edge_colour
                                      current_edge_fill   <- params$edge_fill
                                    }

                                    edge_alpha <- params$alpha %||% 1
                                    node_alpha <- params$node_alpha %||% data$node_alpha %||% 1

                                    line_col <- scales::alpha(current_edge_colour, edge_alpha)
                                    fill_col <- scales::alpha(current_edge_fill, edge_alpha)

                                    node_line_col <- scales::alpha(current_node_colour, node_alpha)
                                    node_fill_col <- scales::alpha(current_node_fill, node_alpha)

                                    # Set the correct text colour (hide the "x" and "y" under all legend items except the last one)
                                    text_color <- ifelse(data$size == 1, "black", "white")

                                    # Create the legend items
                                    grid::grobTree(
                                      grid::polygonGrob(
                                        df$x, df$y, default.units = "npc",
                                        gp = grid::gpar(col = line_col, fill = fill_col)
                                      ),
                                      grid::pointsGrob(
                                        x = grid::unit(c(0.12, 1.5), "npc"),
                                        y = grid::unit(c(0.5, 0.5), "npc"),
                                        pch = rep_len(data$shape %||% current_node_shape %||% 21, 2),
                                        size = grid::unit(3, "mm"),
                                        gp = grid::gpar(col = node_line_col, fill = node_fill_col)
                                      ),
                                      grid::textGrob(
                                        "X",
                                        x = grid::unit(0.12, "npc"),
                                        y = grid::unit(-0.15, "npc"), # Position below the left point
                                        gp = grid::gpar(fontsize = 8, col = text_color)
                                      ),
                                      grid::textGrob(
                                        "Y",
                                        x = grid::unit(1.5, "npc"),
                                        y = grid::unit(-0.15, "npc"), # Position below the right point
                                        gp = grid::gpar(fontsize = 8, col = text_color)
                                      )
                                    )

                                  },

                                  draw_panel = function(data, panel_params, coord) {
                                    if (nrow(data) == 0) {
                                      return(grid::nullGrob())
                                    }

                                    missing_value_grey <- "grey20"
                                    default_fill_grey <- "grey"
                                    coords <- coord$transform(data, panel_params)

                                    grobs <- lapply(seq_len(nrow(coords)), function(i) {
                                      row <- coords[i, ]
                                      edge_df <- .generate_edge(row$x, row$y, row$xend, row$yend, row$start_width, row$end_width)

                                      final_outline_color <- if (identical(row$colour, missing_value_grey)) {
                                        row$fill_internal
                                      } else {
                                        row$colour
                                      }

                                      final_fill_color <- if (identical(row$fill, default_fill_grey)) {
                                        row$fill_internal
                                      } else {
                                        row$fill
                                      }

                                      grid::polygonGrob(
                                        edge_df$x, edge_df$y, default.units = "native",
                                        gp = grid::gpar(
                                          col  = scales::alpha(final_outline_color, row$alpha),
                                          fill = scales::alpha(final_fill_color, row$alpha)
                                        )
                                      )
                                    })

                                    do.call(grid::gList, grobs)
                                  }
)

#' GeomGlyphNode ggproto object
#'
#' A custom ggproto object for drawing nodes with labels at specified angles and positions.
#'
#' @format A ggproto object for custom node rendering in ggplot2.
#' @noRd
GeomGlyphNode <- ggproto("GeomGlyphNode", Geom,
                         required_aes = c("x", "y", "label", "angle"),
                         default_aes  = aes(
                           color = "grey", fill = "grey", shape = -1,
                           node_colour = "black", node_fill = "black",
                           node_alpha = 1, node_size = 6, node_shape = 21,
                           label_size = 5, hjust = 0.5, vjust = 0.5
                         ),
                         extra_params = c("na.rm", "legend_node_colour", "legend_node_fill", "node_shape_final"),

                         # Create the legend
                         draw_key = function(data, params, size) {
                           final_key_shape <- if (hasName(data, "shape") && data$shape != -1) {data$shape}
                           else {data$node_shape %||% 21}

                           final_key_shape <- if (!is.null(params$node_shape_final)) {params$node_shape_final}

                           key_alpha <- params$node_alpha %||% 1

                           final_node_colour <- params$legend_node_colour %||% data$node_colour %||% "black"
                           final_node_fill   <- params$legend_node_fill %||% data$node_fill %||% final_node_colour

                           node_line_col <- scales::alpha(final_node_colour, key_alpha)
                           node_fill_col <- scales::alpha(final_node_fill, key_alpha)

                           # Create legend elements
                           pts <- grid::pointsGrob(
                             x = c(0.2, 3.75), y = c(0.6, 0.6),
                             pch  = final_key_shape, # Use the determined shape
                             size = unit(data$node_size %||% 6, "mm") * 0.8,
                             gp   = grid::gpar(col  = node_line_col, fill = node_fill_col)
                           )
                           grid::grobTree(
                             pts,
                             grid::textGrob("X", 0.2, 0, gp = grid::gpar(fontsize = 8)),
                             grid::textGrob("Y", 3.75, 0, gp = grid::gpar(fontsize = 8))
                           )
                         },

                         # Create the panel plot
                         draw_panel = function(data, panel_params, coord) {
                           coords <- coord$transform(data, panel_params)

                           # If coords$shape is not the default, use it. Otherwise, use node_shape.
                           final_shapes <- if (hasName(coords, "shape") && any(coords$shape != -1)) {
                             coords$shape
                           } else {
                             coords$node_shape
                           }

                           grid::gList(
                             grid::pointsGrob(
                               coords$x, coords$y, pch = final_shapes, # Use the determined shapes
                               size = unit(coords$node_size, "mm"),
                               gp = grid::gpar(
                                 col  = coords$node_colour,
                                 fill = coords$node_fill %||% coords$node_colour,
                                 alpha = coords$node_alpha
                               )
                             ),
                             grid::textGrob(
                               coords$label, coords$x, coords$y,
                               hjust = coords$hjust, vjust = coords$vjust,
                               gp = grid::gpar(col = "black", fontsize = coords$label_size),
                               default.units = "native"
                             )
                           )
                         }
)


# --------------------------
# --- Main Geom Function ---
# --------------------------
#' Create a directed network-style graph
#'
#' Create a network-style graph that illustrates directed pairwise relationships using custom edges.
#'
#' @param mapping Set of aesthetic mappings created by aes(). You must supply mapping if there is no plot mapping.
#' @param data A DataFrame with preprocessed data from either gglyph::preprocess_data_general() or gglyph::preprocess_data_statistical(). To be passed to ggplot2::ggplot().
#' @param edge_size A numeric scaling factor indicating the size/width of the edges. Default is 1.
#' @param edge_colour Color(s) of the edge outlines. Can be a single string (for non-grouped data) or a vector of strings or a function (for grouped data). Default is "grey".
#' @param edge_fill Color(s) for the edge fill. Can be a single string, a vector of strings, or a color function. If NULL, defaults to edge_colour.
#' @param edge_alpha A numeric value indicating the transparency of the edges. Default is 1.
#' @param node_size A numeric value indicating the size of the nodes. Default is 8.
#' @param node_colour Color(s) of the node outlines. Can be a single string (for non-grouped data) or a vector of strings or a function (for grouped data). Default is "black".
#' @param node_fill Color for the node fill. If NULL, defaults to node_colour.
#' @param node_alpha A numeric value indicating the transparency of the nodes. Default is 1.
#' @param node_shape A numeric value specifying the shape of the nodes, following ggplot2's shape specifications. Default is 21 (a circle with a border).
#' @param node_spacing A numeric scaling factor for the distance between nodes. Values > 1 will push nodes further apart, while values < 1 will bring them closer. Default is 1.
#' @param label_size A numeric value indicating the size of the node labels. Default is 12.
#' @param group_label_size A numeric value indicating the size of group label. Default is 13.
#' @param legend_title Title for the legend as a string.
#' @param legend_subtitle Subtitle for the legend as a string.
#' @param ... Additional arguments passed to ggplot2 layer.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position A position adjustment to use on the data for this layer.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend Should this layer be included in the legends? Default is TRUE.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. Default is FALSE.
#' @return A ggplot2 layer with custom network-based graph.
#' @seealso [ggplot2::ggsave()]
#' @export
#' @examples
#' # For non-grouped/-facetted plot
#' data <- gglyph::generate_mock_data(n_groups = 1)
#'
#' ggplot2::ggplot(data = data) +
#'   gglyph::geom_glyph()
#'
#' ggplot2::ggplot(data = data) +
#'   gglyph::geom_glyph(edge_colour = "purple", node_colour = "blue")
#'
#' ggplot2::ggplot(data = data) +
#'   gglyph::geom_glyph(edge_colour = "purple", node_colour = "blue") +
#'   ggplot2::labs(title = "A beautiful glyph")
#'
#' # For grouped/facetted plot
#' data <- gglyph::generate_mock_data(n_groups = 3)
#'
#' ggplot2::ggplot(data = data) +
#'   gglyph::geom_glyph() +
#'   ggplot2::facet_wrap(~ group)
#'
#' ggplot2::ggplot(data = data) +
#'   gglyph::geom_glyph(edge_colour = viridis::viridis, node_colour = viridis::viridis) +
#'   ggplot2::facet_wrap(~ group)
#'
#' ggplot2::ggplot(data = data) +
#'   gglyph::geom_glyph(edge_colour = viridis::viridis, node_colour = viridis::viridis) +
#'   ggplot2::facet_wrap(~ group) +
#'   ggplot2::labs(title = "Beautiful glyphs")
geom_glyph <- function(
    mapping = NULL,
    data = NULL,
    edge_size = 1,
    edge_colour = "grey",
    edge_fill = NULL,
    edge_alpha = 1,
    node_size = 1,
    node_colour = "black",
    node_fill = NULL,
    node_alpha = 1,
    node_shape = 21,
    node_spacing = 1,
    label_size = 12,
    group_label_size = 13,
    legend_title = NULL,
    legend_subtitle = NULL,
    ...,
    stat = "identity",
    position = "identity",
    na.rm = FALSE,
    show.legend = TRUE,
    inherit.aes = TRUE
) {

  # Inherit data from ggplot()
  data <- data %||% ggplot2::ggplot_build(last_plot())$plot$data

  # Check function usage
  ## Data argument
  if (nrow(data) == 0) {
    stop("Please provide a non-empty DataFrame.", call. = FALSE)
  } else if (!is.data.frame(data)) {
    stop("Please provide a DataFrame object.", call. = FALSE)
  }

  ## Color argument
  if ("group" %in% names(data)) {
    n_groups <- length(unique(data$group))
    if (length(node_colour) > 1) {
      if (length(node_colour) < n_groups) {
        warning("You have provided fewer colors than groups. The colors you have provided will be repeated across the groups. To have different unique colors for each group, please provide either a string vector with length = number of groups or a color function (e.g., viridisLite::viridis).", call. = FALSE)
      } else if (length(node_colour) > n_groups) {
        warning("You have provided more colors than groups. The additional colors are not considered.", call. = FALSE)
      }
    }
  }

  ## Other arguments
  vals <- list(node_size, label_size, group_label_size, node_spacing)

  ok <- vapply(
    vals,
    function(x) is.numeric(x) && !is.character(x),
    logical(1)
  )

  if (any(!ok)) {
    stop(
      "Please make sure 'node_size', 'label_size', 'group_label_size', and ",
      "'node_spacing' are numeric (integers or decimals), not characters.",
      call. = FALSE
    )
  }

  # Add a flag for last group (for legend text "X" and "Y" in faceted plot)
  combined_mapping <- c(mapping, ggplot2::last_plot()$mapping)
  auto_guides_layer <- NULL

  if ("group" %in% names(data)) {
    # Determine the number of groups to create the override vector
    all_groups <- sort(unique(data$group))
    n_groups <- length(all_groups)

    override_size_vector <- c(rep(0, n_groups - 1), 1)

    auto_guides_layer <- guides(
      shape = guide_legend(override.aes = list(size = override_size_vector))
    )
  }

  # Split data into two separate DataFrames
  edges <- data %>%
    filter(type == "edge") %>%
    select(-type)

  node_positions <- data %>%
    filter(type == "node") %>%
    select(-type) %>%
    select_if(~ !all(is.na(.)))

  # Check that the essential node column exists
  if (!"angle" %in% names(node_positions)) {
    stop("Node data is missing the required 'angle' column.", call. = FALSE)
  }

  # Check for edge columns only if edges exist
  if (nrow(edges) > 0 && !all(c("to", "from") %in% names(edges))) {
    stop("Edge data is missing the required 'to' or 'from' columns.", call. = FALSE)
  }

  # Dynamically calculate size of nodes, edges, and node labels
  if ("group" %in% names(edges)) {
    n_groups <- length(unique(edges$group))

    # Check if parameters were explicitly provided, and only override missing ones
    base_node_size <- case_when(
      n_groups >= 7 ~ 4.0,
      n_groups >= 5 ~ 4.5,
      n_groups >= 2 ~ 5.5,
      TRUE ~ 8.0
    )

    final_node_size <- base_node_size * node_size

    if (missing(label_size)) {
      if (n_groups >= 2 && n_groups <= 4) {
        label_size <- 9
      } else if (n_groups >= 5 && n_groups <= 6) {
        label_size <- 8
      } else if (n_groups >= 7 && n_groups <= 9) {
        label_size <- 8
      } else {
        label_size <- 5
      }
    }

    if (missing(group_label_size)) {
      if (n_groups >= 2 && n_groups <= 4) {
        group_label_size <- 13
      } else if (n_groups >= 5 && n_groups <= 6) {
        group_label_size <- 12
      } else if (n_groups >= 7 && n_groups <= 9) {
        group_label_size <- 11
      } else {
        group_label_size <- 10
      }
    }
  }

  # Dynamically position the node labels
  if ("group" %in% names(edges)) {
    # Order by group
    node_positions <- node_positions[order(node_positions$group, decreasing = FALSE), ]

    # Calculate the number of nodes per group and assign hjust and vjust dynamically
    node_positions <- node_positions %>%
      group_by(group) %>%
      mutate(
        node_count = n(),
        hjust = case_when(
          node_count == 3 ~ rep(c(0.5, 0.5, 0.5), length.out = n()),
          node_count == 4 ~ rep(c(0.5, 0, 0.5, 1), length.out = n()),
          node_count == 5 ~ rep(c(0.5, 0, 0.5, 0.5, 1), length.out = n()),
          node_count == 6 ~ rep(c(0.5, 0, 0, 0.5, 1, 1), length.out = n()),
          node_count == 7 ~ rep(c(0.5, 0, 0, 0, 1, 1, 1), length.out = n()),
          node_count == 8 ~ rep(c(0.5, 0, -1.25, 0, 0.5, 1, 2.25, 1), length.out = n()),
          node_count == 9 ~ rep(c(0.5, 0, -1.25, 0, 0, 1, 1, 2.25, 1), length.out = n()),
          TRUE ~ rep(0.5, n()) # Default for more than 9 nodes
        ),
        vjust = case_when(
          node_count == 3 ~ rep(c(-1.75, 2.5, 2.5), length.out = n()),
          node_count == 4 ~ rep(c(-1.75, -1.5, 2.5, -1.5), length.out = n()),
          node_count == 5 ~ rep(c(-1.75, -1.5, 2.5, 2.5, -1.5), length.out = n()),
          node_count == 6 ~ rep(c(-1.75, -1.5, 2.5, 2.5, 2.5, -1.5), length.out = n()),
          node_count == 7 ~ rep(c(-1.75, -1.5, 2.5, 2.5, 2.5, 2.5, -1.5), length.out = n()),
          node_count == 8 ~ rep(c(-1.75, -1.5, 0.5, 2.25, 2.5, 2.25, 0.5, -1.5), length.out = n()),
          node_count == 9 ~ rep(c(-1.75, -1.5, 0.5, 2.25, 2.5, 2.5, 2.25, 0.5, -1.5), length.out = n()),
          TRUE ~ rep(-1.75, n()) # Default for more than 9 nodes
        )
      ) %>%
      ungroup()
  }
  else {
    # Handle non-grouped data
    node_count <- nrow(node_positions)

    if (node_count < 3) {
      stop("Please make sure you have at least 3 nodes to plot.")
    }

    node_positions$hjust <- case_when(
      node_count == 3 ~ rep(c(0.5, 0.5, 0.5), length.out = node_count),
      node_count == 4 ~ rep(c(0.5, 0, 0.5, 1), length.out = node_count),
      node_count == 5 ~ rep(c(0.5, 0, 0.5, 0.5, 1), length.out = node_count),
      node_count == 6 ~ rep(c(0.5, 0, 0, 0.5, 1, 1), length.out = node_count),
      node_count == 7 ~ rep(c(0.5, 0, 0, 0, 1, 1, 1), length.out = node_count),
      node_count == 8 ~ rep(c(0.5, 0, -1.45, 0, 0.5, 1, 2.45, 1), length.out = node_count),
      node_count == 9 ~ rep(c(0.5, 0, -1.45, 0, 0, 1, 1, 2.45, 1), length.out = node_count),
      TRUE ~ rep(0.5, node_count) # Default for more than 9 nodes
    )

    node_positions$vjust <- case_when(
      node_count == 3 ~ rep(c(-1.75, 2.5, 2.5), length.out = node_count),
      node_count == 4 ~ rep(c(-1.75, -1.5, 2.5, -1.5), length.out = node_count),
      node_count == 5 ~ rep(c(-1.75, -1.5, 2.5, 2.5, -1.5), length.out = node_count),
      node_count == 6 ~ rep(c(-1.75, -1.5, 2.5, 2.5, 2.5, -1.5), length.out = node_count),
      node_count == 7 ~ rep(c(-1.75, -1.5, 2.5, 2.5, 2.5, 2.5, -1.5), length.out = node_count),
      node_count == 8 ~ rep(c(-1.75, -1.5, 0.5, 2.25, 2.5, 2.25, 0.5, -1.5), length.out = node_count),
      node_count == 9 ~ rep(c(-1.75, -1.5, 0.5, 2.25, 2.5, 2.5, 2.25, 0.5, -1.5), length.out = node_count),
      TRUE ~ rep(-1.75, node_count) # Default for more than 9 nodes
    )
  }

  # Dynamically calculate x and y limits of the plot
  max_pos <- max(abs(node_positions$x), abs(node_positions$y))

  # Use a fixed padding value instead of node_spacing
  padding <- 0.5
  xlim <- c(-max_pos - padding, max_pos + padding)
  ylim <- c(-max_pos - padding, max_pos + padding)

  # Vertically center the plots / facets
  ylim[1] <- ylim[1] + 0.5
  ylim[2] <- ylim[2] - 0.25

  # Dynamically adapt legend text
  if (is.null(legend_title) && is.null(legend_subtitle)) {
    if ("significance" %in% names(edges)) {
      legend_title <- "Significance"
      legend_subtitle <- paste0("X significantly<br>exceeds Y<br>(p < ", edges$thresh[1], ")")
    } else {
      legend_title <- "Directed Pairwise<br>Relationship"
      legend_subtitle <- paste0("X exceeds Y")
    }
  }

  # Set the color(s) for the glyph components
  all_groups <- if ("group" %in% names(data)) sort(unique(data$group)) else NULL

  # If fill is specified but colour is not, make colour same as fill
  if (!missing(edge_fill) && missing(edge_colour)) {edge_colour <- edge_fill}
  if (!missing(node_fill) && missing(node_colour)) {node_colour <- node_fill}

  # If colour is specified but fill is not, make fill same as colour
  if (!missing(edge_colour) && missing(edge_fill)) {edge_fill <- edge_colour}
  if (!missing(node_colour) && missing(node_fill)) {node_fill <- node_colour}

  # Apply node spacing logic
  node_positions <- node_positions %>%
    mutate(
      x = x * node_spacing,
      y = y * node_spacing
    )

  edges <- edges %>%
    mutate(
      start_width = 0.0175 * edge_size,
      end_width = 0.001 * edge_size,
      x.from = x.from * node_spacing,
      y.from = y.from * node_spacing,
      x.to = x.to * node_spacing,
      y.to = y.to * node_spacing,
      color = .process_colour_arg(., edge_colour, all_groups, "edge_colour"),
      fill_internal = if (is.null(edge_fill)) color else .process_colour_arg(., edge_fill, all_groups, "edge_fill")
    )

  # Process node colors
  node_positions$node_colour_processed <- .process_colour_arg(node_positions, node_colour, all_groups, "node_colour")
  if (is.null(node_fill)) {node_positions$node_fill_processed <- node_positions$node_colour_processed}
  else {node_positions$node_fill_processed <- .process_colour_arg(node_positions, node_fill, all_groups, "node_fill")}

  # Process node shapes
  node_positions$node_shape_processed <- .process_shape_arg(node_positions, node_shape, all_groups, "node_shape")

  # Define custom legend box positioning logic
  if ("group" %in% names(data) && exists("n_groups")) {
    if (n_groups <= 3) {legend_box_pos <- margin(l = 20, b = 20)}
    else if (n_groups >= 4 && n_groups <= 6) {legend_box_pos <- margin(l = 20, b = 20)}
    else if (n_groups >= 7 && n_groups <= 9) {legend_box_pos <- margin(l = 20, b = 20)}
    else {legend_box_pos <- margin(l = 20, b = 20)}
  }
  else {legend_box_pos <- margin(r = 20)}

  # Define theme
  plot_theme <- theme_void() +
    theme(
      legend.title = ggtext::element_markdown(color = "black"),
      strip.text = element_text(size = group_label_size, margin = margin(b = 12)),
      panel.spacing = unit(1.5, "lines"),
      plot.margin = margin(10, 20, 30, 20),
      legend.box.margin = legend_box_pos,
      legend.text = element_text(hjust = 0, margin = margin(l = 18, unit = "pt"))
    )

  if ("group" %in% names(data)) {plot_theme <- plot_theme + theme(plot.title = element_text(margin = margin(b = 20)))}

  # Build the edge mapping
  colour_mapped <- tryCatch({
    last <- ggplot2::last_plot()
    "colour" %in% names(last$mapping) ||
      "color"  %in% names(last$mapping)
  }, error = function(e) FALSE) ||
    ("colour" %in% names(list(...)) || "color" %in% names(list(...)))

  # Initialize a counter for the legend colouring
  if ("group" %in% names(data)) {
    counter_env <- new.env()
    counter_env$i <- 0 # Initialize counter at zero

    # Create ordered vectors of node properties for the legend
    all_groups <- sort(unique(data$group))
    group_df <- data.frame(group = all_groups)
    node_colour_vector <- .process_colour_arg(group_df, node_colour, all_groups, "node_colour")
    node_fill_vector <- .process_colour_arg(group_df, node_fill, all_groups, "node_fill")
    node_shape_vector <- .process_shape_arg(group_df, node_shape, all_groups, "node_shape")
  }

  # Build edge mapping
  edge_map <- aes(
    x = x.from,  y = y.from,
    xend = x.to, yend = y.to,
    start_width = start_width,
    end_width   = end_width,
    fill_internal = fill_internal
  )

  # This dummy mapping creates a legend with an entry for each group
  if ("group" %in% names(data)) {
    edge_map$alpha <- quote(group)
  }

  # Adjust colour for the edges
  if (!colour_mapped) {edge_map$colour <- quote(I(color))}

  # Check if we should use default legends or the custom one
  used_manual_scale <- any(c("colour", "color", "fill", "shape") %in% names(combined_mapping))
  is_significance_key <- (!used_manual_scale)
  col <- node_positions$node_colour_processed

  node_colour_map <- setNames(
    node_positions$node_colour_processed,
    node_positions$group
  )

  # Ensure the map has only unique group-color pairs
  unique_node_colour_map <- node_colour_map[!duplicated(names(node_colour_map))]

  # Create flag for forcing default legend
  custom_color_function_used <- is.function(node_colour) || is.function(node_fill) || is.function(edge_colour) || is.function(edge_fill)
  default_colours_used <- if (!custom_color_function_used && length(unique(node_positions$node_colour_processed)) == 1) {(unique(node_positions$node_colour_processed) == "black" && unique(node_positions$node_fill_processed) == "black" && unique(edges$color) == "grey" && unique(edges$fill_internal) == "grey")} else {FALSE}
  manual_scale_and_color_function_used <- used_manual_scale && custom_color_function_used
  force_default_key <- ((custom_color_function_used && default_colours_used) && manual_scale_and_color_function_used) || (custom_color_function_used && !used_manual_scale) || default_colours_used

  # Create list with params for the edges
  if ("group" %in% names(data)) { # For grouped data
    edge_params <- c(list(
      na.rm = FALSE,
      alpha = edge_alpha,
      node_alpha = node_alpha,
      counter = counter_env,
      node_colours = node_colour_vector,
      node_fills = node_fill_vector,
      node_shapes = node_shape_vector,
      edge_colour = .process_colour_arg(group_df, edge_colour, all_groups, "edge_colour"),
      edge_fill = .process_colour_arg(group_df, edge_fill %||% edge_colour, all_groups, "edge_fill"),
      force_default_legend = force_default_key,
      is_grouped_legend = used_manual_scale && !is.null(data$group)
    ),
    list(...)
    )
  }
  else {
    # Original params for non-grouped data
    edge_params <- c(list(
      na.rm = FALSE,
      alpha = edge_alpha,
      node_alpha = node_alpha,
      edge_colour = edge_colour,
      edge_fill = edge_fill %||% edge_colour, # Ensure fill is not NULL

      key_node_colour = node_colour %||% "black",
      key_node_fill   = node_fill %||% node_colour %||% "black",
      key_node_shape  = node_shape %||% 21
    ),
    list(...))
  }

  if (is_significance_key) edge_params$key_type <- "significance"

  # Custom mapping
  final_mapping_list <- c(mapping, edge_map)

  # Safely determine the legend node color, avoiding passing a function
  final_legend_node_colour <- if (is.function(node_colour)) {
    "black" # Use a default placeholder as GeomGlyphEdge controls the actual legend color
  }
  else {
    node_colour
  }

  # Safely determine the legend node fill
  final_legend_node_fill <- if (is.function(node_fill)) {
    final_legend_node_colour # Use the same default
  }
  else {
    node_fill %||% node_colour
  }

  # Define the core layers of the new geom
  core_layers <- list(
    layer(
      geom = GeomGlyphEdge,
      mapping = do.call(aes, final_mapping_list), # As aes object
      data = edges,
      stat = stat, position = position,
      show.legend = TRUE,
      inherit.aes = TRUE,
      params = edge_params
    ),

    layer(
      geom = GeomGlyphNode,
      mapping = aes(
        x = x, y = y, label = label, angle = angle,
        colour      = I("black"),
        node_colour = I(node_colour_processed),
        node_fill   = I(node_fill_processed),
        node_size   = if (exists("final_node_size")) final_node_size else 9 * node_size,
        node_shape  = I(node_shape_processed),
        label_size  = label_size, hjust = hjust, vjust = vjust
      ),
      data = node_positions,
      stat = stat, position = position,
      show.legend = if (used_manual_scale) FALSE else show.legend,
      inherit.aes = inherit.aes,
      params = list(
        node_alpha = node_alpha,
        legend_node_colour = final_legend_node_colour,
        legend_node_fill = final_legend_node_fill,
        node_shape_final = unique(node_positions$node_shape_processed)
      )
    ),
    plot_theme,
    coord_fixed(xlim = xlim, ylim = ylim, clip = "off")
  )

  # Create custom legend
  if (used_manual_scale) {
    # If aesthetics are mapped, return only the core layers ggplot2 will automatically create the default legends
    final_legend_title <- paste0("<span style='font-size:11pt'>", legend_title, "</span><br><span style='font-size:8.5pt;'>", legend_subtitle, "</span>")
    legend_layers <- list(labs(colour = final_legend_title, fill = final_legend_title, shape = final_legend_title, size = final_legend_title))
    return(c(core_layers, legend_layers, auto_guides_layer))
  }
  else {
    # If no aesthetics are mapped, add the custom legend
    custom_legend_layers <- list(
      geom_blank(aes(linetype = "dummy")),
      guides(
        linetype = guide_legend(title = paste0("<span style='font-size:11pt'>", legend_title, "</span><br><span style='font-size:8.5pt;'>", legend_subtitle, "</span>"), label = FALSE, order = 1),
        alpha = "none", colour = "none", fill = "none", shape = "none", size = "none" # Hide all other potential legends
      )
    )
    return(c(core_layers, custom_legend_layers, auto_guides_layer))
  }
}
