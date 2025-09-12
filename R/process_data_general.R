#' Process general/non-statistical data
#'
#' Prepare general/non-statistical data for plotting with gglyph::geom_glyph().
#'
#' @param data A DataFrame or tibble containing the input data to be processed.
#' @param from A string indicating the column name for the start nodes.
#' @param to A string indicating the column name for the end nodes.
#' @param group A string indicating the column name for the grouping variable.
#' @return A DataFrame with the preprocessed data that is to be passed to gglyph::geom_glyph().
#' @export
#' @examples
#' \donttest{
#' ####################
#' # Create mock data #
#' ####################
#'
#' data <- data.frame(
#'   from = sample(LETTERS, 5),
#'   to = sample(LETTERS, 5),
#'   group = sample(LETTERS, 5)
#' )
#'
#' ########################
#' # For non-grouped data #
#' ########################
#'
#' processed_data <- process_data_general(
#'   data = data,
#'   from = "from",
#'   to = "to"
#' )
#'
#' ####################
#' # For grouped data #
#' ####################
#'
#' processed_data <- process_data_general(
#'   data = data,
#'   from = "from",
#'   to = "to",
#'   group = "group"
#' )
#' }
process_data_general <- function(
    data,
    from,
    to,
    group = NULL
    ) {

  # Check function usage
  if (missing(data) || !is.data.frame(data)) {
    stop("Please provide a DataFrame.", call. = FALSE)
  }

  if (missing(from) || !from %in% names(data) || !is.character(from)) {
    stop("Please provide a valid column name (as a string) from your DataFrame containing the start nodes.", call. = FALSE)
  }

  if (missing(to) || !to %in% names(data) || !is.character(to)) {
    stop("Please provide a valid column name (as a string) from your DataFrame containing the end nodes.", call. = FALSE)
  }

  # Preprocess data based on whether grouping is applied
  if (!is.null(group)) {
    # Rename columns for consistency
    data <- data %>%
      rename(from = !!sym(from), to = !!sym(to), group = !!sym(group))

    # Identify unique nodes and group
    unique_nodes <- sort(unique(c(data$from, data$to)))
    all_groups <- unique(data$group)

    # Create node positions based on circular layout
    n <- length(unique_nodes)
    start_angle <- pi / 2
    angles <- seq(start_angle, start_angle - 2 * pi, length.out = n + 1)[-n - 1]
    node_positions <- data.frame(label = unique_nodes, angle = angles, x = cos(angles), y = sin(angles)) %>%
      tidyr::expand_grid(group = all_groups)

    # Merge edge data with node positions and filter out NA rows
    edges <- merge(data, node_positions, by.x = c("from", "group"), by.y = c("label", "group"), all = TRUE)
    edges <- merge(edges, node_positions, by.x = c("to", "group"), by.y = c("label", "group"), suffixes = c(".from", ".to"), all = TRUE)
    edges <- edges[complete.cases(edges),]

  } else {
    # Rename columns for consistency
    data <- data %>%
      rename(from = !!sym(from), to = !!sym(to))

    # Identify unique nodes
    unique_nodes <- sort(unique(c(data$from, data$to)))

    # Create node positions based on circular layout
    n <- length(unique_nodes)
    start_angle <- pi / 2
    angles <- seq(start_angle, start_angle - 2 * pi, length.out = n + 1)[-n - 1]
    node_positions <- data.frame(label = unique_nodes, angle = angles, x = cos(angles), y = sin(angles))

    # Merge edge data with node positions and filter out NA rows
    edges <- merge(data, node_positions, by.x = "from", by.y = "label", all = TRUE)
    edges <- merge(edges, node_positions, by.x = "to", by.y = "label", suffixes = c(".from", ".to"), all = TRUE)
    edges <- edges[complete.cases(edges),]
  }

  # Add identifier to each DataFrame
  edges <- edges %>% mutate(type = "edge")
  node_positions <- node_positions %>% mutate(type = "node")

  # Combine the DataFrames and return it
  combined_df <- dplyr::bind_rows(edges, node_positions)

  return(combined_df)
}
