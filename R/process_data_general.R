#' Process general/non-statistical data
#'
#' Prepare general/non-statistical data for plotting with gglyph::geom_glyph().
#'
#' @param data A DataFrame or tibble containing the input data to be processed.
#' @param from A string indicating the column name for the start nodes.
#' @param to A string indicating the column name for the end nodes.
#' @param group A string indicating the column name for the grouping variable.
#' @returns A DataFrame with the preprocessed data that is to be passed to gglyph::geom_glyph().
#' @export
#' @examples
#' data(sipri_milex_1995_2023)
#'
#' # For non-grouped data
#' processed_data <- process_data_general(
#'   data = sipri_milex_1995_2023,
#'   from = "from",
#'   to = "to"
#' )
#'
#' # For grouped data
#' processed_data <- process_data_general(
#'   data = sipri_milex_1995_2023,
#'   from = "from",
#'   to = "to",
#'   group = "group"
#' )
process_data_general <- function(
    data,
    from = "from",
    to = "to",
    group = NULL
    ) {

  # -- Check function usage

  # (1) Check 'data' argument
  if (missing(data) || !is.data.frame(data) || is.null(data)) {
    stop("The 'data' argument must be a DataFrame.", call. = FALSE)
  }

  # (2) Check 'from' argument (start nodes)
  if (missing(from) && !from %in% names(data)) {
    stop("Please provide a column name for the start nodes using the 'from' argument.", call. = FALSE)
  }
  if (!is.character(from) || length(from) != 1) {
    stop("The 'from' argument must be a single string specifying a column name.", call. = FALSE)
  }

  # (3) Check 'to' argument (end nodes)
  if (missing(to) && !to %in% names(data)) {
    stop("Please provide a column name for the end nodes using the 'to' argument.", call. = FALSE)
  }
  if (!is.character(to) || length(to) != 1) {
    stop("The 'to' argument must be a single string specifying a column name.", call. = FALSE)
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

  # Remove group variable if it exists
  if (is.null(group) & "group" %in% names(combined_df)) {combined_df$group <- NULL}

  return(combined_df)
}
