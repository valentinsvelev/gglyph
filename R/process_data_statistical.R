#' Process statistical data
#'
#' Prepare statistical data for plotting with gglyph::geom_glyph().
#'
#' @param data A DataFrame or tibble containing the input data to be processed.
#' @param from A string indicating the column name for the start nodes.
#' @param to A string indicating the column name for the end nodes.
#' @param group A string indicating the column name for the grouping variable.
#' @param sig A string indicating the column name for the significance level.
#' @param thresh A single number indicating the significance threshold. Default is 0.05.
#' @return A DataFrame with the preprocessed data that is to be passed to gglyph::geom_glyph().
#' @export
#' @examples
#' data(pisa_2022)
#'
#' # For non-grouped data
#' processed_data <- process_data_statistical(
#'   data = pisa_2022,
#'   from = "from",
#'   to = "to",
#'   sig = "sig",
#'   thresh = 0.05
#' )
#'
#' # For grouped data
#' processed_data <- process_data_statistical(
#'   data = pisa_2022,
#'   from = "from",
#'   to = "to",
#'   sig = "sig",
#'   group = "group",
#'   thresh = 0.05
#' )
process_data_statistical <- function(
    data,
    from,
    to,
    group = NULL,
    sig,
    thresh = 0.05
    ) {

  # Check function usage
  if (missing(data) || !is.data.frame(data)) {
    stop("Please provide a DataFrame.", call. = FALSE)
  }

  if (missing(from) || !from %in% names(data) || !is.character(from)) {
    stop("Please provide a valid column name (as a single string) from your DataFrame containing the start nodes.", call. = FALSE)
  }

  if (missing(to) || !to %in% names(data) || !is.character(to)) {
    stop("Please provide a valid column name (as a single string) from your DataFrame containing the end nodes.", call. = FALSE)
  }

  if (missing(sig) || !sig %in% names(data) || !is.character(sig)) {
    stop("Please provide a valid column name (as a single string) from your DataFrame containing the statistical significances.", call. = FALSE)
  }

  if (!is.numeric(thresh)) {
    stop("Please provide decimal number that indicates the chosen significance threshold.", call. = FALSE)
  }

  # Preprocess data based on whether grouping is applied
  if (!is.null(group)) {
    # Rename columns for consistency
    data <- data %>%
      rename(from = !!sym(from), to = !!sym(to), significance = !!sym(sig), group = !!sym(group))

    # Identify unique nodes and group
    unique_nodes <- sort(unique(c(data$from, data$to)))
    all_groups <- unique(data$group)

    # Create node positions based on circular layout
    n <- length(unique_nodes)
    start_angle <- pi / 2
    angles <- seq(start_angle, start_angle - 2 * pi, length.out = n + 1)[-n - 1]
    node_positions <- data.frame(label = unique_nodes, angle = angles, x = cos(angles), y = sin(angles)) %>%
      tidyr::expand_grid(group = all_groups)

    # Merge edge data with node positions and filter by significance
    edges <- merge(data, node_positions, by.x = c("from", "group"), by.y = c("label", "group"), all = TRUE)
    edges <- merge(edges, node_positions, by.x = c("to", "group"), by.y = c("label", "group"), suffixes = c(".from", ".to"), all = TRUE)
    edges <- edges[complete.cases(edges),] %>% filter(significance < thresh)

  } else {
    # Rename columns for consistency
    data <- data %>%
      rename(from = !!sym(from), to = !!sym(to), significance = !!sym(sig))

    # Identify unique nodes
    unique_nodes <- sort(unique(c(data$from, data$to)))

    # Create node positions based on circular layout
    n <- length(unique_nodes)
    start_angle <- pi / 2
    angles <- seq(start_angle, start_angle - 2 * pi, length.out = n + 1)[-n - 1]
    node_positions <- data.frame(label = unique_nodes, angle = angles, x = cos(angles), y = sin(angles))

    # Merge edge data with node positions and filter by significance
    edges <- merge(data, node_positions, by.x = "from", by.y = "label", all = TRUE)
    edges <- merge(edges, node_positions, by.x = "to", by.y = "label", suffixes = c(".from", ".to"), all = TRUE)
    edges <- edges[complete.cases(edges),] %>% filter(significance < thresh)
  }

  # Add identifier to each DataFrame
  edges <- edges %>% mutate(type = "edge")
  node_positions <- node_positions %>% mutate(type = "node")

  # Add significance level for geom_glyph()
  edges <- edges %>% mutate(threshold = thresh)

  # Combine the DataFrames and return it
  combined_df <- dplyr::bind_rows(edges, node_positions)

  # Remove group variable if it exists
  if (is.null(group) & "group" %in% names(combined_df)) {combined_df$group <- NULL}

  return(combined_df)
}
