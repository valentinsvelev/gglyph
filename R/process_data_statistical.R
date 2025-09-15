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
#' @returns A DataFrame with the preprocessed data that is to be passed to gglyph::geom_glyph().
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
    from = "from",
    to = "to",
    group = NULL,
    sig = "sig",
    thresh = 0.05
    ) {

  # -- Check function usage

  # (1) Check 'data' argument
  if (missing(data) || !is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.", call. = FALSE)
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

  # (4) Check 'sig' argument
  if (missing(sig) && !sig %in% names(data)) {
    stop("Please provide a column name for the statistical significance using the 'sig' argument.", call. = FALSE)
  }
  if (!is.character(sig) || length(sig) != 1) {
    stop("The 'sig' argument must be a single string specifying a column name.", call. = FALSE)
  }

  # (5) Check 'thresh' argument
  if (!is.numeric(thresh) || length(thresh) != 1) {
    stop("The 'thresh' argument must be a single numeric value.", call. = FALSE)
  }
  if (thresh < 0 || thresh > 1) {
    warning(paste0("The significance threshold 'thresh' is typically between 0 and 1. You provided: ", thresh), call. = FALSE)
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
