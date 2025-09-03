#' Generate mock data for gglyph::geom_glyph()
#'
#' Generates custom mock data to be passed to gglyph::geom_glyph().
#'
#' @param n_nodes Number of nodes in the graph.
#' @param n_edges Number of edges to generate.
#' @param n_groups Number of groups (for faceting).
#' @param statistical If TRUE, generates mock p-values for edges.
#' @param p_threshold The significance threshold for filtering edges.
#' @return A data frame with mock data for nodes and edges.
#' @export
#' @examples
#' ########################
#' # For non-grouped data #
#' ########################
#' 
#' mock_data <- generate_mock_data(
#'   n_nodes = 5,
#'   n_edges = 7, 
#'   n_groups = 1, 
#'   statistical = FALSE,
#'   p_threshold = 0.05
#' )
#' 
#' ####################
#' # For grouped data #
#' ####################
#' 
#' mock_data <- generate_mock_data(
#'   n_nodes = 5,
#'   n_edges = 7, 
#'   n_groups = 3, 
#'   statistical = TRUE,
#'   p_threshold = 0.05
#' )
generate_mock_data <- function(
    n_nodes = 5,
    n_edges = 7,
    n_groups = 1,
    statistical = FALSE,
    p_threshold = 0.05
) {
  all_groups_data <- list()
  start_angle <- pi / 2
  
  for (g in 1:n_groups) {
    node_pool <- LETTERS[1:n_nodes]
    
    # Calculate the maximum number of unique edges possible.
    max_possible_edges <- choose(n_nodes, 2)
    
    # Check if the requested number of edges exceeds the maximum.
    if (n_edges > max_possible_edges) {
      #warning(paste("Requested n_edges (", n_edges, ") is greater than the maximum possible (", max_possible_edges, "). Using max_possible_edges instead."), call. = FALSE)
      n_edges <- max_possible_edges
    }
    
    # Generate all unique combinations of 2 nodes
    all_unique_edges <- t(combn(node_pool, 2))
    
    # Sample the desired number of edges from the pool of unique edges.
    sampled_indices <- sample(1:nrow(all_unique_edges), n_edges)
    edges <- tibble(
      from = all_unique_edges[sampled_indices, 1],
      to = all_unique_edges[sampled_indices, 2]
    )
    
    # Add statistical values
    if (statistical) {edges$significance <- runif(n_edges, p_threshold - 0.05, p_threshold + 0.05)} 
    #else {edges$significance <- NA_real_}
    
    angles <- seq(start_angle, start_angle - 2 * pi, length.out = n_nodes + 1)[-n_nodes - 1]
    
    node_positions <- tibble(
      label = node_pool,
      angle = angles,
      x = cos(angles),
      y = sin(angles)
    )
    
    if (n_groups > 1) {
      group_label <- paste("Group", g)
      edges$group <- group_label
      node_positions <- node_positions %>% mutate(group = group_label)
    }
    
    # Apply statistical filtering if enabled
    if (statistical) {
      edges <- edges %>% filter(significance < p_threshold)
    }
    
    if (nrow(edges) == 0) {merged_edges <- tibble()} 
    else {
      if (statistical) {
        edges$threshold <- p_threshold
      }
      
      if (n_groups > 1) {
        merged_edges <- merge(edges, node_positions, by.x = c("from", "group"), by.y = c("label", "group"), all = TRUE)
        merged_edges <- merge(merged_edges, node_positions, by.x = c("to", "group"), by.y = c("label", "group"), suffixes = c(".from", ".to"), all = TRUE)
      }
      else {
        merged_edges <- merge(edges, node_positions, by.x = "from", by.y = "label", all = TRUE)
        merged_edges <- merge(merged_edges, node_positions, by.x = "to", by.y = "label", suffixes = c(".from", ".to"), all = TRUE)
      }
      
      required_cols <- c("x.from", "y.from", "x.to", "y.to")
      merged_edges <- merged_edges[complete.cases(merged_edges[, required_cols]), ]
      if (nrow(merged_edges) > 0) {merged_edges$type <- "edge"}
    }
    
    node_data <- node_positions %>% mutate(type = "node")
    group_data <- bind_rows(merged_edges, node_data)
    all_groups_data[[g]] <- group_data
  }
  
  bind_rows(all_groups_data)
}