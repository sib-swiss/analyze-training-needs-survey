# Build a respondent-similarity network from long-format Likert responses to
# a set of topic questions, and detect communities with Louvain clustering.
#
# survey_long     : long-format survey data (one row per respondent x sub_question)
# meta_df         : question metadata (main_question, question_meta_type, scale)
# topic_questions : main_question stems to include as clustering features
# edge_percentile : quantile of positive pairwise correlations used as the
#                   minimum edge weight, to prune the network to its
#                   strongest connections (default: 80th percentile)
# resolution      : Louvain resolution parameter; higher values yield more,
#                   smaller communities
# seed            : random seed for the community-detection step
#
# Returns a list with the igraph object, the detected community structure,
# the correlation threshold used to prune edges, and a tibble mapping each
# respondent_id to its assigned cluster.
build_cluster_network <- function(
  survey_long,
  meta_df,
  topic_questions,
  edge_percentile = 0.8,
  resolution = 1,
  seed = 20260511
) {
  course_scale <- meta_df |>
    dplyr::filter(main_question %in% topic_questions) |>
    dplyr::pull(scale) |>
    purrr::pluck(1)

  course_matrix_tbl <- survey_long |>
    dplyr::filter(
      question_type == "likert",
      main_question %in% topic_questions,
      !is.na(sub_question),
      !is.na(answer)
    ) |>
    dplyr::mutate(
      feature = paste(main_question, sub_question, sep = " :: "),
      score = as.numeric(match(answer, course_scale))
    ) |>
    dplyr::select(respondent_id, feature, score) |>
    tidyr::pivot_wider(names_from = feature, values_from = score)

  course_matrix <- course_matrix_tbl |>
    dplyr::select(-respondent_id) |>
    as.matrix()

  # Missing values are set to 1 (the lowest score, corresponding to "Not
  # needed"), which keeps respondents with partial answers in the
  # correlation step, under the assumption that non-responses indicate a
  # lack of interest in the topic.
  course_matrix[is.na(course_matrix)] <- 1

  # Pairwise respondent correlation network. NAs are handled pairwise.
  respondent_cor <- stats::cor(t(course_matrix), use = "pairwise.complete.obs")
  diag(respondent_cor) <- 0

  # Keep only stronger positive edges to avoid a fully connected graph.
  edge_threshold <- stats::quantile(
    respondent_cor[upper.tri(respondent_cor)],
    probs = edge_percentile,
    na.rm = TRUE
  )

  adjacency <- respondent_cor
  adjacency[is.na(adjacency)] <- 0
  adjacency[adjacency < edge_threshold] <- 0

  g <- igraph::graph_from_adjacency_matrix(
    adjacency,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )

  # add respondent_id as vertex attribute for later merging with survey data
  igraph::V(g)$respondent_id <- course_matrix_tbl$respondent_id

  # louvain community detection with edge weights and resolution parameter to control cluster granularity.
  set.seed(seed)
  community <- igraph::cluster_louvain(
    g,
    weights = igraph::E(g)$weight,
    resolution = resolution
  )

  # create a tibble mapping respondent_id to cluster membership
  cluster_assignments <- tibble::tibble(
    respondent_id = igraph::V(g)$respondent_id,
    cluster = factor(paste("Cluster", igraph::membership(community)))
  )

  list(
    graph = g,
    community = community,
    edge_threshold = edge_threshold,
    cluster_assignments = cluster_assignments
  )
}

# Within-cluster interconnectedness: node/edge counts and density per Louvain
# community, for evaluating how internally similar each cluster is.
cluster_interconnectedness <- function(g, community) {
  membership_vec <- igraph::membership(community)

  sort(unique(membership_vec)) |>
    purrr::map_dfr(function(cl_id) {
      vids <- which(membership_vec == cl_id)
      subg <- igraph::induced_subgraph(g, vids = vids)

      # number of nodes and edges within the cluster
      n_nodes <- igraph::vcount(subg)
      # number of edges with both endpoints in the cluster
      internal_edges <- igraph::ecount(subg)
      # number of possible edges within the cluster (undirected, no self-loops)
      possible_edges <- n_nodes * (n_nodes - 1) / 2
      # total weight of edges within the cluster
      total_internal_weight <- if (internal_edges > 0) sum(igraph::E(subg)$weight) else 0

      tibble::tibble(
        cluster = paste("Cluster", cl_id),
        n_nodes = n_nodes,
        internal_edges = internal_edges,
        possible_edges = possible_edges,
        # proportion of possible edges that actually exist within the cluster
        edge_density = ifelse(possible_edges > 0, internal_edges / possible_edges, NA_real_),
        # average number of internal connections per node within the cluster
        mean_internal_degree = mean(igraph::degree(subg)),
        total_internal_weight = total_internal_weight,
        mean_internal_weight = ifelse(internal_edges > 0, total_internal_weight / internal_edges, NA_real_)
      )
    })
}

# Force-directed plot of the respondent network, coloured by Louvain
# community. Returns a drawing function (rather than drawing immediately) so
# the same fixed layout can be reused across multiple output devices, e.g. to
# render the same plot on-screen and to a file.
plot_cluster_network <- function(g, community, palette_fn = cvd_palette) {
  layout_fr <- igraph::layout_with_fr(g)

  membership_vec <- igraph::membership(community)
  cluster_levels <- sort(unique(paste("Cluster", membership_vec)))

  cluster_palette <- palette_fn(length(cluster_levels))
  names(cluster_palette) <- cluster_levels

  vertex_clusters <- paste("Cluster", membership_vec)
  vertex_colors <- unname(cluster_palette[vertex_clusters])

  edge_weights <- igraph::E(g)$weight
  edge_width <- scales::rescale(edge_weights, to = c(0.4, 2.5))

  function() {
    plot(
      g,
      layout = layout_fr,
      vertex.size = 4,
      vertex.label = NA,
      vertex.color = vertex_colors,
      edge.width = edge_width,
      edge.color = grDevices::adjustcolor("grey35", alpha.f = 0.35),
      margin = 0.05
    )

    legend(
      "topleft",
      legend = names(cluster_palette),
      col = unname(cluster_palette),
      pch = 16,
      pt.cex = 1.2,
      bty = "n",
      cex = 0.85
    )
  }
}
