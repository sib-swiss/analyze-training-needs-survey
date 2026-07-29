# Build a respondent-similarity network from long-format Likert responses to
# a set of topic questions, and detect communities via Leiden or Louvain
# clustering.
#
# survey_long     : long-format survey data (one row per respondent x sub_question)
# meta_df         : question metadata (main_question, question_meta_type, scale)
# topic_questions : main_question stems to include as clustering features
# edge_percentile : quantile of positive pairwise correlations used as the
#                   minimum edge weight, to prune the network to its
#                   strongest connections (default: 80th percentile)
# resolution      : resolution parameter passed to the community-detection
#                   algorithm; higher values yield more, smaller communities
# algorithm       : "leiden" (default) or "louvain". Leiden optimizes the same
#                   modularity objective but guarantees internally connected
#                   communities, which Louvain does not; Louvain remains
#                   available for users who want to compare the two.
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
  algorithm = c("leiden", "louvain"),
  seed = 20260511
) {
  algorithm <- match.arg(algorithm)
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

  # community detection with edge weights and resolution parameter to control cluster granularity.
  set.seed(seed)
  community <- if (algorithm == "leiden") {
    igraph::cluster_leiden(
      g,
      objective_function = "modularity",
      weights = igraph::E(g)$weight,
      resolution = resolution
    )
  } else {
    igraph::cluster_louvain(
      g,
      weights = igraph::E(g)$weight,
      resolution = resolution
    )
  }

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

# Resampling stability of the clustering: repeatedly subsample respondents
# (without replacement), recluster each subsample from scratch with the same
# parameters, and compare its cluster assignments to the reference (full-data)
# clustering restricted to the same respondents. The same seed is reused for
# community detection on every iteration, so subsampling is the only source
# of variation being measured — Louvain's own run-to-run randomness is a
# separate question from whether the profiles are stable to which
# respondents happen to be included.
#
# Returns two tibbles:
#   - ari: one global Adjusted Rand Index per iteration (1 = identical
#     partitions; ~0 = agreement no better than chance) — an overall
#     stability score for the full partition.
#   - purity: for each reference cluster and iteration, the fraction of its
#     (sampled) members that end up sharing a single most-common cluster
#     label in the subsample's own clustering — which reference clusters
#     are individually more or less reproducible than the global ARI alone
#     would suggest.
#
# reference_assignments : cluster_assignments tibble from a build_cluster_network()
#                          call on the full respondent pool
# subsample_frac         : fraction of respondents kept per iteration
# n_iterations           : number of resampling iterations
cluster_stability <- function(
  survey_long,
  meta_df,
  topic_questions,
  reference_assignments,
  edge_percentile = 0.8,
  resolution = 1,
  algorithm = c("leiden", "louvain"),
  subsample_frac = 0.8,
  n_iterations = 100,
  seed = 20260511
) {
  algorithm <- match.arg(algorithm)
  set.seed(seed)
  all_ids <- reference_assignments$respondent_id
  n_sample <- floor(subsample_frac * length(all_ids))

  results <- purrr::map(seq_len(n_iterations), function(i) {
    sampled_ids <- sample(all_ids, size = n_sample)

    survey_long_sub <- survey_long |>
      dplyr::filter(respondent_id %in% sampled_ids)

    result_sub <- build_cluster_network(
      survey_long_sub,
      meta_df,
      topic_questions,
      edge_percentile = edge_percentile,
      resolution = resolution,
      algorithm = algorithm,
      seed = seed
    )

    sub_assignments <- result_sub$cluster_assignments
    ref_matched <- reference_assignments$cluster[
      match(sub_assignments$respondent_id, reference_assignments$respondent_id)
    ]

    ari <- igraph::compare(
      as.integer(ref_matched),
      as.integer(sub_assignments$cluster),
      method = "adjusted.rand"
    )

    purity <- tibble::tibble(
      cluster_ref = ref_matched,
      cluster_sub = sub_assignments$cluster
    ) |>
      dplyr::group_by(cluster_ref) |>
      dplyr::summarise(
        n = dplyr::n(),
        purity = max(table(cluster_sub)) / dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(iteration = i)

    list(ari = ari, purity = purity)
  })

  list(
    ari = tibble::tibble(
      iteration = seq_len(n_iterations),
      ari = vapply(results, \(r) r$ari, numeric(1))
    ),
    purity = purrr::map_dfr(results, "purity")
  )
}

# Sweep edge_percentile x resolution (optionally x algorithm), scoring each
# combination by resampling stability (median ARI, via cluster_stability()),
# modularity, and the number of non-trivial clusters. Naively maximizing
# stability alone is misleading — a single giant cluster is trivially
# "stable" — so n_clusters_valid is reported alongside stability and should
# be used to rule out degenerate solutions before picking a configuration.
#
# edge_percentiles, resolutions, algorithms : vectors of candidate values;
#   every combination is evaluated
# min_cluster_size                          : clusters at or below this size
#   are excluded from n_clusters_valid (matches the > 5 threshold used
#   elsewhere for "valid" clusters)
sweep_cluster_parameters <- function(
  survey_long,
  meta_df,
  topic_questions,
  edge_percentiles,
  resolutions,
  algorithms = "leiden",
  min_cluster_size = 5,
  subsample_frac = 0.8,
  n_iterations = 100,
  seed = 20260511
) {
  grid <- tidyr::expand_grid(
    edge_percentile = edge_percentiles,
    resolution = resolutions,
    algorithm = algorithms
  )

  purrr::pmap_dfr(grid, function(edge_percentile, resolution, algorithm) {
    ref <- build_cluster_network(
      survey_long,
      meta_df,
      topic_questions,
      edge_percentile = edge_percentile,
      resolution = resolution,
      algorithm = algorithm,
      seed = seed
    )

    sizes <- ref$cluster_assignments |> dplyr::count(cluster, name = "n")

    modularity <- igraph::modularity(
      ref$graph,
      igraph::membership(ref$community),
      weights = igraph::E(ref$graph)$weight
    )

    stab <- cluster_stability(
      survey_long,
      meta_df,
      topic_questions,
      ref$cluster_assignments,
      edge_percentile = edge_percentile,
      resolution = resolution,
      algorithm = algorithm,
      subsample_frac = subsample_frac,
      n_iterations = n_iterations,
      seed = seed
    )

    tibble::tibble(
      edge_percentile = edge_percentile,
      resolution = resolution,
      algorithm = algorithm,
      n_clusters_total = nrow(sizes),
      n_clusters_valid = sum(sizes$n > min_cluster_size),
      modularity = modularity,
      median_ari = stats::median(stab$ari$ari),
      min_ari = min(stab$ari$ari)
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
