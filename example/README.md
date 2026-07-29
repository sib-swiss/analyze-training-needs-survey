# Example: reusing the clustering pipeline

`run_clustering_template.R` runs the graph-based clustering method from the manuscript end-to-end, using only the functions in [`../R/`](../R/). It is meant to be copied and adapted, not run as-is for a different survey.

It currently points at this repository's own SIB training needs data, so you can run it directly to see the pipeline work. It uses [`here`](https://here.r-lib.org/) to locate the repository root, so it can be run from either the repository root or from inside `example/`:

```sh
Rscript example/run_clustering_template.R   # from the repo root
Rscript run_clustering_template.R           # from example/
```

Running it reproduces the manuscript's exact clustering result (82/52/60/63 respondents across 4 clusters, plus a 1-respondent outlier), using the manuscript's chosen settings: 60th-percentile edge threshold, resolution 1, Leiden community detection.

## Adapting it to your own survey

1. Export your own responses to an Excel file, and describe your questions in a `question_metadata.json` following the schema in [`../survey/README.md`](../survey/README.md).
2. Point `excel_path` and `metadata_path` at those files.
3. Set `topic_questions` to the main-question stems you want respondents clustered on (the template defaults to this survey's 5 course-topic domains).
4. Run the script. It prints cluster sizes and within-cluster interconnectedness, and plots the respondent network.

Don't just guess `edge_percentile`/`resolution`/`algorithm` for your own data. Use `sweep_cluster_parameters()` (in [`../R/clustering.R`](../R/clustering.R)) to score a grid of candidate settings by resampling stability (`cluster_stability()`'s median Adjusted Rand Index), and pick the most stable setting among configurations with a comparable, interpretable number of clusters — naively maximizing stability alone rewards degenerate solutions (a single giant cluster is trivially "stable"). This is exactly how the manuscript's own settings were chosen: an initial guess (80th percentile, Louvain) reached only median ARI = 0.35, while sweeping found a substantially more stable configuration (60th percentile, Leiden: median ARI = 0.93) that preserved the same four learner profiles.

If the resulting clusters look off:

- **Too few / too large clusters** — lower `resolution` in `build_cluster_network()`, or lower `edge_percentile` to keep more edges.
- **Too many small/singleton clusters** — raise `resolution`, or raise `edge_percentile` to prune more weak edges.
- Very small respondent pools (well under ~100) may not have enough pairwise signal for stable communities; inspect `cluster_interconnectedness()` and `cluster_stability()` output before trusting the grouping.
