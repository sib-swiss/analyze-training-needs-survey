# Example: reusing the clustering pipeline

`run_clustering_template.R` runs the graph-based clustering method from the manuscript end-to-end, using only the functions in [`../R/`](../R/). It is meant to be copied and adapted, not run as-is for a different survey.

It currently points at this repository's own SIB training needs data, so you can run it directly to see the pipeline work. It uses [`here`](https://here.r-lib.org/) to locate the repository root, so it can be run from either the repository root or from inside `example/`:

```sh
Rscript example/run_clustering_template.R   # from the repo root
Rscript run_clustering_template.R           # from example/
```

Running it reproduces the manuscript's exact clustering result (69/50/65/73 respondents across 4 clusters, plus a 1-respondent outlier).

## Adapting it to your own survey

1. Export your own responses to an Excel file, and describe your questions in a `question_metadata.json` following the schema in [`../survey/README.md`](../survey/README.md).
2. Point `excel_path` and `metadata_path` at those files.
3. Set `topic_questions` to the main-question stems you want respondents clustered on (the template defaults to this survey's 5 course-topic domains).
4. Run the script. It prints cluster sizes and within-cluster interconnectedness, and plots the respondent network.

If the resulting clusters look off:

- **Too few / too large clusters** — lower `resolution` in `build_cluster_network()`, or lower `edge_percentile` to keep more edges.
- **Too many small/singleton clusters** — raise `resolution`, or raise `edge_percentile` to prune more weak edges.
- Very small respondent pools (well under ~100) may not have enough pairwise signal for stable communities; inspect `cluster_interconnectedness()` output before trusting the grouping.
