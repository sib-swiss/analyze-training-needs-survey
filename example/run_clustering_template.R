# Template: apply the training-needs clustering pipeline to your own survey.
#
# Can be run from any directory — `here` locates the repository root (via
# .git), and also activates renv even when launched from a directory with no
# .Rprofile of its own to trigger it at R startup (as long as `here` itself
# is reachable outside the project's renv library, true for most R
# installations since it's tiny and extremely commonly pre-installed):
#   Rscript run_clustering_template.R          # from example/
#   Rscript example/run_clustering_template.R   # from the repo root
#
# This runs the same steps as the "Graph-based network and community
# detection" section of the manuscript (see manuscript/manuscript.qmd), using
# the R functions in R/. To reuse it for your own survey:
#   1. Point excel_path at your own response export, and metadata_path at a
#      question_metadata.json describing your questions (see
#      survey/README.md for the expected schema).
#   2. Update `topic_questions` below to the main-question stems you want to
#      cluster respondents on.
#   3. Don't just guess edge_percentile/resolution/algorithm — sweep them with
#      sweep_cluster_parameters() (also in R/clustering.R) and pick a setting
#      that maximizes resampling stability (cluster_stability()) among
#      configurations with a comparable, interpretable number of clusters.
#      The manuscript's own choice (60th percentile, resolution 1, Leiden)
#      was arrived at this way, starting from an initial guess of the 80th
#      percentile with Louvain that turned out to be far less stable.

library(here)
here::i_am("example/run_clustering_template.R")

library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(readxl)
library(purrr)
library(tibble)
library(igraph)
library(scales)

source(here::here("R", "data_prep.R"))
source(here::here("R", "plotting.R"))
source(here::here("R", "clustering.R"))

excel_path <- here::here("data", "supplementary_data1.xlsx")
metadata_path <- here::here("survey", "question_metadata.json")

survey_results <- readxl::read_excel(excel_path)
colnames(survey_results) <- str_trim(colnames(survey_results))

raw_meta <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)$questions
meta_df <- purrr::map_dfr(raw_meta, \(q) {
  tibble::tibble(
    main_question = q$main_question_stem,
    question_meta_type = q$type,
    scale = list(unlist(q$scale))
  )
})

# abbrev_path shortens long sub-question labels for display/plotting; it does
# not change which features feed the clustering step. Set it to NULL if you
# don't have (or don't want) an abbreviation lookup for your own survey.
question_dictionary <- build_question_dictionary(
  survey_results,
  meta_df,
  abbrev_path = here::here("data", "supplementary_data2.csv")
)

survey_long <- survey_results |>
  dplyr::mutate(respondent_id = dplyr::row_number()) |>
  dplyr::mutate(dplyr::across(-respondent_id, as.character)) |>
  tidyr::pivot_longer(
    cols = -respondent_id,
    names_to = "raw_column",
    values_to = "answer"
  ) |>
  dplyr::left_join(question_dictionary, by = "raw_column") |>
  dplyr::mutate(answer = dplyr::na_if(answer, "")) |>
  # This survey's export had "Highly needed" and "Very needed" as two labels
  # for the same scale point; fold the stray label into the real one so it
  # isn't silently dropped as an unmatched scale value. A data quirk of this
  # specific dataset — check for similar label drift in your own export.
  dplyr::mutate(
    answer = ifelse(answer == "Highly needed", "Very needed", answer)
  )

# These are the SIB survey's 5 course-topic domains (as used in the
# manuscript) — the Likert blocks respondents are actually clustered on.
# Other Likert-type blocks in this survey (barriers, format preferences) are
# deliberately excluded: clustering on them too would mix "what topics do you
# need" with "how do you like to learn", diluting the topic signal. Replace
# this list with your own topic-block stems from question_metadata.json.
topic_questions <- c(
  "Data management and knowledge representation: specify your training needs You can skip topics that are not relevant",
  "Computational methods and AI: specify your training needs You can skip topics that are not relevant",
  "Omics analysis: specify your training needs You can skip topics that are not relevant",
  "Biomedicine and pathogens: specify your training needs You can skip topics that are not relevant",
  "Biodiversity and Ecology: specify your training needs You can skip topics that are not relevant"
)

cluster_result <- build_cluster_network(
  survey_long,
  meta_df,
  topic_questions,
  edge_percentile = 0.6,
  resolution = 1,
  algorithm = "leiden",
  seed = 20260511
)

cat("Cluster sizes:\n")
cluster_result$cluster_assignments |>
  dplyr::count(cluster, name = "n_respondents") |>
  print(n = Inf)

cat("\nWithin-cluster interconnectedness:\n")
cluster_interconnectedness(cluster_result$graph, cluster_result$community) |>
  print(n = Inf)

plot_network <- plot_cluster_network(cluster_result$graph, cluster_result$community)
plot_network()
