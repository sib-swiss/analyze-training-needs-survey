library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(jsonlite)

excel_path <- "/Users/geertvangeest/Library/CloudStorage/OneDrive-SIBSwissInstituteofBioinformatics/SIB Training survey.xlsx"
metadata_path <- "question_metadata.json"

survey_results <- readxl::read_excel(excel_path)

survey_results$region <- ifelse(
	survey_results$`In which country do you work?  ` == "Switzerland",
	"Switzerland",
	"Other"
)

# Load question metadata from JSON.
# Each entry has: main_question_stem, type, and optionally scale.
# Types: "likert", "single_choice", "open_text"
if (!file.exists(metadata_path)) {
	stop("Question metadata file not found: ", metadata_path)
}

raw_meta <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)$questions
meta_df <- purrr::map_dfr(raw_meta, \(q) {
	tibble::tibble(
		main_question = q$main_question_stem,
		question_meta_type = q$type,
		scale = list(unlist(q$scale))
	)
})


question_dictionary <- build_question_dictionary(survey_results, meta_df)
View(question_dictionary)

# Validate: warn about metadata stems that don't match any dictionary entry.
unmatched <- meta_df$main_question[
	!meta_df$main_question %in% question_dictionary$main_question
]
if (length(unmatched) > 0) {
	warning(
		"These metadata stems don't match any column:\n  ",
		paste(unmatched, collapse = "\n  ")
	)
}

# Pivot to long format and join dictionary (which already carries question_type
# and scale from the JSON via build_question_dictionary).
survey_long <- survey_results |>
	dplyr::mutate(respondent_id = dplyr::row_number()) |>
	# Cast all columns to character before pivoting; the Excel file contains
	# mixed types (double, datetime, character) that pivot_longer cannot combine.
	dplyr::mutate(dplyr::across(-respondent_id, as.character)) |>
	tidyr::pivot_longer(
		cols = -respondent_id,
		names_to = "raw_column",
		values_to = "answer"
	) |>
	dplyr::left_join(question_dictionary, by = "raw_column") |>
	dplyr::mutate(answer = dplyr::na_if(answer, ""))

# fixing mistake of interchang 'Highly needed' and 'Very needed'. Replace all 'Highly needed' with 'Very needed' in the answer column. 
survey_long <- survey_long %>%
	mutate(answer = ifelse(answer == "Highly needed", "Very needed", answer))

all_topic_questions <- c(
 "Data management and knowledge representation: specify your training needs You can skip topics that are not relevant",
 "Computational methods and AI: specify your training needs You can skip topics that are not relevant",
 "Omics analysis: specify your training needs You can skip topics that are not relevant",
 "Biomedicine and pathogens: specify your training needs You can skip topics that are not relevant",
 "Biodiversity and Ecology: specify your training needs You can skip topics that are not relevant"
)

swiss_participants <- survey_long |>
	dplyr::filter(
		main_question == "In which country do you work?",
		answer == "Switzerland"
	) |>
	dplyr::pull(respondent_id)


p <- survey_long_swiss <- survey_long |>
	dplyr::filter(respondent_id %in% swiss_participants) |>
plot_likert(
	all_topic_questions,
	counts = TRUE,
	labels = TRUE
)
print(p)

p <- survey_long_swiss <- survey_long |>
	dplyr::filter(respondent_id %in% swiss_participants) |>
plot_likert(
	all_topic_questions,
	counts = TRUE,
	labels = TRUE,
		split_by = "Which career stage are you currently in?",
	split_values = c(
		"PhD candidate",
		"Postdoctoral researcher",
		"Senior scientist/ Principal investigator"
	)
)
print(p)

# Example: plot a Likert question without splitting.
plot_likert(
	survey_long,
	"Computational methods and AI: specify your training needs You can skip topics that are not relevant",
	counts = TRUE,
	labels = TRUE,
		split_by = "Which career stage are you currently in?",
	split_values = c(
		"PhD candidate",
		"Postdoctoral researcher",
		"Senior scientist/ Principal investigator"
	)
)

# Example: plot a Likert question without splitting.
plot_likert(
	survey_long,
	"Computational methods and AI: specify your training needs You can skip topics that are not relevant",
	counts = TRUE,
	labels = TRUE,
		split_by = "Which career stage are you currently in?",
	split_values = c(
		"PhD candidate",
		"Postdoctoral researcher",
		"Senior scientist/ Principal investigator"
	)
)

p <- survey_long_swiss <- survey_long |>
	dplyr::filter(respondent_id %in% swiss_participants) |>
plot_likert(
	"Specify which learning formats you prefer Note: these are not mutually exclusive, i.e. some can be used in combination",
	counts = TRUE,
	labels = TRUE
)
print(p)

# Example: compare PhD candidates vs postdocs.
p <- survey_long_swiss <- survey_long |>
	dplyr::filter(respondent_id %in% swiss_participants) |>
plot_likert(
	"Omics analysis: specify your training needs You can skip topics that are not relevant",
	split_by = "Which career stage are you currently in?",
	split_values = c(
		"PhD candidate",
		"Postdoctoral researcher",
		"Senior scientist/ Principal investigator"
	)
)

print(p)

p <- survey_long_swiss <- survey_long |>
	dplyr::filter(respondent_id %in% swiss_participants) |>
plot_likert(
all_topic_questions,
	split_by = "When did you last attend a SIB course? ", labels = TRUE, counts = TRUE,
	split_values = c(
		"I have never attended a SIB course",
		"More than a year ago",
		"Within the last year"
	)
)
print(p)

p <- survey_long_swiss <- survey_long |>
	dplyr::filter(respondent_id %in% swiss_participants) |>
plot_likert(
	"Data management and knowledge representation: specify your training needs You can skip topics that are not relevant",
	split_by = "When did you last attend a SIB course? ", labels = TRUE, counts = TRUE,
	split_values = c(
		"I have never attended a SIB course",
		"More than a year ago",
		"Within the last year"
	)
)
print(p)


plot_likert(
	survey_long,
	"Specify which learning formats you prefer Note: these are not mutually exclusive, i.e. some can be used in combination",
	split_by = "Which career stage are you currently in?",
	split_values = c(
		"PhD candidate",
		"Postdoctoral researcher",
		"Senior scientist/ Principal investigator"
	)
)

plot_likert(
	survey_long,
	"Please indicate how much each of the following factors would be a barrier for your participation in a SIB training course.",
	split_by = "region",
	split_values = c(
		"Switzerland",
		"Other"
	)
)



phd_postdoc_pi_participants <- survey_long |>
	dplyr::filter(
		main_question == "Which career stage are you currently in?",
		answer %in% c(
			"PhD candidate",
			"Postdoctoral researcher",
			"Senior scientist/ Principal investigator"
		)
	) |>
	dplyr::pull(respondent_id)

p <- survey_long_swiss <- survey_long |>
	dplyr::filter(respondent_id %in% swiss_participants) |>
	plot_likert(
		"Specify which learning formats you prefer Note: these are not mutually exclusive, i.e. some can be used in combination",
		split_by = "Which career stage are you currently in?",
		split_values = c(
			"PhD candidate",
			"Postdoctoral researcher",
			"Senior scientist/ Principal investigator"
		)
	)
print(p)

p <- survey_long_swiss <- survey_long |>
		dplyr::filter(respondent_id %in% swiss_participants, respondent_id %in% phd_postdoc_pi_participants) |>
	plot_likert(
		"Specify which learning formats you prefer Note: these are not mutually exclusive, i.e. some can be used in combination",
	)
print(p)

p <- survey_long_swiss <- survey_long |>
	dplyr::filter(respondent_id %in% swiss_participants) |>
	plot_likert(
		"Please indicate how much each of the following factors would be a barrier for your participation in a SIB training course.",
		split_by = "Which career stage are you currently in?",
		split_values = c(
			"PhD candidate",
			"Postdoctoral researcher",
			"Senior scientist/ Principal investigator"
		)
	)
print(p)

# Example visualization for grouped questions.
# Uncomment to plot.
# grouped_summary |>
#   ggplot2::ggplot(ggplot2::aes(x = sub_question, y = pct, fill = answer)) +
#   ggplot2::geom_col(position = "fill") +
#   ggplot2::facet_wrap(~ main_question, scales = "free_x") +
#   ggplot2::scale_y_continuous(labels = scales::percent_format()) +
#   ggplot2::labs(x = NULL, y = "Share of responses", fill = "Answer")

# Example visualization for single-choice questions.
# Uncomment to plot.
# single_summary |>
#   ggplot2::ggplot(ggplot2::aes(x = answer, y = pct)) +
#   ggplot2::geom_col() +
#   ggplot2::facet_wrap(~ main_question, scales = "free_x") +
#   ggplot2::scale_y_continuous(labels = scales::percent_format()) +
#   ggplot2::labs(x = NULL, y = "Share of responses")
