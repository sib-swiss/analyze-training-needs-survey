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

# Normalize a character vector: collapse whitespace/newlines.
normalize_str <- function(x) {
	x |>
		stringr::str_replace_all("[\\r\\n]+", " ") |>
		stringr::str_squish()
}

# Build a per-column dictionary.
# Grouped questions follow: "Main question stem. Sub question"
# The stems are taken from likert-type entries in the metadata JSON.
build_question_dictionary <- function(df, meta_df) {
	normalized_columns <- names(df) |> normalize_str()

	# Use only likert stems for grouping detection, sorted longest-first to
	# avoid partial matches from shorter stems.
	grouped_stems <- meta_df |>
		dplyr::filter(question_meta_type == "likert") |>
		dplyr::pull(main_question) |>
		normalize_str() |>
		unique()
	grouped_stems <- grouped_stems[order(nchar(grouped_stems), decreasing = TRUE)]

	question_dictionary <- tibble::tibble(
		raw_column = names(df),
		main_question = normalized_columns,
		sub_question = NA_character_
	)

	for (i in seq_along(normalized_columns)) {
		col_name <- normalized_columns[[i]]
		matched_idx <- which(stringr::str_starts(
			col_name,
			stringr::fixed(grouped_stems)
		))

		if (length(matched_idx) > 0) {
			matched_main <- grouped_stems[[matched_idx[[1]]]]
			remainder <- stringr::str_sub(col_name, nchar(matched_main) + 1L) |>
				stringr::str_replace("^\\s*[:.;\\-]+\\s*", "") |>
				stringr::str_trim()

			if (nzchar(remainder)) {
				question_dictionary$main_question[[i]] <- matched_main
				question_dictionary$sub_question[[i]] <- remainder
			}
		}
	}

	# Join type and scale from metadata; columns not in the JSON get NA.
	question_dictionary |>
		dplyr::left_join(
			dplyr::select(
				meta_df,
				main_question,
				question_type = question_meta_type,
				scale
			),
			by = "main_question"
		)
}

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

# Likert (matrix-style) summary: one row per main/sub-question/answer combination.
grouped_summary <- survey_long |>
	dplyr::filter(
		question_type == "likert",
		!is.na(sub_question),
		!is.na(answer)
	) |>
	dplyr::count(main_question, sub_question, answer, name = "n") |>
	dplyr::group_by(main_question, sub_question) |>
	dplyr::mutate(pct = n / sum(n)) |>
	dplyr::ungroup()

# Single-choice question summary.
single_summary <- survey_long |>
	dplyr::filter(question_type == "single_choice", !is.na(answer)) |>
	dplyr::count(main_question, answer, name = "n") |>
	dplyr::group_by(main_question) |>
	dplyr::mutate(pct = n / sum(n)) |>
	dplyr::ungroup()

# Diverging Likert bar chart.
#
# survey_long          : the long-format data frame produced above
# main_question_filter : exact main_question string to plot
# split_by             : optional question label (normalized) to facet by,
#                        e.g. "Which career stage are you currently in?"
# split_values         : optional character vector to subset split_by values
plot_likert <- function(
	survey_long,
	main_question_filter,
	split_by = NULL,
	split_values = NULL
) {
	df <- survey_long |>
		dplyr::filter(
			question_type == "likert",
			main_question == main_question_filter,
			!is.na(answer),
			!is.na(sub_question)
		)

	scale_levels <- df$scale[[1]]
	n_levels <- length(scale_levels)
	mid_idx <- ceiling(n_levels / 2)

	if (!is.null(split_by)) {
		# Match against normalized column names to handle trailing non-breaking spaces.
		norm_names <- names(survey_results) |> stringr::str_squish()
		col_idx <- which(norm_names == stringr::str_squish(split_by))
		if (length(col_idx) == 0) {
			stop("split_by column not found: ", split_by)
		}

		split_col <- survey_results |>
			dplyr::mutate(respondent_id = dplyr::row_number()) |>
			dplyr::select(respondent_id, split_value = col_idx[1])

		df <- df |> dplyr::left_join(split_col, by = "respondent_id")
		if (!is.null(split_values)) {
			df <- df |> dplyr::filter(split_value %in% split_values)
		}
	}

	group_vars <- if (!is.null(split_by)) {
		c("sub_question", "answer", "split_value")
	} else {
		c("sub_question", "answer")
	}
	denom_vars <- if (!is.null(split_by)) {
		c("sub_question", "split_value")
	} else {
		"sub_question"
	}

	counts <- df |>
		dplyr::count(dplyr::across(dplyr::all_of(group_vars)), name = "n") |>
		dplyr::group_by(dplyr::across(dplyr::all_of(denom_vars))) |>
		dplyr::mutate(
			pct = n / sum(n),
			answer = factor(answer, levels = scale_levels),
			# Mid level contributes half-width to each side
			width = dplyr::if_else(as.integer(answer) == mid_idx, pct * 0.5, pct)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(sub_question = stringr::str_wrap(sub_question, width = 40))

	pal <- RColorBrewer::brewer.pal(n_levels, "RdYlGn")
	colors <- setNames(pal, scale_levels)

	# Compute xmin/xmax explicitly by cumulating outward from zero.
	# This avoids relying on ggplot's stacking order.
	# Left side: mid first (innermost), then levels below mid outward
	# Right side: mid first (innermost), then levels above mid outward
	left_order <- c(mid_idx, seq(mid_idx - 1, 1))
	right_order <- c(mid_idx, seq(mid_idx + 1, n_levels))

	make_side <- function(counts, level_order, sign) {
		counts |>
			dplyr::filter(as.integer(answer) %in% level_order) |>
			dplyr::mutate(order_rank = match(as.integer(answer), level_order)) |>
			dplyr::arrange(dplyr::across(dplyr::all_of(denom_vars)), order_rank) |>
			dplyr::group_by(dplyr::across(dplyr::all_of(denom_vars))) |>
			dplyr::mutate(
				xmax = sign * cumsum(width),
				xmin = sign * (cumsum(width) - width)
			) |>
			dplyr::ungroup()
	}

	segments <- dplyr::bind_rows(
		make_side(counts, left_order, -1),
		make_side(counts, right_order, 1)
	)

	# Order sub-questions by mean scale score (most positive on top).
	mean_score <- counts |>
		dplyr::mutate(score = as.integer(answer) * pct) |>
		dplyr::group_by(sub_question) |>
		dplyr::summarise(mean_score = sum(score), .groups = "drop")

	sub_levels <- mean_score |>
		dplyr::arrange(mean_score) |>
		dplyr::pull(sub_question)

	segments <- segments |>
		dplyr::mutate(y = as.numeric(factor(sub_question, levels = sub_levels)))

	p <- ggplot2::ggplot(
		segments,
		ggplot2::aes(
			xmin = xmin,
			xmax = xmax,
			ymin = y - 0.4,
			ymax = y + 0.4,
			fill = answer
		)
	) +
		ggplot2::geom_rect() +
		ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey30") +
		ggplot2::scale_x_continuous(
			labels = \(x) scales::percent(abs(x)),
			limits = c(-1, 1)
		) +
		ggplot2::scale_y_continuous(
			breaks = seq_along(sub_levels),
			labels = sub_levels
		) +
		ggplot2::scale_fill_manual(values = colors, breaks = scale_levels) +
		ggplot2::labs(
			title = stringr::str_wrap(main_question_filter, width = 60),
			x = NULL,
			y = NULL,
			fill = NULL
		) +
		ggplot2::theme_minimal(base_size = 11) +
		ggplot2::theme(legend.position = "bottom")

	if (!is.null(split_by)) {
		p <- p + ggplot2::facet_wrap(~split_value)
	}

	p
}

# Example: plot a Likert question without splitting.
plot_likert(
	survey_long,
	"Omics analysis: specify your training needs You can skip topics that are not relevant"
)

plot_likert(
	survey_long,
	"Specify which learning formats you prefer Note: these are not mutually exclusive, i.e. some can be used in combination",
)

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


plot_likert(
	survey_long,
	"Omics analysis: specify your training needs You can skip topics that are not relevant",
	split_by = "When did you last attend a SIB course? ",
	split_values = c(
		"I have never attended a SIB course",
		"More than a year ago",
		"Within the last year"
	)
)


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

swiss_participants <- survey_long |>
	dplyr::filter(
		main_question == "In which country do you work?",
		answer == "Switzerland"
	) |>
	dplyr::pull(respondent_id)

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
