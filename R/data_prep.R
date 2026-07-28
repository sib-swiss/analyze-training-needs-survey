# Normalize a character vector: collapse whitespace/newlines.
normalize_str <- function(x) {
	x |>
		stringr::str_replace_all("[\\r\\n]+", " ") |>
		stringr::str_squish()
}

# Build a per-column dictionary.
# Grouped questions follow: "Main question stem. Sub question"
# The stems are taken from likert-type entries in the metadata JSON.
#
# abbrev_path: optional CSV with `original`/`abbreviation` columns used to
# shorten long sub-question labels for plotting. Set to NULL to skip this step.
build_question_dictionary <- function(df, meta_df, abbrev_path = here::here("data", "supplementary_data2.csv")) {
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
	question_dictionary_out <- question_dictionary |>
		dplyr::left_join(
			dplyr::select(
				meta_df,
				main_question,
				question_type = question_meta_type,
				scale
			),
			by = "main_question"
		) |>
		dplyr::mutate(
			sub_question = dplyr::if_else(
				main_question == "Specify which learning formats you prefer Note: these are not mutually exclusive, i.e. some can be used in combination" & !is.na(sub_question),
				stringr::str_squish(stringr::str_remove(sub_question, "\\s*\\(.*$")),
				sub_question
			)
		)

	if (!is.null(abbrev_path) && file.exists(abbrev_path)) {
		abbrev_tbl <- utils::read.csv(abbrev_path, stringsAsFactors = FALSE) |>
			tibble::as_tibble() |>
			dplyr::mutate(
				original = normalize_str(original),
				abbreviation = normalize_str(abbreviation)
			)

		question_dictionary_out <- question_dictionary_out |>
			dplyr::mutate(sub_question = normalize_str(sub_question)) |>
			dplyr::left_join(abbrev_tbl, by = c("sub_question" = "original")) |>
			dplyr::mutate(sub_question = dplyr::coalesce(abbreviation, sub_question)) |>
			dplyr::select(-abbreviation)
	}

	question_dictionary_out
}
