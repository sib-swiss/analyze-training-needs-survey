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

# Diverging Likert bar chart.
#
# survey_long          : the long-format data frame produced above
# main_question_filter : one or more main_question strings to plot; when
#                        multiple are given they must share the same scale and
#                        all sub-questions are pooled into a single plot
# split_by             : optional question label (normalized) to facet by,
#                        e.g. "Which career stage are you currently in?"
# split_values         : optional character vector to subset split_by values
# counts               : if TRUE, use absolute counts on the x-axis instead of percentages
# labels               : if TRUE, show the value inside each bar segment
plot_likert <- function(
	survey_long,
	main_question_filter,
	split_by = NULL,
	split_values = NULL,
	counts = FALSE,
	labels = FALSE,
	base_size = 14
) {
	df <- survey_long |>
		dplyr::filter(
			question_type == "likert",
			main_question %in% main_question_filter,
			!is.na(answer),
			!is.na(sub_question)
		)

	# Validate that all supplied questions share the same scale.
	scales_used <- df |>
		dplyr::distinct(main_question, scale) |>
		dplyr::pull(scale)
	if (!all(sapply(scales_used, identical, scales_used[[1]]))) {
		stop("All questions in main_question_filter must share the same scale.")
	}

	scale_levels <- df$scale[[1]]
	n_levels     <- length(scale_levels)
	mid_idx      <- ceiling(n_levels / 2)

	if (!is.null(split_by)) {
		# Match against normalized column names to handle trailing non-breaking spaces.
		norm_names <- names(survey_results) |> stringr::str_squish()
		col_idx    <- which(norm_names == stringr::str_squish(split_by))
		if (length(col_idx) == 0) stop("split_by column not found: ", split_by)
		split_col <- survey_results |>
			dplyr::mutate(respondent_id = dplyr::row_number()) |>
			dplyr::select(respondent_id, split_value = col_idx[1])
		df <- df |> dplyr::left_join(split_col, by = "respondent_id")
		if (!is.null(split_values)) df <- df |> dplyr::filter(split_value %in% split_values)
		# Factor split_value so facets follow the order of split_values.
		df <- df |> dplyr::mutate(
			split_value = factor(split_value, levels = split_values)
		)
	}

	group_vars <- c("sub_question", "answer", if (!is.null(split_by)) "split_value")
	denom_vars <- c("sub_question",            if (!is.null(split_by)) "split_value")

	tally <- df |>
		dplyr::count(dplyr::across(dplyr::all_of(group_vars)), name = "n") |>
		dplyr::group_by(dplyr::across(dplyr::all_of(denom_vars))) |>
		dplyr::mutate(
			pct        = n / sum(n),
			answer     = factor(answer, levels = scale_levels),
			# Mid level contributes half-width to each side.
			# Use raw counts or proportions depending on `counts` argument.
			base_width = if (counts) as.double(n) else pct,
			width      = dplyr::if_else(as.integer(answer) == mid_idx, base_width * 0.5, base_width)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(sub_question = stringr::str_wrap(sub_question, width = 40))

	pal    <- RColorBrewer::brewer.pal(n_levels, "RdYlGn")
	colors <- setNames(pal, scale_levels)

	# Compute xmin/xmax explicitly by cumulating outward from zero.
	# Left side: mid first (innermost), then levels below mid outward.
	# Right side: mid first (innermost), then levels above mid outward.
	left_order  <- c(mid_idx, seq(mid_idx - 1, 1))
	right_order <- c(mid_idx, seq(mid_idx + 1, n_levels))

	make_side <- function(tally, level_order, sign) {
		tally |>
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
		make_side(tally, left_order,  -1),
		make_side(tally, right_order,  1)
	)

	# Order sub-questions by mean scale score (most positive on top).
	# When counts = TRUE, weight by raw counts instead of proportions.
	mean_score <- tally |>
		dplyr::mutate(score = as.integer(answer) * if (counts) n else pct) |>
		dplyr::group_by(sub_question) |>
		dplyr::summarise(mean_score = sum(score), .groups = "drop")

	sub_levels <- mean_score |>
		dplyr::arrange(mean_score) |>
		dplyr::pull(sub_question)

	segments <- segments |>
		dplyr::mutate(sub_question = factor(sub_question, levels = sub_levels))

	x_scale <- if (counts) {
		ggplot2::scale_x_continuous(labels = \(x) abs(x))
	} else {
		ggplot2::scale_x_continuous(labels = \(x) scales::percent(abs(x)), limits = c(-1, 1))
	}

	# Only show title when a single question is plotted.
	plot_title <- if (length(main_question_filter) == 1) {
		stringr::str_wrap(main_question_filter, width = 60)
	} else {
		NULL
	}

	p <- ggplot2::ggplot(
		segments,
		ggplot2::aes(
			xmin = xmin,
			xmax = xmax,
			ymin = as.numeric(sub_question) - 0.4,
			ymax = as.numeric(sub_question) + 0.4,
			fill = answer
		)
	) +
		ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey30") +
		ggplot2::geom_rect() +
		{
			if (labels) {
				# For the mid level, deduplicate: only label the right half (xmin == 0),
				# placing the label at x=0 — the true centre of the full mid bar.
				# For all other segments, place the label at the bar midpoint.
				label_data <- segments |>
					dplyr::mutate(
						is_mid  = answer == scale_levels[mid_idx],
						x_label = dplyr::if_else(is_mid, 0, (xmin + xmax) / 2),
						label   = if (counts) {
							as.character(n)
						} else {
							ifelse(abs(xmax - xmin) < 0.03, "", scales::percent(pct, accuracy = 1))
						}
					) |>
					dplyr::filter(!(is_mid & xmax <= 0))

				ggplot2::geom_text(
					ggplot2::aes(x = x_label, y = as.numeric(sub_question), label = label),
					data        = label_data,
					size        = 2.8,
					colour      = "grey20",
					inherit.aes = FALSE
				)
			}
		} +
		x_scale +
		ggplot2::scale_y_continuous(
			breaks = seq_along(levels(segments$sub_question)),
			labels = levels(segments$sub_question)
		) +
		ggplot2::scale_fill_manual(values = colors, breaks = scale_levels) +
		ggplot2::labs(title = plot_title, x = NULL, y = NULL, fill = NULL) +
		ggplot2::theme_minimal(base_size = base_size) +
		ggplot2::theme(legend.position = "bottom")

	if (!is.null(split_by)) p <- p + ggplot2::facet_wrap(~ split_value)

	p
}