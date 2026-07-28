# Colorblind-friendly categorical palette (Okabe & Ito, 2008), the standard
# palette for scientific figures. Excludes black and grey, which are reserved
# for text/axes and "Other"/reference categories respectively. Interpolated
# if more colors are requested than the base palette provides.
cvd_palette <- function(n) {
  base_pal <- unname(grDevices::palette.colors(palette = "Okabe-Ito"))
  base_pal <- base_pal[!base_pal %in% c("#000000", "#999999")]
  if (n <= length(base_pal)) {
    base_pal[seq_len(n)]
  } else {
    grDevices::colorRampPalette(base_pal)(n)
  }
}

# Pick a readable label colour (near-black or white) for text drawn on top of
# a given fill colour, via WCAG relative luminance. A fixed label colour reads
# fine on light segments but disappears on dark ones (e.g. the dark ends of a
# diverging palette), so labels drawn on a fill need to adapt per-segment.
label_colour_for <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  luminance <- 0.2126 * lin["red", ] + 0.7152 * lin["green", ] + 0.0722 * lin["blue", ]
  ifelse(luminance > 0.179, "grey15", "white")
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
	base_size = 14,
	positive_only = FALSE,
	top_n_sub_questions = NULL
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

	# Positive-only mode: keep only the top two levels.
	if (positive_only) {
		pos_levels <- tail(scale_levels, 2)
		df <- df |> dplyr::filter(answer %in% pos_levels)
	}

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
			base_width = if (counts) as.double(n) else pct,
			width      = dplyr::if_else(!positive_only & as.integer(answer) == mid_idx, base_width * 0.5, base_width)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(sub_question = stringr::str_wrap(sub_question, width = 40))

	# RdBu (blue <-> red) is colorblind-safe; RdYlGn is not (relies on
	# red-green hue discrimination, which fails for the most common CVD types).
	pal    <- RColorBrewer::brewer.pal(n_levels, "RdBu")
	colors <- setNames(pal, scale_levels)

	if (positive_only) {
		# Simple stacked bars from 0 for the two positive levels.
		pos_levels <- tail(scale_levels, 2)
		# Use actual denominator = all respondents for this sub_question (restore from full tally).
		segments <- tally |>
			dplyr::filter(answer %in% pos_levels) |>
			dplyr::mutate(answer = factor(answer, levels = pos_levels)) |>
			dplyr::arrange(dplyr::across(dplyr::all_of(denom_vars)), answer) |>
			dplyr::group_by(dplyr::across(dplyr::all_of(denom_vars))) |>
			dplyr::mutate(
				xmax = cumsum(width),
				xmin = cumsum(width) - width
			) |>
			dplyr::ungroup()
	} else {

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
	} # end else (diverging)

	# Order sub-questions by mean scale score (most positive on top).
	# In positive_only mode, re-score using ranks 1/2 across the two positive levels only.
	# When counts = TRUE, weight by raw counts instead of proportions.
	mean_score <- if (positive_only) {
		pos_levels <- tail(scale_levels, 2)
		tally |>
			dplyr::filter(answer %in% pos_levels) |>
			dplyr::mutate(rank = match(as.character(answer), pos_levels)) |>
			dplyr::mutate(score = rank * if (counts) n else pct) |>
			dplyr::group_by(sub_question) |>
			dplyr::summarise(mean_score = sum(score), .groups = "drop")
	} else {
		tally |>
			dplyr::mutate(score = as.integer(answer) * if (counts) n else pct) |>
			dplyr::group_by(sub_question) |>
			dplyr::summarise(mean_score = sum(score), .groups = "drop")
	}

	if (!is.null(top_n_sub_questions)) {
		if (!is.numeric(top_n_sub_questions) || length(top_n_sub_questions) != 1 || top_n_sub_questions < 1) {
			stop("top_n_sub_questions must be a single positive number.")
		}
		top_n_sub_questions <- as.integer(top_n_sub_questions)
		mean_score <- mean_score |>
			dplyr::slice_max(order_by = mean_score, n = top_n_sub_questions, with_ties = FALSE)
	}

	sub_levels <- mean_score |>
		dplyr::arrange(mean_score) |>
		dplyr::pull(sub_question)

	segments <- segments |>
		dplyr::filter(sub_question %in% sub_levels) |>
		dplyr::mutate(sub_question = factor(sub_question, levels = sub_levels))

	x_scale <- if (positive_only) {
		if (counts) {
			ggplot2::scale_x_continuous(labels = \(x) abs(x), expand = ggplot2::expansion(mult = c(0, 0.05)))
		} else {
			ggplot2::scale_x_continuous(labels = \(x) scales::percent(abs(x)), limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.05)))
		}
	} else if (counts) {
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
		{ if (!positive_only) ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey30") } +
		ggplot2::geom_rect() +
		{
			if (labels) {
				label_data <- if (positive_only) {
					segments |>
						dplyr::mutate(
							x_label = (xmin + xmax) / 2,
							label   = if (counts) {
								as.character(n)
							} else {
								ifelse(abs(xmax - xmin) < 0.03, "", scales::percent(pct, accuracy = 1))
							},
							label_colour = label_colour_for(colors[as.character(answer)])
						)
				} else {
					# For the mid level, deduplicate: only label the right half (xmin == 0),
					# placing the label at x=0 — the true centre of the full mid bar.
					# For all other segments, place the label at the bar midpoint.
					segments |>
						dplyr::mutate(
							is_mid  = answer == scale_levels[mid_idx],
							x_label = dplyr::if_else(is_mid, 0, (xmin + xmax) / 2),
							label   = if (counts) {
								as.character(n)
							} else {
								ifelse(abs(xmax - xmin) < 0.03, "", scales::percent(pct, accuracy = 1))
							},
							label_colour = label_colour_for(colors[as.character(answer)])
						) |>
						dplyr::filter(!(is_mid & xmax <= 0))
				}

				ggplot2::geom_text(
					ggplot2::aes(x = x_label, y = as.numeric(sub_question), label = label, colour = label_colour),
					data        = label_data,
					size        = 2.8,
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
		ggplot2::scale_colour_identity() +
		ggplot2::labs(title = plot_title, x = NULL, y = NULL, fill = NULL) +
		ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
		ggplot2::theme(legend.position = "bottom")

	if (!is.null(split_by)) p <- p + ggplot2::facet_wrap(~ split_value)

	p
}

# Retrieve the choices vector for a given main question stem from the metadata.
# Depends on `raw_meta` (the parsed question_metadata.json) in the calling
# environment.
get_question_choices <- function(stem) {
  raw_meta |>
    purrr::keep(~ identical(.x$main_question_stem, stem)) |>
    purrr::pluck(1) |>
    purrr::pluck("choices") |>
    unlist(use.names = FALSE)
}

# Prepare data for faceted pie charts of cluster composition by a categorical variable.
make_cluster_pie_data <- function(df, value_col, allowed_choices, top_n = 5) {
  allowed_plus_other <- unique(c(allowed_choices, "Other"))

  d <- df |>
    dplyr::filter(!is.na(cluster)) |>
    dplyr::filter(.data[[value_col]] %in% allowed_plus_other) |>
    dplyr::transmute(cluster, value = .data[[value_col]])

  non_other_levels <- d |>
    dplyr::filter(value != "Other") |>
    dplyr::count(value, sort = TRUE) |>
    dplyr::pull(value)

  if (length(non_other_levels) > top_n) {
    keep_levels <- d |>
      dplyr::filter(value != "Other") |>
      dplyr::count(value, sort = TRUE) |>
      dplyr::slice_head(n = top_n) |>
      dplyr::pull(value)

    d <- d |>
      dplyr::mutate(
        value = dplyr::case_when(
          value == "Other" ~ "Other",
          value %in% keep_levels ~ value,
          TRUE ~ "Other"
        )
      )
  }

  d |>
    dplyr::count(cluster, value, name = "n") |>
    dplyr::group_by(cluster) |>
    dplyr::mutate(pct = n / sum(n)) |>
    dplyr::ungroup()
}

# Faceted pie charts of cluster composition by a categorical variable.
# Depends on `valid_clusters` and `cluster_display_labels` (the filtered
# cluster levels and their display labels) in the calling environment.
plot_cluster_pies <- function(d, title) {
  value_order <- d |>
    dplyr::group_by(value) |>
    dplyr::summarise(total = sum(n), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total)) |>
    dplyr::pull(value)

  if ("Other" %in% value_order) {
    value_order <- c(setdiff(value_order, "Other"), "Other")
  }

  wrapped_levels <- stringr::str_wrap(value_order, width = 24)

  d <- d |>
    dplyr::mutate(
      cluster = factor(cluster, levels = valid_clusters, labels = cluster_display_labels),
      value = factor(value, levels = value_order),
      value_label = factor(stringr::str_wrap(as.character(value), width = 24), levels = wrapped_levels)
    )

	n_vals <- nlevels(d$value_label)
	fill_pal <- cvd_palette(n_vals)
	names(fill_pal) <- levels(d$value_label)
	other_label <- stringr::str_wrap("Other", width = 24)
	if (other_label %in% names(fill_pal)) {
		fill_pal[[other_label]] <- "lightgrey"
	}
	label_pal <- setNames(label_colour_for(fill_pal), names(fill_pal))

  ggplot2::ggplot(d, ggplot2::aes(x = "", y = pct, fill = value_label)) +
    ggplot2::geom_col(width = 1, colour = "white") +
		ggplot2::scale_fill_manual(values = fill_pal, drop = FALSE) +
		ggplot2::scale_colour_manual(values = label_pal, guide = "none") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::facet_wrap(~ cluster) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(pct >= 0.05, scales::percent(pct, accuracy = 1), ""), colour = value_label),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3
    ) +
    ggplot2::labs(title = title, fill = NULL) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, byrow = TRUE)) +
    ggplot2::theme(
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 9, lineheight = 0.95),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold")
    )
}
