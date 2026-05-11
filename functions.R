# Normalize a character vector: collapse whitespace/newlines.
normalize_str <- function(x) {
	x |>
		stringr::str_replace_all("[\\r\\n]+", " ") |>
		stringr::str_squish()
}

# Render a ggplot to an inline SVG string.
plot_to_svg <- function(p, width, height) {
  svg_file <- tempfile(fileext = ".svg")
  svglite::svglite(svg_file, width = width, height = height,
                   system_fonts = list(sans = "DejaVu Sans"))
  print(p)
  grDevices::dev.off()
	paste(readLines(svg_file, warn = FALSE), collapse = "\n") |>
		stringr::str_replace_all(
			'font-family:\\s*"[^"]+";',
			'font-family: sans-serif;'
		) |>
		stringr::str_replace_all(
			"\\stextLength='[^']*'",
			""
		) |>
		stringr::str_replace_all(
			"\\slengthAdjust='[^']*'",
			""
		)
}

# Escape a string for safe embedding inside a JS template literal (backticks).
# Only backticks and ${ need escaping.
js_escape <- function(s) {
  s |>
    gsub("`",     "\\\\`",   x = _) |>
    gsub("\\$\\{", "\\\\${", x = _)
}

# Render a Likert plot with per-plot toggle buttons for counts/% and
# optionally Swiss-only/all respondents.
#
# id         : unique string id for this plot
# all_expr   : function(counts) returning a ggplot for all respondents
# swiss_expr : optional function(counts) returning a ggplot for Swiss only
# width, height : plot dimensions in inches
likert_toggle <- function(id, all_expr, width, height, swiss_expr = NULL, pos_expr = NULL) {
  has_swiss <- !is.null(swiss_expr)
  has_pos   <- !is.null(pos_expr)
  jid       <- gsub("-", "_", id)

  svg_pct_all   <- js_escape(plot_to_svg(all_expr(counts = FALSE), width, height))
  svg_cnt_all   <- js_escape(plot_to_svg(all_expr(counts = TRUE),  width, height))
  svg_pct_swiss <- if (has_swiss) js_escape(plot_to_svg(swiss_expr(counts = FALSE), width, height)) else ""
  svg_cnt_swiss <- if (has_swiss) js_escape(plot_to_svg(swiss_expr(counts = TRUE),  width, height)) else ""
  svg_pct_pos   <- if (has_pos) js_escape(plot_to_svg(pos_expr(counts = FALSE), width, height)) else ""
  svg_cnt_pos   <- if (has_pos) js_escape(plot_to_svg(pos_expr(counts = TRUE),  width, height)) else ""

  swiss_btn <- if (has_swiss) glue::glue(
    '<button id="{id}-scope-btn" onclick="lt_{jid}.toggleScope()"
       style="padding:5px 14px;font-size:0.9em;cursor:pointer;margin-left:4px;">
       Swiss only
     </button>'
  ) else ""

  pos_btn <- if (has_pos) glue::glue(
    '<button id="{id}-pos-btn" onclick="lt_{jid}.togglePos()"
       style="padding:5px 14px;font-size:0.9em;cursor:pointer;margin-left:4px;">
       Positive only
     </button>'
  ) else ""

  # Use a tempfile to write the JS so glue never interprets backticks.
  js <- paste0(
'(function() {
  var svgs = {
    "pct-all":   `', svg_pct_all,   '`,
    "cnt-all":   `', svg_cnt_all,   '`,
    "pct-swiss": `', svg_pct_swiss, '`,
    "cnt-swiss": `', svg_cnt_swiss, '`,
    "pct-pos":   `', svg_pct_pos,   '`,
    "cnt-pos":   `', svg_cnt_pos,   '`
  };
  var mode  = "cnt";
  var scope = "all";
  function render() {
    var plotHost = document.getElementById("', id, '-plot");
    plotHost.innerHTML = svgs[mode + "-" + scope];

    // Make injected SVG responsive to available container width.
    var svg = plotHost.querySelector("svg");
    if (svg) {
      svg.removeAttribute("width");
      svg.removeAttribute("height");
			svg.setAttribute("preserveAspectRatio", "xMinYMin meet");

			var viewBox = svg.getAttribute("viewBox");
			if (viewBox) {
				var parts = viewBox.trim().split(/\\s+/);
				if (parts.length === 4) {
					var vbWidth = parseFloat(parts[2]);
					var vbHeight = parseFloat(parts[3]);
					if (vbWidth > 0 && vbHeight > 0) {
						svg.style.aspectRatio = vbWidth + " / " + vbHeight;
					}
				}
			}

      svg.style.width = "100%";
      svg.style.height = "auto";
      svg.style.display = "block";
    }
  }
  window.lt_', jid, ' = {
    toggleMode: function() {
      mode = (mode === "pct") ? "cnt" : "pct";
      document.getElementById("', id, '-mode-btn").textContent =
        (mode === "cnt") ? "Show percentages" : "Show counts";
      render();
    },
    toggleScope: function() {
      scope = (scope === "all") ? "swiss" : "all";
      document.getElementById("', id, '-scope-btn").textContent =
        (scope === "all") ? "Swiss only" : "All respondents";
      render();
    },
    togglePos: function() {
      scope = (scope === "pos") ? "all" : "pos";
      document.getElementById("', id, '-pos-btn").textContent =
        (scope === "pos") ? "All responses" : "Positive only";
      render();
    }
  };
  render();
})();')

	html <- paste0(
		'<div style="width:min(112vw, calc(100% + 16rem));padding:0 0.75rem 0 0;box-sizing:border-box;">\n',
		'  <div style="margin:0.6em 0 0.4em;">\n',
		'    <button id="', id, '-mode-btn" onclick="lt_', jid, '.toggleMode()"',
		' style="padding:5px 14px;font-size:0.9em;cursor:pointer;">Show percentages</button>\n',
		'    ', swiss_btn, '\n',
		'    ', pos_btn, '\n',
		'  </div>\n',
		'  <div id="', id, '-plot" style="width:100%;"></div>\n',
		'</div>\n',
		'<script>\n', js, '\n</script>\n'
	)

	# Force pandoc to treat output as raw HTML in Quarto/knitr pipelines.
	paste0("```{=html}\n", html, "\n```\n")
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

# 100% stacked bar chart of SIB course attendance by a grouping variable.
#
# df             : a data frame containing the grouping column, sib_course column,
#                  and optionally a recode_to_other vector
# group_col      : name of the grouping column (string)
# group_levels   : ordered factor levels for the grouping column; values not in
#                  this vector are recoded to "Other"
# sib_col        : name of the SIB course column (string)
# sib_levels     : ordered factor levels for the SIB course fill variable
# min_n          : minimum total respondents per group to include (default 0)
plot_sib_course_pct <- function(
  df,
  group_col,
  group_levels,
  sib_col,
  sib_levels,
  min_n = 0,
  base_size = 14
) {
  sib_colors <- c(
    "#74C0E0",  # never
    "#F4A942",  # more than a year ago
    "#5BAF7A"   # within last year
  )
  names(sib_colors) <- sib_levels

  df |>
    dplyr::rename(group = dplyr::all_of(group_col), sib = dplyr::all_of(sib_col)) |>
    dplyr::filter(!is.na(group), !is.na(sib)) |>
    dplyr::mutate(group = dplyr::if_else(group %in% group_levels, group, "Other")) |>
    dplyr::count(group, sib) |>
    dplyr::group_by(group) |>
    dplyr::mutate(pct = n / sum(n)) |>
    dplyr::filter(sum(n) >= min_n) |>
    dplyr::ungroup() |>
    dplyr::mutate(sib = factor(sib, levels = sib_levels)) |>
    # Order groups by the proportion that answered sib_levels[1] (ascending,
    # so bars with the highest "never attended" proportion appear at the top).
    (\(d) {
      order_vals <- d |>
        dplyr::filter(sib == sib_levels[1]) |>
        dplyr::arrange(pct) |>
        dplyr::pull(group)
      # Ensure all groups are represented (some may have 0 for sib_levels[1]).
      all_groups <- unique(d$group)
      ordered_levels <- c(setdiff(all_groups, order_vals), order_vals)
      dplyr::mutate(d, group = factor(group, levels = ordered_levels))
    })() |>
    (\(d) {
      ggplot2::ggplot(d, ggplot2::aes(x = pct, y = group, fill = sib)) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::geom_text(
          ggplot2::aes(label = n),
          position = ggplot2::position_stack(vjust = 0.5),
          size = base_size / 4, colour = "grey20"
        ) +
        ggplot2::scale_x_continuous(
          labels = scales::percent_format(),
          expand = ggplot2::expansion(mult = c(0, 0.05))
        ) +
        ggplot2::scale_fill_manual(values = sib_colors) +
        ggplot2::labs(x = "Percentage", y = NULL, fill = NULL) +
        ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
        ggplot2::theme(legend.position = "bottom")
    })()
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
	positive_only = FALSE
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

	pal    <- RColorBrewer::brewer.pal(n_levels, "RdYlGn")
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

	sub_levels <- mean_score |>
		dplyr::arrange(mean_score) |>
		dplyr::pull(sub_question)

	segments <- segments |>
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
							}
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
							}
						) |>
						dplyr::filter(!(is_mid & xmax <= 0))
				}

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
		ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
		ggplot2::theme(legend.position = "bottom")

	if (!is.null(split_by)) p <- p + ggplot2::facet_wrap(~ split_value)

	p
}