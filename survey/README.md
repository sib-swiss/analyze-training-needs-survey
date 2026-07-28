# Survey design

This folder holds the survey instrument as a reusable design artifact, independent of the SIB-specific response data (which lives in [`../data/`](../data/)).

- `question_metadata.json` — describes every question: its exact stem as it appears in the response export, its type, and (for Likert-type questions) its ordinal scale.
- `grouped_questions.txt` — the main-question stems that expand into multiple sub-questions in the response export (e.g. one Likert scale asked once per topic within a domain).

## `question_metadata.json` schema

A `questions` array of objects, each with:

- `main_question_stem` (string, required) — must match the question text as it appears in column headers of the response export (see [`build_question_dictionary()`](../R/data_prep.R), which matches on this stem to split grouped columns into `main_question`/`sub_question` pairs).
- `type` (string, required) — one of:
  - `"likert"` — an ordinal-scale question; requires a `scale` array (ordered low to high). Likert-type stems are the ones matched for column grouping and are what `example/run_clustering_template.R` clusters respondents on.
  - `"single_choice"` — a categorical question; optionally has a `choices` array (used by `get_question_choices()` for demographic breakdowns).
  - `"open_text"` — free text; no scale or choices.

## Adapting this survey design for another institution

The clustering method in [`../R/clustering.R`](../R/clustering.R) only needs: a set of Likert-type question blocks sharing a common scale, asked of every respondent, covering enough distinct topics that respondent-to-respondent correlation is meaningful (this survey used 33 topics across 5 domains). It does not depend on the specific topics, wording, or number of domains — reuse the survey structure (multi-domain Likert topic ratings + a `question_metadata.json` describing it), swap in your own topics, and the pipeline in [`../example/`](../example/) applies unchanged.
