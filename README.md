# Data and analysis of SIB training needs survey in 2026

The SIB Training Group delivers 60+ training events annually across academic, industry, and healthcare sectors. To ensure our future portfolio remains relevant, impactful, and accessible, we conducted a training needs survey, which ran from March 18th to April 30th 2026.

The goal was to identify emerging scientific topics of interest, preferred learning formats, and common structural/organizational barriers to participation.​ With that it directly informs the strategic design of the 2027 SIB Training programme. 

Feel free to reuse the questions, results and/or analysis. If you do, please take into consideration the CC-BY license and acknowledge us by citing:

DiCola, V., Zahn, M., Marek, D., Rossier, G., Duchemin, W., Wyss, T., Carlevaro-Fita, J., Tanwar, D., Vaucher, M., Palagi, P., & van Geest, G. (2026). SIB training survey 2026 - data and analysis. Zenodo. https://doi.org/10.5281/zenodo.20446694

## Repository structure

This repository is organized so the underlying method — survey design and graph-based clustering — can be reused independently of the SIB-specific manuscript and data:

- [`survey/`](survey/) — the survey instrument: question metadata/scales (`question_metadata.json`) and grouped-question stems. See [`survey/README.md`](survey/README.md) for the schema and notes on adapting the survey design for another institution.
- [`data/`](data/) — anonymized responses (`supplementary_data1.xlsx`), question label abbreviations, and clustering output.
- [`R/`](R/) — the reusable pipeline: data preprocessing (`data_prep.R`), graph-based clustering (`clustering.R`), and plotting helpers (`plotting.R`).
- [`example/`](example/) — a runnable template demonstrating the clustering pipeline end-to-end; see [`example/README.md`](example/README.md) for how to adapt it to your own survey.
- [`manuscript/`](manuscript/) — manuscript source, figures, and submission documents.
- `survey_analysis.qmd` / `functions.R` — a separate, exploratory analysis report (not part of the reusable pipeline above).

## Setup

- Raw report quarto markdown file: `survey_analysis.qmd`
- Manuscript source: `manuscript/manuscript.qmd`

Find the full analysis report at [sib-swiss.github.io/analyze_training_needs_survey](https://sib-swiss.github.io/analyze-training-needs-survey/).