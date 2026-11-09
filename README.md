# Scidata File Validation Workflow

This workflow validates CSV data files against the Scidata templates via the validation API, and generates an HTML report summarising the results.

------------------------------------------------------------------------

## TL;DR

1.  Create a `{cc}_files/` folder (e.g. `ph_files/`) and paste your CSV files inside

2.  Check `input/mapping_file_pattern.csv` — update file_pattern to match your csv filenames

    *2.1* At the end of each filename add: "\_"{fishing_year} (for example, for the 2026 submission we are expecting all the data from 2025 fishing year, so your ace csv file should be named something like: `ace_2025.csv`)

3.  Open `main.R` and set country_codes \<- e.g.: c("PH")

4.  Run `main.R` — results and report will be saved to `output/`

------------------------------------------------------------------------

## Requirements

-   R (≥ 4.1)
-   Packages: `tidyverse`, `quarto`, `gt`, `glue`
-   Quarto CLI installed
-   Access to the Scidata validation API

Install packages if needed:

``` r
install.packages(c("tidyverse", "quarto", "gt", "glue"))
```

------------------------------------------------------------------------

## Folder structure

Keep all files you want to validate inside folder **'{cc}\_files/'**. After copying or cloning this workflow, create a **'{cc}\_files/'** where **cc** should be your country_code ("jp", "us", "kr", ...). Paste all the scidata csvs you would like to validate inside this folder. You can create multiple folders in case you need to submit data for multiple CCMs.

```         
project/
├── main.R
├── utils.R
├── validation_report.qmd
├── input/
│   └── mapping_file_pattern.csv   # template mapping file (see below)
├── {cc}_files/                    # one folder per country code, e.g. jp_files/    
│   ├── ace_2023.csv
│   └── trip_ps_2023.csv
│   └── trip_ll_2023.csv
└── output/                        # reports and logs written here
```

> **Single country setup:** If you only have data for one country, you still need a country-code folder. For example, if your country code is `PH`, create a folder called `ph_files/` and place your CSV files there. In `main.R`, set `country_codes <- c("PH")`.

------------------------------------------------------------------------

## The mapping file

The file `input/mapping_file_pattern.csv` tells the workflow which files to validate and which template to validate them against. It must contain exactly three columns:

| Column | Description |
|------------------------------------|------------------------------------|
| `file_pattern` | The filename prefix used to identify files belonging to this template |
| `template_name` | The exact template name as registered in the Scidata API |
| `data_submission` | The exact Data Type in the Scidata Submission Portal |

*Please, only modify the `file_pattern` column to match the pattern you used when saving your .csv files with the corresponding Data Type in the Scidata Submission Portal.*

### Example

| file_pattern   | template_name    | data_submission                   |
|----------------|------------------|-----------------------------------|
| catch_estimate | ace              | Annual Catch Estimates            |
| ll_agg_vessel  | ce_agg_vessel_ll | Aggregated Vessels - Longline     |
| ll_agg_effort  | ce_agg_effort_ll | Aggregated Effort - Longline      |
| pl_agg_effort  | ce_agg_effort_pl | Aggregated Effort - Pole-and-Line |

With this mapping, any file in a `{cc}_files/` folder whose name starts with `catch_estimate` will be validated against the `ace` template which is used in the Scidata Portal to validate the Annual Catch Estimates submissions.

### How file matching works in the main.R script

The workflow looks for files matching `{file_pattern}_` at the start of the filename. The remainder of the filename (typically a year) is parsed as the `submission_year`. For example:

```         
catch_estimate_2023.csv → Annual Catch Estimates submission, year: 2023
ll_agg_vessel_2022.csv  → Aggregated Vessels - Longline submission, year: 2022
```

Files that do not match any pattern in the mapping file are silently skipped.

------------------------------------------------------------------------

## Configuration

Open `main.R` and update the following before running:

``` r
country_codes  <- c("PH")          # specify you country code
# country_codes <- c("JP", "ID", "PH", "US")  # or multiple
```

------------------------------------------------------------------------

## Running the workflow

Run `main.R` in full. It will:

1.  Read the mapping file
2.  Scan each `{cc}_files/` folder and match files to templates
3.  Submit each file to the validation API
4.  Save raw results to `output/log_validation.csv`
5.  Render `validation_report.qmd` and save the HTML report to `output/validation_report_{date}.html`

------------------------------------------------------------------------

## Output

| File | Description |
|------------------------------------|------------------------------------|
| `output/log_validation.csv` | Raw validation results, one row per error per file |
| `output/validation_report_{date}.html` | Self-contained HTML report — can be shared without any other files |

### Report sections

-   **Overview** — total files validated, pass/fail counts
-   **By country** — pass/fail breakdown per country (only countries with failures shown)
-   **Error summary** — which checks and columns are generating the most failures across all files
-   **Error detail by country** — file-level breakdown of every failure case

------------------------------------------------------------------------

## Troubleshooting

**No files found** Check that your folder is named correctly — it must be `{lowercase country code}_files/`. For example, country code `PH` → folder `ph_files/`.

**File matched but no submission year** The workflow uses `readr::parse_number()` to extract the year from the filename. Make sure your filenames include a 4-digit year, e.g. `catch_estimate_2023.csv`.

**Quarto report fails to render** Make sure Quarto CLI is installed (`quarto --version` in a terminal) and that `output/log_validation.csv` exists before rendering.
