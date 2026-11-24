# Scidata File Validation Workflow

This workflow validates CSV data files against the Scidata templates via the validation API, and generates an HTML report summarising the results.


# Installation

``` r
# install.packages("pak")
pak::pak("PacificCommunity/SciDataCheck")
library(SciDataCheck)
```

# Usage

## Basic usage

There is one primary function of this package; the `validate_data()` function. This argument has a single optional argument of `year`. But default the previous year from the current date is used. 

``` r
validate_data()
```

If this is the first time running the function. It will generate the folder structure that you will need to populate your *.csv* data files with. 

Once you have populated the folders with the appropriate *.csv* data files, you can re-run the `validate_data()` function.

## Extra

If you would like to validate data from a year other than the previous year (e.g., validate data from the current year), then you can specify the year as:

``` r
validate_data(year = 2026)
```

# Output

| File | Description |
|------------------------------------|------------------------------------|
| `{year}/output/log_validation.csv` | Raw validation results, one row per error per file |
| `{year}/output/validation_report_{date}.html` | Self-contained HTML report — can be shared without any other files |

## Report sections

-   **Overview** — total files validated, pass/fail counts
-   **By country** — pass/fail breakdown per country (only countries with failures shown)
-   **Error summary** — which checks and columns are generating the most failures across all files
-   **Error detail by country** — file-level breakdown of every failure case

------------------------------------------------------------------------

## Troubleshooting

**No files found** Check that your folder is named correctly — it must be `{lowercase country code}_files/`. For example, country code `PH` → folder `ph_files/`.

**File matched but no submission year** The workflow uses `readr::parse_number()` to extract the year from the filename. Make sure your filenames include a 4-digit year, e.g. `catch_estimate_2023.csv`.

**Quarto report fails to render** Make sure Quarto CLI is installed (`quarto --version` in a terminal) and that `output/log_validation.csv` exists before rendering.
