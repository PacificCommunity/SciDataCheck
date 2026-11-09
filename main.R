###################################################
## Validate files against Scidata templates      ## 
##                                               ##  
## Author: SPC                                   ##
## Year: 2026                                    ##
###################################################

# 1. Load libraries ####
rm(list = ls())
source("utils.R")

# 2. Configuration ####
# it should be submissions/year/country/data_files
submission_year <- 2027         # e.g. c(2025, 2026), or NULL for all years present
country_code    <- "KR"      # e.g. c("ca", "eu"), or NULL for all countries present
create_folder_structure(submission_year, country_code)

# 3. Validate files ####
df_files <- build_file_inventory(years = submission_year, 
                                 country_codes = country_code)

write_rds(df_files, "output/df_files.rds") # save results - required for audit
gt(df_files)

res <- validate_files(df_files) 
write.csv(res, "output/log_validation.csv", row.names = FALSE) # save results - required for audit

# 4. Generate report ####
#timerun = Sys.Date()
quarto::quarto_render(
  input          = "validation_report.qmd"#,
  # execute_params = list(last_run = as.character(timerun))
)



