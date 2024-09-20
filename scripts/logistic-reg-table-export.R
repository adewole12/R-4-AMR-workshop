# Load necessary libraries (assuming these are already installed)
library(gtsummary)
library(huxtable)
library(dplyr)
library(officer) # for exporting to Word
library(ftExtra)

# Generate descriptive statistics alongside univariate regression with enhancements
region_model_summary <- amrData %>%
  # Select relevant columns for summary statistics
  select(age_years_sd, sex_male, region, had_surgery_past_yr, ethnicity, imd) %>%
  # Create summary statistics (mean and SD for continuous variables)
  tbl_summary(
    missing = "no", 
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    label = list(
      age_years_sd = "Age (years, standardized)",
      sex_male = "Male",
      had_surgery_past_yr = "Had surgery in the past year",
      ethnicity = "Ethnicity",
      imd = "Index of Multiple Deprivation (IMD)"
    )
  ) %>%
  # Add a column for sample size (n) for each variable
  add_n() %>%
  # Modify header to label the statistics column
  modify_header(stat_0 ~ "**Summary Statistics**") %>%
  # Add footnote for missing data treatment, if necessary
  modify_footnote(everything() ~ "No missing data included in the analysis")

# Run the logistic regression and store the output
region_model <- tbl_regression(
  cipro_region_logit,  # logistic regression model
  exponentiate = TRUE  # present odds ratios (exp(coefficients))
)

# Merge the summary table and regression results, removing any spanning headers
final_table <- tbl_merge(
  tbls = list(region_model_summary, region_model)
) %>%
  # Remove spanning headers by setting them to NA
  modify_spanning_header(everything() ~ NA_character_) %>%
  # Correctly modify the formatting of p-values using the new column name
  modify_fmt_fun(p.value_2 ~ style_pvalue)

# View or save the final merged table
final_table


# Convert gtsummary table to huxtable
hux_table <- as_hux_table(final_table)

# Customize table appearance (optional)
width(hux_table) <- 0.8  # set table width
bold(hux_table)[1,] <- TRUE  # bold the first row (headers)
align(hux_table) <- 'center'  # center-align content

# Export the huxtable as a Word document
quick_docx(hux_table, file = "region_model_summary.docx")


