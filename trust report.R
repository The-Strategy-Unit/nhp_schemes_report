# purpose is a report for the NHP on the general state of play re schemes'
# inputs to the NHP D&C model

library(dplyr)
library(here)
library(ggplot2)
library(ggrepel)

# establish a connection to the board containing the data
board <- pins::board_connect()

# loading the historical data
historical_mitigators_data <- pins::pin_read(
  board, "thomas.jemmett/inputs_app_rates_data_v2-1")

# loading the app inputs from pin
nhp_tagged_runs_params <- pins::pin_read(
  board, "matt.dray/nhp_tagged_runs_params")

# loading the metadata from pin
nhp_tagged_runs_meta <- pins::pin_read(
  board, "matt.dray/nhp_tagged_runs_meta")

# loading the NEE data
nee_results <- readRDS(here("data","nee_table.Rds"))

# loading the mitigator lookup
mitigator_lookup <- read.csv(
  here(
    "data",
    "mitigator-lookup.csv"), 
  check.names = FALSE)

# loading the trust code lookup
trust_code_lookup <- read.csv(
  here(
    "data",
    "nhp-scheme-lookup.csv"), 
  check.names = FALSE) 

# load the functions which are defined for the app developed by Data Science team
# https://github.com/The-Strategy-Unit/nhp_inputs_report_app/blob/main/R/fct_tabulate.R

source("fct tabulate.R")

# derive the cleaned data frame (same as that being used for Shiny app)
extracted_params <- extract_params(nhp_tagged_runs_params, nhp_tagged_runs_meta)
skeleton_table <- prepare_skeleton_table(extracted_params)

dat <- populate_table(
  skeleton_table,
  extracted_params,
  trust_code_lookup,
  mitigator_lookup,
  nee_results
)

# attempt at scatter plot -------------------------------------------------

# get the baseline data
baseline <- historical_mitigators_data |> 
  filter(fyear == 201920)

baseline_falls_related_admissions <- baseline |> 
  filter(strategy == "falls_related_admissions")

# get the scheme inputs
falls_related_admissions_inputs <- dat |> 
  filter(mitigator_variable == "falls_related_admissions") 

# cross-reference the baseline value and scheme inputs
falls_related_admissions_combined_data <- left_join(
  falls_related_admissions_inputs,
  baseline_falls_related_admissions,
  by = c("scheme_code" = "procode"))

ggplot(falls_related_admissions_combined_data, aes(x = value_mid, y = rate)) +
  geom_point() +
  xlab("scheme inputs midpoint (% of baseline)") +
  ylab("scheme baseline rate") +
  ggtitle("scatter plot comparison of scheme baseline values and inputs for falls related admissions") +
  geom_text_repel(aes(label = scheme_code), size = 4)

