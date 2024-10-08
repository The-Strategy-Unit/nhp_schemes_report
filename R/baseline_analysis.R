library(here)
library(dplyr)
library(janitor)
board <- pins::board_connect()

# loading the historical data
historical_mitigators_data <- pins::pin_read(
  board, "thomas.jemmett/inputs_app_rates_data_v2-1")


# loading the mitigator lookup
mitigator_lookup <- read.csv(
  here(
    "data",
    "mitigator-lookup.csv"), 
  check.names = FALSE) |> 
  clean_names()

# tagged runs 
nhp_tagged_runs_meta <- pins::pin_read(
  board, "matt.dray/nhp_tagged_runs_meta")

# loading the trust code lookup
trust_code_lookup <- read.csv(
  here(
    "data",
    "nhp-scheme-lookup.csv"), 
  check.names = FALSE) |> 
  distinct() |> 
  clean_names()


# wrangling ---------------------------------------------------------------

# create the baseline data and filter to the 28 schemes
baseline <- historical_mitigators_data |> 
  filter(fyear==201920,
         procode %in% nhp_tagged_runs_meta$dataset)

baseline <- baseline |> 
  left_join(trust_code_lookup, by = c("procode" = "trust_ods_code")) |> 
  left_join(mitigator_lookup, by = c("strategy" = "strategy_variable")) |> 
  select(fyear, 
         procode, 
         name_of_trust, 
         name_of_hospital_site, 
         activity_type, 
         mitigator_type,
         grouping, 
         strategy_name,
         rate, 
         n)

write.csv(baseline, "baseline_mitigators_data.csv")


# try multuplying the rate / 100 by the n
baseline <- baseline |> mutate(
  baseline_rate_times_n = baseline_rate * baseline_n / 100)


# it looks like "n" is the denominator (i.e. all the activity in scope) and "rate" 
# is the percentage (needing to be divided by 100)

# if we take the combined data of baseline and inputs we can check against the 
# output reports

baseline_inputs_data_ <- baseline_inputs_data |> 
  mutate(baseline_rate_n = baseline_rate / 100 * baseline_n,
         activity_reduction = (1 - value_mid) * baseline_rate_n) |> 
  select(scheme_name, scheme_code, mitigator_name:mitigator_group, value_mid, baseline_rate:activity_reduction)
           


# yaml lookup golem -------------------------------------------------------

# there is a yaml file, shared by Tom that is actually the lookup for the 
# baseline

yaml <- yaml::read_yaml(
  "data/golem-config.yml"
)

activity_mitigator_yaml_lookup <- yaml$default$mitigators_config

historical_mitigators_data |> 
  filter(procode=="R1H", strategy=="alcohol_partially_attributable_acute") |>
  mutate(year=as.numeric(stringr::str_sub(fyear,1,4))) |> 
  ggplot(aes(x=year, y=rate)) +
  geom_line() +
  ylab(activity_mitigator_yaml_lookup[["mitigators_admission_avoidance"]][["y_axis_title"]]) +
  xlab(activity_mitigator_yaml_lookup[["mitigators_admission_avoidance"]][["x_axis_title"]])

# as an example, reviewing the output report for hillingdon, the most significant 
# mitigator for avoiding inpatient admissions is Medically Unexplained Symptoms 
# Admissions - just over 400

# this is rate per 1,000

# taking hillingdon
hillingdon_medically_unexplained_related_admissions <- baseline |> 
  filter(procode=="RAS",
         strategy=="medically_unexplained_related_admissions") |> 
  mutate(admissions = rate * n / 1000)

# get the input

nhp_tagged_runs_params <- pins::pin_read(
  board, "matt.dray/nhp_tagged_runs_params")

# reviewing the values are 0.618 and 0.8 
hillingdon_medically_unexplained_related_admissions_input <- mean(0.618, 0.8)

hillingdon_medically_unexplained_related_admissions <- hillingdon_medically_unexplained_related_admissions |> 
  mutate(model_input = hillingdon_medically_unexplained_related_admissions_input,
         change = ((model_input - 1) * admissions))

# -650 close enough - there's also standardisiation to deal with...

# -- note there will be a way of systematically doing this:
# hillingdon_medically_unexplained_related_admissions_input <- mean(
#   nhp_tagged_runs_params$RAS$activity_avoidance$ip$medically_unexplained_related_admissions$interval[[1]],
#   nhp_tagged_runs_params$RAS$activity_avoidance$ip$medically_unexplained_related_admissions$interval[[2]])
  

# NOTE: try going into the yaml, collect all the strategies in one of the
# sublists, then review the baseline data -- presumably the n will be the same 
# for all of them


 
  

