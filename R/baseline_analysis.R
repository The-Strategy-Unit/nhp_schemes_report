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
         strategy,
         strategy_name,
         rate, 
         n)

write.csv(baseline, "baseline_mitigators_data.csv")



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
         strategy_name=="Medically Unexplained Symptoms Admissions") |> 
  mutate(admissions = rate * n / 1000)

# get the input

nhp_tagged_runs_params <- pins::pin_read(
  board, "matt.dray/nhp_tagged_runs_params")

# get the central value
hillingdon_medically_unexplained_related_admissions_input <- mean(
  c(
    nhp_tagged_runs_params$RAS$activity_avoidance$ip$medically_unexplained_related_admissions$interval[[1]], 
    nhp_tagged_runs_params$RAS$activity_avoidance$ip$medically_unexplained_related_admissions$interval[[2]])
  )

hillingdon_medically_unexplained_related_admissions <- hillingdon_medically_unexplained_related_admissions |> 
  mutate(model_input = hillingdon_medically_unexplained_related_admissions_input,
         change = ((model_input - 1) * admissions))

# -495 - close enough - there's also standardisiation to deal with...


# NOTE: try going into the yaml, collect all the strategies in one of the
# sublists, then review the baseline data -- presumably the n will be the same 
# for all of them

library(purrr)

# Abbreviate the path to the mitigators_config list
mitigator_yaml <- yaml$default$mitigators_config

# Use purrr to map over the list of categories and create the data frame
mitigator_yaml_df <- map_df(names(mitigator_yaml), function(category_name) {
  
  # Extract the relevant components for each category
  activity_type <- mitigator_yaml[[category_name]]$activity_type
  mitigator_type <- mitigator_yaml[[category_name]]$mitigators_type
  strategy_variable <- names(mitigator_yaml[[category_name]]$strategy_subset)
  y_axis_title <- mitigator_yaml[[category_name]]$y_axis_title
  
  # Return a data frame with category, element, and y_axis_title
  tibble(
    activity_type = activity_type,
    mitigator_type = mitigator_type,
    category = category_name,
    strategy_variable = strategy_variable,
    y_axis_title = y_axis_title
  )
})

mitigators_admission_avoidance <- names(yaml$default$mitigators_config$mitigators_admission_avoidance$strategy_subset)

baseline |> filter(strategy %in% mitigators_admission_avoidance,
                   procode == "R1H")

#there are 33 (as expected) and all have the same n - thus the grouping that 
# mitigator falls into in the yaml appears to determine the count we are 
# considering

# QUESTION: where are the groupings in the yaml coming from?
# suspect that they are the unique combinatons of activity type and mitigator 
# type in the mitigator lookup...
unique(expand.grid(mitigator_lookup$activity_type, mitigator_lookup$mitigator_type))

# nope there are only 3 unique activity types (ip, op, and aae) and two mitigator
# mitigator types (avoidance and efficiencies) -- so 6 unique combinations


# quantifying the materiality of mitigators -------------------------------

# to summarise:
# - from the historical mitigators data (2008-09 to 2022-23) we extract the 
#   data for 2019-20 which constitutes the baseline of the NHP D&C model
# - this has one row per trust and mitigator containing the mitigator's name
#   and the rate as well as the n 
# - the rate is the a standardised ratio where the n is the denominator - thus 
#   the product of the rate and the n is proportional to the actual count of 
#   activity (be that admissions avoided, bed days, outpatient attendances 
#   avoided)
# - the yaml file provides instructions for the NHP D&C inputs model to present
#   these baseline figures, including the y-axis which allows us to determine 
#   what the rate and n are
# - these are unique to each of the categories in 
#   mitigator_yaml <- yaml$default$mitigators_config
# - each of those will partition unique combinations of activity type and
#   mitigator type from the lookup (they're also noted in the yaml) - 
#   i.e. there is a 1:many relation between mitigator:activity combos and the 
#   categories in the yaml

# we get the list of the categories and the y-axis
distinct(mitigator_yaml_df, category, y_axis_title)

# from that, we create a new column in the baseline data which will provide the 
# materialty
baseline <- baseline |> 
  left_join(
    select(mitigator_yaml_df, 
           strategy_variable, 
           category, 
           rate_calculation = y_axis_title), 
    by = c("strategy" = "strategy_variable"))

baseline <- baseline |> 
  mutate(
    materiality = case_when(
      # Admissions per 1,000 population
      category == "mitigators_admission_avoidance" ~ rate * n / 1000,
      
      # Mean Length of Stay (days)
      category == "mitigators_mean_los_reduction" ~ rate * n,
      
      # % admissions with a greater than 0-day length of stay
      category == "mitigators_aec_los_reduction" ~ rate * n,
      
      # Procedures with a Pre-op LoS (per 1000 operations) -- check this one
      category == "mitigators_preop_los_reduction" ~ rate * n / 1000,
      
      # % of Procedures Performed in Non-Target Setting
      category == "mitigators_day_procedures_daycase" ~ rate * n,
      
      # % of Procedures Performed in Non-Target Setting
      category == "mitigators_day_procedures_outpatients" ~ rate * n, 
      
      # % of Appointments that are Consultant to Consultant Referrals
      category == "mitigators_op_c2c_reduction" ~ rate * n,
      
      # % of Appointments that are Face-to-Face
      category == "mitigators_op_convert_tele" ~ rate * n,
      
      # Followup to First Ratio
      category == "mitigators_op_fup_reduction" ~ rate * n,
      
      # % of Attendances that were GP Referred First Attendances
      category == "mitigators_op_gp_referred_first_attendance_reduction" ~ rate * n,
      
      # % of Attendances that were Frequent Attenders
      category == "mitigators_aae_frequent_attenders" ~ rate * n,
      
      # % of Attendances Where the Patient Left Before Being Seen
      category == "mitigators_aae_left_before_seen" ~ rate * n,
      
      # % of Arrivals where the patient was discharged with no treatment or 
      # investigations being performed
      category == "mitigators_aae_discharged_no_treatment" ~ rate * n,
      
      # % of Attendances Which Were Low Cost and Patient Was Discharged Home
      category == "mitigators_aae_low_cost_discharged" ~ rate * n
      )
    )



