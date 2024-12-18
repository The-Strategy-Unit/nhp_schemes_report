# Load the function copied from the nhp_outputs repo
purrr::walk(list.files("R/nhp_outputs", ".R$", , TRUE, TRUE), source)

# load some of the functions from the final reports repo
source("R/nhp_final_reports_functions.R")

# function to load the data
load_data <- function(scheme_code) {
  
  # Load the JSON
  output = paste0("scheme_output_jsons/", scheme_code, ".json") |>
    jsonlite::read_json() |>
    parse_results()
  
  return(output)
}

calculate_forecast_length <- function(output) {
  output[["params"]][["end_year"]] - output[["params"]][["start_year"]]
}

# function to generate a table of values of the impact
produce_change_factor_df <- function(json_data) {
  # derive the principal change factors
  pcfs = prepare_all_principal_change_factors(
    r = json_data)
  
  # convert to a table
  table = as.data.frame(dplyr::bind_rows(pcfs))
  
  return(table)
}

# derive the growth figures
generate_values_table <- function(table, forecast_length) {
  growth_figures = table |>
    # we want to separate inpatient by admissions and beddays but for outpatient
    # and aae we regard as a monolith
    dplyr::mutate(
      activity_measure = dplyr::case_when(
        measure == "admissions" ~ "ip_admissions",
        measure == "beddays" ~ "ip_beddays",
        activity_type == "op" ~ "op_attendances",
        activity_type == "aae" ~ "aae_attendances",
        TRUE ~ NA_character_
      )
    ) |> 
    dplyr::filter(change_factor %in% c("baseline", "activity_avoidance", "efficiencies")) |> 
    dplyr::summarise(value = sum(value), .by = c(activity_measure, change_factor)) |> 
    tidyr::pivot_wider(names_from = change_factor, values_from = value, values_fill = 0) |> 
    dplyr::mutate(
      all_mitigation = activity_avoidance + efficiencies,
      activity_avoidance_pc = janitor::round_half_up(
        ((baseline + activity_avoidance) / baseline) ^ (1 / forecast_length) - 1, digits = 4),
      efficiencies_pc = janitor::round_half_up(
        ((baseline + efficiencies) / baseline) ^ (1 / forecast_length) - 1, digits = 4),
      all_mitigation_pc = janitor::round_half_up(
        ((baseline + all_mitigation) / baseline) ^ (1 / forecast_length) - 1, digits = 4)
    )
  return(growth_figures)
}

# function to convert table to list for easy quoting in quarto
generate_values_list <- function(values_table) {
  # Create a list of objects for Quarto inline reporting
  growth_figures_list = values_table |>
    dplyr::rowwise() |>
    dplyr::mutate(
      object = list(
        as.list(dplyr::cur_data())
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::pull(object)
  
  # add labels to make it easier to quote
  growth_figures_list <- growth_figures_list |> 
    purrr::set_names(purrr::map_chr(growth_figures_list, "activity_measure"))  
  
  return(growth_figures_list)
}

# calculate general los productivity cagr
calculate_general_los_productivity_cagr <- function(df, forecast_length) {
  baseline_admissions = df |>
    dplyr::filter(measure == "admissions") |>
    dplyr::filter(change_factor == "baseline") |>
    # filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()
  
  baseline_beddays = df |>
    dplyr::filter(measure == "beddays") |>
    dplyr::filter(change_factor == "baseline") |>
    # filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()
  
  admissions = df |>
    dplyr::filter(measure == "admissions") |>
    # filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()
  
  beddays = df |>
    dplyr::filter(measure == "beddays") |>
    # filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()
  
  # baseline_average_los (adjusting for +1 day)
  baseline_avg_los = janitor::round_half_up((baseline_beddays - baseline_admissions) / baseline_admissions, digits = 2)
  
  # the cagr for bau productivity
  bau_productivity_cagr = janitor::round_half_up(((((beddays - admissions) / admissions) / baseline_avg_los)^(1 / forecast_length) - 1), digits = 3)
  
  return(bau_productivity_cagr)
  }
