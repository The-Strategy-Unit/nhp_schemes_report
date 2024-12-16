purrr::walk(list.files("R/nhp_outputs", ".R$", , TRUE, TRUE), source)

prepare_all_principal_change_factors <- function(
    r,
    site_codes = list(ip = NULL,  op = NULL, aae = NULL)  # character vectors
) {
  
  
  mitigators_lookup <- read_mitigators()
  atmpo_lookup <- read_atmpo()
  
  
  activity_types_long <- list("inpatients", "outpatients", "aae")
  activity_types_short <- list("ip", "op", "aae")
  
  
  pods <- purrr::map(
    activity_types_long,
    \(x) {
      atmpo_lookup |>
        dplyr::filter(activity_type == x) |>
        dplyr::pull(pod) |>
        unique()
    }
  ) |>
    purrr::set_names(activity_types_short)
  
  
  possibly_prep_principal_change_factors <-
    purrr::possibly(prep_principal_change_factors)
  
  
  principal_change_data <- purrr::map2(
    activity_types_short,
    pods,
    \(activity_type, pod) {
      possibly_prep_principal_change_factors(
        data = r,
        site_codes = site_codes,
        mitigators = mitigators_lookup,
        at = activity_type,
        pods = pod
      )
    }
  ) |>
    purrr::set_names(activity_types_short)
  
  
  # Scenarios run under v1.0 don't have A&E data when filtering by site, so
  # provide results for whole-scheme level.
  if (r$params$app_version == "v1.0" & !is.null(site_codes[["aae"]])) {
    principal_change_data[["aae"]] <-
      possibly_prep_principal_change_factors(
        data = r,
        site_codes = list(aae = NULL),  # provide for whole scheme, not site
        mitigators = mitigators_lookup,
        at = "aae",
        pods = pods[["aae"]]
      )
  }
  
  
  principal_change_data
  
  
}

read_mitigators <- function(remove_codes = TRUE) {
  
  mitigators <- "data/mitigators.json" |>
    jsonlite::read_json(simplifyVector = TRUE) |>
    purrr::simplify() |>
    tibble::enframe("strategy", "mitigator_name")
  
  if (remove_codes) {
    mitigators = mitigators |>
      dplyr::mutate(
        mitigator_name = stringr::str_remove(
          mitigator_name,
          " \\(\\w{2}-\\w{2}-\\d{3}\\)"
        )
      )
  }
  
  mitigators
  
}

read_atmpo <- function() {
  get_activity_type_pod_measure_options() |>
    dplyr::select("activity_type", "pod", "measure" = "measures") |>
    dplyr::mutate(
      activity_type = dplyr::case_match(
        activity_type,
        "ip"  ~ "inpatients",
        "op"  ~ "outpatients",
        "aae" ~ "aae"
      )
    )
}


get_stepcounts <- function(r_trust) {
  r_trust[["results"]][["step_counts"]]
} 



output <- "scheme_output_jsons/RCF.json" |>
  jsonlite::read_json() |>
  parse_results()

years_to_forecast <- as.numeric(
  output[["params"]][["end_year"]] - output[["params"]][["start_year"]]
)


pcfs <- prepare_all_principal_change_factors(
  r = output)

table <- as.data.frame(dplyr::bind_rows(pcfs))


# Figure 8.1
# Items 7 to 18
# Items 7 & 9 - admissions
baseline_admissions <- table |>
  dplyr::filter(measure == "admissions") |>
  dplyr::filter(change_factor == "baseline") |>
  # filter_sites_conditionally(site_codes$ip) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

activity_avoidance_admissions <- table |>
  dplyr::filter(measure == "admissions") |>
  dplyr::filter(change_factor == "activity_avoidance") |>
  # filter_sites_conditionally(site_codes$ip) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

efficiencies_admissions <- table |>
  dplyr::filter(measure == "admissions") |>
  dplyr::filter(change_factor == "efficiencies") |>
  # filter_sites_conditionally(site_codes$ip) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

item_7 <- janitor::round_half_up((((baseline_admissions + activity_avoidance_admissions) / baseline_admissions)^(1 / years_to_forecast) - 1) * 100, digits = 2)
item_9 <- janitor::round_half_up((((baseline_admissions + efficiencies_admissions) / baseline_admissions)^(1 / years_to_forecast) - 1) * 100, digits = 2)

# only display value when there is one. If it is 0 then the relevant mitigators weren't set and therefore it is N/A
item_9 <- ifelse(item_9 == 0, "N/A", item_9)

# Items 8 & 10 - beddays
baseline_beddays <- table |>
  dplyr::filter(measure == "beddays") |>
  dplyr::filter(change_factor == "baseline") |>
  # filter_sites_conditionally(site_codes$ip) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

activity_avoidance_beddays <- table |>
  dplyr::filter(measure == "beddays") |>
  dplyr::filter(change_factor == "activity_avoidance") |>
  # filter_sites_conditionally(site_codes$ip) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

efficiencies_beddays <- table |>
  dplyr::filter(measure == "beddays") |>
  dplyr::filter(change_factor == "efficiencies") |>
  # filter_sites_conditionally(site_codes$ip) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

item_8 <- janitor::round_half_up((((baseline_beddays + activity_avoidance_beddays) / baseline_beddays)^(1 / years_to_forecast) - 1) * 100, digits = 2)
item_10 <- janitor::round_half_up((((baseline_beddays + efficiencies_beddays) / baseline_beddays)^(1 / years_to_forecast) - 1) * 100, digits = 2)

item_11 <- janitor::round_half_up((((baseline_admissions + activity_avoidance_admissions + efficiencies_admissions) / baseline_admissions)^(1 / years_to_forecast) - 1) * 100, digits = 2)
item_12 <- janitor::round_half_up((((baseline_beddays + activity_avoidance_beddays + efficiencies_beddays) / baseline_beddays)^(1 / years_to_forecast) - 1) * 100, digits = 2)

# outpatients
baseline_op <- table |>
  dplyr::filter(activity_type == "op") |>
  dplyr::filter(change_factor == "baseline") |>
  # filter_sites_conditionally(site_codes$op) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

activity_avoidance_op <- table |>
  dplyr::filter(activity_type == "op") |>
  dplyr::filter(change_factor == "activity_avoidance") |>
  # filter_sites_conditionally(site_codes$op) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

efficiencies_op <- table |>
  dplyr::filter(activity_type == "op") |>
  dplyr::filter(change_factor == "efficiencies") |>
  # filter_sites_conditionally(site_codes$op) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()


# item13
item_13 <- janitor::round_half_up((((baseline_op + activity_avoidance_op) / baseline_op)^(1 / years_to_forecast) - 1) * 100, digits = 2)

# item14
item_14 <- janitor::round_half_up((((baseline_op + efficiencies_op) / baseline_op)^(1 / years_to_forecast) - 1) * 100, digits = 2)

# only display value when there is one. If it is 0 then the relevant mitigators weren't set and therefore it is N/A
item_14 <- ifelse(item_14 == 0, "N/A", item_14)



# item15
item_15 <- janitor::round_half_up((((baseline_op + activity_avoidance_op + efficiencies_op) / baseline_op)^(1 / years_to_forecast) - 1) * 100, digits = 2)


# a&e

baseline_ae <- table |>
  dplyr::filter(activity_type == "aae") |>
  dplyr::filter(change_factor == "baseline") |>
  # filter_sites_conditionally(site_codes$aae) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

activity_avoidance_ae <- table |>
  dplyr::filter(activity_type == "aae") |>
  dplyr::filter(change_factor == "activity_avoidance") |>
  # filter_sites_conditionally(site_codes$aae) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

efficiencies_ae <- table |>
  dplyr::filter(activity_type == "aae") |>
  dplyr::filter(change_factor == "efficiencies") |>
  # filter_sites_conditionally(site_codes$aae) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::pull()

# item16
item_16 <- janitor::round_half_up((((baseline_ae + activity_avoidance_ae) / baseline_ae)^(1 / years_to_forecast) - 1) * 100, digits = 2)

# item17
item_17 <- janitor::round_half_up((((baseline_ae + efficiencies_ae) / baseline_ae)^(1 / years_to_forecast) - 1) * 100, digits = 2)

# only display value when there is one. If it is 0 then the relevant mitigators weren't set and therefore it is N/A
item_17 <- ifelse(item_17 == 0, "N/A", item_17)

# item18
item_18 <- janitor::round_half_up((((baseline_ae + activity_avoidance_ae + efficiencies_ae) / baseline_ae)^(1 / years_to_forecast) - 1) * 100, digits = 2)


# change factors plots ----------------------------------------------------

prepare_all_principal_change_factors_plots <- function(
    r,
    site_codes = list(ip = NULL,  op = NULL, aae = NULL),  # character vectors
    pcf
) {
  
  
  pcf_data <- prepare_all_principal_change_factors(r, site_codes)
  
  
  dats <- list(pcf_data$ip, pcf_data$ip, pcf_data$ip, pcf_data$op, pcf_data$aae)
  measures <- list("admissions", "beddays", "beddays", "attendances", "arrivals")
  change_factors <- list(
    "activity_avoidance",
    "activity_avoidance",
    "efficiencies",
    "activity_avoidance",
    "activity_avoidance"
  )
  
  
  possibly_plot_individual_change_factors <-
    purrr::possibly(plot_individual_change_factors)
  
  
  purrr::pmap(
    list(dats, measures, change_factors),
    \(dat, measure, change_factor) {
      dat |>
        possibly_plot_individual_change_factors(
          measure = measure,
          change_factor = change_factor
        )
    }
  )
  
  
}

plots_data <- prepare_all_principal_change_factors_plots(output)

purrr::walk2(
  paste0(fig_dir, "/figure_8.", 2:6, ".png"),
  plots_pcf,
  \(filename, plot) {
    ggplot2::ggsave(filename, plot, width = w, height = h)
    cli::cli_text("- Wrote to {filename}")
  }
