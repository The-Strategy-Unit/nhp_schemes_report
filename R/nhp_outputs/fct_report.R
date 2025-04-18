#' Create a Table of Model Run Metadata
#' @param p A list. Parameter selections for a given model scenario.
#' @noRd
tabulate_model_run_info <- function(p) {
  p_model_run <- purrr::keep(p, rlang::is_atomic)

  p_model_run[["start_year"]] <- scales::number(
    p_model_run[["start_year"]] + ((p_model_run[["start_year"]] + 1) %% 100) / 100,
    0.01,
    big.mark = "", decimal.mark = "/"
  )

  p_model_run[["end_year"]] <- scales::number(
    p_model_run[["end_year"]] + ((p_model_run[["end_year"]] + 1) %% 100) / 100,
    0.01,
    big.mark = "",
    decimal.mark = "/"
  )

  p_model_run[["create_datetime"]] <- p_model_run[["create_datetime"]] |>
    lubridate::fast_strptime("%Y%m%d_%H%M%S") |>
    format("%d-%b-%Y %H:%M:%S")

  p_model_run |>
    unlist() |>
    tibble::enframe() |>
    gt::gt("name") |>
    gt_theme_param_diffs()
}

#' Prepare Individual Change Factors Data
#' @param principal_change_factors Data.frame. Prepared data for prinicpal
#'     change factors, derived using [prep_principal_change_factors].
#' @param measure Character. A selected measure (e.g. 'beddays').
#' @details Used by [plot_individual_change_factors].
#' @noRd
prep_individual_change_factors <- function(
    principal_change_factors,
    measure
) {

  principal_change_factors |>
    dplyr::filter(
      .data$measure == .env$measure,
      .data$strategy != "-",
      .data$value < 0
    ) |>
    dplyr::mutate(
      dplyr::across(
        "mitigator_name",
        \(.x) forcats::fct_reorder(.x, -.data$value)
      )
    )

}

#' Plot Individual Change Factors Data
#' @param principal_change_factors Data.frame. Prepared data for principal
#'     change factors, derived using [prep_principal_change_factors].
#' @param measure Character. A selected measure (e.g. 'beddays').
#' @param change_factor Character. The change factor grouping (e.g.
#'     activity_avoidance').
#' @details Used by [plot_impact_and_individual_change].
#' @noRd
plot_individual_change_factors <- function(
    principal_change_factors,
    measure,
    change_factor
) {

  individual_change_factors <-
    prep_individual_change_factors(principal_change_factors, measure) |>
    dplyr::filter(change_factor == .env$change_factor)

  mod_principal_change_factor_effects_ind_plot(
    individual_change_factors,
    change_factor,
    "#f9bf07",
    snakecase::to_title_case(change_factor),
    snakecase::to_title_case(measure)
  )

}

#' Plot Activity Distribution Charts (Beeswarm and S-curve)
#' @param data List. Contents of the JSON generated by the inputs app.
#' @param sites Character. Location codes selected by the user from the outputs
#'     app interface.
#' @param activity_type Character. The activity type (e.g. 'ip' for inpatients).
#' @param pod Character. The point of delivery (e.g.
#'     'ip_non-elective_admission').
#' @param measure Character. A selected measure (e.g. 'beddays').
#' @noRd
plot_activity_distributions <- function(
    data,
    site_codes,
    activity_type,
    pod,
    measure
) {

  # Convert activity type to abbreviated form and filter for the set of site
  # codes specific to this activity type
  activity_type_short <-
    switch(activity_type, "inpatients" = "ip", "outpatients" = "op", "aae")
  site_codes <- site_codes[[activity_type_short]]

  selected_measure <- c(activity_type, pod, measure)

  aggregated_data <- data |>
    mod_model_results_distribution_get_data(selected_measure, site_codes) |>
    require_rows()

  beeswarm_plot <- mod_model_results_distribution_beeswarm_plot(
    aggregated_data,
    show_origin = FALSE
  )

  ecdf_plot <- mod_model_results_distribution_ecdf_plot(
    aggregated_data,
    show_origin = FALSE
  )

  dplyr::lst(beeswarm_plot, ecdf_plot)

}

#' Prepare Principal Change Factors Data
#' @param data List. Contents of the JSON generated by the inputs app.
#' @param site_codes List of character vectors. Location codes selected by the
#'     user from the outputs app interface.
#' @param mitigators Data.frame. A lookup from mitgator codes to huamn-readable
#'     names. Derived from the internal mitigators.json file.
#' @param at Character. The activity type (e.g. 'ip' for inpatients).
#' @param pods Character. The point of delivery (e.g.
#'     'ip_non-elective_admission').
#' @details This data will be used in the waterfall chart.
#' @noRd
prep_principal_change_factors <- function(
    data,
    site_codes,
    mitigators,
    at,
    pods
) {

  # Filter for the set of site codes specific to this activity type
  site_codes <- site_codes[[at]]

  principal_change_factors_raw <- data |>
    get_principal_change_factors(at, site_codes)

  # if a site is selected then there are no rows for A&E
  if (nrow(principal_change_factors_raw) == 0) stop("No data")

  principal_change_factors_raw |>
    dplyr::mutate(
      dplyr::across("change_factor", forcats::fct_inorder),
      dplyr::across(
        "change_factor",
        \(.x) {
          forcats::fct_relevel(
            .x,
            "baseline",
            "demographic_adjustment",
            "health_status_adjustment"
          )
        }
      )
    ) |>
    dplyr::left_join(
      mitigators,
      by = dplyr::join_by("strategy")
    ) |>
    tidyr::replace_na(list("mitigator_name" = "-")) |>
    dplyr::filter(.data[["pod"]] %in% pods) |>
    dplyr::select(-"pod") |>
    dplyr::count(
      dplyr::across(-"value"),
      wt = .data[["value"]],
      name = "value"
    )

}
