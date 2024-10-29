library(here)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(purrr)
# Loading data ------------------------------------------------------------

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
  check.names = FALSE) |> 
# Imperial College (RYJ) appears three times due to different hospital
# sites, so simplify to one row
  dplyr::mutate(
    `Name of Hospital site` = dplyr::case_match(
      `Trust ODS Code`,
      'RYJ' ~ 'Imperial',
      .default = `Name of Hospital site`))|>
  # Ensure one row per trust - deals with Hampshire which appears twice
  dplyr::distinct(`Trust ODS Code`, .keep_all = TRUE)


# wrangling ---------------------------------------------------------------
# load the functions which are defined for the app developed by Data Science team
# https://github.com/The-Strategy-Unit/nhp_inputs_report_app/blob/main/R/fct_tabulate.R

source("R/fct tabulate.R")

# derive the cleaned data frame (same as that being used for Shiny app)
extracted_params <- extract_params(nhp_tagged_runs_params, nhp_tagged_runs_meta)
skeleton_table <- prepare_skeleton_table(extracted_params)



# Rename the day procedure columns so as to avoid the use of BADS
nee_results <- nee_results |> 
  dplyr::mutate(
    param_name = dplyr::case_match(
      param_name,
      "bads_daycase" ~ "day_procedures_usually_dc",
      "bads_daycase_occasional" ~ "day_procedures_occasionally_dc",
      "bads_outpatients" ~ "day_procedures_usually_op",
      "bads_outpatients_or_daycase" ~ "day_procedures_occasionally_op",
      .default = param_name
    )
  )

dat <- populate_table(
  skeleton_table,
  extracted_params,
  trust_code_lookup,
  mitigator_lookup,
  nee_results
) |> 
  # divide nee results by 100 so as to standardise
  mutate(across(nee_p10:nee_mean, ~ .x / 100)) 


#get the baseline data
baseline <- historical_mitigators_data |> 
  filter(fyear == 201920,
         procode %in% nhp_tagged_runs_meta$dataset) |> 
  select(!fyear,
         baseline_rate = rate,
         baseline_n = n)


# cross-reference the baseline value and scheme inputs
baseline_inputs_data <- left_join(dat, baseline,
                                  by = c("scheme_code" = "procode",
                                         "mitigator_variable" = "strategy"))

# wrangling baseline metadata ---------------------------------------------

# load in the yaml
yaml <- yaml::read_yaml(
  "data/golem-config.yml"
)

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

# join the yaml data onto the baseline / inputs data
baseline_inputs_data <- baseline_inputs_data |> 
  left_join(
    select(mitigator_yaml_df, 
           strategy_variable, 
           category, 
           y_axis_title), 
    by = c("mitigator_variable" = "strategy_variable"))


# Function for scatter plot -----------------------------------------------

# this function will create a scatter plot comparing the schemes' baseline values
# on the x-axis to their D&C model inputs on the y-axis

# it adds vertical and horizontal lines at the average baseline value and average
# scheme input, respectively

# furthermore, it labels the four quadrants resulting from these two divider
# lines in a way that explains the data, e.g. "high baseline value, large reduction"
baseline_comparison_plot <- function(strategy, scheme) {
  # create the plot data, i.e. filtering to specific strategy and removing the NAs
  plot_data = baseline_inputs_data |> 
    filter(mitigator_variable == strategy,
           !is.na(value_mid)) |> 
    # highlight the specific scheme
    mutate(colour_fill = if_else(scheme_code == scheme, 
                                "blue",
                                "grey"))
  
  # Add vertical and horizontal lines to divide the plot into quadrants based on means
  average_input = mean(plot_data$value_mid)
  average_baseline_rate = mean(plot_data$baseline_rate) 
  
  # Get the x-axis title
  x_label = plot_data$y_axis_title[[1]]
  
  # create the plot
  plot = ggplot(plot_data, aes(x = baseline_rate, y = value_mid)) +
    # use the colour_fill field to highlight the specific scheme
    geom_point(aes(colour = colour_fill)) +
    geom_smooth(method = "lm", se=FALSE) +
    ylab("scheme inputs midpoint (% of baseline)") +
    xlab(paste0("Baseline: ", x_label)) +
    labs(title = "scatter plot comparison of scheme baseline values and inputs",
         subtitle = paste(strategy)) +
    geom_text_repel(aes(label = scheme_code, colour = colour_fill), size = 4) +
    # Add vertical and horizontal lines to divide the plot into quadrants based on means
    geom_hline(yintercept = average_input, linetype = "dashed") +
    geom_vline(xintercept = average_baseline_rate, linetype = "dashed") +
    # get the colours from the value of the colour_fill column
    scale_colour_identity()
    
  
  # Get the limits of the plot
  plot_limits <- ggplot_build(plot)$layout$panel_params[[1]]
  
  # Define the corner coordinates
  xmin <- plot_limits$x.range[1]
  xmax <- plot_limits$x.range[2]
  ymin <- plot_limits$y.range[1]
  ymax <- plot_limits$y.range[2]
  
  # annote the quadrants 
  plot +
    annotate("text", x = xmin, y = ymax, label = "Low baseline\nLow reduction", hjust = 0, vjust = 1) +
    annotate("text", x = xmax, y = ymax, label = "High baseline\nLow reduction", hjust = 1, vjust = 1) +
    annotate("text", x = xmin, y = ymin, label = "Low baseline\nHigh reduction", hjust = 0, vjust = 0) +
    annotate("text", x = xmax, y = ymin, label = "High baseline\nHigh reduction", hjust = 1, vjust = 0)
  
}

# producing a plot

baseline_vs_input_scatter_plot <- baseline_comparison_plot("falls_related_admissions", "RBT")


# historical trends - line chart ------------------------------------------

# we will look a falls for Leighton as an example
falls_time_series <- historical_mitigators_data |> 
  filter(strategy == "falls_related_admissions",
         fyear <= 201920,
         procode == "RBT",
         procode %in% nhp_tagged_runs_meta$dataset) |> 
  mutate(year = as.numeric(str_sub(fyear, 1,4)),
         input = dat$value_mid[dat$scheme_code=="RBT" & dat$mitigator_variable=="falls_related_admissions"][[1]],
         horizon = input * last(rate, order_by = fyear))

# a plot showing the time series with the the horizon value overlaid
historic_trend_vs_input_line_chart_1 <- ggplot(falls_time_series, aes(x = year, y = rate)) +
  geom_line(color = "blue") + # Solid line for historical falls_time_series
  geom_hline(yintercept = mean(falls_time_series$horizon), linetype = "dashed", color = "red") + # Horizontal line for horizon
  annotate("text", x = max(falls_time_series$year), y = mean(falls_time_series$horizon), 
           label = "horizon value", vjust = -1, color = "red") + # Label for horizon line
  labs(title = "Historical Rates and Horizon",
       subtitle = "Leighton Falls related admissions",
       x = "Financial Year",
       y = "Rate") +
  theme_minimal()

# a plot showing the time series with the horizon value added on at the end,
# alongside a linear line from the baseline value to horizon

historic_trend_vs_input_line_chart_2 <- ggplot(falls_time_series, aes(x = year, y = rate)) +
  geom_line(color = "blue") + # Solid line for historical falls_time_series
  geom_vline(xintercept = 2019, linetype = "dotted", color = "black") + # Vertical dotted line
  annotate("segment", 
           x = 2019, 
           y = falls_time_series$rate[falls_time_series$year == 2019], 
           xend = 2040, 
           yend = mean(falls_time_series$horizon), 
           linetype = "dashed", color = "red") + # Dashed line for projection
  annotate("text", 
           x = 2040, 
           y = mean(falls_time_series$horizon),
           label = "Horizon value") + 
  labs(title = "Historical Rates and Horizon",
       subtitle = "Leighton Falls related admissions",
       x = "Financial Year",
       y = "Rate") +
  theme_minimal()
  


# historical trends - scatter plot ----------------------------------------

# calculate the 5-yr trends in data for each scheme and mitigator as well as the  
# year-on-year growth (assuming compound growth)
baseline_5yr_trends <- historical_mitigators_data |>
  select(!n) |> 
  filter(procode %in% nhp_tagged_runs_meta$dataset,
         fyear %in% c(201516,201920)) |> 
  tidyr::pivot_wider(names_from = fyear, values_from = rate) |> 
  mutate(total_relative_change = `201920` / `201516`,
         annual_relative_change = total_relative_change^(1/5))

# calculate the implied yearly growth rates from the model inputs
input_yearly_trends <- dat |> 
  select(scheme_name, scheme_code, mitigator_variable, value_mid, year_range) |> 
  mutate(reduction_pa = (value_mid - 1) / year_range)

# combine them together
baseline_and_input_trends <- input_yearly_trends |> 
  left_join(baseline_5yr_trends, 
            by = c("scheme_code"="procode", "mitigator_variable"="strategy"))

# create chart
historic_trend_vs_input_scatter_plot <- baseline_and_input_trends |> 
  filter(
    mitigator_variable == "falls_related_admissions") |> 
  ggplot(aes(y = reduction_pa, x = annual_relative_change)) +
  geom_point() +
  ylab("scheme's projected annual change") +
  xlab("scheme's annual change in 5 years to 2019-20") +
  labs(title = "scatter plot showing comparison between historical change and projected change",
       subtitle = "for falls_related_admissions cases") +
  geom_text_repel(aes(label = scheme_code), size = 4)



