# mitigator coverage section ----------------------------------------------

# ```{r mitigator_coverage} 

# dat |>  
#   group_by(mitigator_type, mitigator_activity_type, mitigator_variable) |>  
#   summarise(n = sum(!is.na(value_mid)), 
#             coverage = n / nrow(nhp_tagged_runs_meta)) |>  
#   select(!n) |>  
#   arrange(mitigator_type, mitigator_activity_type,coverage) |>  
#   mutate(coverage = scales::percent(coverage)) |>  
#   DT::datatable(filter = "top", 
#                 colnames = c("Mitigator type", 
#                              "Activity type", 
#                              "Mitigator", 
#                              "Coverage")) 
# ``` 


# Mitigator ambition section ----------------------------------------------

# ```{r mitigator_ambition} 
  
# dat |>  

#   group_by(mitigator_type, mitigator_activity_type, mitigator_variable) |>  

#   summarise(mean_value = mean(value_mid, na.rm = TRUE)) |>  

#   arrange(mitigator_type, mitigator_activity_type,mean_value) |>  

#   DT::datatable(filter = "top", 

#                 colnames = c("Mitigator type", 

#                              "Activity type", 

#                              "Mitigator", 

#                              "Average central value")) 

# ``` 

# The table shows the distribution of the average scheme mid-points. For example, we can see that the least ambitious third of average mid-points fell between 0.87 and 0.95. 

# ```{r ambition_terciles} 

# terciles_ambition <- mitigator_agg_data |>  

#   mutate( 

#     mitigator_ambition = case_when( 

#       ntile(avg_mid, 3) == 1 ~ "Most ambition", 

#       ntile(avg_mid, 3) == 2 ~ "Median ambition", 

#       ntile(avg_mid, 3) == 3 ~ "Least ambition", 

#       TRUE ~ NA_character_)) |>  

#   group_by(mitigator_ambition) |>  

#   summarise(n=n(), 

#             min = min(avg_mid), 

#             max = max(avg_mid)) 

#  

# terciles_ambition |>  

#   select(!n) |>  

#   knitr::kable(col.names = c("Mitigator ambition", 

#                              "Range min", 

#                              "Range max"), 

#                caption = "Distribution of aggregate mitigator midpoints") 

# ``` 

# ```{r ambition_by_group} 

# 

#  mitigator_group_ambition <- mitigator_agg_data |> 

#    mutate( 

#      mitigator_ambition = case_when( 

#        ntile(avg_mid, 3) == 1 ~ "Most ambition", 

#        ntile(avg_mid, 3) == 2 ~ "Median ambition", 

#        ntile(avg_mid, 3) == 3 ~ "Least ambition", 

#        TRUE ~ NA_character_)) |> 

#    count(mitigator_group, mitigator_ambition) |> 

#    tidyr::pivot_wider(names_from = mitigator_ambition, values_from = n, values_fill = 0) 

# 

#  mitigator_group_ambition |> 

#    rename("Mitigator group" = mitigator_group) |> 

#    knitr::kable(caption = "Mitigator groups by ambition level") 

# 

# ``` 

# ```{r mitigator_ambition} 

# mitigator_group_ambition <- mitigator_agg_data |>  

#   mutate(  

#     mitigator_ambition = case_when(  

#       avg_mid > 0.9 ~ "Low ambition",  

#       avg_mid > 0.5 ~ "Medium ambition",  

#       avg_mid <= 0.5 ~ "High ambition",  

#       TRUE ~ NA_character_)) |>  

#   count(mitigator_group, mitigator_ambition) |>  

#   tidyr::pivot_wider(names_from = mitigator_ambition, values_from = n, values_fill = 0) 

#  

#  

# mitigator_ambition_by_group |>  

#   select(!n) |>  

#   mutate(p = scales::percent(p)) |>  

#   pivot_wider(names_from = mitigator_ambition, values_from = p) |>  

#   select(mitigator_group, high, med, low) 

#  

# ``` 
  

# Relative certainty section ----------------------------------------------

# ```{r mitigator_certainty} 
  
# dat |>  

#   group_by(mitigator_type, mitigator_activity_type, mitigator_variable) |>  

#   summarise(mean_range = mean(value_range, na.rm = TRUE)) |>  

#   arrange(mitigator_type, mitigator_activity_type,mean_range) |>  

#   DT::datatable(filter = "top", 

#                 colnames = c("Mitigator type", 

#                              "Activity type", 

#                              "Mitigator", 

#                              "Average CI width")) 

# ``` 


# nee section -------------------------------------------------------------

# -   Grey horizontal bar represents the total range of values from NEE, with vertical line being the p50 
#   
# -   black horizontal line represents the range between the means of the low values and high values for trusts selecting that mitigator, with dot being the mean of the central values 
# 
# -   top 6 in terms of absolute difference between aggregate central value and NEE central value shown 
# 
# ```{r comparison_to_NEE} 
# 
# Need to check the difference between nee_p50 and nee_mean (the first is  
# 
# mid_point of p10 and p90) 
# 
# nee_comparison <- dat |> 
#   group_by(mitigator_type, mitigator_activity_type, mitigator_variable) |>  
#   summarise(mean_lo = mean(value_lo, na.rm = TRUE), 
#                                  mean_hi = mean(value_hi, na.rm = TRUE), 
#                                  mean_value = mean(value_mid, na.rm = TRUE), 
#                                  nee_p10 = mean(nee_p10, na.rm = TRUE), 
#                                  nee_p90 = mean(nee_p90, na.rm = TRUE), 
#                                  nee_p50 = mean(nee_mean, na.rm = TRUE), 
#                                  mean_abs_diff = abs(nee_p50 - mean_value)) |>  
#   # note that we select those with the largest difference between the central  
#   # value of the NEE range and the mean of the central values from trusts 
#   slice_max(order_by = mean_abs_diff)  
#   nee_comparison |>  
#   # creating plot from here   
#   ggplot2::ggplot( 
#     ggplot2::aes( 
#     x = mean_value,  
#     y = mitigator_variable)) + 
#     ggplot2::geom_crossbar( 
#       ggplot2::aes(
#         x = nee_p50,  
#         xmin = nee_p90, 
#         xmax = nee_p10
#         ), 
#       fill = "lightgrey", 
#       colour = "grey85", 
#       alpha = 0.2, 
#       width = 0.4 
#     ) + 
#     ggplot2::geom_pointrange(
#       ggplot2::aes(x = mean_value,
#                    xmin = mean_lo, 
#                    xmax = mean_hi), 
#                    size = 0.3, 
#                    linewidth = 1.2 
#       ) + 
#     labs(title = "NEE results vs scheme aggregate",  
#          subtitle = "6 biggest differences", 
#          caption = "Grey bar represents NEE; black line represents scheme average") 
#   
# ``` 
    

# baseline section --------------------------------------------------------

# ```{r baseline_credibility}
# source("baseline_comparison_plot.R")
# 
# baseline_comparison_plot("falls_related_admissions")
# ```

# trend section -----------------------------------------------------------
# ```{r time_series_credibility}
# # calculate the 5-year trends in the historical data leading up to 2019-20
# # and annualise (assuming compound growth)
# baseline_5yr_trends <- historical_mitigators_data |>
#   select(!n) |> 
#   filter(procode %in% nhp_tagged_runs_meta$dataset,
#          fyear %in% c(201516,201920)) |> 
#   tidyr::pivot_wider(names_from = fyear, values_from = rate) |> 
#   mutate(total_relative_change = `201920` / `201516`,
#          annual_relative_change = total_relative_change^(1/5))
# 
# # calculate the schemes' implied yearly trends for mitigators 
# # (assuming compound growth)
# input_yearly_trends <- dat |> 
#   select(scheme_name, scheme_code, mitigator_variable, value_mid, year_range) |> 
#   mutate(reduction_pa = value_mid ^ (1 / year_range))
# 
# # join together 
# baseline_and_input_trends <- input_yearly_trends |> 
#   left_join(baseline_5yr_trends, 
#             by = c("scheme_code"="procode", "mitigator_variable"="strategy"))
# 
# baseline_and_input_trends |> 
#   filter(
#     mitigator_variable == "alcohol_partially_attributable_acute") |> 
#   ggplot(aes(x =annual_relative_change, y = reduction_pa)) +
#   geom_point() +
#   xlab("scheme's annual change in 5 years to 2019-20") +
#   ylab("scheme's projected annual change") +
#   labs(title = "scatter plot showing comparison between historical change and projected change",
#        subtitle = "for alcohol_partially_attributable_acute cases") +
#   geom_text_repel(aes(label = scheme_code), size = 4)
# ```


# baseline - for scheme-level ---------------------------------------------

# ```{r baseline_cross_reference_by_scheme}
# baseline_cross_reference_by_scheme <- baseline_inputs_data |> 
#   filter(!is.na(value_mid), !is.na(baseline_rate)) |> 
#   summarise(average_value = mean(value_mid),
#             baseline_rate = mean(baseline_rate),
#             .by = scheme_name)
# 
# baseline_cross_reference_by_scheme |> 
#   mutate(value_z = (average_value - mean(average_value))/sd(average_value),
#          baseline_z = (baseline_rate - mean(baseline_rate))/sd(baseline_rate))


# ```


# trend - scheme-level ----------------------------------------------------

# Below we can see the average difference between the schemes' mitigator mid-points and the NEE midpoints, for the `r nrow(nee_results)` mitigators which were included in the NEE exercise. A negative number means that the scheme's average mitigator input was lower than the average NEE mid-point, i.e. on average, the scheme was more ambitious in its selections than the NEE exercise.
# 
# ```{r nee_comparison_by_scheme}
# mitigator_nee_credibility <- dat |> 
#   filter(!is.na(value_mid) & !is.na(nee_p50)) |> 
#   summarise(comparison_to_nee = mean(value_mid - nee_p50),
#             .by = scheme_name)
# 
# mitigator_nee_credibility |> 
#   arrange(comparison_to_nee) |> 
#   knitr::kable(caption = "Difference to NEE by scheme",
#                col.names = c("Scheme",
#                              "Average difference between scheme midpoint and NEE midpoint"))
# ```

### More or less ambitious given baseline (average over all mitigators)


### More or less ambitious given historical trend (average over all mitigators)

# NB Kettering (RNQ) shows infinite historical change due to a rate of 0 for discharged_no_treatment_child_ambulance in 2015-16 (i.e. division by zero error when dividing the 2019-20 rate).
# 
# ```{r time_series_by_scheme}
# time_series_by_scheme <- baseline_and_input_trends |> 
#   filter(!is.na(value_mid), !is.na(annual_relative_change)) |> 
#   summarise(avg_historical_change_pa = mean(annual_relative_change),
#             avg_reduction_pa = mean(reduction_pa),
#             .by = scheme_name)
# 
# time_series_by_scheme |> 
#   knitr::kable(caption = "Comparison of sheme historical trends and mitigator inputs",
#                col.names = c("Scheme",
#                              "Average historical trend",
#                              "Average mitigator yearly input"))
# 
# ```

