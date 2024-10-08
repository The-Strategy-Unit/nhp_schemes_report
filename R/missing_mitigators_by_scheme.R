# Step 1: Create a data frame with all combinations of scheme codes and mitigator
all_scheme_mitigator_combinations <- expand_grid(
  scheme_code = nhp_tagged_runs_meta$dataset, 
  mitigator_variable = mitigator_lookup$`Strategy variable`)

# Step 2: Join with the original data to identify missing combinations
missing_scheme_mitigator_combinations <- all_scheme_mitigator_combinations |>
  left_join(dat, by = c("scheme_code", "mitigator_variable")) |>
  filter(is.na(value_mid)) |> 
  select(scheme_code, mitigator_variable)

# Step 3: Count the total occurrences of each `mitigator_variable`
mitigator_counts <- dat |>
  filter(!is.na(value_mid)) |> 
  count(mitigator_variable)

# Step 4: Merge the missing combinations with the counts
missing_mitigators_by_scheme <- missing_scheme_mitigator_combinations |>
  left_join(mitigator_counts, by = "mitigator_variable") |>
  arrange(scheme_code, mitigator_variable) |> 
  left_join(trust_code_lookup, by = c("scheme_code"="Trust ODS Code")) |> 
  left_join(mitigator_lookup, by = c("mitigator_variable"="Strategy variable")) |> 
  select(scheme_code,
         scheme = `Name of Hospital site`,
         mitigator_group = Grouping,
         mitigator = `Strategy name`,
         total_incidence = n)
