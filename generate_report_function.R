generate_report_function <- function(selected_scheme_code) {
  
  datetime <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  file_name <- paste0(selected_scheme_code, "_", datetime, ".docx")
  
  quarto::quarto_render(
    input = "scheme_roundtable_comparative_report.qmd", 
    output_file = file_name,
    execute_params = list(
      scheme_code= selected_scheme_code)
  )
}
