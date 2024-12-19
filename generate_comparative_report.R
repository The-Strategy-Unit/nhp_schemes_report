quarto::quarto_render(
  "scheme_roundtable_comparative_report.qmd", 
  execute_params = list(
    scheme_code= "insert_scheme_code", 
    scheme_name = "insert_scheme_name")
  )
