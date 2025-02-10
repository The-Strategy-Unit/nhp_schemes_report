generate_report_function <- function(selected_scheme_code) {
  
  # check for the template file 
  # if missing, download from Azure
  
  
  if(!file.exists("nhp_template_empty.docx")) {
    source("R/azure.R")
    
    AzureStor::storage_download(
      container = get_container(container_name = Sys.getenv("AZ_STORAGE_CONTAINER_SUPPORT")),
      src = "nhp_template_empty.docx", 
      dest = "nhp_template_empty.docx"
    )
  }

  datetime <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  file_name <- paste0(selected_scheme_code, "_", datetime, ".docx")
  
  quarto::quarto_render(
    input = "scheme_roundtable_comparative_report.qmd", 
    output_file = file_name,
    execute_params = list(
      scheme_code= selected_scheme_code)
  )
}
