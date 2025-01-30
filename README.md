# nhp_schemes_report

A report for the NHP into the general state of play regarding NHP schemes' inputs to the D&C model

## Prerequisites

The following files are required are required to be stored locally for this report to render:

-   data/golem-config.yml
-   data/mitigator-lookup.csv
-   data/mitigators.json
-   data/nee_table.Rds
-   data/nhp-scheme-lookup.csv
-   nhp_template_empty.docx

Please request these from the data science team if you do not have them.

To access the Azure containers, create a `.Renviron` file in the project root using `.Renviron.sample` as a template.

Ask a member of the Data Science team for the values required by each variable.

During this process, you'll be prompted to authorise with Azure through the browser. See [the Data Science website](https://the-strategy-unit.github.io/data_science/presentations/2024-05-16_store-data-safely/#/authenticating-to-azure-data-storage) for detail on authorisation.

## Instructions

In order to render the report, open "generate_comparative_report.R" and insert the trust code for the specific scheme you are interested in at line 4 then run the command.

For example, if we wanted to generate the report for Frimley (RDU), we would run the following in the console:

    generate_report_function(selected_scheme_code = "RDU")

This will then produce a Word output using the file "nhp_template_empty.docx" as a template.

Subsequently, you can move the Word output to the subfolder "outputs/" after proof reading.
