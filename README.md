# nhp_schemes_report

A report for the NHP into the general state of play regarding NHP schemes' inputs to the D&C model

## Prerequisites

The following files are required to be stored locally for this report to render:

-   data/golem-config.yml 
-   data/mitigators.json

Please request these from the data science team if you do not have them.

We access the following files from Azure:

-   the mitigators look-up
-   the results of the NEE exercise
-   the look-up of NHP schemes
-   the schemes' outputs data
-   the blank NHP template (this will only be downloaded if not already present)

To access the Azure containers, create a `.Renviron` file in the project root using `.Renviron.sample` as a template.

[Ask a member of the Data Science team](mailto:mlcsu.su.datascience@nhs.net) for the values required by each variable.

During this process, you'll be prompted to authorise with Azure through the browser. See [the Data Science website](https://the-strategy-unit.github.io/data_science/presentations/2024-05-16_store-data-safely/#/authenticating-to-azure-data-storage) for detail on authorisation.

## Instructions

In order to render the report, open "generate_comparative_report.R" and insert the trust code for the specific scheme you are interested in at line 4 then run the command.

For example, if we wanted to generate the report for Frimley (RDU), we would run the following in the console:

    generate_report_function(selected_scheme_code = "RDU")

This will then produce a Word output using the file "nhp_template_empty.docx" as a template.

To write an executive summary of the report, create a .md file with the scheme code as its name in the folder "scheme_text_markdown/" and write a set of bullets summarising the contents of the report - for example, in the case of Frimley we would create the following file "scheme_text_markdown/RDU.md". You may find it easier to initially run the Quarto file chunk by chunk and write the commentary before saving and then running `generate_report_function()` to generate the final report.

Subsequently, you can move the Word output to the subfolder "outputs/" after proof reading.
