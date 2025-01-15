# nhp_schemes_report
A report for the NHP into the general state of play regarding NHP schemes' inputs to the D&amp;C model

## Prerequisites

The following files are required are required to be stored locally for this report to render:

- data/golem-config.yml 
- data/mitigator-lookup.csv
- data/mitigators.json
- data/nee_table.Rds
- data/nhp-scheme-lookup.csv
- nhp_template_empty.docx

Please request these from the data science team if you do not have them. 

## Instructions

In order to render the report, open "generate_comparative_report.R" and insert the trust code for the specific scheme you are interested in at line 4 then run the command. 

For example, if we wanted to generate the report for Frimley (RDU), we would run the following in the console:

```
quarto::quarto_render(
  "scheme_roundtable_comparative_report.qmd", 
  execute_params = list(
    scheme_code= "RDU")
  )
```

This will then produce a html output as well as a Word output using the file "nhp_template_empty.docx" as a template. 

Then we amend the name of the Word output to something including the scheme's name prior to sharing.

