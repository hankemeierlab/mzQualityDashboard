## Selecting Compounds

This table shows the compounds that have been entered in the `compound` column. If there were any compounds containing *ISTD*, they will not be shown by default. This functionality can be disabled in the `Advanced Settings` at the start up. 

For each compound, several metrics are shown:

- `RSDQC`: Variation between selected QC samples in percentages. This is before batch correction
- `RSDQC Corrected`: Variation between selected QC samples after batch correction. This number should be lower than before correction. If this number exceeds the threshold in the `Advanced Settings`, it will be flagged red and is excluded from further analysis. 
- `Background Signal`: For this compound, the background signal is calculated as the mean area of study samples divided by the median area of blank samples. By default, the threshold is set to 40%, meaning that any compound above this level will be disabled from analysis unless manually changed. 
- `Use`: This column indicates if mzQuality recommends to use this compound for downstream analysis. This column remains unchanged, even if compounds are manually enabled or disabled.

Similar to the aliquot table, compounds can be manually enabled or disabled by clicking the corresponding row. A red colored row indicates the compound is disabled from further analysis. 
