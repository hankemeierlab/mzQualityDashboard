## Start an mzQuality project
This box shows the input controls for creating an mzQuality project. First, a project name can be entered and will create a folder in the current working directory. It also ensures that the export will have the same name. If left empty, a folder created in the format __mzQuality_<date>_<time>__ to ensure it is a unique folder.

## Data
mzQuality works by importing tab-delimited peak-picked data. This data must contain the following columns: 


mzQuality also provides example data with anonimized samples and compounds. To use example data, enable the checkbox `Use Example Data`. 
To start the analysis, press the `Submit` button.

## (Optional) Concentrations
To calculate absolute concentrations, calibration lines with spiked compounds are necessary. The known concentrations field allows you to add these concentrations in a matrix format. Each column should contain the sample name of the calibration line and the rows should be equal to the compound names in __data__. The numbers in the matrix can be of any unit and calculated concentrations will be reported in the same unit. See the calibration plot for further explanation on calibration lines and calculating concentrations.

## Advanced options
mzQuality runs with default settings which are recommended for most studies. However, in case when specific settings are needed, these can be adjusted here. The settings changed here are applied throughout mzQuality.
