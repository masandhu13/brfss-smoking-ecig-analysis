# BRFSS Data Analysis
### Author: Mannat Sandhu
### July 14th, 2019

The project uses 2017 edition of the CDC's annual behavioral survey on risk factors to
understand smoking and e-cigarette usage in the US population. 

The final report is present as "Prevalence and Demographics of Smoking and E-cigarettes Use (2017).pdf" in the repository.

We analyze the data to find the prevalence and demographics of smokers and e-cigarette users, and compare and contrast findings between these groups to identify meaningful differences.

The code contains 3 files - 
* data-visualizations.R
* logistic-regressions-analyses.R
* utils.R

The code for the visualizations is in data-visualizations.R and the code for the logistic regressions is in logistic-regressions-analyses.R. These two files are independent of each other.

The file utils.R contains utility functions which are used by both the above files. 

To run the code, you will need to change the data in utils.R to point to unzipped SAS file LLCP2017.XPT (downloaded from https://www.cdc.gov/brfss/annual_data/2017/files/LLCP2017XPT.zip).
