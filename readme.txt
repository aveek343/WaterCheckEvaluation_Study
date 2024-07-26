****Read this document before running the scripts****
Author: Mahmud Aveek
Date: 07-25-2024


The scripts will reproduce the figures and results presented in the manuscript named “Evaluating the Impact of Residential Landscape Audits Using 5-Second Water Use Data.” Download following scripts and files from the Github:  
•	1_DataHandler.R
•	1_2_Second_and_Later_run_DataHandler.R
•	2_DataProcessing.R
•	3_KSTestFunctions.R
•	4_MaintText_Plots.R
•	5_Appendix_Plots.R
•	6_Appendix_Figure_PopulationVsParticipant_HP.R
•	“WaterCheckEvaluation_Study.Rproj”,
•	“urls_USU_WaterConservation.csv”,
•	md_withSiteID_anonymous_summer_HP.csv.

Follow the instructions listed below to run the scripts.


###################################################################################################### 
Running the scripts for the first time: 
Follow these steps in chronological order to set up the necessary database and reproduce the plots for the first time. 
1.	1_DataHandler.R—will create a database which is almost 8 Gigabites 
2.	2_DataProcessing.R—will  create all the datasets and dataframes required for analysis
3.	3_KSTestFunctions.R—will create the functions required to perform Kolmogorov-Smirnov Tests (KS-tests)
4.	4_MaintText_Plots.R—will produce all the figures presented in the main text
5.	5_Appendix_Plots.R—will produce first 6 plots presented in appendix 1 (Figures)
6.	6_Appendix_Figure_PopulationVsParticipant_HP.R—will  produce last 2 plots presented in  appendix 1 (Figures)



###################################################################################################### 
Any future runs: 
These instructions assume that the necessary database has already been created during the initial run. Follow these steps in chronological order if this is your second time (or later) reproducing the plots.
1.	1_2_Second_and_Later_run_DataHandler.R—connects with the database
2.	2_DataProcessing.R—processes the data for analysis
3.	3_KSTestFunctions.R—creates the functions required for KS-test
4.	4_MaintText_Plots.R—creates the plots shown in the main text
5.	5_Appendix_Plots.R—creates the first 6 plots shown in the appendix 1
6.	6_Appendix_Figure_PopulationVsParticipant_HP.R—creates the last 2 plots shown in the appendix 1

###################################################################################################### 
# versions
Here we list the versions of r, rStudio, and packages used during the creation of the plots and results.


r version	R 4.4.0
r-studio version	2023.06.0.421
	
	
Package	Version

broom	1.0.6
DBI	1.2.3
dplyr	1.1.4
dygraphs	1.1.1.6
forcats	1.0.0
ggplot2	3.5.1
ggpubr	0.6.0
ggpubr	0.6.0
HSClientR	0.3.1.9000
infer	1.0.7
lubridate	1.9.3
modeldata	1.3.0
parsnip	1.2.1
PerformanceAnalytics	2.0.4
purrr	1.0.2
quantmod	0.4.26
RColorBrewer	1.1-3
readr	2.1.5
recipes	1.0.10
remotes	2.5.0
rsample	1.2.1
RSQLite	2.3.7
stringr	1.5.1
tibble	3.2.1
tidymodels	1.2.0
tidyquant	1.0.7
tidyr	1.3.1
tidyverse	2.0.0
TTR	0.24.4
tune	1.2.1
versions	0.3
workflows	1.1.4
workflowsets	1.1.0
xts	0.14.0
yardstick	1.3.1
zoo	1.8-12