****Read the entire readme document before running the scripts****
****After the first run, all future runs must follow the steps under "Any future runs"****
 
Author: Mahmud Aveek
Date: 07-25-2024
Updated: 10-02-2025

Version details of R, Rstudio, and the libraries are provided at the end of this note.

Rstudio automatically instructs which libraries to download when you load the scripts. 
Check and download all required libraries.

The scripts will reproduce the figures and results presented in the manuscript named “Evaluating the Impact of Residential Landscape Audits Using 5-Second Water Use Data.” 
Download following scripts and files from the Github:  
•	1_DataHandler.R
•	1_2_Second_and_Later_run_DataHandler.R
•	2_DataProcessing.R
•	3_KSTestFunctions.R
•	4_MaintText_Plots.R
•	5_Appendix_Plots.R
•	6_Appendix_Figure_PopulationVsParticipant_HP_liter.R
•	7_Additional_Calculations.R
•	“WaterCheckEvaluation_Study.Rproj”,
•	“urls_USU_WaterConservation.csv”,
•	md_withSiteID_anonymous_summer_HP.csv
•	WaterCheckEvaluation_Study.Rproj.


Follow the instructions listed below to run the scripts.


###################################################################################################### 
Running the scripts for the first time: 

First, run the "WaterCheckEvaluation_Study.Rproj". It will set your directory current.
Follow these steps in chronological order to set up the necessary database and reproduce the plots for the first time. Note the initial run may take between 20-40 minutes to run as the database is being created.

1.	1_DataHandler.R—will create a database which is almost 8 Gigabites 
2.	2_DataProcessing.R—will  create all the datasets and dataframes required for analysis
3.	3_KSTestFunctions.R—will create the functions required to perform Kolmogorov-Smirnov Tests (KS-tests)
4.	4_MaintText_Plots.R—will produce all the figures presented in the main text
5.	5_Appendix_Plots.R—will produce first 6 plots presented in appendix 1 (Figures)
6.	6_Appendix_Figure_PopulationVsParticipant_HP.R—will  produce last 2 plots presented in  appendix 1 (Figures)



###################################################################################################### 
Any future runs: 

Run the "WaterCheckEvaluation_Study.Rproj".
Next, run the scripts from R-studio.

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

r-studio version	2025.9.1.401
	
Package	Version

R version 4.5.1 (2025-06-13 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/Denver
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] RColorBrewer_1.1-3         PerformanceAnalytics_2.0.8 quantmod_0.4.28           
 [4] TTR_0.24.4                 xts_0.14.1                 zoo_1.8-14                
 [7] tidyquant_1.0.11           dygraphs_1.1.1.6           versions_0.3              
[10] randomForest_4.7-1.2       ggpubr_0.6.1               yardstick_1.3.2           
[13] workflowsets_1.1.1         workflows_1.3.0            tune_2.0.0                
[16] tailor_0.1.0               rsample_1.3.1              recipes_1.3.1             
[19] parsnip_1.3.3              modeldata_1.5.1            infer_1.0.9               
[22] dials_1.4.2                scales_1.4.0               broom_1.0.10              
[25] tidymodels_1.4.1           DBI_1.2.3                  lubridate_1.9.4           
[28] forcats_1.0.1              stringr_1.5.2              dplyr_1.1.4               
[31] purrr_1.1.0                readr_2.1.5                tidyr_1.3.1               
[34] tibble_3.3.0               ggplot2_4.0.0              tidyverse_2.0.0           
[37] HSClientR_0.3.1.9000       remotes_2.5.0             

loaded via a namespace (and not attached):
 [1] rlang_1.1.6         magrittr_2.0.4      furrr_0.3.1         compiler_4.5.1     
 [5] RSQLite_2.4.3       vctrs_0.6.5         quadprog_1.5-8      lhs_1.2.0          
 [9] pkgconfig_2.0.3     crayon_1.5.3        fastmap_1.2.0       backports_1.5.0    
[13] dbplyr_2.5.1        labeling_0.4.3      utf8_1.2.6          prodlim_2025.04.28 
[17] tzdb_0.5.0          bit_4.6.0           cachem_1.1.0        jsonlite_2.0.0     
[21] blob_1.2.4          parallel_4.5.1      R6_2.6.1            stringi_1.8.7      
[25] parallelly_1.45.1   car_3.1-3           rpart_4.1.24        Rcpp_1.1.0         
[29] future.apply_1.20.0 triebeard_0.4.1     Matrix_1.7-3        splines_4.5.1      
[33] nnet_7.3-20         timechange_0.3.0    tidyselect_1.2.1    rstudioapi_0.17.1  
[37] abind_1.4-8         timeDate_4041.110   codetools_0.2-20    curl_7.0.0         
[41] listenv_0.9.1       lattice_0.22-7      withr_3.0.2         S7_0.2.0           
[45] future_1.67.0       survival_3.8-3      xml2_1.4.0          pillar_1.11.1      
[49] carData_3.0-5       generics_0.1.4      vroom_1.6.6         hms_1.1.3          
[53] globals_0.18.0      class_7.3-23        glue_1.8.0          tools_4.5.1        
[57] data.table_1.17.8   gower_1.0.2         ggsignif_0.6.4      grid_4.5.1         
[61] urltools_1.7.3.1    RobStatTM_1.0.11    ipred_0.9-15        Formula_1.2-5      
[65] cli_3.6.5           DiceDesign_1.10     lava_1.8.1          gtable_0.3.6       
[69] GPfit_1.0-9         rstatix_0.7.2       digest_0.6.37       htmlwidgets_1.6.4  
[73] farver_2.1.2        htmltools_0.5.8.1   memoise_2.0.1       lifecycle_1.0.4    
[77] hardhat_1.4.2       httr_1.4.7          bit64_4.6.0-1       MASS_7.3-65        
[81] sparsevctrs_0.3.4  