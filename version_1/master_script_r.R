
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script master_script_r.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Implement the R-scripts required to replicate the analysis.

# R version 4.0.5, RStudio version 1.2.5033.
rm(list=ls())

# First, make sure that the SAS scripts (listed in master_script_sas.sas) 
# have run successfully.


# To install packages from a CRAN mirror repository in FIONA:
# 1) Create .Rprofile file to the root where the project is:
#     local({
#       r <- getOption("repos")
#       r["CRAN"] <- "https://cran.isaacus.local/"
#       options(repos = r)
#     })
# 2) Restart RStudio. Now you can load CRAN packages by install.packages():
# 3) Use the library() function to load packages.

# The packages used are listed below.
library(readxl)           # Reading xlsx files.
library(data.table)       # Mutating data. 
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure. 
library(rdrobust)         # Tools for RD designs.
library(RDHonest)         # Honest inference in RDDs.
library(lfe)              # linear fixed effects estimation. 
library(openxlsx)         # Save as excel file. 
library(stargazer)        # Save as tex file.
library(rdlocrand)        # Local randomization methods.

writeLines(capture.output(sessionInfo()), 'sessionInfo.txt')

###
###

Sys.time()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Assess visit data quality. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Create a dataset with which to analyze data quality:
source(file="W:/ASMA1/data/1_data_for_quality_checks.R")
#     Input:  visits_quality_1x.csv (x in 1:9) +
#             folk_data_201x.csv (x in 1:9) + referrals_201x.csv (x in 1:9) + 
#             specialist_visits_201x.csv (x in 1:8)
#     Outputs: visits_quality.rds
# Running time 7 minutes.


# Analyse data quality and decide which municipality-year observations 
# are dropped for quality reasons:
source(file="W:/ASMA1/data/1_quality_checks.R")
#     Input: visits_quality.rds
#     Outputs: visits_quality_small.pdf + visits_changes.rds + 
#           refs_quality_muni.pdf
# Running time <0.5 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Create analysis datasets. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### The main analysis data allows for unbalanced data at the person level.
# E.g., if the person moves to a sample municipality from abroad, he/she will
# be in the analysis data in those calendar years at the end of which the 
# person lived in a sample municipality. Between-municipality migration thus
# affects the sample sizes over time. ###

# Create ID-date panels:
source(file="W:/ASMA1/data/2_id_date_panels_set1.R")
#     Input: visits_changes.rds + rdd_policies.csv + 
#           visits_quality_1X.csv (X in 1:9) + 
#           visits_diagnoses_1x.csv (X in 1:8) + 
#           visits_main_1x.csv (x in 1:9) + folk_data_201x.csv (x in 1:9) +
#           referrals_201x.csv (x in 1:9)
#     Interim: interim_set1_id_dateAA_genderB.rds (AA in 11:19 and B in 1:2)
#     Outputs: analysis_set1_id_yearAA_genderB.rds (AA in 11:19 and B in 1:2)
# Running time approximately 37 minutes.

# Create datasets for analyses:
source(file="W:/ASMA1/data/2_analysis_data_set1.R")
#     Input: rdd_policies.csv + 
#           analysis_set1_id_yearAA_genderB.rds (AA in 11:18 and B in 1:2) +
#           folk_data_201X.csv (X in 1:9)
#     Outputs: data1_local_pol.rds + data1_parametric.rds + data1_local_rand.rds
# Running time approximately 39 minutes.


### As a supplement, we construct new and smaller analysis datasets. This time 
# we include only those individuals who are observed to live in the same policy 
# area throughout a bandwidth of six months. Thus, between-municipality 
# migration does not affect sample sizes over time. ###

# Create ID-date panels:
source(file="W:/ASMA1/data/2_id_date_panels_set2.R")
#     Input: visits_changes.rds + rdd_policies.csv + 
#           visits_quality_1X.csv (X in 1:9) + 
#           visits_main_1x.csv (x in 1:9) + folk_data_20xx.csv (xx in 11:20)
#     Interim: interim_set2_id_dateAA_genderB.rds (AA in 11:19 and B in 1:2)
#     Outputs: analysis_set2_id_yearAA_genderB.rds (AA in 11:19 and B in 1:2)
# Running time approximately 24 minutes. 

# Create datasets for analyses:
source(file="W:/ASMA1/data/2_analysis_data_set2.R")
#     Input: rdd_policies.csv + 
#           analysis_set2_id_yearAA_genderB.rds (AA in 11:18 and B in 1:2) +
#           folk_data_201X.csv (X in 1:9)
#     Outputs: data2_local_pol.rds + data2_parametric.rds + data2_local_rand.rds
# Running time approximately 18 minutes. 


### As a second supplementary analysis, we focus on an even narrower subgroup:
# those individuals for whom we observe no between-municipality migration nor a 
# change in family relationships (a proxy for moving away from parents). ###

# Create ID-date panels:
source(file="W:/ASMA1/data/2_id_date_panels_set3.R")
#     Input: visits_changes.rds + rdd_policies.csv + 
#           visits_quality_1X.csv (X in 1:9) + 
#           visits_main_1x.csv (x in 1:9) + folk_data_20xx.csv (xx in 11:20)
#     Interim: interim_set3_id_dateAA_genderB.rds (AA in 11:19 and B in 1:2)
#     Outputs: analysis_set3_id_yearAA_genderB.rds (AA in 11:19 and B in 1:2)
# Running time approximately 20 minutes.

# Create datasets for analyses:
source(file="W:/ASMA1/data/2_analysis_data_set3.R")
#     Input: rdd_policies.csv + 
#           analysis_set3_id_yearAA_genderB.rds (AA in 11:18 and B in 1:2) +
#           folk_data_201X.csv (X in 1:9)
#     Outputs: data3_local_pol.rds + data3_parametric.rds + data3_local_rand.rds
# Running time approximately 14 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Analyze. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Functions that will be used in many R scripts.
#source(file="W:/ASMA1/analysis/3_shared_functions.R")
#     Functions: save.table()


# Analyze how adolescents move away from their parents.
source(file="W:/ASMA1/analysis/3_moving.R")
#     Inputs: rdd_policies.csv + visits_quality_1X.csv (X in 1:9) + 
#             folk_data_201X.csv (X in 1:9) + visits_main_1X.csv, X in 1:9
# Running time approximately 9 minutes. 


# Examine whether there are observable discontinuities in covariates:
source(file="W:/ASMA1/analysis/3_rd_balance_tests.R")
#     Inputs: visits_changes.rds + rdd_policies.csv + 
#             visits_quality_1X.csv (X in 1:9) + folk_data_201X.csv (X in 1:9)
#     Outputs: data_balance_tests.rds + rd_plot_balance.pdf + 
#             rd_plot_balance_730.pdf
# Running time 13 minutes.


# Plot RD plots:
source(file="W:/ASMA1/analysis/3_rd_plots.R")
#     Inputs: data1_local_pol.rds + data2_local_pol.rds + data3_local_pol.rds +
#             data1_parametric.rds
#     Outputs: rd_plot_population.pdf + rd1_plot_raw.pdf + 
#             rd1_plot_health_checks.pdf + rd1_plot.pdf + rd1_plot_men.pdf +
#             rd1_plot_hki.pdf + rd1_plot_refs.pdf + rd2_plot.pdf +
#             rd3_plot.pdf + rd_plot_assistance.pdf
# Running time approximately 1 minute.


# Conduct the RD-DID regression analyses (the local polynomial approach):
source(file="W:/ASMA1/analysis/3_rddid_local_pol.R")
#     Inputs: data1_local_pol.rds + data2_local_pol.rds + data3_local_pol.rds +
#           3_shared_functions.R
#     Outputs: rddid_table(tex + rds + xlsx) + rddid_bw.pdf + rddid1_placebo.pdf
# Running time approximately 1 minutes.


# Conduct the RD-DID regression analyses (the local randomization approach):
source(file="W:/ASMA1/analysis/3_rddid_local_rand.R")
#     Inputs: data1_local_pol.rds + data2_local_pol.rds + data3_local_pol.rds +
#           3_shared_functions.R
#     Outputs: rddid_locrand_table(tex + rds + xlsx) + rddid_locrand_bw.pdf
# Running time approximately 3 minutes.


# Conduct the local polynomial RD analysis:
source(file="W:/ASMA1/analysis/3_rd_local_pol.R")
#     Inputs: data1_local_pol.rds + data2_local_pol.rds + data3_local_pol.rds +
#           3_shared_functions.R
#     Outputs: rd1_locpol_table(tex + rds + xlsx) + 
#           rd1_locpol_results_plot.pdf + rd1_locpol_bw.pdf + 
#           rd1_locpol_spec.pdf + rd1_locpol_placebo.pdf +
#           rd1_locpol_donut(tex + rds + xlsx) + 
#           rd1_locpol_table_refs(tex + rds + xlsx) + 
#           rd2_locpol_table(tex + rds + xlsx) + 
#           rd3_locpol_table(tex + rds + xlsx) + 
#           rd2_locpol_bw.pdf(tex + rds + xlsx) + 
#           rd3_locpol_bw.pdf(tex + rds + xlsx) + 
#           rd2_locpol_table_rdrob(tex + rds + xlsx) + 
#           rd3_locpol_table_rdrob(tex + rds + xlsx)
# Running time approximately 2 minutes. 


# Conduct the analysis using RDHonest:
source(file="W:/ASMA1/analysis/3_rd_rdhonest.R")
#     Inputs: data1_local_pol.rds + data2_local_pol.rds + data3_local_pol.rds + 
#             3_shared_functions.R
#     Outputs: rd1_rdhonest_table(tex + rds + xlsx) + 
#           rd1_rdhonest_table_refs(tex + rds + xlsx) +
#           rd2_rdhonest_table(tex + rds + xlsx) +
#           rd3_rdhonest_table(tex + rds + xlsx)
# Running time <0.5 minutes.


# Conduct the local randomization analysis:
source(file="W:/ASMA1/analysis/3_rd_local_rand.R")
#     Inputs: data1_local_pol.rds + data2_local_pol.rds + data3_local_pol.rds +
#             3_shared_functions.R
#     Outputs: rd1_locrand_table(tex + rds + xlsx) + rd1_locrand_bw.pdf + 
#             rd2_locrand_table(tex + rds + xlsx) + 
#             rd3_locrand_table(tex + rds + xlsx)
# Running time approximately 3 minutes.

Sys.time()

# End.
