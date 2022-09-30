
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 3_rd_rdhonest.R           ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate RD models using RDHonest.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(RDHonest)         # Honest inference in RDDs.
library(openxlsx)         # Save as excel file.
library(stargazer)        # Save as tex file.

# Inputs:
input.1 = "W:/ASMA1/data/cleaned/data1_local_pol.rds"
input.2 = "W:/ASMA1/data/cleaned/data2_local_pol.rds"
input.3 = "W:/ASMA1/data/cleaned/data3_local_pol.rds"
functions = "W:/ASMA1/analysis/3_shared_functions.R"

# Outputs:
output_rd1_table = "W:/ASMA1/analysis/tables/rd1_rdhonest_table"
output_rd1_table_refs = "W:/ASMA1/analysis/tables/rd1_rdhonest_table_refs"
output_rd2_table = "W:/ASMA1/analysis/tables/rd2_rdhonest_table"
output_rd3_table = "W:/ASMA1/analysis/tables/rd3_rdhonest_table"

###
###


# Results are estimated for women only:

df.1 = readRDS(input.1)
df.1= df.1[gender==2]

df.2 = readRDS(input.2)
df.2 = df.2[gender==2]

df.3 = readRDS(input.3)
df.3 = df.3[gender==2]

# Read shared functions
source(functions) # save.table()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) RD models - functions for estimation and plotting. ####
### ### ### ### ### ### ### ### ### ### ### ### ### 


### a function that estimates an RD model ###

rd.estimate_rdhonest = function(
  df, start_year=2011, copay_policy, client_gender, decile, 
  order=1, donut_hole=3, outcome) {
  # INPUTS:
  # start_year: this year and later years are included
  # copay_policy: either "exemptions" for those municipalities that exempted 
  #             students from paying (and Helsinki); "copayment" for those 
  #             municipalities where there was a discontinuity in copayments 
  #             at the 18th birthday
  # client_gender: 1 for male, 2 for female
  # decile: a vector of deciles based on the distr. of equivalized 
  #         0:9 are mapped to deciles 1:10.
  # order: 0=means, 1=linear, 2=quadratic
  # donut_hole: as an integer
  # outcome: either 'gp_visits_ann' or 'referrals_ann'
  # OUTPUTS:
  # a data.table containing regression results for a given run
  
  
  # Extract relevant subset of data:
  data = df[year >= start_year & policy %in% copay_policy & 
              gender == client_gender & income_decile_17 %in% decile]
  if(donut_hole > 0) { 
    data = data[!(running_var %in% c(-donut_hole:(donut_hole-1)))] }
  
  # Aggregate:
  data[, outcome := get(outcome)]
  data = data[, .(outcome = weighted.mean(outcome, w=population),
                  population = sum(population)), by='running_var']
  
  
  # Choose the smoothness constant M in a data-driven way.
  DT = data[, .(outcome, running_var, population)]
  setnames(DT, old='population', new='weights')
  DT = RDHonest::RDData(DT, cutoff = 0)
  M = NPR_MROT.fit(DT)
  
  # Estimate:
  out = RDHonest::RDHonest(
    outcome ~ running_var, cutoff = 0, data=data, se.method = 'nn',
    weights = data$population, bw.equal = FALSE, order=order,
    kern = 'triangular', M = M, opt.criterion = 'MSE', sclass = 'H') 
  
  
  # Next, we will collect relevant data from the estimated model:
  
  # Sample sizes:
  bw_l = out$hm
  bw_r = out$hp
  individuals = data[running_var==(-1-donut_hole), population]
  
  # Pre-treatment mean in the outcome:
  DT.2 = data[running_var > -bw_l & running_var < 0]
  ols = lm(outcome ~ running_var, data=DT.2)
  pre_mean = ols$coefficients['(Intercept)']
  
  # Main estimate:
  est = out$estimate
  
  # Std. error:
  se = out$sd
  
  # Confidence intervals:
  conf_low = out$lower
  conf_high = out$upper
  
  # Change (%):
  change = 100 * est / pre_mean
  
  
  # Finally, collect the results to a tidy table:
  
  table = data.table(
    
    "Variable" = c(
      "mean", "above", 'std_error', "change_per",
      "conf_low", "conf_high",
      "individuals","bw_l","bw_r",
      'order', 'donut_hole'),
    
    "Values" = c(
      pre_mean, est, se, change,
      conf_low, conf_high,
      individuals, bw_l, bw_r,
      order, donut_hole)
  )
  
  # From long to wide:
  table = data.table(t(table))
  setnames(table, as.character(table[1,]))
  table = table[-1,][, names(table) := lapply(.SD, as.numeric)]
  
  return(table)

}

# Using the function:
test = rd.estimate_rdhonest(
  df.1, start_year = 2011, copay_policy="copayment", client_gender=2, 
  decile=c(0:3), order=1, donut_hole = 3, 
  outcome='gp_visits_ann'
)
print(test)


### a function that computes the results. ###

rdd_results_rdhonest = function(data, models_list) {
  
  # Estimate the main results:
  results_main = lapply(models_list, function(x) {
    regs = rd.estimate_rdhonest(
      data, start_year = x[[1]], copay_policy = x[[2]], client_gender = x[[3]], 
      decile = x[[4]], order=1, donut_hole=3, outcome = x[[6]]
    )
  })
  
  return(list(results_main=results_main))
  
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) RD models - implement estimation. ####
### ### ### ### ### ### ### ### ### ### ### ### ### 

# List all the models that will be estimated:
mods = function(start_year, gender=2, outcome) {
  ret = list(
    women_copay_all = list(start_year, "copayment", gender, c(0:9), "Copayment, All.", outcome),
    women_copay_bot20 = list(start_year, "copayment", gender, c(0:1), "Copayment, Bottom 20%.", outcome),
    women_copay_bot40 = list(start_year, "copayment", gender, c(0:3), "Copayment, Bottom 40%.", outcome),
    women_copay_top50 = list(start_year, "copayment", gender, c(5:9), "Copayment, Top 50%.", outcome),
    
    women_freeexempt_all = list(start_year, c('exemptions', 'free'), gender, c(0:9), "Exemption, All.", outcome),
    women_freeexempt_bot20 = list(start_year, c('exemptions', 'free'), gender, c(0:1), "Exemption, Bottom 20%.", outcome),
    women_freeexempt_bot40 = list(start_year, c('exemptions', 'free'), gender, c(0:3), "Exemption, Bottom 40%.", outcome),
    women_freeexempt_top50 = list(start_year, c('exemptions', 'free'), gender, c(5:9), "Exemption, Top 50%.", outcome)
  )
  return(ret)
}
models = mods(start_year=2011, outcome='gp_visits_ann')

results = rdd_results_rdhonest(df.1, models)


### Result tables (one chosen bandwidth) ###

rd.tables.rdhonest = function(data, output) {
  # INPUTS:
  # data: results from rdd_results_rdhonest()
  # output: file path for the output
  # RD table 
  
  
  data = data$results_main
  
  results = list(data$women_copay_all,
                 data$women_copay_bot20,
                 data$women_copay_bot40,
                 data$women_copay_top50,
                 
                 data$women_freeexempt_all,
                 data$women_freeexempt_bot20,
                 data$women_freeexempt_bot40,
                 data$women_freeexempt_top50)
  
  
  tables = lapply(results, function(x) {
    
    part1 = format(round(c(x$mean, x$above, x$std_error, x$change_per, x$p), 
                         digits=3), nsmall=3)
    
    if(x$conf_low < 0) { sign.low = '-' } else { sign.low = '' }
    if(x$conf_high < 0) { sign.high = '-' } else { sign.high = ''}
    
    part2 = paste(
      '[', sign.low, format(abs(round(x$conf_low, digits=2)), nsmall=2), ', ', 
      sign.high, format(abs(round(x$conf_high, digits=2)), nsmall=2), ']', 
      sep='')
    
    part3 = as.character(x$individuals)
    part4 = paste('(', ceiling(x$bw_l), ', ', floor(x$bw_r), ')', sep='')
    
    table = data.table(
      'Variable' = c("Level", "RD estimate", "Std. error", "Change (%)", 
                     "CI", "Individuals", "Bandwidth"),
      'Value' = c(part1, part2, part3, part4))
    
  })
  
  table1 = cbind(tables[[1]], tables[[2]], tables[[3]], tables[[4]])
  table2 = cbind(tables[[5]], tables[[6]], tables[[7]], tables[[8]])
  blank = matrix(nrow = 1, ncol = 8)
  colnames(blank) = rep(c("Variable", "Value"), times=4)
  
  table = rbind(table1, blank, table2)[,c(1,2,4,6,8)]
  colnames(table) = c("A. Copayment.","All","Bottom 20%","Bottom 40%","Top 50%")
  table[8,1] = "B. Helsinki or Exemption."
  
  save.table(table, output, label_tex = 'tab:rdd_table_women_rdhonest', 
             title_tex = 'RD Results for Women, RDHonest')
  
}

rd.tables.rdhonest(results, output = output_rd1_table)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Effects on referrals. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# List all the models that will be estimated:
models = mods(start_year=2011, outcome='referrals_ann')

# Estimate and save:
results.refs = rdd_results_rdhonest(df.1, models)
rd.tables.rdhonest(results.refs, output = output_rd1_table_refs)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Alternative samples. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# List all the models that will be estimated:
models = mods(start_year=2011, outcome='gp_visits_ann')

# First, estimate the results with a fixed 180-day bandwidth:
results.alt.1 = rdd_results_rdhonest(df.2, models)
results.alt.2 = rdd_results_rdhonest(df.3, models)

# Save tables:
rd.tables.rdhonest(results.alt.1, output = output_rd2_table)
rd.tables.rdhonest(results.alt.2, output = output_rd3_table)

# End.
