
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 3_rd_local_rand.R          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate RD models using the local randomization approach.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(rdlocrand)        # Local randomization methods.
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure.
library(openxlsx)         # Save as excel file.
library(stargazer)        # Save as tex file.

# Inputs:
input.1 = "W:/ASMA1/data/cleaned/data1_local_rand.rds"
input.2 = "W:/ASMA1/data/cleaned/data2_local_rand.rds"
input.3 = "W:/ASMA1/data/cleaned/data3_local_rand.rds"
functions = "W:/ASMA1/analysis/3_shared_functions.R"

# Outputs:
output_rd1_table = "W:/ASMA1/analysis/tables/rd1_locrand_table"
output_rd1_bw = "W:/ASMA1/analysis/figures/rd1_locrand_bw.pdf"
output_rd2_table = "W:/ASMA1/analysis/tables/rd2_locrand_table"
output_rd3_table = "W:/ASMA1/analysis/tables/rd3_locrand_table"

###
###


df.1 = readRDS(input.1)
df.1 = df.1[post_d==0, post_d := -1]

df.2 = readRDS(input.2)
df.2 = df.2[post_d==0, post_d := -1]

df.3 = readRDS(input.3)
df.3 = df.3[post_d==0, post_d := -1]

# Read shared functions
source(functions) # save.table()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) RD models - functions for estimation and plotting. ####
### ### ### ### ### ### ### ### ### ### ### ### ### 


### a function that estimates an RD model ###

rd.estimate_locrand = function(
  df, start_year=2011, copay_policy, client_gender, decile,
  bandwidth, dh=3, outcome) {
  # INPUTS:
  # start_year: this year and later years are included
  # copay_policy: either "exemptions" for those municipalities that exempted 
  #             students from paying (and Helsinki); "copayment" for those 
  #             municipalities where there was a discontinuity in copayments 
  #             at the 18th birthday
  # client_gender: 1 for male, 2 for female
  # decile: a vector of deciles based on the distr. of equivalized 
  #         0:9 are mapped to deciles 1:10.
  # bandwidth: a symmetric bandwidth as an integer
  # dh: either 0 or 3 (an integer)
  # outcome: either 'gp_visits_ann' or 'referrals_ann'
  # OUTPUTS:
  # a data.table containing estimates for a given run

  
  # Extract the subset of data:
  data = df[income_decile_17 %in% decile & policy %in% copay_policy & 
              gender == client_gender & bw==bandwidth & donut_hole==dh &
              year >= start_year]
  data[, outcome := get(outcome)]
  
  # Estimate the model:
  reg = rdlocrand::rdrandinf(Y=data$outcome, R=data$post_d, 
                             wl=-1, wr=1, seed=50, reps=1, quietly=TRUE)
  
  
  # Next, we will collect relevant data from the estimated model:
  
  # Sample sizes:
  n = nrow(data)
  n_ind = length(unique(data$id_no))
  if(outcome=='gp_visits_ann') { n_visits = sum(data$gp_visits) }
  if(outcome=='referrals_ann') { n_visits = sum(data$referrals) }
  
  # Pre-treatment mean in the outcome:
  pre_mean = reg$sumstats[3,1]
  
  # P values:
  p = reg$asy.pvalue
  
  # Main estimate:
  est = reg$obs.stat
  
  # Change (%):
  change = 100 * est / pre_mean
  
  
  # Finally, collect the results to a tidy table:
  
  table = data.table(
    
    "Variable" = c(
      "mean", "above", "change_per", "p_value",
      "n","n_ind","n_visits","bw", 'donut_hole'),
    
    "Values" = c(
      pre_mean, est, change, p, 
      n, n_ind, n_visits, bandwidth, dh)
  )
  
  # From long to wide:
  table = data.table(t(table))
  setnames(table, as.character(table[1,]))
  table = table[-1,][, names(table) := lapply(.SD, as.numeric)]
  

  return(table)
  
}

test = rd.estimate_locrand(
  df.1, start_year=2011, copay_policy='copayment', client_gender=2, 
  decile=c(0:9), bandwidth=30, dh=3, outcome='gp_visits_ann')
print(test)


### a function that estimates the same RD model but with several bandwidths
# and without the 3-day donut hole, collecting results to a table ###

rd.estimate_locrand_bws_donuts = function(
  data, start_year=2011, copay_policy, client_gender, decile, otc) {
  # INPUTS:
  # start_year: this year and later years are included
  # copay_policy: either "exemptions" for those municipalities that exempted 
  #             students from paying (and Helsinki); "copayment" for those 
  #             municipalities where there was a discontinuity in copayments 
  #             at the 18th birthday
  # client_gender: 1 for male, 2 for female
  # decile: a vector of deciles based on the distr. of equivalized 
  #         0:9 are mapped to deciles 1:10.
  # otc: either 'gp_visits_ann' or 'referrals_ann'
  # OUTPUTS:
  # a data.table containing regression results

  
  # Loop over donut holes:
  
  donut_holes = c(0, 3)

  results = lapply(donut_holes, function(donut) {
    
    # Loop over bandwidths:
    
    bws = unique(data$bw) 
    
    regs = lapply(seq_along(bws), function(i) { 
      rd.estimate_locrand(
        data, start_year=start_year, copay_policy=copay_policy, 
        client_gender=client_gender, decile=decile, dh = donut,
        bandwidth=bws[[i]], outcome=otc
      ) 
    })
    regs = as.data.table(do.call("rbind",regs))
    
  })
  results = as.data.table(do.call("rbind",results))
  
}

test = rd.estimate_locrand_bws_donuts(
  df.1, copay_policy="copayment", client_gender=2, decile=c(0:9), 
  otc='gp_visits_ann'
)
print(test)


### a function that plots the results using several bandwidths or 
# several donut holes ###

plot.bws_donuts = function(data, bw_choice) {
  # INPUTS:
  # bw_choice: the choice of preferred bandwidth.
  # OUTPUTS:
  # a plot
  
  
  data = data[, mget(colnames(data))
              ][, donut_hole := as.factor(donut_hole)] 
  
  plot = ggplot(data, aes(x=bw, y=above, group=donut_hole, shape=donut_hole)) +
    geom_point(size=3) +
    ylim(-0.15, 0.15) + 
    scale_x_continuous(breaks=unique(data$bw)) +
    ylab('') +
    xlab('Bandwith (in days)') +
    labs(color='Donut hole size', shape='Donut hole size') +
    theme(text = element_text(size=19), 
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = 'bottom') +
    geom_hline(yintercept=0, linetype="dashed")  +
    geom_vline(xintercept=bw_choice, linetype="dashed")
  
  
  return(plot)
}

plot.bws_donuts(test, bw_choice = 30)


### a function that computes the results (regression tables and bandwidth 
# plots) ###

rdd_results = function(data, models_list, bw) {
  
  # Estimate the results using several bandwidths and save them to a table:
  results_bws = lapply(models_list, function(x) {
    regs = rd.estimate_locrand_bws_donuts(
      data, start_year = x[[1]], copay_policy = x[[2]], client_gender = x[[3]], 
      decile = x[[4]], otc = x[[6]]
    )
  })
  
  # Plot a bandwidth plot for each of the models:
  plots_bws = lapply(results_bws, function(x) {
    plot = plot.bws_donuts(x, bw_choice=bw)
  })
  
  return(list(tables_bw = results_bws, plots_bw = plots_bws))
  
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Estimation. ####
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

# Estimate:
results = rdd_results(df.1, models, bw=30)


### Result tables (one chosen bandwidth) ###

rd.tables = function(data, bw, output) {
  # INPUTS:
  # data: results from rdd_results()
  # bw: a choice of bandwidth
  # output: file path for the output
  # RD table 
  
  bw_choice = bw
  
  results = list(data$tables_bw$women_copay_all,
                 data$tables_bw$women_copay_bot20,
                 data$tables_bw$women_copay_bot40,
                 data$tables_bw$women_copay_top50,
                 
                 data$tables_bw$women_freeexempt_all,
                 data$tables_bw$women_freeexempt_bot20,
                 data$tables_bw$women_freeexempt_bot40,
                 data$tables_bw$women_freeexempt_top50)
  
  
  tables = lapply(results, function(x) {
    
    df = x[bw == bw_choice & donut_hole==3]
    rows = rownames(t(df))
    
    df = as.data.table(t(df))
    setnames(df, 'Value')
    
    df[, Variable := rows]
    
    df = df[Variable %in% c("mean", "above", "p_value", "change_per", 
                            "n_ind", "n_visits")
            ][, Variable := c("Level", "RD estimate", "Change, %", 
                              "P value", "Individuals", "Visits")]
    
    setcolorder(df, c('Variable', 'Value'))
    
  })
  
  table1 = cbind(tables[[1]], tables[[2]], tables[[3]], tables[[4]])
  table2 = cbind(tables[[5]], tables[[6]], tables[[7]], tables[[8]])
  blank = matrix(nrow = 1, ncol = 8)
  colnames(blank) = rep(c("Variable", "Value"), times=4)
  
  table = rbind(table1, blank, table2)[,c(1,2,4,6,8)]
  colnames(table) = c("A. Copayment.","All","Bottom 20%","Bottom 40%","Top 50%")
  table[8,1] = "B. Helsinki or Exemption."
  
  save.table(table, output, label_tex = 'tab:rdd_locrand_table_women', 
             title_tex = 'Local Randomization Results for Women')
  
}

rd.tables(results, bw=30, output = output_rd1_table)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Robustness checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

### Sensitivity plots (to different bandwidths) ###

cairo_pdf(filename = output_rd1_bw, width = 16.0, height = 8.0)
print(
  (results$plots_bw$women_copay_all + ggtitle("Copay: All")) +
    (results$plots_bw$women_copay_bot20 + ggtitle("Copay: Bottom 20%")) +
    (results$plots_bw$women_copay_bot40 + ggtitle("Copay: Bottom 40%")) +
    (results$plots_bw$women_copay_top50 + ggtitle("Copay: Top 50%.")) +
    (results$plots_bw$women_freeexempt_all + ggtitle("Exempt: All")) +
    (results$plots_bw$women_freeexempt_bot20 + ggtitle("Exempt: Bottom 20%")) +
    (results$plots_bw$women_freeexempt_bot40 + ggtitle("Exempt: Bottom 40%")) +
    (results$plots_bw$women_freeexempt_top50 + ggtitle("Exempt: Top 50%")) +
    plot_layout(ncol=4, guides = 'collect') &
    theme(legend.position = 'bottom') )
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Alternative samples. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# List all the models that will be estimated:
models = mods(start_year=2011, outcome='gp_visits_ann')

# First, estimate the results.
results.alt.1 = rdd_results(df.2, models[1:8], bw=30)
results.alt.2 = rdd_results(df.3, models[1:8], bw=30)

# Save tables:
rd.tables(results.alt.1, bw=30, output = output_rd2_table)
rd.tables(results.alt.2, bw=30, output = output_rd3_table)

# End.
