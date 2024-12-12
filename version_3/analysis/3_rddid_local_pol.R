
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 3_rddid_local_pol.R        ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2023 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate RD-DID models with the local polynomial approach.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(lfe)              # linear fixed effects estimation.
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure.
library(openxlsx)         # Save as excel file.
library(stargazer)        # Save as tex file.

# Inputs:
input.1 = "W:/ASMA1/data/cleaned/data1_local_pol.rds"
input.2 = "W:/ASMA1/data/cleaned/data2_local_pol.rds"
input.3 = "W:/ASMA1/data/cleaned/data3_local_pol.rds"
functions = "W:/ASMA1/analysis/3_shared_functions.R"

# Outputs:
output_rddid_table = "W:/ASMA1/analysis/tables/rddid_table"
output_rddid_bw = "W:/ASMA1/analysis/figures/rddid_bw.pdf"
output_rddid1_placebo = "W:/ASMA1/analysis/figures/rddid1_placebo.pdf"
output_rddid_table_donut = "W:/ASMA1/analysis/tables/rddid_table_donut"
output_rddid_table_2015 = "W:/ASMA1/analysis/tables/rddid_table_2015"

###
###

# Read shared functions and datasets:
source(functions) # save.table()
df.1 = readRDS(input.1)
df.2 = readRDS(input.2)
df.3 = readRDS(input.3)


dfs = lapply(list(df.1, df.2, df.3), function(data) {
  
  # Results are estimated for women only:
  df = data[gender==2]
  
  # Some name changes and treatment variables are needed:
  
  setnames(df, old='running_var', new='relative_time')
  
  df[, above := as.integer(relative_time >= 0)]
  df[, relative_time_X_above := above * relative_time]
  df[, treat := as.integer(policy == 'copayment')]
  df[, ':=' (relative_time_X_treat = treat * relative_time,
             above_X_treat = above * treat,
             relative_time_X_above_X_treat = relative_time * above * treat)]
  
})

df.1 = dfs[[1]]
df.2 = dfs[[2]]
df.3 = dfs[[3]]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) RD-DID models - functions for estimation and plotting. ####
### ### ### ### ### ### ### ### ### ### ### ### ### 


# Standard errors: robust standard errors
# No kernel weighting (= uniform kernel)
# Inference as if the regression function is exactly correct.

### A function that estimates the effect on the treated 
# (copayment municipalities) relative to those untreated. ###

rddid.estimate = function(
  df, start_year=2011, copay_policy=c("copayment", "exemptions", 'free'),
  client_gender=2, decile=c(0:9), bw=180, donut_hole=3) {
  # INPUTS:
  # start_year: this year and later years are included
  # copay_policy: either "exemptions" for those municipalities that exempted 
  #             students from paying (and Helsinki); "copayment" for those 
  #             municipalities where there was a discontinuity in copayments 
  #             at the 18th birthday
  # client_gender: 1 for male, 2 for female
  # decile: a vector of deciles based on the distr. of equivalized 
  #         0:9 are mapped to deciles 1:10.
  # bw: a symmetric bandwidth as an integer
  # donut_hole: a number of days that are excluded from both sides of the cutoff
  
  
  # Extract relevant subset of data:
  data = df[year >= start_year & policy %in% copay_policy & 
              gender == client_gender & income_decile_17 %in% decile & 
              relative_time %in% c(-bw:(bw-1))]
  if(donut_hole > 0) { 
    data = data[!(relative_time %in% c(-donut_hole:(donut_hole-1)))] }
  
  # Aggregate:
  data = data[, .(gp_visits_ann = weighted.mean(gp_visits_ann, w=population),
                  population = sum(population)), 
              by=c('relative_time', 'above', 'relative_time_X_above', 'treat',
                   'relative_time_X_treat', 'above_X_treat', 
                   'relative_time_X_above_X_treat')]
  
  
  # Estimate the model and collect results:
  
  reg = lfe::felm(
    gp_visits_ann ~ relative_time + above + relative_time_X_above + treat + 
      relative_time_X_treat + above_X_treat + relative_time_X_above_X_treat, 
    weights = data$population, data=data
  )
  
  reg = summary(reg, robust=TRUE)$coefficients
  
  est = reg['above_X_treat', 'Estimate']
  se = reg['above_X_treat', 'Robust s.e']
  p = reg['above_X_treat', 'Pr(>|t|)']
  pre_mean = reg['(Intercept)', 'Estimate'] + reg['above', 'Estimate'] + 
    reg['treat', 'Estimate']
  
  individuals = data[relative_time==(-1-donut_hole), sum(population)]
  
  # Collect results to a table:
  table = data.frame(
    "Variable" = c('pre_mean', "above", "change_per", "std_error", "conf_low",
                   "conf_high", "p_value", "n", "bw"),
    "Values" = c(pre_mean, est, 100 * est / pre_mean, se, 
                 est - qnorm(0.05/2,lower=F) * se,
                 est + qnorm(0.05/2,lower=F) * se,
                 p, individuals, bw)) 
  
  # Mutate the table:
  table = transpose(table)
  colnames(table) = unlist(table[1, ])
  table = table[2, ]
  
  table = lapply(table, as.numeric)
  table = do.call(cbind.data.frame, table)
  
  return(table)
}

test = rddid.estimate(df=df.1, client_gender = 2, decile = c(0:9))
test


### A function that estimates the same RD-DID models but with 
# several bandwidths and collects results to a table. ###

rddid.estimate_bws = function(
  data, start_year=2011, copay_policy=c("copayment","exemptions", 'free'), 
  client_gender, decile=c(0:9), bw=180, donut_hole=3) {
  # INPUTS:
  # start_year: this year and later years are included
  # copay_policy: either "exemptions" for those municipalities that exempted 
  #             students from paying (and Helsinki); "copayment" for those 
  #             municipalities where there was a discontinuity in copayments 
  #             at the 18th birthday
  # client_gender: 1 for male, 2 for female
  # decile: a vector of deciles based on the distr. of equivalized 
  #         0:9 are mapped to deciles 1:10.
  # bw: the baseline bandwidth as an integer
  # donut_hole: a number of days that are excluded from both sides of the cutoff
 
  
  # Bandwidths:
  if(data[, min(relative_time) < -700]) { vec = seq(80, 300, by=10) }
  if(data[, min(relative_time) > -400]) { vec = seq(80, 200, by=10) }
  
  
  regs = lapply(vec, function(i) { 
    
    reg = rddid.estimate(
      df = data, start_year = start_year, copay_policy = copay_policy,
      client_gender = client_gender, decile = decile, bw = i, donut_hole = 3
    )
    
  })
  
  regs = setDT(data.frame(do.call("rbind",regs)))
  return(regs)
}

test = rddid.estimate_bws(data=df.1, client_gender=2, decile=c(0:9))
print(test)


### a function that plots the results using several bandwidths. ###

plot.bws = function(data) {
  # OUTPUTS:
  # a bandwidth plot
  
  
  plot = ggplot(data, aes(x=bw, y=above)) +
    labs(x="Bandwidth", y="") +
    geom_point(data=data[data$bw!=180,], aes(x=bw, y=above), color='grey40') +
    geom_point(data=data[data$bw==180,], 
               aes(x=bw, y=above), shape=15, size=3) +
    geom_segment(data=data[data$bw!=180,], aes(x=bw, y=conf_low, xend=bw, 
                                             yend=conf_high), color='grey40') +
    geom_segment(data=data[data$bw==180,], 
                 aes(x=bw, y=conf_low, xend=bw, yend=conf_high)) +
    theme(text = element_text(size=19),
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, 
                                      size = 0.5)) +
    geom_hline(yintercept=0, linetype="dashed") 
  
  return(plot)
}

plot.bws(test)


### a function that conducts falsification tests exploiting 
# placebo thresholds  ###

# First, list all possible placebo cutoffs for which
# - modulo(x, 30) == 0
# - we can construct a 150-day (approximately 5 months) bandwidth.
# Note that we only use data from one side of the real cutoff at a given run.

list_placebos = function(run_var, bw) {
  # INPUTS:
  # run_var: centered values of the running variables sorted
  # bw: bandwidth, say 150
  # OUTPUTS:
  # a list containing the set of relative_time cohorts for each placebo run
  
  
  neg = run_var[run_var < 0]
  pos = run_var[run_var >= 0]
  placebo_lst = list()
  
  i = 0
  last = -2
  while(last < -1) {
    i = i + 1
    
    first = neg[i]
    first_end = first + bw - 1
    cutoff = first_end + 1
    last = cutoff + (bw-1)
    ret = list(cutoff=cutoff, bw_vals =c(first:first_end, cutoff:last)) 
    if(cutoff %% 30 == 0) { placebo_lst[[i]] = ret } 
  }
  
  i = 0
  last_pos = tail(pos, n=1)
  length_lst = length(placebo_lst)
  while(last < last_pos) {
    i = i + 1
    
    first = pos[i]
    first_end = first + bw - 1
    cutoff = first_end + 1
    last = cutoff + (bw-1)
    ret = list(cutoff=cutoff, bw_vals =c(first:first_end, cutoff:last)) 
    if(cutoff %% 30 == 0) { placebo_lst[[length_lst + i]] = ret }
  }
  
  return(placebo_lst[lengths(placebo_lst) != 0])
}

list_placebos(c(-730:-1, 0:729), 150)


### A function that estimates placebo effects. ###

rddid.estimate_placebo = function(
  data, start_year=2011, copay_policy=c("copayment", "exemptions", 'free'), 
  client_gender=2, decile=c(0:9), donut_hole=3, bw_choice=150) {
  # INPUTS:
  # start_year: this year and later years are included
  # copay_policy: either "exemptions" for those municipalities that exempted 
  #             students from paying (and Helsinki); "copayment" for those 
  #             municipalities where there was a discontinuity in copayments 
  #             at the 18th birthday
  # client_gender: 1 for male, 2 for female
  # decile: a vector of deciles based on the distr. of equivalized 
  #         0:9 are mapped to deciles 1:10.
  # donut_hole: as an integer
  # bw_choice: bandwidth choice as an integer if "donut" is chosen.
  # OUTPUTS:
  # a data.table containing effect estimates
  
  
  # First, get a list of relative_time subsets that we will loop over:
  running_var = sort(unique(data$relative_time))
  placebo_lst = list_placebos(running_var, bw_choice)
  
  
  # Estimate the placebo effects:
  
  regs = lapply(placebo_lst, function(x) {
    
    # Mutate the data:
    df = data[, mget(colnames(data))]
    df[, relative_time := relative_time - x$cutoff]
    
    # Update the treatment indicators:
    df[, above := as.integer(relative_time >= 0)]
    df[, ':=' (relative_time_X_above = above * relative_time,
               relative_time_X_treat = treat * relative_time,
               above_X_treat = above * treat,
               relative_time_X_above_X_treat = relative_time * above * treat)]
    
    # Estimate:
    reg = rddid.estimate(
      df = df, start_year = start_year, copay_policy = copay_policy,
      client_gender = client_gender, decile = decile, bw = bw_choice, 
      donut_hole = donut_hole
    )
    
    reg = as.data.table(reg)[, cutoff := x$cutoff]
    
  })
  regs = as.data.table(do.call("rbind", regs))
  
  
  # Also estimate the base model:
  
  base = rddid.estimate(
    data, start_year = start_year, copay_policy = copay_policy, 
    client_gender = client_gender, decile = decile, donut_hole = donut_hole,
    bw = bw_choice
  )
  base = as.data.table(base)[, cutoff := 0]
  regs = rbind(regs, base) 
  regs = regs[order(above)]
  regs[, cutoff_no := c(1:nrow(regs))]
  
  return(regs)
}

test_placebo = rddid.estimate_placebo(
  data=df.1, client_gender=2, decile=c(0:9), bw_choice = 150
)
print(test_placebo)


### a function that plots the results using placebo cutoffs ###

plot.placebo = function(data) {
  # INPUTS:
  # data: RD estimates from rd.estimate_local_pol_placebo
  # OUTPUTS:
  # placebo plot
  
  
  x_vals = sort(unique(data$cutoff_no))
  x_labels = data$cutoff
  
  plot = ggplot(data, aes(x=cutoff_no, y=above)) +
    labs(x="Placebo cutoffs", y="") +
    geom_point(data=data[data$cutoff!=0,], aes(x=cutoff_no, y=above),
               color='grey40') +
    geom_point(data=data[data$cutoff==0,], aes(x=cutoff_no, y=above), 
               shape=15, size=3) +
    geom_segment(data=data[data$cutoff!=0,], 
                 aes(x=cutoff_no, y=conf_low, xend=cutoff_no, yend=conf_high),
                 color='grey40') +
    geom_segment(data=data[data$cutoff==0,], 
                 aes(x=cutoff_no, y=conf_low, xend=cutoff_no, yend=conf_high)) +
    
    scale_x_continuous(breaks=c(x_vals[1], x_vals[which(x_labels==0)], 
                                tail(x_vals, n=1))) +
    
    theme(text = element_text(size=19),     
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor.x = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, 
                                      size = 0.5)) +
    geom_hline(yintercept=0, linetype="dashed") 
  
  return(plot)
  
}
plot.placebo(test_placebo)


### a function that computes the results. ###

rddid_results = function(data, models_list, 
                         policies=c("copayment", "exemptions", 'free')) {
  
  # Main results with a 180-day bandwidth and 3-day donut hole:
  main_results = lapply(models_list, function(x) {
    reg = rddid.estimate(df=data, client_gender = 2, decile = x[[2]],
                         start_year = x[[3]], copay_policy=policies)
    reg = as.data.table(reg)
    reg[, mod := x[[1]]]
  })
  
  # Results WITHOUT the 3-day donut hole:
  results_donut = lapply(models_list, function(x) {
    reg = rddid.estimate(df=data, client_gender = 2, decile = x[[2]],
                         start_year = x[[3]], 
                         copay_policy=policies, donut_hole = 0)
    reg = as.data.table(reg)
    reg[, mod := x[[1]]]
  })
  
  # Estimate the results using several bandwidths and save them to a table:
  results_bws = lapply(models_list, function(x) {
    regs = rddid.estimate_bws(data=data, client_gender = 2, decile = x[[2]],
                              start_year = x[[3]], copay_policy=policies)
    regs = as.data.table(regs)
    regs[, mod := x[[1]]]
  })
  
  # Plot a bandwidth plot for each of the models:
  plots_bws = lapply(results_bws, function(x) {
    plot = plot.bws(x)
  })
  
  # Estimate the results using several placebo cutoffs and save them to a table:
  results_placebo = lapply(models_list, function(x) {
    regs = rddid.estimate_placebo(
      data, client_gender=2, decile=x[[2]], bw_choice = 150,
      start_year = x[[3]], copay_policy = policies
    )
  })
  
  # Plot the estimates from placebo cutoffs:
  plots_placebo = lapply(results_placebo, function(x) {
    plot = plot.placebo(x)
  })
  
  
  return(list(tables_main = main_results,
              tables_donut = results_donut,
              tables_bw = results_bws, plots_bw = plots_bws,
              tables_placebo=results_placebo, plots_placebo=plots_placebo))
  
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) RD-DID models - implement estimation. ####
### ### ### ### ### ### ### ### ### ### ### ### ### 

models = list(list("All",c(0:9), 2011), 
              list("Bottom 20%",c(0:1), 2011),
              list("Bottom 40%",c(0:3), 2011),
              list("Top 50%",c(5:9), 2011))

results = rddid_results(df.1, models)
results.alt.1 = rddid_results(df.2, models)
results.alt.2 = rddid_results(df.3, models)
results.hki.1 = rddid_results(df.1, models, policies = c('copayment', 'free'))
results.hki.2 = rddid_results(df.2, models, policies = c('copayment', 'free'))
results.hki.3 = rddid_results(df.3, models, policies = c('copayment', 'free'))


### RD-DID results tables ###

rddid.table = function(results, donut=0, output_policy) {
  # donut: 1: results WITHOUT a 3-day donut hole that is used in main analysis.
  
  tables = lapply(results, function(res) {
    
    if(donut==1) {
      data = res$tables_donut
    } else {
      data = res$tables_main }
    
    table = lapply(data, function(x) {
      
      rows = rownames(t(x))
      df = as.data.table(t(x))
      
      setnames(df, 'Value')
      
      df[, Value := as.numeric(Value)]
      df[, Variable := rows]
      
      df = df[Variable %in% c('pre_mean', "above", "change_per", "std_error", 
                              "p_value", "n")
              ][, Variable := c('Level', "RD-DID estimate", "Change, %",  
                                "Std. error", "P-value", 'Individuals')]
      
      setcolorder(df, c('Variable', 'Value'))
      
    })
    
    table_policy = cbind(table[[1]],table[[2]],table[[3]],table[[4]])
    table_policy = table_policy[, c(1,2,4,6,8)]
    colnames(table_policy) = c("A. Women.", "All", "Bottom 20%", 
                               "Bottom 40%", "Top 50%")
    
    return(table_policy)
    
  })
  
  tables = do.call(rbind.data.frame, tables)
  
  save.table(tables, output_policy, 
             label_tex = 'tab:table', title_tex = 'Title')
  
}

rddid.table(list(results, results.alt.1, results.alt.2, 
                 results.hki.1, results.hki.2, results.hki.3), 
            output_policy = output_rddid_table)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) RD-DID models - robustness checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ### 


### RD-DID results bw sensitivity plots ###

cairo_pdf(filename = output_rddid_bw, width = 16.0, height = 12)
print(
  (results$plots_bw[[1]] + ggtitle("Main: All")) + 
    (results$plots_bw[[2]] + ggtitle("Main: Bottom 20%")) +
    (results$plots_bw[[3]] + ggtitle("Main: Bottom 40%")) +
    (results$plots_bw[[4]] + ggtitle("Main: Top 50%")) +
    (results.alt.1$plots_bw[[1]] + ggtitle("Alt 1: All")) + 
    (results.alt.1$plots_bw[[2]] + ggtitle("Alt 1: Bottom 20%")) +
    (results.alt.1$plots_bw[[3]] + ggtitle("Alt 1: Bottom 40%")) +
    (results.alt.1$plots_bw[[4]] + ggtitle("Alt 1: Top 50%")) +
    (results.alt.2$plots_bw[[1]] + ggtitle("Alt 2: All")) + 
    (results.alt.2$plots_bw[[2]] + ggtitle("Alt 2: Bottom 20%")) +
    (results.alt.2$plots_bw[[3]] + ggtitle("Alt 2: Bottom 40%")) +
    (results.alt.2$plots_bw[[4]] + ggtitle("Alt 2: Top 50%")) +
  plot_layout(ncol=4) )
dev.off()


### Estimates at placebo cutoffs. ###

cairo_pdf(filename = output_rddid1_placebo, width = 16.0, height = 4)
print(
  (results$plots_placebo[[1]] + ggtitle("All")) + 
    (results$plots_placebo[[2]] + ggtitle("Bottom 20%")) +
    (results$plots_placebo[[3]] + ggtitle("Bottom 40%")) +
    (results$plots_placebo[[4]] + ggtitle("Top 50%")) +
    plot_layout(ncol=4) )
dev.off()


# Show the main results WITHOUT a 3-day donut hole:
rddid.table(list(results, results.alt.1, results.alt.2, 
                 results.hki.1, results.hki.2, results.hki.3),
            donut = 1, output_policy = output_rddid_table_donut)



### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Effects using data from 2015-2019. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

models = list(list("All",c(0:9), 2015), 
              list("Bottom 20%",c(0:1), 2015),
              list("Bottom 40%",c(0:3), 2015),
              list("Top 50%",c(5:9), 2015))

results.2015 = rddid_results(df.1, models)

rddid.table(list(results.2015), output_policy = output_rddid_table_2015)

# End.
