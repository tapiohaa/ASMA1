
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 3_rd_local_pol.R           ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate RD models.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(rdrobust)         # Tools for RD designs.
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
output_rd1_table = "W:/ASMA1/analysis/tables/rd1_locpol_table"
output_rd1_plot = "W:/ASMA1/analysis/figures/rd1_locpol_results_plot.pdf"
output_rd1_bw = "W:/ASMA1/analysis/figures/rd1_locpol_bw.pdf"
output_rd1_spec = "W:/ASMA1/analysis/figures/rd1_locpol_spec.pdf"
output_rd1_placebo = "W:/ASMA1/analysis/figures/rd1_locpol_placebo.pdf"
output_rd1_table_donut = "W:/ASMA1/analysis/tables/rd1_locpol_donut"
output_rd1_table_refs = "W:/ASMA1/analysis/tables/rd1_locpol_table_refs"
output_rd2_table = "W:/ASMA1/analysis/tables/rd2_locpol_table"
output_rd3_table = "W:/ASMA1/analysis/tables/rd3_locpol_table"
output_rd2_bw = "W:/ASMA1/analysis/figures/rd2_locpol_bw.pdf"
output_rd3_bw = "W:/ASMA1/analysis/figures/rd3_locpol_bw.pdf"
output_rd2_table_rdrob = "W:/ASMA1/analysis/tables/rd2_locpol_table_rdrob"
output_rd3_table_rdrob = "W:/ASMA1/analysis/tables/rd3_locpol_table_rdrob"


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

rd.estimate_local_pol = function(
  df, start_year=2011, copay_policy, client_gender, decile,
  order=1, donut_hole=3, bw=NULL, outcome) {
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
  # bw: bandwidth as an integer. If NULL, use data.driven methods.
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
  
  # Estimate:
  
  if(is.null(bw)) {
    
    out = rdrobust::rdrobust(
      y=data$outcome, x=data$running_var, weights = data$population,
      c = -0.5, p = order, q = order + 1, 
      kernel = 'tri', bwselect = "msetwo", vce = "nn"
    )
    
  } else {
    
    out = rdrobust::rdrobust(
      y=data$outcome, x=data$running_var, weights = data$population,
      c = -0.5, p = order, q = order + 1, 
      kernel = 'tri', h = c(bw,bw), b = c(bw,bw), vce = "nn"
    )
    
  }
  
  
  # Next, we will collect relevant data from the estimated model:
  
  # Sample sizes:
  bw_l = out$N_h[1]
  bw_r = out$N_h[2]
  individuals = data[running_var==(-1-donut_hole), population]
  
  # Pre-treatment mean in the outcome:
  pre_mean = out$tau_cl[1]
  
  # Main estimate:
  est = out$coef['Conventional', 'Coeff']
  
  # Confidence intervals:
  conf_low = out$ci['Robust', 'CI Lower']
  conf_high = out$ci['Robust', 'CI Upper']
  
  # P value:
  p = out$pv['Robust', ]
  
  # Change (%):
  change = 100 * est / pre_mean
  
  
  # Finally, collect the results to a tidy table:
  
  table = data.table(
    
    "Variable" = c(
      "mean", "above", "change_per",
      "conf_low", "conf_high", 'p',
      "individuals","bw_l","bw_r",
      'order', 'donut_hole'),
    
    "Values" = c(
      pre_mean, est, change,
      conf_low, conf_high, p,
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
test = rd.estimate_local_pol(
  df.1, start_year = 2011, copay_policy="copayment", client_gender=2, 
  decile=c(0:3), order=1, donut_hole = 3, outcome='gp_visits_ann'
)
print(test)


### a function that plots an RD model ###

rd.estimate_local_pol_plot = function(results_row,
  df, start_year=2011, copay_policy, client_gender, decile,
  order=1, donut_hole=3, title, outcome) {
  # INPUTS:
  # results_row: output from rd.estimate_local_pol() 
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
  # title for the RD plot:
  # outcome: either 'gp_visits_ann' or 'referrals_ann'
  # OUTPUTS:
  # a data.table containing regression results for a given run
  
  
  # Extract relevant subset of data:
  DT = df[year >= start_year & policy %in% copay_policy & 
            gender == client_gender & income_decile_17 %in% decile]
  if(donut_hole > 0) { 
    DT = DT[!(running_var %in% c(-donut_hole:(donut_hole-1)))] }
  
  # Aggregate:
  DT[, outcome := get(outcome)]
  DT = DT[, .(outcome = weighted.mean(outcome, w=population),
              population = sum(population)), by='running_var']
    
  # Use the chosen bandwidths:
  DT = DT[running_var %in% c(-results_row$bw_l :
                               (results_row$bw_r - 1))]
  
  # Plot:
  p = rdrobust::rdplot(
    y=DT$outcome, x=DT$running_var, weights = DT$population, 
    c=-0.5, p=order,
    kernel='tri', binselect = 'esmv',
    title = title,
    x.label = 'Days relative to the 18th birthday', 
    y.label = 'Ann. contacts per capita',
    col.dots = 'grey40', col.lines = 'black'
  )
  
  p = p$rdplot + theme(text=element_text(size=19))
  
  return(p)
  
}

rd.estimate_local_pol_plot(results_row=test,
  df=df.1, start_year = 2011, copay_policy="copayment", client_gender=2, 
  decile=c(0:3), order=1, donut_hole = 3, title='Title', outcome='gp_visits_ann'
)


### a function that estimates several models to assess robustness to the 
# specification choice  ###

rd.estimate_local_pol_spec = function(
  
  data, start_year=2011, copay_policy, client_gender, decile,
  donut_hole=3, bandwidth=NULL, otc) {
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
  # bandwidth: bandwidth as an integer. If NULL, use data.driven methods.
  # otc: either 'gp_visits_ann' or 'referrals_ann'
  # OUTPUTS:
  # a data.table containing regression results
  
  
  # List the models that we estimate:
  specs = list(list(title="Different means", order=0),
               list(title="Local linear", order=1),
               list(title="Local quadratic", order=2))
  
  # Loop over the models and estimate the effects:
  regs = lapply(specs, function(i) { 
    reg = rd.estimate_local_pol(
      data, start_year=start_year, copay_policy=copay_policy, 
      client_gender=client_gender, decile=decile,
      donut_hole=donut_hole, bw=bandwidth, order=i$order, outcome=otc
    ) 
    reg[, spec := i$title]
  })
  
  regs = as.data.table(do.call("rbind", regs))
  regs[, spec_no := c(1:nrow(regs))]
  return(regs)
}

test_spec = rd.estimate_local_pol_spec(
  df.1, copay_policy="copayment", client_gender=2, decile=c(0:3), 
  donut_hole = 3, otc='gp_visits_ann'
)
print(test_spec)


### a function that plots the results using several specifications ###

plot.spec = function(data) {
  # INPUTS:
  # data: data.table from rd.estimate_lm_spec()
  # OUTPUTS:
  # specification plot
  
  plot = ggplot(data, aes(x=spec_no, y=above)) +
    labs(x="Specification", y="") +
    geom_point(data=data[data$spec!="Local linear",], aes(x=spec_no, y=above),
               color='grey40') +
    geom_point(data=data[data$spec=="Local linear",], aes(x=spec_no, y=above), 
               shape=15, size=3) +
    geom_segment(data=data[data$spec!="Local linear",], 
                 aes(x=spec_no, y=conf_low, xend=spec_no, yend=conf_high),
                 color='grey40') +
    geom_segment(data=data[data$spec=="Local linear",], 
                 aes(x=spec_no, y=conf_low, xend=spec_no, yend=conf_high)) +
    scale_x_continuous(breaks=c(1:12)) +
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

plot.spec(test_spec)


### a function that estimates the same RD model but with several bandwidths. ###

rd.estimate_local_pol_bws = function(
  data, start_year=2011, copay_policy, client_gender, decile,
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
  # a data.table containing regression results
  
  
  # Extract relevant subset of data:
  data = data[year >= start_year & policy %in% copay_policy & 
                gender == client_gender & income_decile_17 %in% decile]
  if(donut_hole > 0) {
    data = data[!(running_var %in% c(-donut_hole:(donut_hole-1)))] }
  
  # Aggregate:
  data = data[, outcome := get(outcome)]
  data = data[, .(outcome = weighted.mean(outcome, w=population),
                  population = sum(population)), by='running_var']
  
  # Bandwidths:
  if(data[, min(running_var) < -700]) { vec = seq(80, 300, by=10) }
  if(data[, min(running_var) > -400]) { vec = seq(80, 200, by=10) }

  
  regs = lapply(vec, function(i) { 
    
    # Estimate:
    out = rdrobust::rdrobust(
      y=data$outcome, x=data$running_var, weights = data$population,
      c = -0.5, p = order, q = order + 1, 
      h = c(i,i), b = c(i,i), kernel = 'tri', vce = "nn"
    )
    
    
    # Collect the results to a tidy table:
    
    # Main estimate:
    est = out$coef['Conventional', 'Coeff']
    
    # Confidence intervals:
    conf_low = out$ci['Robust', 'CI Lower']
    conf_high = out$ci['Robust', 'CI Upper']
    
    table = data.table(
      
      "Variable" = c(
        "above", "conf_low", "conf_high", 'order', 'donut_hole', 'bw'),
      
      "Values" = c(
        est, conf_low, conf_high, order, donut_hole, i)
    )
    
    # From long to wide:
    table = data.table(t(table))
    setnames(table, as.character(table[1,]))
    table = table[-1,][, names(table) := lapply(.SD, as.numeric)]
    
    return(table)
    
  })
  
  regs = as.data.table(do.call("rbind",regs))
  return(regs)
}


test_bws = rd.estimate_local_pol_bws(
  df.1, copay_policy="copayment", client_gender=2, decile=c(0:3), 
  donut_hole = 3, outcome='gp_visits_ann'
)
print(test_bws)


### a function that plots the results using several bandwidths. ###

plot.bws = function(data) {
  # OUTPUTS:
  # a bandwidth plot
  
  
  plot = ggplot(data, aes(x=bw, y=above)) +
    labs(x="Bandwidth", y="") +
    geom_point(data=data[data$bw!=0,], aes(x=bw, y=above), color='grey40') +
    geom_point(data=data[data$bw==0,], 
               aes(x=bw, y=above), shape=15, size=3) +
    geom_segment(data=data[data$bw!=0,], aes(x=bw, y=conf_low, xend=bw, 
                                             yend=conf_high), color='grey40') +
    geom_segment(data=data[data$bw==0,], 
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

plot.bws(test_bws)


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

#list_placebos(c(-730:-1, 0:729), 150)


### A function that estimates placebo effects. ###

rd.estimate_local_pol_placebo = function(
  data, start_year=2011, copay_policy, client_gender, decile, order=1, 
  donut_hole=3, bw_choice, outcome) {
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
  # bw_choice: bandwidth choice as an integer if "donut" is chosen.
  # outcome: either 'gp_visits_ann' or 'referrals_ann'
  # OUTPUTS:
  # a data.table containing effect estimates
  
  
  # First, get a list of running_var subsets that we will loop over:
  running_var = sort(unique(data$running_var))
  placebo_lst = list_placebos(running_var, bw_choice)
  
  
  # Estimate the placebo effects:
  
  regs = lapply(placebo_lst, function(x) {
    

    # Extract relevant subset of data:
    df = data[year >= start_year & policy %in% copay_policy & 
                gender == client_gender & income_decile_17 %in% decile &
                running_var %in% x$bw_vals]
    
    # Aggregate:
    df[, outcome := get(outcome)]
    df = df[, .(outcome = weighted.mean(outcome, w=population),
                population = sum(population)), by='running_var']
    
    # Mutate the data:
    df = df[, running_var := running_var - x$cutoff]
    if(donut_hole > 0) { 
      df = df[!(running_var %in% c(-donut_hole:(donut_hole-1)))] }
    
    # Estimate:
    out = rdrobust::rdrobust(
      y=df$outcome, x=df$running_var, weights = df$population,
      c = -0.5, p = order, q = order + 1, vce = "nn",
      kernel = 'tri', h = c(bw_choice, bw_choice), b = c(bw_choice, bw_choice)
    )
    
    table = data.table(est = out$coef['Conventional', 'Coeff'], 
                       conf_low = out$ci['Robust', 'CI Lower'], 
                       conf_high = out$ci['Robust', 'CI Upper'],
                       cutoff = x$cutoff)
    
  })
  regs = as.data.table(do.call("rbind", regs))
  
  # Also estimate the base model:
  
  base = rd.estimate_local_pol(
    data, start_year = start_year, copay_policy = copay_policy, 
    client_gender = client_gender, decile = decile, order = order, 
    donut_hole = donut_hole, outcome = outcome
  )
  base[, cutoff := 0]
  setnames(base, old='above', new='est')
  regs = rbind(regs, base, fill=TRUE) 
  regs = regs[order(est)]
  regs[, cutoff_no := c(1:nrow(regs))]
  
  return(regs)
}

test_placebo = rd.estimate_local_pol_placebo(
  data=df.1, copay_policy="copayment", client_gender=2, decile=c(0:9), 
  bw_choice = 150, outcome = 'gp_visits_ann'
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
  
  plot = ggplot(data, aes(x=cutoff_no, y=est)) +
    labs(x="Placebo cutoffs", y="") +
    geom_point(data=data[data$cutoff!=0,], aes(x=cutoff_no, y=est),
               color='grey40') +
    geom_point(data=data[data$cutoff==0,], aes(x=cutoff_no, y=est), 
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


### a function that computes the results (regression tables and 
# bandwidth plots) ###

rdd_results = function(data, models_list, bw_int=NULL) {
  
  
  # Estimate the main results and plot RD result plots:
  results_main = lapply(models_list, function(x) {
    regs = rd.estimate_local_pol(
      data, start_year = x[[1]], copay_policy = x[[2]], client_gender = x[[3]], 
      decile = x[[4]], outcome = x[[6]], bw=bw_int
    )
  })
  
  # RD result plots:
  results_main_plots = lapply(seq_along(models_list), function(i) {
    regs = rd.estimate_local_pol_plot(
      results_row = results_main[[i]], df=data, 
      start_year = models_list[[i]][[1]], 
      copay_policy = models_list[[i]][[2]], 
      client_gender = models_list[[i]][[3]], 
      decile = models_list[[i]][[4]],
      title = models_list[[i]][[5]],
      outcome = models_list[[i]][[6]]
    )
  })
  names(results_main_plots) = names(models_list)
 
  # Estimate the main results WITHOUT using a 3-day donut hole:
  results_donut = lapply(models_list, function(x) {
    regs = rd.estimate_local_pol(
      data, start_year = x[[1]], copay_policy = x[[2]], client_gender = x[[3]], 
      decile = x[[4]], donut_hole = 0, outcome = x[[6]], bw=bw_int
    )
  })
  
  
  # Estimate the results using several bandwidths and save them to a table:
  results_bws = lapply(models_list, function(x) {
    regs = rd.estimate_local_pol_bws(
      data, start_year = x[[1]], copay_policy = x[[2]], client_gender = x[[3]], 
      decile = x[[4]], outcome = x[[6]]
    )
  })
  
  # Plot a bandwidth plot for each of the models:
  plots_bws = lapply(results_bws, function(x) {
    plot = plot.bws(x)
  })
  
  
  # Estimate the results using several specifications and save them to a table:
  results_spec = lapply(models_list, function(x) {
    regs = rd.estimate_local_pol_spec(
      data, start_year = x[[1]], copay_policy = x[[2]], client_gender=x[[3]], 
      decile = x[[4]], otc = x[[6]], bandwidth = bw_int
    )
  })
  
  # Plot the estimates from several specifications:
  plots_spec = lapply(results_spec, function(x) {
    plot = plot.spec(x)
  })
  
  # Estimate the results using several placebo cutoffs and save them to a table:
  results_placebo = lapply(models_list, function(x) {
    regs = rd.estimate_local_pol_placebo(
      data, start_year=x[[1]], copay_policy=x[[2]], 
      client_gender=x[[3]], decile=x[[4]], bw_choice = 150, outcome = x[[6]]
    )
  })
  
  # Plot the estimates from placebo cutoffs:
  plots_placebo = lapply(results_placebo, function(x) {
    plot = plot.placebo(x)
  })
  
  
  return(list(results_main=results_main, results_main_plots=results_main_plots,
              results_donut=results_donut,
              tables_bw = results_bws, plots_bw = plots_bws, 
              tables_spec=results_spec, plots_spec=plots_spec,
              tables_placebo=results_placebo, plots_placebo=plots_placebo))
  
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

results = rdd_results(df.1, models)


### Result tables. ###

rd.tables = function(data, donut, output) {
  # INPUTS:
  # data: results from rdd_results()
  # donut: 1: results WITHOUT a 3-day donut hole that is used in main analysis.
  # output: file path for the output
  # RD table 
  
  
  if(donut==1) {
    data = data$results_donut
  } else {
    data = data$results_main }
  
  results = list(data$women_copay_all,
                 data$women_copay_bot20,
                 data$women_copay_bot40,
                 data$women_copay_top50,
                 
                 data$women_freeexempt_all,
                 data$women_freeexempt_bot20,
                 data$women_freeexempt_bot40,
                 data$women_freeexempt_top50)
  
  
  tables = lapply(results, function(x) {
    
    part1 = format(round(c(x$mean, x$above, x$change_per, x$p), digits=3), 
                   nsmall=3)
    
    if(x$conf_low < 0) { sign.low = '-' } else { sign.low = '' }
    if(x$conf_high < 0) { sign.high = '-' } else { sign.high = ''}
    
    part2 = paste(
      '[', sign.low, format(abs(round(x$conf_low, digits=2)), nsmall=2), ', ', 
      sign.high, format(abs(round(x$conf_high, digits=2)), nsmall=2), ']', 
      sep='')
    
    part3 = as.character(x$individuals)
    part4 = paste('(', x$bw_l, ', ', x$bw_r, ')', sep='')
    
    table = data.table(
      'Variable' = c("Level", "RD estimate", "Change, %", "P-value", 
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
  
  save.table(table, output, label_tex = 'tab:rdd_table_women', 
             title_tex = 'RD Results for Women')
  
}

rd.tables(results, donut=0, output = output_rd1_table)


### RD result plots. ###

cairo_pdf(filename = output_rd1_plot, width = 16.0, height = 8.0)
print(
  (results$results_main_plots$women_copay_all + ggtitle("Copay: All")) +
    (results$results_main_plots$women_copay_bot20 + ggtitle("Copay: Bottom 20%")) +
    (results$results_main_plots$women_copay_bot40 + ggtitle("Copay: Bottom 40%")) +
    (results$results_main_plots$women_copay_top50 + ggtitle("Copay: Top 50%")) +
    (results$results_main_plots$women_freeexempt_all + ggtitle("Exempt: All")) +
    (results$results_main_plots$women_freeexempt_bot20 + ggtitle("Exempt: Bottom 20%")) +
    (results$results_main_plots$women_freeexempt_bot40 + ggtitle("Exempt: Bottom 40%")) +
    (results$results_main_plots$women_freeexempt_top50 + ggtitle("Exempt: Top 50%")) +
    plot_layout(ncol=4))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) RD models - robustness checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

### Sensitivity plots (to different bandwidths) ###

cairo_pdf(filename = output_rd1_bw, width = 16.0, height = 8.0)
print(
  (results$plots_bw$women_copay_all + ggtitle("Copay: All")) +
    (results$plots_bw$women_copay_bot20 + ggtitle("Copay: Bottom 20%")) +
    (results$plots_bw$women_copay_bot40 + ggtitle("Copay: Bottom 40%")) +
    (results$plots_bw$women_copay_top50 + ggtitle("Copay: Top 50%")) +
    (results$plots_bw$women_freeexempt_all + ggtitle("Exempt: All")) +
    (results$plots_bw$women_freeexempt_bot20 + ggtitle("Exempt: Bottom 20%")) +
    (results$plots_bw$women_freeexempt_bot40 + ggtitle("Exempt: Bottom 40%")) +
    (results$plots_bw$women_freeexempt_top50 + ggtitle("Exempt: Top 50%")) +
    plot_layout(ncol=4))
dev.off()

### Sensitivity plots (to different specifications) ###

cairo_pdf(filename = output_rd1_spec, width = 16.0, height = 8.0)
print(
  (results$plots_spec$women_copay_all + ggtitle("Copay: All.")) +
    (results$plots_spec$women_copay_bot20 + ggtitle("Copay: Bottom 20%")) +
    (results$plots_spec$women_copay_bot40 + ggtitle("Copay: Bottom 40%")) +
    (results$plots_spec$women_copay_top50 + ggtitle("Copay: Top 50%")) +
    (results$plots_spec$women_freeexempt_all + ggtitle("Exempt: All")) +
    (results$plots_spec$women_freeexempt_bot20 + ggtitle("Exempt: Bottom 20%")) +
    (results$plots_spec$women_freeexempt_bot40 + ggtitle("Exempt: Bottom 40%")) +
    (results$plots_spec$women_freeexempt_top50 + ggtitle("Exempt: Top 50%")) +
    plot_layout(ncol=4))
dev.off()

### Placebo cutoffs ###

cairo_pdf(filename = output_rd1_placebo, width = 16.0, height = 8.0)
print(
  (results$plots_placebo$women_copay_all + ggtitle("Copay: All.")) +
    (results$plots_placebo$women_copay_bot20 + ggtitle("Copay: Bottom 20%")) +
    (results$plots_placebo$women_copay_bot40 + ggtitle("Copay: Bottom 40%")) +
    (results$plots_placebo$women_copay_top50 + ggtitle("Copay: Top 50%")) +
    (results$plots_placebo$women_freeexempt_all + ggtitle("Exempt: All")) +
    (results$plots_placebo$women_freeexempt_bot20 + ggtitle("Exempt: Bottom 20%")) +
    (results$plots_placebo$women_freeexempt_bot40 + ggtitle("Exempt: Bottom 40%")) +
    (results$plots_placebo$women_freeexempt_top50 + ggtitle("Exempt: Top 50%")) +
    plot_layout(ncol=4))
dev.off()


# Show the main results WITHOUT a 3-day donut hole:
rd.tables(results, donut=1, output = output_rd1_table_donut)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Effects on referrals. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# List all the models that will be estimated:
models = mods(start_year=2011, outcome='referrals_ann')

# Estimate and save:
results.refs = rdd_results(df.1, models)
rd.tables(results.refs, donut=0, output = output_rd1_table_refs)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Effects using alternative samples. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# List all the models that will be estimated:
models = mods(start_year=2011, outcome='gp_visits_ann')

# First, estimate the results with a fixed 180-day bandwidth:
results.alt.1 = rdd_results(df.2, models[1:8], bw_int = 180)
results.alt.2 = rdd_results(df.3, models[1:8], bw_int = 180)

# Save tables:
rd.tables(results.alt.1, donut=0, output = output_rd2_table)
rd.tables(results.alt.2, donut=0, output = output_rd3_table)

### Sensitivity plots (to different bandwidths) ###

cairo_pdf(filename = output_rd2_bw, width = 16.0, height = 8.0)
print(
  (results.alt.1$plots_bw$women_copay_all + ggtitle("Copay: All.")) +
    (results.alt.1$plots_bw$women_copay_bot20 + ggtitle("Copay: Bottom 20%")) +
    (results.alt.1$plots_bw$women_copay_bot40 + ggtitle("Copay: Bottom 40%")) +
    (results.alt.1$plots_bw$women_copay_top50 + ggtitle("Copay: Top 50%")) +
    (results.alt.1$plots_bw$women_freeexempt_all + ggtitle("Exempt: All")) +
    (results.alt.1$plots_bw$women_freeexempt_bot20 + ggtitle("Exempt: Bottom 20%")) +
    (results.alt.1$plots_bw$women_freeexempt_bot40 + ggtitle("Exempt: Bottom 40%")) +
    (results.alt.1$plots_bw$women_freeexempt_top50 + ggtitle("Exempt: Top 50%")) +
    plot_layout(ncol=4))
dev.off()

cairo_pdf(filename = output_rd3_bw, width = 16.0, height = 8.0)
print(
  (results.alt.2$plots_bw$women_copay_all + ggtitle("Copay: All.")) +
    (results.alt.2$plots_bw$women_copay_bot20 + ggtitle("Copay: Bottom 20%")) +
    (results.alt.2$plots_bw$women_copay_bot40 + ggtitle("Copay: Bottom 40%")) +
    (results.alt.2$plots_bw$women_copay_top50 + ggtitle("Copay: Top 50%")) +
    (results.alt.2$plots_bw$women_freeexempt_all + ggtitle("Exempt: All")) +
    (results.alt.2$plots_bw$women_freeexempt_bot20 + ggtitle("Exempt: Bottom 20%")) +
    (results.alt.2$plots_bw$women_freeexempt_bot40 + ggtitle("Exempt: Bottom 40%")) +
    (results.alt.2$plots_bw$women_freeexempt_top50 + ggtitle("Exempt: Top 50%")) +
    plot_layout(ncol=4))
dev.off()


# Next, estimate the results with a data-driven bandwidth choice:
results.alt.1 = rdd_results(df.2, models[1:8], bw_int = NULL)
results.alt.2 = rdd_results(df.3, models[1:8], bw_int = NULL)

# Save tables:
rd.tables(results.alt.1, donut=0, output = output_rd2_table_rdrob)
rd.tables(results.alt.2, donut=0, output = output_rd3_table_rdrob)

# End.
