
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 3_rddid_local_rand.R       ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate RD-DID models with the local randomization approach.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(lfe)              # linear fixed effects estimation.
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
output_rddid_table = "W:/ASMA1/analysis/tables/rddid_locrand_table"
output_rddid_bw = "W:/ASMA1/analysis/figures/rddid_locrand_bw.pdf"


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
#### 1) RD-DID models - functions for estimation and plotting. ####
### ### ### ### ### ### ### ### ### ### ### ### ### 


### a function that estimates an RD-DID model ###

rddid.estimate_locrand = function(
  df, start_year=2011, copay_policy=c('copayment', 'exemptions', 'free'), 
  client_gender, decile, bandwidth, dh=3, outcome) {
  # INPUTS:
  # start_year: this year and later years are included
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
  
  # Create indicators for the regression model:
  data[, ':=' (post = as.integer(post_d == 1),
               copay = as.integer(policy == 'copayment'))]
  
  # Estimate the model:
  reg = lfe::felm(outcome ~ copay + post + copay:post, data=data)
  reg = summary(reg, robust=TRUE)$coefficients

  
  est = reg['copay:post', 'Estimate']
  se = reg['copay:post', 'Robust s.e']
  p = reg['copay:post', 'Pr(>|t|)']
  pre_mean = reg['(Intercept)', 'Estimate'] + reg['copay', 'Estimate']
  
  individuals = data[post==0, length(unique(id_no))]
  
  # Collect results to a table:
  table = data.frame(
    "Variable" = c('pre_mean', "above", "change_per", "std_error", "conf_low",
                   "conf_high", "p_value", "n", "bw", 'donut_hole'),
    "Values" = c(pre_mean, est, 100 * est / pre_mean, se, 
                 est - qnorm(0.05/2,lower=F) * se,
                 est + qnorm(0.05/2,lower=F) * se,
                 p, individuals, bandwidth, dh)) 
  
  # Mutate the table:
  table = transpose(table)
  colnames(table) = unlist(table[1, ])
  table = table[2, ]
  
  table = lapply(table, as.numeric)
  table = do.call(cbind.data.frame, table)
  
  return(table)
  
}

test = rddid.estimate_locrand(
  df.1, start_year=2011, client_gender=2, decile=c(0:9), bandwidth=30, dh=3, 
  outcome='gp_visits_ann')
print(test)


### a function that estimates the same RD-DID model but with several bandwidths
# and without the 3-day donut hole, collecting results to a table ###

rddid.estimate_locrand_bws_donuts = function(
  data, start_year=2011, copay_policy=c('copayment', 'exemptions', 'free'), 
  client_gender, decile, otc) {
  # INPUTS:
  # start_year: this year and later years are included
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
      rddid.estimate_locrand(
        data, start_year=start_year, copay_policy = copay_policy, 
        client_gender=client_gender, decile=decile, dh = donut,
        bandwidth=bws[[i]], outcome=otc
      ) 
    })
    regs = as.data.table(do.call("rbind",regs))
    
  })
  results = as.data.table(do.call("rbind",results))
  
}

test = rddid.estimate_locrand_bws_donuts(
  df.1, client_gender=2, decile=c(0:9), otc='gp_visits_ann'
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
    ylim(-0.20, 0.10) + 
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

rddid_results = function(data, models_list, bw, 
                         policies=c('copayment', 'exemptions', 'free')) {
  
  # Estimate the results using several bandwidths and save them to a table:
  results_bws = lapply(models_list, function(x) {
    regs = rddid.estimate_locrand_bws_donuts(
      data, start_year = 2011, copay_policy = policies, 
      client_gender = 2, decile = x[[2]], otc = 'gp_visits_ann'
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
models = list(list("All",c(0:9)), 
              list("Bottom 20%",c(0:1)),
              list("Bottom 40%",c(0:3)),
              list("Top 50%",c(5:9)))

# Estimate:
results = rddid_results(df.1, models, bw=30)
results.alt.1 = rddid_results(df.2, models, bw=30)
results.alt.2 = rddid_results(df.3, models, bw=30)


### RD-DID results tables ###

rddid.table = function(results, output_policy) {
  
  tables = lapply(results, function(res) {
    
    table = lapply(res$tables_bw, function(x) { 
      
      x = x[bw==30 & donut_hole==3]
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

rddid.table(list(results, results.alt.1, results.alt.2), 
            output_rddid_table)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Robustness checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

### Sensitivity plots (to different bandwidths) ###

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
    plot_layout(ncol=4, guides = 'collect') & theme(legend.position = 'bottom'))
dev.off()

# End.
