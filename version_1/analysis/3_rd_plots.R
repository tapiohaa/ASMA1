
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###              r-script 3_rd_plots.R            ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Plot RD plots.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(rdrobust)         # RD plots.
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure. 

# Inputs:
input.1 = "W:/ASMA1/data/cleaned/data1_local_pol.rds"
input.2 = "W:/ASMA1/data/cleaned/data2_local_pol.rds"
input.3 = "W:/ASMA1/data/cleaned/data3_local_pol.rds"
input_param = "W:/ASMA1/data/cleaned/data1_parametric.rds"

# Outputs:
output_population = "W:/ASMA1/analysis/figures/rd_plot_population.pdf"
output_rd1_raw = "W:/ASMA1/analysis/figures/rd1_plot_raw.pdf"
output_rd1_plot_health_checks = 
  "W:/ASMA1/analysis/figures/rd1_plot_health_checks.pdf"
output_rd1_plot = "W:/ASMA1/analysis/figures/rd1_plot.pdf"
output_rd1_plot_men = "W:/ASMA1/analysis/figures/rd1_plot_men.pdf"
output_rd1_plot_hki = "W:/ASMA1/analysis/figures/rd1_plot_hki.pdf"
output_rd1_plot_refs = "W:/ASMA1/analysis/figures/rd1_plot_refs.pdf"
output_rd2_plot = "W:/ASMA1/analysis/figures/rd2_plot.pdf"
output_rd3_plot = "W:/ASMA1/analysis/figures/rd3_plot.pdf"
output_rd_plot_assistance = 
  "W:/ASMA1/analysis/figures/rd_plot_assistance.pdf"

###
###


# Read datasets:
df.1 = readRDS(input.1)
df.2 = readRDS(input.2)
df.3 = readRDS(input.3)
df.param = readRDS(input_param)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Plot the population size of age cells. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# The population size by area:

plots.1 = lapply(list(df.1, df.2, df.3), function(data) {
  
  DT = data[, mget(colnames(data))]
  
  DT[policy=='free', policy := 'exemptions']
  
  DT = DT[gender==2, .(population=sum(population)), 
          by=c('policy', 'running_var')
          ][, population := population / 1000]
  
  # Plot:
  p = ggplot(DT, aes(x=running_var, y=population, 
                       group=policy, linetype=policy)) +
    geom_line(size=1) +
    xlab('Days to the 18th birthday') +
    ylab('Population size (thousands)') +
    scale_linetype_discrete(name='Policy') + 
    ylim(0, 80) + 
    theme(text = element_text(size=19), 
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = "bottom") +
    geom_vline(xintercept = -0.5, linetype='dashed')
  
})


# The population size by income groups.

plots.2 = lapply(list(df.1, df.2, df.3), function(data) {
  
  DT = data[gender==2 & income_decile_17 %in% c(0:3, 5:9)]
  DT[income_decile_17 %in% c(0:3), Group := 'bottom 40%']
  DT[income_decile_17 %in% c(5:9), Group := 'top 50%'] 
  
  DT = DT[, .(population = sum(population)), by=c('Group', 'running_var')
          ][, population := population / 1000]
  
  p = ggplot(DT, aes(x=running_var, y=population, 
                     group=Group, linetype=Group)) +
    geom_line(size=1) +
    xlab('Days to the 18th birthday') +
    ylab('Population size (thousands)') +
    scale_linetype_discrete(name='Income Group') + 
    ylim(0, 60) + 
    theme(text = element_text(size=19), 
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = "bottom") +
    geom_vline(xintercept = -0.5, linetype='dashed')
  
})


# Save:

cairo_pdf(filename = output_population, width = 15.0, height = 12.0)
print(
  wrap_elements(
    plot=plots.1[[1]] + ggtitle('Main: by Policy') + 
      plots.1[[2]] + ggtitle('Alternative 1: by Policy') + 
      plots.1[[3]] + ggtitle('Alternative 2: by Policy') + 
      plot_layout(guides = 'collect') & theme(legend.position = 'bottom')) + 
    wrap_elements(
      plot=plots.2[[1]] + ggtitle('Main: by Income') + 
        plots.2[[2]] + ggtitle('Alternative 1: by Income') +
        plots.2[[3]] + ggtitle('Alternative 2: by Income') +
        plot_layout(guides = 'collect') & theme(legend.position = 'bottom')) + 
    plot_layout(nrow=2))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Plot the raw data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that extracts a subset of analysis data and then aggregates. ###

subset_data = function(data, client_decile=c(0:9), municipal_policy,
                       outcome) {
  # INPUTS:
  # data: analysis data, analysis_data_local_pol.rds
  # client_decile: equivalized disposable income, from 0 to 9
  # municipal_policy: 'exemptions' for exemption municipalities (and Helsinki), 
  #                   and 'copayment' if such was charged.
  # outcome: either 'gp_visits_ann' or 'referrals_ann'
  # OUTPUTS:
  # a subset of the analysis data
  
  df = data[gender == 2 & income_decile_17 %in% client_decile & 
              policy %in% municipal_policy
            ][, .(otc = weighted.mean(get(outcome), w=population),
                  population = sum(population)), 
              by='running_var']
  
  return(df)
}

test = subset_data(df.1, client_decile = c(0:1), municipal_policy = 'copayment',
                   outcome='gp_visits_ann')


# Models to be plotted:

models = list(
  # Women: GP visits:
  list(income_decile = c(0:9), policy = 'copayment', title = 'Copay: All', outcome='gp_visits_ann'),
  list(income_decile = c(0:1), policy = 'copayment', title = 'Copay: Bottom 20%', outcome='gp_visits_ann'),
  list(income_decile = c(0:3), policy = 'copayment', title = 'Copay: Bottom 40%', outcome='gp_visits_ann'),
  list(income_decile = c(5:9), policy = 'copayment', title = 'Copay: Top 50%', outcome='gp_visits_ann'),
  list(income_decile = c(0:9), policy = c('exemptions', 'free'), title = 'Exempt: All', outcome='gp_visits_ann'),
  list(income_decile = c(0:1), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 20%', outcome='gp_visits_ann'),
  list(income_decile = c(0:3), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 40%', outcome='gp_visits_ann'),
  list(income_decile = c(5:9), policy = c('exemptions', 'free'), title = 'Exempt: Top 50%', outcome='gp_visits_ann')
)


# Loop over models to plot RD plots:

rd_plots = lapply(models, function(mod) {
  
  # Extract the right data:
  data = subset_data(df.1, client_decile = mod$income_decile, 
                     municipal_policy = mod$policy, outcome = mod$outcome)
  data = data[running_var %in% c(-30:29)]
  
  # Plot:
  
  p = ggplot(data, aes(x=running_var, y=otc)) +
    geom_point(color='grey40') +
    geom_point(data=data[running_var %in% c(-3:2)],
               mapping = aes(x=running_var, y=otc), 
               color = 'black', size=3, shape=15) +
    ggtitle(mod$title) +
    xlab('Days to the 18th birthday') +
    ylab('Ann. contacts per capita') +
    geom_vline(xintercept = -0.5) +
    theme(text = element_text(size=19),     
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = "bottom")
  
})


# Save plots:

cairo_pdf(filename = output_rd1_raw, width = 16.0, height = 9)
print( patchwork::wrap_plots(rd_plots, nrow = 2, byrow = TRUE))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) RD plots. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Note: We use a 3-day donut hole:


### A function that extracts a subset of analysis data and then aggregates. ###

subset_data = function(data, client_gender, client_decile=c(0:9), 
                       municipal_policy, outcome) {
  # INPUTS:
  # data: analysis data, analysis_data_local_pol.rds
  # client_gender: 1=men, 2=women
  # client_decile: equivalized disposable income, from 0 to 9
  # municipal_policy: 'exemptions' for exemption municipalities (and Helsinki), 
  #                   and 'copayment' if such was charged.
  # outcome: either 'gp_visits_ann' or 'health_checks_ann'
  # OUTPUTS:
  # a subset of the analysis data
  
  df = data[gender == client_gender & income_decile_17 %in% client_decile & 
              policy %in% municipal_policy
            ][, outcome := mget(outcome)
              ][, .(outcome = weighted.mean(outcome, w=population, na.rm=TRUE),
                    population = sum(population)), 
              by='running_var']
  
  return(df)
}

test = subset_data(df.1, client_gender = 2, client_decile = c(0:1),
                   municipal_policy = 'copayment',
                   outcome = 'gp_visits_ann')


# Models to be plotted:

models = list(
  # Health checks:
  list(gender = 2, income_decile = c(0:9), policy = c('copayment', 'exemptions', 'free'), title = 'Women', outcome = 'health_checks_ann'),
  list(gender = 1, income_decile = c(0:9), policy = c('copayment', 'exemptions', 'free'), title = 'Men', outcome = 'health_checks_ann'),
  # Women: GP visits:
  list(gender = 2, income_decile = c(0:9), policy = 'copayment', title = 'Copay: All', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:1), policy = 'copayment', title = 'Copay: Bottom 20%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:3), policy = 'copayment', title = 'Copay: Bottom 40%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(5:9), policy = 'copayment', title = 'Copay: Top 50%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:9), policy = c('exemptions', 'free'), title = 'Exempt: All', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:1), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 20%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:3), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 40%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(5:9), policy = c('exemptions', 'free'), title = 'Exempt: Top 50%', outcome = 'gp_visits_ann'),
  # Men: GP visits:
  list(gender = 1, income_decile = c(0:9), policy = 'copayment', title = 'Copay: All', outcome = 'gp_visits_ann'),
  list(gender = 1, income_decile = c(0:1), policy = 'copayment', title = 'Copay: Bottom 20%', outcome = 'gp_visits_ann'),
  list(gender = 1, income_decile = c(0:3), policy = 'copayment', title = 'Copay: Bottom 40%', outcome = 'gp_visits_ann'),
  list(gender = 1, income_decile = c(5:9), policy = 'copayment', title = 'Copay: Top 50%', outcome = 'gp_visits_ann'),
  list(gender = 1, income_decile = c(0:9), policy = c('exemptions', 'free'), title = 'Exempt: All', outcome = 'gp_visits_ann'),
  list(gender = 1, income_decile = c(0:1), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 20%', outcome = 'gp_visits_ann'),
  list(gender = 1, income_decile = c(0:3), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 40%', outcome = 'gp_visits_ann'),
  list(gender = 1, income_decile = c(5:9), policy = c('exemptions', 'free'), title = 'Exempt: Top 50%', outcome = 'gp_visits_ann'),
  # Women: GP visits in Helsinki:
  list(gender = 2, income_decile = c(0:9), policy = 'free', title = 'Helsinki: All', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:1), policy = 'free', title = 'Helsinki: Bottom 20%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:3), policy = 'free', title = 'Helsinki: Bottom 40%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(5:9), policy = 'free', title = 'Helsinki: Top 50%', outcome = 'gp_visits_ann'),
  # Women: referrals:
  list(gender = 2, income_decile = c(0:9), policy = 'copayment', title = 'Copay: All', outcome = 'referrals_ann'),
  list(gender = 2, income_decile = c(0:1), policy = 'copayment', title = 'Copay: Bottom 20%', outcome = 'referrals_ann'),
  list(gender = 2, income_decile = c(0:3), policy = 'copayment', title = 'Copay: Bottom 40%', outcome = 'referrals_ann'),
  list(gender = 2, income_decile = c(5:9), policy = 'copayment', title = 'Copay: Top 50%', outcome = 'referrals_ann'),
  list(gender = 2, income_decile = c(0:9), policy = c('exemptions', 'free'), title = 'Exempt: All', outcome = 'referrals_ann'),
  list(gender = 2, income_decile = c(0:1), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 20%', outcome = 'referrals_ann'),
  list(gender = 2, income_decile = c(0:3), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 40%', outcome = 'referrals_ann'),
  list(gender = 2, income_decile = c(5:9), policy = c('exemptions', 'free'), title = 'Exempt: Top 50%', outcome = 'referrals_ann')
)


# Loop over models to plot RD plots:

rd_plots = lapply(models, function(mod) {
  
  # Extract the right data:
  data = subset_data(
    df.1, client_gender = mod$gender, client_decile = mod$income_decile,
    municipal_policy = mod$policy, outcome=mod$outcome
  )
  data = data[!(running_var %in% c(-3:2))]
  
  # Plot:
  
  p = rdrobust::rdplot(
    y=data$outcome, x=data$running_var, c=-0.5, p=4, 
    kernel='uni', binselect = 'esmv', weights = data$population,
    title=mod$title, 
    x.label = 'Days to the 18th birthday', 
    y.label = 'Ann. contacts per capita',
    col.dots = 'grey40', col.lines = 'black'
  )
  p = p$rdplot + theme(text = element_text(size=19)) +
    scale_x_continuous(breaks = c(-600, -300, 0, 300, 600))
  
  return(p)

})


# Save plots:

# Health checks:
cairo_pdf(filename = output_rd1_plot_health_checks, width = 10.0, height = 5.0)
print( patchwork::wrap_plots(rd_plots[1:2], nrow = 1, byrow = TRUE))
dev.off()

# Women: GP visits:
cairo_pdf(filename = output_rd1_plot, width = 16.0, height = 9)
print( patchwork::wrap_plots(rd_plots[3:10], nrow = 2, byrow = TRUE))
dev.off()

# Men: GP visits:
cairo_pdf(filename = output_rd1_plot_men, width = 16.0, height = 9)
print( patchwork::wrap_plots(rd_plots[11:18], nrow = 2, byrow = TRUE))
dev.off()

# Women: GP visits in Helsinki:
cairo_pdf(filename = output_rd1_plot_hki, width = 16.0, height = 4.5)
print( patchwork::wrap_plots(rd_plots[19:22], nrow = 1, byrow = TRUE))
dev.off()

# Women: referrals:
cairo_pdf(filename = output_rd1_plot_refs, width = 16.0, height = 9)
print( patchwork::wrap_plots(rd_plots[23:30], nrow = 2, byrow = TRUE))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) RD plots with the alternative samples. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

models = list(
  # Women: GP visits:
  list(gender = 2, income_decile = c(0:9), policy = 'copayment', title = 'Copay: All', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:1), policy = 'copayment', title = 'Copay: Bottom 20%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:3), policy = 'copayment', title = 'Copay: Bottom 40%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(5:9), policy = 'copayment', title = 'Copay: Top 50%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:9), policy = c('exemptions', 'free'), title = 'Exempt: All', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:1), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 20%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(0:3), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 40%', outcome = 'gp_visits_ann'),
  list(gender = 2, income_decile = c(5:9), policy = c('exemptions', 'free'), title = 'Exempt: Top 50%', outcome = 'gp_visits_ann')
)

# Loop over datasets to plot RD plots:
rd_plots_alt = lapply(list(df.2, df.3), function(df.alt) {
  
  
  # Loop over models:
  rd_plots = lapply(models, function(mod) {
    
    # Extract the right data:
    data = subset_data(
      df.alt, client_gender = mod$gender, client_decile = mod$income_decile,
      municipal_policy = mod$policy, outcome=mod$outcome
    )
    data = data[!(running_var %in% c(-3:2))]
    
    # Plot:
    
    p = rdrobust::rdplot(
      y=data$outcome, x=data$running_var, c=-0.5, p=4, 
      kernel='uni', binselect = 'esmv', weights = data$population,
      title=mod$title, 
      x.label = 'Days to the 18th birthday', 
      y.label = 'Ann. contacts per capita',
      col.dots = 'grey40', col.lines = 'black'
    )
    p = p$rdplot + theme(text = element_text(size=19)) +
      scale_x_continuous(breaks = c(-200, 0, 200))
    
  })
  
  
})


# Save plots:

# Women: GP visits: Alternative 1:
cairo_pdf(filename = output_rd2_plot, width = 16.0, height = 9)
print( patchwork::wrap_plots(rd_plots_alt[[1]], nrow = 2, byrow = TRUE))
dev.off()

# Women: GP visits: Alternative 2:
cairo_pdf(filename = output_rd3_plot, width = 16.0, height = 9)
print( patchwork::wrap_plots(rd_plots_alt[[2]], nrow = 2, byrow = TRUE))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) RD plots on social assistance use. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Make minor changes to the function that subsets the data:

subset_data_param = function(data, client_gender, client_decile=c(0:9), 
                             municipal_policy, outcome) {
  # INPUTS:
  # data: analysis data, analysis_data_local_pol.rds
  # client_gender: 1=men, 2=women
  # client_decile: equivalized disposable income, from 0 to 9
  # municipal_policy: 'exemptions' for exemption municipalities (and Helsinki), 
  #                   and 'copayment' if such was charged.
  # outcome: outcome as character
  # OUTPUTS:
  # a subset of the analysis data
  
  df = data[gender == client_gender & income_decile_17 %in% client_decile & 
              policy %in% municipal_policy
            ][, outcome := get(outcome)
              ][, .(outcome = mean(outcome, na.rm=TRUE),
                    population = .N), by='running_var']
  
  return(df)
}


# Models to be plotted:

models = list(
  # Women: social assistance:
  list(gender = 2, income_decile = c(0:3), policy = 'copayment', title = 'Copay: Bottom 40%'),
  list(gender = 2, income_decile = c(0:3), policy = c('exemptions', 'free'), title = 'Exempt: Bottom 40%')
)


# Loop over models to plot RD plots:

rd_plots_sa = lapply(models, function(mod) {
  
  # Extract the right data:
  data = subset_data_param(
    df.param, client_gender = mod$gender, client_decile = mod$income_decile, 
    municipal_policy = mod$policy, outcome = 'social_assistance')
  
  # Plot:
  
  p = rdrobust::rdplot(
    y=data$outcome, x=data$running_var, c=0, p=4, 
    kernel='uni', nbins = c(24, 24), weights = data$population,
    title= mod$title, 
    x.label = 'Months to the 18th birthday', 
    y.label = 'Probability of social assistance',
    col.dots = 'grey40', col.lines = 'black', y.lim = c(0.05, 0.30)
  )
  p = p$rdplot + theme(text = element_text(size=19))
  
  return(p)
  
})


# Women: Probability of receiving social assistance:
cairo_pdf(filename = output_rd_plot_assistance, width = 10.0, height = 5.0)
print( patchwork::wrap_plots(rd_plots_sa, ncol = 2))
dev.off()

# End.
