
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###          r-script 3_rd_balance_tests.R        ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Examine whether there are observable discontinuities in covariates.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(rdrobust)         # RD plots.
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure. 

# Inputs:
input_quality = "W:/ASMA1/data/intermediate/visits_changes.rds"
input_policies = "W:/ASMA1/data/raw/rdd_policies.csv"
input_ids = "W:/ASMA1/data/intermediate/visits_quality_1" # visits_quality_201X.csv, X in 1:9
input_folk = "W:/ASMA1/data/intermediate/folk_data_201" # folk_data_201X.csv, X in 1:9

# Outputs:
output_data = "W:/ASMA1/data/cleaned/data_balance_tests.rds" 
output_rd_plot_balance = "W:/ASMA1/analysis/figures/rd_plot_balance.pdf"
output_rd_plot_balance_730 = "W:/ASMA1/analysis/figures/rd_plot_balance_730.pdf"

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read some data into memory. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Read data on municipality-year (or municipality-month pairs for referrals) 
# pairs that will be dropped from analyses due to quality issues 
# (see 1_quality_checks.R).
changes = readRDS(input_quality)

# The study years are 2011-2019:
years = c(1:9)

# Read data on municipal copayment policies:
policies = data.table::fread(input_policies, select=c('no','policy2020'))
policies = policies[policy2020 != 'remove']
setnames(policies, old='policy2020', new='policy')

# The following 23 municipalities are our sample:
sample_munies = unique(policies$no)
print(sample_munies)


# A function that calculates age in years:

age_in_years = function(from, to) {
  # INPUTS:
  # from: birthday as a date
  # to: observation date as a date
  # OUTPUTS:
  # age in years as an integer
  
  
  from_year = as.integer(format(from, "%Y"))
  from_month = as.integer(format(from, "%m"))
  from_day = as.integer(format(from, "%d"))
  to_year = as.integer(format(to, "%Y"))
  to_month = as.integer(format(to, "%m"))
  to_day = as.integer(format(to, "%d"))
  
  age = to_year - from_year
  
  ifelse(to_month < from_month |
           (to_month == from_month & to_day < from_day),
         age-1, age)
  
}
age_in_years(from = as.Date("1999-10-01"), to = as.Date("2015-01-01"))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) The set of individuals. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

### Take all individuals ever observed in the data 
# (= who had a curative primary care GP visit in 2011-2019). ###

person_ids = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read AvoHilmo data:
  source = paste(input_ids, year, ".csv", sep="")
  visits = data.table::fread(
    source, select=c('id', 'birth_month', 'birth_year', 'birth_day')
  )
  
  # Take unique persons:
  visits = unique(visits)
  
  # Derive birth date:
  visits = visits[, ':=' (birth_date = as.Date(paste(as.character(birth_year), 
                                                     as.character(birth_month),
                                                     as.character(birth_day), 
                                                     sep="-")),
                          birth_year = NULL, birth_month = NULL, 
                          birth_day = NULL)]
  
  
  # To preserve memory, we take only those aged 30 or below at the end of 2019:
  visits = visits[, age_12_2019 := age_in_years(from=birth_date, 
                                                to=as.Date('2019-12-31'))]
  visits = visits[age_12_2019 <= 30][, age_12_2019 := NULL]
  
  return(visits)
  
})
person_ids = do.call(rbind.data.frame, person_ids)

# Keep only unique rows:
person_ids = unique(person_ids)

# There are about a dozen persons who "have" two birthdates in the data.
# Examine the issue:
doubles = person_ids[, .N, by='id']
doubles = merge(person_ids, doubles, by='id', all.x = TRUE)
print(length(doubles[N>1, unique(id)]))

# Drop the persons who have multiple birthdays in the data:
person_ids = doubles[N==1][, N := NULL]
rm(doubles)

# Create variables for the birth year and 18th birthday
person_ids[, byear := as.integer(format(birth_date, "%Y"))]
person_ids[, bday_18th := as.Date(paste(as.character(byear + 18), 
                                        format(birth_date, "%m"),
                                        format(birth_date, "%d"), sep='-'))] 

# Take care of those born on the leap day:
person_ids = 
  person_ids[format(birth_date, "%m")=='02' & format(birth_date, "%d")=='29',
             bday_18th := as.Date(paste(as.character(byear + 18), 
                                        '03', '01', sep='-'))]

# Create variables denoting the year and month of the 18th birthday:
person_ids[, ':=' (bday_18_year = byear + 18,
                   bday_18_month = as.integer(format(bday_18th, '%m')))]


### Expand the data to include each year in 2011-2019 for
#   each observed individual. ###

person_yr = CJ(person_ids$id, 2010 + years)
setnames(person_yr, old = c('V1','V2'), new = c('id', 'year'))


### Merge birthdays and constrain the size by using the difference
# of calendar year and birth year. ###

# Merge birthdays to the panel:
person_yr = merge(person_yr, person_ids, by='id', all.x=TRUE)
print("Merging birthdates to the panel is done")

# Filter and drop some variables:
person_yr = person_yr[year - byear <= 20 & year - byear >= 15
                      ][, byear := NULL]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) FOLK data.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###

### Read FOLK data. ###

folk = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read FOLK data:
  source_folk = paste(input_folk, year, ".csv", sep="")
  folk = data.table::fread(source_folk)
  
  # Percentage of rows where the family equivalized income is exactly zero:
  print(100 * nrow(folk[income_eq==0]) / nrow(folk))
  
  # We drop individuals with family equivalized income exactly zero (missing).
  folk = folk[income_eq > 0]
  
  # Create variable income_decile (by year):
  folk[, income_decile := cut(income_eq,
                              breaks = quantile(income_eq, probs = 0:10/10),
                              labels = 0:9, right = FALSE)]
  
  folk = folk[!is.na(income_decile)]
  
  return(folk)
  
})
folk = do.call(rbind.data.frame, folk)

# Keep the following columns:
cols = c('year', 'id', 'gender', 'municipality', 'activity', 'household_size', 
         'family_relationship', 'income_eq', 'income_decile')
folk = folk[, mget(cols)]


### Merge FOLK data to the ID-year panel and keep only observations when 
# the person lives in one of the 23 sample municipalities. ###

# Keep only sample municipalities in the FOLK data:
folk = folk[municipality %in% sample_munies]

# Merge:
person_yr = merge(person_yr, folk, by=c('id', 'year'), all.x = TRUE)

# Release some memory:
rm(folk, person_ids)
gc()

# Keep only women:
person_yr = person_yr[gender==2]

# Add date (31.12.201X) to the rows:
person_yr[, date := as.Date(paste(year, '-12-31', sep=''))]

# Running variable:
person_yr[, running_var := as.integer(date - bday_18th)]
person_yr[, ':=' (birth_date = NULL, bday_18th = NULL,
                  bday_18_year = NULL, bday_18_month = NULL)]

# A 2-year bandwidth (2x365 days):
person_yr = person_yr[running_var >= -2*365 & running_var <= 2*365-1]


# Merge policies:

# Group Helsinki (free) and the exemption municipalities.
policies[policy=='free', policy := 'exemptions']

person_yr = merge(person_yr, policies, by.x='municipality', by.y='no', 
                  all.x=TRUE)


# Create variables for balance checks:
person_yr[, ':=' (income_eq = income_eq / 1000,
                  with_parents = as.integer(family_relationship == 3),
                  unemployed = as.integer(activity == 12))]


# To increase comparability with our main analysis sample, we drop specific 
# municipality-year pairs for data quality issues in the primary care data.

changes = changes[month==12][, month := NULL]
person_yr = merge(person_yr, changes, by=c('municipality', 'year'), all.x=TRUE)

# Drop the specific municipality-year pairs:
person_yr = person_yr[drop_transfer != 1][, drop_transfer := NULL]

# Drop 2011 in Rauma (an unusually large number of referrals per capita
# in a relatively large municipality):
person_yr = person_yr[drop_refs != 1][, drop_refs := NULL]


# Drop person-year observations in which the person living in exemption 
# municipalities (Espoo, Turku, Tuusula) is NOT a student at the end of year.###
person_yr = person_yr[(municipality %in% c(49, 853, 858) & activity==22) |
                        !(municipality %in% c(49, 853, 858))]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Primary care visits.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read and aggregate primary care GP visits:

gp_visits = lapply(years, function(year) {
  
  yr = 2010 + year
  print(yr)
  
  # Read GP visits:
  source = paste(input_ids, as.character(year), ".csv", sep="")
  visits = data.table::fread(
    source, drop = c('birth_year', 'birth_month', 'birth_day')
  )

  # Aggregate GP visits at the ID level:
  visits = visits[year==yr, .(gp_visits = .N), by='id']
  
})
gp_visits = do.call(rbind.data.frame, gp_visits)

# Merge:
person_yr = merge(person_yr, gp_visits, by='id', all.x=TRUE)

# Fill zeroes if no GP visits are observed:
person_yr[is.na(gp_visits), gp_visits := 0]

saveRDS(person_yr, output_data)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) RD balance plots. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# A function that subsets and aggregates the data:

subset_data_balance = function(data, client_gender, municipal_policy, outcome,
                               bw=730) {
  # INPUTS:
  # data: person_yr
  # client_gender: 1=men, 2=women
  # municipal_policy: 'exemptions' for exemption municipalities (and Helsinki), 
  #                   and 'copayment' if such was charged.
  # outcome: outcome as character
  # bw: bandwidth as days as an integer
  # OUTPUTS:
  # a subset of the analysis data
  
  df = data[gender == client_gender & policy %in% municipal_policy
            ][, outcome := get(outcome)
              ][, .(outcome = mean(outcome, na.rm=TRUE),
                    population = .N), by='running_var']
  
  # The cohort t=-1 (those born on January 1st) is an outlier. Drop it:
  print(df[running_var %in% c(-5:4)][order(running_var)])
  print(df[, min(population)])
  df = df[running_var != -1]
  df = df[running_var %in% c(-bw:(bw-1))]
  
  return(df)
}



# Models to be plotted:
models = list(
  list(policy = 'copayment', title = 'Copay: '),
  list(policy = c('exemptions', 'free'), title = 'Exempt: ')
)


# Outcomes to be looped over:
outcomes = data.table(
  var = c('gp_visits', 'income_eq', 'with_parents', 'unemployed'),
  label = c('GP Visits', 'Income', 'with Parents', 'Unemployed'),
  ylab = c('No. of visits', 'Euros (in thousands)', 'Share', 'Share')
)

# Bandwidths to be looped over:
bws = c(365, 730)



# Loop over bandwidths:

rd_plots_bal = lapply(bws, function(bandwidth) {
  
  # Loop over models:
  
  plots.mods = lapply(models, function(mod) {
    
    # Loop over outcomes:
    plots.otc = lapply(c(1:4), function(i) {
      
      
      # Extract the right data:
      data = subset_data_balance(
        person_yr, client_gender = 2, municipal_policy = mod$policy, 
        outcome = outcomes[i, var], bw=bandwidth)
      
      # Plot:
      
      p = rdrobust::rdplot(
        y=data$outcome, x=data$running_var, c=-0.5, p=4, 
        kernel='uni', binselect = 'esmv', weights = data$population,
        title= paste(mod$title, outcomes[i, label], sep=''), 
        x.label = 'Days to the 18th birthday', 
        y.label = outcomes[i, ylab],
        col.dots = 'grey40', col.lines = 'black'
      )
      
      p = p$rdplot + theme(text = element_text(size=19))
      
      
      if(bandwidth==365) { 
        p = p + scale_x_continuous(breaks = c(-200, 0, 200))
      } else if (bandwidth==730) {
        p = p + scale_x_continuous(breaks = c(-600, -300, 0, 300, 600)) } 
      
      return(p)
    })
    names(plots.otc) = outcomes$var
    
    return(plots.otc)
    
  })
  
  
})
names(rd_plots_bal) = paste0('bw_', bws)


# Save:

cairo_pdf(filename = output_rd_plot_balance, width = 16.0, height = 8.0)
print( patchwork::wrap_plots(
  c(rd_plots_bal$bw_365[[1]], rd_plots_bal$bw_365[[2]]),
    nrow = 2, byrow = TRUE))
dev.off()

cairo_pdf(filename = output_rd_plot_balance_730, width = 16.0, height = 8.0)
print( patchwork::wrap_plots(
  c(rd_plots_bal$bw_730[[1]], rd_plots_bal$bw_730[[2]]),
  nrow = 2, byrow = TRUE))
dev.off()

# End.
