
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         r-script 2_analysis_data_set2.R       ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Create datasets for supplementary analyses. Here, we focus on a 
# subset of individuals who are observed to live in the same policy area 
# (copayment or exemption) throughout the study period of six months.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.

# Inputs:
input_policies = "W:/ASMA1/data/raw/rdd_policies.csv"
input_folk = "W:/ASMA1/data/intermediate/folk_data_201" # folk_data_201X.csv, X in 1:9
#panel = "W:/ASMA1/data/intermediate/id_date_panels/analysis_set2_id_yearAA_genderB.rds" # where AA in 11:19 and B in 1:2

# Outputs:
output_local_pol = "W:/ASMA1/data/cleaned/data2_local_pol.rds"
output_parametric = "W:/ASMA1/data/cleaned/data2_parametric.rds"
output_local_rand = "W:/ASMA1/data/cleaned/data2_local_rand.rds"


###
###


# Read data on municipal copayment policies:
policies = data.table::fread(input_policies, select=c('no','policy2020'))
policies = policies[policy2020 != 'remove']
setnames(policies, old='policy2020', new='policy')

# Study years:
years = c(1:9)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Aggregated analysis data for local polynomial methods.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Loop over years:
df = lapply(years, function(i) {
  
  i_char = as.character(i+10)
  
  # Loop over genders:
  data = lapply(1:2, function(j) {
    
    j_char = as.character(j)
    
    # Write the input paths:
    input = paste(
      'W:/ASMA1/data/intermediate/id_date_panels/analysis_set2_id_date',
      i_char, '_gender', j_char, '.rds', sep='' )
    print(input)
    
    # Read and merge policies to the panel:
    df = readRDS(input)
    df = merge(df, policies, by.x='municipality', by.y='no', all.x = TRUE)
    
    # Helsinki abolished the copayment in 1/2013:
    df[municipality==91 & date < as.Date('2013-01-01'), policy := 'copayment']
    
    # We observe Turku's policy from 9/2011:
    df[municipality==853 & date < as.Date('2011-09-01'), 
       policy := NA_character_]
    df = df[!is.na(policy)]
    
    
    # Aggregate at the policy-decile-date level:
    
    df = df[!is.na(income_decile_17), 
            .(gp_visits = sum(gp_visits),
              population = length(unique(id))), 
            by = c('policy', 'income_decile_17', 'running_var')]
    
    
    # Compute annualized contacts per resident:
    # Multiply contacts by 365 to get annualized contacts:
    df[, gp_visits_ann := gp_visits * 365 / population]
    
    # Input gender and year:
    df[, ':=' (year = 2010 + i,
               gender = j)]
  })
  data = do.call(rbind.data.frame, data)
  
  return(data)
})
df = do.call(rbind.data.frame, df)

# The number of total visits:
df[is.na(income_decile_17), sum(gp_visits)]
df[!is.na(income_decile_17), sum(gp_visits)]

# Save:
saveRDS(df, output_local_pol)


# Free some memory:
rm(list=setdiff(
  ls(), c('policies', 'years', 'output_parametric', 
          'output_local_rand', 'input_folk'))
)
gc()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Id-month level data for parametric analysis.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Loop over years:
df = lapply(years, function(i) {
  
  i_char = as.character(i+10)
  
  # Loop over genders:
  data = lapply(1:2, function(j) {
    
    j_char = as.character(j)
    
    # Write the input paths:
    input = paste(
      'W:/ASMA1/data/intermediate/id_date_panels/analysis_set2_id_date',
      i_char, '_gender', j_char, '.rds', sep='' )
    print(input)
    
    # Read:
    df = readRDS(input)
    
    # Create variables for year and month:
    df[, ':=' (year = 2010 + i,
               month = as.integer(format(date, '%m')))]
    
    
    if(i %in% c(2:8)) { # social assistance is observed in 2012-2018:
      
      # Collect data on social assistance:
      assistance = unique(
        df[, .(id, jan, feb, mar, apr, may, june, 
               july, aug, sep, oct, nov, dec)])
      
      # Pivot longer:
      assistance = melt(
        assistance, id.vars = c('id'),
        measure.vars = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
                         'july', 'aug', 'sep', 'oct', 'nov', 'dec'),
        variable.name = 'month', value.name = 'social_assistance',
        variable.factor = FALSE
      )
      
      # Replace month abbreviations with dates:
      
      months = data.table(month_no = c(1:12),
                          month = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
                                    'july', 'aug', 'sep', 'oct', 'nov', 'dec'))
      
      assistance = merge(assistance, months, by='month', all.x = TRUE)
      assistance[, month := NULL]
      setnames(assistance, old='month_no', new='month')
      
    }
    
    
    # Aggregate visits at the ID-calendar month level:
    df = df[, .(gp_visits = sum(gp_visits),
                bday_18_year = mean(bday_18_year),
                bday_18_month = mean(bday_18_month)),
            by=c('id', 'year', 'month')]
    
    
    # Merge social assistance data to the 'df' if they are observed:
    if(i %in% c(2:8)) { 
      df = merge(df, assistance, by=c('id', 'month'), all.x = TRUE) }
    
    return(df)
    
  })
  data = do.call(rbind.data.frame, data)
  
  return(data)
})

# Row bind:
df.1 = do.call(rbind.data.frame, df[c(1,9)])
df.2 = do.call(rbind.data.frame, df[2:8])
df = rbind(df.1, df.2, fill=TRUE)


# Map calendar months to integers:

months = data.table(
  year = rep(2000:2030, each=12),
  month = rep(1:12, times=length(2000:2030)),
  month_no = c(1:((length(2000:2030))*12))
)

df = merge(df, months, by=c('year', 'month'), all.x=TRUE)

setnames(months, old=c('year', 'month', 'month_no'), 
         new=c('bday_18_year', 'bday_18_month', 'bday_month_no'))

df = merge(df, months, by=c('bday_18_year', 'bday_18_month'), all.x=TRUE)


# Count distance to the 18th birthday:
df[, running_var := month_no - bday_month_no]

# Drop the calendar month during which the person turns 18.
df = df[running_var != 0][running_var %in% c(-23:23)]

# Drop variables that are no longer needed:
df[, ':=' (month_no=NULL, bday_month_no=NULL,
           bday_18_month=NULL, bday_18_year=NULL)]

# Multiply visits by 12 to get annualized visits:
df[, gp_visits := gp_visits * 12]


# Next, we need FOLK data to get covariates:

folk = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read FOLK data. Drop social assistance variables:
  cols = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
           'july', 'aug', 'sep', 'oct', 'nov', 'dec')
  source_folk = paste(input_folk, year, ".csv", sep="")
  folk = data.table::fread(source_folk, drop=cols)
  
  # Percentage of rows where the family equivalized income is exactly zero:
  print(100 * nrow(folk[income_eq==0]) / nrow(folk))
  
  # We drop individuals with family equivalized income exactly zero (missing).
  folk = folk[income_eq > 0]
  
  # Create variable income_decile (by year):
  folk[, income_decile := cut(income_eq,
                              breaks = quantile(income_eq, probs = 0:10/10),
                              labels = 0:9, right = FALSE)]
  
  folk = folk[!is.na(income_decile)][, income_eq := NULL]
  
  return(folk)
  
})
folk = do.call(rbind.data.frame, folk)


# Extract the income decile from the year at the end of which the individuals
# is aged 17:
income_17 = folk[age==17, .(id, income_decile)]
setnames(income_17, old='income_decile', new='income_decile_17')
folk[, age := NULL]
income_17 = unique(income_17)


# Merge FOLK data to the ID-calendar month panel (left join):
df = merge(df, income_17, by='id', all.x = TRUE)
df = merge(df, folk, by=c('id', 'year'), all.x = TRUE)
print("Merging folk data to the panel is done")
rm(folk, income_17)


# Merge policies to the panel:
df = merge(df, policies, by.x='municipality', by.y='no', all.x = TRUE)

# Helsinki abolished the copayment in 1/2013:
df[municipality==91 & year %in% c(2011:2012), policy := 'copayment']

# We observe Turku's policy from 9/2011:
df[municipality==853 & year==2011 & month %in% c(1:8), policy := NA_character_]
df = df[!is.na(policy)]


# Replace the ID with a more memory-efficient integer:

ids = unique(df$id)
ids = data.table(id=ids, id_no=c(1:length(ids)))

df = merge(df, ids, by='id', all.x=TRUE)
print("Merging new IDs to the panel is done")
rm(ids)

# Drop variables that are not needed in analyses:
df[, ':=' (id = NULL, month = NULL)]

# Save:
df = df[order(id_no, running_var)]
saveRDS(df, output_parametric)

# The number of total visits:
sum(df$gp_visits) / 12
length(df$gp_visits) 

# Free some memory:
rm(list=setdiff(
  ls(), c('policies', 'years', 'output_local_rand'))
)
gc()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Id level data for local randomization analysis.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Loop over years:
df = lapply(years, function(i) {
  
  i_char = as.character(i+10)
  
  # Loop over genders:
  data = lapply(1:2, function(j) {
    
    j_char = as.character(j)
    
    # Write the input paths:
    input = paste(
      'W:/ASMA1/data/intermediate/id_date_panels/analysis_set2_id_date',
      i_char, '_gender', j_char, '.rds', sep='' )
    print(input)
    
    # Read and filter:
    df = readRDS(input)
    df = df[running_var %in% c(-45:44)]
    
    # Drop social assistance variables:
    cols = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
             'july', 'aug', 'sep', 'oct', 'nov', 'dec')
    df = df[, mget(setdiff(colnames(df), cols))]
    
    # Merge policies to the panel:
    df = merge(df, policies, by.x='municipality', by.y='no', all.x = TRUE)
    
    # Helsinki abolished the copayment in 1/2013:
    df[municipality==91 & date < as.Date('2013-01-01'), policy := 'copayment']
    
    # We observe Turku's policy from 9/2011:
    df[municipality==853 & date < as.Date('2011-09-01'), 
       policy := NA_character_]
    df = df[!is.na(policy)]
    
    # Drop some variables:
    df[, ':=' (municipality=NULL, health_checks=NULL, date=NULL)]
    
    # Input gender and year:
    df[, ':=' (year = 2010 + i,
               gender = j)]
  })
  data = do.call(rbind.data.frame, data)
  
  return(data)
})
df = do.call(rbind.data.frame, df)


# Keep only those who are observed throughout the whole window (-45:44):
ids = df[, .N, by='id'][N == 2*45, id]
df = df[id %in% ids]


# Keep only those with the same policy throughout the whole window:
ids = df[, .(policy = length(unique(policy))), by='id'][policy==1, id]
df = df[id %in% ids]


# Sum pre- and post-cutoff visits for each individual using several bandwidths
#   and additionally a 3-day donut hole:

bandwidths = seq(15, 45, by=5)
donut_holes = c(0, 3)

# Loop over bandwidths:
data = lapply(bandwidths, function(bw) {
  
  print(bw)
  
  # Loop over donut holes:
  dfs = lapply(donut_holes, function(dh) {
    
    # Extract the data:
    data = df[running_var %in% c(-bw:(bw-1))
              ][, post_d := as.integer(running_var >= 0)]
    
    if(dh > 0) {
      data = data[!(running_var %in% c(-dh:(dh-1)))] }
    
    # Aggregate:
    data = data[, .(income_decile_17 = unique(income_decile_17),
                    policy = unique(policy),
                    gender = unique(gender),
                    year = floor(mean(year)),
                    gp_visits = sum(gp_visits)), by=c('id', 'post_d')
    ][, ':=' (bw = bw, donut_hole = dh)]
    
  })
  dfs = do.call(rbind.data.frame, dfs)
  
})
data = do.call(rbind.data.frame, data)


# Compute annualized contacts per resident:
data = data[, mget(colnames(data))]
data[, gp_visits_ann := gp_visits * (365 / (bw-donut_hole))]


# Replace the ID with a more memory-efficient integer:

data = data[order(id)]
ids = unique(data$id)
ids = data.table(id=ids, id_no=c(1:length(ids)))

data = merge(data, ids, by='id', all.x=TRUE)
print("Merging new IDs to the dataset is done")
rm(ids)
data[, id := NULL]


# The number of total visits:
sum(data[bw==30 & donut_hole == 0]$gp_visits)

# Save:
saveRDS(data, output_local_rand)

# End.
