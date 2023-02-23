
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###          r-script 2_id_date_panels_set3.R     ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Create ID-date panels on healthcare use for analyses, date being 
# relative to the 18th bday in a 365-day bandwidth. Here, we focus on a subset
# of individuals who are observed to live in the same policy area (copayment or
# exemption) at the end of subsequent calendar years at 17 and 18 with no change 
# in family relationship (a proxy for moving away from parents).

rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.

# Inputs:
input_quality = "W:/ASMA1/data/intermediate/visits_changes.rds"
input_policies = "W:/ASMA1/data/raw/rdd_policies.csv"
input_ids = "W:/ASMA1/data/intermediate/visits_quality_1" # visits_quality_201X.csv, X in 1:9
input_main = "W:/ASMA1/data/intermediate/visits_main_1" # visits_main_201X.csv, X in 1:9
input_folk = "W:/ASMA1/data/intermediate/folk_data_20" # folk_data_201XX.csv, X in 11:20

# Intermediate outputs (also used as inputs):
#interim1 = "W:/ASMA1/data/intermediate/id_date_panels/interim_set3_id_dateAA_genderB.rds" # where AA in 11:19 and B in 1:2

# Outputs:
#output = "W:/ASMA1/data/intermediate/id_date_panels/analysis_set3_id_yearAA_genderB.rds" # where AA in 11:19 and B in 1:2


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


### Expand the data to include each year in 2011-2020 for
#   each observed individual. ###

person_yr = CJ(person_ids$id, c(2011:2020))
setnames(person_yr, old = c('V1','V2'), new = c('id', 'year'))


### Merge birthdays and constrain the size by using the difference
# of calendar year and birth year. ###

# Merge birthdays to the panel:
person_yr = merge(person_yr, person_ids, by='id', all.x=TRUE)
print("Merging birthdates to the panel is done")

# Filter and drop some variables:
person_yr = person_yr[year - byear <= 20 & year - byear >= 16
                      ][, byear := NULL]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) FOLK data.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###

### Read FOLK data. ###

folk = lapply(c(11:20), function(year) {
  
  yr = as.integer(paste("20", year, sep=""))
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
  
  folk = folk[!is.na(income_decile)][, income_eq := NULL]
  
  return(folk)
  
})
folk = do.call(rbind.data.frame, folk)


# Extract the income decile from the year at the end of which the individuals
# is aged 17:
income_17 = folk[age==17, .(id, income_decile)]
setnames(income_17, old='income_decile', new='income_decile_17')


### Merge FOLK data to the ID-year panel. ###

# Impute zeroes for month variables if no social assistance was received.
# Note: we have currently no social assistance data from 2019, so leave NAs.

cols = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
         'july', 'aug', 'sep', 'oct', 'nov', 'dec')

for(col in cols) {
  folk[!(year %in% c(2019:2020)) & is.na(get(col)), (col) := 0]
  print(folk[, mean(get(col), na.rm=TRUE)])
}

# Currently, social assistance variables (months) are not indicators. The value
# is above one if there are multiple persons in the family receiving social 
# assistance. Transform them to indicators:

for(col in cols) {
  folk[!(year %in% c(2019:2020)), (col) := as.integer(get(col) > 0)]
  print(folk[, mean(get(col), na.rm=TRUE)])
}

# Merge:
person_yr = merge(person_yr, income_17, by='id', all.x = TRUE)
person_yr = merge(person_yr, folk, by=c('id', 'year'), all.x = TRUE)
person_yr = person_yr[!is.na(gender)]
print("Merging folk data to the panel is done")

# Release some memory:
rm(folk, person_ids, income_17)
gc()


# We take person-year observations where the person is aged 17 to 18 at the end 
# of year:
person_yr = person_yr[age %in% c(17:18)]

# Each person should have two person-year observations:
ids = person_yr[, .N, by='id'][N==2, id]
person_yr = person_yr[id %in% ids]


# Create a variable that shows the policy group:

person_yr[municipality %in% policies[policy %in% c('copayment'), no],
          copay := 1]
person_yr[municipality %in% policies[policy %in% c('free', 'exemptions'), no],
          copay := 0]
person_yr[is.na(copay), copay := -1]


# We require that the person lived in the same policy group (either in 
# copayment municipalities or exemption municipalities) throughout our study 
# period with the same family relationship (e.g., child, spouse, or not in the 
# family population).

ids = person_yr[, .(unique_copay = length(unique(copay)),
                    unique_status = length(unique(family_relationship))), 
                by='id'][unique_copay==1 & unique_status==1, id]

person_yr = person_yr[id %in% ids]


# Include only sample municipalities in 2011-2019:
person_yr = person_yr[municipality %in% sample_munies &
                        year %in% c(2011:2019)]


### Drop specific municipality-year pairs for data quality issues
#     (see 1_quality_checks). ###

changes = changes[month==12][, month := NULL]

# Merge:
person_yr = merge(person_yr, changes, by=c('municipality', 'year'), all.x=TRUE)
print("Merging changes data to the panel is done")

# Drop the specific municipality-year pairs:
person_yr = person_yr[drop_transfer != 1][, drop_transfer := NULL]

# Drop 2011 in Rauma (an unusually large number of referrals per capita
# in a relatively large municipality):
person_yr = person_yr[drop_refs != 1][, drop_refs := NULL]


# In exemption areas (Helsinki not included here), the exemption only applied 
# to those who actually studies. We drop persons who did not study at the end
# of year at 17. 
ids = person_yr[(municipality %in% c(49, 853, 858) & age==17)]

# 5.1% are excluded:
ids[, mean(activity!=22)]
ids = ids[activity!=22, id]
person_yr = person_yr[!(id %in% ids)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) ID-date panels.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Next, we will expand the data.table to obtain ID-date panels which 
# are stored at the year-gender level.


# Loop over years:
for(i in 2011:2019) {
  
  i_char = as.character(i-2000)
  
  # Store all dates between January 1st and December 31th:
  dates = seq(from = as.Date(paste(as.character(i), '-01-01', sep='')),
              to = as.Date(paste(as.character(i), '-12-31', sep='')),
              by='days')
  
  # Loop over genders:
  for(j in 1:2) {
    
    j_char = as.character(j)
    
    # Write the output path:
    path = paste(
      'W:/ASMA1/data/intermediate/id_date_panels/interim_set3_id_date',
      i_char, '_gender', j_char, '.rds', sep=''
    )
    print(path)
    
    # Extract:
    data = person_yr[year == i & gender == j
                     ][, ':=' (year=NULL, gender=NULL)]
    
    # Expand:
    person_date = CJ(data$id, dates)
    setnames(person_date, old = c('V1','dates'), new = c('id', 'date'))
    
    # Merge:
    person_date = merge(person_date, data, by='id', all.x=TRUE)
    
    # Compute age (in days) relative to the 18th birthday:
    person_date[, running_var := as.integer(date - bday_18th)]
    person_date[, ':=' (birth_date = NULL, bday_18th = NULL)]
    
    # A 365-day bandwidth:
    person_date = person_date[running_var >= -365 &
                                running_var <= 365-1]
    
    # Save:
    saveRDS(person_date, path)
    
  }  
}

# Free some memory:
rm(list=setdiff(
  ls(), c('input_assumptions', 'input_main', 'input_refs', 
          'input_spec', 'output_main'))
)
gc()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) GP visits.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Loop over years:

for(i in 1:9) {
  
  i_char = as.character(i+10)
  
  # Read GP visits:
  source = paste(input_main, as.character(i), ".csv", sep="")
  visits = data.table::fread(
    source, drop = c('birth_year', 'birth_month', 'birth_day')
  )
  
  # Create date variable:
  visits[, date := as.Date(paste(year, month, day, sep='-'))]
  
  # Aggregate GP visits at the ID-date level:
  visits = visits[, .(gp_visits = .N), by=c('id', 'date')]
  
  
  # Next, we read ID-date panels and left join 'visits' to them.
  
  # Loop over genders:
  for(j in 1:2) {
    
    j_char = as.character(j)
    
    # Write the input and output paths:
    input = paste(
      'W:/ASMA1/data/intermediate/id_date_panels/interim_set3_id_date',
      i_char, '_gender', j_char, '.rds', sep='' )
    output = paste(
      'W:/ASMA1/data/intermediate/id_date_panels/analysis_set3_id_date',
      i_char, '_gender', j_char, '.rds', sep='' )
    print(input)
    
    # Read and merge:
    panel = readRDS(input)
    panel = merge(panel, visits, by=c('id', 'date'), all.x = TRUE)
    
    # Fill zeroes (if no contacts are observed):
    panel[is.na(gp_visits), gp_visits := 0]
    
    # Save:
    saveRDS(panel, output)
    
  }  
}

# End.
