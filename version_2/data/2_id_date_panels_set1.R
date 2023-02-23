
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###          r-script 2_id_date_panels_set1.R     ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Create ID-date panels on healthcare use, date being 
# relative to the 18th bday in a 730-day bandwidth (two years). This is the
# main analysis dataset.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.

# Inputs:
input_quality = "W:/ASMA1/data/intermediate/visits_changes.rds"
input_policies = "W:/ASMA1/data/raw/rdd_policies.csv"
input_ids = "W:/ASMA1/data/intermediate/visits_quality_1" # visits_quality_201X.csv, X in 1:9
input_assumptions = "W:/ASMA1/data/intermediate/visits_diagnoses_1" # visits_diagnoses_201X.csv, X in 1:8
input_main = "W:/ASMA1/data/intermediate/visits_main_1" # visits_main_201X.csv, X in 1:9
input_folk = "W:/ASMA1/data/intermediate/folk_data_201" # folk_data_201X.csv, X in 1:9
input_refs = "W:/ASMA1/data/intermediate/referrals_201" # referrals_201X.csv, X in 1:9

# Intermediate outputs (also used as inputs):
#interim1 = "W:/ASMA1/data/intermediate/id_date_panels/interim_set1_id_dateAA_genderB.rds" # where AA in 11:18 and B in 1:2

# Outputs:
#output = "W:/ASMA1/data/intermediate/id_date_panels/analysis_set1_id_yearAA_genderB.rds" # where AA in 11:18 and B in 1:2


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
person_yr = person_yr[year - byear <= 20 & year - byear >= 16
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
  
  folk = folk[!is.na(income_decile)][, income_eq := NULL]
  
  return(folk)
  
})
folk = do.call(rbind.data.frame, folk)


# Extract the income decile from the year at the end of which the individuals
# is aged 17:
income_17 = folk[age==17, .(id, income_decile)]
setnames(income_17, old='income_decile', new='income_decile_17')
folk[, age := NULL]


### Merge FOLK data to the ID-year panel and keep only years when 
# the person lives in one of the 23 sample municipalities. ###

# Keep only sample municipalities in the FOLK data:
folk = folk[municipality %in% sample_munies]

# Impute zeroes for month variables if no social assistance was received.
# Note: we have currently no social assistance data from 2019, so leave NAs.

cols = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
         'july', 'aug', 'sep', 'oct', 'nov', 'dec')

for(col in cols) {
  folk[year != 2019 & is.na(get(col)), (col) := 0]
  print(folk[, mean(get(col), na.rm=TRUE)])
}

# Currently, social assistance variables (months) are not indicators. The value
# is above one if there are multiple persons in the family receiving social 
# assistance. Transform them to indicators:

for(col in cols) {
  folk[year != 2019, (col) := as.integer(get(col) > 0)]
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


### Drop person-year observations in which the person living in exemption 
# municipalities (Espoo, Turku, Tuusula) is NOT a student at the end of year.###

person_yr = person_yr[(municipality %in% c(49, 853, 858) & activity==22) |
                        !(municipality %in% c(49, 853, 858))]


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
      'W:/ASMA1/data/intermediate/id_date_panels/interim_set1_id_date',
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
    
    # A 2-year bandwidth (2x365 days):
    person_date = person_date[running_var >= -2*365 &
                                running_var <= 2*365-1]
    
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
#### 5) Health checks, GP visits, and referrals.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Loop over years:

for(i in 1:9) {
  
  i_char = as.character(i+10)
  
  
  ### First, read and tidy health checks and GP visits. ###
  
  if(i %in% c(1:8)) {
    
    # Read health checks:
    source = paste(input_assumptions, as.character(i), ".csv", sep="")
    checks = data.table::fread(
      source, drop = c('birth_year', 'birth_month', 'birth_day')
    )
    
    # Create date variable:
    checks[, date := as.Date(paste(year, month, day, sep='-'))]
    
    # Aggregate health checks at the ID-date level:
    checks = checks[, .(health_checks = .N), by=c('id', 'date')]
    
  } 
  
  # Read GP visits:
  source = paste(input_main, as.character(i), ".csv", sep="")
  visits = data.table::fread(
    source, drop = c('birth_year', 'birth_month', 'birth_day')
  )
  
  # Create date variable:
  visits[, date := as.Date(paste(year, month, day, sep='-'))]
  
  # Aggregate GP visits at the ID-date level:
  visits = visits[, .(gp_visits = .N), by=c('id', 'date')]
  
  
  if(i %in% c(1:8)) {
    
    # Merge health checks and GP visits:
    visits = merge(visits, checks, by=c('id', 'date'), all = TRUE)
    rm(checks)
    
  }
  
  
  # Read referrals to specialist consultations:
  source = paste(input_refs, i, ".csv", sep="")
  refs = data.table::fread(source, drop=c('discharged'))
  
  # Take those rows where we observe the contact start date and where the
  # referral is written in public primary care:
  refs = refs[contact_started==1 & ref_origin==1
              ][, ':=' (contact_started = NULL, ref_origin = NULL)]
  
  # Take unique id-date pairs. More than one referrals could be written
  # on each day, but in our analyses we create a dummy on whether an 
  # individual X received any referrals on date Y:
  refs = unique(refs)
  
  # Create date variable:
  refs[, date := as.Date(paste(year, month, day, sep='-'))]
  
  # Aggregate referrals at the ID-date level:
  refs = refs[, .(refs = .N), by=c('id', 'date')]
  
  # Merge health checks, GP visits and referrals:
  visits = merge(visits, refs, by=c('id', 'date'), all = TRUE)
  rm(refs)
  
  
  ### Next, we read ID-date panels and left join 'visits' to them. ###
  
  # Loop over genders:
  for(j in 1:2) {
    
    j_char = as.character(j)
    
    # Write the input and output paths:
    input = paste(
      'W:/ASMA1/data/intermediate/id_date_panels/interim_set1_id_date',
      i_char, '_gender', j_char, '.rds', sep='' )
    output = paste(
      'W:/ASMA1/data/intermediate/id_date_panels/analysis_set1_id_date',
      i_char, '_gender', j_char, '.rds', sep='' )
    print(input)
    
    # Read and merge:
    panel = readRDS(input)
    panel = merge(panel, visits, by=c('id', 'date'), all.x = TRUE)
    
    # Fill zeroes (if no contacts are observed):
    panel[is.na(gp_visits), gp_visits := 0]
    panel[is.na(refs), refs := 0]
    
    if(i %in% c(1:8)) {
      panel[is.na(health_checks), health_checks := 0]
    } else if (i==9) {
      panel[, health_checks := NA] }
    
    # Save:
    saveRDS(panel, output)
    
  }  
}

# End.
