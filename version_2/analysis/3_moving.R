
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###              r-script 3_moving.R              ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Construct statistics on how common is it to move from/to 
#         our target municipalities in order to assess potential biases
#         to the results.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.

# Inputs:
input_policies = "W:/ASMA1/data/raw/rdd_policies.csv"
input_ids = "W:/ASMA1/data/intermediate/visits_quality_1" # visits_quality_201X.csv, X in 1:9
input_folk = "W:/ASMA1/data/intermediate/folk_data_201" # folk_data_201X.csv, X in 1:9
input_pc = "W:/ASMA1/data/intermediate/visits_main_1" # visits_main_1X.csv, X in 1:9

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read some data into memory. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# The study years are 2011-2019:
years = c(1:9)

# Read data on municipal copayment policies:
policies = data.table::fread(input_policies, select=c('no','policy2020'))
policies = policies[policy2020 != 'remove']
setnames(policies, old='policy2020', new='policy')

# The following 23 municipalities are our sample:
sample_munies = unique(policies$no)
print(sample_munies)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) FOLK data.  ####
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
  
  # Keep only women at age 17 or 18:
  folk = folk[gender==2 & age %in% c(17:18)]
  
  # Keep the following columns:
  cols = c('id', 'age', 'municipality', 'activity', 'family_relationship',
           'income_decile', 'year')
  folk = folk[, mget(cols)]
  
  return(folk)
  
})
folk = do.call(rbind.data.frame, folk)


# Keep only those who are observed twice (at age 17 and 18):
ids = folk[, .N, by='id'][N==2, id]
folk = folk[id %in% ids]


# We will take only those who are ever observed in the data 
# (= who had a curative primary care GP visit in 2011-2019):


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Primary care data.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###

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


### Take all individuals ever observed in the data 
# (= who had a curative primary care GP visit in 2011-2019). ###

person_visit = lapply(years, function(year) {
  
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
person_visit = do.call(rbind.data.frame, person_visit)

# Keep only unique rows:
person_visit = unique(person_visit)

# There are about a dozen persons who "have" two birthdates in the data.
# Examine the issue:
doubles = person_visit[, .N, by='id']
doubles = merge(person_visit, doubles, by='id', all.x = TRUE)
print(length(doubles[N>1, unique(id)]))

# Drop the persons who have multiple birthdays in the data:
person_visit = doubles[N==1][, N := NULL]
rm(doubles)

folk = folk[id %in% person_visit[, id]]
rm(person_visit)


# Replace the municipality information with a variable that shows
# policy group (1:copayment, 0:exemption, -1: not in sample):


# Create a variable that shows the policy group:

folk[municipality %in% policies[policy %in% c('copayment'), no],
          copay := 1]
folk[municipality %in% policies[policy %in% c('free', 'exemptions'), no],
     copay := 0]
folk[is.na(copay), copay := -1]
folk[, ':=' (income_decile = as.integer(income_decile))]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) GP visits in public primary care. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read GP visits in public primary care:

pc_visits = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read AvoHilmo data:
  source_pc = paste(input_pc, year, ".csv", sep="")
  visits = data.table::fread(
    source_pc, 
    drop=c('birth_month', 'birth_year', 'birth_day', 'day')
  )
  
  # Aggregate at the id-year level:
  visits = visits[year==yr][, .(visits = .N), by=c('id','year')]
  
})
pc_visits = do.call(rbind.data.frame, pc_visits)

folk = merge(folk, pc_visits, by=c('id', 'year'), all.x=TRUE)
folk[is.na(visits), visits := 0]
folk[, year := NULL]

# Pivot wider:
folk = dcast(folk, id ~ age, value.var = c('activity', 'family_relationship', 
                                           'income_decile', 'copay',
                                           'municipality', 'visits'))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Analyse.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# How common is it to move away from parents between December 31 when 17 and
# December 31 when 18? Show the share of those who lived with their parents on 
# December 31 when 17 but whose family status changed by December 31 when 18.

folk[family_relationship_17==3, 
     .(share = mean(family_relationship_17 != family_relationship_18))]


# How many of those moves were within the same municipality or within the same
# policy area? Again, use the change in the family status as a proxy.

# Within the same municipality:
folk[family_relationship_17==3 & 
       family_relationship_17 != family_relationship_18, 
     .(share = mean(municipality_17 == municipality_18))]

# Within the same policy area:
folk[family_relationship_17==3 & 
       family_relationship_17 != family_relationship_18, 
     .(share = mean(copay_17 == copay_18))]


# So, a clear majority of the moves are within the same policy area.
# Examine whether moving within treatment areas is more common in 
# copayment municipalities. Thus, we keep those whose treatment area variable
# does not change between the two years.

# Finding 1: A change in family status is more common in the copayment area:
folk[copay_17==copay_18, 
     .(change.family = mean(family_relationship_17 != family_relationship_18)), 
     by='copay_17'][copay_17 != -1]

# Finding 2: A change from a child status is more common in the copayment area:
folk[copay_17==copay_18 & family_relationship_17==3, 
     .(change.child = mean(family_relationship_17 != family_relationship_18)), 
     by='copay_17'][copay_17 != -1]

# Finding 3: The mean income decile decreases more in the copayment area:
folk[copay_17==copay_18, 
     .(change.decile = 
           mean(income_decile_18 - income_decile_17)), 
     by='copay_17'][copay_17 != -1]


# Next, we examine moves between areas.

folk[, .(share.status = 100 * .N / nrow(folk)), by=c('copay_17', 'copay_18')]

folk[family_relationship_17==3 & 
       family_relationship_17 != family_relationship_18, 
     .(share.move = 100 * .N / nrow(folk[family_relationship_17==3 &
                                           family_relationship_17 !=
                                           family_relationship_18])), 
     by=c('copay_17', 'copay_18')]


# Is there selection in moving?

# Finding 4: Adolescents whose policy area changes have lower income.
folk[family_relationship_17==3, .(income = mean(income_decile_17)), 
     by= copay_17==copay_18]

# Finding 5: Adolescents whose policy area changes have higher primary care use.
folk[family_relationship_17==3, .(pc_use = mean(visits_17)), 
     by= copay_17==copay_18]

# Finding 6: adolescents whose family status changes but policy area is fixed 
# have lower income.
folk[copay_17==copay_18 & family_relationship_17==3, 
     .(income = mean(income_decile_17)), 
     by= family_relationship_17 == family_relationship_18]

# End.
