
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###       R script 1_data_for_quality_checks.R    ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Merge different datasets together to create a dataset
#           with which to analyze data quality.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating data.

# Inputs:
input_pc = "W:/ASMA1/data/intermediate/visits_quality_1" # visits_quality_1X.csv, X in 1:9
input_folk = "W:/ASMA1/data/intermediate/folk_data_201" # folk_data_201X.csv, X in 1:9
input_refs = "W:/ASMA1/data/intermediate/referrals_201" # referrals_201.csv, X in 1:9

# Outputs:
output_quality = "W:/ASMA1/data/cleaned/visits_quality.rds"

###
###


# We will loop over the following years:
years = as.character(c(1:9))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) GP visits in public primary care. ####
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
  
  # Read FOLK data:
  source_folk = paste(input_folk, year, ".csv", sep="")
  folk = data.table::fread(source_folk, select=c('id', 'municipality'))
  
  # Merge 'folk' to 'visits':
  visits = merge(visits, folk, by='id', all.x=TRUE)
  visits = visits[year==yr & !is.na(municipality)][, id := NULL]
  print("Merging municipalities of residence to visits is done")
  
  # Aggregate at the municipality-year-month level:
  muni = visits[, .(visits = .N), by=c('municipality','year','month')]
  
  # Aggregate FOLK data to population stats:
  folk = folk[, .(population = .N), by='municipality'][, year := yr]
  
  # Merge 'folk' to 'muni':
  muni = merge(muni, folk, by=c('municipality','year'), all=TRUE)
  
  rm(visits)
  
  return(muni)
  
})
pc_visits = do.call(rbind.data.frame, pc_visits)
pc_visits = pc_visits[, mget(colnames(pc_visits))]

pc_visits[, ann_visits_per_capita := 12 * visits / population]
pc_visits[, visits := NULL]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Referrals to specialist consultations. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read referrals to specialist consultations:

refs = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read referrals to specialist consultations:
  source_refs = paste(input_refs, year, ".csv", sep="")
  df = data.table::fread(source_refs, drop=c('discharged'))
  
  # Take those rows where we observe the contact start date and where the
  # referral is written in public primary care:
  df = df[contact_started==1 & ref_origin==1
          ][, ':=' (contact_started = NULL, ref_origin = NULL)]
  
  # Take unique id-date pairs. More than one referrals could be written
  # on each day, but in our analyses we create a dummy on whether an 
  # indivual X received any referrals on date Y:
  df = unique(df)
  
    # Read FOLK data:
  source_folk = paste(input_folk, year, ".csv", sep="")
  folk = data.table::fread(source_folk, select=c('id', 'municipality'))
  
  # Merge 'folk' to 'df':
  df = merge(df, folk, by='id', all.x=TRUE)
  df = df[year==yr & !is.na(municipality)][, id := NULL]
  print("Merging municipalities of residence to referrals is done")
  
  # Aggregate at the municipality-year-month level:
  muni = df[, .(refs = .N), by=c('municipality','year','month')]
  
  rm(df)
  
  return(muni)
  
})
refs = do.call(rbind.data.frame, refs)


# Merge 'refs' to 'pc_visits':
df = merge(pc_visits, refs, by=c('municipality', 'month', 'year'), all.x = TRUE)
df[is.na(refs), refs := 0]
df[, ':=' (ann_refs_per_capita = 12 * refs / population)]
df[, refs := NULL]

saveRDS(df, output_quality)

# End. 
