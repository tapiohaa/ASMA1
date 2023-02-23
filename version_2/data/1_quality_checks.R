
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         r-script 1_quality_checks.R           ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Inspect the quality of the primary care data.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.

# Inputs:
input_quality = "W:/ASMA1/data/cleaned/visits_quality.rds"

# Outputs:
output_transfer_small = "W:/ASMA1/analysis/figures/visits_quality_small.pdf"
output_changes = "W:/ASMA1/data/intermediate/visits_changes.rds"
output_refs_muni = "W:/ASMA1/analysis/figures/refs_quality_muni.pdf"

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Tidy the data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# The following 23 municipalities are our sample:
sample_munies = 
  c(91, 49, 837, 564, 853, 179, 398, 297, 609, 167, 405, 109, 743, 698,
    491, 734, 638, 272, 444, 257, 684, 858, 245)

df = CJ(municipality=sample_munies, year=c(2011:2019), month=c(1:12))

# Read data on those municipalies:
quality = readRDS(input_quality)
df = merge(df, quality, by=c('municipality', 'month', 'year'), all.x = TRUE)

# We should have a balanced panel:
nrow(df)
years = c(2011:2019)
length(years)*12*23
 
# Impute relative time to the data frame:

time = data.table(
  month = rep(c(1:12), times=length(years)),
  year = rep(years, each=12))
time$relative_time = c(1:nrow(time))

df = merge(df, time, by=c('year','month'))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) GP visits in public primary care. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Analyse whether some areas have had problems in transferring data

# It is a known issue that in early years of AvoHilmo primary care data 
# collection some areas had problems in transferring data from their IT systems 
# to the national register. This results in very low numbers of visits per 
# capita for some periods (note that this figure does not need to be exactly 
# zero as residents may also have used services in other municipalities). Here, 
# we try to discover those municipality-period pairs so that these 
# municipalities can be dropped from analysis data.


# We will proceed in the following way:
# 	1) For each municipality, compute a distribution of mean contacts
#			by dropping every combination of four consequtive months.
#	2) Take the largest mean.
#	3) Mark an observation "weirdly low" if it is less than 40 % of the mean.

months = max(df$relative_time) - 3
municipalities = unique(df$municipality)
combinations = lapply(c(1:months), function(int) { vec = c(0:3) + int })


means = lapply(municipalities, function(muni) {
  
  avr = lapply(combinations, function(combo) {
    data = df[municipality==muni & !(relative_time %in% combo), 
              .(avr = mean(ann_visits_per_capita))]
  })
  avr = max(unlist(avr))
  
  data = data.table(municipality=muni, avr_largest=avr)
  
})
means = do.call(rbind.data.frame, means)


# Merge 'means' to 'df' and find 'weirdly' low values:
DT = merge(df, means, by='municipality', all.x=TRUE)
DT[, weird := (ann_visits_per_capita - avr_largest) / avr_largest]
weird_60 = unique(DT[weird < -0.6 & month != 7]$municipality)


# Plot the "weird municipalities":

DT[, muni_f := as.factor(municipality)]

ggplot(data=DT[municipality %in% weird_60],
       aes(x=relative_time, y=ann_visits_per_capita)) +
  geom_line() +
  facet_grid(~ muni_f)


# Based on the above plots, our assessment is that the following 
#   municipality-year pairs should be dropped from analysis data. We
#   highlight them in figures by red.

drop = list(
  data.table(municipality=109, year=2012),
  data.table(municipality=109, year=2013),
  data.table(municipality=297, year=2011),
  data.table(municipality=297, year=2012),
  data.table(municipality=638, year=2012),
  data.table(municipality=684, year=2015),
  data.table(municipality=684, year=2016),
  data.table(municipality=698, year=2011),
  data.table(municipality=734, year=2018)
)
drop = do.call(rbind.data.frame, drop)
drop$drop_transfer = 1

DT = merge(DT, drop, by=c('municipality','year'), all.x = TRUE)
DT[is.na(drop_transfer), drop_transfer := 0]


# First, draw plots of each of the 'weird' municipality-year pairs separately:

plots = lapply(weird_60, function(muni) {
  
  # Initiate a plot:
  
  p = ggplot2::ggplot() +
    scale_x_continuous(breaks = seq(1, 129, by=24),
                       labels = as.character(c(11, 13, 15, 17, 19, 21))) + 
    ggtitle(paste("No.", as.character(muni), sep=' ')) + 
    ylim(0, 1.8) +
    theme(text = element_text(size=19),
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  
  # Highlight by grey background those municipality-year pairs that contain 
  # weird months:
  
  df = DT[municipality == muni & drop_transfer == 1 & month %in% c(1, 12), 
          mget(c('year', 'relative_time')) ]
  
  if(nrow(df) > 0) {
    df = df[, .(min = min(relative_time),
                max = max(relative_time)), by='year']
    
    p = p +
      geom_rect(data = df, 
                mapping = aes(xmin=min, xmax=max, ymin=-Inf, ymax=Inf, 
                              alpha=0.5), fill='grey') +
      guides(alpha = 'none')
    
  }
  
  
  # Finally, plot the evolution of GP visits:
  
  p = p +
    geom_line(data = DT[municipality %in% muni], 
              mapping = aes(x=relative_time, y=ann_visits_per_capita),
              colour="black")
  
  return(p)
  
})


# Save:

# A plot showing data on 'weird' municipalities separately (w.r.t. transfer):

cairo_pdf(filename = output_transfer_small, width = 16.0, height = 8.0)
print( patchwork::wrap_plots(plots, ncol=4, byrow=TRUE) )
dev.off()

# Data on municipality-year pairs to be dropped:

cols = c('municipality', 'year', 'month', 'drop_transfer')
DT = DT[, mget(cols)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Referrals to specialist consultations. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# One municipality has a large spike in one month (Rauma: 12/2011). We drop
# that observation:
#df = df[!(municipality==684 & month==12 & year==2011)]

DT[, drop_refs := as.integer(municipality==684 & month==12 & year==2011)]
saveRDS(DT, output_changes)


# At the municipal level:

plots.refs = lapply(sample_munies, function(muni) {
  
  # Initiate a plot:
  
  p = ggplot2::ggplot() +
    geom_line(data = df[municipality %in% muni], 
              mapping = aes(x=relative_time, y=ann_refs_per_capita),
              colour="black") + 
    scale_x_continuous(breaks = seq(1, 129, by=24),
                       labels = as.character(c(11, 13, 15, 17, 19, 21))) + 
    ggtitle(paste("No.", as.character(muni), sep=' ')) + 
    ylim(0, 0.30) +
    theme(text = element_text(size=19),
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  return(p)
  
})

# Save:

cairo_pdf(filename = output_refs_muni, width = 15.0, height = 10.0)
print( patchwork::wrap_plots(plots.refs, ncol=6, byrow=TRUE) )
dev.off()

# End.
