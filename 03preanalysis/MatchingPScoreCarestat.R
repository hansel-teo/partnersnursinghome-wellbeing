# LOAD REQUIRED PACKAGES
require(tidyverse); require(haven); require(MatchIt)


# PATHS TO DATA AND WORKING DIRECTORY
######
# # Home
# dir_root <- "/Users/hanselteo/Data/"
# dir_working <- paste0(dir_root, "nhspouse_replication/")

# Work
dir_root <- "E:/My Drive/" 
dir_working <- paste0(dir_root, "projects/nhspouse_wellbeing/")

setwd(dir_working)
######


# LOAD DATA
######
# Candidate treated and never treated
load(paste0(dir_working, "02selectsample/samp_pre.rda"))

######



# NEAREST-NEIGHBOUR MATCHING ON PROPENSITY SCORES BY CAREGIVING GROUPS
############
source(paste0(dir_working, "03preanalysis/", "matching_pscore_caregive.R"), verbose=TRUE)

source(paste0(dir_working, "03preanalysis/", "matching_pscore_noncare.R"), verbose=TRUE)

dat_matched1 %>% select(treated, wave) %>% table(useNA='ifany')
dat_matched0 %>% select(treated, wave) %>% table(useNA='ifany')

identical(colnames(dat_matched0), colnames(dat_matched1))

dat_matched <- bind_rows(dat_matched0, dat_matched1)

dat_matched %>% select(treated) %>% table(useNA='ifany')

rm(dat_matched0, dat_matched1)
############


# CONSTRUCT ESTIMATION SAMPLE
############
# Leads/lags of status
load(paste0(dir_working, "01constructdata/vars_leadslags.rda"))

# HRS data (constructed from running ConstructSample.R)
load(paste0(dir_working, "01constructdata/dat_hrs_prep_v2.rda"))

source(paste0(dir_working, "03preanalysis/", "samp_matched.R"), verbose=TRUE)

# Save data frame as R data file
save(dat_event, file = paste0(dir_working, "03preanalysis/samp_event.rda"))
############

rm(lgrid)
rm(dat_hrs)
rm(dat_matched)

rm(samp_pre)

