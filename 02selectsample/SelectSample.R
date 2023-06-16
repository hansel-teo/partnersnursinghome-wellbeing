require(tidyverse); require(haven)

# PATHS TO DATA AND WORKING DIRECTORY
######
dir_working <- 

setwd(dir_working)
######


# LOAD DATA
######
# HRS data (constructed from running ConstructSample.R)
load(paste0(dir_working, "01constructdata/dat_hrs_prep_v2.rda"))

# # HHIDPN-Wave survey status index
# load(paste0(dir_working, "01constructdata/dat_index.rda"))

# Load panel of leads/lags
load(paste0(dir_working, "01constructdata/vars_leadslags.rda"))

######


# DEFINE TREATMENT VARIABLE
######
vars_tr <- c("rnrshom", "rnrshom_e", "rnhmliv", "rnhmliv_e")

source(paste0(dir_working, "02selectsample/define_treatindex.R"))

vars_tr_nrshom %>%
  group_by(wave) %>%
  summarise(`First spells` = sum(rnrshom_e_wv1==wave),
            `Second spells` = sum(rnrshom_e_wv2==wave, na.rm=TRUE),
            `Third spells` = sum(rnrshom_e_wv3==wave, na.rm=TRUE))

# Save data frame as R data file
save(vars_tr_nrshom, file = paste0(dir_working, "02selectsample/temp/vars_treatindex.rda"))
######

load(paste0(dir_working, "02selectsample/temp/vars_treatindex.rda"))


depvarsin <- c("rcesd")


####################################
# BASELINE SAMPLE
####################################

# 0. ANALYSIS SAMPLE PARAMETERS
###
window_min <- -1
window_max <- 0
###


# 1. IDENTIFY CANDIDATE TREATEDS
############
gc()
source(paste0(dir_working, "02selectsample/sampselect_treated.R"), verbose = TRUE)

save(select_treated, file = paste0(dir_working, "02selectsample/temp/samp_treated.rda"))

############


# 2. IDENTIFY CANDIDATE NEVER-TREATEDS
############
gc()
source(paste0(dir_working, "02selectsample/sampselect_nevtreat.R"), verbose = TRUE)

save(select_nevtreat, file = paste0(dir_working, "02selectsample/temp/samp_nevtreat.rda"))

############


# 3. CANDIDATE TREATED AND NEVER-TREATED FROM SAMPLE SELECTION STEPS
############
# Loads output from previous step (to avoid repeating)
# load(paste0(dir_working, "02selectsample/temp/samp_treated.rda"))
# load(paste0(dir_working, "02selectsample/temp/samp_nevtreat.rda"))

# Names of variables required for analysis
source(paste0(dir_working, "02selectsample/modvarnames.R"), verbose=TRUE)

source(paste0(dir_working, "02selectsample/samp_prematch.R"), verbose=TRUE)

save(samp_pre, file = paste0(dir_working, "02selectsample/samp_pre.rda"))
############


# 4. 
############
gc()
source(paste0(dir_working, "03preanalysis/durations.R"), verbose=TRUE)

gc()
samp_pre %>%
  group_by(treated) %>%
  summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn))

samp_pre <- samp_pre %>% filter(!is.na(rrscare.lag1))

samp_pre %>%
  select(rrscaredur1, rrscaredur2, rrscare_i) %>% 
  table(useNA="ifany")

samp_pre %>%
  group_by(treated) %>%
  summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn))

save(samp_pre, file = paste0(dir_working, "02selectsample/samp_pre.rda"))
############

rm(lgrid)

