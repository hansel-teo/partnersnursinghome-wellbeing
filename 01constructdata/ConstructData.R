require(tidyverse); require(haven)
require(readxl)

dir_working <- 

setwd(dir_working)


# EXTRACT VARIABLES AND COMPILE WORKING DATASET
######
dir_input <- 

### FILENAMES
filenames <- list()
filenames["rand"] <- "randhrs1992_2018v2.dta"
filenames["dimp"] <- "randhrsimp1992_2018v2.dta"
filenames["harm"] <- "H_HRS_c.dta"

### VARIABLE NAMES
source(paste0(dir_working, "01constructdata/varnames.R"))

# Extract variables
source(paste0(dir_working, "01constructdata/extract.R"), verbose = TRUE)
gc()

# Extract and merge cognitive functioning variables
source(paste0(dir_working, "01constructdata/merge_cogfunction.R"), verbose = TRUE)
gc()

# Extract and merge CPI variables
source(paste0(dir_working, "01constructdata/merge_cpi.R"), verbose = TRUE)
detach("package:readxl", unload = TRUE)
gc()

# Save data frame as R data file
save(dat, file = paste0(dir_working, "01constructdata/dat_hrs_v2.rda"))
######



# CLEAN DATA AND DERIVE VARIABLES
######
load(paste0(dir_working, "/01constructdata/dat_hrs_v2.rda"))

dat %>% group_by(wave) %>% summarise(n(), sum(!is.na(riwstat)))
dat %>% select(wave, riwstat) %>% table(useNA="always")

# Save balanced panel index of HHIDPN and RIWSTAT
dat_index_bal <- dat %>% select(hhidpn, wave, riwstat)

# Save data frame as R data file
save(dat_index_bal, file = paste0(dir_working, "01constructdata/dat_index.rda"))

# Version of data with NA hhidpn-waves dropped (non-balanced panel)
dat_hrs <- dat %>% filter(riwstat !=0)

gc()
source(paste0(dir_working, "01constructdata/prep_data.R"), verbose = TRUE)

colnames(dat_hrs)

dat_hrs %>% select(riwstat) %>% table(useNA="always")
dat_hrs <- dat_hrs %>% filter(riwstat !=7)
dat_hrs <- dat_hrs %>% filter(riwstat !=9)

# Save data frame as R data file
save(dat_hrs, file = paste0(dir_working, "01constructdata/dat_hrs_prep_v2.rda"))
######
rm(dat)



# CONSTRUCT BALANCED PANEL OF OF LEADS/LAGS FOR SAMPLE SELECTION
######
gc()
source(paste0(dir_working, "01constructdata/panel_leadslags.R"), verbose = TRUE)

save(lgrid, file = paste0(dir_working, "01constructdata/vars_leadslags.rda"))
######



rm(lgrid, dat_hrs, dat_index_bal)
rm(dir_root, dir_working)

