# LOAD REQUIRED PACKAGES
require(tidyverse); require(haven); require(did)

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
# # HRS data (constructed from running ConstructSample.R)
# load(paste0(dir_working, "01constructdata/dat_hrs_prep_v2.rda"))

# Estimation sample
load(paste0(dir_working, "03preanalysis/samp_event.rda"))
######

source(paste0(dir_working, "04estimation/regspecs.R"))

ESTOUT <- list()

### POOLED SAMPLE
######
window_min <- -2
window_max <- 1
test_min <- -2

print(regspec <- regspec_main)
dat <- dat_event

source(paste0(dir_working, "04estimation/pre_attgt_scesd.R"))
summary(attg_pre)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Pooled']]$attet <- attet
ESTOUT[['Pooled']]$attgpre <- attg_pre
######
rm(attg_pre, attet, attg)
rm(dat)



### EFFECT BY CAREGIVING STATUS
############
window_min <- -2
window_max <- 1
test_min <- -2

### Non-caregivers
print(regspec <- regspec_noncare)
dat <- dat_event %>% filter(rrscare_i_pre==0)

source(paste0(dir_working, "04estimation/pre_attgt_scesd.R"))
summary(attg_pre)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)
# ggdid(attet)

ESTOUT[['Non-caregivers']]$attet <- attet
ESTOUT[['Non-caregivers']]$attgpre <- attg_pre

rm(attg_pre, attet, attg)
rm(dat)
###

### Caregivers
print(regspec <- regspec_caregive)
dat <- dat_event %>% filter(rrscare_i_pre==1)

source(paste0(dir_working, "04estimation/pre_attgt_scesd.R"))
summary(attg_pre)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)
# ggdid(attet)

ESTOUT[['Caregivers']]$attet <- attet
ESTOUT[['Caregivers']]$attgpre <- attg_pre

rm(attg_pre, attet, attg)
rm(dat)
############



########################
# Extract results as tables
EST.ATT.CESD <- lapply(ESTOUT, function(x) {
  bind_cols(et = x$attet$egt,
            att = x$attet$att.egt,
            se = x$attet$se.egt,
            ci.lo = x$attet$att.egt - x$attet$crit.val.egt*x$attet$se.egt,
            ci.hi = x$attet$att.egt + x$attet$crit.val.egt*x$attet$se.egt) %>%
    mutate(depvar_pre = x$attet$DIDparams$data %>% filter(wave == treatwave-1) %>% summarise(mean(r.cesd)) %>% as.numeric()) %>%
    mutate(relchg = att/depvar_pre) %>%
    mutate(Treated = as.integer(x$attet$DIDparams$n/2)) %>%
    mutate(Observations = nrow(x$attet$DIDparams$data))})

for(x in names(ESTOUT)) {
  EST.ATT.CESD[[x]] <- EST.ATT.CESD[[x]] %>%
    mutate(Wald = as.numeric(ESTOUT[[x]]$attgpre$W)) %>%
    mutate(pval = as.numeric(ESTOUT[[x]]$attgpre$Wpval))
}

save(EST.ATT.CESD, file = paste0(dir_working, "04estimation/output/event_scesd_mainresults.rda"))

# Save estimation output as R data file
save(ESTOUT, file = paste0(dir_working, "04estimation/output/estout_mainresults.rda"))
########################
rm(EST.ATT.CESD, ESTOUT)



ESTOUT <- list()

## EFFECT BY CAREGIVING DURATION
############
window_min <- -2
window_max <- 0
# test_min <- -2

### Short duration
print(regspec <- regspec_caredur)
dat <- dat_event %>% filter(rrscare_i_pre==1 & rrscaredur2_pre==FALSE)

# source(paste0(dir_working, "04estimation/pre_attgt_scesd.R"))
# summary(attg_pre)
source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['< 2 waves of caregiving']]$attet <- attet
ESTOUT[['< 2 waves of caregiving']]$attgpre <- attg_pre

rm(attet, attg)
rm(dat)
###

### Long duration
print(regspec <- regspec_caredur)
dat <- dat_event %>% filter(rrscare_i_pre==1 & rrscaredur2_pre==TRUE)

# source(paste0(dir_working, "04estimation/pre_attgt_scesd.R"))
# summary(attg_pre)
source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['2+ waves of caregiving']]$attet <- attet

rm(attg, attet)
rm(dat)
###
############



## EFFECT BY CAREGIVING INTENSITY
############
window_min <- -2
window_max <- 0
test_min <- -2


dat_event %>% filter(et==-1) %>% 
  group_by(rrscarehpw_qtile) %>% 
  summarise(n(), min(rrscarehpw_i_pre), max(rrscarehpw_i_pre))
dat_event %>% filter(et==-1) %>% 
  group_by(rrscarehpw_grp_pre) %>% 
  summarise(n(), min(rrscarehpw_i_pre), max(rrscarehpw_i_pre))

### Low intensity
print(regspec <- regspec_careint)
dat <- dat_event %>% filter(rrscarehpw_qtile%in%c("2", "3", "4"))
# dat <- dat_event %>% filter(rrscarehpw_grp_pre%in%c("2"))

# source(paste0(dir_working, "04estimation/pre_attgt_scesd.R"))
# summary(attg_pre)
source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Low-intensity: <31 hrs']]$attet <- attet

rm(attg, attet)
rm(dat)
###

### High intensity
print(regspec <- regspec_careint)
dat <- dat_event %>% filter(rrscarehpw_qtile%in%c("5", "6", "7"))
# dat <- dat_event %>% filter(rrscarehpw_grp_pre%in%c("2"))
# dat <- dat_event %>% filter(rrscare_i_pre ==1 & rrscarehpw_hi_pre==1)

# source(paste0(dir_working, "04estimation/pre_attgt_scesd.R"))
# summary(attg_pre)
source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['High-intensity: 31+ hrs']]$attet <- attet

rm(attg, attet)
rm(dat)
###
############


########################
# Extract results as tables
EST.ATT.CESD <- lapply(ESTOUT, function(x) {
  bind_cols(et = x$attet$egt,
            att = x$attet$att.egt,
            se = x$attet$se.egt,
            ci.lo = x$attet$att.egt - x$attet$crit.val.egt*x$attet$se.egt,
            ci.hi = x$attet$att.egt + x$attet$crit.val.egt*x$attet$se.egt) %>%
    mutate(depvar_pre = x$attet$DIDparams$data %>% filter(wave == treatwave-1) %>% summarise(mean(r.cesd)) %>% as.numeric()) %>%
    mutate(relchg = att/depvar_pre) %>%
    mutate(Treated = as.integer(x$attet$DIDparams$n/2)) %>%
    mutate(Observations = nrow(x$attet$DIDparams$data))})

save(EST.ATT.CESD, file = paste0(dir_working, "04estimation/output/event_scesd_caregiving.rda"))

# Save estimation output as R data file
save(ESTOUT, file = paste0(dir_working, "04estimation/output/estout_caregiving.rda"))
########################
rm(EST.ATT.CESD, ESTOUT)



### EFFECT BY SUBGROUPS
############
ESTOUT <- list()

window_min <- -2
window_max <- 0
# test_min <- -2


# PHYSICAL IMPAIRMENT
######
# <2 ADL
print(regspec <- regspec_adl)
dat <- dat_event %>% filter(radlcount_pre<2)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['<2 ADL']]$attet <- attet

rm(attg, attet)
rm(dat)

# 2+ ADL
print(regspec <- regspec_adl)
dat <- dat_event %>% filter(radlcount_pre>=2)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['2+ ADL']]$attet <- attet

rm(attg, attet)
rm(dat)
######


# COGNITIVE FUNCTION
######
# NO COGNITIVE IMPAIRMENT
print(regspec <- regspec_cogfunction)
dat <- dat_event %>% filter(rimpaired_i_pre==0)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['No cognitive impairment']]$attet <- attet

rm(attg, attet)
rm(dat)

# COGNITIVELY IMPAIRED
print(regspec <- regspec_cogfunction)
dat <- dat_event %>% filter(rimpaired_i_pre==1)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Cognitively impaired']]$attet <- attet

rm(attg, attet)
rm(dat)
######


# AD DIAGNOSIS
######
# NO DIAGNOSIS
print(regspec <- regspec_memrye)
dat <- dat_event %>% filter(rmemrye_i_pre==0)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['No AD diagnosis']]$attet <- attet

rm(attg, attet)
rm(dat)

# AD DIAGNOSED
print(regspec <- regspec_memrye)
dat <- dat_event %>% filter(rmemrye_i_pre==1)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['AD diagnosed']]$attet <- attet

rm(attg, attet)
rm(dat)
######


# GENDER
######
# MALE
print(regspec <- regspec_gender)
dat <- dat_event %>% filter(safemale_pre==0)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Male respondents']]$attet <- attet

rm(attg, attet)
rm(dat)

# FEMALE
print(regspec <- regspec_gender)
dat <- dat_event %>% filter(safemale_pre==1)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Female respondents']]$attet <- attet

rm(attg, attet)
rm(dat)
######

############



########################
# Extract results as tables
EST.ATT.CESD <- lapply(ESTOUT, function(x) {
  bind_cols(et = x$attet$egt,
            att = x$attet$att.egt,
            se = x$attet$se.egt,
            ci.lo = x$attet$att.egt - x$attet$crit.val.egt*x$attet$se.egt,
            ci.hi = x$attet$att.egt + x$attet$crit.val.egt*x$attet$se.egt) %>%
    mutate(depvar_pre = x$attet$DIDparams$data %>% filter(wave == treatwave-1) %>% summarise(mean(r.cesd)) %>% as.numeric()) %>%
    mutate(relchg = att/depvar_pre) %>%
    mutate(Treated = as.integer(x$attet$DIDparams$n/2)) %>%
    mutate(Observations = nrow(x$attet$DIDparams$data))})


save(EST.ATT.CESD, file = paste0(dir_working, "04estimation/output/event_scesd_subgroups.rda"))

# Save estimation output as R data file
save(ESTOUT, file = paste0(dir_working, "04estimation/output/estout_subgroups.rda"))
########################
rm(EST.ATT.CESD, ESTOUT)



rm(regspec_noncare, regspec_caregive)
rm(regspec_caredur, regspec_careint, 
   regspec_gender, regspec_memrye, regspec_adl, regspec_cogfunction)




