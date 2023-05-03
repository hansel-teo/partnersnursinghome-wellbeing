# ESTIMATE TREATMENT EFFECTS BY SUBGROUPS

### LOAD REQUIRED PACKAGES
require(tidyverse); require(haven); require(did)


### PATHS TO DATA AND WORKING DIRECTORY
# dir_root <- "/Users/hanselteo/Data/" # Home
dir_root <- "E:/My Drive/" # Work
dir_working <- paste0(dir_root, "projects/nhspouse_wellbeing/")

setwd(dir_working)


### LOAD ESTIMATION SAMPLE
load(paste0(dir_working, "03preanalysis/samp_event.rda"))

### LOAD REGRESSION SPECIFICATIONS
source(paste0(dir_working, "04estimation/regspecs.R"))


ESTOUT <- list()

window_min <- -2
window_max <- 0

## PHYSICAL IMPAIRMENT
######
### <2 ADL
print(regspec <- regspec_adl)
dat <- dat_event %>% filter(radlcount_pre<2)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['<2 ADL']]$attet <- attet

rm(attg, attet)
rm(dat, regspec)

### 2+ ADL
print(regspec <- regspec_adl)
dat <- dat_event %>% filter(radlcount_pre>=2)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['2+ ADL']]$attet <- attet

rm(attg, attet)
rm(dat, regspec)
######



## COGNITIVE FUNCTION
######
### No cognitive impairment
print(regspec <- regspec_cogfunction)
dat <- dat_event %>% filter(rimpaired_i_pre==0)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['No cognitive impairment']]$attet <- attet

rm(attg, attet)
rm(dat, regspec)

### Cognitively impaired
print(regspec <- regspec_cogfunction)
dat <- dat_event %>% filter(rimpaired_i_pre==1)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Cognitively impaired']]$attet <- attet

rm(attg, attet)
rm(dat, regspec)
######




## AD DIAGNOSIS
######
### No AD diagnosis
print(regspec <- regspec_memrye)
dat <- dat_event %>% filter(rmemrye_i_pre==0)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['No AD diagnosis']]$attet <- attet

rm(attg, attet)
rm(dat, regspec)

### AD diagnosed
print(regspec <- regspec_memrye)
dat <- dat_event %>% filter(rmemrye_i_pre==1)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['AD diagnosed']]$attet <- attet

rm(attg, attet)
rm(dat, regspec)
######



## RESPONDENT GENDER
######
### Male
print(regspec <- regspec_gender)
dat <- dat_event %>% filter(safemale_pre==0)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Male respondents']]$attet <- attet

rm(attg, attet)
rm(dat, regspec)

### Female
print(regspec <- regspec_gender)
dat <- dat_event %>% filter(safemale_pre==1)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Female respondents']]$attet <- attet

rm(attg, attet)
rm(dat, regspec)
######



### Extract results as tables
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

### Save compiled estimates
save(EST.ATT.CESD, file = paste0(dir_working, "04estimation/output/event_scesd_subgroups.rda"))

### Save estimation output
save(ESTOUT, file = paste0(dir_working, "04estimation/output/estout_subgroups.rda"))

rm(EST.ATT.CESD, ESTOUT)

rm(regspec_gender, regspec_memrye, regspec_adl, regspec_cogfunction)




