# ESTIMATE TREATMENT EFFECTS FOR CAREGIVERS BY DURATION AND INTENSITY

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



## CAREGIVING DURATION
######
### Short duration
print(regspec <- regspec_caredur)
dat <- dat_event %>% filter(rrscare_i_pre==1 & rrscaredur2_pre==FALSE)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['< 2 waves of caregiving']]$attet <- attet

rm(attet, attg)
rm(dat, regspec)
###

### Long duration
print(regspec <- regspec_caredur)
dat <- dat_event %>% filter(rrscare_i_pre==1 & rrscaredur2_pre==TRUE)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['2+ waves of caregiving']]$attet <- attet

rm(attg, attet)
rm(dat, regspec)
###
######



## CAREGIVING INTENSITY
############
dat_event %>% filter(et==-1) %>% 
  group_by(rrscarehpw_qtile) %>% 
  summarise(n(), min(rrscarehpw_i_pre), max(rrscarehpw_i_pre))
dat_event %>% filter(et==-1) %>% 
  group_by(rrscarehpw_grp_pre) %>% 
  summarise(n(), min(rrscarehpw_i_pre), max(rrscarehpw_i_pre))

### Low intensity
print(regspec <- regspec_careint)
dat <- dat_event %>% filter(rrscarehpw_qtile%in%c("2", "3", "4"))

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Low-intensity: <31 hrs']]$attet <- attet

rm(attg, attet)
rm(dat)
###

### High intensity
print(regspec <- regspec_careint)
dat <- dat_event %>% filter(rrscarehpw_qtile%in%c("5", "6", "7"))

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['High-intensity: 31+ hrs']]$attet <- attet

rm(attg, attet)
rm(dat)
###
############



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
save(EST.ATT.CESD, file = paste0(dir_working, "04estimation/output/event_scesd_caregiving.rda"))

### Save estimation output
save(ESTOUT, file = paste0(dir_working, "04estimation/output/estout_caregiving.rda"))

rm(EST.ATT.CESD, ESTOUT)

rm(regspec_noncare, regspec_caregive)





