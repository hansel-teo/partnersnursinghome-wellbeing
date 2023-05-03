# ESTIMATE TREATMENT EFFECTS FOR POOLED SAMPLE AND BY CAREGIVING STATUS

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
window_max <- 1
test_min <- -2



## POOLED SAMPLE
######
print(regspec <- regspec_main)
dat <- dat_event

source(paste0(dir_working, "04estimation/pre_attgt_scesd.R"))
summary(attg_pre)

source(paste0(dir_working, "04estimation/event_scesd.R"), verbose=TRUE)
summary(attet)

ESTOUT[['Pooled']]$attet <- attet
ESTOUT[['Pooled']]$attgpre <- attg_pre

rm(attg_pre, attet, attg)
rm(dat, regspec)
######


## CAREGIVING STATUS
######
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
rm(dat, regspec)
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

for(x in names(ESTOUT)) {
  EST.ATT.CESD[[x]] <- EST.ATT.CESD[[x]] %>%
    mutate(Wald = as.numeric(ESTOUT[[x]]$attgpre$W)) %>%
    mutate(pval = as.numeric(ESTOUT[[x]]$attgpre$Wpval))
}

### Save compiled estimates
save(EST.ATT.CESD, file = paste0(dir_working, "04estimation/output/event_scesd_mainresults.rda"))

### Save estimation output
save(ESTOUT, file = paste0(dir_working, "04estimation/output/estout_mainresults.rda"))

rm(EST.ATT.CESD, ESTOUT)