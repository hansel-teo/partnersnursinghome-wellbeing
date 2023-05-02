# INDIVIDUAL, HOUSEHOLD AND SPOUSE IDENTIFIERS
##############################
dat <- read_dta(file = paste0(dir_input, filenames[['rand']]),
                col_select = c(hhidpn, hhid, pn,
                               matches(paste0(names(vars_id)[3],"$"))))

dat <- dat %>% 
  pivot_longer(cols = -c(hhidpn, hhid, pn),
               names_to = c("wave"),
               names_pattern = "h(\\d+\\d*)",
               values_to = "hhhid")

dat1 <- read_dta(file = paste0(dir_input, filenames[['rand']]),
                 col_select = c(hhidpn, 
                                matches(paste0(names(vars_id)[4],"$"))))

dat1 <- dat1 %>% 
  pivot_longer(cols = -c(hhidpn),
               names_to = c("wave"),
               names_pattern = "s(\\d+\\d*)",
               values_to = "shhidpn")

dat <- left_join(dat, dat1, by = c("hhidpn", "wave"))

rm(dat1)
rm(vars_id)
##############################


# INDIVIDUAL-LEVEL VARIABLES FROM RAND LONGITUDINAL
##############################
varnames <- c(names(vars_resp), names(vars_demog), 
              names(vars_insur), names(vars_nrshom), 
              names(vars_med), names(vars_hlth), 
              names(vars_exp), 
              names(vars_cesd))
varnames <- c(varnames,
              paste0(names(vars_adl), "a"),
              paste0(names(vars_iadl), "a"),
              paste0(names(vars_mobl), "a"))


dat1 <- read_dta(file = paste0(dir_input, filenames[['rand']]),
                 col_select = c(hhidpn, matches(paste0("^r([a]|[e]|\\d+\\d*)", varnames, "$"))))

dat1 <- dat1 %>% 
  pivot_longer(cols = -c(hhidpn, starts_with("ra"), starts_with("re")),
               names_to = c("wave", ".value"),
               names_pattern = "r(\\d+\\d*)(.*)")

dat1 <- dat1 %>% 
  rename_with(.cols = -c(hhidpn, wave, starts_with("ra"), starts_with("re")),
              .fn = ~ paste0("r",.))

# colnames(dat1)

dat <- left_join(dat, dat1, by = c("hhidpn", "wave"))

rm(dat1)
rm(varnames)
rm(vars_resp, vars_demog, vars_insur, vars_nrshom, vars_med, vars_hlth, vars_exp, vars_cesd, vars_adl, vars_iadl, vars_mobl)
##############################


# INDIVIDUAL-LEVEL VARIABLES FROM HARMONIZED HRS
##############################
varnames <- c(names(vars_acute), names(vars_supp),
              names(vars_spcare), names(vars_incare), names(vars_focare))

dat1 <- read_dta(file = paste0(dir_input, filenames[['harm']]),
                 col_select = c(hhidpn, matches(paste0("^r\\d+\\d*", varnames))))
dat1 <- dat1 %>% 
      pivot_longer(cols = -hhidpn,
                   names_to = c("wave", ".value"),
                   names_pattern = "r(\\d+\\d*)(.*)")

dat1 <- dat1 %>% 
  rename_with(.cols = -c(hhidpn, wave),
              .fn = ~ paste0("r",.))

# colnames(dat1)

dat <- left_join(dat, dat1, by = c("hhidpn", "wave"))

rm(dat1)
rm(varnames)
rm(vars_spcare, vars_incare, vars_focare, vars_supp, vars_acute)
##############################


# INDIVIDUAL-LEVEL VARIABLES FROM RAND DETAILED IMPUTATIONS FILE
##############################
varnames <- c(names(vars_mds))

dat1 <- read_dta(file = paste0(dir_input, filenames[['dimp']]),
                 col_select = c(hhidpn, matches(paste0("^r([e]|\\d+\\d*)", varnames))))
dat1 <- dat1 %>% 
  pivot_longer(cols = -c(hhidpn, starts_with("re")),
               names_to = c("wave", ".value"),
               names_pattern = "r(\\d+\\d*)(.*)")

dat1 <- dat1 %>% 
  rename_with(.cols = -c(hhidpn, wave, starts_with("re")),
              .fn = ~ paste0("r",.))

# colnames(dat1)

dat <- left_join(dat, dat1, by = c("hhidpn", "wave"))

rm(dat1)

dat1 <- read_dta(file = paste0(dir_input, filenames[['dimp']]),
                 col_select = c(hhidpn, matches(paste0("^s\\d+\\d*", varnames))))
dat1 <- dat1 %>% 
  pivot_longer(cols = -c(hhidpn),
               names_to = c("wave", ".value"),
               names_pattern = "s(\\d+\\d*)(.*)")

dat1 <- dat1 %>% 
  rename_with(.cols = -c(hhidpn, wave, starts_with("re")),
              .fn = ~ paste0("s",.))

# colnames(dat1)

dat <- left_join(dat, dat1, by = c("hhidpn", "wave"))

rm(dat1)
rm(varnames)
rm(vars_mds)
##############################

# HOUSEHOLD-LEVEL VARIABLES FROM RAND LONGITUDINAL
##############################
varnames <- names(vars_hhold)

dat1 <- read_dta(file = paste0(dir_input, filenames[['rand']]),
                 col_select = c(hhidpn, matches(paste0("^h\\d+\\d*", varnames, "$"))))

dat1 <- dat1 %>% 
  pivot_longer(cols = -hhidpn,
               names_to = c("wave", ".value"),
               names_pattern = "h(\\d+\\d*)(.*)")

dat1 <- dat1 %>% 
  rename_with(.cols = -c(hhidpn, wave),
              .fn = ~ paste0("h",.))

# colnames(dat1)

dat <- left_join(dat, dat1, by = c("hhidpn", "wave"))

rm(dat1)
rm(varnames)
rm(vars_hhold)
##############################

# HOUSEHOLD-LEVEL VARIABLES FROM HARMONIZED HRS
##############################
varnames <- names(vars_fam)

dat1 <- read_dta(file = paste0(dir_input, filenames[['harm']]),
                 col_select = c(hhidpn, matches(paste0("^h\\d+\\d*", varnames, "$"))))

dat1 <- dat1 %>% 
  pivot_longer(cols = -hhidpn,
               names_to = c("wave", ".value"),
               names_pattern = "h(\\d+\\d*)(.*)")

dat1 <- dat1 %>% 
  rename_with(.cols = -c(hhidpn, wave),
              .fn = ~ paste0("h",.))

# colnames(dat1)

dat <- left_join(dat, dat1, by = c("hhidpn", "wave"))

rm(dat1)
rm(varnames)
rm(vars_fam)
##############################

# HOUSEHOLD-LEVEL VARIABLES FROM DETAILED IMPUTATIONS FILE
##############################
varnames <- names(vars_fin)

dat1 <- read_dta(file = paste0(dir_input, filenames[['dimp']]),
                 col_select = c(hhidpn, matches(paste0("^h\\d+\\d*", varnames))))
dat1 <- dat1 %>% 
  pivot_longer(cols = -hhidpn,
               names_to = c("wave", ".value"),
               names_pattern = "h(\\d+\\d*)(.*)")

dat1 <- dat1 %>% 
  rename_with(.cols = -c(hhidpn, wave),
              .fn = ~ paste0("h",.))

# colnames(dat1)

dat <- left_join(dat, dat1, by = c("hhidpn", "wave"))

rm(dat1)
rm(varnames)
rm(vars_fin)
##############################

dat <- dat %>% mutate(wave = as.integer(wave))
dat <- dat %>% select(hhidpn, wave, everything())
dat %>% group_by(wave) %>% summarise(n())

rm(filenames)


