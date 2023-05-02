
##############################
# CONSTRUCT CANDIDATE TREATMENT GROUP 
##############################

### Base sample from NH entry index
samp_treated <-left_join(vars_tr_nrshom,
                         dat_hrs %>%
                           select(hhidpn, wave, 
                                  hhhid, hcpl, hhhresp, hlvwith, shhidpn, rmstat,
                                  riwstat, rnhmliv_e, rnrshom_e),
                         by = c("hhidpn", "wave"))


# Treatment wave definition
###############
ids_candidate1 <- samp_treated %>%
  filter((wave == rnrshom_e_wv1) & (wave == rnhmliv_e_wv)) %>%
  mutate(treatwave = wave) %>%
  select(hhidpn, treatwave)

samp_treated <- left_join(samp_treated, ids_candidate1, by = "hhidpn", multiple="all")

# Keep pre-treatment observations
samp_treated <- samp_treated %>% filter(wave==treatwave-1)

print(logtab <- samp_treated %>% summarise(n(), n_distinct(hhidpn), n_distinct(shhidpn)))
print(logtab_w <- samp_treated %>% group_by(treatwave) %>% summarise(n()))
###############


# Keep only individuals alive pre-event and alive or just died in next wave
###############
samp_treated <- left_join(samp_treated,
                          lgrid %>% select(hhidpn, wave, riwstat.lead1),
                          by = c("hhidpn", "wave"))

samp_treated <- samp_treated %>%
  filter(riwstat==1 & riwstat.lead1 %in% c(1,5))

logtab <- bind_rows(logtab, samp_treated %>% summarise(n(), n_distinct(hhidpn), n_distinct(shhidpn)))
logtab_w <- left_join(logtab_w, samp_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
###############


# Keep individuals in couples and with identifiable partners
###############
# samp_treated %>%  
#   filter(hcpl==1 & hhhresp==2) %>% 
#   summarise(n(), sum(!is.na(shhidpn)))

samp_treated <- samp_treated %>%  
  filter(hcpl==1 & hhhresp==2 & !is.na(shhidpn))

logtab <- bind_rows(logtab, samp_treated %>% summarise(n(), n_distinct(hhidpn), n_distinct(shhidpn)))
logtab_w <- left_join(logtab_w, samp_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
###############


# Keep individuals in cohabiting couples at pre-treatment wave
###############
samp_treated <- samp_treated %>%  
  filter(hlvwith %in% c("2.Lives with spouse only",
                        "4.Lives with combination of spouse/child/hhm",
                        "5.Lives with other household members only"))

logtab <- bind_rows(logtab, samp_treated %>% summarise(n(), n_distinct(hhidpn), n_distinct(shhidpn)))
logtab_w <- left_join(logtab_w, samp_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
###############


# Drop cases with simultaneous entry
###############
spent <- vars_tr_nrshom %>% 
  select(hhidpn, wave, rnrshom_e_wv) %>%
  arrange(hhidpn, wave) %>%
  group_by(hhidpn) %>%
  mutate(snrshom_e_wv.lead1 = lead(rnrshom_e_wv,1)) %>%
  ungroup() %>%
  rename(shhidpn = hhidpn)

samp_treated <- left_join(samp_treated, spent %>% select(shhidpn, wave, snrshom_e_wv.lead1),
                          by = c("shhidpn", "wave"))

samp_treated %>% filter(treatwave==snrshom_e_wv.lead1) %>% summarise(n())
# samp_treated %>% filter(treatwave==snrshom_e_wv.lead1) %>% select(hhidpn, shhidpn, wave, rnrshom_e_wv1) %>% print(n=500)
samp_treated <- samp_treated %>% filter(treatwave!=snrshom_e_wv.lead1)

logtab <- bind_rows(logtab, samp_treated %>% summarise(n(), n_distinct(hhidpn), n_distinct(shhidpn)))
logtab_w <- left_join(logtab_w, samp_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")

rm(spent)
###############


# DROP INDIVIDUALS WHOSE SPOUSE APPEARS MULTIPLE TIMES
###############
samp_treated %>% 
  group_by(shhidpn) %>%
  filter(n_distinct(hhidpn)>1) %>%
  ungroup() %>% arrange(shhidpn, wave) %>% 
  select(hhidpn, wave, treatwave, shhidpn, hhhid)

samp_treated <- samp_treated %>% 
  group_by(shhidpn) %>%
  filter(n_distinct(hhidpn)==1) %>%
  ungroup()

logtab <- bind_rows(logtab, samp_treated %>% summarise(n(), n_distinct(hhidpn), n_distinct(shhidpn)))
logtab_w <- left_join(logtab_w, samp_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
###############


# KEEP INDIVIDUALS AGE 50+ WITH PARTNERS AGE 50+ AT ET=-1 
###############
samp_treated <- left_join(samp_treated,
                          dat_hrs %>% 
                            select(hhidpn, wave, ragey_e),
                          by = c("hhidpn", "wave"))

samp_treated <- left_join(samp_treated,
                          dat_hrs %>% 
                            select(hhidpn, wave, ragey_e) %>%
                            rename(sagey_e = ragey_e),
                          by = c("shhidpn" = "hhidpn", "wave" = "wave"))

samp_treated <- samp_treated %>% 
  filter(ragey_e>49 & sagey_e>49)

logtab <- bind_rows(logtab, samp_treated %>% summarise(n(), n_distinct(hhidpn), n_distinct(shhidpn)))
logtab_w <- left_join(logtab_w, samp_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
###############


### JOIN LEADS AND LAGS
###############
dat_treated <- samp_treated %>% 
  mutate(cr.hhidpn = hhidpn, r.hhidpn = shhidpn, treatwave, wave) %>%
  select(cr.hhidpn, r.hhidpn, treatwave, wave)

dat_treated <- left_join(dat_treated,
                         lgrid %>% 
                           select(hhidpn, wave, starts_with("riwstat")) %>%
                           rename_with(.cols = -c(hhidpn, wave),
                                       .fn = ~str_replace(., "^r", "cr.")),
                         by = c("cr.hhidpn"="hhidpn", "wave" = "wave"))

dat_treated <- left_join(dat_treated,
                         lgrid %>%
                           rename_with(.cols = -c(hhidpn, wave),
                                       .fn = ~str_replace(., "^r", "r.")),
                         by = c("r.hhidpn" = "hhidpn", "wave" = "wave"))

dat_treated <- left_join(dat_treated,
                         lgrid %>%
                           select(hhidpn, wave, starts_with("shhidpn")) %>%
                           rename_with(.cols = starts_with("shhidpn"),
                                       .fn = ~paste0("r.", .)),
                         by = c("r.hhidpn" = "hhidpn", "wave" = "wave"))
###############


# KEEP INDIVIDUALS WITH NON-MISSING DEPENDENT VARIABLES
###############
depvars <- str_replace(depvarsin, "^r", "r.")
reqdepvars <- c(paste0(depvars, ".lead", seq(1,window_max+1,1)),
                paste0(depvars, ".lag", seq(0,abs(window_min)-1,1)))

select_treated <- dat_treated %>% 
  filter(if_all(.cols = all_of(reqdepvars),
                .fns = ~!is.na(.)))

colnames(logtab) <- colnames(select_treated %>% summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn)))
logtab <- bind_rows(logtab, select_treated %>% summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn)))
logtab_w <- left_join(logtab_w, select_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
rm(reqdepvars)
###############


# KEEP CASES WITH LIVE AND RESPONDING RESPONDENTS
###############
reqdepvars <- c(paste0("r.iwstat.lead", seq(1,window_max+1,1)),
                paste0("r.iwstat.lag", seq(0,abs(window_min)-1,1)))

select_treated <- select_treated %>% 
  filter(if_all(.cols = all_of(reqdepvars),
                .fns = ~.==1))

logtab <- bind_rows(logtab, select_treated %>% summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn)))
logtab_w <- left_join(logtab_w, select_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
rm(reqdepvars)
###############


# KEEP CASES WITH NON-MISSING CARE RECIPIENT STATUS
###############
reqdepvars <- c(paste0("cr.iwstat.lead", seq(1,window_max+1,1)),
                paste0("cr.iwstat.lag", seq(0,abs(window_min)-1,1)))

select_treated <- select_treated %>% 
  filter(if_all(.cols = all_of(reqdepvars),
                .fns = ~.%in%c(1,5,6)))

logtab <- bind_rows(logtab, select_treated %>% summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn)))
logtab_w <- left_join(logtab_w, select_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
rm(reqdepvars)
###############


# KEEP CASES WITH VALID PARTNERSHIP STATUS
###############
reqdepvars <- paste0("r.shhidpn.lag", seq(0,abs(window_min)-1,1))

select_treated <- select_treated %>% 
  filter(if_all(.cols = all_of(reqdepvars),
                .fns = ~.==`cr.hhidpn`))


logtab <- bind_rows(logtab, select_treated %>% summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn)))
logtab_w <- left_join(logtab_w, select_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
rm(reqdepvars)

for(i in seq(1,window_max+1,1)){
  select_treated <- select_treated %>%
    filter(!(get(paste0("cr.iwstat.lead", i))==1 & get(paste0("r.shhidpn.lead", i))!=`cr.hhidpn`))
}

logtab <- bind_rows(logtab, select_treated %>% summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn)))
logtab_w <- left_join(logtab_w, select_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
###############

select_treated <- select_treated %>% 
  select(cr.hhidpn, r.hhidpn, treatwave, wave)

rm(dat_treated, samp_treated)


logtab <- bind_rows(logtab, select_treated %>% summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn)))
logtab_w <- left_join(logtab_w, select_treated %>% group_by(treatwave) %>% summarise(n()), by = "treatwave")
