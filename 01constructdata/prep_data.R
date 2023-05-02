# Correct HPICKHH
########################################################################
dat_hrs %>% 
  group_by(wave) %>%
  summarise(n(), 
            n_distinct(hhhid), 
            sum(hpickhh==1, na.rm=TRUE))
# one fewer hpickhh than hhhid in all waves except 3
# two more hpickhh than hhhid in Wave 3

dat_hrs <- bind_rows(dat_hrs %>% 
                       filter(wave!=3),
                     dat_hrs %>% 
                       filter(wave==3) %>%
                       group_by(hhhid) %>% 
                       mutate(hpickhh = if_else(sum(hpickhh==1)>1 & n_distinct(hhidpn)>1 & hhidpn==max(hhidpn),
                                                as.integer(0), as.integer(hpickhh))) %>%
                       ungroup())

dat_hrs <- dat_hrs %>% arrange(hhidpn, wave)

dat_hrs %>% 
  group_by(wave) %>%
  summarise(n(), 
            n_distinct(hhhid), 
            sum(hpickhh==1, na.rm=TRUE))

########################################################################

# Define rdwe_days
########################################################################
dat_hrs <- left_join(dat_hrs, 
                     dat_hrs %>% 
                       filter(riwstat==1) %>%
                       arrange(hhidpn, wave) %>% 
                       group_by(hhidpn) %>%
                       mutate(rdwe_days = as.numeric(riwend - lag(riwend,1))) %>%
                       ungroup() %>%
                       select(hhidpn, wave, rdwe_days),
                     by = c("hhidpn", "wave"))

dat_hrs %>% select(rdwe_days) %>% summary()

dat_hrs <- dat_hrs %>%
  mutate(rdwe_wk = rdwe_days/7,
         rdwe_yr = rdwe_wk/52,
         rdwe_mth = 12*rdwe_yr)

dat_hrs %>% select(rdwe_days, rdwe_wk, rdwe_mth, rdwe_yr) %>% summary()
########################################################################

########################################################################
# Replace rnrsnit = rdwe_days 
# if number of nights in NH between interviews > number of days between interviews
dat_hrs <- dat_hrs %>%
  mutate(rnrsnit = if_else(!is.na(rdwe_days) & rnrsnit > rdwe_days, 
                           as.numeric(rdwe_days), as.numeric(rnrsnit)))

# Replace rnrstim = 1 if number of nights in NH between interviews 
# >= number of days between interviews
dat_hrs <- dat_hrs %>%
  mutate(rnrstim = if_else(is.na(rnrstim) & rnrstim >= rdwe_days, 
                           as.numeric(1),
                           as.numeric(rnrstim)))

# Define NH move in day
dat_hrs <- dat_hrs %>%
  mutate(rnhmmv_date = riwend - rnhmday) %>%
  mutate(rnhmmvd = paste(as.character(rnhmmvy), 
                         str_pad(as.character(rnhmmvm), 
                                 width=2, pad="0"), "15", sep="-"),
         rnhmmvd = as.Date(rnhmmvd, format = "%Y-%m-%d"))
########################################################################

########################################################################


gc()
### INCORPORATE MEDICAL CARE USE INFORMATION FROM EXIT INTERVIEW
########################################################################

meduse <- c("hosp", "homcar", "nrshom", "nhmliv")
medfreq <- c("nrsnit", "nrstim")
medexp <- c("oopmd", "mnhm", "mhhc")

for(i in meduse) {
  dat_hrs <- dat_hrs %>%
    mutate("r{i}_e" := if_else(riwstat==5 & is.na(get(paste0("r", i))), 
                               as.integer(get(paste0("re", i))), 
                               as.integer(get(paste0("r", i)))))
  }

dat_hrs %>% select(hhidpn, wave, riwstat, contains(meduse[3])) %>% print(n=100)
dat_hrs %>% select(hhidpn, wave, riwstat, contains(meduse[4])) %>% print(n=100)

for(i in medfreq) {
  dat_hrs <- dat_hrs %>%
    mutate("r{i}_e" := if_else(riwstat==5 & is.na(get(paste0("r", i))), 
                               as.integer(get(paste0("re", i))), 
                               as.integer(get(paste0("r", i)))))
}

dat_hrs %>% select(hhidpn, wave, riwstat, contains(medfreq[1])) %>% print(n=100)


for(i in 1:length(medexp)) {
  dat_hrs <- dat_hrs %>%
    mutate("r{medexp[i]}_i" := if_else(is.na(get(paste0("r", medexp[i]))) & get(paste0("r", meduse[i]))==0, 
                                       as.numeric(0), 
                                       get(paste0("r", medexp[i])))) %>%
    mutate("re{medexp[i]}_i" := if_else(is.na(get(paste0("re", medexp[i]))) & get(paste0("re", meduse[i]))==0, 
                                        as.numeric(0), 
                                        get(paste0("re", medexp[i])))) %>%
    mutate("r{medexp[i]}_e" := if_else(riwstat==5 & is.na(get(paste0("r", medexp[i], "_i"))), 
                                       get(paste0("re", medexp[i], "_i")), 
                                       get(paste0("r", medexp[i], "_i"))))
}


dat_hrs <- dat_hrs %>%
  mutate(rnhmliv_e = if_else(rnrshom_e==0 & is.na(rnhmliv_e), as.integer(0), rnhmliv_e))

rm(meduse, medfreq, medexp)
########################################################################




########################################################################
###### DEFINE DUMMIES AND CATEGORICALS 
########################################################################

######
dat_hrs <- dat_hrs %>%
  mutate(rafemale = if_else(ragender==2, as.integer(1),
                            if_else(!is.na(ragender), as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(ragender = as_factor(as.character(as_factor(ragender))))

dat_hrs %>% select(ragender, rafemale) %>% table(useNA="always")
######

######
dat_hrs <- dat_hrs %>%
  mutate(ranonwhite = if_else(raracem %in% c(2,3), as.integer(1),
                              if_else(is.na(raracem)==FALSE, as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(raracem = as_factor(as.character(as_factor(raracem))))

dat_hrs %>% select(raracem, ranonwhite) %>% table(useNA="always")
######

######
dat_hrs <- dat_hrs %>%
  mutate(racollege = if_else(raeduc==5, as.integer(1),
                             if_else(!is.na(raeduc), as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(rahsdropout = if_else(raeduc==1, as.integer(1),
                               if_else(!is.na(raeduc), as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(raeduc = as_factor(as.character(as_factor(raeduc))))

dat_hrs %>%
  select(raeduc, racollege) %>% table(useNA="always")
dat_hrs %>%
  select(raeduc, rahsdropout) %>% table(useNA="always")
######

######
dat_hrs %>% select(rabplace) %>% table(useNA="always")
dat_hrs <- dat_hrs %>%
  mutate(raforeign = if_else(rabplace==11, as.integer(1),
                             if_else(is.na(rabplace)==FALSE, as.integer(0), 
                                     as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(rabplace = as_factor(as.character(as_factor(rabplace))))

dat_hrs %>%
  select(rabplace, raforeign) %>% table(useNA="always")
######

######
dat_hrs %>% select(rmstat) %>% table(useNA="always")
dat_hrs <- dat_hrs %>%
  mutate(rmarried = if_else(rmstat==1, as.integer(1),
                            if_else(is.na(rmstat)==FALSE, as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(rmstat = as_factor(as.character(as_factor(rmstat))))

dat_hrs %>%
  select(rmstat, rmarried) %>% table(useNA="always")

dat_hrs <- dat_hrs %>%
  mutate(rmstat_grp = as.character(as_factor(rmstat))) %>%
  mutate(rmstat_grp = case_match(rmstat_grp, 
                                 "1.married" ~ "Married/Partnered",
                                 "3.partnered" ~ "Married/Partnered",
                                 "2.married,spouse absent" ~ "Married/Partnered",
                                 "4.separated" ~ "Divorced/Separated",
                                 "5.divorced" ~ "Divorced/Separated",
                                 "6.separated/divorced" ~ "Divorced/Separated",
                                 "7.widowed" ~ "Widowed",
                                 "8.never married" ~ "Never married")) %>%
  mutate(rmstat_grp = as_factor(rmstat_grp)) 


dat_hrs %>% 
  select(rmstat_grp, rmstat) %>% table(useNA="always")

dat_hrs %>% select(rmstat_grp, hcpl) %>% table(useNA="always")
######

######
dat_hrs <- dat_hrs %>%
  mutate(halone = if_else(hlvwith == 1, as.integer(1),
                          if_else(is.na(hlvwith)==FALSE, as.integer(0), as.integer(NA))))
dat_hrs <- dat_hrs %>%
  mutate(hspouseonly = if_else(hlvwith == 2, as.integer(1),
                               if_else(is.na(hlvwith)==FALSE, as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(hlvwith_grp = as.character(as_factor(hlvwith))) %>%
  mutate(hlvwith_grp = case_match(hlvwith_grp, 
                                  "1.Lives alone" ~ "Alone",
                                  "2.Lives with spouse only" ~ "Spouse only",
                                  "3.Lives with children only" ~ "Combination of spouse/children/others",
                                  "4.Lives with combination of spouse/child/hhm" ~ "Combination of spouse/children/others",
                                  "5.Lives with other household members only" ~ "Combination of spouse/children/others")) %>%
  mutate(hlvwith_grp = as_factor(hlvwith_grp)) 


dat_hrs <- dat_hrs %>%
  mutate(hlvwith = as.character(as_factor(hlvwith))) %>%
  mutate(hlvwith = na_if(hlvwith, ".m:Missing")) %>%
  mutate(hlvwith = as_factor(hlvwith))

dat_hrs %>% select(hlvwith, hlvwith_grp) %>%
  select(hlvwith, hlvwith_grp) %>% table(useNA="always")
######

######
dat_hrs <- dat_hrs %>%
  mutate(hadults = hhhres - hkidu14)

dat_hrs %>% filter(riwstat==1) %>% select(hhhres, hadults, hkidu14) %>% summary()

dat_hrs <- dat_hrs %>%
  mutate(hhhres_grp = cut(hhhres, breaks = c(1,2,3,Inf),
                          right = FALSE,
                          labels = c("1", "2", "3+"),
                          ordered_result = FALSE))
dat_hrs <- dat_hrs %>%
  mutate(hadults_grp = cut(hadults, breaks = c(1,2,3,Inf),
                           right = FALSE,
                           labels = c("1", "2", "3+"),
                           ordered_result = FALSE))

dat_hrs <- dat_hrs %>%
  mutate(hchild_grp = cut(hchild, breaks = c(0,1,2,3,Inf),
                          right = FALSE,
                          labels = c("0", "1", "2", "3+"),
                          ordered_result = FALSE))

dat_hrs <- dat_hrs %>%
  mutate(hhhres.sq = hhhres^2)
dat_hrs <- dat_hrs %>%
  mutate(hchild.sq = hchild^2)
######

######
dat_hrs <- dat_hrs %>%
  mutate(ragey_i = if_else(ragey_e >95, 95, ragey_e),
         ragey_i = factor(ragey_i))

dat_hrs <- dat_hrs %>%
  mutate(ragey_grp = cut(ragey_e, breaks = c(18, seq(20, 100, 5), Inf),
                         include.lowest = TRUE, right = FALSE))

dat_hrs <- dat_hrs %>%
  mutate(ragey_65plus = if_else(ragey_e>=65, TRUE, FALSE))

dat_hrs %>% select(ragey_i, ragey_grp) %>% table(useNA="always")

dat_hrs <- dat_hrs %>%
  mutate(ragey.sq = ragey_e^2)
######

######
dat_hrs <- dat_hrs %>%
  mutate(remployed = if_else(rlbrf %in% c(1,2), as.integer(1),
                             if_else(!is.na(rlbrf), as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(runemployed = if_else(rlbrf %in% c(3), as.integer(1),
                               if_else(!is.na(rlbrf), as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(rnotinlbrf = if_else(rlbrf %in% c(5,6,7), as.integer(1),
                              if_else(!is.na(rlbrf), as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(rlbrf_grp = as.character(as_factor(rlbrf))) %>%
  mutate(rlbrf_grp = case_match(rlbrf_grp, 
                                "1.works ft" ~ "Employed",
                                "2.works pt" ~ "Employed",
                                "3.unemployed" ~ "Unemployed",
                                "4.partly retired" ~ "Partly retired",
                                "5.retired" ~ "Retired",
                                "6.disabled" ~ "Disabled",
                                "7.not in lbrf" ~ "Not in labor force")) %>%
  mutate(rlbrf_grp = as_factor(rlbrf_grp)) 

dat_hrs <- dat_hrs %>%
  mutate(rlbrf = as_factor(as.character(as_factor(rlbrf))))

dat_hrs %>%
  select(rlbrf, remployed) %>% table(useNA="always")

dat_hrs %>%
  select(rlbrf, rlbrf_grp) %>% table(useNA="always")
######

######
dat_hrs <- dat_hrs %>%
  mutate(rhlthpoor = if_else(rshlt %in% c(4,5), as.integer(1),
                             if_else(!is.na(rshlt), as.integer(0), as.integer(NA))))

dat_hrs <- dat_hrs %>%
  mutate(rshlt_grp = if_else(rshlt %in% c(1,2), 1, 
                             if_else(rshlt %in% c(3), 2, 3))) %>%
  mutate(rshlt_grp = factor(rshlt_grp))

dat_hrs <- dat_hrs %>%
  mutate(rshlt_i = as_factor(as.character(as_factor(rshlt))))
######

######
dat_hrs <- dat_hrs %>% mutate(rimpaired_i = (rcogfunction !=1))
dat_hrs <- dat_hrs %>% mutate(rdemented_i = (rcogfunction ==3))

dat_hrs <- dat_hrs %>%
  mutate(rcogfunction_i = as_factor(as.character(as_factor(rcogfunction))))
######

######
dat_hrs <- dat_hrs %>%
  mutate(racohbyr = as_factor(as.character(as_factor(racohbyr))))

dat_hrs <- dat_hrs %>%
  mutate(rcenreg = as_factor(as.character(as_factor(rcenreg))),
         rcendiv = as_factor(as.character(as_factor(rcendiv))))
######

########################################################################
########################################################################
########################################################################



########################################################################
###### RECODE AND DERIVE VARIABLES
########################################################################

######
dat_hrs %>% select(rhiltc, rtyltc) %>% table(useNA="always")

dat_hrs <- dat_hrs %>%
  mutate(rtyltc_i = if_else(rhiltc==0, as.integer(0), as.integer(rtyltc)))

dat_hrs %>% select(rhiltc, rtyltc_i) %>% table(useNA="always")
######

######
dat_hrs <- dat_hrs %>% 
  mutate(rmemrye_i = as.integer(rmemrye)) %>%
  mutate(rmemrye_i = if_else((ralzhee==1 | rdemene==1) & is.na(rmemrye_i), as.integer(1), 
                             if_else((ralzhee==0 & rdemene==0) & is.na(rmemrye_i), as.integer(0), rmemrye_i)))

dat_hrs %>% 
  filter(riwstat==1) %>% 
  group_by(wave) %>%
  summarise(n(), across(.cols = c(rmemrye_i),
                        .fns = list(resprate = ~ sum(!is.na(.))/n()),
                        .names = "{.fn} {.col}"))

dat_hrs <- dat_hrs %>% 
  arrange(hhidpn, wave) %>%
  group_by(hhidpn) %>%
  mutate(rmemrye_i = if_else(is.na(rmemrye_i) & dplyr::lag(rmemrye_i,1)==1, as.integer(1), as.integer(rmemrye_i))) %>%
  ungroup()
######

######
dat_hrs$rheartprb <- rowSums(dat_hrs[, c("rhrtatt", "rangin", "rconhrtf")], na.rm=TRUE)
dat_hrs <- dat_hrs %>% 
  mutate(rheartprb = if_else(rheartprb>0, as.integer(1), 
                                 if_else(is.na(rhrtatt) | is.na(rangin) | is.na(rconhrtf), as.integer(NA), 
                                         as.integer(0))))
######

########################################################################
########################################################################
########################################################################



gc()
########################################################################
vars_care <- c("rrscare", "rrccare", "rrrcare", "rrfcare","rrpfcare", "rrufcare")

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = all_of(vars_care),
                .fns = ~ as.character(as_factor(.)),
                .names = "{.col}_i"))

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~na_if(., ".d:DK"))) %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~na_if(., ".m:Missing"))) %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~na_if(., ".r:Refuse")))

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~ if_else(. == ".x:no difficulties", "0.no", .),
                .names = "{.col}"))

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~ if_else(. == ".h:no help received", "0.no", .),
                .names = "{.col}"))

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~ if_else(. == ".f:infrequent care", "0.no", .),
                .names = "{.col}"))

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~ if_else(. == ".q:not asked this wave" & radl6a==0 & riadl5a==0 & riwstat==1, "0.no", .),
                .names = "{.col}"))

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~ na_if(., ".q:not asked this wave"),
                .names = "{.col}"))

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~ recode(., "0.no"=0, "1.yes"=1),
                .names = "{.col}"))

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = all_of(paste0(vars_care, "_i")),
                .fns = ~ as.integer(.),
                .names = "{.col}"))

lapply(all_of(vars_care), function(x) {
  dat_hrs %>% select(all_of(x), paste0(x, "_i")) %>% table(useNA="always")
})


# Summary variables
dat_hrs <- dat_hrs %>%
  rowwise() %>%
  mutate(rrnscare_i = sum(rrccare_i, rrrcare_i, rrfcare_i)) %>%
  ungroup()

dat_hrs <- dat_hrs %>% 
  mutate(rrnscare_i = if_else(rrnscare_i>0, as.integer(1), 
                              if_else(is.na(rrccare_i) & is.na(rrrcare_i) & is.na(rrfcare_i), as.integer(NA), 
                                      as.integer(0))))

dat_hrs <- dat_hrs %>%
  rowwise() %>%
  mutate(rrfrcare_i = sum(rrpfcare_i, rrufcare_i)) %>%
  ungroup()

dat_hrs <- dat_hrs %>% 
  mutate(rrfrcare_i = if_else(rrfrcare_i>0, as.integer(1), 
                              if_else(is.na(rrpfcare_i) & is.na(rrufcare_i), as.integer(NA), 
                                      as.integer(0))))
dat_hrs <- dat_hrs %>%
  rowwise() %>%
  mutate(rrrfcare_i = sum(rrrcare_i, rrfcare_i)) %>%
  ungroup()

dat_hrs <- dat_hrs %>% 
  mutate(rrrfcare_i = if_else(rrrfcare_i>0, as.integer(1), 
                              if_else(is.na(rrrcare_i) & is.na(rrfcare_i), as.integer(NA), 
                                      as.integer(0))))

###

dat_hrs %>%
  filter(rrscare==0) %>%
  # filter(rrscare_i==0) %>%
  select(rrscaredpw, rrscarehr) %>%
  table(useNA="always")

dat_hrs %>%
  filter(rrccare_i==0) %>%
  select(rrccaredpw, rrccarehr) %>%
  table(useNA="always")

dat_hrs %>%
  filter(rrpfcare_i==0) %>%
  select(rrpfcaredpw, rrpfcarehr) %>%
  table(useNA="always")

# Since any help is asked before dpw and hr, override dpw and hr with zero if any ==0
dat_hrs <- dat_hrs %>%
  mutate(rrscaredpw_i = if_else(rrscare_i==0, as.integer(0), as.integer(rrscaredpw))) %>%
  mutate(rrccaredpw_i = if_else(rrccare_i==0, as.integer(0), as.integer(rrccaredpw))) %>%
  mutate(rrrcaredpw_i = if_else(rrrcare_i==0, as.integer(0), as.integer(rrrcaredpw))) %>%
  mutate(rrfcaredpw_i = if_else(rrfcare_i==0, as.integer(0), as.integer(rrfcaredpw))) %>%
  mutate(rrufcaredpw_i = if_else(rrufcare_i==0, as.integer(0), as.integer(rrufcaredpw)))  %>%
  mutate(rrpfcaredpw_i = if_else(rrpfcare_i==0, as.integer(0), as.integer(rrpfcaredpw)))

dat_hrs <- dat_hrs %>%
  mutate(rrscarehr_i = if_else(rrscare_i==0, as.integer(0), as.integer(rrscarehr))) %>%
  mutate(rrccarehr_i = if_else(rrccare_i==0, as.integer(0), as.integer(rrccarehr))) %>%
  mutate(rrrcarehr_i = if_else(rrrcare_i==0, as.integer(0), as.integer(rrrcarehr))) %>%
  mutate(rrfcarehr_i = if_else(rrfcare_i==0, as.integer(0), as.integer(rrfcarehr))) %>%
  mutate(rrufcarehr_i = if_else(rrufcare_i==0, as.integer(0), as.integer(rrufcarehr))) %>%
  mutate(rrpfcarehr_i = if_else(rrpfcare_i==0, as.integer(0), as.integer(rrpfcarehr)))

dat_hrs %>% select(rrpfcare_i, rrpfcaren) %>% table(useNA="always")

dat_hrs <- dat_hrs %>%
  mutate(rrufcaren_i = if_else(rrufcare_i==0, as.integer(0), as.integer(rrufcaren))) %>%
  mutate(rrpfcaren_i = if_else(rrpfcare_i==0, as.integer(0), as.integer(rrpfcaren)))

dat_hrs %>% select(rrpfcare_i, rrpfcaren_i) %>% table(useNA="always")

dat_hrs %>% filter(rrscare_i==1) %>% select(rrscaredpw_i, rrscarehr_i) %>% table(useNA="always")
dat_hrs %>% filter(rrpfcare_i==1) %>% select(rrpfcaredpw_i, rrpfcarehr_i) %>% table(useNA="always")

dat_hrs %>% select(rrscare_i, rrscaredpw_i) %>% table(useNA="always")
dat_hrs %>% select(rrpfcare_i, rrpfcaredpw_i) %>% table(useNA="always")

# REMERK dpw and hr only available continuously from Wave 5
# dat_hrs %>%
#   group_by(wave) %>%
#   summarise(n(), sum(!is.na(rrscaredpw)), sum(!is.na(rrscarehr)))

dat_hrs <- dat_hrs %>%
  mutate(rrscarehpw_i = rrscaredpw_i*rrscarehr_i)

dat_hrs <- dat_hrs %>%
  mutate(rrscarehpw_i.sq = rrscarehpw_i^2)

# dat_hrs <- dat_hrs %>%
#   mutate(rrscaredpw_i = if_else(rrscare_i==1 & rrscaredpw_i==0, as.integer(NA), as.integer(rrscaredpw_i))) %>%
#   mutate(rrccaredpw_i = if_else(rrccare_i==1 & rrccaredpw_i==0, as.integer(NA), as.integer(rrccaredpw_i))) %>%
#   mutate(rrrcaredpw_i = if_else(rrrcare_i==1 & rrrcaredpw_i==0, as.integer(NA), as.integer(rrrcaredpw_i))) %>%
#   mutate(rrfcaredpw_i = if_else(rrfcare_i==1 & rrfcaredpw_i==0, as.integer(NA), as.integer(rrfcaredpw_i))) %>%
#   mutate(rrufcaredpw_i = if_else(rrufcare_i==1 & rrufcaredpw_i==0, as.integer(NA), as.integer(rrufcaredpw_i))) %>%
#   mutate(rrpfcaredpw_i = if_else(rrpfcare_i==1 & rrpfcaredpw_i==0, as.integer(NA), as.integer(rrpfcaredpw_i)))

dat_hrs %>% select(rrpfcaren_i, rrpfcare_i) %>% table(useNA="always")

dat_hrs %>% select(matches(paste0(vars_care, ".*_i$"))) %>% summary()
lapply(vars_care, function(x){
  dat_hrs %>% filter(get(paste0(x,"_i"))==1) %>% select(matches(paste0(x, ".*_i$"))) %>% summary()
})
lapply(vars_care, function(x){
  dat_hrs %>% filter(get(paste0(x,"_i"))==0) %>% select(matches(paste0(x, ".*_i$"))) %>% summary()
})

rm(vars_care)
########################################################################
########################################################################
########################################################################


gc()
########################################################################
### WINSORIZE VARIABLES
########################################################################
vars_assets <- c("hatotb", "hatotf", "hatotn", "hatoth")


temp <- dat_hrs %>% 
  filter(hpickhh==1) %>%
  group_by(wave) %>%
  mutate(across(.cols = all_of(vars_assets),
                .fns = ~ if_else((. > quantile(., probs=0.99, na.rm=TRUE))==TRUE,
                                 quantile(., probs=0.995, na.rm=TRUE), .),
                .names = "{.col}_cens")) %>%
  mutate(across(.cols = c(paste0(vars_assets, "_cens")),
                .fns = ~ if_else((. < quantile(., probs=0.01, na.rm=TRUE))==TRUE,
                                 quantile(., probs=0.005, na.rm=TRUE), .),
                .names = "{.col}")) %>%
  ungroup() %>% 
  select(hhhid, wave, paste0(vars_assets, "_cens")) %>% unique()

dat_hrs <- left_join(dat_hrs, temp, by = c("hhhid", "wave"))
rm(temp)


temp <- dat_hrs %>% 
  filter(hpickhh==1) %>%
  group_by(wave) %>%
  mutate(across(.cols = c(hitot),
                .fns = ~ if_else((. > quantile(., probs=0.99, na.rm=TRUE))==TRUE,
                                 quantile(., probs=0.99, na.rm=TRUE), .),
                .names = "{.col}_cens")) %>%
  ungroup() %>% 
  select(hhhid, wave, hitot_cens) %>% unique()

dat_hrs <- left_join(dat_hrs, temp, by = c("hhhid", "wave"))
rm(temp)
dat_hrs %>% select(ends_with("_cens")) %>% summary()


vars_medexp <- c(paste0("r", c("oopmd", "mnhm", "mhhc")),
                 paste0("s", c("oopmd", "mnhm", "mhhc")))
dat_hrs <- dat_hrs %>% 
  group_by(wave) %>%
  mutate(across(.cols = starts_with(vars_medexp),
                .fns = ~ if_else((. > quantile(., probs=0.99, na.rm=TRUE))==TRUE,
                                 quantile(., probs=0.99, na.rm=TRUE), .),
                .names = "{.col}_cens")) %>%
  ungroup()


dat_hrs <- dat_hrs %>%
  mutate(across(.cols = paste0(c("hitot", vars_medexp, vars_assets), "_cens"),
                .fns = ~./cpi_reg_year,
                .names = "{.col}_defl"))

dat_hrs <- dat_hrs %>%
  rename_with(.cols = ends_with("_cens_defl"),
              .fn = ~str_replace(., "_cens_defl", "_defl"))


dat_hrs <- dat_hrs %>%
  mutate(eq_pp = 1/hhhres) %>%
  mutate(eq_oecd = 1/(1 +0.5*(hadults-1) + 0.3*hkidu14)) %>%
  mutate(eq_sqrt = 1/sqrt(hhhres))

dat_hrs <- dat_hrs %>%
  mutate(across(.cols = paste0(vars_assets, "_defl"),
                .fns = ~eq_pp*.,
                .names = "{.col}.pp"))
dat_hrs <- dat_hrs %>%
  mutate(across(.cols = paste0(c("hitot"), "_defl"),
                .fns = ~eq_oecd*.,
                .names = "{.col}.oecdeq"))
dat_hrs <- dat_hrs %>%
  mutate(across(.cols = paste0(c("hitot"), "_defl"),
                .fns = ~eq_sqrt*.,
                .names = "{.col}.sqrteq"))

dat_hrs <- dat_hrs %>%
  group_by(wave) %>%
  mutate(across(.cols = contains(c("hitot", vars_assets)),
                .fns = ~cut(., 
                            breaks = quantile(.,probs = seq(0,1,0.2), na.rm=TRUE),
                            labels = FALSE),
                .names = "{.col}_qtile")) %>%
  ungroup()

rm(vars_assets, vars_medexp)
# dat_hrs <- dat_hrs %>%
#   mutate(across(.cols = contains("hpw"),
#                 .fns = ~ if_else((. > quantile(., probs=0.995, na.rm=TRUE))==TRUE,
#                                  quantile(., probs=0.995, na.rm=TRUE), .),
#                 .names = "{.col}_cens"))
# 
# dat %>% select(ends_with("hpw_cens"), ends_with("hpw")) %>% summary()
########################################################################
