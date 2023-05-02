
vars_dur <- samp_pre %>%
      select(cr.hhidpn, wave, treated, treatwave, r.hhidpn, rrscare_i)

vars_dur <- left_join(vars_dur, 
                      lgrid %>%
                            select(hhidpn, wave, starts_with("rrscare_i.lag")),
                      by = c("cr.hhidpn"="hhidpn", "wave"="wave"))

vars_dur <- left_join(vars_dur, 
                      lgrid %>%
                            select(hhidpn, wave, 
                                   starts_with("riwstat.lag"),
                                   starts_with("rmstat.lag"), 
                                   starts_with("shhidpn.lag")) %>%
                            rename(cr.hhidpn = hhidpn) %>%
                            rename_with(.cols = c(starts_with("riwstat.lag"),
                                                  starts_with("rmstat.lag")),
                                        .fn = ~str_replace(., "^r", "cr.")) %>%
                            rename_with(.cols = starts_with("shhidpn.lag"),
                                        .fn = ~paste0("cr.",.)),
                      by = c("cr.hhidpn", "wave"))

vars_dur <- left_join(vars_dur, 
                      lgrid %>%
                            select(hhidpn, wave, 
                                   starts_with("riwstat.lag"),
                                   starts_with("rmstat.lag"), 
                                   starts_with("shhidpn.lag")) %>%
                            rename(r.hhidpn = hhidpn) %>%
                            rename_with(.cols = c(starts_with("riwstat.lag"),
                                                  starts_with("rmstat.lag")),
                                        .fn = ~str_replace(., "^r", "r.")) %>%
                            rename_with(.cols = starts_with("shhidpn.lag"),
                                        .fn = ~paste0("r.",.)),
                      by = c("r.hhidpn", "wave"))

colnames(vars_dur)

vars_dur %>% 
      filter(rrscare_i==1) %>%
      select(cr.hhidpn, wave, treatwave, starts_with("rrscare_i.lag")) %>%
      print(n=100)


# Recode rrscare_i in e=-2
######
vars_dur %>%
   filter(is.na(rrscare_i.lag1)==TRUE) %>%
   select(cr.iwstat.lag1, r.iwstat.lag1, treated) %>% 
   table(useNA="ifany")

# Code as -1 if Respondent not in survey in previous wave
vars_dur <- vars_dur %>%
   mutate(rrscare.lag1 = as.integer(rrscare_i.lag1)) %>%
   mutate(rrscare.lag1 = if_else(r.iwstat.lag1 != 1,
                                 as.integer(-1), rrscare.lag1))
# Code as -1 if Care Recipient not in survey in previous wave
vars_dur <- vars_dur %>%
   mutate(rrscare.lag1 = if_else(cr.iwstat.lag1 != 1,
                                 as.integer(-1), rrscare.lag1))
vars_dur %>% 
   select(r.iwstat.lag1, cr.iwstat.lag1, rrscare.lag1) %>% table(useNA="always")
###



# Recode rrscare_i in e=-3
######
vars_dur %>%
   filter(is.na(rrscare_i.lag2)==TRUE) %>%
   select(cr.iwstat.lag2, r.iwstat.lag2) %>% 
   table(useNA="ifany")

# Code as -2 if Respondent not in survey in e=-2
vars_dur <- vars_dur %>%
   mutate(rrscare.lag2 = as.integer(rrscare_i.lag2)) %>%
   mutate(rrscare.lag2 = if_else(r.iwstat.lag2 != 1,
                                 as.integer(-2), rrscare.lag2))

# Code as -2 if Care Recipient not in survey in e=-2
vars_dur <- vars_dur %>%
   mutate(rrscare.lag2 = if_else(cr.iwstat.lag2 != 1,
                                 as.integer(-2), rrscare.lag2))
vars_dur %>% 
   select(r.iwstat.lag2, cr.iwstat.lag2, rrscare.lag2) %>% table(useNA="always")


vars_dur %>% 
   select(r.iwstat.lag1, cr.iwstat.lag1, rrscare.lag2) %>% table(useNA="always")

# Code as -1 if rrscare.lag2==-2 and Respondent not in survey in e=-1
vars_dur <- vars_dur %>%
   mutate(rrscare.lag2 = if_else(rrscare.lag2 %in% c(-2, NA) & r.iwstat.lag1 != 1,
                                 as.integer(-1), rrscare.lag2))

# Code as -1 if rrscare.lag2==-2 and Care Recipient not in survey in e=-1
vars_dur <- vars_dur %>%
   mutate(rrscare.lag2 = if_else(rrscare.lag2 %in% c(-2, NA) & cr.iwstat.lag1 != 1,
                                 as.integer(-1), rrscare.lag2))

vars_dur %>% 
   select(r.iwstat.lag1, cr.iwstat.lag1, rrscare.lag2) %>% table(useNA="always")

vars_dur %>%
   select(rrscare.lag1, rrscare.lag2, rrscare_i) %>% table(useNA="ifany")
###

###
vars_dur <- vars_dur %>% 
   mutate(rrscaredur1 = (rrscare_i==1 & rrscare.lag1==0))

vars_dur <- vars_dur %>% 
   mutate(rrscaredur2 = (rrscare_i==1 & rrscare.lag1==1 & rrscare.lag2!=1))

vars_dur <- vars_dur %>% 
  mutate(rrscaredur3plus = (rrscare_i==1 & rrscare.lag1==1 & rrscare.lag2==1))

vars_dur %>%
  group_by(treated) %>%
  summarise(n(), `1` = sum(rrscaredur1, na.rm=TRUE), `2` = sum(rrscaredur2, na.rm=TRUE), `3+` = sum(rrscaredur3plus, na.rm=TRUE))

colnames(vars_dur) 

vars_dur <- vars_dur %>%
   mutate(rrscaredur_grp = if_else(rrscare_i==0, as.integer(0),
                                   if_else(rrscare_i==1 & rrscaredur1==TRUE, as.integer(1), 
                                           if_else(rrscare_i==1 & rrscaredur2==TRUE, as.integer(2),
                                                   if_else(rrscare_i==1 & rrscaredur3plus==TRUE, as.integer(3), as.integer(-4))))))

vars_dur <- vars_dur %>%
   mutate(rrscaredur_grp = as.factor(rrscaredur_grp))

vars_dur %>%
  filter(rrscare_i==1) %>%
  group_by(rrscaredur_grp) %>%
  summarise(n(), `1` = sum(rrscaredur1, na.rm=TRUE), `2` = sum(rrscaredur2, na.rm=TRUE), `3+` = sum(rrscaredur3plus, na.rm=TRUE))

samp_pre <- left_join(samp_pre,
                      vars_dur %>% 
                            select(cr.hhidpn, wave, 
                                   starts_with("rrscaredur"),
                                   starts_with("rrscare.lag")),               
                      by = c("cr.hhidpn", "wave"))

rm(vars_dur)
