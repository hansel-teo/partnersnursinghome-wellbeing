
# dat_matched <- match.data(matchout) %>% 
#   mutate(t0 = wave + 1) %>%
#   mutate(e.hhidpn = paste0(str_pad(as.character(t0), width = 2, side = "left", pad = "0"), hhidpn)) %>%
#   select(e.hhidpn, hhidpn, treatwave, t0, treated, weights, subclass, wave) 
# 
# dat_matched <- dat_matched %>%
#   mutate(e.hhidpn = as.numeric(e.hhidpn))


# Care Recipient's ID
dat_matched <- left_join(dat_matched, 
                         samp_pre %>% 
                           select(-treated, -treatwave) %>%
                           rename_with(.cols = -c(cr.hhidpn, wave),
                                       .fn = ~paste0(., "_pre")),
                         by = c("cr.hhidpn", "wave"))

# Respondent's ID
dat_matched <- dat_matched %>% 
  select(e.hhidpn, treated, t0, cr.hhidpn, r.hhidpn_pre, everything())

colnames(dat_matched)

dat_matched %>%
  group_by(treated) %>%
  summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn_pre))

# Care recipient's status in all waves (from index)
dat_matched <- inner_join(dat_matched %>% select(-wave),
                          lgrid %>% 
                            select(hhidpn, wave, riwstat) %>%
                            rename(cr.hhidpn = hhidpn) %>%
                            rename(cr.iwstat = riwstat),
                          by = "cr.hhidpn", multiple = "all")

dat_matched %>%
  group_by(treated) %>%
  summarise(n(), n_distinct(cr.hhidpn), n_distinct(r.hhidpn_pre))

# Respondent's status in all waves (from index)
dat_matched <- left_join(dat_matched,
                         lgrid %>% 
                            select(hhidpn, wave, riwstat) %>%
                            rename(r.hhidpn_pre = hhidpn) %>%
                            rename(r.iwstat = riwstat),
                          by = c("r.hhidpn_pre", "wave"))


# Respondent's partner identifier and partnership status
dat_matched <- left_join(dat_matched,
                         dat_hrs %>% 
                           select(hhidpn, wave, shhidpn, rmstat_grp) %>%
                           rename(r.mstat_grp = rmstat_grp) %>%
                           rename(r.hhidpn_pre = hhidpn) %>%
                           rename(r.shhidpn = shhidpn),
                         by = c("r.hhidpn_pre", "wave"))

# Care Recipient's partner identifier and partnership status
dat_matched <- left_join(dat_matched,
                         dat_hrs %>% 
                           select(hhidpn, wave, shhidpn, rmstat_grp) %>%
                           rename(cr.mstat_grp = rmstat_grp) %>%
                           rename(cr.hhidpn = hhidpn) %>%
                           rename(cr.shhidpn = shhidpn),
                         by = c("cr.hhidpn", "wave"))

# Dependent variable
dat_matched <- left_join(dat_matched,
                         dat_hrs %>% 
                           select(hhidpn, wave, rcesd) %>%
                           rename(r.hhidpn_pre = hhidpn) %>%
                           rename(r.cesd = rcesd),
                         by = c("r.hhidpn_pre", "wave"))

######

dat_event <- dat_matched %>%
  mutate(et = wave - t0)

dat_event %>% select(et, treated) %>% table(useNA="ifany")

### Keep only observations with non-missing dependent var
dat_event <- dat_event %>%
  filter(!is.na(r.cesd))

dat_event %>% select(et, treated) %>% table(useNA="ifany")


### Keep only observations with non-missing Care Recipient status
dat_event %>% select(cr.iwstat) %>% table(useNA="always")
dat_event <- dat_event %>%
  filter(!(cr.iwstat %in% c(0,4,7,9)))

dat_event %>% select(et, treated) %>% table(useNA="ifany")


### Keep only observations where couple is intact pre-treatment
#########
dat_event %>%
  filter(wave <= t0) %>%
  select(cr.mstat_grp, r.mstat_grp, t0) %>% 
  table(useNA="always")

dat_event %>%
  filter(wave < t0) %>%
  filter(cr.mstat_grp != "Married/Partnered") %>%
  select(t0, wave, cr.hhidpn, cr.mstat_grp, cr.shhidpn, r.hhidpn_pre, r.mstat_grp, r.shhidpn) %>%
  print(n=100)

dat_event %>%
  filter(wave < t0) %>%
  filter(r.mstat_grp != "Married/Partnered") %>%
  select(t0, wave, cr.hhidpn, cr.iwstat, cr.mstat_grp, cr.shhidpn, r.hhidpn_pre, r.mstat_grp, r.shhidpn) %>%
  print(n=100)

# Drop pre-treatment cases where Care Recipient or Respondent are not Married/Partnered
dat_event <- dat_event %>%
  filter(!(wave<t0 & cr.mstat_grp=="Divorced/Separated"))
dat_event <- dat_event %>%
  filter(!(wave<t0 & r.mstat_grp=="Divorced/Separated"))

# REMARK: One case where CR (cr.hhidpn==17626011) incorrectly labelled as Never married

# Check that CR's partner ID == R's ID and vice versa
dat_event %>%
  filter(wave < t0) %>%
  filter(r.shhidpn!=cr.hhidpn) %>%
  select(t0, wave, cr.hhidpn, cr.mstat_grp, cr.shhidpn, r.hhidpn_pre, r.mstat_grp, r.shhidpn) %>%
  print(n=100)
dat_event %>%
  filter(wave < t0) %>%
  filter(cr.shhidpn!=r.hhidpn_pre) %>%
  select(t0, wave, cr.hhidpn, cr.iwstat, cr.mstat_grp, cr.shhidpn, r.hhidpn_pre, r.mstat_grp, r.shhidpn) %>%
  print(n=100)

dat_event %>%
  select(et, treated) %>% table(useNA="ifany")
#########


### Exclude post-treatment observations involving separations
#########
dat_event %>%
  filter(wave >= t0) %>%
  select(cr.mstat_grp, r.mstat_grp, t0) %>% 
  table(useNA="always")

###
dat_event %>%
  filter(wave >= t0) %>%
  filter(cr.mstat_grp %in% c("Divorced/Separated", "Never married")) %>%
  select(t0, wave, cr.hhidpn, cr.iwstat, cr.mstat_grp, cr.shhidpn, r.hhidpn_pre, r.iwstat, r.mstat_grp, r.shhidpn) %>%
  print(n=100)

# Drop post-treatment cases where Care Recipient is Divorced/Separated post-treatment
dat_event <- dat_event %>%
  filter(!(wave>=t0 & cr.iwstat==1 & cr.mstat_grp=="Divorced/Separated"))

# Drop post-treatment cases where Care Recipient partner ID != Respondent ID
dat_event <- dat_event %>%
  filter(!(wave>=t0 & cr.iwstat==1 & cr.mstat_grp=="Married/Partnered" & cr.shhidpn!=r.hhidpn_pre))
###


###
dat_event %>%
  filter(wave >= t0) %>%
  filter(r.mstat_grp %in% c("Divorced/Separated", "Never married")) %>%
  select(t0, wave, cr.hhidpn, cr.iwstat, cr.mstat_grp, cr.shhidpn, r.hhidpn_pre, r.iwstat, r.mstat_grp, r.shhidpn) %>%
  print(n=100)

# Drop post-treatment cases where Respondent is Divorced/Separated post-treatment
dat_event <- dat_event %>%
  filter(!(wave>=t0 & cr.iwstat==1 & r.mstat_grp=="Divorced/Separated"))
###


# Check that CR's partner ID == R's ID and vice versa
dat_event %>%
  filter(wave >= t0) %>%
  filter(cr.iwstat==1 & r.shhidpn!=cr.hhidpn) %>%
  select(t0, wave, cr.hhidpn, cr.mstat_grp, cr.shhidpn, r.hhidpn_pre, r.mstat_grp, r.shhidpn) %>%
  print(n=100)

dat_event %>%
  filter(wave >= t0) %>%
  filter(cr.iwstat==1 & cr.shhidpn!=r.hhidpn_pre) %>%
  select(t0, wave, cr.hhidpn, cr.iwstat, cr.mstat_grp, cr.shhidpn, r.hhidpn_pre, r.mstat_grp, r.shhidpn) %>%
  print(n=100)

# Drop post-treatment cases where  CR's partner ID != R's ID and vice versa
dat_event <- dat_event %>%
  filter(!(wave>=t0 & cr.iwstat==1 & cr.shhidpn!=r.hhidpn_pre))
dat_event <- dat_event %>%
  filter(!(wave>=t0 & cr.iwstat==1 & r.shhidpn!=cr.hhidpn))

dat_event %>%
  select(et, treated) %>% table(useNA="ifany")


dat_event <- dat_event %>%
  mutate(treatwave = if_else(treated==TRUE, as.integer(t0), as.integer(0)))

###
dat_event <- dat_event %>%
  mutate(rinsur_pre = if_any(.cols = c(rgovmd_pre, rhiltc_pre),
                             .fns = ~.==1))
dat_event %>%
  select(rgovmd_pre, rhiltc_pre, rinsur_pre) %>% table(useNA="always")

dat_event <- dat_event %>%
  mutate(rhomecare_pre = if_any(.cols = c(rrfrcare_i_pre, rhomcar_e_pre),
                             .fns = ~.==1))

dat_event %>%
  select(rrfrcare_i_pre, rhomcar_e_pre, rhomecare_pre) %>% table(useNA="always")


qtiles <- quantile(dat_event[dat_event$rrscare_i_pre==1,]$rrscarehpw_i_pre, probs = seq(0,1,0.2))
dat_event <- dat_event %>% 
  mutate(rrscarehpw_qtile = cut(rrscarehpw_i_pre, 
                                breaks = c(0,qtiles[-6], 167, Inf),
                                labels = FALSE, include.lowest = TRUE, right = FALSE)) 

dat_event <- dat_event %>% 
  mutate(rrscarehpw_qtile = factor(rrscarehpw_qtile))

dat_event %>% 
  select(rrscare_i_pre, rrscarehpw_qtile) %>% table(useNA='ifany')

colnames(dat_event)

dat_event %>% 
  select(treatwave, treated) %>% table(useNA="always")

dat_event %>% filter(et %in% -3:2) %>% group_by(et, treated) %>% summarise(n(), n_distinct(e.hhidpn))


# Keep only pairs that are balanced in e=-1,0
dat_event <- dat_event %>% 
  filter(subclass %in% (dat_event %>%
                          filter(et%in%-1:0) %>%
                          group_by(subclass) %>%
                          mutate(nobs = n()) %>%
                          ungroup() %>% 
                          mutate(subclass = as.character(subclass)) %>%
                          filter(nobs==4) %>% pull(subclass) %>% unique()))

dat_event %>% group_by(treated) %>% summarise(n(), n_distinct(e.hhidpn))


# Drop unpaired Treated/Control observations in each wave
# dat_event %>% filter(et %in% -3:2) %>% group_by(et, treated) %>% summarise(n(), n_distinct(e.hhidpn))
dat_event <- dat_event %>%
  group_by(subclass, et) %>%
  filter(n()==2) %>%
  ungroup()

dat_event %>% filter(et %in% -3:2) %>% group_by(et, treated) %>% summarise(n(), n_distinct(e.hhidpn))

dat_event %>% group_by(treated) %>% summarise(n(), n_distinct(e.hhidpn))
