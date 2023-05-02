dat_cog <- read_dta(paste0(dir_input, "cogfinalimp_9518wide.dta"))

dat_cog <- dat_cog %>% 
  mutate(hhidpn = as.numeric(paste0(hhid, pn)))

dat_cog <- dat_cog %>% 
  pivot_longer(cols = -c("hhidpn", "hhid", "pn"),
               names_to = c(".value", "iwyear"),
               names_pattern = "(.*)(\\d\\d\\d\\d)")

dat_cog <- dat_cog %>% mutate(wave = iwyear)
dat_cog$wave <- recode(dat_cog$wave, 
                       `2018` = 14, `2016` = 13, `2014` = 12, `2012` = 11, `2010` = 10, 
                       `2008` = 9, `2006` = 8, `2004` = 7, `2002` = 6, `2000` = 5, 
                       `1998` = 4, `1996` = 3, `1995` = 2)

dat_cog %>% select(wave, iwyear) %>% table()

dat_cog <- dat_cog %>% rename_with(.cols = -c(hhidpn, hhid, pn, wave), .fn = ~paste0("r",.))

dat_cog %>% colnames()

dat <- left_join(dat, dat_cog %>% select(-hhid, -pn, -riwyear, -rproxy), by = c("hhidpn", "wave"))

rm(dat_cog)