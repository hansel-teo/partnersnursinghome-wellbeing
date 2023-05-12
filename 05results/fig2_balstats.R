vars_health <- c("rshlt_grp",
                 "radlcount", #"radlcount.sq",
                 "rwalkra", "rdressa", "rbatha", "reata", "rbeda", "rtoilta",  "rclim1a",
                 "rcogfunction_i",
                 "rmemrye_i",
                 "rhibpe", "rdiabe", "rcancre", "rlunge", 
                 "rhearte", "rstroke", "rpsyche", "rarthre",
                 "rhrtatt", "rangin", "rconhrtf")

vars_demog <- c("safemale", "sanonwhite", "sacollege",
                "sagey_e", # "sagey.sq", 
                "semployed", 
                "rafemale", "ranonwhite", "racollege", 
                "ragey_e", #"ragey.sq",
                "hspouseonly", 
                "hhhres", # "hhhres.sq", 
                "hchild", # "hchild.sq",
                "haohous",
                "hatoth_defl", 
                "hatotf_defl", 
                "hitot_defl", "rhiltc", "rgovmd")

vars_care <- c("rrscarehpw_i", #"rrscarehpw_i.sq", 
               "rrnscare_i", 
               # "rrfrcare_i", 
               "rhomcar_e", "rhosp_e")

require(MatchIt)


# HEALTH VARIABLES
############
dat_balstat <- summary(matchit(formula(paste0("treated ~ ", paste0(paste0(c(vars_health, vars_care), "_pre"), collapse = " + "))),
                               method = NULL,
                               # exact = c("wave", match_exacts),
                               # replace = FALSE,
                               data = dat_event %>% filter(et==-1)))

dat_balstat <- data.frame(dat_balstat$sum.all[, 1:4]) %>% 
  mutate(Subgroup = "Pooled")

dat_balstat <- dat_balstat[rownames(dat_balstat)!="distance",]

temp <- summary(matchit(formula(paste0("treated ~ ", paste0(paste0(c(vars_health, vars_care), "_pre"), collapse = " + "))),
                        method = NULL,
                        # exact = c("wave", match_exacts),
                        # replace = FALSE,
                        data = dat_event %>% filter(et==-1 & rrscare_i_pre==0)))

temp <- data.frame(temp$sum.all[, 1:4]) %>% 
  mutate(Subgroup = "Non-caregivers")

temp <- temp[rownames(temp)!="distance",]

dat_balstat <- bind_rows(dat_balstat, temp)

rm(temp)

temp <- summary(matchit(formula(paste0("treated ~ ", paste0(paste0(c(vars_health, vars_care), "_pre"), collapse = " + "))),
                        method = NULL,
                        # exact = c("wave", match_exacts),
                        # replace = FALSE,
                        data = dat_event %>% filter(et==-1 & rrscare_i_pre==1)))

temp <- data.frame(temp$sum.all[, 1:4]) %>% 
  mutate(Subgroup = "Caregivers")

temp <- temp[rownames(temp)!="distance",]

dat_balstat <- bind_rows(dat_balstat, temp)

rm(temp)

names_health <- c("Self-reported health: Excellent/Very Good", 
                  "Self-reported health: Good",
                  "Self-reported health: Fair/Poor",
                  "Number of difficulties with ADLs",
                  "Diff. walking across room", 
                  "Diff. getting dressed", 
                  "Diff. bathing/shower", 
                  "Diff. eating", 
                  "Diff. getting in/out of bed", 
                  "Diff. using toilet",
                  "Diff. climbing one flight of stairs",
                  "Cog. function: Normal", 
                  "Cog. function: Impaired", 
                  "Cog. function: Severely impaired",
                  "Ever diag. with Alzheimer's/Dementia", 
                  "Ever diag. with high blood pressure", "Ever diag. with diabetes", "Ever diag. with cancer", "Ever diag. with lung disease", 
                  "Ever diag. with heart problems", "Ever had a stroke", "Ever had psychiatric problems", "Ever had arthritis",
                  "Had heart attack since last wave", "Had angina since last wave", "Had congestive heart failure since last wave")

names_care <- c("Hours of care per week from partner",
                "Received informal care from other sources", 
                # "Received formal help with daily living",
                "Received home health care",
                "Had a hospital stay")

dat_balstat$Variable <- rep(c(names_health, names_care), 3)

dat_balstat <- dat_balstat %>%
  mutate(Variable = factor(Variable, levels = c(names_health, names_care)))


dat_balstat <- dat_balstat %>%
  mutate(`Standardized Mean Difference` = abs(`Std..Mean.Diff.`))

plot.smd <- ggplot(data = dat_balstat,
                   aes(x=`Standardized Mean Difference`, y =Variable, shape = Subgroup, color=Subgroup)) + 
  geom_point(size = 1.8) +
  geom_vline(xintercept = 0.1, linetype = "dashed", colour = "grey30") +
  geom_vline(xintercept = 0.25, linetype = "dashed", colour = "grey30")


plot.smd + 
  ylab("") +
  # scale_colour_viridis_d(labels = c("Caregiver", "Non-caregiver", "Pooled"), end = 0.55, option = "D", direction=-1) +
  scale_color_manual(name = "Subsample:", labels = c("Caregiver", "Non-caregiver", "Pooled"), values = c("grey50","grey50","black")) + 
  scale_shape_manual(name = "Subsample:", labels = c("Caregiver", "Non-caregiver", "Pooled"), values = c(2,1,16)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 6), 
        axis.line = element_line(linewidth=0.3),
        axis.ticks = element_line(linewidth=0.3),
        legend.position = "bottom",
        # legend.direction = "vertical",
        legend.margin  = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.box.spacing = unit(6, units = "pt"),
        legend.key.size = unit(8, units = "pt"),
        legend.title =  element_text(size = 6), 
        legend.text = element_text(size = 6), 
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5, size = 9))

ggsave(path = paste0(dir_working, "05results/figures/"), 
       filename = "fig2.jpg", 
       device = "jpg", dpi = 1200, units="mm", width = 140, height = 210)   


tab_balstat <- dat_balstat %>% filter(Subgroup=="Pooled")

rm(dat_balstat)
rm(plot.smd)
############

# DEMOGRAPHIC VARIABLES
############
dat_balstat <- summary(matchit(formula(paste0("treated ~ ", paste0(paste0(vars_demog, "_pre"), collapse = " + "))),
                               method = NULL,
                               # exact = c("wave", match_exacts),
                               # replace = FALSE,
                               data = dat_event %>% filter(et==-1)))

dat_balstat <- data.frame(dat_balstat$sum.all[, 1:4]) %>% 
  mutate(Subgroup = "Pooled")

dat_balstat <- dat_balstat[rownames(dat_balstat)!="distance",]

temp <- summary(matchit(formula(paste0("treated ~ ", paste0(paste0(vars_demog, "_pre"), collapse = " + "))),
                        method = NULL,
                        # exact = c("wave", match_exacts),
                        # replace = FALSE,
                        data = dat_event %>% filter(et==-1 & rrscare_i_pre==0)))

temp <- data.frame(temp$sum.all[, 1:4]) %>% 
  mutate(Subgroup = "Non-caregivers")

temp <- temp[rownames(temp)!="distance",]

dat_balstat <- bind_rows(dat_balstat, temp)

rm(temp)

temp <- summary(matchit(formula(paste0("treated ~ ", paste0(paste0(vars_demog, "_pre"), collapse = " + "))),
                        method = NULL,
                        # exact = c("wave", match_exacts),
                        # replace = FALSE,
                        data = dat_event %>% filter(et==-1 & rrscare_i_pre==1)))

temp <- data.frame(temp$sum.all[, 1:4]) %>% 
  mutate(Subgroup = "Caregivers")

temp <- temp[rownames(temp)!="distance",]

dat_balstat <- bind_rows(dat_balstat, temp)

rm(temp)

names_demog <- c("Resp.: Is female", 
                 "Resp.: Is non-white", 
                 "Resp.: Has college education", 
                 "Resp.: Age (years)", 
                 "Resp.: Is employed",
                 "Care Recip.: Is female", 
                 "Care Recip.: Is non-white", 
                 "Care Recip.: Has college education", 
                 "Care Recip.: Age (years)",
                 "Couple only household",
                 "# of household residents",
                 "# of living children",
                 "Owns home",
                 "Net value of home (2012 USD)", 
                 "Couple's non-housing financial wealth (2012 USD)", 
                 "Per-person household income (2012 USD)",
                 "Care recipient has private LTC insurance", 
                 "Care recipient has Medicaid coverage")

dat_balstat$Variable <- rep(names_demog, 3)


dat_balstat <- dat_balstat %>%
  mutate(`Standardized Mean Difference` = abs(`Std..Mean.Diff.`))

plot.smd <- ggplot(data = dat_balstat,
                   aes(x=`Standardized Mean Difference`, y =Variable, shape = Subgroup)) + 
  geom_point(size = 1.8) +
  geom_vline(xintercept = 0.1, linetype = "dashed", colour = "grey30") +
  geom_vline(xintercept = 0.25, linetype = "dashed", colour = "grey30")


plot.smd + 
  scale_color_manual(labels = c("Caregiver", "Non-caregiver", "Pooled"), values = c("grey50","grey50","black")) + 
  scale_shape_manual(labels = c("Caregiver", "Non-caregiver", "Pooled"), values = c(2,1,16)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 6), 
        axis.line = element_line(linewidth=0.3),
        axis.ticks = element_line(linewidth=0.3),
        legend.position = "bottom",
        # legend.direction = "vertical",
        legend.margin  = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.box.spacing = unit(6, unit = "pt"),
        legend.key.size = unit(8, unit = "pt"),
        legend.title =  element_text(size = 6), 
        legend.text = element_text(size = 6), 
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5, size = 9))

ggsave(path = paste0(dir_working, "05results/figures/"), 
       filename = "fig_balstats_demog-20230406.jpg", 
       device = "jpg", dpi = 1000, width = 9, height = 6)   



tab_balstat <- bind_rows(tab_balstat, dat_balstat %>% filter(Subgroup=="Pooled"))
rm(dat_balstat)




tab_balstat <- tab_balstat %>% select(Variable, `Standardized Mean Difference`)

write.csv(desc, 
          row.names = FALSE, 
          paste0(dir_working, "05results/tables/tabappendix1-20230406.csv"))


