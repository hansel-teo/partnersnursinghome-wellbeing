vars_cesd <- c("r.depres", "r.fsad", "r.flone",
               "r.whappy", "r.enlife",
               "r.effort", "r.sleepr", "r.going")

names <- c("Felt depressed", "Felt sad", "Felt lonely", 
           "Was happy",  "Enjoyed life",
           "Everything an effort", "Restless sleep", "Could not get going")


load(paste0(dir_working, "04estimation/output/event_cesdqns.rda"))

EST.ATT.CESD.DUMMIES <- lapply(names(EST.ATT.CESD.DUMMIES), function(x) EST.ATT.CESD.DUMMIES[[x]] %>% mutate(depvar = x))

est.scesd.dummies <- left_join(bind_rows(EST.ATT.CESD.DUMMIES),
                               bind_cols(depvar = vars_cesd,
                                         `Dependent variable` = names),
                               by = "depvar")

est.scesd.dummies <- est.scesd.dummies %>%
  mutate(`Dependent variable` = factor(`Dependent variable`, levels = rev(names)))

est.scesd.dummies <- est.scesd.dummies %>%
  mutate(Subgroup = "Pooled")

######
# plot.dummies <- ggplot(data = est.scesd.dummies %>% filter(et==0), 
#                               aes(x = att, y = `Dependent variable`,
#                                   # shape = `Dependent variable`,
#                                   xmin = ci.lo, xmax = ci.hi)) + 
#   # facet_grid(rows = vars(`Dependent variable`), shrink = TRUE) +
#   geom_point(size = 3.5, stroke = 2) + geom_errorbar(width = 0.15, size = 0.8) +
#   geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
#   scale_x_continuous(#limits = c(-0.15, 0.5), 
#                      breaks = seq(-0.1, 0.5,0.1), 
#                      labels = seq(-0.1, 0.5,0.1), name = "Change in probability from pre-admission to admission wave")
# 
# plot.dummies + 
#   scale_colour_viridis_d(end = 0.7, option = "D") + 
#   scale_shape_manual(values = c(1,1,1,1,1,1,1,1)) +
#   # scale_linetype_manual(values = c("solid", "f6", "solid", "f6", "solid", "f6", "solid", "f6")) +
#   ylab("Probability of reporting")+
#   # annotate(geom="text", y = -0.1, x=-1,
#   #          label = paste0("Pre-event mean = ", scesd.gend$depvar_pre %>% unique() %>% mean() %>% round(digits = 3))) +
#   theme_classic() + theme(axis.text = element_text(size = 12), 
#                           axis.title = element_text(size = 12), 
#                           strip.text = element_text(size = 12),
#                           legend.position = "none",
#                           plot.caption = element_text(hjust = 0),
#                           plot.title = element_text(hjust = 0.5, size = 14))
# 
# ggsave(path = paste0(dir_working, "05results/figures/"), 
#        filename = "fig_cesd.att_cesdqns-20230412.jpg", 
#        device = "jpg", dpi = 500, width = 9, height = 8) 
# 
# rm(est.scesd.dummies)
# rm(plot.dummies)
######

rm(EST.ATT.CESD.DUMMIES)
load(paste0(dir_working, "04estimation/output/event_cesdqns_noncare.rda"))

EST.ATT.CESD.DUMMIES <- lapply(names(EST.ATT.CESD.DUMMIES), function(x) EST.ATT.CESD.DUMMIES[[x]] %>% mutate(depvar = x))
est.scesd.dummies0 <- left_join(bind_rows(EST.ATT.CESD.DUMMIES),
                                bind_cols(depvar = vars_cesd, 
                                          `Dependent variable` = names),
                                by = "depvar")

est.scesd.dummies0 <- est.scesd.dummies0 %>% 
  mutate(`Dependent variable` = factor(`Dependent variable`, levels = rev(names))) %>% 
  mutate(Subgroup = "Non-caregiver")


rm(EST.ATT.CESD.DUMMIES)
load(paste0(dir_working, "04estimation/output/event_cesdqns_caregive.rda"))

EST.ATT.CESD.DUMMIES <- lapply(names(EST.ATT.CESD.DUMMIES), function(x) EST.ATT.CESD.DUMMIES[[x]] %>% mutate(depvar = x))
est.scesd.dummies1 <- left_join(bind_rows(EST.ATT.CESD.DUMMIES),
                                bind_cols(depvar = vars_cesd, 
                                          `Dependent variable` = names),
                                by = "depvar")

est.scesd.dummies1 <- est.scesd.dummies1 %>% 
  mutate(`Dependent variable` = factor(`Dependent variable`, levels = rev(names))) %>% 
  mutate(Subgroup = "Caregiver")


est.scesd.dummies <- bind_rows(est.scesd.dummies,
          bind_rows(est.scesd.dummies0, est.scesd.dummies1))


plot.dummies <- ggplot(data = est.scesd.dummies %>% filter(et==0), 
                       aes(x = att, y = `Dependent variable`,
                           shape = Subgroup, color = Subgroup,
                           xmin = ci.lo, xmax = ci.hi)) + 
  geom_point(size = 1.8, stroke = 0.6) + 
  geom_errorbar(data = est.scesd.dummies %>% filter(et==0&Subgroup=="Pooled"),
                aes(x = att, y = `Dependent variable`,
                    xmin = ci.lo, xmax = ci.hi), linewidth=0.3, width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
  scale_x_continuous(#limits = c(-0.15, 0.5), 
    breaks = seq(-0.1, 0.5,0.1), 
    labels = seq(-0.1, 0.5,0.1), name = "Change in probability from pre-admission to admission wave")

plot.dummies + 
  scale_colour_manual(name = "Subsample:", values = c("grey50","grey50","black")) + 
  scale_shape_manual(name = "Subsample:", values = c(2,0,16)) +
  # scale_linetype_manual(values = c("solid", "f6", "solid", "f6", "solid", "f6", "solid", "f6")) +
  ylab("Probability of reporting")+
  # annotate(geom="text", y = -0.1, x=-1,
  #          label = paste0("Pre-event mean = ", scesd.gend$depvar_pre %>% unique() %>% mean() %>% round(digits = 3))) +
  theme_classic() + 
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 6), 
        axis.line = element_line(linewidth=0.3),
        axis.ticks = element_line(linewidth=0.3),
        legend.position = "bottom",
        # legend.direction = "vertical",
        legend.margin  = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.box.spacing = unit(6, units = "pt"),
        legend.key.size = unit(6, units = "pt"),
        legend.title =  element_text(size = 6),
        legend.text = element_text(size = 6),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5, size = 9))

ggsave(path = paste0(dir_working, "05results/figures/"), 
       filename = "fig7.jpg", 
       device = "jpg", dpi = 1200, units = "mm", width = 90, height = 90) 

rm(est.scesd.dummies)
rm(plot.dummies)
