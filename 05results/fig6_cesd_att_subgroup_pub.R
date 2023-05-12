# PLOT SUBGROUP ANALYSIS
####################################
load(paste0(dir_working, "04estimation/output/event_scesd_subgroups.rda"))

names_subgroups <- names(EST.ATT.CESD)
Est <- lapply(names_subgroups, function(x) EST.ATT.CESD[[x]])

Est[[1]] <- Est[[1]] %>% mutate(Comparison = "cr.adlgrp")
Est[[2]] <- Est[[2]] %>% mutate(Comparison = "cr.adlgrp")
Est[[3]] <- Est[[3]] %>% mutate(Comparison = "cr.impaired")
Est[[4]] <- Est[[4]] %>% mutate(Comparison = "cr.impaired")
Est[[5]] <- Est[[5]] %>% mutate(Comparison = "cr.memrye")
Est[[6]] <- Est[[6]] %>% mutate(Comparison = "cr.memrye")
Est[[7]] <- Est[[7]] %>% mutate(Comparison = "r.female")
Est[[8]] <- Est[[8]] %>% mutate(Comparison = "r.female")


for (i in 1:length(names_subgroups)){
  Est[[i]] <- Est[[i]] %>% mutate(Subgroup = names_subgroups[i])
}

# Est <- lapply(names_subgroups, function(x) Est[[x]] %>% mutate(Subgroup = x))

Est <- bind_rows(Est)

Est <- Est %>%
  mutate(subgroup = factor(Subgroup,
                           levels = names_subgroups[c(7,8,5,6,3,4,1,2)]))
                                         

Est <- Est %>% 
  # mutate(relchg = att/depvar_pre) %>%
  mutate(relci.lo = ci.lo/depvar_pre) %>%
  mutate(relci.hi = ci.hi/depvar_pre)

plot.scesd.subgroup <- ggplot(data = Est %>% filter(et==0), 
                             aes(x = relchg, y = subgroup, 
                                 color = Comparison, 
                                 fill = Comparison, 
                                 shape = Comparison,
                                 xmin = relci.lo, xmax = relci.hi)) + 
  geom_point(size = 1.8, stroke = 0.6) + 
  geom_errorbar(linewidth=0.3, width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")

plot.scesd.subgroup + 
  scale_x_continuous(limits = c(-0.05, 1.05),
                     breaks = seq(0, 1,0.25),
                     labels = seq(0, 1,0.25), name = "Proportional increase in CES-D \n from pre-admission to admission wave") +
  scale_colour_viridis_d(end = 0.8, option = "D") + 
  scale_fill_viridis_d(end = 0.8, option = "D") + 
  scale_shape_manual(values = c(22,21,24,23)) +
  # scale_shape_manual(values = c("square filled", "circle filled", "triangle filled", "diamond filled")) +
  # scale_linetype_manual(values = c("f6", "solid", "f6", "solid", "f6", "solid", "f6", "solid")) +
  ylab("")+
  # labs(title="Effect of partner's nursing home entry on respondent's mental health") +
  # annotate(geom="text", y = -0.1, x=-1,
  #          label = paste0("Pre-event mean = ", scesd.gend$depvar_pre %>% unique() %>% mean() %>% round(digits = 3))) +
  theme_classic() + 
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 6), 
        axis.line = element_line(linewidth=0.3),
        axis.ticks = element_line(linewidth=0.3),
        legend.position = "none",
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5, size = 9))

####################################


ggsave(path = paste0(dir_working, "05results/figures/"), 
       filename = "fig6.jpg", 
       device = "jpg", dpi = 1200, units = "mm", width = 90, height = 90) 

rm(plot.scesd.subgroup)
rm(Est)


