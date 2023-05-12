
# PLOT EFFECT X CAREGIVING SUBGROUPS
####################################
load(paste0(dir_working, "04estimation/output/event_scesd_mainresults.rda"))

Est <- list(EST.ATT.CESD$`Non-caregivers`, EST.ATT.CESD$Caregivers)
Est[[1]] <- Est[[1]] %>% mutate(Subgroup = "Non-caregiver")
Est[[2]] <- Est[[2]] %>% mutate(Subgroup = "Caregiver")

Est <- bind_rows(Est)
  
plot.cesd.carestat <- ggplot(data = Est, 
                              aes(x = et, y = att, ymin = ci.lo, ymax = ci.hi, 
                                  shape = Subgroup, 
                                  color = Subgroup)) + 
  geom_point(size = 1.8, position = position_dodge(width = 0.4)) + 
  geom_errorbar(linewidth=0.25, width = 0.15, position = position_dodge(width = 0.4)) +
  # geom_ribbon(aes(x = et, ymin = cb.lo, ymax = cb.hi, fill = Subgroup), position = position_dodge(width = 0.4), alpha = 0.2) +
  # geom_line(linetype="dashed", position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linewidth=0.3) +
  geom_vline(xintercept = -0.5, linewidth=0.3,linetype = "dashed", colour = "grey30") +
  scale_x_continuous(limits = c(min(Est$et)-0.5, max(Est$et)+0.5), 
                     breaks = seq(min(Est$et), max(Est$et),1), 
                     labels = seq(min(Est$et), max(Est$et),1), name = "Waves since partner's nursing home admission")


plot.cesd.carestat + 
  scale_y_continuous(limits = c(-0.75,1.75), 
                     breaks = seq(-0.5,1.5,0.5), labels = seq(-0.5,1.5,0.5),
                     name = "Change in CES-D score relative to pre-event wave") +
  scale_colour_viridis_d(name = "Caregiving:", labels = c("Caregiver", "Non-caregiver"), end = 0.55, option = "D", direction=-1) +
  scale_shape_manual(name = "Caregiving:",labels = c("Caregiver", "Non-caregiver"), values = c(17,16)) +
  # scale_colour_grey(start = 0, end = 0.6) + 
  annotate(geom="text", size = 5.5/.pt, y = c(-0.1, -0.2, -0.3), x=-1,
           label = c("Pre-event mean:", 
                     paste0(round(unique(Est[Est$Subgroup=='Caregiver',]$depvar_pre), digits=3), " (Caregivers)"),
                     paste0(round(unique(Est[Est$Subgroup=='Non-caregiver',]$depvar_pre), digits=3), " (Non-caregivers)"))) +
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


####################################


ggsave(path = paste0(dir_working, "05results/figures/"), 
       filename = "fig4b.jpg", 
       device = "jpg", dpi = 1200, units = "mm", width = 90, height = 67.5)   


rm(plot.cesd.carestat)
rm(Est)