
load(paste0(dir_working, "04estimation/output/event_scesd_mainresults.rda"))
Est <- EST.ATT.CESD$Pooled

# PLOT MAIN EFFECT
####################################
plot.scesd <- ggplot(data = Est, aes(x = et, y = att, ymin = ci.lo, ymax = ci.hi)) + 
  geom_point(size=1.8) + geom_errorbar(width=0.15, linewidth=0.25) + 
  geom_hline(yintercept = 0, linewidth=0.3) + 
  geom_vline(xintercept = -0.5, linewidth=0.3, linetype = "dashed", colour = "grey30") +
  scale_x_continuous(limits = c(min(Est$et)-0.2, max(Est$et)+0.2), 
                     breaks = seq(min(Est$et), max(Est$et),1), 
                     labels = seq(min(Est$et), max(Est$et),1), 
                     name = "Waves since partner's nursing home admission")

plot.scesd + 
  # geom_ribbon(aes(x = et, ymin = cb.lo, ymax = cb.hi), alpha = 0.2) +
  scale_y_continuous(#limits = c(-0.5,1.25), 
                     breaks = seq(-0.5,1.25,0.25), labels = round(seq(-0.5,1.25,0.25),2),
                     name = "Change in CES-D score relative to pre-event wave") +
  annotate(geom="text", y = -0.1, x=-1, 
           size = 5.5/.pt,
           label = paste0("Pre-event mean: ", Est$depvar_pre %>% unique() %>% signif(4))) +
  # labs(title="Effect of partner's nursing home admission on respondent's mental well-being") +
  theme_classic() + 
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 6), 
        axis.line = element_line(linewidth=0.3),
        axis.ticks = element_line(linewidth=0.3),
        # legend.position = "bottom",
        # legend.direction = "vertical",
        # legend.margin  = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        # legend.box.spacing = unit(6, unit = "pt"),
        # legend.key.size = unit(8, unit = "pt"),
        # legend.title =  element_text(size = 6), 
        # legend.text = element_text(size = 6), 
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5, size = 9))
####################################

ggsave(path = paste0(dir_working, "05results/figures/"), 
       filename = "fig3b.jpg", 
       device = "jpg", dpi = 1200, units = "mm", width = 90, height = 67.5)   


rm(plot.scesd)
rm(Est)