# LOAD REQUIRED PACKAGES
require(tidyverse)

# Estimation sample
load(paste0(dir_working, "03preanalysis/samp_event.rda"))

window_min <- -2
window_max <- 1

gc()

fig <- dat_event %>%
  filter(et %in% window_min:window_max) %>%
  rename(Treated = treated) %>% 
  group_by(Treated, et) %>%
  mutate(depvar = mean(r.cesd)) %>%
  ungroup() %>%
  ggplot(aes(x=et, y=depvar, shape=Treated, linetype = Treated))

### Publication quality
fig + geom_line(linewidth=0.25) + geom_point(size=1.8) +
  geom_vline(xintercept = -0.5, linewidth=0.3, linetype = "dashed", colour = "grey30") +
  scale_x_continuous(limits = c(window_min-0.2, window_max+0.2), 
                     breaks = seq(window_min, window_max,1), 
                     labels = seq(window_min, window_max,1), 
                     name = "Waves since partner's nursing home admission") +
  scale_y_continuous(limits = c(1,3),
                     breaks = seq(1,3,0.5),
                     name = "CES-D score") +
  scale_linetype_manual(name =  "Group:", labels = c("Comparison", "Treated"), values = c("dashed", "solid")) +
  scale_shape_manual(name =  "Group:", labels = c("Comparison", "Treated"), values = c(1,16)) +
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
       filename = "fig3a.jpg", 
       device = "jpg", dpi = 1200, units = "mm", width = 90, height = 67.5)  



rm(fig)
