# LOAD REQUIRED PACKAGES
require(tidyverse)

# PATHS TO DATA AND WORKING DIRECTORY
######
# # Home
# dir_root <- "/Users/hanselteo/Data/"
# dir_working <- paste0(dir_root, "nhspouse_replication/")

# Work
dir_root <- "E:/My Drive/" 
dir_working <- paste0(dir_root, "projects/nhspouse_wellbeing/")

setwd(dir_working)
######

# Estimation sample
load(paste0(dir_working, "03preanalysis/samp_event.rda"))

window_min <- -2
window_max <- 1

gc()
fig <- dat_event %>%
  filter(et %in% window_min:window_max) %>%
  rename(Treated = treated) %>% 
  mutate(`Partner caregiver` = factor(rrscare_i_pre)) %>%
  group_by(Treated, `Partner caregiver`, et) %>%
  mutate(depvar = t.test(r.cesd)$estimate) %>%
  ungroup() %>%
  ggplot(aes(x=et, y=depvar, 
             linetype = Treated,
             shape = `Partner caregiver`,
             color = `Partner caregiver`))


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
  scale_colour_viridis_d(name = "Caregiving:", labels = c("Non-caregiver", "Caregiver"), end = 0.55, option = "D") + 
  scale_shape_manual(name = "Caregiving:", labels = c("Non-caregiver", "Caregiver"), values = c(16,17)) + 
  theme_classic() + 
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 6), 
        axis.line = element_line(linewidth=0.3),
        axis.ticks = element_line(linewidth=0.3),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.margin  = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.box.spacing = unit(6, units = "pt"),
        legend.key.size = unit(8, units = "pt"),
        legend.title =  element_text(size = 6), 
        legend.text = element_text(size = 6), 
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5, size = 9))

ggsave(path = paste0(dir_working, "05results/figures/"), 
       filename = "fig4a.jpg", 
       device = "jpg", dpi = 1200, units="mm", width = 90, height = 67.5)  
