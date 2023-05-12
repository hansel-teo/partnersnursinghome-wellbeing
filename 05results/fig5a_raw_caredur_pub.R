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


dat_plot <- dat_event %>%
  select(e.hhidpn, wave, treated, t0, et,
         r.cesd, 
         rrscaredur1_pre, rrscaredur2_pre,
         rrscare_i_pre, rrscarehpw_i_pre)

### Define FD of CESD at admission wave  
dat_plot <- dat_plot %>%
  arrange(e.hhidpn, wave) %>%
  group_by(e.hhidpn) %>%
  mutate(r.cesd.fd = r.cesd - lag(r.cesd,1)) %>%
  ungroup()
###

dat_plot <- dat_plot %>% filter(et==0)

########
fig <-  dat_plot %>%
  filter(rrscare_i_pre==1) %>%
  ggplot(aes(x=rrscaredur2_pre, y = r.cesd.fd, 
             shape = treated, linetype = treated))

fig + stat_summary(geom = "point", size = 1.8, position = position_dodge(width=0.4)) + 
  stat_summary(geom = "errorbar", width=0.15, linewidth=0.25, position = position_dodge(width=0.4)) +
  geom_hline(yintercept=0) +
  scale_shape_manual(name =  "Group", labels = c("Comparison", "Treated"), values = c(1, 16)) +
  scale_linetype_manual(name =  "Group", labels = c("Comparison", "Treated"), values = c("dashed", "solid")) +
  # scale_colour_viridis_d(name =  "Group", labels = c("Treated", "Comparison"), end = 0.55, option = "D") +
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  scale_x_discrete(name = "At least two waves of caregiving before admission") +
  ylab("Change in CES-D score at NH admission wave") + 
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

########

ggsave(path = paste0(dir_working, "05results/figures/"), 
       filename = "fig5a.jpg", 
       device = "jpg", dpi = 1200, units = "mm", width = 90, height = 60)  

rm(dat_plot)
rm(fig)