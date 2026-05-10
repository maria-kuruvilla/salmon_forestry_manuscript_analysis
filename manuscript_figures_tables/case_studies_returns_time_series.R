## Script name: returns time-series 
##
## Purpose of the script: Plot case studies Returns with ECA and CDA over time
##
## Author: Joao Braga
##
## Date Created: 2026-05-05

## Required packages
list.of.packages <- c("tidyverse", "extrafont", "lubridate", "ggplot2", "sf", "qs", "openxlsx", "cowplot", "rnaturalearth", "ggpubr", "scales", "here") 

#What packages need to be installed?
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])] 

## Install missing packages
if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE, repos = c("http://cran.rstudio.com/", "http://R-Forge.R-project.org"))
}

## Loading libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))

source(here("manuscript_figures_tables","r_functions", "plot_returns_ECA_CPD.R"))

# Load case studies information ----
# Forestry time series
forestry_data <- read_csv(here("forestry_data_compilation", "forestry_data_timeseries.csv"))

ECA_viner <- forestry_data %>%
  filter(LINEAR_FEATURE_ID ==  88067322) # Viner Sound

# Chum salmon
chum_dat_plt <- read_csv(here("salmon_forestry_data_analysis", "data", "salmon_datasets_plotting", "CM_case_studies_data.csv"))

# Pink salmon (both even and odd years)
pink_dat_plot <- read_csv(here("salmon_forestry_data_analysis", "data", "salmon_datasets_plotting", "PK_case_studies_data.csv"))


# Chum Salmon Plots ----
plots <- split(chum_dat_plt, chum_dat_plt$River) %>%
  lapply(., function(x) returns_ECA_CPD_plot(dat_plot = x, 
                                             river_str = unique(x$River), 
                                             return_plot = T,
                                             save_plot = F,
                                             species_label = "Chum",
                                             add_points = T,
                                             units = "in") + guides(color = guide_legend(nrow = 2)))

plots$`VINER SOUND CREEK`
plots$`CARNATION CREEK`
plots$`PHILLIPS RIVER`
plots$`NIMPKISH RIVER`
plots$`DEENA CREEK`
plots$`NEEKAS CREEK`

# Pink Salmon Plots ----
plots <- split(pink_dat_plot, pink_dat_plot$River) %>%
  lapply(., function(x)     returns_ECA_CPD_plot(dat_plot = x, 
                                                 river_str = unique(x$River), 
                                                 return_plot = T,
                                                 save_plot = T,
                                                 filename = here("output_figures_tables","forest_supplement","case_studies", paste0(unique(x$River),".png")), 
                                                 species_label = "Pink",
                                                 width = 8, 
                                                 height = 5,
                                                 add_points = T,
                                                 units = "in") + guides(color = guide_legend(nrow = 2)))

plots$`VINER SOUND CREEK`
plots$`PHILLIPS RIVER`
plots$`DEENA CREEK`
plots$`NEEKAS CREEK`

# Viner Sound Disturbance Plot ----
forest_dist_viner_plt <- ggplot(ECA_viner, aes(x = year )) +
  geom_line(aes(color = "Equivalent Clearcut Area (%)", y = ECA_age_proxy_forested_only * 100), linetype = "solid", linewidth = 1) +
  geom_line(aes(color = "Cumulative Disturbed Area (%)", y = haarea_prct_cs ), linetype = "dotted", linewidth = 1) +
  scale_y_continuous(breaks = pretty(seq(0, 50, length.out = 5)),
                     name = "Forest Disturbance (%)") +
  scale_x_continuous(breaks = pretty(seq(1880, 2025, 10))) +
  scale_color_manual(values = c( "Equivalent Clearcut Area (%)" = "black","Cumulative Disturbed Area (%)" = "black")) +
  labs(colour = "", x = "") +
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() + theme(panel.grid = element_blank(),
                          axis.title = element_text(face = "bold", size = 12),
                          axis.text = element_text(face = "bold", size = 10),
                          strip.background = element_blank(),
                          strip.text = element_text(hjust = 0, face = "bold", size = 15),
                          base_family = "ArcherPro Book", 
                          legend.position = "bottom") + 
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))  

forest_dist_viner_plt

ggsave(plot = forest_dist_viner_plt,  filename = here("output_figures_tables","forest_supplement","case_studies", "viner_sound_forest_disturbance.png"), width = 8, height = 5)
# End