## Script name: ECA_CDA_timseries_panel_plots.R
##
## Purpose of the script: Create ECA & CDA plots per CU region for each species. These plots are part of the Forest Sup mat.
##
## Author: Joao Braga
##
## Date Created: May-2026
##
## Trinity Consultants Canada (Victoria Office), 2026
## Email: joao.braga@trinityconsultants.com
##

## Required packages
list.of.packages <- c("tidyverse", "extrafont", "lubridate", "ggplot2", "sf", "qs", "openxlsx", "cowplot", "rnaturalearth", "ggpubr", "here") 

#What packages need to be installed?
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])] 

## Install missing packages
if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE, repos = c("http://cran.rstudio.com/", "http://R-Forge.R-project.org"))
}

## Loading libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))

source(here("manuscript_figures_tables","r_functions", "plot_forest_disturance_timeseries_inset.R"))

# Loading data ----
# Data for plot
world <- ne_countries(scale='medium',returnclass = 'sf')
Canada <- subset(world, admin == "Canada")
USA <- subset(world, admin == "United States of America")

# Conservation Units polygons ----
# Chum
CM_poly <- st_read(here("salmon_forestry_data_analysis", "data", "salmon_datasets_plotting", "chum_CUS.gpkg")) %>%
  mutate(CU = gsub("(?<![0-9])0+", "", CU, perl = TRUE))

# Pink-Odd years
PKO_poly <- st_read(here("salmon_forestry_data_analysis", "data", "salmon_datasets_plotting", "pink_odd_CUS.gpkg")) %>%
  mutate(CU = gsub("(?<![0-9])0+", "", CU, perl = TRUE))

# Pink-Even years
PKE_poly <- st_read(here("salmon_forestry_data_analysis", "data", "salmon_datasets_plotting", "pink_even_CUS.gpkg")) %>%
  mutate(CU = gsub("(?<![0-9])0+", "", CU, perl = TRUE))

# Salmon Lookup Table ----
# Link betwork salmon populations and forestry information
salmon_IDS <- read_csv(here("salmon_forestry_data_analysis", "data", "salmon_datasets_plotting", "salmon_watersheds_lookup.csv"))

salmon_IDS$LINEAR_FEATURE_ID <- as.character(salmon_IDS$LINEAR_FEATURE_ID) 

# Forestry Dataset ----
# Timeseries
ECA_data <- read_csv(here("forestry_data_compilation", "forestry_data_timeseries.csv"))  %>%  
  mutate(LINEAR_FEATURE_ID = as.character(LINEAR_FEATURE_ID))

# Forestry data summaries
ECA_sum <- read_csv(here("forestry_data_compilation", "forestry_data_metadata.csv"))  %>% 
  select(group, 
         LINEAR_FEATURE_ID = this_lfid , 
         portion_reporting_vri_cover1_missing) %>%  # Proportion of reported VRI
  mutate(LINEAR_FEATURE_ID = as.character(LINEAR_FEATURE_ID)) # Fresh water atlas linear feature ID

# Joining information
ECA_data <- left_join(ECA_data, ECA_sum, by = join_by(group, LINEAR_FEATURE_ID))

ECA_data <- left_join(salmon_IDS, ECA_data, by = join_by(LINEAR_FEATURE_ID))

# Filtering incomplete information
ECA_data <- ECA_data %>%
  filter(!is.na(LINEAR_FEATURE_ID) & portion_reporting_vri_cover1_missing < 0.1 & !is.na(ECA_age_proxy_forested_only)) %>%
  mutate(BroodYear = as.numeric(year))

# ECA time series panel plots ----
## CHUM CU ----
mod.dat.CM <- ECA_data %>% filter(Species %in% "CM")

mod.dat.CM <- mod.dat.CM %>% 
  mutate(ECA_age_proxy_forested_only = ECA_age_proxy_forested_only * 100) # Convert to percentage

CM_poly_sub <- CM_poly %>% filter(CU %in% mod.dat.CM$CU)   

# Region:
### Vancouver Island south and central coast ----
vanc_isl <- paste0("CM-", seq(4, 17))

CU_plt <- lapply(vanc_isl, \(x) plot_forest_disturance_timeseries_inset(x, 
                                                                        mod.dat = mod.dat.CM, 
                                                                        var_to_plot = "ECA_age_proxy_forested_only",
                                                                        y_lim = c(0, 60),
                                                                        y_lab = "Equivalent Clearcut Area (%)",
                                                                        CU_poly = CM_poly_sub, 
                                                                        Canada, 
                                                                        USA)
)

vanc_isl_plot <- ggarrange(plotlist = CU_plt, ncol = 4, nrow = 4)

x11()
vanc_isl_plot

ggsave(vanc_isl_plot,  filename = here("output_figures_tables","forest_supplement","ECA_CU_CHUM_Vanc_Isl.png"), width = 12, height = 8, units = "in")

### Haida Gwaii isl and north coast ----
hgwai <-  paste0("CM-", seq(18, 32))

hgwai <- hgwai[hgwai != "CM-29"]

CU_plt <- lapply(hgwai, \(x) plot_forest_disturance_timeseries_inset(x, 
                                                                     mod.dat = mod.dat.CM, 
                                                                     var_to_plot = "ECA_age_proxy_forested_only",
                                                                     y_lim = c(0, 60),
                                                                     y_lab = "Equivalent Clearcut Area (%)",
                                                                     CU_poly = CM_poly_sub, 
                                                                     Canada, 
                                                                     USA)
)

hgwai_plot <- ggarrange(plotlist = CU_plt, ncol = 4, nrow = 4)
hgwai_plot
ggsave(hgwai_plot,  filename = here("output_figures_tables","forest_supplement","ECA_CU_CHUM_hgwaii_Isl.png"), width = 12, height = 8, units = "in")

## Pink-odd years ----
mod.dat.PKO <- ECA_data %>% filter(Species %in% "PKO")

mod.dat.PKO <- mod.dat.PKO %>% 
  mutate(ECA_age_proxy_forested_only = ECA_age_proxy_forested_only * 100) # Convert to percentage


unique(mod.dat.PKO$CU) # PKO-9 & 8 contain a zero to form PKO-09 and PKO-08, and this is not like the others.
mod.dat.PKO <- mod.dat.PKO %>%
  mutate(CU = gsub(pattern = "PKO-09", replacement = "PKO-9", x = CU), 
         CU = gsub(pattern = "PKO-08", replacement = "PKO-8", x = CU))

# Filtering the CUs
PKO_poly_sub <- PKO_poly %>% filter(CU %in% mod.dat.PKO$CU)   

# Plottig per region 
### Vancouver Island south and central coast ----
#' *PKO-5, PKO-4, PKO-3 and PKO-6* PKO-6 contains no salmon recruit information (excluded)
vanc_isl <- paste0("PKO-", seq(2, 8))

vanc_isl <- vanc_isl[vanc_isl %in% unique(mod.dat.PKO$CU)]

CU_plt <- lapply(vanc_isl, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                               mod.dat = mod.dat.PKO,
                                                                               var_to_plot = "ECA_age_proxy_forested_only",
                                                                               y_lim = c(0, 60),
                                                                               y_lab = "Equivalent Clearcut Area (%)",
                                                                               CU_poly = PKO_poly_sub, 
                                                                               Canada, 
                                                                               USA)
)

vanc_isl_plot <- ggarrange(plotlist = CU_plt, ncol = 3, nrow = 2)
vanc_isl_plot

ggsave(vanc_isl_plot,  filename = here("output_figures_tables","forest_supplement","ECA_CU_PKO_Vanc_Isl.png"),
       width = 9, height = 4, units = "in")

### Haida Gwaii isl and North coast ----
hgwai <-  paste0("PKO-", seq(9, 18))

hgwai <- hgwai[hgwai %in% unique(mod.dat.PKO$CU)]

CU_plt <- lapply(hgwai, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                            mod.dat = mod.dat.PKO, 
                                                                            var_to_plot = "ECA_age_proxy_forested_only",
                                                                            y_lim = c(0, 60),
                                                                            y_lab = "Equivalent Clearcut Area (%)",
                                                                            CU_poly = PKO_poly_sub, 
                                                                            Canada, 
                                                                            USA)
)


hgwai_plot <- ggarrange(plotlist = CU_plt, ncol = 4, nrow = 3)
hgwai_plot

ggsave(hgwai_plot,  filename = here("output_figures_tables","forest_supplement","ECA_CU_PKO_hgwaii_Isl.png"),
       width = 12, height = 6, units = "in")

## Pink-even years ----
mod.dat.PKE <- ECA_data %>% filter(Species %in% "PKE")
mod.dat.PKE$CU <- gsub(pattern = "-0", replacement = "-", x = mod.dat.PKE$CU)

mod.dat.PKE <- mod.dat.PKE %>% 
  mutate(ECA_age_proxy_forested_only = ECA_age_proxy_forested_only * 100) # Convert to percentage


PKE_poly_sub <- PKE_poly %>% filter(CU %in% mod.dat.PKE$CU)   

### Vancouver Island south and central coast ----
vanc_isl <- paste0("PKE-", seq(1, 6))

vanc_isl <- vanc_isl[vanc_isl %in% unique(mod.dat.PKE$CU)]

CU_plt <- lapply(vanc_isl, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                               mod.dat = mod.dat.PKE,
                                                                               var_to_plot = "ECA_age_proxy_forested_only",
                                                                               y_lim = c(0, 60),
                                                                               y_lab = "Equivalent Clearcut Area (%)",
                                                                               CU_poly = PKE_poly_sub,
                                                                               Canada,
                                                                               USA)
)

vanc_isl_plot <- ggpubr::ggarrange(plotlist = CU_plt, ncol = 2, nrow = 2)
vanc_isl_plot

ggsave(vanc_isl_plot,  filename = here("output_figures_tables","forest_supplement","ECA_CU_PKE_Van_south_coast.png"),
       width = 6, height = 4, units = "in")

### Haida Gwaii and North coast ----
haida_ac <- paste0("PKE-", seq(7, 13))

haida_ac <- haida_ac[haida_ac %in% unique(mod.dat.PKE$CU)]

CU_plt <- lapply(haida_ac, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                               mod.dat = mod.dat.PKE, 
                                                                               var_to_plot = "ECA_age_proxy_forested_only",
                                                                               y_lim = c(0, 60),
                                                                               y_lab = "Equivalent Clearcut Area (%)",
                                                                               CU_poly = PKE_poly_sub,
                                                                               Canada, 
                                                                               USA)
)

haida_plot <- ggarrange(plotlist = CU_plt, ncol = 3, nrow = 2)
haida_plot

ggsave(haida_plot,  filename = here("output_figures_tables","forest_supplement","ECA_CU_PKE_haida_gwa.png"),
       width = 9, height = 4, units = "in")


# Cumulative Disturbed Area panel plots ----
## CHUM CU ----
### Vancouver Island south and central coast ----
vanc_isl <- paste0("CM-", seq(4, 17))

CU_plt <- lapply(vanc_isl, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                               mod.dat = mod.dat.CM,
                                                                               var_to_plot = "haarea_prct_cs",
                                                                               y_lim = c(0, 80),
                                                                               y_lab = "CDA (%)",
                                                                               CU_poly = CM_poly_sub,
                                                                               Canada,
                                                                               USA)
)

vanc_isl_plot <- ggarrange(plotlist = CU_plt, ncol = 4, nrow = 4)
vanc_isl_plot

ggsave(vanc_isl_plot,  filename = here("output_figures_tables","forest_supplement","CD_CU_CHUM_Vanc_Isl.png"),
       width = 12, height = 8, units = "in")

### Haida Gwaii isl and north coast ----
hgwai <-  paste0("CM-", seq(18, 32))

hgwai <- hgwai[hgwai != "CM-29"]

CU_plt <- lapply(vanc_isl, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                               mod.dat = mod.dat.CM,
                                                                               var_to_plot = "haarea_prct_cs",
                                                                               y_lim = c(0, 80),
                                                                               y_lab = "CDA (%)",
                                                                               CU_poly = CM_poly_sub,
                                                                               Canada,
                                                                               USA)
)

hgwai_plot <- ggarrange(plotlist = CU_plt, ncol = 4, nrow = 4)

ggsave(hgwai_plot,  filename = here("output_figures_tables","forest_supplement","CD_CU_CHUM_hgwaii_Isl.png"),
       width = 12, height = 8, units = "in")



## Pink-odd year ----
### Vancouver Island south and central coast ----
#' *PKO-5, PKO-4, PKO-3 and PKO-6* \ PKO-6 contains no salmon recruit information (excluded)
vanc_isl <- paste0("PKO-", seq(2, 8))

vanc_isl <- vanc_isl[vanc_isl %in% unique(mod.dat.PKO$CU)]

CU_plt <- lapply(vanc_isl, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                               mod.dat = mod.dat.PKO,
                                                                               var_to_plot = "haarea_prct_cs",
                                                                               y_lim = c(0, 80),
                                                                               y_lab = "CDA (%)",
                                                                               CU_poly = PKO_poly_sub,
                                                                               Canada,
                                                                               USA)
)

vanc_isl_plot <- ggarrange(plotlist = CU_plt, ncol = 3, nrow = 2)
vanc_isl_plot

ggsave(vanc_isl_plot,  filename = here("output_figures_tables","forest_supplement","CD_CU_PKO_Vanc_Isl.png"),
       width = 9, height = 4, units = "in")

### Haida Gwaii isl and north coast ----
#' *PKO-11, PKO-10, PKO-09* \ only PKO-09 contains salmon recruit information
hgwai <-  paste0("PKO-", seq(9, 18))

hgwai <- hgwai[hgwai %in% unique(mod.dat.PKO$CU)]

CU_plt <- lapply(hgwai, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                            mod.dat = mod.dat.PKO,
                                                                            var_to_plot = "haarea_prct_cs",
                                                                            y_lim = c(0, 80),
                                                                            y_lab = "CDA (%)",
                                                                            CU_poly = PKO_poly_sub,
                                                                            Canada,
                                                                            USA)
)

hgwai_plot <- ggarrange(plotlist = CU_plt, ncol = 4, nrow = 3)
hgwai_plot

ggsave(hgwai_plot,  filename = here("output_figures_tables","forest_supplement","CD_CU_PKO_hgwaii_Isl.png"),
       width = 12, height = 6, units = "in")

## Pink-even years ----
### Vancouver Island south and central coast ----
vanc_isl <- paste0("PKE-", seq(1, 6))

vanc_isl <- vanc_isl[vanc_isl %in% unique(mod.dat.PKE$CU)]

CU_plt <- lapply(vanc_isl, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                               mod.dat = mod.dat.PKE, 
                                                                               var_to_plot = "haarea_prct_cs",
                                                                               y_lim = c(0, 80),
                                                                               y_lab = "CDA (%)",
                                                                               CU_poly = PKE_poly_sub,
                                                                               Canada,
                                                                               USA)
)

vanc_isl_plot <- ggarrange(plotlist = CU_plt, ncol = 2, nrow = 2)
vanc_isl_plot

ggsave(vanc_isl_plot,  filename = here("output_figures_tables","forest_supplement","CD_CU_PKE_Van_south_coast.png"),
       width = 6, height = 4, units = "in")


### Haida Gwaii and North coast ----
haida_ac <- paste0("PKE-", seq(7, 13))

haida_ac <- haida_ac[haida_ac %in% unique(mod.dat.PKE$CU)]

CU_plt <- lapply(haida_ac, function(x) plot_forest_disturance_timeseries_inset(x, 
                                                                               mod.dat = mod.dat.PKE, 
                                                                               var_to_plot = "haarea_prct_cs",
                                                                               y_lim = c(0, 80),
                                                                               y_lab = "CDA (%)",
                                                                               CU_poly = PKE_poly_sub,
                                                                               Canada,
                                                                               USA)
)

haida_plot <- ggarrange(plotlist = CU_plt, ncol = 3, nrow = 2)
haida_plot

ggsave(haida_plot,  filename = here("output_figures_tables","forest_supplement","CD_CU_PKE_haida_gwa.png"),
       width = 9, height = 4, units = "in")

# end 