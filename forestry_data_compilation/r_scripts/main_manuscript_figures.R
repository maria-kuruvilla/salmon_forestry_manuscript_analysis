## Script name: Manuscript figures
##
## Purpose of the script: 
##
## Author: Joao Braga
##
## Date Created: 2026-05-05
##
## Trinity Consultants Canada (Victoria Office)
## Email: joao.braga@trinityconsultants.com

 
# Required packages ----
list.of.packages <- c("tidyverse", "extrafont", "lubridate", "ggplot2", "sf", "qs", "openxlsx", "stringi","rnaturalearth", "scales", "ggpubr", "giscoR", "rmapshaper", "patchwork") 
 
# What packages need to be installed?
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])] 

## Install missing packages
if (length(new.packages)) {
install.packages(new.packages, dependencies = TRUE, repos = c("http://cran.rstudio.com/", "http://R-Forge.R-project.org"))
}

## Loading libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))

scale_round <- function(x) round(x, digits = 2)
wrapper <- function(x, ...) paste(stri_wrap(x, ...), collapse = "\n")

source("forestry_data_compilation/r_functions/plot_returns_ECA_CPD.R")

# Read-in Natural Resource districts 
nr_dist <- st_read(dsn = "forestry_data_compilation/inputs/salmon_datasets_plotting/nat_res_districts.gpkg")

# Salmon watersheds and ECA & CDA stats
pop_sheds <- st_read(dsn = "forestry_data_compilation/inputs/salmon_datasets_plotting/salmon_watersheds.gpkg")

# Chum salmon
chum_dat_plt <- read_csv("forestry_data_compilation/inputs/salmon_datasets_plotting/CM_case_studies_data.csv")

# Data for plot
world <- ne_countries(scale='medium',returnclass = 'sf')
Canada <- subset(world, admin == "Canada")
USA <- subset(world, admin == "United States of America")


# Coast wide CDA plot ----
## Single time series Plot ----
split(chum_dat_plt, chum_dat_plt$River) %>%
  lapply(., function(x) returns_ECA_CPD_plot(dat_plot = x, river_str = unique(x$River), 
                                             save_plot = T,  
                                             filename = paste0("forestry_data_compilation/Plots/manuscript_figures/regional_plot/", unique(x$River),".png"), 
                                             width = 5, 
                                             height = 2.5, 
                                             units = "in"))

returns_ECA_CPD_plot(dat_plot = chum_dat_plt, river_str = "NIMPKISH RIVER",
                     save_plot = F,
                     filename = paste0("forestry_data_compilation/Plots/manuscript_figures/regional_plot/", unique(x$River),".png"),
                     width = 5*0.75,
                     height = 2.5*0.75,
                     units = "in")

b <- st_bbox(pop_sheds) 
bbox <- st_as_sfc(b)

high_lighted_sheds <- pop_sheds %>% filter(outlet_lfid %in% unique(chum_dat_plt$LINEAR_FEATURE_ID))

## Inset map ----
wld <- gisco_get_countries(resolution = "20")

wld <- wld |>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON")

wld <- st_make_valid(wld)

ortho_crs <-'+proj=ortho +lat_0=45 +lon_0=-126 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>% 
  st_sfc(crs = ortho_crs)

world <-   st_intersection(wld, st_transform(ocean, 4326)) %>%
  st_transform(crs = ortho_crs)

world_line <- ms_innerlines(world)

wld_map <- ggplot(world) +
  geom_sf(data = ocean, fill = "#deebf7", linewidth = .2) +
  geom_sf(fill = "grey50", 
          colour = NA,
          show.legend = F) +
  geom_sf(data = world_line, linewidth = .05, colour = "white") +
  geom_sf(data = bbox, col = "red", fill = NA, linewidth = 0.5) +
  scale_fill_manual(values = c("grey50", "red")) + 
  theme_void()

# binned
breaks_p <- seq(0, 80, 20)

## Spatial Map ----
base_map <- ggplot() +
  # Base map
  geom_sf(data = Canada, fill = "grey90", color = NA) +
  geom_sf(data = USA, fill = "grey90", color = NA) +
  
  # ECA map
  geom_sf(data = pop_sheds, aes(fill = haarea_prct_css_current), linewidth = 0.1, color = "black") +
  
  geom_sf(data = high_lighted_sheds, linewidth = 0.5, color = "black", fill = NA) +
  
  # JFB: Oct 25 color update
  scale_fill_stepsn(colors = c('#35978f', "#7ba9a4", "gray" , "#c49f77",'#bf812d'), breaks = breaks_p) +
  
  # scale_fill_stepsn(colors = c("#1a9641", "#a6d96a", "#ffffbf" , "#fdae61","#d7191c"), breaks = breaks_p) +
  # scale_color_gradient2(low = "#134e5a", mid = "#4699a7", high = "#f79f43") +
  
  labs(x = "", y = "", fill = "Cumulative Disturbed Area in 2022 (%):", color = "Cumulative Disturbed Area in 2022 (%):") +
  
  # Windows
  coord_sf(crs = st_crs(pop_sheds), xlim = c(b["xmin"] - 100000, b["xmax"] + 100000) , ylim = c(b["ymin"] - 4000, b["ymax"] + 86000)) +
  
  # Theme
  theme(panel.grid.major = element_line(colour = "aliceblue", linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"), 
        panel.border = element_rect(fill = NA, color = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        base_family = "ArcherPro Book",
        legend.position = "bottom") +
  
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("black", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    # pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("black", "white"),
      line_col = "black",
      text_family = "ArcherPro Book"
    )
  )

b2 <- base_map + inset_element(wld_map, left = 0.71, bottom = 0.04, right = 1.085, top = .12, align_to = "plot")

# ggsave(plot = b2, filename = "forestry_data_compilation/Plots/manuscript_figures/regional_plot/base_CPD_bin_map2_Feb26.png", width = 16, height = 12, units = "in")

# Coastwide trends in equivalent clearcut area and cumulative disturbed area ----
## Data frame with specific data and events
text_note <- data.frame(BroodYear = c(1890, 
                                      1912,
                                      1970,
                                      2013,
                                      2010, 
                                      1935,
                                      1970),
                        value = c(9, 17, 12, 23.5, 13, 24, 24),
                        event = c("Canadian Pacific Railway completed (1886)",
                                  "Trucks main source of log transportation (1940’s)",
                                  "Haida Gwaii protests",
                                  "Forest Practices Code Implementation (1995)",
                                  "Great Bear Rainforest Agreement",
                                  "Salmon data time series begins ~1950",
                                  "Clayoquot protests"),
                        name = "Equivalent Clearcut Area (%)")

text_note$name <- as.factor(text_note$name)

text_note <- text_note %>%
  rowwise() %>%
  mutate(event_w = wrapper(event, width = 20)) %>%
  ungroup()

text_arrows <- data.frame(x.st = c(1890, 1912, 1970, 2008, 2010, 1935, 1970),
                          x.nd = c(1886, 1945, 1985, 1995, 2002, 1950, 1993),
                          y.st = c(6.5, 13.5, 11.4, 21.5, 11.5, 22, 23.3),
                          y.nd = c(2.8, 8, 10, 20, 10.5, 15, 19.5),
                          name =  "Equivalent Clearcut Area (%)")

text_arrows$name <- as.factor(text_note$name)

#' These estimations of ECA between 1820 to 2022 do not consider permanent disturbance. For plot only.
ECA_estimates_NPD <- read_csv("forestry_data_compilation/inputs/forestry_data/forestry_data_without_perm_dist_timeseries.csv") 

eca_NPD <- ECA_estimates_NPD %>%
  rename(outlet_lfid  = LINEAR_FEATURE_ID ,
         ECA_year = year ) %>%
  mutate(BroodYear = as.numeric(ECA_year), 
         outlet_lfid = as.character(outlet_lfid), 
         ECA_year = as.numeric(ECA_year))

metaECA_NPD <- read_csv("forestry_data_compilation/inputs/forestry_data/forestry_data_without_perm_dist_metadata.csv")

metaECA_NPD <- select(metaECA_NPD, outlet_lfid = this_lfid, portion_reporting_vri_cover1_missing) %>%
  mutate(  outlet_lfid = as.character(outlet_lfid))

### Wrangling ----
pop_sheds_nrdist <- st_drop_geometry(pop_sheds) |>
  select(outlet_lfid, wgc, ORG_UNIT)

mod.dat_NPD <- eca_NPD %>% 
  left_join(., pop_sheds_nrdist , by = "outlet_lfid") %>%
  left_join(., metaECA_NPD, by = "outlet_lfid")

mod.dat_NPD <- mod.dat_NPD %>% 
  select(wgc , outlet_lfid, ORG_UNIT, BroodYear, ECA_age_proxy_forested_only, haarea_prct_cs) %>%
  filter(complete.cases(.))

# Grouping
mod.dat_NPD <- mod.dat_NPD %>%
  mutate(Region = case_when(
    ORG_UNIT %in% c("DKM", "DND", "DSS", "DVA") ~ "North Coast - Skeena",
    ORG_UNIT %in% c("DSC", "DSQ", "DCK", "DCS") ~ "South Coast",
    ORG_UNIT %in% c("DQC") ~ "Haida Gwaii",
    ORG_UNIT %in% c("DNI", "DCC") ~ "North Island - Central Coast",
    ORG_UNIT %in% c("DCR") ~ "Campbell River",
    ORG_UNIT %in% c("DSI") ~ "South Island")) %>%
  group_by(Region) %>%
  mutate(U_sheds = length(unique(outlet_lfid ))) %>%
  rowwise() %>%
  mutate(Region = paste0(Region, " (n = ", U_sheds, ")")) %>%
  ungroup() %>%
  mutate(ECA_age_proxy_forested_only = ECA_age_proxy_forested_only * 100)

unique(mod.dat_NPD$Region)

# Pivot longer
mod.dat_NPD_plot <- mod.dat_NPD %>%
  pivot_longer(ECA_age_proxy_forested_only:haarea_prct_cs ) %>%
  mutate(name = gsub(pattern = "ECA_age_proxy_forested_only", replacement = "Equivalent Clearcut Area (%)", x = name)) %>%
  mutate(name = gsub(pattern = "haarea_prct_cs", replacement = "Cumulative Disturbed Area (%)", x = name ),
         name = factor(name, levels = c("Equivalent Clearcut Area (%)",  "Cumulative Disturbed Area (%)"))) 


### Plotting ----
colrs <- c("#e64b35", "#3c5388", "#2ca02c", "#8a4198", "#eea236", "#8f4c2d")#, "#fdbf6f")
names(colrs) <- c("North Coast - Skeena", 
                  "South Coast", 
                  "Haida Gwaii",
                  "North Island - Central Coast",
                  "Campbell River", 
                  "South Island")

colrs_sheds <- c("#e64b35", "#3c5388", "#2ca02c", "#8a4198", "#eea236", "#8f4c2d")#, "#fdbf6f")
names(colrs_sheds) <- c("North Coast - Skeena (n = 544)" , 
                        "South Coast (n = 274)",
                        "Haida Gwaii (n = 197)" ,
                        "North Island - Central Coast (n = 349)",
                        "Campbell River (n = 150)",
                        "South Island (n = 217)")

nr_dist <- nr_dist  %>%
  mutate(Region = case_when(
    ORG_UNIT %in% c("DKM", "DND", "DSS", "DVA") ~ "North Coast - Skeena",
    ORG_UNIT %in% c("DSC", "DSQ", "DCK", "DCS") ~ "South Coast",
    ORG_UNIT %in% c("DQC") ~ "Haida Gwaii",
    ORG_UNIT %in% c("DNI", "DCC") ~ "North Island - Central Coast",
    ORG_UNIT %in% c("DCR") ~ "Campbell River",
    ORG_UNIT %in% c("DSI") ~ "South Island"))  %>%
  filter(!is.na(Region))


CU_ECA_NPD <- ggplot(mod.dat_NPD_plot, 
                     aes(x = BroodYear,
                         y = value)) +
  
  geom_smooth(method = "gam",
              formula = y ~ s(x, k = 10),
              se = TRUE, aes(color = Region, fill = Region),
              level = 0.95,
              size = 0.5) +
  
  facet_wrap(~name, ncol = 1, scales = "free_y", strip.position = "left") +
  
  # geom_text(data = text_note, aes(label = event_w), size = 4, colour = "white", fontface = "bold") +
  geom_label(data = text_note, aes(label = event_w), size = 4, alpha = 0.3, label.size = 0) +
  
  
  geom_segment(data = text_arrows, aes(x = x.st, y = y.st, xend = x.nd, yend = y.nd), arrow = arrow(length = unit(0.1, "cm"))) +
  
  ggh4x::facetted_pos_scales(y = list( 
    
    name ==  "Equivalent Clearcut Area (%)" ~   scale_y_continuous(labels = scale_round, breaks = seq(0, 25, 5), limits = c(0, 25)),
    name ==  "Cumulative Disturbed Area (%)" ~   scale_y_continuous(labels = scale_round, breaks = seq(0, 80, 20))
    
    
  )) +
  scale_x_continuous(breaks = seq(1820, 2022, 20), 
                     limits = c(1883, 2022)) +
  
  scale_color_manual(values = colrs_sheds) + 
  scale_fill_manual(values = colrs_sheds) +
  labs(x = "", y = "", color = "", fill = "") +
  theme_bw() + 
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    strip.text =  element_text(size = 14, face = "bold"), 
    legend.text = element_text(size = 10),
    legend.position="none", 
    legend.box = "vertical",
    
    strip.placement = "outside",
    strip.background = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = NA), nrow = 2, by = TRUE), 
         color = "none")

# Inset map
b <- st_bbox(pop_sheds)

inset.plt <- ggplot() +
  theme_void() + 
  theme(plot.background =  element_rect( colour = "black", fill = "aliceblue")) +
  geom_sf(data = Canada, fill = alpha(colour = "gray5", alpha = 0.3), color = alpha(colour = "gray5", alpha = 0), size = 0)  +
  geom_sf(data = USA, fill = alpha(colour = "gray5", alpha = 0.3), color = alpha(colour = "gray5", alpha = 0), size = 0)  +
  geom_sf(data = pop_sheds, aes(fill = Region, color = Region)) +
  scale_color_manual(values = colrs) + 
  scale_fill_manual(values = colrs) +
  coord_sf(crs = st_crs(nr_dist), xlim = c(b["xmin"], b["xmax"]) , ylim = c(b["ymin"], b["ymax"])) +
  theme(legend.position = "none")  

ECA_plot.with.inset_NPD <- CU_ECA_NPD + patchwork::inset_element(inset.plt, 
                                                                 left = 0.082, 
                                                                 bottom = 0.8, 
                                                                 right = 0.235, 
                                                                 top = 1, 
                                                                 align_to = "plot")

x11()
ECA_plot.with.inset_NPD

# ggsave(ECA_plot.with.inset_NPD,  filename = "forestry_data_compilation/Plots/manuscript_figures/Forest_disturbance_Region_1880_No_PD_feb26.png",
#        width = 9, height = 8, units = "in")

# END