# Load libraries

library(tidyverse)
library(here)
library(ersst)
library(sf)
library(bcmaps)
library(ggplot2)
library(hues)

# # Load data
# 
# data <- read.csv(here("salmon_forestry_data_analysis","data","chum_SR_20_hat_yr.csv"))
# data_pink_e <- read.csv(here("salmon_forestry_data_analysis","data","pke_SR_10_hat_yr_reduced_VRI90.csv"))
# data_pink_o <- read.csv(here("salmon_forestry_data_analysis","data","PKO_SR_10_hat_yr_reduced_VRI90.csv"))

min_max <- rbind(data %>% 
                   select(Y_LAT, X_LONG), data_pink_e %>% 
                   select(Y_LAT, X_LONG), data_pink_o %>% 
                   select(Y_LAT, X_LONG)) %>% 
  summarise_all(list(min = min, max = max))


years <- rbind(data %>% 
                 select(BroodYear), data_pink_e %>% 
                 select(BroodYear), data_pink_o %>% 
                 select(BroodYear)) %>% 
  distinct() %>% 
  arrange(BroodYear) %>% 
  mutate(sst_year = BroodYear + 1)


# sst_download(years = years$sst_year, months = 4:7, save.dir = here("data_processing", "sst_ersst"),
#              version = 5)

sst_download(years = 1996:2014, months = 4:7, save.dir = here("data_processing", "sst_ersst"),
                          version = 5)

sst <- sst_load(years$sst_year, 4:7, here("data_processing", "sst_ersst"), version = 5)


# subset data 

sst_subset <- sst_subset_space(sst, 
                               lat.min = min_max$Y_LAT_min-2, 
                               lat.max = min_max$Y_LAT_max,
                               lon.min = min_max$X_LONG_min -2 + 360,
                               lon.max = min_max$X_LONG_max + 360)

sst_df <- sst_dataframe(sst_subset) %>% 
  mutate(lon = ifelse(lon > 180, lon - 360, lon)) 

#save sst_df

write.csv(sst_df, here("data_processing_sst", "data", "sst_ersst_df.csv"), row.names = FALSE)


#plot
# 
# bc_boundary <- bc_bound() %>% st_transform(4326)
# 
# sst_df %>% filter(!is.na(sst), year == 1959 | year == 2014) %>% 
#   ggplot() +
#   geom_sf(data = bc_boundary, fill = "transparent", color = "slategray", alpha = 0.2) +
#   facet_wrap(~year) +
#   geom_raster(aes(x = lon, y = lat, fill = sst), alpha = 0.5) +
#   #plot locations of sst data
#   geom_point(aes(x = lon, y = lat), color = "slategray", alpha = 0.6) +
#   scale_fill_viridis_c() +
#   # geom_point(data = lighthouse_locations, aes(x = long, y = lat), color = "darkred", size = 3, alpha=0.8) +
#   geom_point(data=salmon_data_location, aes(x = X_LONG, y = Y_LAT, color = "chum"), size = 2, alpha=0.2) +
#   geom_point(data=pke_salmon_data_location, aes(x = X_LONG, y = Y_LAT, color = "pink-even"), size = 2, alpha=0.2) +
#   geom_point(data=pko_salmon_data_location, aes(x = X_LONG, y = Y_LAT, color = "pink-odd"), size = 2, alpha=0.2) +
#   # geom_text(data = lighthouse_locations, aes(x = long, y = lat, label = location), 
#   #           nudge_x = -1.5, nudge_y = 0.2, size = 3) +
#   # ggrepel::geom_label_repel(data = lighthouse_locations, aes(x = long, y = lat, label = location),
#   #                           nudge_x = -1.5, nudge_y = 0.2, size = 3, background = "white", alpha = 0.5) +
#   scale_color_manual(values = c("chum" = "#69C5C5",
#                                 "pink-even" = "#C76F6F",
#                                 "pink-odd" = "#9E70A1")) +
#   scale_x_continuous(limits = c(-134, -122), breaks = seq(-134,-122,5)) +
#   scale_y_continuous(limits = c(48, 58)) +
#   guides(color = guide_legend(title = "Species"), override.aes = list(size = 4, alpha = 1),
#          fill = guide_legend(title = "Spring SST (C)")) +
#   labs(title = "Spring ERSST") + 
#   xlab("Longitude") +
#   ylab("Latitude") +
#   theme_classic()+
#   theme(legend.position = "right",
#         strip.background = element_blank(),
#         strip.text = element_text(size = 14),
#         legend.title = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         title = element_text(size = 16),
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 10))
# 
# ggsave(here("figures", "sstersst_1959_2014.png"), width = 10, height = 5, dpi = 300)
# 











  
  