library(tidyverse)
library(here)
# library(ersst)
library(sf)
library(bcmaps)
library(ggplot2)
library(hues)

data_pink_e <- read.csv(here("salmon_forestry_data_analysis","data","pke_SR_10_hat_yr_reduced_VRI90.csv"))
data_pink_o <- read.csv(here("salmon_forestry_data_analysis","data","PKO_SR_10_hat_yr_reduced_VRI90.csv"))

glimpse(data_pink_e)

pke_cu <- read.csv(here("data_processing_sst","data","PKE_CU_SITES_En.csv"))

glimpse(pke_cu)

pko_cu <- read.csv(here("data_processing_sst","data","PKO_CU_SITES_En.csv"))

pke_cu_subset <- pke_cu %>% 
  select(FULL_CU_IN, X_LONG, Y_LAT, WS_CDE_20K,
         WS_CDE_50K, GFE_ID, CU_NAME)

pko_cu_subset <- pko_cu %>% 
  select(FULL_CU_IN, X_LONG, Y_LAT, WS_CDE_20K,
         WS_CDE_50K, GFE_ID, CU_NAME)

glimpse(pko_cu_subset)

data_pink_o %>% 
  pull(GFE_ID) %>% 
  {all (. %in% pko_cu_subset$GFE_ID)} #TRUE %in% chum_cu_subset$GFE_ID)} #TRUE}

data_pink_e %>% 
  pull(GFE_ID) %>% 
  {all (. %in% pke_cu_subset$GFE_ID)} #TRUE %in% chum_cu_subset$GFE_ID)} #TRUE}

pke_cu_subset <- pke_cu %>% 
  select(FULL_CU_IN, X_LONG, Y_LAT, GFE_ID, CU_NAME) %>% 
  unique()

pko_cu_subset <- pko_cu %>%
  select(FULL_CU_IN, X_LONG, Y_LAT, GFE_ID, CU_NAME) %>% 
  unique()



#left join with lat long data
pke_data_w_coord <- data_pink_e %>% 
  left_join(pke_cu_subset, by = c("CU" = "FULL_CU_IN", "GFE_ID" = "GFE_ID"),
            # t(-CU, -GFE_ID) %>%     
            relationship = "many-to-one")

pko_data_w_coord <- data_pink_o %>%
  left_join(pko_cu_subset, by = c("CU" = "FULL_CU_IN", "GFE_ID" = "GFE_ID"),
            # t(-CU, -GFE_ID) %>%     
            relationship = "many-to-one")


pke_data_w_coord %>% 
  select(River_GFE_ID) %>% 
  unique() %>% 
  nrow()

pko_data_w_coord %>% 
  select(River_GFE_ID) %>% 
  unique() %>% 
  nrow()


salmon_data_pink_e_location <- pke_data_w_coord %>% 
  select(CU,  Y_LAT, X_LONG, River, GFE_ID) %>% 
  distinct()


# read sst df

sst_df <- read.csv(here("data_processing_sst","data","sst_ersst_df.csv"))

haversine <- function(lat1, lon1, lat2, lon2) {
  ## This function computes the great circle distance between two points given
  ## their latitiude and longitude (in decimal degrees) using the haversine
  ## formula. The output is the distance between the two points in km.
  ##
  ## lat1 = latitude of first point
  ## lon1 = longitude of first point
  ## lat2 = latitude of second point
  ## lon2 = longitude of second point
  
  # Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  R <- 6371 # earth mean radius [km]
  delta.lon <- (lon2 - lon1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.lon/2)^2
  d <- 2 * R * asin(min(1, sqrt(a)))
  
  return(d) # distance in km
}

#distinct location of sst data

location_data_long_df_distinct <- sst_df %>% 
  # mutate(lon = ifelse(lon > 180, lon - 360, lon)) %>%
  filter(!is.na(sst)) %>%
  select(lat, lon) %>% 
  distinct()



