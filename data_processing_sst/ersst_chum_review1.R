# write new csv data file with river coordinates and with ersst
# read file without duplicated data
#oct 2026 - add new sst data for March for the first half of the  time series

library(tidyverse)
library(here)
# library(ersst)
library(sf)
# library(bcmaps)
library(ggplot2)
library(hues)

# data <- read.csv(here("salmon_forestry_data_analysis","data","chum_SR_20_hat_yr.csv"))
data <- read.csv(here('salmon_forestry_data_analysis','data','chum_SR_20_hat_yr_w_ersst_npgo.csv'))


glimpse(data)


chum_cu <- read.csv(here("data_processing_sst","data","CM_CU_SITES_En.csv"))

glimpse(chum_cu)

chum_cu_subset <- chum_cu %>% 
  select(FULL_CU_IN, X_LONG, Y_LAT, WS_CDE_20K,
         WS_CDE_50K, GFE_ID, CU_NAME)

glimpse(chum_cu_subset)

#check if all GFE ID in data are in chum_cu_subset

data %>% 
  pull(GFE_ID) %>% 
  {all (. %in% chum_cu_subset$GFE_ID)} #TRUE %in% chum_cu_subset$GFE_ID)} #TRUE}
  
# yes

data %>% 
  pull(WATERSHED_CDE) %>% 
  {all (. %in% chum_cu_subset$WS_CDE_50K)} #TRUE %in% chum_cu_subset$GFE_ID)} #TRUE}

#no 

chum_cu_subset <- chum_cu %>% 
  select(FULL_CU_IN, X_LONG, Y_LAT, GFE_ID, CU_NAME) %>% 
  unique()

#left join with lat long data
chum_data_w_coord <- data %>%
  rename("Y_LAT_old" = "Y_LAT", "X_LONG_old" = "X_LONG", "CU_NAME_old" = "CU_NAME") %>%
  left_join(chum_cu_subset, by = c("CU" = "FULL_CU_IN", "GFE_ID" = "GFE_ID"),
       # t(-CU, -GFE_ID) %>%     
       relationship = "many-to-one")

chum_data_w_coord %>% 
  select(River_GFE_ID) %>% 
  unique() %>% 
  nrow()

salmon_data_location <- chum_data_w_coord %>% 
  select(CU,  Y_LAT, X_LONG, River, GFE_ID) %>% 
  distinct()


# read sst df

sst_df <- read.csv(here("data_processing_sst","data","sst_ersst_df_sep2026.csv"))

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

distance_df <- tibble()

# looking at the distances between sst data points and salmon locations 
for(i in 1:nrow(salmon_data_location)){
  for(j in 1:nrow(location_data_long_df_distinct)){
    distance <- haversine(salmon_data_location$Y_LAT[i], salmon_data_location$X_LONG[i], 
                          location_data_long_df_distinct$lat[j], location_data_long_df_distinct$lon[j])
    distance_df <- distance_df %>% bind_rows(data.frame(CU = salmon_data_location$CU[i], 
                                                        River = salmon_data_location$River[i],
                                                        GFE_ID = salmon_data_location$GFE_ID[i],
                                                        sst_ersst_lat = location_data_long_df_distinct$lat[j], 
                                                        sst_ersst_lon = location_data_long_df_distinct$lon[j],
                                                        distance = distance))
    
  }
  
}

# looking the minimum of those distances for each river

min_distance_df <- distance_df %>% 
  group_by(CU, River, GFE_ID) %>% 
  filter(distance == min(distance)) %>% 
  ungroup() 

sst_df_early_spring_variable <- sst_df %>% 
  group_by(lat, lon ,year) %>% 
  filter(month %in% c(3,4,5,6)) %>%
  summarise(early_spring_ersst = mean(sst)) %>%
  ungroup()

sst_df_spring_variable <- sst_df %>% 
  group_by(lat, lon ,year) %>% 
  filter(month %in% c(4,5,6,7)) %>%
  summarise(late_spring_ersst = mean(sst)) %>%
  ungroup() %>% 
  left_join(sst_df_early_spring_variable, by = c("lat" = "lat", "lon" = "lon", "year" = "year")) %>% 
  mutate(spring_ersst_variable = case_when(year <= 1980 ~ late_spring_ersst,
                                           year > 1980 ~ early_spring_ersst))

sst_df_may_june_july <- sst_df %>% 
  group_by(lat, lon ,year) %>% 
  filter(month %in% c(5,6,7)) %>%
  summarise(may_june_july_ersst = mean(sst)) %>%
  mutate(rolling_mean_sst = rollmean(sst, k = 5, fill = NA, align = "right")) %>%()

sst_df_april_may_june <- sst_df %>% 
  group_by(lat, lon ,year) %>% 
  filter(month %in% c(4,5,6)) %>%
  summarise(april_may_june_ersst = mean(sst)) %>%
  ungroup()

sst_df_march_april_may <- sst_df %>% 
  group_by(lat, lon ,year) %>% 
  filter(month %in% c(3,4,5)) %>%
  summarise(march_april_may_ersst = mean(sst)) %>%
  ungroup()

sst_df_spring_variable_3_month <- sst_df_march_april_may %>% 
  left_join(sst_df_april_may_june, by = c("lat" = "lat", "lon" = "lon", "year" = "year")) %>%
  left_join(sst_df_may_june_july, by = c("lat" = "lat", "lon" = "lon", "year" = "year")) %>% 
  mutate(spring_ersst_variable_3_month = case_when(year <= 1974 ~ may_june_july_ersst,
                                                   year > 1974 & year <= 1994 ~ april_may_june_ersst,
                                                   year > 1994 ~ march_april_may_ersst)) %>% 
  left_join(sst_df_spring_variable, by = c("lat" = "lat", "lon" = "lon", "year" = "year"))



salmon_data_distance_temp <- chum_data_w_coord %>% 
    select(-sst_ersst_lat, -sst_ersst_lon, -distance, -sst_ersst_year) %>% 
    left_join(min_distance_df %>% 
                select(CU, River, distance, sst_ersst_lat, sst_ersst_lon),
              by = c("CU" = "CU", "River" = "River")) %>% 
    left_join(sst_df_spring_variable_3_month %>% 
                group_by(lat, lon , year) %>%
                mutate(BroodYear = year-1) %>% #sst fron year n will affect salmon whose BroodYear is n-1
                rename("sst_ersst_year" = "year"),
              by = c("BroodYear" = "BroodYear", "sst_ersst_lat" = "lat", "sst_ersst_lon" = "lon"))

#check how many rows have NA for spring_ersst

salmon_data_distance_temp %>% 
  filter(is.na(spring_ersst)) %>% 
  nrow()
#none

glimpse(salmon_data_distance_temp)

# look at all combinations of River_GFE_ID and BroodYears

salmon_data_distance_temp %>% 
  select(River_GFE_ID, BroodYear) %>% 
  group_by(River_GFE_ID, BroodYear) %>% 
  summarise(num_years = n()) %>% 
  # filter(num_years > 1) %>% 
  View()

salmon_data_distance_temp %>% 
  select(River_GFE_ID, BroodYear) %>% 
  group_by(River_GFE_ID) %>% 
  summarise(num_years = n()) %>% 
  # filter(num_years > 1) %>% 
  View()

# url = "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat"
# 
# pdo = read.table(url, header = TRUE, skip = 1) #, col.names = c("year", "month", "pdo"))
# 
# # convert to long format
# 
# pdo_long <- pdo %>%
#   pivot_longer(cols = -c(Year), names_to = "month", values_to = "pdo")
# 
# pdo_long_annual <- pdo_long %>%
#   group_by(Year) %>%
#   summarise(pdo = mean(pdo))
# 
# 
# salmon_data_distance_temp_pdo <- salmon_data_distance_temp %>% 
#   left_join(pdo_long_annual, by = c('sst_ersst_year' = 'Year')) %>% 
#   mutate(pdo.std = (pdo - mean(pdo))/sd(pdo))
# 
# salmon_data_distance_temp_pdo %>% 
#   select(River_GFE_ID, BroodYear) %>% 
#   group_by(River_GFE_ID, BroodYear) %>% 
#   summarise(num_years = n()) %>% 
#   filter(num_years > 1) %>%
#   View()

#save df

write.csv(salmon_data_distance_temp, 
          here("salmon_forestry_data_analysis",
               "data",
               "chum_SR_20_hat_yr_w_variable_ersst_npgo.csv"), row.names = FALSE)

#plot correlation between late_spring_ersst and spring_ersst_variable

ggplot(salmon_data_distance_temp)+
  geom_point(aes(x = late_spring_ersst, y = spring_ersst_variable), alpha = 0.5)+
  annotate("text", x = 10, y = 15, 
           label = paste("correlation =", round(cor(salmon_data_distance_temp$late_spring_ersst, salmon_data_distance_temp$spring_ersst_variable), 2))) +
  geom_abline() +
  theme_classic() +
  labs(x = "Late spring SST", y = "Variable spring SST")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

ggplot(salmon_data_distance_temp)+
  geom_point(aes(x = late_spring_ersst, y = spring_ersst_variable_3_month), alpha = 0.5)+
  annotate("text", x = 10, y = 15, 
           label = paste("correlation =", round(cor(salmon_data_distance_temp$late_spring_ersst, salmon_data_distance_temp$spring_ersst_variable_3_month), 2))) +
  geom_abline() +
  theme_classic() +
  labs(x = "Late spring SST", y = "Variable spring SST (3 month)")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  
  
  
  
  
  
