library(tidyverse)
library(here)
# library(ersst)
library(sf)
library(bcmaps)
library(ggplot2)
library(hues)


data_pink_e <- read.csv(here('salmon_forestry_data_analysis','data',"pke_SR_10_hat_yr_w_ersst_npgo.csv"))

#odd year pinks
data_pink_o <- read.csv(here('salmon_forestry_data_analysis','data',"pko_SR_10_hat_yr_w_ersst_npgo.csv"))


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
  select(-X_LONG, -Y_LAT, CU_NAME) %>%
  left_join(pke_cu_subset, by = c("CU" = "FULL_CU_IN", "GFE_ID" = "GFE_ID"),
            # t(-CU, -GFE_ID) %>%     
            relationship = "many-to-one")

pko_data_w_coord <- data_pink_o %>%
  select(-X_LONG, -Y_LAT, CU_NAME) %>%
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

salmon_data_pink_o_location <- pko_data_w_coord %>% 
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


distance_df_pink_e <- tibble()

# looking at the distances between sst data points and salmon locations 
for(i in 1:nrow(salmon_data_pink_e_location)){
  for(j in 1:nrow(location_data_long_df_distinct)){
    distance <- haversine(salmon_data_pink_e_location$Y_LAT[i], salmon_data_pink_e_location$X_LONG[i], 
                          location_data_long_df_distinct$lat[j], location_data_long_df_distinct$lon[j])
    distance_df_pink_e <- distance_df_pink_e %>% bind_rows(data.frame(CU = salmon_data_pink_e_location$CU[i], 
                                                        River = salmon_data_pink_e_location$River[i],
                                                        GFE_ID = salmon_data_pink_e_location$GFE_ID[i],
                                                        sst_ersst_lat = location_data_long_df_distinct$lat[j], 
                                                        sst_ersst_lon = location_data_long_df_distinct$lon[j],
                                                        distance = distance))
    
  }
  
}

distance_df_pink_o <- tibble()

# looking at the distances between sst data points and salmon locations 
for(i in 1:nrow(salmon_data_pink_o_location)){
  for(j in 1:nrow(location_data_long_df_distinct)){
    distance <- haversine(salmon_data_pink_o_location$Y_LAT[i], salmon_data_pink_o_location$X_LONG[i], 
                          location_data_long_df_distinct$lat[j], location_data_long_df_distinct$lon[j])
    distance_df_pink_o <- distance_df_pink_o %>% bind_rows(data.frame(CU = salmon_data_pink_o_location$CU[i], 
                                                                      River = salmon_data_pink_o_location$River[i],
                                                                      GFE_ID = salmon_data_pink_o_location$GFE_ID[i],
                                                                      sst_ersst_lat = location_data_long_df_distinct$lat[j], 
                                                                      sst_ersst_lon = location_data_long_df_distinct$lon[j],
                                                                      distance = distance))
    
  }
  
}

# looking the minimum of those distances for each river

min_distance_df_pink_e <- distance_df_pink_e %>% 
  group_by(CU, River, GFE_ID) %>% 
  filter(distance == min(distance)) %>% 
  ungroup() 

min_distance_df_pink_o <- distance_df_pink_o %>% 
  group_by(CU, River, GFE_ID) %>% 
  filter(distance == min(distance)) %>% 
  ungroup() 

sst_df_spring <- sst_df %>% 
  group_by(lat, lon ,year) %>% 
  summarise(spring_ersst = mean(sst)) %>%
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
  ungroup()

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

salmon_pink_e_data_distance_temp <- pke_data_w_coord %>% 
  select(-sst_ersst_lat, -sst_ersst_lon, -distance, -sst_ersst_year) %>% 
  left_join(min_distance_df_pink_e %>% 
              select(CU, River, distance, sst_ersst_lat, sst_ersst_lon, GFE_ID),
            by = c("CU" = "CU", "River" = "River", "GFE_ID"="GFE_ID")) %>% 
  left_join(sst_df_spring_variable_3_month %>% 
              group_by(lat, lon , year) %>%
              mutate(BroodYear = year-1) %>% #sst fron year n will affect salmon whose BroodYear is n-1
              rename("sst_ersst_year" = "year"),
            by = c("BroodYear" = "BroodYear", "sst_ersst_lat" = "lat", "sst_ersst_lon" = "lon"))


salmon_pink_o_data_distance_temp <- pko_data_w_coord %>% 
  select(-sst_ersst_lat, -sst_ersst_lon, -distance, -sst_ersst_year) %>% 
  left_join(min_distance_df_pink_o %>% 
              select(CU, River, distance, sst_ersst_lat, sst_ersst_lon, GFE_ID),
            by = c("CU" = "CU", "River" = "River", "GFE_ID"="GFE_ID")) %>% 
  left_join(sst_df_spring_variable_3_month %>% 
              group_by(lat, lon , year) %>%
              mutate(BroodYear = year-1) %>% #sst fron year n will affect salmon whose BroodYear is n-1
              rename("sst_ersst_year" = "year"),
            by = c("BroodYear" = "BroodYear", "sst_ersst_lat" = "lat", "sst_ersst_lon" = "lon"))



#check how many rows have NA for spring_ersst

salmon_pink_e_data_distance_temp %>% 
  filter(is.na(spring_ersst)) %>% 
  nrow()
#none

salmon_pink_o_data_distance_temp %>% 
  filter(is.na(spring_ersst)) %>% 
  nrow()
#none

glimpse(salmon_pink_e_data_distance_temp)

glimpse(salmon_pink_o_data_distance_temp)


#check differences between current dataset and this dataset
# old_data <- read.csv(here("..","coastwide-salmon-forestry","origional-ecofish-data-models","Data","Processed",
#                           "pke_SR_10_hat_yr_w_ersst.csv"))
# summary(arsenal::comparedf(salmon_pink_e_data_distance_temp, old_data))





write.csv(salmon_pink_e_data_distance_temp, here("salmon_forestry_data_analysis",
                                                "data",
                                                "pke_SR_10_hat_yr_w_variable_ersst_npgo.csv"), row.names = FALSE)


write.csv(salmon_pink_o_data_distance_temp, here("salmon_forestry_data_analysis",
                                                "data",
                                                "pko_SR_10_hat_yr_w_variable_ersst_npgo.csv"), row.names = FALSE)


#plot correlation between late_spring_ersst and spring_ersst_variable

ggplot(salmon_pink_e_data_distance_temp)+
  geom_point(aes(x = late_spring_ersst, y = spring_ersst_variable), alpha = 0.5)+
  annotate("text", x = 10, y = 15, 
           label = paste("correlation =", round(cor(salmon_pink_e_data_distance_temp$late_spring_ersst, salmon_pink_e_data_distance_temp$spring_ersst_variable), 2))) +
  geom_abline() +
  theme_classic() +
  labs(x = "Late spring SST", y = "Variable spring SST")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

ggplot(salmon_pink_o_data_distance_temp)+
  geom_point(aes(x = late_spring_ersst, y = spring_ersst_variable), alpha = 0.5)+
  annotate("text", x = 10, y = 15, 
           label = paste("correlation =", round(cor(salmon_pink_o_data_distance_temp$late_spring_ersst, salmon_pink_o_data_distance_temp$spring_ersst_variable), 2))) +
  geom_abline() +
  theme_classic() +
  labs(x = "Late spring SST", y = "Variable spring SST")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))


ggplot(salmon_pink_o_data_distance_temp)+
  geom_point(aes(x = late_spring_ersst, y = spring_ersst_variable_3_month), alpha = 0.5)+
  annotate("text", x = 10, y = 15, 
           label = paste("correlation =", round(cor(salmon_pink_o_data_distance_temp$late_spring_ersst, salmon_pink_o_data_distance_temp$spring_ersst_variable_3_month), 2))) +
  geom_abline() +
  theme_classic() +
  labs(x = "Late spring SST", y = "Variable spring SST")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))



