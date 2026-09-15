# use all salmon watersheds within the CU to calculate
# the latest average (2022) CDA within the CU

# Required packages ----
list.of.packages <- c("tidyverse", "extrafont", "giscoR", "ggplot2", "sf", "rmapshaper", "rnaturalearth", "patchwork", "here", "scatterpie") 

#install packages that need installing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

## Install missing packages
if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE, repos = c("http://cran.rstudio.com/", "http://R-Forge.R-project.org"))
}


## Loading libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))

# read the forestry_data_timeseries data
forestry_data_timeseries <- read.csv(here("forestry_data_compilation", "inputs",
                                          "forestry_data", 
                                          "forestry_data_timeseries.csv"))



forestry_data_timeseries_2022 <- forestry_data_timeseries %>% 
  group_by(group,LINEAR_FEATURE_ID) %>% 
  filter(year == max(year)) %>%
  select(haarea_prct_cs, year) %>% 
  rename(haarea_prct_cs_2022 = haarea_prct_cs, year_2022 = year) 

ch20rsc <- read.csv(here('salmon_forestry_data_analysis','data','chum_SR_20_hat_yr_w_ersst_npgo.csv'))

#two rivers with duplicated names:
ch20rsc$River=ifelse(ch20rsc$WATERSHED_CDE=='950-169400-00000-00000-0000-0000-000-000-000-000-000-000','SALMON RIVER 2',ch20rsc$River)
ch20rsc$River=ifelse(ch20rsc$WATERSHED_CDE=="915-486500-05300-00000-0000-0000-000-000-000-000-000-000",'LAGOON CREEK 2',ch20rsc$River)


ch20rsc=ch20rsc[order(factor(ch20rsc$River),ch20rsc$BroodYear),]

ch20rsc$River_n <- as.numeric(factor(ch20rsc$River))

#normalize ECA 2 - square root transformation (ie. sqrt(x))
ch20rsc$sqrt.ECA=sqrt(ch20rsc$ECA_age_proxy_forested_only)
ch20rsc$sqrt.ECA.std=(ch20rsc$sqrt.ECA-mean(ch20rsc$sqrt.ECA))/sd(ch20rsc$sqrt.ECA)

#normalize CPD 2 - square root transformation (ie. sqrt(x))
ch20rsc$sqrt.CPD=sqrt(ch20rsc$disturbedarea_prct_cs)
ch20rsc$sqrt.CPD.std=(ch20rsc$sqrt.CPD-mean(ch20rsc$sqrt.CPD))/sd(ch20rsc$sqrt.CPD)

ch20rsc$npgo.std=(ch20rsc$npgo-mean(ch20rsc$npgo))/sd(ch20rsc$npgo)
ch20rsc$sst.std=(ch20rsc$spring_ersst-mean(ch20rsc$spring_ersst))/sd(ch20rsc$spring_ersst)

cu = distinct(ch20rsc,.keep_all = T)

cu_n = as.numeric(factor(cu$CU))

ch20rsc$CU_n <- cu_n

# make CU_NAME values Title case instead of all caps
ch20rsc$CU_name <- str_to_title(ch20rsc$CU_NAME)
ch20rsc$CU_name <- ifelse(ch20rsc$CU_name == "East Hg", "East Haida Gwaii", ch20rsc$CU_name)

# forestry timeseries has linear feature id and the ch20rsc has GFE_ID
# I need a dataset that has both to combine the two datasets


lookup_og <- read.csv(here("forestry_data_compilation", "inputs","salmon_datasets_plotting", "salmon_watersheds_lookup.csv"))

# check number of unique combinations GFE_ID, LINEAR_FEATURE_ID

length(unique(lookup_og$LINEAR_FEATURE_ID))
length(unique(lookup_og$GFE_ID))
lookup_og %>% 
  filter(Species == "CM") %>% 
  select(LINEAR_FEATURE_ID, Species, CU, River) %>% 
  distinct() %>% 
  summarize(num = n_distinct(LINEAR_FEATURE_ID))
# this dataset has n=1142

lookup_og %>% 
  filter(Species == "CM") %>% 
  select(GFE_ID, Species, CU, River) %>% 
  distinct() %>% 
  summarize(num = n_distinct(GFE_ID))
# this dataset has n=1230

forestry_data_timeseries_2022 %>% 
  ungroup() %>% 
  select(LINEAR_FEATURE_ID) %>% 
  distinct() %>% 
  summarise(num = n())
#this dataset has n=1745

ch20rsc %>% 
  select(GFE_ID) %>% 
  distinct() %>% 
  summarize(num = n())
# this dataset has 374

#check if all the GFE_ID in ch20rsc is in lookup_og
ch20rsc %>% 
  select(GFE_ID) %>% 
  distinct() %>% 
  filter(!GFE_ID %in% lookup_og$GFE_ID)

# check if all LINEAR_FEATURE_ID in lookup_og is in forestry_data_timeseries_2022

lookup_og %>% 
  filter(Species == "CM") %>% 
  select(LINEAR_FEATURE_ID, GFE_ID, CU, River) %>% 
  distinct() %>% 
  filter(!LINEAR_FEATURE_ID %in% forestry_data_timeseries_2022$LINEAR_FEATURE_ID)

# make one dataset with GFE_ID, LINEAR_FEATURE_ID, CU, River, Species, haarea_prct_cs_2022

full_lookup <- lookup_og %>% 
  filter(Species == "CM") %>% 
  select(LINEAR_FEATURE_ID, GFE_ID, CU, River) %>% 
  distinct() %>% 
  full_join(forestry_data_timeseries_2022 %>% 
              select(LINEAR_FEATURE_ID, haarea_prct_cs_2022), by = "LINEAR_FEATURE_ID")

pop_sheds <- st_read(dsn = "salmon_forestry_data_analysis/data/salmon_datasets_plotting/salmon_watersheds.gpkg")


ch20rsc_w_outlet_lfid <- ch20rsc %>% 
  left_join(full_lookup %>% select(CU, GFE_ID, LINEAR_FEATURE_ID, haarea_prct_cs_2022), by = c("CU" = "CU", "GFE_ID" = "GFE_ID")) %>% 
  left_join(pop_sheds %>% select(outlet_lfid, Region) %>% mutate(outlet_lfid = as.integer(outlet_lfid)), 
            by = c("LINEAR_FEATURE_ID" = "outlet_lfid")) 


# make a dataset with the latest (2022) average values of CDA for all CUs

CDA_2022_CU <- full_lookup %>% 
  group_by(CU, River) %>% 
  select(haarea_prct_cs_2022) %>% 
  group_by(CU) %>% 
  summarize(mean_2022_CDA = mean(haarea_prct_cs_2022, na.rm = TRUE),
            n_rivers = n())


productivity_decline_cu_df_full_2022 <- function(posterior, CDA_2022_CU, effect, 
                                                 species, full_2022 = FALSE,
                                                 within_model_2022 = TRUE
                                                 ){
  
  if(species == "chum"){
    df <- ch20rsc_w_outlet_lfid
    
  } else if(species == "pink"){
    df <- pk10r
  }
  
  full_productivity <- NULL
  
  for (i in 1:length(unique(df$CU_n))){
    
    cu <- unique(df$CU_n)[i]
    
    cu_data <- df %>% filter(CU_n == cu)
    
    b_cu <- posterior %>% select(starts_with("b_for_cu")) %>%
      select(ends_with(paste0("[",cu,"]")))
    
    
    # cpd_sqrt_std_cu <- max(cu_data$sqrt.CPD.std) # should not be using max from CU
    #minimum forestry possible - 0
    real_cpd_cu <- cu_data %>% group_by(River) %>% 
      filter(disturbedarea_prct_cs == max(disturbedarea_prct_cs)) %>% 
      distinct(disturbedarea_prct_cs, haarea_prct_cs_2022) %>% 
      ungroup %>% 
      summarize(mean = mean(disturbedarea_prct_cs), mean_2022 = mean(haarea_prct_cs_2022)) 
    
    print(paste(cu_data$CU[1], cu_data$CU_NAME[1]))
    
    print(paste("2012 cda: ", real_cpd_cu$mean))
    
    print(paste("2022 cda: ", real_cpd_cu$mean_2022))
    
    real_full_cpd_cu <- CDA_2022_CU %>% filter(CU == cu_data$CU[1])
    
    print(paste("2022 full cda: ",real_full_cpd_cu$mean_2022_CDA))
    
    
    # current_forestry <- theoretical_df$theoretical_cpd_sqrt_std[which.min(abs(theoretical_df$theoretical_cpd - real_cpd_cu$mean))]
    # current_forestry_2022 <- theoretical_df$theoretical_cpd_sqrt_std[which.min(abs(theoretical_df$theoretical_cpd - real_cpd_cu$mean_2022))]
    
    #this amounts to the difference between standardized values of forestry and no forestry
    
    # if we are including all watersheds that are not in the model
    if(full_2022){
      forestry_diff <- sqrt(real_full_cpd_cu$mean_2022_CDA)/sd(df$sqrt.CPD)
    } else if(within_model_2022){
      forestry_diff <- sqrt(real_cpd_cu$mean_2022)/sd(df$sqrt.CPD)
    } else{
      forestry_diff <- sqrt(real_cpd_cu$mean)/sd(df$sqrt.CPD)
    }
    
    
    
    
    
    
    productivity <- (exp(as.matrix(b_cu[,1])%*%
                           (forestry_diff)))*100 - 100
    
    productivity_median <- apply(productivity,2,median)
    
    productivity_median_df <- data.frame(CU = unique(cu_data$CU_name),
                                         productivity_50 = apply(productivity,2,median),
                                         productivity_25 = apply(productivity,2,quantile, probs = 0.25),
                                         productivity_75 = apply(productivity,2,quantile, probs = 0.75),
                                         productivity_025 = apply(productivity,2,quantile, probs = 0.025),
                                         productivity_975 = apply(productivity,2,quantile, probs = 0.975),
                                         # productivity_025_hdi = apply(productivity,2, hdi, ci = 0.95)[[1]]$CI_low,
                                         # productivity_975_hdi = apply(productivity,2, hdi, ci = 0.95)[[1]]$CI_high,
                                         forestry = real_cpd_cu$mean,
                                         CU_n = unique(cu_data$CU_n))
    
    full_productivity <- rbind(full_productivity, productivity_median_df)
    
    
  }
  
  
  return(full_productivity)
  
  
}

ric_chm_cpd_ocean_covariates_logR_long_chain <- read.csv(here('salmon_forestry_data_analysis','stan models','outs','posterior',
                                                              'ric_chm_cpd_ocean_covariates_logR_long_chain.csv'),check.names=F)

ric_chm_cpd_productivity_decline_cu_full_2022 <- productivity_decline_cu_df_full_2022(ric_chm_cpd_ocean_covariates_logR_long_chain, 
                                                                                   CDA_2022_CU, 
                                                                                   effect = "cpd", 
                                                                                   species ="chum",
                                                                                   full_2022 = FALSE,
                                                                                   within_model_2022 = TRUE)


##### figure



# Get Canada 
world <- ne_countries(scale='medium',returnclass = 'sf')
Canada <- subset(world, admin == "Canada")
USA <- subset(world, admin == "United States of America")

# Watershed by region plot for forest  appendix ----
# Custom extent
b <- st_bbox(pop_sheds) 
bbox <- st_as_sfc(b)

# # Custom colors based on development districts
colrs_w_alpha <- c("#e64b3580", "#3c538880", "#2ca02c80", "#8a419880", "#eea23680", "#8f4c2d80")
colrs <- c("#e64b35", "#3c5388", "#2ca02c", "#8a4198", "#eea236", "#8f4c2d")

#make vector of colours with colours between the ones above

colrs_extended <- c("#b84666", "#5d5064")



names(colrs) <- c("North Coast - Skeena",
                  "South Coast",
                  "Haida Gwaii",
                  "North Island - Central Coast",
                  "Campbell River",
                  "South Island")
names(colrs_w_alpha) <- c("North Coast - Skeena",
                          "South Coast",
                          "Haida Gwaii",
                          "North Island - Central Coast",
                          "Campbell River",
                          "South Island")




# Globe Inset
wld <- gisco_get_countries(resolution = "20")
ortho_crs <-'+proj=ortho +lat_0=45 +lon_0=-126 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>% # radio Tierra
  st_sfc(crs = ortho_crs)

#trying to fix error
wld <- st_make_valid(wld)
wld <- st_transform(wld, 3857)  # Web Mercator
wld <- st_make_valid(wld)
wld <- st_transform(wld, 4326)  # Back to WGS84
##

world <-   st_intersection(wld, st_transform(ocean, 4326)) %>%
  st_transform(crs = ortho_crs)

world_line <- ms_innerlines(world)

wld_map <- ggplot(world) +
  geom_sf(data = ocean, fill = "#deebf7", linewidth = .2) +
  geom_sf(fill = "grey50", 
          colour = NA,
          show.legend = F) +
  geom_sf(data = world_line, linewidth = .05, colour = "white") +
  geom_sf(data = bbox, col = "red", fill = NA, linewidth = 1) +
  scale_fill_manual(values = c("grey50", "red")) + 
  theme_void()

# Plot 

bc_region_wtrs_simple <- ggplot() +
  
  # Base map
  geom_sf(data = Canada, fill = "grey90", color = NA) +
  geom_sf(data = USA, fill = "grey90", color = NA) +
  
  # Regional colors layer
  geom_sf(data = pop_sheds, aes(fill = Region, color = Region), 
          linewidth = 0, 
          # color = "#d3d3d350"
  ) +
  
  # Plot Window
  coord_sf(crs = st_crs(pop_sheds), xlim = c(b["xmin"] - 30000, b["xmax"] + 30000) , ylim = c(b["ymin"] - 4000, b["ymax"])) +
  
  # Theme
  scale_fill_manual(values = colrs) + 
  scale_color_manual(values = colrs_w_alpha) +
  
  theme(panel.grid.major = element_line(colour = "aliceblue", linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"), 
        panel.border = element_rect(fill = NA, color = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        base_family = "ArcherPro Book",
        legend.position = "bottom") +
  
  labs(x = "", y = "", color = "", fill = "")




#relevel regions by hand

Region_relevel_custom2 <- c("South Island","Campbell River",  "South Coast",
                            "North Island - Central Coast",
                            "North Coast - Skeena", "Haida Gwaii")

# make a pie chart of the number of data points

foo2 <- ric_chm_cpd_productivity_decline_cu_full_2022 %>% left_join(ch20rsc_w_outlet_lfid %>% 
                                                                     select(CU, Region, CU_name, Y_LAT) %>% 
                                                                     group_by(CU, Region) %>%
                                                                     summarize(n_data =n(), CU_name= first(CU_name), Y_LAT = max(Y_LAT)) %>%  
                                                                     arrange(desc(n_data)),
                                                                   
                                                                   by = c("CU" = "CU_name")) %>% 
  # select(-n_data) %>% \
  # have column for Region in which majority of the rivers are in
  group_by(CU) %>% 
  mutate(majority_region = Region[which.max(n_data)],
         Y_LAT_1 = Y_LAT[which.max(n_data)]) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Region, values_from = c(n_data), id_cols=c(CU, majority_region, Y_LAT_1, productivity_50, productivity_025, productivity_975,
                                                                      productivity_25, productivity_75, forestry)) %>% 
  
  # pivot_wider(names_from = Region_n, values_from = c(Region,Y_LAT, n_data), id_cols=c(CU, productivity_50, productivity_025, productivity_975,
  #                                                                                     productivity_25, productivity_75, forestry)) %>%
  # mutate(Region_2 = ifelse(is.na(Region_2), Region_1, Region_2)) %>%
  # # mutate(Region_3 = ifelse(is.na(Region_3), Region_1, Region_3)) %>%
  mutate(Region_new = factor(majority_region, levels = Region_relevel_custom2)) %>% 
  # arrange(Region_new, Y_LAT_1) %>% 
  #convert NA to 0 in Region
  mutate(`North Island - Central Coast` = ifelse(is.na(`North Island - Central Coast`), 0, `North Island - Central Coast`),
         `South Coast` = ifelse(is.na(`South Coast`), 0, `South Coast`),
         `North Coast - Skeena` = ifelse(is.na(`North Coast - Skeena`), 0, `North Coast - Skeena`),
         `Haida Gwaii` = ifelse(is.na(`Haida Gwaii`), 0, `Haida Gwaii`),
         `Campbell River` = ifelse(is.na(`Campbell River`), 0, `Campbell River`),
         `South Island` = ifelse(is.na(`South Island`), 0, `South Island`)
  ) %>% 
  
  # 
  # # arrange(Region2, (productivity_50)) %>%
  arrange(Region_new, Y_LAT_1) %>%
  # mutate(n_data_1 = ifelse(is.na(n_data_1),0,n_data_1),
  #        n_data_2 = ifelse(is.na(n_data_2),0,n_data_2),
  #        n_data_3 = ifelse(is.na(n_data_3),0,n_data_3)) %>% 
  mutate(CU2 = factor(CU, levels = CU)) %>%
  mutate(CU2_numeric = as.numeric(CU2)*10, pie_chart_position = productivity_025-7) 


cu_forest_plot_new7 <- foo2 %>%  
  ggplot(aes(y = CU2_numeric, x = productivity_50)) +
  geom_point(aes(x = productivity_50, y = CU2_numeric, color = majority_region), 
             fill = "white", size = 3, alpha = 0.5) +
  geom_errorbar(aes(xmin = productivity_25, xmax = productivity_75, color = majority_region),
                # color = '#516479', 
                width = 0, alpha = 0.5, size = 2) +
  geom_errorbar(aes(xmin = productivity_025, xmax = productivity_975, color = majority_region), 
                # color = '#516479',
                width = 0, alpha = 0.7, size = 1) +
  #add estimated median decline to the right of each error bar
  geom_text(aes(label = paste(round(productivity_50,1),"%")), 
            hjust = -0.25, 
            vjust = -0.35,
            size = 3, color = "gray20") +
  
  #add scatterpie plot next to each errorbar to show number of rivers within each CU from each region
  geom_scatterpie(data= foo2, aes(y = CU2_numeric, x = pie_chart_position, r = rep(4,24*6)),
                  # bg_circle_radius=1.5,
                  # size = 1,
                  #aes(x = Y_LAT_1, y = productivity_50),
                  cols = c("North Island - Central Coast", "South Coast", "North Coast - Skeena",
                           "Haida Gwaii", "Campbell River", "South Island"),
                  color = NA
                  #use same colours as map
                  # color = colrs_w_alpha
  ) + #color = NA, alpha = 0.8, size = 0.5) +
  coord_equal() +
  #add dashed v line
  scale_color_manual(name = 'Region', values = colrs_w_alpha) +
  scale_fill_manual(name = 'Region', values = colrs_w_alpha) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  xlim(-100, 110) +
  scale_y_continuous(
    breaks = foo2$CU2_numeric,
    labels = foo2$CU2
  ) +
  # coord_equal() +
  # scale_color_manual(name = 'Model type', values = c('independent alpha' = 'cadetblue', 'hierarchical alpha' = 'coral', 'hierarchical alpha - ricker' = 'darkgoldenrod')) +
  labs(#title = 'Estimated percent change in CU-level productivity',
    y = 'Conservation Unit',
    x = 'Change in chum productivity (%)') +
  theme_classic() +
  theme(legend.position = "none",
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  inset_element(bc_region_wtrs_simple , 
                left = 0.65, bottom = 0.65, right = 1, top = 1, align_to = "plot")+
  theme(legend.position = c(0.5,-0.15),
        legend.direction = "vertical",
        legend.key.size = unit(0.25, "cm"),
        legend.text.position = "left",
        legend.text = element_text(size = 7, hjust=1),
        legend.background = element_rect(fill = "transparent", size = 0.5),
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent'))




cu_forest_plot_new7


ggsave(here("output_figures_tables","manuscript_fig5_sep2026_chum_ricker_cda_recruitment_decline_2022_by_cu_forest_plot_arranged_latitude_pie_chart.png"),
       cu_forest_plot_new7, width = 6, height = 6, bg = "white")

ggsave(here("output_figures_tables","manuscript_fig5_review1.pdf"),
       cu_forest_plot_new7, width = 6, height = 6, bg = "white",dpi = 300)




                                                                                    




