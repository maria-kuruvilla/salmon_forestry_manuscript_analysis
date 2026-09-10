# goal - to read npgo data from https://www.npgo.org/data/NPGO.txt

# then average over values from Dec-March for every year

# join with chum and pink salmon dataset

# libraries
library(tidyverse)
library(ggplot2)
library(here)
library(GGally)



# read data from website

npgo_data <- read.table("https://www.npgo.org/data/NPGO.txt", 
                      stringsAsFactors = FALSE) %>% 
  as.data.frame()

colnames(npgo_data) <- c("Year", "Month", "NPGO")  

glimpse(npgo_data)

# average over Dec - March of every year

# make column for brood-year winter which is Dec to March

npgo_data_winter <- npgo_data %>% 
  mutate(BroodYear = ifelse(Month %in% c(1,2,3), Year - 1, Year)) %>% 
  filter(Month == 12 | Month == 1 | Month == 2 | Month == 3) %>%
  group_by(BroodYear) %>% 
  summarize(winter_npgo = mean(NPGO))

  
# read chum data

ch20r <- read.csv(here('salmon_forestry_data_analysis','data','chum_SR_20_hat_yr_w_ersst.csv'))

# left join with npgo

ch20r_w_npgo <- ch20r %>% 
  left_join(npgo_data_winter %>% select(winter_npgo, BroodYear), join_by(BroodYear))

# check the correlation between npgo and winter_npgo

ggplot(ch20r_w_npgo, aes(x = npgo, y = winter_npgo)) +
  geom_point(alpha = 0.1) +
  geom_abline() +
  theme_classic()

# check the correlation between npgo and sst, use only distinct conbinations

ch20r_w_npgo %>% 
  select(winter_npgo, spring_ersst) %>% 
  distinct() %>% 
  ggplot()+
  geom_point(aes(x = winter_npgo, y = spring_ersst), alpha = 0.5) +
  annotate("text", x = 2, y = 14, label = paste("correlation =", round(cor(ch20r_w_npgo$winter_npgo, ch20r_w_npgo$spring_ersst), 2))) +
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  labs(x = "Winter NPGO (Dec-Mar)", y = "Spring SST (March-May)")

# plot correlation matrix plots with ggpairs
ch20r_w_npgo %>% 
  select(winter_npgo, spring_ersst, disturbedarea_prct_cs) %>% 
  ggpairs(aes(alpha = 0.1),
          columnLabels = c("Winter NPGO", "Spring SST", "CDA %")) +
  # change name of the variables shown in strip
  
  theme_classic()

ggsave(here('output_figures_tables','correlation_matrix_npgo_sst_cda.png'), width = 8, height = 6)


ch20r_w_npgo %>% 
  select(winter_npgo, spring_ersst, ECA_age_proxy_forested_only) %>% 
  ggpairs(aes(alpha = 0.1),
          columnLabels = c("Winter NPGO", "Spring SST", "ECA")) +
  theme_classic()

ggsave(here('output_figures_tables','correlation_matrix_npgo_sst_eca.png'), width = 8, height = 6)


#save the new dataset

write.csv(ch20r_w_npgo, here('salmon_forestry_data_analysis','data','chum_SR_20_hat_yr_w_ersst_npgo.csv'), row.names = FALSE)

# read pink data
#even year pinks
pk10r_e <-  read.csv(here('salmon_forestry_data_analysis','data',"pke_SR_10_hat_yr_w_ersst.csv"))

#odd year pinks
pk10r_o <-  read.csv(here('salmon_forestry_data_analysis','data',"pko_SR_10_hat_yr_w_ersst.csv"))


# left join with npgo data


pk10r_e_w_npgo <- pk10r_e %>% 
  left_join(npgo_data_winter %>% select(winter_npgo, BroodYear), join_by(BroodYear))

pk10r_o_w_npgo <- pk10r_o %>%
  left_join(npgo_data_winter %>% select(winter_npgo, BroodYear), join_by(BroodYear))


pk10r_o_w_npgo$Broodline='Odd'
pk10r_e_w_npgo$Broodline='Even'


pk10r_w_npgo=rbind(pk10r_e_w_npgo,pk10r_o_w_npgo)


ggplot(pk10r_w_npgo, aes(x = npgo, y = winter_npgo)) +
  geom_point(alpha = 0.1) +
  geom_abline() +
  theme_classic()

# correlation matrix

pk10r_w_npgo %>% 
  select(winter_npgo, spring_ersst, disturbedarea_prct_cs) %>% 
  ggpairs(aes(alpha = 0.01),
          columnLabels = c("Winter NPGO", "Spring SST", "CDA %")) +
  # change name of the variables shown in strip
  
  theme_classic()

ggsave(here('output_figures_tables','correlation_matrix_npgo_sst_cda_pink.png'), width = 8, height = 6)

pk10r_w_npgo %>% 
  select(winter_npgo, spring_ersst, ECA_age_proxy_forested_only) %>% 
  ggpairs(aes(alpha = 0.1),
          columnLabels = c("Winter NPGO", "Spring SST", "ECA")) +
  theme_classic()

ggsave(here('output_figures_tables','correlation_matrix_npgo_sst_eca_pink.png'), width = 8, height = 6)

#save the new dataset

write.csv(pk10r_e_w_npgo , here('salmon_forestry_data_analysis','data','pke_SR_10_hat_yr_w_ersst_npgo.csv'), row.names = FALSE)
write.csv(pk10r_o_w_npgo , here('salmon_forestry_data_analysis','data','pko_SR_10_hat_yr_w_ersst_npgo.csv'), row.names = FALSE)




