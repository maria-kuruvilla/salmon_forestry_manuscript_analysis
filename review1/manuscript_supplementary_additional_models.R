

library(here);library(dplyr); library(stringr)
library(ggplot2)
library(tidyverse)
library(bayesplot)
library(patchwork)
library(hues)
library(GGally)
library(latex2exp)
library(ggrepel)
library(latex2exp)
library(ggpubr)
library(bayestestR)
library(ggrepel)



ch20rsc <- read.csv(here('salmon_forestry_data_analysis','data','chum_SR_20_hat_yr_w_variable_ersst_npgo.csv'))

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

ch20rsc$npgo.std=(ch20rsc$winter_npgo-mean(ch20rsc$winter_npgo))/sd(ch20rsc$winter_npgo)
ch20rsc$sst.std=(ch20rsc$spring_ersst_variable-mean(ch20rsc$spring_ersst_variable))/sd(ch20rsc$spring_ersst_variable)

cu = distinct(ch20rsc,.keep_all = T)

cu_n = as.numeric(factor(cu$CU))

ch20rsc$CU_n <- cu_n

# make CU_NAME values Title case instead of all caps
ch20rsc$CU_name <- str_to_title(ch20rsc$CU_NAME)

# change "East Hg" to"East Haida Gwaii"
ch20rsc$CU_name <- ifelse(ch20rsc$CU_name == "East Hg", "East Haida Gwaii", ch20rsc$CU_name)


pk10r_e <- read.csv(here('salmon_forestry_data_analysis','data',"pke_SR_10_hat_yr_w_ersst_npgo.csv"))

#odd year pinks
pk10r_o <- read.csv(here('salmon_forestry_data_analysis','data',"pko_SR_10_hat_yr_w_ersst_npgo.csv"))


ric_chm_eca_variable_ocean_covariates_logR_long_chain=read.csv(here('salmon_forestry_data_analysis','stan models','outs','posterior',
                                                           'ric_chm_eca_variable_ocean_covariates_logR_long_chain.csv'),check.names=F)
ric_chm_cpd_variable_ocean_covariates_logR_long_chain=read.csv(here('salmon_forestry_data_analysis','stan models','outs','posterior',
                                                           'ric_chm_cpd_variable_ocean_covariates_logR_long_chain.csv'),check.names=F)






# look at effect sizes

effect_sizes_df <- function(posterior, species){
  
  if(species == "chum"){
    df <- ch20rsc 
    
  } else if(species == "pink"){
    df <- pk10r
  }
  
  effect_df <- NULL
  b_effect <- posterior %>% select("b_for", "b_sst", "b_npgo")
  
  
  effect_df <- data.frame(effect = names(b_effect),
                          effect_median = round(apply(as.matrix(b_effect),2,median),2),
                          effect_ci_lower = round(apply(as.matrix(b_effect),2,HDInterval::hdi),2)[1,],
                          effect_ci_upper = round(apply(as.matrix(b_effect),2,HDInterval::hdi),2)[2,]
  )
  return(effect_df)
  
}


chum_effect_sizes_eca <- effect_sizes_df(ric_chm_eca_variable_ocean_covariates_logR_long_chain, species = "chum" )
chum_effect_sizes_cpd <- effect_sizes_df(ric_chm_cpd_variable_ocean_covariates_logR_long_chain, species = "chum" )
pink_effect_sizes_eca <- effect_sizes_df(ric_pk_eca_ersst_long_chain, species = "pink" )
pink_effect_sizes_cpd <- effect_sizes_df(ric_pk_cpd_ersst_long_chain, species = "pink" )

all_effect_sizes <- rbind(
  chum_effect_sizes_eca %>% mutate(species = "Chum", forestry_metric = "ECA"),
  chum_effect_sizes_cpd %>% mutate(species = "Chum", forestry_metric = "CDA"),
  pink_effect_sizes_eca %>% mutate(species = "Pink", forestry_metric = "ECA"),
  pink_effect_sizes_cpd %>% mutate(species = "Pink", forestry_metric = "CDA")
) %>% 
  mutate(effect = case_when(effect == "b_for" ~ "Effect of forestry",
                            effect == "b_sst" ~ "Effect of SST",
                            effect == "b_npgo" ~ "Effect of NPGO")) %>% 
  mutate(effect_size = paste0(as.character(effect_median), " [",as.character(effect_ci_lower), ", ",as.character(effect_ci_upper),"]")) %>%
  select(-effect_median, -effect_ci_lower, -effect_ci_upper) %>%
  pivot_wider(names_from = effect,
              values_from = effect_size)

all_effect_sizes




