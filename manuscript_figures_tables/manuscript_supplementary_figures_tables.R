# add results to supplementary materials of the manuscript -

## coefficient for CDA, ECA, SST, NPGO
## decline predicted at current average CDA for chum
## decline predicted at current average ECA for chum
## decline predicted at current average CDA for pink  
## decline predicted at current average ECA for pink
## decline predicted at current average SST for pink  
## decline predicted at current average NPGO for pink


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



ch20rsc <- read.csv(here('salmon_forestry_data_analysis','data','chum_SR_20_hat_yr_w_ersst.csv'))


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

# change "East Hg" to"East Haida Gwaii"
ch20rsc$CU_name <- ifelse(ch20rsc$CU_name == "East Hg", "East Haida Gwaii", ch20rsc$CU_name)


pk10r_e <- read.csv(here('salmon_forestry_data_analysis','data',"pke_SR_10_hat_yr_w_ersst.csv"))

#odd year pinks
pk10r_o <- read.csv(here('salmon_forestry_data_analysis','data',"pko_SR_10_hat_yr_w_ersst.csv"))

options(mc.cores=8)

# Pink salmon - even/odd croodlines #####
pk10r_o$River=ifelse(pk10r_o$WATERSHED_CDE=='950-169400-00000-00000-0000-0000-000-000-000-000-000-000','SALMON RIVER 2',pk10r_o$River)
pk10r_o$River=ifelse(pk10r_o$WATERSHED_CDE=='915-765500-18600-00000-0000-0000-000-000-000-000-000-000','HEAD CREEK 2',pk10r_o$River)
pk10r_o$River=ifelse(pk10r_o$WATERSHED_CDE=='915-488000-41400-00000-0000-0000-000-000-000-000-000-000','WINDY cAY CREEK 2',pk10r_o$River)
pk10r_o$River=ifelse(pk10r_o$WATERSHED_CDE=="915-486500-05300-00000-0000-0000-000-000-000-000-000-000",'LAGOON CREEK 2',pk10r_o$River)
pk10r_o=pk10r_o[order(factor(pk10r_o$River),pk10r_o$BroodYear),]
rownames(pk10r_o)=seq(1:nrow(pk10r_o))

pk10r_e$River=ifelse(pk10r_e$WATERSHED_CDE=='950-169400-00000-00000-0000-0000-000-000-000-000-000-000','SALMON RIVER 2',pk10r_e$River)
pk10r_e$River=ifelse(pk10r_e$WATERSHED_CDE=='915-765500-18600-00000-0000-0000-000-000-000-000-000-000','HEAD CREEK 2',pk10r_e$River)
pk10r_e$River=ifelse(pk10r_e$WATERSHED_CDE=='915-488000-41400-00000-0000-0000-000-000-000-000-000-000','WINDY cAY CREEK 2',pk10r_e$River)
pk10r_e$River=ifelse(pk10r_e$WATERSHED_CDE=="915-486500-05300-00000-0000-0000-000-000-000-000-000-000",'LAGOON CREEK 2',pk10r_e$River)
pk10r_e=pk10r_e[order(factor(pk10r_e$River),pk10r_e$BroodYear),]
rownames(pk10r_e)=seq(1:nrow(pk10r_e))


#normalize ECA 2 - square root transformation (ie. sqrt(x))
pk10r_o$sqrt.ECA=sqrt(pk10r_o$ECA_age_proxy_forested_only)
pk10r_o$sqrt.ECA.std=(pk10r_o$sqrt.ECA-mean(pk10r_o$sqrt.ECA))/sd(pk10r_o$sqrt.ECA)

#normalize CPD 2 - square root transformation (ie. sqrt(x))
pk10r_o$sqrt.CPD=sqrt(pk10r_o$disturbedarea_prct_cs)
pk10r_o$sqrt.CPD.std=(pk10r_o$sqrt.CPD-mean(pk10r_o$sqrt.CPD))/sd(pk10r_o$sqrt.CPD)

#normalize ECA 2 - square root transformation (ie. sqrt(x))
pk10r_e$sqrt.ECA=sqrt(pk10r_e$ECA_age_proxy_forested_only)
pk10r_e$sqrt.ECA.std=(pk10r_e$sqrt.ECA-mean(pk10r_e$sqrt.ECA))/sd(pk10r_e$sqrt.ECA)

#normalize CPD 2 - square root transformation (ie. sqrt(x))
pk10r_e$sqrt.CPD=sqrt(pk10r_e$disturbedarea_prct_cs)
pk10r_e$sqrt.CPD.std=(pk10r_e$sqrt.CPD-mean(pk10r_e$sqrt.CPD))/sd(pk10r_e$sqrt.CPD)


#standardize npgo
pk10r_o$npgo.std=(pk10r_o$npgo-mean(pk10r_o$npgo))/sd(pk10r_o$npgo)
pk10r_e$npgo.std=(pk10r_e$npgo-mean(pk10r_e$npgo))/sd(pk10r_e$npgo)

# pk10r_o$sst.std = (pk10r_o$spring_lighthouse_temperature-mean(pk10r_o$spring_lighthouse_temperature))/sd(pk10r_o$spring_lighthouse_temperature)
# pk10r_e$sst.std = (pk10r_e$spring_lighthouse_temperature-mean(pk10r_e$spring_lighthouse_temperature))/sd(pk10r_e$spring_lighthouse_temperature)

pk10r_o$sst.std = (pk10r_o$spring_ersst-mean(pk10r_o$spring_ersst))/sd(pk10r_o$spring_ersst)
pk10r_e$sst.std = (pk10r_e$spring_ersst-mean(pk10r_e$spring_ersst))/sd(pk10r_e$spring_ersst)


pk10r_o$escapement.t_1=pk10r_e$Spawners[match(paste(pk10r_o$WATERSHED_CDE,pk10r_o$BroodYear-1),paste(pk10r_e$WATERSHED_CDE,pk10r_e$BroodYear))]
pk10r_e$escapement.t_1=pk10r_o$Spawners[match(paste(pk10r_e$WATERSHED_CDE,pk10r_e$BroodYear-1),paste(pk10r_o$WATERSHED_CDE,pk10r_o$BroodYear))]

pk10r_o$Broodline='Odd'
pk10r_e$Broodline='Even'

L_o=pk10r_o%>%group_by(River)%>%summarize(l=n(),by=min(BroodYear),tmin=(min(BroodYear)-min(pk10r_o$croodYear))/2+1,tmax=(max(BroodYear)-min(pk10r_o$BroodYear))/2)
L_e=pk10r_e%>%group_by(River)%>%summarize(l=n(),by=min(BroodYear),tmin=(min(BroodYear)-min(pk10r_e$croodYear))/2+1,tmax=(max(BroodYear)-min(pk10r_e$BroodYear))/2)
L_o$River2=paste(L_o$River,'Odd',sep='_')
L_e$River2=paste(L_e$River,'Even',sep='_')
L_all=rbind(L_e,L_o)
L_all=L_all[order(factor(L_all$River2)),]

pk10r_o$ii=as.numeric(factor(pk10r_o$BroodYear))
pk10r_e$ii=as.numeric(factor(pk10r_e$BroodYear))

pk10r=rbind(pk10r_e,pk10r_o)
pk10r$River2=paste(pk10r$River,pk10r$Broodline,sep='_')
pk10r=pk10r[order(factor(pk10r$River2),pk10r$BroodYear),]

#extract max S for priors on capacity & eq. recruitment
smax_prior=pk10r%>%group_by(River2) %>%summarize(m.s=max(Spawners),m.r=max(Recruits))

#ragged start and end points for each SR series
# N_s=rag_n(pk10r$River2)

#cus by stock
cu1=distinct(pk10r,CU,.keep_all = T)
cu2=distinct(pk10r,River,.keep_all = T)
cu3=distinct(pk10r,River2,.keep_all = T)

pk10r$River_n <- as.numeric(factor(pk10r$River))
pk10r$River_n2 <- as.numeric(factor(pk10r$River2))
pk10r$CU_name <- pk10r$CU_NAME

max_eca_pink_df <- pk10r %>% 
  select(River2, River, River_n, CU, ECA_age_proxy_forested_only) %>%
  group_by(River) %>%
  summarize(River = first(River),
            River_n = first(River_n),
            CU = first(CU),
            eca_max = 100*max(ECA_age_proxy_forested_only, na.rm =TRUE)) %>% 
  mutate(eca_level = case_when(eca_max < 12 ~ 'low',
                               eca_max >= 12 & eca_max < 24 ~ 'medium',
                               eca_max >= 24 ~ 'high'))

max_cpd_pink_df <- pk10r %>%
  select(River2, River, River_n,  CU, disturbedarea_prct_cs) %>%
  group_by(River) %>% 
  summarize(River = first(River),
            River_n = first(River_n),
            CU = first(CU),
            cpd_max = max(disturbedarea_prct_cs, na.rm = TRUE))


ric_chm_eca_ocean_covariates_logR_long_chain=read.csv(here('salmon_forestry_data_analysis','stan models','outs','posterior',
                                                'ric_chm_eca_ocean_covariates_logR_long_chain.csv'),check.names=F)
ric_chm_cpd_ocean_covariates_logR_long_chain=read.csv(here('salmon_forestry_data_analysis','stan models','outs','posterior',
                                                'ric_chm_cpd_ocean_covariates_logR_long_chain.csv'),check.names=F)



ric_pk_eca_ersst_long_chain = read.csv(here('salmon_forestry_data_analysis','stan models',
                                 'outs',
                                 'posterior',
                                 'ric_pk_eca_st_noac_ocean_covariates_logR_long_chain_sigma_vector.csv'),check.names=F)

ric_pk_cpd_ersst_long_chain = read.csv(here('salmon_forestry_data_analysis','stan models',
                                 'outs',
                                 'posterior',
                                 'ric_pk_cpd_st_noac_ocean_covariates_logR_long_chain_sigma_vector.csv'),check.names=F)


# residuals figure


posterior_mu2 <- read.csv(here('salmon_forestry_data_analysis','stan models','outs','posterior',
                               'ric_chm_cpd_ocean_covariates_logR_long_chain_mu2.csv'),check.names=F)

glimpse(posterior_mu2)


n_rows <- nrow(ch20rsc)

#make df

residual_df <- data.frame(observed = ch20rsc$ln_RS, residual = NA)



residual_df <- data.frame(observed = log(ch20rsc$Recruits), forestry = ch20rsc$disturbedarea_prct_cs) %>% 
  mutate(predicted = posterior_mu2 %>% 
           select(starts_with("mu2")) %>%
           apply(., 2, median),
         residual = observed - predicted)

#plot residuals as a function of fitted

chum_residuals <- ggplot(residual_df)+
  geom_point(aes(x = predicted, y = residual, color = forestry), alpha = 0.2, size = 2) +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
  labs(title = "Chum", x = TeX(r"(Predicted $\log (Recruits)$)"), y = "Residuals") +
  ylim(-6, 6) +
  theme_classic() +
  scale_color_gradient2(name = 'CDA (%)',
                        low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 50)+
  theme(legend.position = "right",
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(1, "lines"),
        legend.text = element_text(size = 7),
        legend.spacing.y = unit(0.001, "cm"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0)
  )

# residuals for pink

posterior_mu2_pk <- read.csv(here('salmon_forestry_data_analysis','stan models','outs','posterior',
                                  'ric_pk_cpd_st_noac_ocean_covariates_logR_long_chain_sigma_vector_mu2.csv'),check.names=F)


#glimpse(posterior_mu2_pk)

n_rows_pk <- nrow(pk10r)

#make df

# residual_df_pk <- data.frame(observed = pk10r$ln_RS, residual = NA)
# 
# mu_cols <- grep("^mu2\\[", names(posterior_mu2_pk), value = TRUE)
# medians <- vapply(posterior_mu2_pk[mu_cols], median, numeric(1))
# 



residual_df_pk <- data.frame(observed = log(pk10r$Recruits), forestry = pk10r$disturbedarea_prct_cs) %>% 
  mutate(predicted = posterior_mu2_pk %>% 
           select(starts_with("mu2")) %>%
           apply(., 2, median),
         residual = observed - predicted)

#plot residuals as a function of fitted

pink_residuals <- ggplot(residual_df_pk)+
  geom_point(aes(x = predicted, y = residual, color = forestry), alpha = 0.2, size = 2) +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
  labs(title = "Pink", x = TeX(r"(Predicted $\log (Recruits)$)"), y = "Residuals") +
  ylim(-6, 6) +
  theme_classic() +
  scale_color_gradient2(name = 'CDA (%)',
                        low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 50)+
  theme(legend.position = "right",
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(1, "lines"),
        legend.text = element_text(size = 7),
        legend.spacing.y = unit(0.001, "cm"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0)
  )

combined_residuals <- (chum_residuals)/(pink_residuals) + plot_layout(guides = 'collect', axis_titles = 'collect_x')

combined_residuals

ggsave(here("output_figures_tables", "supplementary_fig9_residuals_w_autocorrelation_logR_chum_pink_long_chain_may2026.png"),
       width = 6, height = 6, dpi = 300, units = "in")


#make diagnostics table


library(DT)
library(kableExtra)
library(tidyverse)

# read in diagnostics table

file <- 'ric_chm_cpd_ocean_covariates_logR_long_chain.csv'


summary_cpd_chm_file <- read.csv(here('salmon_forestry_data_analysis','stan models','outs','summary',file))

summary_cpd_chum <- summary_cpd_chm_file %>% 
  dplyr::select(variable, rhat, median, ess_tail, ess_bulk) %>%
  mutate(variable_group = str_replace_all(variable,c("[0-9]"), "")) %>%
  # mutate(variable_group = str_extract_all(variable,c("[a-z]"))) %>% 
  # mutate(variable_group = str_replace_all(variable_group,"\[", "")) %>% 
  group_by(variable_group) %>% 
  # filter(startsWith(variable, "alpha")) %>% 
  summarize(max_rhat = round(max(rhat),2),
            min_ess_tail = round(min(ess_tail)),
            median_estimate = round(median(median),2),
            n = n()) %>% 
  #remove mu[], lp__, log_lik
  filter(!variable_group %in% c("mu[]", "lp__", "log_lik[]","b[]")) %>% 
  #change "alpha" to "$\alpha$"
  mutate(variable_group = case_when(variable_group == "alpha" ~ "$\\alpha_0$",
                                    variable_group == "alpha_cu[]" ~ "$\\alpha_{cu}$",
                                    variable_group == "alpha_j[]" ~ "$\\alpha_{i}$",
                                    variable_group == "Smax[]" ~ "$S_{max}$",
                                    variable_group == "b_for" ~ "$\\beta^{Forestry}_0$",
                                    variable_group == "b_for_cu[]" ~ "$\\beta^{Forestry}_{CU}$",
                                    variable_group == "b_for_rv[]" ~ "$\\beta^{Forestry}_{i}$",
                                    variable_group == "b_npgo" ~ "$\\beta^{NPGO}_{0}$",
                                    variable_group == "b_npgo_cu[]" ~ "$\\beta^{NPGO}_{CO}$",
                                    variable_group == "b_npgo_rv[]" ~ "$\\beta^{NPGO}_{i}$",
                                    variable_group == "b_sst" ~ "$\\beta^{SST}_{0}$",
                                    variable_group == "b_sst_cu[]" ~ "$\\beta^{SST}_{CU}$",
                                    variable_group == "b_sst_rv[]" ~ "$\\beta^{SST}_{i}$",
                                    variable_group == "cu_sigma[]" ~ "$\\sigma_{CU}$",
                                    variable_group == "mu_sigma" ~ "$\\sigma^{\\mu}$",
                                    variable_group == "e_t[]" ~ "$\\epsilon_{i,t}$",
                                    variable_group == "rho[]" ~ "$\\rho_{i}$",
                                    variable_group == "sigmaAR[]" ~ "$\\sigma^{AR}_{i}$",
                                    variable_group == "sd_sigma" ~ "$\\sigma^{\\sigma}$",
                                    variable_group == "sd_sigma_cu" ~ "$\\sigma^{\\sigma}_{CU}$",
                                    variable_group == "sigma[]" ~ "$\\sigma_i$",
                                    variable_group == "sigma_a_cu" ~ "$\\sigma^{\\alpha,CU}$",
                                    variable_group == "sigma_a_rv[]" ~ "$\\sigma^{\\alpha,i}_{CU}$",
                                    variable_group == "sigma_for_cu" ~ "$\\sigma^{Forestry,CU}$",
                                    variable_group == "sigma_for_rv" ~ "$\\sigma^{Forestry,i}$",
                                    variable_group == "sigma_npgo_cu" ~ "$\\sigma^{NPGO,CU}$",
                                    variable_group == "sigma_npgo_rv" ~ "$\\sigma^{NPGO,i}$",
                                    variable_group == "sigma_sst_cu" ~ "$\\sigma^{SST,CU}$",
                                    variable_group == "sigma_sst_rv" ~ "$\\sigma^{SST,i}$",
                                    variable_group == "z_a_cu[]" ~ "$z^{\\alpha}_{CU}$",
                                    variable_group == "z_a_rv[]" ~ "$z^{\\alpha}_{i}$",
                                    variable_group == "z_for_cu[]" ~ "$z^{Forestry}_{CU}$",
                                    variable_group == "z_for_rv[]" ~ "$z^{Forestry}_{i}$",
                                    variable_group == "z_npgo_cu[]" ~ "$z^{NPGO}_{CU}$",
                                    variable_group == "z_npgo_rv[]" ~ "$z^{NPGO}_{i}$",
                                    variable_group == "z_sst_cu[]" ~ "$z^{SST}_{CU}$",
                                    variable_group == "z_sst_rv[]" ~ "$z^{SST}_{i}$",
                                    variable_group == "z_sig_cu[]" ~ "$z^{\\sigma}_{CU}$",
                                    variable_group == "z_sig_rv[]" ~ "$z^{\\sigma}_{i}$",
                                    TRUE ~ variable_group)) %>% 
  #rename columns
  rename("Parameter group" = variable_group,
         "Rhat (max)" = max_rhat,
         "ESS tail (min)" = min_ess_tail,
         "Estimate (median)" = median_estimate,
         "N_parameters" = n) 



glimpse(summary_cpd_chum)

#print table, make background white

knitr::kable(summary_cpd_chum, format = "html", escape = FALSE)

#save
write.csv(summary_cpd_chum, here("output_figures_tables",
                                 "supplementary_table4_chum_cpd_diagnostics_by_parameter_group.csv"),
          row.names = FALSE)

#do same for chum model with eca

file <- 'ric_chm_eca_ocean_covariates_logR_long_chain.csv'

summary_eca_chm_file <- read.csv(here('salmon_forestry_data_analysis','stan models','outs','summary',file))

summary_eca_chum <- summary_eca_chm_file %>% 
  select(variable, rhat, median, ess_tail, ess_bulk) %>%
  mutate(variable_group = str_replace_all(variable,c("[0-9]"), "")) %>%
  # mutate(variable_group = str_extract_all(variable,c("[a-z]"))) %>% 
  # mutate(variable_group = str_replace_all(variable_group,"\[", "")) %>% 
  group_by(variable_group) %>% 
  # filter(startsWith(variable, "alpha")) %>% 
  summarize(max_rhat = round(max(rhat),2),
            min_ess_tail = round(min(ess_tail)),
            median_estimate = round(median(median),2),
            n = n()) %>% 
  #remove mu[], lp__, log_lik
  filter(!variable_group %in% c("mu[]", "lp__", "log_lik[]","b[]")) %>% 
  #change "alpha" to "$\alpha$"
  mutate(variable_group = case_when(variable_group == "alpha" ~ "$\\alpha_0$",
                                    variable_group == "alpha_cu[]" ~ "$\\alpha_{cu}$",
                                    variable_group == "alpha_j[]" ~ "$\\alpha_{i}$",
                                    variable_group == "Smax[]" ~ "$S_{max}$",
                                    variable_group == "b_for" ~ "$\\beta^{Forestry}_0$",
                                    variable_group == "b_for_cu[]" ~ "$\\beta^{Forestry}_{CU}$",
                                    variable_group == "b_for_rv[]" ~ "$\\beta^{Forestry}_{i}$",
                                    variable_group == "b_npgo" ~ "$\\beta^{NPGO}_{0}$",
                                    variable_group == "b_npgo_cu[]" ~ "$\\beta^{NPGO}_{CO}$",
                                    variable_group == "b_npgo_rv[]" ~ "$\\beta^{NPGO}_{i}$",
                                    variable_group == "b_sst" ~ "$\\beta^{SST}_{0}$",
                                    variable_group == "b_sst_cu[]" ~ "$\\beta^{SST}_{CU}$",
                                    variable_group == "b_sst_rv[]" ~ "$\\beta^{SST}_{i}$",
                                    variable_group == "cu_sigma[]" ~ "$\\sigma_{CU}$",
                                    variable_group == "mu_sigma" ~ "$\\sigma^{\\mu}$",
                                    variable_group == "e_t[]" ~ "$\\epsilon_{i,t}$",
                                    variable_group == "rho[]" ~ "$\\rho_{i}$",
                                    variable_group == "sigmaAR[]" ~ "$\\sigma^{AR}_{i}$",
                                    variable_group == "sd_sigma" ~ "$\\sigma^{\\sigma}$",
                                    variable_group == "sd_sigma_cu" ~ "$\\sigma^{\\sigma}_{CU}$",
                                    variable_group == "sigma[]" ~ "$\\sigma_i$",
                                    variable_group == "sigma_a_cu" ~ "$\\sigma^{\\alpha,CU}$",
                                    variable_group == "sigma_a_rv[]" ~ "$\\sigma^{\\alpha,i}_{CU}$",
                                    variable_group == "sigma_for_cu" ~ "$\\sigma^{Forestry,CU}$",
                                    variable_group == "sigma_for_rv" ~ "$\\sigma^{Forestry,i}$",
                                    variable_group == "sigma_npgo_cu" ~ "$\\sigma^{NPGO,CU}$",
                                    variable_group == "sigma_npgo_rv" ~ "$\\sigma^{NPGO,i}$",
                                    variable_group == "sigma_sst_cu" ~ "$\\sigma^{SST,CU}$",
                                    variable_group == "sigma_sst_rv" ~ "$\\sigma^{SST,i}$",
                                    variable_group == "z_a_cu[]" ~ "$z^{\\alpha}_{CU}$",
                                    variable_group == "z_a_rv[]" ~ "$z^{\\alpha}_{i}$",
                                    variable_group == "z_for_cu[]" ~ "$z^{Forestry}_{CU}$",
                                    variable_group == "z_for_rv[]" ~ "$z^{Forestry}_{i}$",
                                    variable_group == "z_npgo_cu[]" ~ "$z^{NPGO}_{CU}$",
                                    variable_group == "z_npgo_rv[]" ~ "$z^{NPGO}_{i}$",
                                    variable_group == "z_sst_cu[]" ~ "$z^{SST}_{CU}$",
                                    variable_group == "z_sst_rv[]" ~ "$z^{SST}_{i}$",
                                    variable_group == "z_sig_cu[]" ~ "$z^{\\sigma}_{CU}$",
                                    variable_group == "z_sig_rv[]" ~ "$z^{\\sigma}_{i}$",
                                    TRUE ~ variable_group)) %>% 
  #rename columns
  rename("Parameter group" = variable_group,
         "Rhat (max)" = max_rhat,
         "ESS tail (min)" = min_ess_tail,
         "Estimate (median)" = median_estimate,
         "N_parameters" = n) 

#save
write.csv(summary_eca_chum, here("output_figures_tables",
                                 "supplementary_table4_chum_eca_diagnostics_by_parameter_group.csv"),
          row.names = FALSE)


# do same for pink with cpd model

file <- 'ric_pk_cpd_st_noac_ocean_covariates_logR_long_chain_sigma_vector.csv'

summary_cpd_pk_file <- read.csv(here('salmon_forestry_data_analysis',
  'stan models','outs','summary',file))


summary_cpd_pk <- summary_cpd_pk_file %>% 
  select(variable, rhat, median, ess_tail, ess_bulk) %>%
  mutate(variable_group = str_replace_all(variable,c("[0-9]"), "")) %>%
  # mutate(variable_group = str_extract_all(variable,c("[a-z]"))) %>% 
  # mutate(variable_group = str_replace_all(variable_group,"\[", "")) %>% 
  group_by(variable_group) %>% 
  # filter(startsWith(variable, "alpha")) %>% 
  summarize(max_rhat = round(max(rhat),2),
            min_ess_tail = round(min(ess_tail)),
            median_estimate = round(median(median),2),
            n = n()) %>% 
  #remove mu[], lp__, log_lik
  filter(!variable_group %in% c("mu[]", "lp__", "log_lik[]","b[]")) %>% 
  #change "alpha" to "$\alpha$"
  mutate(variable_group = case_when(variable_group == "alpha[]" ~ "$\\alpha_0$",
                                    variable_group == "alpha_cu[]" ~ "$\\alpha_{cu}$",
                                    variable_group == "alpha_j[]" ~ "$\\alpha_{i}$",
                                    variable_group == "Smax[]" ~ "$S_{max}$",
                                    variable_group == "b_for" ~ "$\\beta^{Forestry}_0$",
                                    variable_group == "b_for_cu[]" ~ "$\\beta^{Forestry}_{CU}$",
                                    variable_group == "b_for_rv[]" ~ "$\\beta^{Forestry}_{i}$",
                                    variable_group == "b_npgo" ~ "$\\beta^{NPGO}_{0}$",
                                    variable_group == "b_npgo_cu[]" ~ "$\\beta^{NPGO}_{CO}$",
                                    variable_group == "b_npgo_rv[]" ~ "$\\beta^{NPGO}_{i}$",
                                    variable_group == "b_sst" ~ "$\\beta^{SST}_{0}$",
                                    variable_group == "b_sst_cu[]" ~ "$\\beta^{SST}_{CU}$",
                                    variable_group == "b_sst_rv[]" ~ "$\\beta^{SST}_{i}$",
                                    variable_group == "cu_sigma[]" ~ "$\\sigma_{CU}$",
                                    variable_group == "mu_sigma" ~ "$\\sigma^{\\mu}$",
                                    variable_group == "e_t[]" ~ "$\\epsilon_{i,t}$",
                                    variable_group == "rho[]" ~ "$\\rho_{i}$",
                                    variable_group == "sigmaAR[]" ~ "$\\sigma^{AR}_{i}$",
                                    variable_group == "sd_sigma" ~ "$\\sigma^{\\sigma}$",
                                    variable_group == "sd_sigma_cu" ~ "$\\sigma^{\\sigma}_{CU}$",
                                    variable_group == "sigma[]" ~ "$\\sigma_i$",
                                    variable_group == "sigma_a_cu" ~ "$\\sigma^{\\alpha,CU}$",
                                    variable_group == "sigma_a_j[]" ~ "$\\sigma^{\\alpha,i}_{CU}$",
                                    variable_group == "sigma_for_cu" ~ "$\\sigma^{Forestry,CU}$",
                                    variable_group == "sigma_for_rv" ~ "$\\sigma^{Forestry,i}$",
                                    variable_group == "sigma_npgo_cu" ~ "$\\sigma^{NPGO,CU}$",
                                    variable_group == "sigma_npgo_rv" ~ "$\\sigma^{NPGO,i}$",
                                    variable_group == "sigma_sst_cu" ~ "$\\sigma^{SST,CU}$",
                                    variable_group == "sigma_sst_rv" ~ "$\\sigma^{SST,i}$",
                                    variable_group == "z_a_cu[]" ~ "$z^{\\alpha}_{CU}$",
                                    variable_group == "z_a_j[]" ~ "$z^{\\alpha}_{i}$",
                                    variable_group == "z_for_cu[]" ~ "$z^{Forestry}_{CU}$",
                                    variable_group == "z_for_rv[]" ~ "$z^{Forestry}_{i}$",
                                    variable_group == "z_npgo_cu[]" ~ "$z^{NPGO}_{CU}$",
                                    variable_group == "z_npgo_rv[]" ~ "$z^{NPGO}_{i}$",
                                    variable_group == "z_sst_cu[]" ~ "$z^{SST}_{CU}$",
                                    variable_group == "z_sst_rv[]" ~ "$z^{SST}_{i}$",
                                    variable_group == "z_sig_cu[]" ~ "$z^{\\sigma}_{CU}$",
                                    variable_group == "z_sig_j[]" ~ "$z^{\\sigma}_{i}$",
                                    TRUE ~ variable_group)) %>% 
  #rename columns
  rename("Parameter group" = variable_group,
         "Rhat (max)" = max_rhat,
         "ESS tail (min)" = min_ess_tail,
         "Estimate (median)" = median_estimate,
         "N_parameters" = n)


#save
write.csv(summary_cpd_pk, here("output_figures_tables",
                                 "supplementary_table4_pink_cpd_diagnostics_by_parameter_group.csv"),
          row.names = FALSE)


# do same for pink with eca model

file <- 'ric_pk_eca_st_noac_ocean_covariates_logR_long_chain_sigma_vector.csv'

summary_eca_pk_file <- read.csv(here('salmon_forestry_data_analysis',
  'stan models','outs','summary',file))


summary_eca_pk <- summary_eca_pk_file %>% 
  select(variable, rhat, median, ess_tail, ess_bulk) %>%
  mutate(variable_group = str_replace_all(variable,c("[0-9]"), "")) %>%
  # mutate(variable_group = str_extract_all(variable,c("[a-z]"))) %>% 
  # mutate(variable_group = str_replace_all(variable_group,"\[", "")) %>% 
  group_by(variable_group) %>% 
  # filter(startsWith(variable, "alpha")) %>% 
  summarize(max_rhat = round(max(rhat),2),
            min_ess_tail = round(min(ess_tail)),
            median_estimate = round(median(median),2),
            n = n()) %>% 
  #remove mu[], lp__, log_lik
  filter(!variable_group %in% c("mu[]", "lp__", "log_lik[]","b[]")) %>% 
  #change "alpha" to "$\alpha$"
  mutate(variable_group = case_when(variable_group == "alpha[]" ~ "$\\alpha_0$",
                                    variable_group == "alpha_cu[]" ~ "$\\alpha_{cu}$",
                                    variable_group == "alpha_j[]" ~ "$\\alpha_{i}$",
                                    variable_group == "Smax[]" ~ "$S_{max}$",
                                    variable_group == "b_for" ~ "$\\beta^{Forestry}_0$",
                                    variable_group == "b_for_cu[]" ~ "$\\beta^{Forestry}_{CU}$",
                                    variable_group == "b_for_rv[]" ~ "$\\beta^{Forestry}_{i}$",
                                    variable_group == "b_npgo" ~ "$\\beta^{NPGO}_{0}$",
                                    variable_group == "b_npgo_cu[]" ~ "$\\beta^{NPGO}_{CO}$",
                                    variable_group == "b_npgo_rv[]" ~ "$\\beta^{NPGO}_{i}$",
                                    variable_group == "b_sst" ~ "$\\beta^{SST}_{0}$",
                                    variable_group == "b_sst_cu[]" ~ "$\\beta^{SST}_{CU}$",
                                    variable_group == "b_sst_rv[]" ~ "$\\beta^{SST}_{i}$",
                                    variable_group == "cu_sigma[]" ~ "$\\sigma_{CU}$",
                                    variable_group == "mu_sigma" ~ "$\\sigma^{\\mu}$",
                                    variable_group == "e_t[]" ~ "$\\epsilon_{i,t}$",
                                    variable_group == "rho[]" ~ "$\\rho_{i}$",
                                    variable_group == "sigmaAR[]" ~ "$\\sigma^{AR}_{i}$",
                                    variable_group == "sd_sigma" ~ "$\\sigma^{\\sigma}$",
                                    variable_group == "sd_sigma_cu" ~ "$\\sigma^{\\sigma}_{CU}$",
                                    variable_group == "sigma[]" ~ "$\\sigma_i$",
                                    variable_group == "sigma_a_cu" ~ "$\\sigma^{\\alpha,CU}$",
                                    variable_group == "sigma_a_j[]" ~ "$\\sigma^{\\alpha,i}_{CU}$",
                                    variable_group == "sigma_for_cu" ~ "$\\sigma^{Forestry,CU}$",
                                    variable_group == "sigma_for_rv" ~ "$\\sigma^{Forestry,i}$",
                                    variable_group == "sigma_npgo_cu" ~ "$\\sigma^{NPGO,CU}$",
                                    variable_group == "sigma_npgo_rv" ~ "$\\sigma^{NPGO,i}$",
                                    variable_group == "sigma_sst_cu" ~ "$\\sigma^{SST,CU}$",
                                    variable_group == "sigma_sst_rv" ~ "$\\sigma^{SST,i}$",
                                    variable_group == "z_a_cu[]" ~ "$z^{\\alpha}_{CU}$",
                                    variable_group == "z_a_j[]" ~ "$z^{\\alpha}_{i}$",
                                    variable_group == "z_for_cu[]" ~ "$z^{Forestry}_{CU}$",
                                    variable_group == "z_for_rv[]" ~ "$z^{Forestry}_{i}$",
                                    variable_group == "z_npgo_cu[]" ~ "$z^{NPGO}_{CU}$",
                                    variable_group == "z_npgo_rv[]" ~ "$z^{NPGO}_{i}$",
                                    variable_group == "z_sst_cu[]" ~ "$z^{SST}_{CU}$",
                                    variable_group == "z_sst_rv[]" ~ "$z^{SST}_{i}$",
                                    variable_group == "z_sig_cu[]" ~ "$z^{\\sigma}_{CU}$",
                                    variable_group == "z_sig_j[]" ~ "$z^{\\sigma}_{i}$",
                                    TRUE ~ variable_group)) %>% 
  #rename columns
  rename("Parameter group" = variable_group,
         "Rhat (max)" = max_rhat,
         "ESS tail (min)" = min_ess_tail,
         "Estimate (median)" = median_estimate,
         "N_parameters" = n)

#save
write.csv(summary_eca_pk, here("output_figures_tables",
                                 "supplementary_table4_pink_eca_diagnostics_by_parameter_group.csv"),
          row.names = FALSE)





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


chum_effect_sizes_eca <- effect_sizes_df(ric_chm_eca_ocean_covariates_logR_long_chain, species = "chum" )
chum_effect_sizes_cpd <- effect_sizes_df(ric_chm_cpd_ocean_covariates_logR_long_chain, species = "chum" )
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
#save

write.csv(all_effect_sizes, here("output_figures_tables",
                                 "forestry_ocean_effect_sizes_by_species_and_metric.csv"),
          row.names = FALSE)


#calculate recruitment decline at most recent average CDA for chum and for pink


recruitment_decline_df <- function(posterior, effect, species, covariate_value){
  
  if(species == "chum"){
    df <- ch20rsc 
    
  } else if(species == "pink"){
    df <- pk10r
  }
  
  recruitment_df <- NULL
  
  b <- posterior %>% select(starts_with("b_for"))
  
  if(effect == "eca"){
    covariate_std = (sqrt(covariate_value)-mean(df$sqrt.ECA))/sd(df$sqrt.ECA)
    
    forestry_eca <- seq(0,1, length.out = 100)
    
    forestry_sqrt <- sqrt(forestry_eca)
    
    forestry_sqrt_std = (forestry_sqrt-mean(forestry_sqrt))/sd(forestry_sqrt)
    
    no_forestry <- min(forestry_sqrt_std)
    no_forestry = (min((sqrt(df$disturbedarea_prct_cs)-mean(sqrt(df$disturbedarea_prct_cs)))/sd(sqrt(df$disturbedarea_prct_cs))))
    
    recruitment = (exp(as.matrix(b[,1])%*%
                           (covariate_std - no_forestry)))*100 - 100
    
    
  } else if(effect == "cpd"){
    covariate_std = (sqrt(covariate_value)-mean(df$sqrt.CPD))/sd(df$sqrt.CPD)
    
    forestry_cpd <- seq(0,100, length.out = 100)
    
    #to calculate no forestry in the standardized scale
    forestry_sqrt <- sqrt(forestry_cpd)
    
    forestry_sqrt_std = (forestry_sqrt-mean(forestry_sqrt))/sd(forestry_sqrt)
    
    no_forestry <- min(forestry_sqrt_std)
    
    recruitment_decline = (exp(as.matrix(b[,1])%*%
                           (covariate_std - no_forestry)))*100 - 100
  }
  
  
  recruitment_df = data.frame(species = species,
                               effect = effect,
                               covariate_value = covariate_value,
                               recruitment_median = round(median(recruitment),2),
                               recruitment_025 = HDInterval::hdi(recruitment)[1],
                               recruitment_975 = HDInterval::hdi(recruitment)[2]
  )
  
  return(recruitment_df)
}
  

# most recent average CPD


# chum

df <- ch20rsc
current_average_cpd <- df %>% 
  group_by(River_n) %>% 
  summarize(max_cpd = max(disturbedarea_prct_cs)) %>% 
  summarize(current_average_cpd = mean(max_cpd))

max_average_eca <- df %>% 
  group_by(River_n) %>% 
  summarize(max_eca = max(ECA_age_proxy_forested_only)) %>% 
  summarize(max_average_eca = mean(max_eca))

overall_min_cpd <- df %>% 
  summarize(min_cpd = min(disturbedarea_prct_cs)) 

# pink

df <- pk10r
current_average_cpd_pink <- df %>% 
  group_by(River_n) %>% 
  summarize(max_cpd = max(disturbedarea_prct_cs)) %>% 
  summarize(current_average_cpd = mean(max_cpd))

max_average_eca_pink <- df %>%
  group_by(River_n) %>% 
  summarize(max_eca = max(ECA_age_proxy_forested_only)) %>% 
  summarize(max_average_eca = mean(max_eca))




#calculate recruitment decline for current_average_cpd and max_average_eca

chum_recruitment_decline_cpd <- recruitment_decline_df(ric_chm_cpd_ocean_covariates_logR_long_chain,
                                                         effect = "cpd",
                                                         species = "chum",
                                                         covariate_value = current_average_cpd$current_average_cpd)

#something is wrong - inconsistent with previously calculated values


recruitment_decline_river_df <- function(posterior, effect, species){
  
  if(species == "chum"){
    df <- ch20rsc 
    
  } else if(species == "pink"){
    df <- pk10r
  }
  
  full_productivity <- NULL
  
  for (i in 1:length(unique(df$River_n))){
    
    river <- unique(df$River_n)[i]
    
    river_data <- df %>% filter(River_n == river)
    
    b_rv <- posterior %>% select(starts_with("b_for_rv")) %>%
      select(ends_with(paste0("[",river,"]")))
    
    eca_sqrt_std_river <- max(river_data$sqrt.ECA.std)
    #minimum forestry possible -0
    eca_river <- max(river_data$ECA_age_proxy_forested_only)
    
    forestry_eca <- seq(0,1, length.out = 100)
    
    cpd_sqrt_std_river <- max(river_data$sqrt.CPD.std)
    #minimum forestry possible - 0
    cpd_river <- max(river_data$disturbedarea_prct_cs)
    
    forestry_cpd <- seq(0,100, length.out = 100)
    
    #to calculate no forestry in the standardized scale
    forestry_sqrt <- sqrt(forestry_cpd)
    
    forestry_sqrt_std = (forestry_sqrt-mean(forestry_sqrt))/sd(forestry_sqrt)
    
    no_forestry <- min(forestry_sqrt_std)
    
    productivity <- (exp(as.matrix(b_rv[,1])%*%
                           (cpd_sqrt_std_river-no_forestry)))*100 - 100
    
    productivity_median <- apply(productivity,2,median)
    
    productivity_median_df <- data.frame(River = unique(river_data$River),
                                         productivity_50 = apply(productivity,2,median),
                                         productivity_25 = apply(productivity,2,quantile, probs = 0.25),
                                         productivity_75 = apply(productivity,2,quantile, probs = 0.75),
                                         productivity_025 = apply(productivity,2,quantile, probs = 0.025),
                                         productivity_975 = apply(productivity,2,quantile, probs = 0.975),
                                         productivity_025_hdi = apply(productivity,2, HDInterval::hdi, credMass = 0.95)[1,],
                                         productivity_975_hdi = apply(productivity,2, HDInterval::hdi, credMass = 0.95)[2,],
                                         productivity_25_hdi = apply(productivity,2, HDInterval::hdi, credMass = 0.5)[1,],
                                         productivity_75_hdi = apply(productivity,2, HDInterval::hdi, credMass = 0.5)[2,],
                                         
                                         forestry = cpd_river,
                                         CU = unique(river_data$CU_name)
    )
    
    full_productivity <- rbind(full_productivity, productivity_median_df)
    
    
  }
  
  
  return(full_productivity)
  
  
  
  
  
  
  
}


effect_sizes_cu_df <- function(posterior, effect, species){
  
  if(species == "chum"){
    df <- ch20rsc 
    
  } else if(species == "pink"){
    df <- pk10r
  }
  
  effect_df <- NULL
  
  for (i in 1:length(unique(df$CU_n))){
    
    cu <- unique(df$CU_n)[i]
    
    cu_data <- df %>% filter(CU_n == cu)
    
    if(effect == "cpd" || effect == "eca"){
      b_cu <- posterior %>% select(starts_with("b_for_cu")) %>%
        select(ends_with(paste0("[",cu,"]")))
      
    } else if(effect == "sst"){
      b_cu <- posterior %>% select(starts_with("b_sst_cu")) %>%
        select(ends_with(paste0("[",cu,"]")))
    } else if(effect == "npgo"){
      b_cu <- posterior %>% select(starts_with("b_npgo_cu")) %>%
        select(ends_with(paste0("[",cu,"]")))
    }
    
    effect_df_cu <- data.frame(CU = unique(cu_data$CU_name),
                               effect_median = round(apply(as.matrix(b_cu[,1]),2,median),2))
    # sym(effect)_25 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.25),
    # sym(effect)_75 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.75),
    # sym(effect)_025 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.025),
    # sym(effect)_975 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.975))
    
    effect_df <- rbind(effect_df, effect_df_cu)
  }
  
  return(effect_df)
}

theoretical_cpd = seq(0,100,1)

sqrt_theoretical_cpd = sqrt(theoretical_cpd)

std_sqrt_theoretical_cpd = (sqrt_theoretical_cpd - mean(ch20rsc$sqrt.CPD))/sd(ch20rsc$sqrt.CPD)

cpd = data.frame(theoretical_cpd, sqrt_theoretical_cpd, std_sqrt_theoretical_cpd)

mean(sqrt_theoretical_cpd)





# function to calculate % change in recruitemnt (with 95% credible interval) of particular river
# input posterior of b_for_rv, species, 2 covariate_values

recruitment_decline_river_df2 <- function(River, b_for_rv, forestry_level_high, forestry_level_low){
  
  recruitment = (exp(as.matrix(b_for_rv[,1])%*%
                       (forestry_level_high-forestry_level_low)))*100 - 100
  
  return(data.frame(River = River, median_recruitment_change = median(recruitment),
                    recruitment_025 = quantile(recruitment, 0.025),
                    recruitment_975 = quantile(recruitment, 0.975)))
  
}

river_data <- ch20rsc %>% 
  filter(River == "CARNATION CREEK")

b_for_rv <- ric_chm_cpd_ocean_covariates_logR_long_chain %>% 
  select(starts_with(paste0("b_for_rv[",river_data$River_n[1],"]")))

forestry_level_high <- max(river_data$sqrt.CPD.std)

forestry_level_low <- min(river_data$sqrt.CPD.std)

# disturbed_prct_cs value associated with max and min CPD std

river_data$disturbedarea_prct_cs[river_data$sqrt.CPD.std == forestry_level_high]
river_data$disturbedarea_prct_cs[river_data$sqrt.CPD.std == forestry_level_low]


recruitment_decline_river_df2("CARNATION CREEK", b_for_rv, forestry_level_high, forestry_level_low)


recruitment_decline_river_df2("CARNATION CREEK", b_for_rv, cpd$std_sqrt_theoretical_cpd[cpd$theoretical_cpd == 70],
                              cpd$std_sqrt_theoretical_cpd[cpd$theoretical_cpd == 1])


# make new ersst figure ---------------------------------------------------

# Load libraries

library(tidyverse)
library(here)
library(ersst)
library(sf)
library(bcmaps)
library(ggplot2)
library(hues)

# Load data


#plot

bc_boundary <- bc_bound() %>% st_transform(4326)

sst_df <- read.csv(here("data_processing_sst", "data", "sst_ersst_df.csv"))

chum_salmon_data_location <- ch20rsc %>% 
  select(CU,  Y_LAT, X_LONG, River, GFE_ID) %>% 
  distinct()

pink_salmon_data_location <- pk10r %>% 
  select(CU,  Y_LAT, X_LONG, River, GFE_ID) %>% 
  distinct()

sst_df %>% filter(!is.na(sst), year == 1959 | year == 2014) %>% 
  select(lat, lon, sst, year, month) %>% 
  group_by(lat, lon, year) %>% 
  summarize(sst = mean(sst, na.rm = TRUE)) %>%
  View()

sst_df %>% filter(!is.na(sst), year == 1955 | year == 1997) %>% 
  select(lat, lon, sst, year, month) %>% 
  group_by(lat, lon, year) %>% 
  summarize(sst = mean(sst, na.rm = TRUE)) %>% 
  ggplot() +
  geom_sf(data = bc_boundary, fill = "transparent", color = "slategray", alpha = 0.2, linewidth=0.5) +
  facet_wrap(~year) +
  geom_raster(aes(x = lon, y = lat, fill = sst), alpha = 0.5) +
  #plot locations of sst data
  geom_point(aes(x = lon, y = lat, shape = "SST data location"), color = "slategray", alpha = 0.8, size = 1.5) +
  scale_fill_viridis_c() +
  # geom_point(data = lighthouse_locations, aes(x = long, y = lat), color = "darkred", size = 3, alpha=0.8) +
  geom_point(data=chum_salmon_data_location, aes(x = X_LONG, y = Y_LAT,  shape = "watershed outlet"), size = 1, alpha=0.6, color = "gray20") +
  geom_point(data=pink_salmon_data_location, aes(x = X_LONG, y = Y_LAT, shape = "watershed outlet"), size = 1, alpha=0.6, color = "gray20") +
  # geom_point(data=pko_salmon_data_location, aes(x = X_LONG, y = Y_LAT, color = "pink-odd"), size = 2, alpha=0.2) +
  # geom_text(data = lighthouse_locations, aes(x = long, y = lat, label = location), 
  #           nudge_x = -1.5, nudge_y = 0.2, size = 3) +
  # ggrepel::geom_label_repel(data = lighthouse_locations, aes(x = long, y = lat, label = location),
  #                           nudge_x = -1.5, nudge_y = 0.2, size = 3, background = "white", alpha = 0.5) +
  # scale_color_manual(values = c("chum" = "#69C5C5",
  #                               # "pink-even" = "#C76F6F",
  #                               "pink" = "#9E70A1")) +
  scale_shape_manual(values = c("watershed outlet" = 1, "SST data location" = 0)) +
  scale_color_manual(values = c("watershed outlet" = "gray20")) +
  scale_x_continuous(limits = c(-133, -122), breaks = seq(-133,-122,5)) +
  scale_y_continuous(limits = c(47, 58), breaks = seq(47,58,2)) +
  guides(shape = guide_legend(title = ""), override.aes = list(size = 8, alpha = 1),
         fill = guide_legend(title = "SST (°C)")) +
           # labs(title = "Spring Extended Reconstructed Sea-Surface Temperature (ERSST)") + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic()+
  theme(legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        #remove white space
        plot.margin = unit(c(0,0,0,0), "cm")
  )

ggsave(here("output_figures_tables", "manuscript_supplementary_feb2026_ersst_1955_1997.png"), width = 8, height = 5, dpi = 300)



# chum


current_average_cpd <- ch20rsc %>% 
  group_by(River_n) %>% 
  summarize(max_cpd = max(disturbedarea_prct_cs)) %>% 
  summarize(current_average_cpd = mean(max_cpd))

max_average_eca <- ch20rsc %>% 
  group_by(River_n) %>% 
  summarize(max_eca = max(ECA_age_proxy_forested_only)) %>% 
  summarize(max_average_eca = mean(max_eca))

overall_min_cpd <- df %>% 
  summarize(min_cpd = min(disturbedarea_prct_cs)) 

b_cpd_chum <- ric_chm_cpd_ocean_covariates_logR_long_chain %>% 
  select("b_for")



b_eca_chum <- ric_chm_eca_ocean_covariates_logR_long_chain %>% 
  select("b_for")


# pink


current_average_cpd_pink <- pk10r %>% 
  group_by(River_n) %>% 
  summarize(max_cpd = max(disturbedarea_prct_cs)) %>% 
  summarize(current_average_cpd = mean(max_cpd))

max_average_eca_pink <- pk10r %>%
  group_by(River_n) %>% 
  summarize(max_eca = max(ECA_age_proxy_forested_only)) %>% 
  summarize(max_average_eca = mean(max_eca))



theoretical_cpd = seq(0,100,1)

sqrt_theoretical_cpd = sqrt(theoretical_cpd)

std_sqrt_theoretical_cpd = (sqrt_theoretical_cpd - mean(sqrt_theoretical_cpd))/sd(sqrt_theoretical_cpd)

cpd = data.frame(theoretical_cpd, sqrt_theoretical_cpd, std_sqrt_theoretical_cpd)


recruitment_decline_df2 <- function(b_for, forestry_level_high, forestry_level_low){
  
  recruitment = (exp(as.matrix(b_for[,1])%*%
                       (forestry_level_high-forestry_level_low)))*100 - 100
  
  return(data.frame(median_recruitment_change = median(recruitment),
                    recruitment_025 = quantile(recruitment, 0.025),
                    recruitment_975 = quantile(recruitment, 0.975)))
  
}



recruitment_decline_df2(b_cpd_chum, cpd$std_sqrt_theoretical_cpd[which.min(abs(cpd$theoretical_cpd-current_average_cpd$current_average_cpd))],
                        min(cpd$std_sqrt_theoretical_cpd))


theoretical_eca <- seq(0,1,0.01)

sqrt_theoretical_eca <- sqrt(theoretical_eca)

std_sqrt_theoretical_eca <- (sqrt_theoretical_eca - mean(sqrt_theoretical_eca))/sd(sqrt_theoretical_eca)

eca <- data.frame(theoretical_eca, sqrt_theoretical_eca, std_sqrt_theoretical_eca)

recruitment_decline_df2(b_eca_chum, eca$std_sqrt_theoretical_eca[which.min(abs(eca$theoretical_eca-max_average_eca$max_average_eca))],
                        min(eca$std_sqrt_theoretical_eca))

#pink

b_cpd_pink <- ric_pk_cpd_ersst_long_chain %>% 
  select("b_for")

b_eca_pink <- ric_pk_eca_ersst_long_chain %>% 
  select("b_for")


recruitment_decline_df2(b_cpd_pink, cpd$std_sqrt_theoretical_cpd[which.min(abs(cpd$theoretical_cpd-current_average_cpd_pink$current_average_cpd))],
                        min(cpd$std_sqrt_theoretical_cpd))


recruitment_decline_df2(b_eca_pink, eca$std_sqrt_theoretical_eca[which.min(abs(eca$theoretical_eca-max_average_eca_pink$max_average_eca))],
                        min(eca$std_sqrt_theoretical_eca))

effect_sizes_river_df <- function(posterior, species, river){
  
  if(species == "chum"){
    df <- ch20rsc 
    
  } else if(species == "pink"){
    df <- pk10r
  }
  
  river_data <- df %>% 
    filter(River == river)
  
  effect_df <- NULL
  
  b_for <- posterior %>% select(starts_with("b_for_rv")) %>%
        select(ends_with(paste0("[",river_data$River_n[1],"]")))
      
  b_sst <- posterior %>% select(starts_with("b_sst_rv")) %>%
        select(ends_with(paste0("[",river_data$River_n[1],"]")))
  
  b_npgo <- posterior %>% select(starts_with("b_npgo_rv")) %>%
        select(ends_with(paste0("[",river_data$River_n[1],"]")))
    
  effect_df <- data.frame(River = unique(river_data$River),
                          forestry_effect_median = round(apply(as.matrix(b_for[,1]),2,median),2),
                          forestry_effect_median_025 = round(apply(as.matrix(b_for[,1]),2,quantile, probs = 0.025),2),
                          forestry_effect_median_975 = round(apply(as.matrix(b_for[,1]),2,quantile, probs = 0.975),2),
                          sst_effect_median = round(apply(as.matrix(b_sst[,1]),2,median),2),
                          sst_effect_median_025 = round(apply(as.matrix(b_sst[,1]),2,quantile, probs = 0.025),2),
                          sst_effect_median_975 = round(apply(as.matrix(b_sst[,1]),2,quantile, probs = 0.975),2),
                          npgo_effect_median = round(apply(as.matrix(b_npgo[,1]),2,median),2),
                          npgo_effect_median_025 = round(apply(as.matrix(b_npgo[,1]),2,quantile, probs = 0.025),2),
                          npgo_effect_median_975 = round(apply(as.matrix(b_npgo[,1]),2,quantile, probs = 0.975),2)
                            
                            )
                            
    # sym(effect)_25 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.25),
    # sym(effect)_75 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.75),
    # sym(effect)_025 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.025),
    # sym(effect)_975 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.975))
    
    # effect_df <- rbind(effect_df, effect_df_cu)
  
  
  return(effect_df)
}


effect_sizes_river_df(ric_chm_cpd_ocean_covariates_logR_long_chain, species = "chum", river = "VINER SOUND CREEK")
effect_sizes_river_df(ric_chm_cpd_ocean_covariates_logR_long_chain, species = "chum", river = "CARNATION CREEK")
effect_sizes_river_df(ric_chm_cpd_ocean_covariates_logR_long_chain, species = "chum", river = "PHILLIPS RIVER")
effect_sizes_river_df(ric_pk_cpd_ersst_long_chain, species = "pink", river = "PHILLIPS RIVER")

b_eca_chum_phillips_river <- ric_chm_eca_ocean_covariates_logR_long_chain %>% 
  select(starts_with(paste0("b_for_rv[",ch20rsc$River_n[ch20rsc$River == "PHILLIPS RIVER"][1],"]")))

b_cpd_pink_phillips_river <- ric_pk_cpd_ersst_long_chain %>% 
  select(starts_with(paste0("b_for_rv[",pk10r$River_n[pk10r$River == "PHILLIPS RIVER"][1],"]")))
 
b_cpd_chum_phillips_river <- ric_chm_cpd_ocean_covariates_logR_long_chain %>% 
  select(starts_with(paste0("b_for_rv[",ch20rsc$River_n[ch20rsc$River == "PHILLIPS RIVER"][1],"]")))


recruitment_decline_df2(b_eca_chum_phillips_river, eca$std_sqrt_theoretical_eca[which.min(abs(eca$theoretical_eca-0.20))],
                        min(eca$std_sqrt_theoretical_eca))

recruitment_decline_df2(b_eca_chum_phillips_river, eca$std_sqrt_theoretical_eca[which.min(abs(eca$theoretical_eca-0.25))],
                        min(eca$std_sqrt_theoretical_eca))

recruitment_decline_df2(b_cpd_chum_phillips_river, 
                        cpd$std_sqrt_theoretical_cpd[which.min(abs(cpd$theoretical_cpd-41.4))],
                        min(cpd$std_sqrt_theoretical_cpd))


current_average_cpd_pink_phillips_river <- max(pk10r$disturbedarea_prct_cs[pk10r$River == "PHILLIPS RIVER"])


recruitment_decline_df2(b_cpd_pink_phillips_river, 
                        cpd$std_sqrt_theoretical_cpd[which.min(abs(cpd$theoretical_cpd-current_average_cpd_pink_phillips_river))],
                        min(cpd$std_sqrt_theoretical_cpd))





# table of effect size - cumulative disturbance ---------------------------

effect_chum_cpd_sst_npgo <- effect_sizes_cu_df(ric_chm_cpd_ocean_covariates_logR_long_chain, effect = "cpd", species = "chum") %>% 
  rename(cpd_effect_median = effect_median) %>%
  left_join(effect_sizes_cu_df(ric_chm_cpd_ocean_covariates_logR_long_chain, effect = "sst", species = "chum") %>%
              rename(sst_effect_median = effect_median), by = "CU") %>%
  left_join(effect_sizes_cu_df(ric_chm_cpd_ocean_covariates_logR_long_chain, effect = "npgo", species = "chum") %>%
              rename(npgo_effect_median = effect_median), by = "CU") 



#do same for river level effects

effect_sizes_river_df <- function(posterior, effect, species){
  
  if(species == "chum"){
    df <- ch20rsc 
    
  } else if(species == "pink"){
    df <- pk10r
  }
  
  effect_df <- NULL
  
  for (i in 1:length(unique(df$River_n))){
    
    river <- unique(df$River_n)[i]
    
    river_data <- df %>% filter(River_n == river)
    
    if(effect == "cpd" || effect == "eca"){
      b_rv <- posterior %>% select(starts_with("b_for_rv")) %>%
        select(ends_with(paste0("[",river,"]")))
      
    } else if(effect == "sst"){
      b_rv <- posterior %>% select(starts_with("b_sst_rv")) %>%
        select(ends_with(paste0("[",river,"]")))
    } else if(effect == "npgo"){
      b_rv <- posterior %>% select(starts_with("b_npgo_rv")) %>%
        select(ends_with(paste0("[",river,"]")))
    }
    
    effect_df_rv <- data.frame(River = unique(river_data$River),
                               effect_median = round(apply(as.matrix(b_rv[,1]),2,median),2),
                               CU = unique(river_data$CU_name))
    # sym(effect)_25 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.25),
    # sym(effect)_75 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.75),
    # sym(effect)_025 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.025),
    # sym(effect)_975 = apply(as.matrix(b_cu[,1]),2,quantile, probs = 0.975))
    
    effect_df <- rbind(effect_df, effect_df_rv)
  }
  
  return(effect_df)
}

effect_chum_cpd_sst_npgo_rv <- effect_sizes_river_df(ric_chm_cpd_ocean_covariates_logR_long_chain, effect = "cpd", species = "chum") %>% 
  rename(cpd_effect_median = effect_median) %>%
  left_join(effect_sizes_river_df(ric_chm_cpd_ocean_covariates_logR_long_chain, effect = "sst", species = "chum") %>%
              rename(sst_effect_median = effect_median), by = c("River","CU")) %>%
  left_join(effect_sizes_river_df(ric_chm_cpd_ocean_covariates_logR_long_chain, effect = "npgo", species = "chum") %>%
              rename(npgo_effect_median = effect_median), by = c("River","CU")) 


# group by CU and then calculate the proportion of rivers in which effect of cpd is greater in magnitude
# than effect of NPGO and SST

prop_rivers <- effect_chum_cpd_sst_npgo_rv %>% 
  mutate(flag = (abs(cpd_effect_median) >= abs(sst_effect_median) & abs(cpd_effect_median) >= abs(npgo_effect_median))) %>%
  # filter(CU == "Southwest Vancouver Island") %>% 
  # View()
  group_by(CU) %>% 
  summarize(n_rivers_forestry = sum(flag),
            n_rivers = n()) %>% 
  mutate(proportion_forestry_greater = round(n_rivers_forestry/n_rivers,2)) %>%
  select(CU, proportion_forestry_greater)

#join

effect_chum_cpd_sst_npgo_w_prop <- effect_chum_cpd_sst_npgo %>%
  left_join(prop_rivers, by = "CU")



write.csv(effect_chum_cpd_sst_npgo_w_prop,
          here("output_figures_tables","manuscript_feb2026_chum_ricker_cpd_sst_npgo_effect_sizes_by_cu_table_w_prop.csv"),
          row.names = FALSE)

effect_chum_eca_sst_npgo <- effect_sizes_cu_df(ric_chm_eca_ocean_covariates_logR_long_chain, effect = "eca", species = "chum") %>% 
  rename(eca_effect_median = effect_median) %>%
  left_join(effect_sizes_cu_df(ric_chm_eca_ocean_covariates_logR_long_chain, effect = "sst", species = "chum") %>%
              rename(sst_effect_median = effect_median), by = "CU") %>%
  left_join(effect_sizes_cu_df(ric_chm_eca_ocean_covariates_logR_long_chain, effect = "npgo", species = "chum") %>%
              rename(npgo_effect_median = effect_median), by = "CU") 


effect_chum_eca_sst_npgo_rv <- effect_sizes_river_df(ric_chm_eca_ocean_covariates_logR_long_chain, effect = "eca", species = "chum") %>% 
  rename(eca_effect_median = effect_median) %>%
  left_join(effect_sizes_river_df(ric_chm_eca_ocean_covariates_logR_long_chain, effect = "sst", species = "chum") %>%
              rename(sst_effect_median = effect_median), by = c("River","CU")) %>%
  left_join(effect_sizes_river_df(ric_chm_eca_ocean_covariates_logR_long_chain, effect = "npgo", species = "chum") %>%
              rename(npgo_effect_median = effect_median), by = c("River","CU")) 


prop_rivers_eca <- effect_chum_eca_sst_npgo_rv %>% 
  mutate(flag = (abs(eca_effect_median) >= abs(sst_effect_median) & abs(eca_effect_median) >= abs(npgo_effect_median))) %>%
  # filter(CU == "Southwest Vancouver Island") %>% 
  # View()
  group_by(CU) %>% 
  summarize(n_rivers_forestry = sum(flag),
            n_rivers = n()) %>% 
  mutate(proportion_forestry_greater = round(n_rivers_forestry/n_rivers,2)) %>%
  select(CU, proportion_forestry_greater)

#join

effect_chum_eca_sst_npgo_w_prop <- effect_chum_eca_sst_npgo %>%
  left_join(prop_rivers_eca, by = "CU")


write.csv(effect_chum_eca_sst_npgo_w_prop,
          here("output_figures_tables","manuscript_feb2026_chum_ricker_eca_sst_npgo_effect_sizes_by_cu_table_w_prop.csv"),
          row.names = FALSE)


recruitment_decline_river_df_new <- function(posterior, effect, species){
  
  if(species == "chum"){
    df <- ch20rsc 
    
  } else if(species == "pink"){
    df <- pk10r
  }
  
  full_productivity <- NULL
  
  for (i in 1:length(unique(df$River_n))){
    
    river <- unique(df$River_n)[i]
    
    river_data <- df %>% filter(River_n == river)
    
    b_rv <- posterior %>% select(starts_with("b_for_rv")) %>%
      select(ends_with(paste0("[",river,"]")))
    
    eca_sqrt_std_river <- max(river_data$sqrt.ECA.std)
    #minimum forestry possible -0
    eca_river <- max(river_data$ECA_age_proxy_forested_only)
    
    forestry_eca <- seq(0,1, length.out = 100)
    
    cpd_sqrt_std_river <- max(river_data$sqrt.CPD.std)
    #minimum forestry possible - 0
    cpd_river <- max(river_data$disturbedarea_prct_cs)
    
    forestry_cpd <- seq(0,100, length.out = 100)
    
    #to calculate no forestry in the standardized scale
    forestry_sqrt <- sqrt(forestry_cpd)
    
    forestry_sqrt_std = (forestry_sqrt-mean(forestry_sqrt))/sd(forestry_sqrt)
    
    forestry_cpd_df = data.frame(forestry_cpd, forestry_sqrt,forestry_sqrt_std)
    
    high_forestry = forestry_cpd_df$forestry_sqrt_std[which.min(abs(forestry_cpd_df$forestry_cpd - cpd_river))]
    
    no_forestry <- min(forestry_sqrt_std)
    
    productivity <- (exp(as.matrix(b_rv[,1])%*%
                           (high_forestry-no_forestry)))*100 - 100
    
    productivity_median <- apply(productivity,2,median)
    
    productivity_median_df <- data.frame(River = unique(river_data$River),
                                         productivity_50 = apply(productivity,2,median),
                                         productivity_25 = apply(productivity,2,quantile, probs = 0.25),
                                         productivity_75 = apply(productivity,2,quantile, probs = 0.75),
                                         productivity_025 = apply(productivity,2,quantile, probs = 0.025),
                                         productivity_975 = apply(productivity,2,quantile, probs = 0.975),
                                         productivity_025_hdi = apply(productivity,2, HDInterval::hdi, credMass = 0.95)[1,],
                                         productivity_975_hdi = apply(productivity,2, HDInterval::hdi, credMass = 0.95)[2,],
                                         productivity_25_hdi = apply(productivity,2, HDInterval::hdi, credMass = 0.5)[1,],
                                         productivity_75_hdi = apply(productivity,2, HDInterval::hdi, credMass = 0.5)[2,],
                                         
                                         forestry = cpd_river,
                                         CU = unique(river_data$CU_name)
    )
    
    full_productivity <- rbind(full_productivity, productivity_median_df)
    
    
  }
  
  
  return(full_productivity)
  
  
  
  
  
  
  
}

ric_chm_cpd_recruitment_decline_river <- recruitment_decline_river_df_new(ric_chm_cpd_ocean_covariates_logR_long_chain, 
                                                                          effect = "cpd", species = "chum")



important_rivers <- c("Nimpkish River", "Skeena River", "Fraser River", "Capilano River",
                      "Squamish River", "Cheakamus River", "Pitt River", "Alouette River",
                      "Chilliwack River", "Vedder River", "Cowichan River", "Koksilah River",
                      "Goldstream River", "Campbell River", "Qualicum River",
                      # "Kingcome River", 
                      "Shoal Harbour Creek")

casestudy_watersheds <- c("Carnation Creek", "Viner Sound Creek", 
                          "Neekas Creek", "Deena Creek", "Phillips River")



# make forest plot of estimates of decline from highest to lowest rivers
ric_chm_cpd_recruitment_decline_river %>% 
  arrange(desc(productivity_50)) %>%
  mutate(River = str_to_title(River)) %>% 
  #change River to title case and then compare to important rivers
  mutate(important = ifelse((River %in% important_rivers | River %in% casestudy_watersheds), "yes", "no")) %>%
  mutate(River2 = factor(River, levels = River)) %>% 
  ggplot(aes(x = River, y = productivity_50)) +
  geom_point(aes(y = productivity_50, x = River2), color = '#516479',fill = "white", size = 1, alpha = 0.5) +
  geom_errorbar(aes(ymin = productivity_025_hdi, ymax = productivity_975_hdi ), color = '#516479', width = 0, alpha = 0.5) +
  geom_errorbar(aes(ymin = productivity_25_hdi, ymax = productivity_75_hdi ), color = '#516479', width = 0, alpha = 0.7) +
  geom_text_repel(color = "gray20", aes(label = ifelse(important == "yes", paste(River,CU,
                                                                                 paste0(round(productivity_50,1),"%"), sep = ", "), NA)), 
                  size = 3, max.overlaps = 20,
                  direction    = "y", 
                  box.padding = 0.3, hjust = -1.5)+ 
  coord_flip() +
  # scale_color_manual(name = 'Model type', values = c('independent alpha' = 'cadetblue', 'hierarchical alpha' = 'coral', 'hierarchical alpha - ricker' = 'darkgoldenrod')) +
  labs(#title = 'Estimated percent change in river-level productivity',
    x = 'River',
    y = 'Change in productivity (%)') +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # axis.text.y = element_text(size = 4),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

#save 
ggsave(here("output_figures_tables","supplementary_fig28_may2026_chum_ricker_cpd_recruitment_decline_by_river_forest_plot.png"), width = 8, height = 10)




###########

#case study watersheds



watersheds <- c("VINER SOUND CREEK","CARNATION CREEK", "PHILLIPS RIVER", "NIMPKISH RIVER", "DEENA CREEK", "NEEKAS CREEK")

case_study_watersheds_data <- ch20rsc %>% 
  filter(River %in% watersheds)

plot_predicted_recruits <- function(posterior1 = ric_chm_cpd_ocean_covariates_logR,
                                    posterior2 = ric_chm_eca_ocean_covariates_logR,
                                    river = "CARNATION CREEK",  
                                    effect1 = "cpd",
                                    effect2 = "eca",
                                    species = "chum", 
                                    model1 = "CPD",
                                    model2 = "ECA"){
  if(species == "chum"){
    df <- ch20rsc 
    river_data <- ch20rsc %>% filter(River == river)
    river <- river_data$River_n[1]
    # df$sst.std <- (ch20rsc$spring_ersst-mean(ch20rsc$spring_ersst))/sd(ch20rsc$spring_ersst)
    
  } else if(species == "pink"){
    df <- pk10r
    river_data <- pk10r %>% filter(River == river)
    river <- river_data$River_n2[1]
    # df$sst.std <- (pk10r$spring_ersst-mean(pk10r$spring_ersst))/sd(pk10r$spring_ersst)
  }
  
  posterior_df_b_for <- posterior1 %>%
    select(starts_with(paste0('b_for_rv[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_for_rv',
                 values_to = "forestry") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  posterior_df_alpha <- posterior1 %>%
    select(starts_with(paste0('alpha_j[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'alpha_j',
                 values_to = "alpha_j") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  posterior_df_Smax <- posterior1 %>%
    select(starts_with(paste0('Smax[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'Smax',
                 values_to = "Smax") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  posterior_df_npgo <- posterior1 %>%
    select(starts_with(paste0('b_npgo_rv[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_npgo_rv',
                 values_to = "npgo") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  posterior_df_sst <- posterior1 %>%
    select(starts_with(paste0('b_sst_rv[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_sst_rv',
                 values_to = "sst") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  posterior_df2_b_for <- posterior2 %>% 
    select(starts_with(paste0('b_for_rv[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_for_rv',
                 values_to = "forestry") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  #make a time series of median log Recruits with credible intervals using the spawner time series and forestry time series for each year
  
  predicted_recruits <- data.frame(BroodYear = river_data$BroodYear,
                                   Spawners = river_data$Spawners,
                                   Recruits = river_data$Recruits,
                                   predicted_log_recruits = apply(matrix(log(river_data$Spawners),ncol = length(river_data$Spawners), nrow = length(posterior_df_alpha$alpha_j) ) + 
                                                                    (matrix(posterior_df_alpha$alpha_j, ncol = length(river_data$Spawners), nrow = length(posterior_df_alpha$alpha_j)) - as.matrix(1/posterior_df_Smax[,1])%*%river_data$Spawners + as.matrix(posterior_df_b_for[,1])%*%river_data$sqrt.CPD.std +
                                                                       as.matrix(posterior_df_npgo[,1])%*%river_data$npgo.std + 
                                                                       as.matrix(posterior_df_sst[,1])%*%river_data$sst.std ), 2, median),
                                   predicted_log_recruits_upper <- apply(matrix(log(river_data$Spawners),ncol = length(river_data$Spawners), nrow = length(posterior_df_alpha$alpha_j) ) + 
                                                                           (matrix(posterior_df_alpha$alpha_j, ncol = length(river_data$Spawners), nrow = length(posterior_df_alpha$alpha_j)) - as.matrix(1/posterior_df_Smax[,1])%*%river_data$Spawners + as.matrix(posterior_df_b_for[,1])%*%river_data$sqrt.CPD.std +
                                                                              as.matrix(posterior_df_npgo[,1])%*%river_data$npgo.std + 
                                                                              as.matrix(posterior_df_sst[,1])%*%river_data$sst.std ), 2, quantile, 0.975),
                                   
                                   predicted_log_recruits_lower <- apply(matrix(log(river_data$Spawners),ncol = length(river_data$Spawners), nrow = length(posterior_df_alpha$alpha_j) ) + 
                                                                           (matrix(posterior_df_alpha$alpha_j, ncol = length(river_data$Spawners), nrow = length(posterior_df_alpha$alpha_j)) - as.matrix(1/posterior_df_Smax[,1])%*%river_data$Spawners + as.matrix(posterior_df_b_for[,1])%*%river_data$sqrt.CPD.std +
                                                                              as.matrix(posterior_df_npgo[,1])%*%river_data$npgo.std + 
                                                                              as.matrix(posterior_df_sst[,1])%*%river_data$sst.std ), 2, quantile, 0.025),
                                   
                                   forestry = river_data$sqrt.CPD.std,
                                   
                                   lowest_forestry = min(river_data$sqrt.CPD.std),
                                   
                                   predicted_log_recruits_low_forestry = apply(matrix(log(river_data$Spawners),ncol = length(river_data$Spawners), nrow = length(posterior_df_alpha$alpha_j) ) + 
                                                                                 (matrix(posterior_df_alpha$alpha_j, ncol = length(river_data$Spawners), nrow = length(posterior_df_alpha$alpha_j)) - as.matrix(1/posterior_df_Smax[,1])%*%river_data$Spawners + as.matrix(posterior_df_b_for[,1])%*%rep(min(river_data$sqrt.CPD.std),length(river_data$sqrt.CPD.std)) +
                                                                                    as.matrix(posterior_df_npgo[,1])%*%river_data$npgo.std + 
                                                                                    as.matrix(posterior_df_sst[,1])%*%river_data$sst.std ), 2, median)
                                   
                                   
                                   
  )
  
  
  
  
  
  
  #quick plot of log_recruits time series - observed and predicted
  plot1 <- ggplot() +
    geom_line(data = predicted_recruits, aes(x = BroodYear, y = predicted_log_recruits, color = "Predicted"), size = 1, alpha = 0.5) +
    geom_line(data = predicted_recruits, aes(x = BroodYear, y = predicted_log_recruits_low_forestry, color = "Predicted low forestry"), size = 1, alpha = 0.5) +
    geom_point(data = predicted_recruits, aes(x = BroodYear, y = log(Recruits),  fill = forestry), size = 2,shape = 21, color = "white",  alpha = 0.5) +
    # geom_line(data = predicted_recruits, aes(x = BroodYear, y = log(Recruits), color = "Observed"), size = 2, alpha = 0.5) +
    geom_ribbon(data = predicted_recruits, aes(x = BroodYear, ymin = predicted_log_recruits_lower, ymax = predicted_log_recruits_upper),
                fill = 'gray70', alpha = 0.4) +
    labs(x = "Brood Year", y = "log(Recruits)") +
    scale_color_manual(name = "Legend",
                       values = c("Predicted" = 'gray20',
                                  "Predicted low forestry" = '#A2C5AC')) +
    scale_fill_gradient2(name = 'CPD (standardized)',
                         low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 0)+
    theme_classic() +
    theme(legend.position = "right",
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5))
  
  plot2 <- ggplot() +
    geom_line(data = predicted_recruits, aes(x = BroodYear, y = exp(predicted_log_recruits), color = "Predicted"), size = 1, alpha = 0.5) +
    geom_line(data = predicted_recruits, aes(x = BroodYear, y = exp(predicted_log_recruits_low_forestry), color = "Predicted low forestry"), size = 1, alpha = 0.5) +
    geom_point(data = predicted_recruits, aes(x = BroodYear, y = Recruits,  fill = forestry), size = 2,shape = 21, color = "white",  alpha = 0.5) +
    # geom_line(data = predicted_recruits, aes(x = BroodYear, y = log(Recruits), color = "Observed"), size = 2, alpha = 0.5) +
    geom_ribbon(data = predicted_recruits, aes(x = BroodYear, ymin = exp(predicted_log_recruits_lower), 
                                               ymax = exp(predicted_log_recruits_upper)),
                fill = 'gray70', alpha = 0.4) +
    labs(x = "Brood Year", y = "log(Recruits)") +
    scale_color_manual(name = "Legend",
                       values = c("Predicted" = 'gray20',
                                  "Predicted low forestry" = '#A2C5AC')) +
    scale_fill_gradient2(name = 'CPD (standardized)',
                         low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 0)+
    theme_classic() +
    theme(legend.position = "right",
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5))
  
  return(plot1)
  
  
}

# plot_predicted_recruits(river = watersheds[1])


plot_recruit_spawner_river <- function(data, species = "chum", river_name,  posterior, posterior_a_t, posterior_a_t_bh){
  
  # river_data <- df %>% filter(River_n == river)
  
  if(species == "chum"){
    df <- ch20rsc 
    river_data <- ch20rsc %>% filter(River == river_name)
    river <- river_data$River_n[1]
    # df$sst.std <- (ch20rsc$spring_ersst-mean(ch20rsc$spring_ersst))/sd(ch20rsc$spring_ersst)
    
  } else if(species == "pink"){
    df <- pk10r
    river_data <- pk10r %>% filter(River == river_name)
    river <- river_data$River_n2[1]
    # df$sst.std <- (pk10r$spring_ersst-mean(pk10r$spring_ersst))/sd(pk10r$spring_ersst)
  }
  
  if(species == "chum"){
    posterior_rv_b_for <- posterior %>% 
      select(starts_with('b_for_rv')) %>%
      select(ends_with(paste0("[",river,"]")))
    
    posterior_rv_alpha_j <- posterior %>% 
      select(starts_with('alpha_j')) %>%
      select(ends_with(paste0("[",river,"]")))
    
    posterior_rv_S_max <- posterior %>% 
      select(starts_with('Smax')) %>%
      select(ends_with(paste0("[",river,"]")))
    
    
  } else if(species == "pink"){
    
    river_wo_broodline <- river_data$River_n
    river_w_broodline <- river_data$River_n2
    
    posterior_rv_b_for <- posterior %>% 
      select(starts_with('b_for_rv')) %>%
      select(ends_with(paste0("[",river_wo_broodline,"]")))
    
    posterior_rv_alpha_j <- posterior %>% 
      select(starts_with('alpha_j')) %>%
      select(ends_with(paste0("[",river_w_broodline,"]")))
    
    posterior_rv_S_max <- posterior %>% 
      select(starts_with('Smax')) %>%
      select(ends_with(paste0("[",river_w_broodline,"]")))
    
    #time varying productivity for ricker
    
    # if(river_data$Broodline[1] == "Even"){
    #   
    #   posterior_a_t_alpha_j <- posterior_a_t %>% 
    #   select(starts_with('alpha_j')) %>%
    #   select(ends_with(paste0("[",river_w_broodline,"]")))
    #   
    #   posterior_a_t_alpha_t <- posterior_a_t %>%
    #   select(starts_with('alpha_t[1,'))
    #   
    #   posterior_a_t_alpha_t_j_full <- matrix(posterior_a_t_alpha_j[,1], ncol = length(posterior_a_t_alpha_t), nrow = length(posterior_a_t_alpha_j[,1])) + posterior_a_t_alpha_t
    #   
    #   posterior_a_t_alpha_t_j <- posterior_a_t_alpha_t_j_full %>%
    #     pivot_longer(cols = everything(), names_to = "year", values_to = "alpha_t_odd") %>%
    #     mutate(year = as.numeric(substring(year, 11, ifelse(nchar(year) == 12, 11, 12))) + 1953)
    #   
    # } else if(river_data$Broodline[1] == "Odd"){
    #   
    #   
    #   posterior_a_t_alpha_j <- posterior_a_t %>% 
    #   select(starts_with('alpha_j')) %>%
    #   select(ends_with(paste0("[",river_w_broodline,"]")))
    #   
    #   posterior_a_t_alpha_t <- posterior_a_t %>%
    #   select(starts_with('alpha_t[2,'))
    #   
    #   posterior_a_t_alpha_t_j_full <- matrix(posterior_a_t_alpha_j[,1], ncol = length(posterior_a_t_alpha_t), nrow = length(posterior_a_t_alpha_j[,1])) + posterior_a_t_alpha_t
    #   
    #   posterior_a_t_alpha_t_j <- posterior_a_t_alpha_t_j_full %>%
    #     pivot_longer(cols = everything(), names_to = "year", values_to = "alpha_t_odd") %>%
    #     mutate(year = as.numeric(substring(year, 11, ifelse(nchar(year) == 12, 11, 12))) + 1953)
    #   
    #   
    #   
    #   
    #   
    #   
    # }
    
    
    
    
    
    
  }
  
  
  spawners_predicted <- seq(0, max(river_data$Spawners), length.out = 100)
  
  # calculate recruit prediction
  
  low_cpd <- min(river_data$sqrt.CPD.std)
  high_cpd <- max(river_data$sqrt.CPD.std)
  # avg_cpd <- mean(river_data$sqrt.CPD.std)
  mid_cpd <- min(river_data$sqrt.CPD.std) + (max(river_data$sqrt.CPD.std) - min(river_data$sqrt.CPD.std))/2
  
  mid_cpd_real <- min(river_data$disturbedarea_prct_cs) + (max(river_data$disturbedarea_prct_cs) - min(river_data$disturbedarea_prct_cs))/2
  
  recruits_predicted <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - 
                                     as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*mid_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, median))*spawners_predicted
  
  recruits_predicted_lower <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted), 2, quantile, c(0.025)))*spawners_predicted
  
  recruits_predicted_upper <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted), 2, quantile, c(0.975)))*spawners_predicted
  
  
  
  # if(low_cpd == high_cpd){
  #   scale_color <- scale_color_manual(name = 'CPD in River (%)', values = c("black"))
  #   
  # } else{
  #    scale_color <- scale_color_gradient2(name = 'CPD in River (%)',
  #                                     low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 50)
  # }
  
  
  recruits_predicted_low_cpd <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*low_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, median))*spawners_predicted
  
  recruits_predicted_high_cpd <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*high_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, median))*spawners_predicted
  
  
  recruits_predicted_low_cpd_lower <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*low_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.025)))*spawners_predicted
  
  recruits_predicted_low_cpd_upper <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*low_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.975)))*spawners_predicted
  
  recruits_predicted_high_cpd_lower <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*high_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.025)))*spawners_predicted
  
  recruits_predicted_high_cpd_upper <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*high_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.975)))*spawners_predicted
  
  
  
  #make dataframe
  
  prediction_df <- data.frame(spawners = spawners_predicted,
                              recruits = recruits_predicted,
                              recruits_lower = recruits_predicted_lower,
                              recruits_upper = recruits_predicted_upper,
                              recruits_low_cpd = recruits_predicted_low_cpd,
                              recruits_high_cpd = recruits_predicted_high_cpd,
                              recruits_low_cpd_lower = recruits_predicted_low_cpd_lower,
                              recruits_low_cpd_upper = recruits_predicted_low_cpd_upper,
                              recruits_high_cpd_lower = recruits_predicted_high_cpd_lower,
                              recruits_high_cpd_upper = recruits_predicted_high_cpd_upper,
                              log_RS = log(recruits_predicted/spawners_predicted))
  
  
  
  
  
  
  
  #plot the time varying productivity vs year, with log(R/S) data
  
  
  
  
  
  #plot recruit vs spawner as points
  
  p1 <- ggplot() +
    geom_point(data = river_data,aes(x = Spawners, y = Recruits, color = disturbedarea_prct_cs), alpha = 0.5, size = 2) +
    geom_line(data = prediction_df, aes(x = spawners, y = recruits_low_cpd), color = '#35978f', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = recruits_high_cpd), color = '#bf812d', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = recruits), color = "black", size = 1, alpha = 0.5) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = recruits_low_cpd_lower, ymax = recruits_low_cpd_upper), fill ='#35978f', alpha = 0.5) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = recruits_high_cpd_lower, ymax = recruits_high_cpd_upper), fill ='#bf812d', alpha = 0.5) +
    #make y log scale
    # scale_y_log10() +
    labs(title = "", x = "Spawners", y = "Recruits") +
    # scale_color_manual(name = "CPD", values = c("Low" = '#35978f', "High" = '#bf812d')) +
    scale_color_gradient2(name = 'CDA in River (%)',
                          low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 20)+
    theme_classic() +
    theme(legend.position = "right",
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(1, "lines"),
          legend.text = element_text(size = 7),
          legend.spacing.y = unit(0.001, "cm"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5)
    )
  
  
  # p3 log R/s vs spawners
  
  p3 <- ggplot(river_data) + 
    geom_point(aes(x = Spawners, y = log(Recruits/Spawners), color = disturbedarea_prct_cs), alpha = 0.5, size = 2) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_low_cpd/spawners_predicted)), color = '#35978f', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_high_cpd/spawners_predicted)), color = '#bf812d', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = log_RS), color = "black", size = 1, alpha = 0.5) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = log(recruits_predicted_lower/spawners_predicted), ymax = log(recruits_predicted_upper/spawners_predicted)), fill = "gray", alpha = 0.5) +
    labs(#title = "Ricker model with CPD, NPGO, ERSST", 
      x = "Spawners", y = TeX(r"($\log \left(\frac{Recruits}{Spawners}\right)$)")) +
    scale_color_gradient2(name = 'CDA in River (%)',
                          low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 20)+
    theme_classic() +
    theme(legend.position = "right",
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(1, "lines"),
          legend.text = element_text(size = 7),
          legend.spacing.y = unit(0.001, "cm"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5)
    )
  
  
  
  
  
  
  return((p1+p3)+ plot_layout(guides = "collect"))
}

plot_recruit_spawner_river(data = case_study_watersheds_data,
                           species = "chum",
                           river_name = watersheds[1],
                           posterior = ric_chm_cpd_ocean_covariates_logR_long_chain,
                           posterior_a_t = NULL,
                           posterior_a_t_bh = NULL)


plot_both_forestry_effects_river_together <- function(posterior1 = ric_chm_cpd_ocean_covariates_logR,
                                                      posterior2 = ric_chm_eca_ocean_covariates_logR,
                                                      river_name,
                                                      river,
                                                      effect1 = "cpd",
                                                      effect2 = "eca",
                                                      species = "chum", 
                                                      model1 = "Cumulative\nDisturbance",
                                                      model2 = "ECA",
                                                      xlim = c(-0.5, 0.5)){
  
  
  
  
  posterior_df <- posterior1 %>%
    select(starts_with('b_for_rv')) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_for_rv',
                 values_to = "forestry") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) 
  
  posterior_df2 <- posterior2 %>% 
    select(starts_with('b_for_rv')) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_for_rv',
                 values_to = "forestry") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River)
  
  
  posterior_df_river1 <- posterior_df %>% filter(River_n == river) %>% 
    mutate(model = model1)
  
  posterior_df_river2 <- posterior_df2 %>% filter(River_n == river) %>% 
    mutate(model = model2)
  
  posterior_df_river <- posterior_df_river1 %>% 
    bind_rows(posterior_df_river2)
  
  #color by river
  plot1 <- ggplot() +
    stat_density(data= posterior_df_river, aes(forestry,#!!sym(forestry), 
                                               
                                               group = model, 
                                               color = model,
                                               fill = model),
                 geom = 'area', position = 'identity', 
                 alpha = 0.4, linewidth = 0.8) +
    
    geom_vline(xintercept = 0, color = 'slategray', linewidth = 0.8) +
    labs(x = "Standardized coefficients", y = "Posterior density", title = river_name) +
    xlim(xlim[1], xlim[2]) +
    scale_color_manual(name = "",
                       values = c("ECA" = '#7F6A93', 
                                  "Cumulative\nDisturbance" = '#A2C5AC')) +
    scale_fill_manual(name = "",
                      values = c("ECA" = '#7F6A93', 
                                 "Cumulative\nDisturbance" = '#A2C5AC')) +
    # scale_color +
    # geom_density(aes(posterior$b_for), color = 'black', linewidth = 1.2, alpha = 0.2)+
    #vline at the median value of the posterior
    theme_classic()+
    theme(legend.position = c(0.9,0.9),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0)
    )
  
  
  
  return(plot1)
}


plot_all_effects_river_together <- function(posterior1 = ric_chm_cpd_ocean_covariates_logR,
                                            river_name,
                                            river,
                                            effect1 = "cpd",
                                            effect2 = "sst",
                                            effect3 = "npgo",
                                            species = "chum", 
                                            xlim = c(-0.5, 0.5)){
  
  
  
  
  posterior_df <- posterior1 %>%
    select(starts_with('b_for_rv'),starts_with('b_sst_rv'),starts_with('b_npgo_rv')) %>%
    pivot_longer(cols = everything(),
                 names_to = c('Effect','River'),
                 names_pattern = 'b_(.*)_rv(.*)',
                 values_to = "coefficient") %>%
    
    
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) 
  
  
  if(effect1 == "cpd"){
    posterior_df_river <- posterior_df %>% filter(River_n == river) %>% 
      mutate(Effect = case_when(Effect == "for" ~ "CDA",
                                Effect == "sst" ~ "SST",
                                Effect == "npgo" ~ "NPGO"))
  } else if(effect1 == "eca"){
    posterior_df_river <- posterior_df %>% filter(River_n == river) %>% 
      mutate(Effect = case_when(Effect == "for" ~ "ECA",
                                Effect == "sst" ~ "SST",
                                Effect == "npgo" ~ "NPGO"))
  }
  
  
  
  
  #color by river
  plot1 <- ggplot() +
    stat_density(data= posterior_df_river, aes(coefficient,#!!sym(forestry), 
                                               
                                               group = Effect, 
                                               color = Effect,
                                               fill = Effect),
                 geom = 'area', position = 'identity', 
                 alpha = 0.4, linewidth = 0.8) +
    
    geom_vline(xintercept = 0, color = 'slategray', linewidth = 0.8) +
    geom_vline(aes(xintercept = median(posterior_df_river$coefficient[posterior_df_river$Effect == "CDA"]), 
                   color = "CDA"), linetype = "dashed", linewidth = 0.8) +
    geom_vline(aes(xintercept = median(posterior_df_river$coefficient[posterior_df_river$Effect == "SST"]), 
                   color = "SST"), linetype = "dashed", linewidth = 0.8) +
    geom_vline(aes(xintercept = median(posterior_df_river$coefficient[posterior_df_river$Effect == "NPGO"]),
                   color = "NPGO"), linetype = "dashed", linewidth = 0.8) +
    labs(x = "Standardized coefficients", y = "Posterior density", title = river_name) +
    xlim(xlim[1], xlim[2]) +
    scale_color_manual(name = "",
                       values = c("ECA" = "#ADcCA5", 
                                  "CDA" = "#ADcCA5",
                                  "SST" = "#C78c63",
                                  "NPGO" = "#829Dc6")) +
    scale_fill_manual(name = "",
                      values = c("ECA" = "#ADcCA5", 
                                 "CDA" = "#ADcCA5",
                                 "SST" = "#C78c63",
                                 "NPGO" = "#829Dc6")) +
    # scale_color +
    # geom_density(aes(posterior$b_for), color = 'black', linewidth = 1.2, alpha = 0.2)+
    #vline at the median value of the posterior
    theme_classic()+
    theme(legend.position = c(0.8,0.9),
          legend.background = element_rect(fill = alpha('white', 0.5)),
          legend.text = element_text(size = 7),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_blank()
    )
  
  
  
  return(plot1)
}


plot_productivity_change_river_together <- function(posterior1 = ric_chm_cpd_ocean_covariates_logR_long_chain,
                                                    posterior2 = ric_chm_eca_ocean_covariates_logR_long_chain,
                                                    river_name = "CARNATION CREEK",  
                                                    effect1 = "cpd",
                                                    effect2 = "eca",
                                                    species = "chum", 
                                                    model1 = "CPD",
                                                    model2 = "ECA"){
  
  
  if(species == "chum"){
    df <- ch20rsc 
    river_data <- ch20rsc %>% filter(River == river_name)
    river <- river_data$River_n[1]
    # df$sst.std <- (ch20rsc$spring_ersst-mean(ch20rsc$spring_ersst))/sd(ch20rsc$spring_ersst)
    
  } else if(species == "pink"){
    df <- pk10r
    river_data <- pk10r %>% filter(River == river_name)
    river <- river_data$River_n2[1]
    # df$sst.std <- (pk10r$spring_ersst-mean(pk10r$spring_ersst))/sd(pk10r$spring_ersst)
  }
  
  posterior_df <- posterior1 %>%
    select(starts_with(paste0('b_for_rv[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_for_rv',
                 values_to = "forestry") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  posterior_df2 <- posterior2 %>% 
    select(starts_with(paste0('b_for_rv[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_for_rv',
                 values_to = "forestry") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  
  
  
  eca <- seq(0,1,length.out=100)
  
  eca_sqrt <- sqrt(eca)
  
  eca_sqrt_std <- (eca_sqrt-mean(eca_sqrt))/sd(eca_sqrt)
  
  cpd <- seq(0,100,length.out=100)
  
  cpd_sqrt <- sqrt(cpd)
  
  cpd_sqrt_std <- (cpd_sqrt-mean(cpd_sqrt))/sd(cpd_sqrt)
  
  
  
  
  no_eca <- min(eca_sqrt_std)
  
  no_cpd <- min(cpd_sqrt_std)
  
  
  
  # need to change b_rv to posterior 1 and posterior 2 and then make figure
  
  productivity_cpd <- (exp(as.matrix(posterior_df[,1])%*%
                             (cpd_sqrt_std-no_cpd)))*100 - 100
  
  productivity_eca <- (exp(as.matrix(posterior_df2[,1])%*%
                             (eca_sqrt_std-no_eca)))*100 - 100
  
  
  productivity_cpd_df <- data.frame(productivity_cpd_median = apply(productivity_cpd,2,median),
                                    productivity_cpd_025 = apply(productivity_cpd,2,quantile,c(0.025), 
                                                                 row.names = c("q025")),
                                    productivity_cpd_975 = apply(productivity_cpd,2,quantile,c(0.975),
                                                                 row.names = c("q975")),
                                    productivity_cpd_100 = apply(productivity_cpd,2,quantile,c(0.1),
                                                                 row.names = c("q100")),
                                    productivity_cpd_900 = apply(productivity_cpd,2,quantile,c(0.9),
                                                                 row.names = c("q900")),
                                    
                                    productivity_cpd_025_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.95)[1,],
                                    productivity_cpd_975_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.95)[2,],
                                    productivity_cpd_100_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.8)[1,],
                                    productivity_cpd_900_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.8)[2,],
                                    productivity_cpd_50_lower_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.5)[1,],
                                    productivity_cpd_50_upper_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.5)[2,],
                                    
                                    
                                    
                                    cpd_sqrt_std = cpd_sqrt_std,
                                    cpd = cpd,
                                    max_cpd = max(river_data$disturbedarea_prct_cs),
                                    max_sqrt_cpd = max(river_data$sqrt.CPD),
                                    model = "CPD"
  )
  
  
  productivity_eca_df <- data.frame(productivity_eca_median = apply(productivity_eca,2,median),
                                    productivity_eca_025 = apply(productivity_eca,2,quantile,c(0.025), 
                                                                 row.names = c("q025")),
                                    productivity_eca_975 = apply(productivity_eca,2,quantile,c(0.975),
                                                                 row.names = c("q975")),
                                    productivity_eca_100 = apply(productivity_eca,2,quantile,c(0.1),
                                                                 row.names = c("q100")),
                                    productivity_eca_900 = apply(productivity_eca,2,quantile,c(0.9),
                                                                 row.names = c("q900")),
                                    
                                    productivity_eca_025_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.95)[1,],
                                    productivity_eca_975_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.95)[2,],
                                    productivity_eca_100_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.8)[1,],
                                    productivity_eca_900_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.8)[2,],
                                    productivity_eca_50_lower_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.5)[1,],
                                    productivity_eca_50_upper_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.5)[2,],
                                    
                                    eca_sqrt_std = eca_sqrt_std,
                                    eca = eca,
                                    max_eca = max(river_data$ECA_age_proxy_forested_only),
                                    max_sqrt_eca = max(river_data$sqrt.ECA),
                                    model = "ECA"
  )
  
  
  
  
  plot1 <- ggplot(productivity_cpd_df) +
    geom_line(aes(x = cpd, y = productivity_cpd_median, group = 1,
                  color = "median"),alpha=0.9, linewidth = 0.8) +
    geom_ribbon(aes(x = cpd, ymin = productivity_cpd_025_hd, ymax = productivity_cpd_975_hd, fill = "95% credible interval"),
                alpha = 0.2) +
    geom_ribbon(aes(x = cpd, ymin = productivity_cpd_100_hd, ymax = productivity_cpd_900_hd, fill = "80% credible interval"),
                alpha = 0.4) +
    
    geom_segment(aes(x = unique(productivity_cpd_df$max_cpd), 
                     xend = unique(productivity_cpd_df$max_cpd), 
                     y = -100, 
                     yend = productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],
                     color = "gray"), 
                 linetype = "dashed", linewidth = 0.8) +
    # add horizontal line 
    geom_segment(aes(x = 0, 
                     xend = unique(productivity_cpd_df$max_cpd), 
                     y = productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))], 
                     yend = productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],
                     color = "gray"), 
                 linetype = "dashed", linewidth = 0.8) +
    annotate("text",x = unique(productivity_cpd_df$max_cpd), 
             y = productivity_cpd_df$productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],
             label = paste0("Max CPD: ",round(unique(productivity_cpd_df$max_cpd),1),"%\n",
                            "Median change: ",round(productivity_cpd_df$productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],1),"%"),
             vjust = -0.5, color = "black", size = 2, hjust = 0
    ) +
    
    scale_color_manual(name = "Model",
                       values = c("ECA model" = '#A2C5AC', 
                                  "median" = '#A2C5AC')) +
    scale_fill_manual(name = "Model",
                      values = c("ECA 95% credible interval" = '#A2C5AC', 
                                 "ECA 80% credible interval" = '#A2C5AC',
                                 "95% credible interval" = '#A2C5AC', 
                                 "80% credible interval" = '#A2C5AC')) +
    ylim(-100,100) +
    scale_x_continuous(n.breaks = 5) +
    labs(x = "Cumulative Disturbance (%)",
         y = "Change in recruitment (%)") +
    theme_classic() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.spacing.y = unit(0.1, "cm"),
          legend.key.height = unit(0.5, "cm"),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 10, hjust = 0.5))+
    guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.5)))
  
  plot2 <- ggplot(productivity_eca_df)+
    geom_line(aes(x = eca, y = productivity_eca_median, group = 1,
                  color = "median"),alpha=0.9, linewidth = 0.8) +
    geom_ribbon(aes(x = eca, ymin = productivity_eca_025_hd, ymax = productivity_eca_975_hd, fill = "95% credible interval"),
                alpha = 0.2) +
    geom_ribbon(aes(x = eca, ymin = productivity_eca_100_hd, ymax = productivity_eca_900_hd, fill = "80% credible interval"),
                alpha = 0.4) +
    #add vertical line at max eca - max height should be value of productivity eca median
    geom_segment(aes(x = unique(productivity_eca_df$max_eca), 
                     xend = unique(productivity_eca_df$max_eca), 
                     y = -100, 
                     yend = productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],
                     color = "gray"), 
                 linetype = "dashed", linewidth = 0.8) +
    # add horizontal line 
    geom_segment(aes(x = 0, 
                     xend = unique(productivity_eca_df$max_eca), 
                     y = productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))], 
                     yend = productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],
                     color = "gray"), 
                 linetype = "dashed", linewidth = 0.8) +
    annotate("text",x = unique(productivity_eca_df$max_eca), 
             y = productivity_eca_df$productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],
             label = paste0("Max ECA: ",round(unique(productivity_eca_df$max_eca),2),"\n",
                            "Median change: ",round(productivity_eca_df$productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],1),"%"),
             vjust = -0.5, color = "black", size = 2, hjust = 0
    ) +
    
    
    scale_color_manual(values = c("median" = '#A2C5AC', 
                                  "CPD model" = '#A2C5AC')) +
    scale_fill_manual(values = c("95% credible interval" = '#A2C5AC', 
                                 "80% credible interval" = '#A2C5AC',
                                 "CPD model 95% credible interval" = '#A2C5AC', 
                                 "CPD model 80% credible interval" = '#A2C5AC')) +
    ylim(-100,100) +
    scale_x_continuous(n.breaks = 5) +
    labs(x = "ECA",
         y = "Change in recruitment (%)") +
    theme_classic() +
    theme(legend.position = "inside",
          legend.justification = c("right", "top"),
          legend.byrow = FALSE,
          legend.title = element_blank(),
          # legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.spacing.y = unit(0, "cm"),
          legend.key.height = unit(0.2, "cm"),
          legend.text = element_text(size = 6),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 10, hjust = 0.5))+
    guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.5)))
  
  return(plot1+plot2 + plot_layout(axes = 'collect') +
           plot_annotation(title = str_to_title(river_name),tag_level = 'A')&
           theme(plot.tag.position = c(0.05, 1),
                 plot.tag = element_text(size = 10, hjust = 0, vjust = 0, face = "bold")))
  
}


plot_recruit_spawner_river(data = case_study_watersheds_data,
                           species = "chum",
                           river_name = watersheds[1],
                           posterior = ric_chm_cpd_ocean_covariates_logR_long_chain,
                           posterior_a_t = NULL,
                           posterior_a_t_bh = NULL)



plot_all_effects_river_together(
  posterior1 = ric_chm_cpd_ocean_covariates_logR_long_chain,
  river_name = str_to_title(watersheds[1]),
  river = unique(case_study_watersheds_data$River_n[case_study_watersheds_data$River == watersheds[1]])
)

plot_productivity_change_river_together(
  posterior1 = ric_chm_cpd_ocean_covariates_logR_long_chain,
  posterior2 = ric_chm_eca_ocean_covariates_logR_long_chain,
  river_name = "CARNATION CREEK",
  effect1 = "cpd",
  effect2 = "eca",
  species = "chum",
  model1 = "CPD",
  model2 = "ECA"
)

plot_recruitment_change_river_together <- function(posterior1 = ric_chm_cpd_ocean_covariates_logR_long_chain,
                                                   posterior2 = ric_chm_eca_ocean_covariates_logR_long_chain,
                                                   river_name = "CARNATION CREEK",  
                                                   effect1 = "cpd",
                                                   effect2 = "eca",
                                                   species = "chum", 
                                                   model1 = "CPD",
                                                   model2 = "ECA",
                                                   hd = FALSE){
  
  
  if(species == "chum"){
    df <- ch20rsc 
    river_data <- ch20rsc %>% filter(River == river_name)
    river <- river_data$River_n[1]
    # df$sst.std <- (ch20rsc$spring_ersst-mean(ch20rsc$spring_ersst))/sd(ch20rsc$spring_ersst)
    
  } else if(species == "pink"){
    df <- pk10r
    river_data <- pk10r %>% filter(River == river_name)
    river <- river_data$River_n[1]
    # df$sst.std <- (pk10r$spring_ersst-mean(pk10r$spring_ersst))/sd(pk10r$spring_ersst)
  }
  
  posterior_df <- posterior1 %>%
    select(starts_with(paste0('b_for_rv[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_for_rv',
                 values_to = "forestry") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  posterior_df2 <- posterior2 %>% 
    select(starts_with(paste0('b_for_rv[',as.character(river),']'))) %>%
    pivot_longer(cols = everything(), 
                 names_to = 'River', 
                 names_prefix = 'b_for_rv',
                 values_to = "forestry") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    mutate(River = river)
  
  
  
  
  eca <- seq(0,1,length.out=100)
  
  eca_sqrt <- sqrt(eca)
  
  eca_sqrt_std <- (eca_sqrt-mean(eca_sqrt))/sd(eca_sqrt)
  
  cpd <- seq(0,100,length.out=100)
  
  cpd_sqrt <- sqrt(cpd)
  
  cpd_sqrt_std <- (cpd_sqrt-mean(cpd_sqrt))/sd(cpd_sqrt)
  
  
  
  
  no_eca <- min(eca_sqrt_std)
  
  no_cpd <- min(cpd_sqrt_std)
  
  
  
  # need to change b_rv to posterior 1 and posterior 2 and then make figure
  
  productivity_cpd <- (exp(as.matrix(posterior_df[,1])%*%
                             (cpd_sqrt_std-no_cpd)))*100 - 100
  
  productivity_eca <- (exp(as.matrix(posterior_df2[,1])%*%
                             (eca_sqrt_std-no_eca)))*100 - 100
  
  
  productivity_cpd_df <- data.frame(productivity_cpd_median = apply(productivity_cpd,2,median),
                                    productivity_cpd_025 = apply(productivity_cpd,2,quantile,c(0.025), 
                                                                 row.names = c("q025")),
                                    productivity_cpd_975 = apply(productivity_cpd,2,quantile,c(0.975),
                                                                 row.names = c("q975")),
                                    productivity_cpd_100 = apply(productivity_cpd,2,quantile,c(0.1),
                                                                 row.names = c("q100")),
                                    productivity_cpd_900 = apply(productivity_cpd,2,quantile,c(0.9),
                                                                 row.names = c("q900")),
                                    productivity_cpd_250 = apply(productivity_cpd,2,quantile,c(0.25),
                                                                 row.names = c("q250")),
                                    productivity_cpd_750 = apply(productivity_cpd,2,quantile,c(0.75),
                                                                 row.names = c("q750")),
                                    
                                    
                                    productivity_cpd_025_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.95)[1,],
                                    productivity_cpd_975_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.95)[2,],
                                    productivity_cpd_100_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.8)[1,],
                                    productivity_cpd_900_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.8)[2,],
                                    productivity_cpd_50_lower_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.5)[1,],
                                    productivity_cpd_50_upper_hd = apply(productivity_cpd,2,HDInterval::hdi, credMass = 0.5)[2,],
                                    
                                    
                                    
                                    cpd_sqrt_std = cpd_sqrt_std,
                                    cpd = cpd,
                                    max_cpd = max(river_data$disturbedarea_prct_cs),
                                    max_sqrt_cpd = max(river_data$sqrt.CPD),
                                    model = "CPD"
  )
  
  
  productivity_eca_df <- data.frame(productivity_eca_median = apply(productivity_eca,2,median),
                                    productivity_eca_025 = apply(productivity_eca,2,quantile,c(0.025), 
                                                                 row.names = c("q025")),
                                    productivity_eca_975 = apply(productivity_eca,2,quantile,c(0.975),
                                                                 row.names = c("q975")),
                                    productivity_eca_100 = apply(productivity_eca,2,quantile,c(0.1),
                                                                 row.names = c("q100")),
                                    productivity_eca_900 = apply(productivity_eca,2,quantile,c(0.9),
                                                                 row.names = c("q900")),
                                    productivity_eca_250 = apply(productivity_eca,2,quantile,c(0.25),
                                                                 row.names = c("q250")),
                                    productivity_eca_750 = apply(productivity_eca,2,quantile,c(0.75),
                                                                 row.names = c("q750")),
                                    
                                    productivity_eca_025_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.95)[1,],
                                    productivity_eca_975_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.95)[2,],
                                    productivity_eca_100_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.8)[1,],
                                    productivity_eca_900_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.8)[2,],
                                    productivity_eca_50_lower_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.5)[1,],
                                    productivity_eca_50_upper_hd = apply(productivity_eca,2,HDInterval::hdi, credMass = 0.5)[2,],
                                    
                                    eca_sqrt_std = eca_sqrt_std,
                                    eca = eca,
                                    max_eca = max(river_data$ECA_age_proxy_forested_only),
                                    max_sqrt_eca = max(river_data$sqrt.ECA),
                                    model = "ECA"
  )
  
  
  if(hd == TRUE){
    
    plot1 <- ggplot(productivity_cpd_df) +
      geom_line(aes(x = cpd, y = productivity_cpd_median, group = 1,
                    color = "median"),alpha=0.9, linewidth = 0.8) +
      geom_ribbon(aes(x = cpd, ymin = productivity_cpd_025_hd, ymax = productivity_cpd_975_hd, fill = "95% credible interval"),
                  alpha = 0.2) +
      geom_ribbon(aes(x = cpd, ymin = productivity_cpd_100_hd, ymax = productivity_cpd_900_hd, fill = "80% credible interval"),
                  alpha = 0.4) +
      
      geom_segment(aes(x = unique(productivity_cpd_df$max_cpd), 
                       xend = unique(productivity_cpd_df$max_cpd), 
                       y = -100, 
                       yend = productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],
                       color = "gray"), 
                   linetype = "dashed", linewidth = 0.8) +
      # add horizontal line 
      geom_segment(aes(x = 0, 
                       xend = unique(productivity_cpd_df$max_cpd), 
                       y = productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))], 
                       yend = productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],
                       color = "gray"), 
                   linetype = "dashed", linewidth = 0.8) +
      annotate("text",x = unique(productivity_cpd_df$max_cpd), 
               y = productivity_cpd_df$productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],
               label = paste0("Max CPD: ",round(unique(productivity_cpd_df$max_cpd),1),"%\n",
                              "Median change: ",round(productivity_cpd_df$productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],1),"%"),
               vjust = -0.5, color = "black", size = 2, hjust = 0
      ) +
      
      scale_color_manual(name = "Model",
                         values = c("ECA model" = '#A2C5AC', 
                                    "median" = '#A2C5AC')) +
      scale_fill_manual(name = "Model",
                        values = c("ECA 95% credible interval" = '#A2C5AC', 
                                   "ECA 80% credible interval" = '#A2C5AC',
                                   "95% credible interval" = '#A2C5AC', 
                                   "80% credible interval" = '#A2C5AC')) +
      ylim(-100,100) +
      scale_x_continuous(n.breaks = 5) +
      labs(x = "CDA (%)",
           y = "Change in productivity (%)") +
      theme_classic() +
      theme(legend.position = "none",
            legend.title = element_blank(),
            legend.key.size = unit(0.5, "cm"),
            legend.key.width = unit(0.5, "cm"),
            legend.spacing.y = unit(0.1, "cm"),
            legend.key.height = unit(0.5, "cm"),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10, hjust = 0.5))+
      guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.5)))
    
    plot2 <- ggplot(productivity_eca_df)+
      geom_line(aes(x = eca, y = productivity_eca_median, group = 1,
                    color = "median"),alpha=0.9, linewidth = 0.8) +
      geom_ribbon(aes(x = eca, ymin = productivity_eca_025_hd, ymax = productivity_eca_975_hd, fill = "95% credible interval"),
                  alpha = 0.2) +
      geom_ribbon(aes(x = eca, ymin = productivity_eca_100_hd, ymax = productivity_eca_900_hd, fill = "80% credible interval"),
                  alpha = 0.4) +
      #add vertical line at max eca - max height should be value of productivity eca median
      geom_segment(aes(x = unique(productivity_eca_df$max_eca), 
                       xend = unique(productivity_eca_df$max_eca), 
                       y = -100, 
                       yend = productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],
                       color = "gray"), 
                   linetype = "dashed", linewidth = 0.8) +
      # add horizontal line 
      geom_segment(aes(x = 0, 
                       xend = unique(productivity_eca_df$max_eca), 
                       y = productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))], 
                       yend = productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],
                       color = "gray"), 
                   linetype = "dashed", linewidth = 0.8) +
      annotate("text",x = unique(productivity_eca_df$max_eca), 
               y = productivity_eca_df$productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],
               label = paste0("Max ECA: ",round(unique(productivity_eca_df$max_eca),2),"\n",
                              "Median change: ",round(productivity_eca_df$productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],1),"%"),
               vjust = -0.5, color = "black", size = 2, hjust = 0
      ) +
      
      
      scale_color_manual(values = c("median" = '#A2C5AC', 
                                    "CDA model" = '#A2C5AC')) +
      scale_fill_manual(values = c("95% credible interval" = '#A2C5AC', 
                                   "80% credible interval" = '#A2C5AC',
                                   "CPD model 95% credible interval" = '#A2C5AC', 
                                   "CPD model 80% credible interval" = '#A2C5AC')) +
      ylim(-100,100) +
      scale_x_continuous(n.breaks = 5) +
      labs(x = "ECA",
           y = "Change in productivity (%)") +
      theme_classic() +
      theme(legend.position = "inside",
            legend.justification = c("right", "top"),
            legend.byrow = FALSE,
            legend.title = element_blank(),
            # legend.key.size = unit(0.5, "cm"),
            legend.key.width = unit(0.5, "cm"),
            legend.spacing.y = unit(0, "cm"),
            legend.key.height = unit(0.2, "cm"),
            legend.text = element_text(size = 6),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10, hjust = 0.5))+
      guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.5)))
    
  } else{
    plot1 <- ggplot(productivity_cpd_df) +
      geom_line(aes(x = cpd, y = productivity_cpd_median, group = 1,
                    color = "median"),alpha=0.9, linewidth = 0.8) +
      geom_ribbon(aes(x = cpd, ymin = productivity_cpd_025, ymax = productivity_cpd_975, fill = "95% credible interval"),
                  alpha = 0.2) +
      geom_ribbon(aes(x = cpd, ymin = productivity_cpd_100, ymax = productivity_cpd_900, fill = "80% credible interval"),
                  alpha = 0.4) +
      geom_ribbon(aes(x = cpd, ymin = productivity_cpd_250, ymax = productivity_cpd_750, fill = "50% credible interval"),
                  alpha = 0.6) +
      
      geom_segment(aes(x = unique(productivity_cpd_df$max_cpd), 
                       xend = unique(productivity_cpd_df$max_cpd), 
                       y = -100, 
                       yend = productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],
                       color = "gray"), 
                   linetype = "dashed", linewidth = 0.8) +
      # add horizontal line 
      geom_segment(aes(x = 0, 
                       xend = unique(productivity_cpd_df$max_cpd), 
                       y = productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))], 
                       yend = productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],
                       color = "gray"), 
                   linetype = "dashed", linewidth = 0.8) +
      annotate("text",x = unique(productivity_cpd_df$max_cpd), 
               y = productivity_cpd_df$productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],
               label = paste0("Max CDA: ",round(unique(productivity_cpd_df$max_cpd),1),"%\n",
                              "Median change: ",round(productivity_cpd_df$productivity_cpd_median[which.min(abs(cpd - unique(productivity_cpd_df$max_cpd)))],1),"%"),
               vjust = -0.5, color = "black", size = 2, hjust = 0
      ) +
      
      scale_color_manual(name = "Model",
                         values = c("ECA model" = '#A2C5AC', 
                                    "median" = '#A2C5AC')) +
      scale_fill_manual(name = "Model",
                        values = c("ECA 95% credible interval" = '#A2C5AC', 
                                   "ECA 80% credible interval" = '#A2C5AC',
                                   "95% credible interval" = '#A2C5AC', 
                                   "80% credible interval" = '#A2C5AC',
                                   "50% credible interval" = '#A2C5AC')) +
      ylim(-100,100) +
      scale_x_continuous(n.breaks = 5) +
      labs(x = "CDA (%)",
           y = "Change in productivity (%)") +
      theme_classic() +
      theme(legend.position = "none",
            legend.title = element_blank(),
            legend.key.size = unit(0.5, "cm"),
            legend.key.width = unit(0.5, "cm"),
            legend.spacing.y = unit(0.1, "cm"),
            legend.key.height = unit(0.5, "cm"),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10, hjust = 0.5))+
      guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.5)))
    
    plot2 <- ggplot(productivity_eca_df)+
      geom_line(aes(x = eca, y = productivity_eca_median, group = 1,
                    color = "median"),alpha=0.9, linewidth = 0.8) +
      geom_ribbon(aes(x = eca, ymin = productivity_eca_025, ymax = productivity_eca_975, fill = "95% credible interval"),
                  alpha = 0.2) +
      geom_ribbon(aes(x = eca, ymin = productivity_eca_100, ymax = productivity_eca_900, fill = "80% credible interval"),
                  alpha = 0.4) +
      geom_ribbon(aes(x = eca, ymin = productivity_eca_250, ymax = productivity_eca_750, fill = "50% credible interval"),
                  alpha = 0.6) +
      #add vertical line at max eca - max height should be value of productivity eca median
      geom_segment(aes(x = unique(productivity_eca_df$max_eca), 
                       xend = unique(productivity_eca_df$max_eca), 
                       y = -100, 
                       yend = productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],
                       color = "gray"), 
                   linetype = "dashed", linewidth = 0.8) +
      # add horizontal line 
      geom_segment(aes(x = 0, 
                       xend = unique(productivity_eca_df$max_eca), 
                       y = productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))], 
                       yend = productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],
                       color = "gray"), 
                   linetype = "dashed", linewidth = 0.8) +
      annotate("text",x = unique(productivity_eca_df$max_eca), 
               y = productivity_eca_df$productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],
               label = paste0("Max ECA: ",round(unique(productivity_eca_df$max_eca),2),"\n",
                              "Median change: ",round(productivity_eca_df$productivity_eca_median[which.min(abs(eca - unique(productivity_eca_df$max_eca)))],1),"%"),
               vjust = -0.5, color = "black", size = 2, hjust = 0
      ) +
      
      
      scale_color_manual(values = c("median" = '#A2C5AC', 
                                    "CPD model" = '#A2C5AC')) +
      scale_fill_manual(values = c("95% credible interval" = '#A2C5AC', 
                                   "80% credible interval" = '#A2C5AC',
                                   "50% credible interval" = '#A2C5AC',
                                   "CPD model 95% credible interval" = '#A2C5AC', 
                                   "CPD model 80% credible interval" = '#A2C5AC')) +
      ylim(-100,100) +
      scale_x_continuous(n.breaks = 5) +
      labs(x = "ECA",
           y = "Change in productivity (%)") +
      theme_classic() +
      theme(legend.position = "inside",
            legend.justification = c("right", "top"),
            legend.byrow = FALSE,
            legend.title = element_blank(),
            # legend.key.size = unit(0.5, "cm"),
            legend.key.width = unit(0.5, "cm"),
            legend.spacing.y = unit(0, "cm"),
            legend.key.height = unit(0.2, "cm"),
            legend.text = element_text(size = 6),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10, hjust = 0.5))+
      guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.5)))
  }
  
  
  
  return(plot1+plot2 + plot_layout(axes = 'collect') +
           plot_annotation(title = str_to_title(river_name),tag_level = 'A')&
           theme(plot.tag.position = c(0.05, 1),
                 plot.tag = element_text(size = 10, hjust = 0, vjust = 0, face = "bold")))
  
}


plot_recruitment_change_river_together(
  posterior1 = ric_chm_cpd_ocean_covariates_logR_long_chain,
  posterior2 = ric_chm_eca_ocean_covariates_logR_long_chain,
  river_name = "CARNATION CREEK",
  effect1 = "cpd",
  effect2 = "eca",
  species = "chum",
  model1 = "CPD",
  model2 = "ECA",
  hd = FALSE
)


plot_recruit_spawner_river(data = case_study_watersheds_data,
                           species = "chum",
                           river_name = watersheds[6],
                           posterior = ric_chm_cpd_ocean_covariates_logR_long_chain,
                           posterior_a_t = NULL,
                           posterior_a_t_bh = NULL)

plot_recruit_spawner_river_new <- function(species = "chum", river_name,  posterior){
  
  # river_data <- df %>% filter(River_n == river)
  
  if(species == "chum"){
    df <- ch20rsc 
    river_data <- ch20rsc %>% filter(River == river_name)
    river <- river_data$River_n[1]
    # df$sst.std <- (ch20rsc$spring_ersst-mean(ch20rsc$spring_ersst))/sd(ch20rsc$spring_ersst)
    
  } else if(species == "pink"){
    df <- pk10r
    river_data <- pk10r %>% filter(River == river_name)
    river <- river_data$River_n2[1]
    # df$sst.std <- (pk10r$spring_ersst-mean(pk10r$spring_ersst))/sd(pk10r$spring_ersst)
  }
  
  if(species == "chum"){
    posterior_rv_b_for <- posterior %>% 
      select(starts_with('b_for_rv')) %>%
      select(ends_with(paste0("[",river,"]")))
    
    posterior_rv_alpha_j <- posterior %>% 
      select(starts_with('alpha_j')) %>%
      select(ends_with(paste0("[",river,"]")))
    
    posterior_rv_S_max <- posterior %>% 
      select(starts_with('Smax')) %>%
      select(ends_with(paste0("[",river,"]")))
    
    
  } else if(species == "pink"){
    
    river_wo_broodline <- river_data$River_n
    river_w_broodline <- river_data$River_n2
    
    posterior_rv_b_for <- posterior %>% 
      select(starts_with('b_for_rv')) %>%
      select(ends_with(paste0("[",river_wo_broodline,"]")))
    
    posterior_rv_alpha_j <- posterior %>% 
      select(starts_with('alpha_j')) %>%
      select(ends_with(paste0("[",river_w_broodline,"]")))
    
    posterior_rv_S_max <- posterior %>% 
      select(starts_with('Smax')) %>%
      select(ends_with(paste0("[",river_w_broodline,"]")))
    
    
    
  }
  
  
  spawners_predicted <- seq(0, max(river_data$Spawners), length.out = 100)
  
  # calculate recruit prediction
  
  low_cpd <- min(river_data$sqrt.CPD.std)
  high_cpd <- max(river_data$sqrt.CPD.std)
  # avg_cpd <- mean(river_data$sqrt.CPD.std)
  mid_cpd <- min(river_data$sqrt.CPD.std) + (max(river_data$sqrt.CPD.std) - min(river_data$sqrt.CPD.std))/2
  
  mid_cpd_real <- min(river_data$disturbedarea_prct_cs) + (max(river_data$disturbedarea_prct_cs) - min(river_data$disturbedarea_prct_cs))/2
  
  
  recruits_predicted_low_cpd <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*low_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, median))*spawners_predicted
  
  recruits_predicted_high_cpd <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*high_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, median))*spawners_predicted
  
  
  recruits_predicted_low_cpd_lower <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*low_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.025)))*spawners_predicted
  
  recruits_predicted_low_cpd_upper <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*low_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.975)))*spawners_predicted
  
  recruits_predicted_high_cpd_lower <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*high_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.025)))*spawners_predicted
  
  recruits_predicted_high_cpd_upper <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*high_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.975)))*spawners_predicted
  
  
  
  #make dataframe
  
  prediction_df <- data.frame(spawners = spawners_predicted,
                              
                              recruits_low_cpd = recruits_predicted_low_cpd,
                              recruits_high_cpd = recruits_predicted_high_cpd,
                              recruits_low_cpd_lower = recruits_predicted_low_cpd_lower,
                              recruits_low_cpd_upper = recruits_predicted_low_cpd_upper,
                              recruits_high_cpd_lower = recruits_predicted_high_cpd_lower,
                              recruits_high_cpd_upper = recruits_predicted_high_cpd_upper)
  
  
  
  
  
  
  
  #plot the time varying productivity vs year, with log(R/S) data
  
  
  
  
  
  #plot recruit vs spawner as points
  
  p1 <- ggplot() +
    geom_point(data = river_data,aes(x = Spawners, y = Recruits, color = disturbedarea_prct_cs), alpha = 0.5, size = 2) +
    geom_line(data = prediction_df, aes(x = spawners, y = recruits_low_cpd), color = '#35978f', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = recruits_high_cpd), color = '#bf812d', size = 1, alpha = 0.5) +
    # geom_line(data = prediction_df, aes(x = spawners, y = recruits), color = "black", size = 1, alpha = 0.5) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = recruits_low_cpd_lower, ymax = recruits_low_cpd_upper), fill ='#35978f', alpha = 0.1) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = recruits_high_cpd_lower, ymax = recruits_high_cpd_upper), fill ='#bf812d', alpha = 0.1) +
    #make y log scale
    # scale_y_log10() +
    labs(title = "", x = "Spawners", y = "Recruits") +
    # scale_color_manual(name = "CPD", values = c("Low" = '#35978f', "High" = '#bf812d')) +
    scale_color_gradient2(name = 'CDA (%)', guide = guide_colorbar(barwidth = 3, barheight = 0.5),
                          low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 20, n.breaks = 4) +
    theme_classic() +
    theme(legend.position = c(0.8,0.9),
          legend.background = element_rect(fill = alpha('white', 0.5)),
          legend.direction = "horizontal",
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8, vjust = 1, hjust = 1),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_blank()
          # legend.key.width = unit(0.5, "cm"),
          # legend.key.height = unit(1, "lines"),
          # legend.spacing.y = unit(0.001, "cm")
    )
  
  
  # p3 log R/s vs spawners
  
  p2 <- ggplot(river_data) + 
    geom_point(aes(x = Spawners, y = log(Recruits/Spawners), color = disturbedarea_prct_cs), alpha = 0.5, size = 2) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_low_cpd/spawners_predicted)), color = '#35978f', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_high_cpd/spawners_predicted)), color = '#bf812d', size = 1, alpha = 0.5) +
    # geom_line(data = prediction_df, aes(x = spawners, y = log_RS), color = "black", size = 1, alpha = 0.5) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = log(recruits_predicted_low_cpd_lower/spawners_predicted), ymax = log(recruits_predicted_low_cpd_upper/spawners_predicted)), fill = "#35978f", alpha = 0.1) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = log(recruits_predicted_high_cpd_lower/spawners_predicted), ymax = log(recruits_predicted_high_cpd_upper/spawners_predicted)), fill = "#bf812d", alpha = 0.1) +
    labs(#title = "Ricker model with CPD, NPGO, ERSST", 
      x = "Spawners", y = TeX(r"($\log \left(\frac{Recruits}{Spawners}\right)$)")) +
    scale_color_gradient2(name = 'CDA (%)',
                          low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 20, guide = guide_colorbar(barwidth = 3, barheight = 0.5)) +
    theme_classic() +
    theme(legend.position = c(0.8,0.9),
          legend.background = element_rect(fill = alpha('white', 0.5)),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_blank(),
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(1, "lines"),
          legend.spacing.y = unit(0.001, "cm")
    )
  
  p3 <- ggplot(river_data) + 
    geom_point(aes(x = Spawners, y = log(Recruits), color = disturbedarea_prct_cs), alpha = 0.5, size = 2) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_low_cpd)), color = '#35978f', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_high_cpd)), color = '#bf812d', size = 1, alpha = 0.5) +
    # geom_line(data = prediction_df, aes(x = spawners, y = log_RS), color = "black", size = 1, alpha = 0.5) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = log(recruits_predicted_low_cpd_lower), ymax = log(recruits_predicted_low_cpd_upper)), fill = "#35978f", alpha = 0.1) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = log(recruits_predicted_high_cpd_lower), ymax = log(recruits_predicted_high_cpd_upper)), fill = "#bf812d", alpha = 0.1) +
    labs(#title = "Ricker model with CPD, NPGO, ERSST", 
      x = "Spawners", y = TeX(r"($\log \left(Recruits\right)$)")) +
    scale_color_gradient2(name = 'CDA (%)',
                          low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 20)+
    theme_classic() +
    theme(legend.position = c(0.8,0.9),
          legend.background = element_rect(fill = alpha('white', 0.5)),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_blank(),
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(1, "lines"),
          legend.spacing.y = unit(0.001, "cm")
    )
  
  
  
  
  
  
  return((p1))
}



for(i in watersheds){
  
  effects_plot <- plot_all_effects_river_together(
    posterior1 = ric_chm_cpd_ocean_covariates_logR_long_chain,
    river_name = str_to_title(i),
    river = unique(case_study_watersheds_data$River_n[case_study_watersheds_data$River == i])
  )
  
  change_plot <- plot_recruitment_change_river_together(
    posterior1 = ric_chm_cpd_ocean_covariates_logR_long_chain,
    posterior2 = ric_chm_eca_ocean_covariates_logR_long_chain,
    river_name = i,
    effect1 = "cpd",
    effect2 = "eca",
    species = "chum",
    model1 = "CPD",
    model2 = "ECA",
    hd = FALSE
  )
  
  spawner_recruit_plot <- plot_recruit_spawner_river_new(species = "chum",
                                                         river_name = i,
                                                         posterior = ric_chm_cpd_ocean_covariates_logR_long_chain)
  
  ggsave(filename = here("output_figures_tables",
                         paste0("supplementary_fig_case_study_",str_replace_all(str_to_lower(i), " ", "_"),".png")),
         plot = ((effects_plot+spawner_recruit_plot + plot_layout(widths = c(1,1.2)))/change_plot) +
           plot_annotation(tag_levels = 'A',title = paste(str_to_title(i), "- Chum Salmon"))&
           theme(plot.tag.position = c(0.0, 1.0),
                 plot.tag = element_text(size = 10, hjust = 0, vjust = 0, face = "bold")),
         width = 7,
         height = 6,
         units = "in",
         dpi = 300)
  
  
}




# do same for pink but only for some watersheds - Phillips River, Deena Creek, Neekas
pink_watersheds_w_broodline = c("PHILLIPS RIVER_Even", 
                                "PHILLIPS RIVER_Odd",
                                "DEENA CREEK_Even",
                                "DEENA CREEK_Odd", 
                                "NEEKAS CREEK_Even",
                                "NEEKAS CREEK_Odd")

pink_watersheds <- c("PHILLIPS RIVER", 
                     "DEENA CREEK", 
                     "NEEKAS CREEK")


case_study_watersheds_data_pk <- pk10r %>% 
  filter(River2 %in% pink_watersheds_w_broodline)





plot_recruit_spawner_river_pink <- function(species = "pink", river_name,  posterior){
  
  # river_data <- df %>% filter(River_n == river)
  even_or_odd <- str_extract(river_name, "Even|Odd")
  
  df <- pk10r
  river_data <- pk10r %>% filter(River2 == river_name)
  
  # df$sst.std <- (pk10r$spring_ersst-mean(pk10r$spring_ersst))/sd(pk10r$spring_ersst)
  
  
  river_wo_broodline <- unique(river_data$River_n)
  river_w_broodline <- unique(river_data$River_n2)
  
  posterior_rv_b_for <- posterior %>% 
    select(starts_with('b_for_rv')) %>%
    select(ends_with(paste0("[",river_wo_broodline,"]")))
  
  posterior_rv_alpha_j <- posterior %>% 
    select(starts_with('alpha_j')) %>%
    select(ends_with(paste0("[",river_w_broodline,"]")))
  
  posterior_rv_S_max <- posterior %>% 
    select(starts_with('Smax')) %>%
    select(ends_with(paste0("[",river_w_broodline,"]")))
  
  # for(river in river_w_broodline_both){
  #   
  #   posterior_rv_alpha_j <- posterior %>% 
  #     select(starts_with('alpha_j')) %>%
  #     select(ends_with(paste0("[",river_w_broodline,"]")))
  #   
  #   posterior_rv_S_max <- posterior %>% 
  #     select(starts_with('Smax')) %>%
  #     select(ends_with(paste0("[",river_w_broodline,"]")))
  #   
  # }
  
  
  
  
  
  
  
  
  spawners_predicted <- seq(0, max(river_data$Spawners), length.out = 100)
  
  # calculate recruit prediction
  
  low_cpd <- min(river_data$sqrt.CPD.std)
  high_cpd <- max(river_data$sqrt.CPD.std)
  # avg_cpd <- mean(river_data$sqrt.CPD.std)
  mid_cpd <- min(river_data$sqrt.CPD.std) + (max(river_data$sqrt.CPD.std) - min(river_data$sqrt.CPD.std))/2
  
  mid_cpd_real <- min(river_data$disturbedarea_prct_cs) + (max(river_data$disturbedarea_prct_cs) - min(river_data$disturbedarea_prct_cs))/2
  
  
  recruits_predicted_low_cpd <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*low_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, median))*spawners_predicted
  
  recruits_predicted_high_cpd <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*high_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, median))*spawners_predicted
  
  
  recruits_predicted_low_cpd_lower <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*low_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.025)))*spawners_predicted
  
  recruits_predicted_low_cpd_upper <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*low_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.975)))*spawners_predicted
  
  recruits_predicted_high_cpd_lower <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*high_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.025)))*spawners_predicted
  
  recruits_predicted_high_cpd_upper <- exp(apply((matrix(posterior_rv_alpha_j[,1], ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1])) - as.matrix(1/posterior_rv_S_max)%*%spawners_predicted  + matrix(posterior_rv_b_for[,1]*high_cpd, ncol = length(spawners_predicted), nrow = length(posterior_rv_alpha_j[,1]))), 2, quantile, c(0.975)))*spawners_predicted
  
  
  
  #make dataframe
  
  prediction_df <- data.frame(spawners = spawners_predicted,
                              
                              recruits_low_cpd = recruits_predicted_low_cpd,
                              recruits_high_cpd = recruits_predicted_high_cpd,
                              recruits_low_cpd_lower = recruits_predicted_low_cpd_lower,
                              recruits_low_cpd_upper = recruits_predicted_low_cpd_upper,
                              recruits_high_cpd_lower = recruits_predicted_high_cpd_lower,
                              recruits_high_cpd_upper = recruits_predicted_high_cpd_upper)
  
  
  
  
  
  
  
  #plot the time varying productivity vs year, with log(R/S) data
  
  
  
  
  
  #plot recruit vs spawner as points
  
  p1 <- ggplot() +
    geom_point(data = river_data,aes(x = Spawners, y = Recruits, color = disturbedarea_prct_cs), alpha = 0.5, size = 2) +
    geom_line(data = prediction_df, aes(x = spawners, y = recruits_low_cpd), color = '#35978f', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = recruits_high_cpd), color = '#bf812d', size = 1, alpha = 0.5) +
    # geom_line(data = prediction_df, aes(x = spawners, y = recruits), color = "black", size = 1, alpha = 0.5) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = recruits_low_cpd_lower, ymax = recruits_low_cpd_upper), fill ='#35978f', alpha = 0.1) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = recruits_high_cpd_lower, ymax = recruits_high_cpd_upper), fill ='#bf812d', alpha = 0.1) +
    #make y log scale
    # scale_y_log10() +
    labs(title = even_or_odd, x = "Spawners", y = "Recruits") +
    # scale_color_manual(name = "CPD", values = c("Low" = '#35978f', "High" = '#bf812d')) +
    scale_color_gradient2(name = 'CDA (%)', guide = guide_colorbar(barwidth = 3, barheight = 0.5),
                          low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 20, n.breaks = 4) +
    theme_classic() +
    theme(legend.position = c(0.8,0.9),
          legend.background = element_rect(fill = alpha('white', 0.5)),
          legend.direction = "horizontal",
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8, vjust = 1, hjust = 1),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 9)
          # legend.key.width = unit(0.5, "cm"),
          # legend.key.height = unit(1, "lines"),
          # legend.spacing.y = unit(0.001, "cm")
    )
  
  
  # p3 log R/s vs spawners
  
  p2 <- ggplot(river_data) + 
    geom_point(aes(x = Spawners, y = log(Recruits/Spawners), color = disturbedarea_prct_cs), alpha = 0.5, size = 2) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_low_cpd/spawners_predicted)), color = '#35978f', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_high_cpd/spawners_predicted)), color = '#bf812d', size = 1, alpha = 0.5) +
    # geom_line(data = prediction_df, aes(x = spawners, y = log_RS), color = "black", size = 1, alpha = 0.5) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = log(recruits_predicted_low_cpd_lower/spawners_predicted), ymax = log(recruits_predicted_low_cpd_upper/spawners_predicted)), fill = "#35978f", alpha = 0.1) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = log(recruits_predicted_high_cpd_lower/spawners_predicted), ymax = log(recruits_predicted_high_cpd_upper/spawners_predicted)), fill = "#bf812d", alpha = 0.1) +
    labs(#title = "Ricker model with CPD, NPGO, ERSST", 
      x = "Spawners", y = TeX(r"($\log \left(\frac{Recruits}{Spawners}\right)$)")) +
    scale_color_gradient2(name = 'CDA (%)',
                          low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 20, guide = guide_colorbar(barwidth = 3, barheight = 0.5)) +
    theme_classic() +
    theme(legend.position = c(0.8,0.9),
          legend.background = element_rect(fill = alpha('white', 0.5)),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_blank(),
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(1, "lines"),
          legend.spacing.y = unit(0.001, "cm")
    )
  
  p3 <- ggplot(river_data) + 
    geom_point(aes(x = Spawners, y = log(Recruits), color = disturbedarea_prct_cs), alpha = 0.5, size = 2) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_low_cpd)), color = '#35978f', size = 1, alpha = 0.5) +
    geom_line(data = prediction_df, aes(x = spawners, y = log(recruits_predicted_high_cpd)), color = '#bf812d', size = 1, alpha = 0.5) +
    # geom_line(data = prediction_df, aes(x = spawners, y = log_RS), color = "black", size = 1, alpha = 0.5) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = log(recruits_predicted_low_cpd_lower), ymax = log(recruits_predicted_low_cpd_upper)), fill = "#35978f", alpha = 0.1) +
    geom_ribbon(data = prediction_df, aes(x = spawners, ymin = log(recruits_predicted_high_cpd_lower), ymax = log(recruits_predicted_high_cpd_upper)), fill = "#bf812d", alpha = 0.1) +
    labs(#title = "Ricker model with CPD, NPGO, ERSST", 
      x = "Spawners", y = TeX(r"($\log \left(Recruits\right)$)")) +
    scale_color_gradient2(name = 'CDA (%)',
                          low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 20)+
    theme_classic() +
    theme(legend.position = c(0.8,0.9),
          legend.background = element_rect(fill = alpha('white', 0.5)),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_blank(),
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(1, "lines"),
          legend.spacing.y = unit(0.001, "cm")
    )
  
  
  
  
  
  
  return((p1))
}




for(i in pink_watersheds){
  
  effects_plot <- plot_all_effects_river_together(
    posterior1 = ric_pk_cpd_ersst_long_chain,
    river_name = str_to_title(i),
    river = unique(case_study_watersheds_data_pk$River_n[case_study_watersheds_data_pk$River == i])
  )
  
  change_plot <- plot_recruitment_change_river_together(
    posterior1 = ric_pk_cpd_ersst_long_chain,
    posterior2 = ric_pk_eca_ersst_long_chain,
    river_name = i,
    effect1 = "cpd",
    effect2 = "eca",
    species = "pink",
    model1 = "CPD",
    model2 = "ECA",
    hd = FALSE
  )
  
  spawner_recruit_plot_even <- plot_recruit_spawner_river_pink(species = "pink",
                                                               river_name = paste0(i,"_Even"),
                                                               posterior = ric_pk_cpd_ersst_long_chain)
  spawner_recruit_plot_odd <- plot_recruit_spawner_river_pink(species = "pink",
                                                              river_name = paste0(i,"_Odd"),
                                                              posterior = ric_pk_cpd_ersst_long_chain)
  
  ggsave(filename = here("output_figures_tables",
                         paste0("supplementary_fig_case_study_pink_",str_replace_all(str_to_lower(i), " ", "_"),".png")),
         plot = ((effects_plot+spawner_recruit_plot_even/spawner_recruit_plot_odd + 
                    plot_layout(widths = c(1,1.2)))/change_plot) + plot_layout(heights = c(1.5,1)) +
           plot_annotation(tag_levels = 'A',title = paste(str_to_title(i), "- Pink Salmon"))&
           theme(plot.tag.position = c(0.0, 1.0),
                 plot.tag = element_text(size = 10, hjust = 0, vjust = 0, face = "bold")
           ),
         width = 7,
         height = 6,
         units = "in",
         dpi = 300)
  
  
}



#print effect sizes and CIs

all_river_df <- data.frame()
for(i in watersheds){
  
  river_data <- case_study_watersheds_data %>% filter(River == i)
  river <- river_data$River_n[1]
  
  posterior_cpd_df <- ric_chm_cpd_ocean_covariates_logR_long_chain %>%
    select(starts_with('b_for_rv'),starts_with('b_sst_rv'),starts_with('b_npgo_rv')) %>%
    pivot_longer(cols = everything(),
                 names_to = c('Effect','River'),
                 names_pattern = 'b_(.*)_rv(.*)',
                 values_to = "coefficient") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    filter(River_n == river)
  
  posterior_eca_df <- ric_chm_eca_ocean_covariates_logR_long_chain %>%
    select(starts_with('b_for_rv'),starts_with('b_sst_rv'),starts_with('b_npgo_rv')) %>%
    pivot_longer(cols = everything(),
                 names_to = c('Effect','River'),
                 names_pattern = 'b_(.*)_rv(.*)',
                 values_to = "coefficient") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    filter(River_n == river)
  
  effect_sizes_cpd <- posterior_cpd_df %>%
    group_by(Effect) %>%
    summarise(median = round(median(coefficient),2),
              lower_95 = round(quantile(coefficient, 0.025),2),
              upper_95 = round(quantile(coefficient, 0.975),2))
  
  effect_sizes_eca <- posterior_eca_df %>%
    group_by(Effect) %>%
    summarise(median = round(median(coefficient),2),
              lower_95 = round(quantile(coefficient, 0.025),2),
              upper_95 = round(quantile(coefficient, 0.975),2))
  
  # print(paste0("River: ", i))
  # print("CPD effect sizes:")
  # print(effect_sizes_cpd)
  # print("ECA effect sizes:")
  # print(effect_sizes_eca)
  
  #print predicted recruitment change (%) at most recent CPD levels and most recent ECA levels
  
  eca <- seq(0,1,length.out=100)
  
  eca_sqrt <- sqrt(eca)
  
  eca_sqrt_std <- (eca_sqrt-mean(eca_sqrt))/sd(eca_sqrt)
  
  eca_df <- data.frame(eca = eca,
                       eca_sqrt = eca_sqrt,
                       eca_sqrt_std = eca_sqrt_std)
  
  cpd <- seq(0,100,length.out=100)
  
  cpd_sqrt <- sqrt(cpd)
  
  cpd_sqrt_std <- (cpd_sqrt-mean(cpd_sqrt))/sd(cpd_sqrt)
  
  cpd_df <- data.frame(cpd = cpd,
                       cpd_sqrt = cpd_sqrt,
                       cpd_sqrt_std = cpd_sqrt_std)
  
  
  
  
  no_eca <- min(eca_sqrt_std)
  
  no_cpd <- min(cpd_sqrt_std)
  
  max_eca <- eca_df$eca_sqrt_std[which.min(abs(eca_df$eca - max(river_data$ECA_age_proxy_forested_only)))]
  
  max_cpd <- cpd_df$cpd_sqrt_std[which.min(abs(cpd_df$cpd - max(river_data$disturbedarea_prct_cs)))]
  
  
  
  # need to change b_rv to posterior 1 and posterior 2 and then make figure
  
  recruitment_cpd <- (exp(as.matrix(posterior_cpd_df %>% filter(Effect == "for") %>% select(coefficient))%*%
                            (max_cpd-no_cpd)))*100 - 100
  
  recruitment_eca <- (exp(as.matrix(posterior_eca_df %>% filter(Effect == "for") %>% select(coefficient))%*%
                            (max_eca-no_eca)))*100 - 100
  
  recruitment_df <- data.frame(
    Effect = c("Recruitment change at max CPD level", "Recruitment change at max ECA level"),
    model = c("CDA", "ECA"),
    median = c(round(median(recruitment_cpd),1), round(median(recruitment_eca),1)),
    lower_95 = c(round(quantile(recruitment_cpd, 0.025),1), round(quantile(recruitment_eca, 0.025),2)),
    upper_95 = c(round(quantile(recruitment_cpd, 0.975),1), round(quantile(recruitment_eca, 0.975),1))
  )
  
  #put the effect sizes and recuitment df together
  
  river_df <- effect_sizes_cpd %>% 
    mutate(model = "CDA") %>% 
    rbind(effect_sizes_eca %>% mutate(model = "ECA")) %>% 
    rbind(recruitment_df) %>% 
    mutate(River = i, Species = "chum")
  
  all_river_df <- rbind(all_river_df, river_df)
  
  
}


# pink watersheds

all_river_df_pk <- data.frame()

for(i in pink_watersheds){
  
  river_data <- case_study_watersheds_data_pk %>% filter(River == i)
  river <- river_data$River_n[1]
  
  posterior_cpd_df <- ric_pk_cpd_ersst_long_chain %>%
    select(starts_with('b_for_rv'),starts_with('b_sst_rv'),starts_with('b_npgo_rv')) %>%
    pivot_longer(cols = everything(),
                 names_to = c('Effect','River'),
                 names_pattern = 'b_(.*)_rv(.*)',
                 values_to = "coefficient") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    filter(River_n == river)
  
  posterior_eca_df <- ric_pk_eca_ersst_long_chain %>%
    select(starts_with('b_for_rv'),starts_with('b_sst_rv'),starts_with('b_npgo_rv')) %>%
    pivot_longer(cols = everything(),
                 names_to = c('Effect','River'),
                 names_pattern = 'b_(.*)_rv(.*)',
                 values_to = "coefficient") %>%
    mutate(River_n = as.numeric(str_extract(River, '\\d+'))) %>% 
    select(-River) %>% 
    filter(River_n == river)
  
  effect_sizes_cpd <- posterior_cpd_df %>%
    group_by(Effect) %>%
    summarise(median = round(median(coefficient),2),
              lower_95 = round(quantile(coefficient, 0.025),2),
              upper_95 = round(quantile(coefficient, 0.975),2))
  
  effect_sizes_eca <- posterior_eca_df %>%
    group_by(Effect) %>%
    summarise(median = round(median(coefficient),2),
              lower_95 = round(quantile(coefficient, 0.025),2),
              upper_95 = round(quantile(coefficient, 0.975),2))
  
  
  eca <- seq(0,1,length.out=100)
  
  eca_sqrt <- sqrt(eca)
  
  eca_sqrt_std <- (eca_sqrt-mean(eca_sqrt))/sd(eca_sqrt)
  
  eca_df <- data.frame(eca = eca,
                       eca_sqrt = eca_sqrt,
                       eca_sqrt_std = eca_sqrt_std)
  
  cpd <- seq(0,100,length.out=100)
  
  cpd_sqrt <- sqrt(cpd)
  
  cpd_sqrt_std <- (cpd_sqrt-mean(cpd_sqrt))/sd(cpd_sqrt)
  
  cpd_df <- data.frame(cpd = cpd,
                       cpd_sqrt = cpd_sqrt,
                       cpd_sqrt_std = cpd_sqrt_std)
  
  
  
  
  no_eca <- min(eca_sqrt_std)
  
  no_cpd <- min(cpd_sqrt_std)
  
  max_eca <- eca_df$eca_sqrt_std[which.min(abs(eca_df$eca - max(river_data$ECA_age_proxy_forested_only)))]
  
  max_cpd <- cpd_df$cpd_sqrt_std[which.min(abs(cpd_df$cpd - max(river_data$disturbedarea_prct_cs)))]
  
  
  
  # need to change b_rv to posterior 1 and posterior 2 and then make figure
  
  recruitment_cpd <- (exp(as.matrix(posterior_cpd_df %>% filter(Effect == "for") %>% select(coefficient))%*%
                            (max_cpd-no_cpd)))*100 - 100
  
  recruitment_eca <- (exp(as.matrix(posterior_eca_df %>% filter(Effect == "for") %>% select(coefficient))%*%
                            (max_eca-no_eca)))*100 - 100
  
  recruitment_df <- data.frame(
    Effect = c("Recruitment change at max CPD level", "Recruitment change at max ECA level"),
    model = c("CDA", "ECA"),
    median = c(round(median(recruitment_cpd),1), round(median(recruitment_eca),1)),
    lower_95 = c(round(quantile(recruitment_cpd, 0.025),1), round(quantile(recruitment_eca, 0.025),2)),
    upper_95 = c(round(quantile(recruitment_cpd, 0.975),1), round(quantile(recruitment_eca, 0.975),1))
  )
  
  
  
  river_df <- effect_sizes_cpd %>% 
    mutate(model = "CDA") %>% 
    rbind(effect_sizes_eca %>% mutate(model = "ECA"))  %>% 
    rbind(recruitment_df) %>% 
    mutate(River = i, Species = "pink")
  
  all_river_df_pk <- rbind(all_river_df_pk, river_df)
  
}

# format table

# make a wide table with rivers-species combination in the columns and "median (lower_95, upper_95)"values


all_river_df_chum_pink <- all_river_df %>% 
  rbind(all_river_df_pk) %>% 
  mutate(Estimate = paste(median, " (", lower_95, ", ", upper_95, ")", sep = "")) %>%
  mutate(Effect = case_when(Effect == "for" ~ "Forestry effect size",
                            Effect == "sst" ~ "SST effect size",
                            Effect == "npgo" ~ "NPGO effect size",
                            Effect == "Recruitment change at max CPD level" ~ "Recruitment change",
                            Effect == "Recruitment change at max ECA level" ~ "Recruitment change")) %>%
  rename(Model = model) %>%
  select(River, Species, Effect, Estimate, Model) %>%
  pivot_wider(#id = c(River, Species, model, Effect),
    names_from = c(River, Species),
    values_from = Estimate,
    names_vary = "slowest"
  ) %>% 
  arrange(Model)

# save table

write.csv(all_river_df_chum_pink, here("output_figures_tables","supplementary_table_case_study_effect_sizes_recruitment_chum_pink.csv"), row.names = FALSE)











