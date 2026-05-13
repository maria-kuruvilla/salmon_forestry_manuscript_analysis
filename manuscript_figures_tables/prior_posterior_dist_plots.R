# Goal - make prior and posterior distribution plots for some parameters:
# alpha0, all fixed effects

library(tidyverse)
library(here)
library(ggplot2)


# Load the data:

ric_chm_eca_ocean_covariates_logR_long_chain=read.csv(here('salmon_forestry_data_analysis','stan models','outs','posterior',
                                                           'ric_chm_eca_ocean_covariates_logR_long_chain.csv'),check.names=F)
ric_chm_cpd_ocean_covariates_logR_long_chain=read.csv(here('salmon_forestry_data_analysis','stan models','outs','posterior',
                                                           'ric_chm_cpd_ocean_covariates_logR_long_chain.csv'),check.names=F)


# Make the prior distribution data frame:

prior_df=data.frame(parameter=c('alpha0','b_for','b_sst','b_npgo','mu_sigma'),
                    mean=c(1.2,0,0,0,1),
                    sd=c(1,1,1,1,1))

#draw from prior

set.seed(123)

prior_draws_df <- prior_df %>%
  rowwise() %>%
  do(data.frame(parameter = .$parameter,
                value = rnorm(3000, mean = .$mean, sd = .$sd))) %>% 
  mutate(type = "prior")

#remove all values <0 from mu_sigma prior
prior_draws_df <- prior_draws_df %>%
  filter(!(parameter == "mu_sigma" & value < 0))

ric_chm_cpd_ocean_covariates_logR_long_chain <- readRDS(here("salmon_forestry_data_analysis","stan models","outs","fits",
                                                             "ric_chm_cpd_ocean_covariates_logR_long_chain.RDS"))

post_ric_chm_cpd_npgo_sst=ric_chm_cpd_ocean_covariates_logR_long_chain$draws(variables=c('b_for',
                                                                 'b_npgo',
                                                                 'b_sst',
                                                                 'alpha0','mu_sigma'),format='draws_matrix')

# Plot the prior and posterior distribution of alpha0 in one figure:

ggplot() +
  geom_density(data = prior_draws_df %>% filter(parameter == 'alpha0'), aes(x = value, fill = "prior") , alpha = 0.5) +
  geom_density(data = as.data.frame(post_ric_chm_cpd_npgo_sst) %>% select("alpha0"), aes(x = alpha0, fill = "posterior"), alpha = 0.5) +
  labs(title = 'Intrinsic productivity',
       x = 'Value',
       y = 'Density') +
  theme_classic() +
  scale_fill_manual(values = c("prior" = "#B2A3B5", "posterior" = '#53474E'))+
  theme(legend.title=element_blank(),
        legend.position  = c(0.9,0.5))

#forestry
ggplot() +
  geom_density(data = prior_draws_df %>% filter(parameter == 'beta_for'), aes(x = value, fill = "prior") , alpha = 0.5) +
  geom_density(data = as.data.frame(post_ric_chm_cpd_npgo_sst) %>% select("b_for"), aes(x = b_for, fill = "posterior"), alpha = 0.5) +
  labs(title = 'Effect of Forestry',
       x = 'Value',
       y = 'Density') +
  theme_classic() +
  scale_fill_manual(values = c("prior" = "#B2A3B5", "posterior" = '#53474E'))+
  theme(legend.title=element_blank(),
        legend.position  = c(0.9,0.5))

#sst
ggplot() +
  geom_density(data = prior_draws_df %>% filter(parameter == 'beta_sst'), aes(x = value, fill = "prior") , alpha = 0.5) +
  geom_density(data = as.data.frame(post_ric_chm_cpd_npgo_sst) %>% select("b_sst"), aes(x = b_sst, fill = "posterior"), alpha = 0.5) +
  labs(title = 'Effect of SST',
       x = 'Value',
       y = 'Density') +
  theme_classic() +
  scale_fill_manual(values = c("prior" = "#B2A3B5", "posterior" = '#53474E'))+
  theme(legend.title=element_blank(),
        legend.position  = c(0.9,0.5))



#npgo
ggplot() +
  geom_density(data = prior_draws_df %>% filter(parameter == 'beta_npgo'), aes(x = value, fill = "prior") , alpha = 0.5) +
  geom_density(data = as.data.frame(post_ric_chm_cpd_npgo_sst) %>% select("b_npgo"), aes(x = b_npgo, fill = "posterior"), alpha = 0.5) +
  labs(title = 'Effect of NPGO',
       x = 'Value',
       y = 'Density') +
  theme_classic() +
  scale_fill_manual(values = c("prior" = "#B2A3B5", "posterior" = '#53474E'))+
  theme(legend.title=element_blank(),
        legend.position  = c(0.9,0.5))

#mu_sigma
ggplot() +
  geom_density(data = prior_draws_df %>% filter(parameter == 'mu_sigma'), aes(x = value, fill = "prior") , alpha = 0.5) +
  geom_density(data = as.data.frame(post_ric_chm_cpd_npgo_sst) %>% select("mu_sigma"), aes(x = mu_sigma, fill = "posterior"), alpha = 0.5) +
  labs(title = 'Standard deviation',
       x = 'Value',
       y = 'Density') +
  theme_classic() +
  scale_fill_manual(values = c("prior" = "#B2A3B5", "posterior" = '#53474E'))+
  theme(legend.title=element_blank(),
        legend.position  = c(0.9,0.5))



# make the posterior dataframe long and then use facet_wrap to make all the plots into one plot

post_ric_chm_cpd_npgo_sst_long=as.data.frame(post_ric_chm_cpd_npgo_sst) %>%
  select("alpha0","b_for","b_sst","b_npgo","mu_sigma") %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "value") %>% 
  mutate(type = "posterior")


post_prior_long=prior_draws_df %>%
  rbind(post_ric_chm_cpd_npgo_sst_long)


#convert parameter to factor
post_prior_long$parameter <- factor(post_prior_long$parameter, levels = c("b_for","b_sst","b_npgo","alpha0","mu_sigma"))


math_labels <- as_labeller(c(
  "alpha0" = "alpha[0]",
  "b_for" = "beta[0]^CDA",
  "b_sst" = "beta[0]^SST",
  "b_npgo" = "beta[0]^NPGO",
  "mu_sigma" = "sigma^mu"
  ), default = label_parsed)

math_labels2 <- as_labeller(c(
  "alpha0" = paste(expression("\u03B1")),
  "b_for" = paste(expression("\u03B2")),
  "b_sst" = paste(expression("\u03B2"),"^SST"),
  "b_npgo" = paste(expression("\u03B2"),expression("\u1D2C"),expression("\u1D2C")),
  "mu_sigma" = "mu"
))




ggplot(post_prior_long, aes(x = value, fill = type, color = type)) +
  geom_density(alpha = 0.5, linewidth = 0.7) +
  #label parameters with math symbols
  facet_wrap(~parameter, scales = "free_y", ncol = 2, labeller = labeller(parameter = math_labels), dir = "v") +
  labs(#title = 'Prior and Posterior Distributions',
       x = 'Value',
       y = 'Density') +
  xlim(-1,3) +
  theme_classic() +
  scale_fill_manual(values = c("posterior" = '#72BDA3', "prior" = "#B2A3B5"))+
  scale_color_manual(values = c("posterior" = '#72BDA3', "prior" = "#B2A3B5"))+
  theme(legend.title=element_blank(),
        legend.position  = c(0.8,0.1),
        legend.text = element_text(size = 10),
        title = element_text(size = 12),
        #remove facet label box
        strip.background = element_rect(fill="gray90", color = "transparent"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

#save
ggsave(here('output_figures_tables','prior_posterior_dist.png'), width = 6, height = 6, dpi = 300)



# plot posterior distribtuions of Smax

post_Smax = as.data.frame(ric_chm_cpd_ocean_covariates_logR_long_chain$draws(variables=c('Smax[1]'),format='draws_matrix') )%>%
  rename(Smax_1 = "Smax[1]") 

post_Smax = ric_chm_cpd_ocean_covariates_logR_long_chain %>% 
  select(starts_with("Smax")) %>%
  as.data.frame() %>%
  # rename_with(~ gsub("Smax\\[|\\]", "Smax_", .)) #rename columns to remove brackets and replace with underscore
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "value", names_pattern = "Smax\\[(\\d+)\\]") %>%
  mutate(parameter = paste0("Smax_",parameter)) %>% 
  mutate(type = "posterior")

#extract max S for priors on capacity & eq. recruitment
smax_prior=
  ch20rsc %>%
  group_by(River) %>%
  summarize(m.s=Spawners[which.max(Recruits)],m.r=max(Recruits))


pSmax_mean = smax_prior$m.s
pSmax_sig =  3*smax_prior$m.s

logsmax_pr_sig= sqrt(log(1+((pSmax_sig)^2/(pSmax_mean)^2))) #this converts sigma on the untransformed scale to a log scale
logsmax_pr =log(pSmax_mean)-0.5*logsmax_pr_sig^2


prior_df_Smax=data.frame(parameter=paste0("Smax_",1:length(pSmax_mean)),
                    mean=c(logsmax_pr),
                    sd=c(logsmax_pr_sig))
set.seed(123)

prior_draws_df_Smax <- prior_df_Smax %>%
  rowwise() %>%
  do(data.frame(parameter = .$parameter,
                value = rlnorm(3000, mean = .$mean, sd = .$sd))) %>% 
  mutate(type = "prior")

post_prior_Smax_long=prior_draws_df_Smax %>%
  rbind(post_Smax)



ggplot() +
  geom_density(data = prior_draws_df_Smax %>% filter(parameter == 'Smax_1'), aes(x = value, fill = "prior") , alpha = 0.5) +
  geom_density(data = post_Smax, aes(x =Smax_1, fill = "posterior"), alpha = 0.5) +
  xlim(0,5000)+
  labs(title = 'Smax',
       x = 'Value',
       y = 'Density') +
  theme_classic() +
  scale_fill_manual(values = c("prior" = "#B2A3B5", "posterior" = '#53474E'))+
  theme(legend.title=element_blank(),
        legend.position  = c(0.9,0.5))

x = matrix(1:9)
list_rivers = apply(x, 1, function (x) paste0("Smax_",x))

data_plt = post_prior_Smax_long %>% filter(parameter %in% list_rivers)

ggplot(data_plt, 
       aes(x = value, fill = type, color = type)) +
  geom_density(alpha = 0.5, linewidth = 0.7) +
  #label parameters with math symbols
  facet_wrap(~parameter, scales = "free", ncol = 3, 
             # labeller = labeller(parameter = math_labels), 
             dir = "v") +
  labs(#title = 'Prior and Posterior Distributions',
    x = 'Value',
    y = 'Density') +
  xlim(0 ,data_plt  %>% group_by(type) %>% summarize(max = max(value)) %>% filter(type == 'posterior') %>% pull()) +
  theme_classic() +
  scale_fill_manual(values = c("posterior" = '#72BDA3', "prior" = "#B2A3B5"))+
  scale_color_manual(values = c("posterior" = '#72BDA3', "prior" = "#B2A3B5"))+
  theme(legend.title=element_blank(),
        legend.position  = c(0.9,0.9),
        legend.text = element_text(size = 10),
        title = element_text(size = 12),
        #remove facet label box
        strip.background = element_rect(fill="gray90", color = "transparent"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

#save
ggsave(here('output_figures_tables','prior_posterior_dist_Smax.png'), width = 10, height = 6, dpi = 300)

