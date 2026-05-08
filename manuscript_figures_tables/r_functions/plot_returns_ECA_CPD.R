returns_ECA_CPD_plot <- function(dat_plot = NULL, 
                                 river_str = NULL, 
                                 save_plot = FALSE, 
                                 print_plot = FALSE, 
                                 return_plot = FALSE, 
                                 add_points = FALSE,
                                 species_label = "Chum",
                                 ...){
  
  lfid <- dat_plot %>%
    filter(River == river_str) %>%
    pull(LINEAR_FEATURE_ID) %>%
    unique()
  
  if(length(lfid) != 1) browser() # stop("ERROR LFID")
  
  # add broken line per hatchery_enhancement annual status
  pop_plot_salm <- dat_plot %>%
    filter(River == river_str) %>%
    select(River, BroodYear, Returns, hatchery_enhancement
           # , CU
    ) %>%
    group_by(River, hatchery_enhancement
             # , CU
    ) %>%
    complete(BroodYear = 1950:2023) %>%
    ungroup() %>%
    mutate(River = str_to_title(River))
  
  pop_plot_ECA <- dat_plot %>%
    select(River,LINEAR_FEATURE_ID,  BroodYear, ECA_age_proxy_forested_only, haarea_prct_cs) %>%
    filter(LINEAR_FEATURE_ID == lfid) %>%
    mutate(ECA_age_proxy_forested_only = ECA_age_proxy_forested_only * 100,
           River = str_to_title(River)) 
  
  ylim.prim <- range(pop_plot_salm$Returns, na.rm = T)
  
  ylim.prim <- range(pretty(ylim.prim))
  
  if(diff(ylim.prim) == 0) ylim.prim <- c(0, ylim.prim[1])
  if(is.infinite(diff(ylim.prim))) ylim.prim <- c(0, 100)
  
  ylim.sec <- range(pop_plot_ECA$haarea_prct_cs, na.rm = T)
  
  if(ylim.sec[2] > 80) ylim.sec <- c(0, 100)
  if(ylim.sec[2] < 80) ylim.temp <- c(0, 80)    
  if(ylim.sec[2] < 60) ylim.temp <- c(0, 60)    
  if(ylim.sec[2] < 50) ylim.temp <- c(0, 50)    
  if(ylim.sec[2] < 40) ylim.temp <- c(0, 40)    
  if(ylim.sec[2] < 25) ylim.temp <- c(0, 25)    
  
  ylim.sec <- ylim.temp
  
  b <- diff(ylim.prim)/diff(ylim.sec)
  
  colrs <- c("red","red", "black","black")
  names(colrs ) <-  c(paste0(species_label," Salmon Returns (Hatchery Enhanced Year)"), paste0(species_label, " Salmon Returns"), "Equivalent Clearcut Area (%)", "Cumulative Disturbed Area (%)")
  
  linr <- c("dotted", "solid", "solid", "dashed")
  names(linr) <- c(paste0(species_label," Salmon Returns (Hatchery Enhanced Year)"), paste0(species_label, " Salmon Returns"), "Equivalent Clearcut Area (%)", "Cumulative Disturbed Area (%)")
  
  returns_plt <- pop_plot_salm %>%
    ggplot(., aes(x = BroodYear , y = Returns)) +
    
    
    geom_line(data = pop_plot_ECA , mapping = aes(color = "Equivalent Clearcut Area (%)", y = ECA_age_proxy_forested_only * b ), linetype = "solid", linewidth = 1) +
    geom_line(data = pop_plot_ECA, mapping = aes(color = "Cumulative Disturbed Area (%)", y = haarea_prct_cs * b), linetype = "dashed", linewidth = 1) +
    
    geom_line( aes(color = paste0(species_label, " Salmon Returns"), linetype = hatchery_enhancement), linewidth = 0.5) +  
    
    labs(x = "", color = "" , y = paste0(species_label, " Salmon Returns")) +
    scale_y_continuous( labels = comma, breaks = seq(ylim.prim[1], ylim.prim[2], length.out = 5), 
                        sec.axis = sec_axis(~ (.)/b,
                                            breaks = pretty(seq(0, ylim.sec[2], length.out = 5)),
                                            name = "Forest Disturbance (%)")
    ) +
    coord_cartesian(ylim = ylim.sec * b) +
    facet_wrap(~River, strip.position = "top") +
    
    scale_x_continuous(breaks = seq(1880, 2025, 10)) +
    scale_color_manual(values = colrs, 
                       labels = names(colrs)) +
    
    scale_linetype_manual(values = c("solid", "dotted"), 
                          labels = c( "TRUE", "FALSE")) + 
    
    theme_classic() + theme(legend.position = "none", 
                            panel.grid = element_blank(),
                            axis.title = element_text(face = "bold", size = 12),
                            axis.text = element_text(face = "bold", size = 10),
                            strip.background = element_blank(),
                            strip.text = element_text(hjust = 0, face = "bold", size = 15),
                            base_family = "ArcherPro Book")
  
  
  if(add_points) returns_plt <- returns_plt + geom_point(data = pop_plot_salm, aes(x = BroodYear , y = Returns, shape = hatchery_enhancement), color = "red", size = 1) + scale_shape_manual(values = c(16, 17))
  
  # Plot for custom legend
  ff <- expand.grid(x = 1:20, y = 1:20, code = c(paste0(species_label," Salmon Returns (Hatchery Enhanced Year)"), paste0(species_label, " Salmon Returns"), "Equivalent Clearcut Area (%)", "Cumulative Disturbed Area (%)"))
  
  
  plt.lg <- ggplot(ff, aes(x, y, color = code, linetype = code)) + geom_line() + 
    scale_color_manual(values = colrs) +
    scale_linetype_manual(values = linr) +
    labs(color = "", linetype = "", shape = "") +
    theme_bw() + 
    theme(legend.position = "bottom", legend.text = element_text(size = 10)) + 
    guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) + 
    theme(legend.margin = unit(c(0,0,0,0), units = "npc"))
  
  
  if(add_points) plt.lg <- plt.lg <- plt.lg + geom_point(aes(shape = code)) + scale_shape_manual(values = c(17,16, NA,NA))
  
  leg <- ggpubr::get_legend(plt.lg)
  
  plt.lg <-  ggarrange(returns_plt, leg, ncol = 1, heights = c(1, 0.1), widths = c(1,1.2))
  
  if(save_plot) {
    ggsave(plot = plt.lg, ..., create.dir = T,bg = "white" ) 
    
    cat(unique(pop_plot_salm$River), unique(pop_plot_salm$CU), "Plot saved", "\n")
  }
  
  if(print_plot) print(plt.lg)
  
  if(return_plot) return(plt.lg)
  
}
