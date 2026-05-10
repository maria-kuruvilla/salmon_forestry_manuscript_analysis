#' Title Plot timeseries with map inset
#'
#' @param target_CU Character string with target salmon Conservation Unit (CU) to plot.
#' @param mod.dat Forestry dataset to plot.
#' @param CU_poly SF object with the boundaries of CU to plot as an inset map.
#'
#' @returns A plot a time series of Equivalent clearcut Area over time.
#' @export
#'
#' @examples
plot_forest_disturance_timeseries_inset <- function(target_CU, 
                       mod.dat, 
                       var_to_plot = NULL,
                       CU_poly,
                       y_lab = "INSERT Y LABEL",
                       y_lim = c(0, 60),
                       Canada,
                       USA) {
  
  mod.dat <- mod.dat %>%
    rename(plot_var = matches(var_to_plot))
  
    # main plot
  CU_ECA <- ggplot(mod.dat %>% filter(CU == target_CU), 
                   aes(x = BroodYear, y = plot_var)) +
    geom_line(size = 0.1, aes(group = River), color = "black", alpha = 0.3) + 
    labs(x = "", y = y_lab) +
    scale_y_continuous(labels = scale_round, breaks = seq(0, 100, 20), limits = c(0, 100)) +
    coord_cartesian(ylim = y_lim) +
    theme_bw() + 
    ggtitle(target_CU)  + 
    theme(axis.text.y = element_text(size = 10), 
          axis.title.y = element_text(size = 8))  + 
    geom_smooth(method = "gam", formula = y ~ s(x), 
                se = TRUE, 
                color = "#3182bd", 
                level = 0.95, 
                fill = "#3182bd", linewidth = 0.5)
  
  # Inset
  cu.pol <- CU_poly %>%
    filter(CU == target_CU)
  
  b <- st_bbox(CU_poly)
  
  inset.plt <- ggplot() +
    theme_void() + theme(plot.background =  element_rect(fill = "lightblue", colour = "black")) +
    geom_sf(data = Canada, fill = alpha(colour = "darkgreen", alpha = 0.2), color = alpha(colour = "darkgreen", alpha = 0), size = 0)  +
    geom_sf(data = USA, fill = alpha(colour = "darkgreen", alpha = 0.2), color = alpha(colour = "darkgreen", alpha = 0), size = 0)  +
    geom_sf(data = CU_poly, fill = "gray85", color = "gray85") +
    geom_sf(data = cu.pol, fill = "red", color = "red", size = 0.05)  +
    coord_sf(crs = st_crs(CU_poly), xlim = c(b["xmin"], b["xmax"]) , ylim = c(b["ymin"], b["ymax"]))
  
  plot.with.inset <- ggdraw() +
    draw_plot(CU_ECA) +
    draw_plot(inset.plt, x = 0.08, y = .54, width = .3, height = .3)
}

scale_round <- function(x) round(x, digits = 2)
