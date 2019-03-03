###############################################################################
###############################################################################
########	
########  ggplot graphics and color themes for plotting
########
###############################################################################
###############################################################################


# Define themes for plotting --------------------------------------------------    

# updated overall theme
theme_screen <- function(base_size = 10, base_family = "Helvetica") { # helvetica can cause troubles, but it is nice
  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    theme(axis.title = element_text(face = "bold", 
                                    size = rel(1.2)), 
          axis.title.y = element_text(angle = 90, 
                                      vjust = 0), 
          axis.title.x = element_text(vjust = 1), 
          axis.text = element_text(size = rel(1)), 
          axis.line = element_line(colour = "black"), 
          axis.ticks = element_line(), 
          panel.grid.major = element_line(colour = "grey"),
          panel.grid.minor = element_line(colour = "grey"), 
          legend.key = element_rect(colour = NA), 
          legend.key.height = unit(0.3, "cm"), 
          legend.key.width  = unit(0.4, "cm"), 
          legend.title = element_text(face = "bold", 
                                      size = rel(0.7)), 
          legend.text  = element_text(size = rel(0.6)), 
          legend.background = element_rect(fill = NA), 
          legend.spacing = unit(0, "cm"), 
          panel.background = element_rect(colour = NA), 
          panel.border = element_rect(colour = 1), 
          panel.spacing = unit(3, "mm"), 
          plot.background = element_rect(colour = NA), 
          plot.margin = unit(c(3, 3, 3, 3), "mm"), 
          plot.title = element_text(face = "bold",
                                    size = rel(1.2), 
                                    hjust = 0.5), 
          strip.text = element_text(face = "bold", 
                                    size = rel(1)), 
          strip.background = element_blank()
    )
}


# define color scheme
cols1 <- c("#7fc97f", "#386cb0", "#ef3b2c", "#fdb462", "#662506", 
           "#a6cee3", "#fb9a99", "#984ea3", "#ffff33", "#123456", 
           "#888888")

# theme for fill scales
scale_fill_pub <- scale_fill_Publication <- 
  function(values = cols1, face = "italic", ...){
    discrete_scale("fill", "Publication", 
                   scales::manual_pal(values = values),
                   guide = guide_legend(order = 1, 
                                        label.theme = element_text(size = 8, 
                                                                   angle = 0, 
                                                                   face = face)), 
                   ...)
  }

# theme for colour scales
scale_color_pub <- scale_colour_Publication <- 
  function(nrow_legend = 2, pch_legend = 8, 
           values = cols1, face = "italic", ...){
    discrete_scale("colour", "Publication", 
                   scales::manual_pal(values = values), 
                   guide = guide_legend(order = 1,             
                                        nrow = nrow_legend,
                                        label.theme = element_text(size = pch_legend,
                                                                   angle = 0, 
                                                                   face = face)), 
                   ...)
  }

# theme for linetype scales; legend typeface is set to be plain 
scale_linetype_pub <- scale_linetype_Publication <- 
  function(face = "plain", ...){
    discrete_scale("linetype", "Publication", 
                   scales::manual_pal(values = c(1, 2)), 
                   guide = guide_legend(order = 2,
                                        label.theme = element_text(size = 8, 
                                                                   angle = 0, 
                                                                   face = face)), 
                   ...)
  }
