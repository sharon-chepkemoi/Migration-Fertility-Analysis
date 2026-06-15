
#-------------------------------------------------------------------
# Function to save r plots and theme for r plots
#--------------------------------------------------------------------



save_plot <- function(plot, filename, width = 8, height = 6) {
  ggsave(
    file.path(output_plots_Dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 300
  )
}


save_fun <- function(plot, filename, width = 10, height = 6) {
  ggsave(
    filename = file.path(output_plots_Dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    device = "png",
    bg = "white"
  )
}

theme_plot <- function() {
  theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

plot_fun <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      
      axis.title = element_text(size = 13),
      axis.text  = element_text(size = 12, colour = "black"),
      
      axis.text.x = element_text(
        angle = 15,
        hjust = 1,
        size = 12,
        colour = "black"
      ),
      
      axis.text.y = element_text(size = 12, colour = "black"),
      
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.2, colour = "grey85")
    )
}
