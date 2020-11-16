## --------------------------------------------------------------------------------------------------------------------------------------------------
# prep libs
library(data.table)
library(atlastools)
library(ggplot2)
library(patchwork)

# prepare a palette
pal <- RColorBrewer::brewer.pal(4, "Set1")


## ----read_sim_data_2-------------------------------------------------------------------------------------------------------------------------------
# read in the data and set the window size variable
data <- fread("data/data_sim.csv")[5000:10000, ]
data[, window_size := NA]

# data with small scale errors but no reflections or outliers
data_errors <- fread("data/data_no_reflection.csv")
data_errors[, window_size := 0]


## --------------------------------------------------------------------------------------------------------------------------------------------------
# smooth the data over four K values
list_of_smooths <- lapply(c(3, 5, 11, 21), function(K) {
  
  data_copy <- copy(data_errors)
  
  data_copy <- atl_median_smooth(data = data_copy,
                    x = "x", 
                    y = "y",
                    time = "time",
                    moving_window = K)
  
  data_copy[, window_size := K]
})


## --------------------------------------------------------------------------------------------------------------------------------------------------
fwrite(list_of_smooths[[3]], file = "data/data_smooth.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------
# bind list after offset
data_plot <- mapply(function(df, offset) {
  df[, x := x + offset]
}, list_of_smooths, seq(0.4, 1.25, length.out = 4),
SIMPLIFY = F)

data_plot <- rbindlist(data_plot)


## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------
# prepare data to plot
# make list of data to plot
figure_median_smooth <-
  ggplot()+
  geom_point(data = data_errors,
             aes(x, y),
             col = pal[3],
             size = 0.2, alpha = 0.5)+
  geom_path(data = data,
            aes(x, y),
            col = "grey20",
            lwd = 0.5)+          
  geom_path(data = data_plot,
            aes(x, y,
                col = window_size,
                group = window_size),
            show.legend = F,
            lwd = 0.5)+
  coord_equal(expand = F,
              ylim = c(0.6, 0.85),
              xlim = c(NA, 2.3),
              ratio = 1.75)+
  annotate(geom = "text",
           x = c(0.75, seq(1.1, 2, length.out = 4)),
           y = 0.82,
           label = sprintf("(%s)", letters[seq(5)]),
           fontface = "bold")+
  scale_colour_distiller(palette = "BuPu", direction = 1,
                         values = c(-0.5, 1))+
  ggthemes::theme_few()+
  theme(axis.text = element_blank(),
        axis.title = element_blank())

# save figure
ggsave(figure_median_smooth, filename = "figures/fig_median_smooth.png",
       width = 170, height = 170 / 3, units = "mm")


## --------------------------------------------------------------------------------------------------------------------------------------------------
# choose the 11 point median smooth data
data_agg <- copy(list_of_smooths[[3]])

# get list of aggregated data
list_of_agg <- lapply(c(3, 10, 30, 120), function(z) {
  
  data_return <- atl_thin_data(data = data_agg,
                            interval = z,
                            method = "aggregate")
  
  data_return[, interval := z]
  
  return(data_return)
})

# get mean speed estimate and sd
speed_agg_smooth <- 
  lapply(list_of_agg, function(df) {
    na.omit(df)
    df[, speed := atl_get_speed(df)]
    df[, list(median = median(speed, na.rm = T),
              sd = sd(speed, na.rm = T),
              interval = first(interval))]
  })

# bind
speed_agg_smooth <- rbindlist(speed_agg_smooth)


## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------
### plot figures
fig_agg_data <-
  lapply(list_of_agg, function(df) {
    ggplot(df)+
      geom_path(data = data,
                aes(x, y),
                col = "grey20",
                size = 0.2)+
      geom_point(data = data_agg,
                 aes(x, y),
                 size = 0.2,
                 col = pal[3])+
      geom_path(aes(x,y), 
                col = pal[1])+
      geom_point(aes(x,y,
                     group = interval),
                 shape = 19,
                 col = pal[4],
                 size = 2,
                 alpha = 0.6,
                 show.legend = F)+
      ggthemes::theme_few()+
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.background = element_rect(fill = "white"))+
      coord_cartesian(ylim = c(0.6, NA))
  })


## --------------------------------------------------------------------------------------------------------------------------------------------------
# read data with errors
data_errors <- fread("data/data_errors.csv")
  
# aggregate before correction
list_of_agg <- lapply(c(3, 10, 30, 120), function(z) {
  data_return <- atl_thin_data(data = data_errors,
                            interval = z,
                            method = "aggregate")
  data_return[, interval := z]
  data_return[, speed := atl_get_speed(data_return)]
  return(data_return)
})

# get real speed
data[, speed := atl_get_speed(data)]


## --------------------------------------------------------------------------------------------------------------------------------------------------
# now plot distribution of speed
data_agg <- rbindlist(list_of_agg)


## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------
# show boxplot of speed
fig_agg_speed <-
  ggplot(data_agg)+
  geom_hline(yintercept =
               1 + quantile(data$speed, na.rm = T,
                        probs = c(0.5, 0.95)),
             lty = c(1, 2))+
  geom_errorbar(data = speed_agg_smooth,
                aes(x = factor(interval),
                    ymin = 1 + median - sd,
                    ymax = 1 + median + sd),
                width = 0.2,
                position = position_nudge(x = 0.25)) +
  geom_point(data = speed_agg_smooth,
             aes(x = factor(interval),
                 y = 1 + median),
             shape = 21,
             size = 3,
             fill = pal[3],
             position = position_nudge(x = 0.25)) +
  geom_boxplot(aes(factor(interval), 1 + speed),
               position = position_nudge(x = -0.25,),
               fill = "grey",
               alpha = 0.5,
               show.legend = F, 
               width = 0.25, 
               outlier.size = 0.2)+
  scale_y_log10(label = scales::comma,
                limits = c(NA, 1.005))+
  ggthemes::theme_few()+
  theme(axis.text.y = element_blank())+
  labs(x = "interval (s)",
       y = "speed")


## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------
# make combined figure
fig_aggregate <-
  wrap_plots(append(fig_agg_data[3:4], list(fig_agg_speed)),
             design = "AABBC")+
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold"))

# save figure
ggsave(fig_aggregate,
       filename = "figures/fig_aggregate_errors.png",
       width = 170, height = 85, units = "mm")

