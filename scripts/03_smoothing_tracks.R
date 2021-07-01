## -----------------------------------------------------------------------------
# prep libs
library(data.table)
library(atlastools)
library(ggplot2)
library(patchwork)

# define a four colour palette
pal <- RColorBrewer::brewer.pal(5, "Set1")
pal[3] <- "seagreen"


## ----read_sim_data_2----------------------------------------------------------
# read in the data and set the window size variable
data <- fread("data/data_sim.csv")[5000:10000, ]
data[, window_size := NA]
data$speed_in <- atl_get_speed(data)

# data with small scale errors but no reflections or outliers
data_errors <- fread("data/data_no_reflection.csv")
data_errors[, window_size := 0]


## -----------------------------------------------------------------------------
# smooth the data over four K values
list_of_smooths <- lapply(c(5, 11, 21, 101), function(K) {
  data_copy <- copy(data_errors)

  data_copy <- atl_median_smooth(
    data = data_copy,
    x = "x",
    y = "y",
    time = "time",
    moving_window = K
  )

  data_copy[, window_size := K]
})


## -----------------------------------------------------------------------------
fwrite(list_of_smooths[[2]], file = "data/data_smooth.csv")


## -----------------------------------------------------------------------------
# bind list after offset
data_plot <- mapply(function(df, offset) {
  df <- copy(df)
  df[, x := x + offset]
  df$speed_in <- atl_get_speed(df)
  return(df)
}, list_of_smooths, seq(0.4, 1.25, length.out = 4),
SIMPLIFY = F
)

data_plot <- rbindlist(data_plot)

# add offset to data errors
data_errors <- lapply(
  c(0, seq(0.4, 1.25, length.out = 4)),
  function(offset) {
    df <- copy(data_errors)
    df[, x := x + offset]
    df[, offset := offset]
  }
)

# bind list
data_errors <- rbindlist(data_errors)


## -----------------------------------------------------------------------------
pal2 <- RColorBrewer::brewer.pal(4, "RdPu")[c(2, 4)]

fig_hist_smooth <-
  ggplot() +
  geom_histogram(
    data = data_errors[offset == 0, ],
    aes(
      x = in_speed,
      fill = "errors"
    ),
    bins = 80,
    alpha = 1
  ) +
  geom_histogram(
    data = data_plot[window_size %in% c(5, 21)],
    aes(
      x = speed_in,
      fill = as.factor(window_size),
      group = window_size
    ),
    bins = 80
  ) +
  geom_histogram(
    data = data,
    aes(
      x = speed_in,
      fill = "real"
    ),
    # col = pal[1],
    alpha = 0.8,
    bins = 80
  ) +
  scale_x_log10() +
  scale_fill_manual(
    values = c(
      "real" = alpha("grey20", .8),
      "errors" = "grey",
      "5" = pal2[1],
      "21" = pal2[2]
    ),
    labels = c(
      "real" = "real track",
      "errors" = "track w/\nerrors",
      "5" = "K = 5",
      "21" = "K = 21"
    )
  ) +
  coord_cartesian(xlim = c(1e-5, .05)) +
  theme_test(base_family = "sans") +
  theme(
    legend.position = c(0.2, 0.65),
    axis.text = element_blank(),
    # axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(
      face = "bold"
    ),
    plot.background = element_blank()
  ) +
  labs(
    x = "Speed [log10 scale]",
    fill = "Smoothing",
    title = "(f)"
  )


## ----echo=FALSE---------------------------------------------------------------
# prepare data to plot
# make list of data to plot
figure_median_smooth <-
  ggplot() +
  geom_path(
    data = data_errors,
    aes(x, y,
      group = offset
    ),
    col = "grey60",
    size = 0.1
  ) +
  geom_point(
    data = data_errors[offset == 0, ],
    aes(x, y),
    col = pal[3],
    size = 0.2
  ) +
  geom_path(
    data = data,
    aes(x, y),
    col = "grey20",
    lwd = 0.5
  ) +
  geom_path(
    data = data_plot,
    aes(x, y,
      group = window_size,
      col = factor(window_size)
    ),
    show.legend = F,
    lwd = 0.35
  ) +
  coord_equal(
    expand = F,
    ylim = c(0.6, 0.83),
    xlim = c(NA, 2.3),
    ratio = 1.75
  ) +
  annotate(
    geom = "text",
    x = c(0.75, seq(1.1, 2, length.out = 4)),
    y = 0.81,
    label = sprintf("(%s)", letters[seq(5)]),
    fontface = "bold"
  ) +
  scale_colour_manual(
    values = c(colorspace::sequential_hcl(3,
      l = 40, palette = "PuBu",
      rev = T
    ), "darkred")
  ) +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# save figure
ggsave(figure_median_smooth,
  filename = "figures/fig_04.png",
  width = 170, height = 170 / 3, units = "mm"
)


## -----------------------------------------------------------------------------
# choose the 11 point median smooth data
data_smooth <- fread("data/data_smooth.csv")

# get list of aggregated data
list_of_agg <- lapply(c(3, 10, 30, 120), function(z) {
  data_return <- atl_thin_data(
    data = data_smooth,
    interval = z,
    method = "aggregate"
  )

  data_return[, interval := z]

  return(data_return)
})

# get mean speed estimate and sd
speed_agg_smooth <-
  lapply(list_of_agg, function(df) {
    na.omit(df)
    df[, speed := atl_get_speed(df)]
    # df[, list(
    #   median = median(speed, na.rm = T),
    #   sd = sd(speed, na.rm = T),
    #   interval = first(interval)
    # )]
  })

# bind
speed_agg_smooth <- rbindlist(speed_agg_smooth)


## ----echo=FALSE---------------------------------------------------------------
# prepare data
data_agg_smooth <- copy(list_of_agg[[3]]) # 30s aggregate
### plot figures
fig_agg_data_smooth <-
  ggplot() +
  geom_point(
    data = data_smooth,
    size = 0.2,
    aes(
      x, y,
      colour = "smooth",
      shape = "smooth"
    )
  ) +
  geom_path(
    data = data_agg_smooth,
    aes(
      x, y,
      colour = "thin",
      shape = "thin"
    ),
    lwd = 0.2
  ) +
  geom_point(
    data = data_agg_smooth,
    aes(
      x, y,
      group = interval,
      shape = "thin",
      col = "thin"
    ),
    # shape = 0,
    size = 2,
    # col = pal[4],
    alpha = 1
    # show.legend = F
  ) +
  scale_colour_manual(
    values = c(
      thin = pal[4],
      smooth = pal[3]
    ),
    labels = c(
      thin = "Thinned data",
      smooth = "Smoothed,\nfiltered data"
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      thin = 0,
      smooth = 16
    ),
    labels = c(
      thin = "Thinned data",
      smooth = "Smoothed,\nfiltered data"
    ),
    name = NULL
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = c(0, 1)
      )
    )
  ) +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  theme(
    legend.position = "top"
  ) +
  coord_cartesian(ylim = c(0.6, NA))


## -----------------------------------------------------------------------------
# read data with errors
data_errors <- fread("data/data_errors.csv")

# aggregate before correction
list_of_agg_errors <- lapply(c(3, 10, 30, 120), function(z) {
  data_return <- atl_thin_data(
    data = data_errors,
    interval = z,
    method = "aggregate"
  )
  data_return[, interval := z]
  data_return[, speed := atl_get_speed(data_return)]
  return(data_return)
})

# get real speed
data[, speed := atl_get_speed(data)]


## ----echo=FALSE---------------------------------------------------------------
# prepare data
data_agg_error <- copy(list_of_agg_errors[[3]]) # 30s aggregate
### plot figures
fig_agg_data_error <-
  ggplot() +
  geom_point(
    data = data_errors,
    size = 0.1,
    aes(
      x, y,
      col = "errors",
      shape = "errors"
    )
  ) +
  geom_path(
    data = data_agg_error,
    aes(
      x, y,
      col = "agg"
    )
  ) +
  geom_point(
    data = data_agg_error,
    aes(
      x, y,
      col = "agg",
      shape = "agg",
      group = interval
    ),
    size = 2,
    alpha = 1
  ) +
  scale_colour_manual(
    values = c(
      agg = pal[4],
      errors = "grey"
    ),
    labels = c(
      agg = "Thinned data",
      errors = "Unfiltered data"
    ),
    breaks = c("errors", "agg"),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      agg = 0,
      errors = 16
    ),
    labels = c(
      agg = "Thinned data",
      errors = "Unfiltered data"
    ),
    breaks = c("errors", "agg"),
    name = NULL
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = c(0, 1)
      )
    )
  ) +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  theme(
    legend.position = "top"
  ) +
  coord_cartesian(
    ylim = c(0.6, NA)
  )


## -----------------------------------------------------------------------------
# now plot distribution of speed
data_agg <- rbindlist(list_of_agg_errors)


## ----echo=FALSE---------------------------------------------------------------
# show boxplot of speed
fig_agg_speed <-
  ggplot() +
  geom_hline(
    yintercept =
      1 + quantile(data$speed,
        na.rm = T,
        probs = c(0.5, 0.95)
      ),
    lty = c(1, 2)
  ) +
  geom_boxplot(
    data = speed_agg_smooth,
    aes(
      x = as.factor(interval),
      y = 1 + speed,
      fill = "aggsmooth"
    ),
    size = 0.3,
    width = 0.25,
    outlier.size = 0.2,
    position = position_nudge(x = 0.15)
  ) +
  geom_boxplot(
    data = data_agg,
    aes(
      factor(interval), 1 + speed,
      fill = "aggunfil"
    ),
    position = position_nudge(x = -0.15, ),
    size = 0.3,
    alpha = 0.5,
    show.legend = F,
    width = 0.25,
    outlier.size = 0.2
  ) +
  scale_y_log10(
    label = scales::comma,
    limits = c(NA, 1.005)
  ) +
  scale_fill_manual(
    values = c(
      aggsmooth = pal[3],
      aggunfil = "grey"
    ),
    labels = c(
      aggunfil = "Unfiltered data",
      aggsmooth = "Smoothed,\nfiltered data"
    ),
    breaks = c("aggunfil", "aggsmooth"),
    name = NULL
  ) +
  ggthemes::theme_few() +
  theme(
    axis.text.y = element_blank(),
    legend.position = "top"
  ) +
  labs(
    x = "Thinning interval (s)",
    y = "Speed"
  )


## ----echo=FALSE---------------------------------------------------------------
# make combined figure
fig_aggregate <-
  wrap_plots(
    fig_agg_data_smooth, fig_agg_data_error, fig_agg_speed,
    design = "AABBCC"
  ) +
    plot_annotation(
      tag_levels = "a",
      tag_prefix = "(",
      tag_suffix = ")"
    ) &
    theme(
      legend.text = element_text(
        size = 6
      ),
      plot.tag = element_text(face = "bold")
    )

# save figure
ggsave(fig_aggregate,
  filename = "figures/fig_05.png",
  width = 170, height = 85, units = "mm"
)
