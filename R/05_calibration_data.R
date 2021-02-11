## -----------------------------------------------------------------------------
# for data handling
library(data.table)
library(atlastools)

# for recursion analysis
library(recurse)

# for plotting
library(ggplot2)
library(patchwork)

# making a colour palette
pal <- RColorBrewer::brewer.pal(5, "Set1")
pal[3] <- "seagreen"


## ----install_atlastools-------------------------------------------------------
install.packages("remotes")

# installation using remotes
remotes::install_github("pratikunterwegs/atlastools")


## -----------------------------------------------------------------------------
# read and plot example data
data <- fread("data/atlas1060_allTrials_annotated.csv")
data_raw <- copy(data)


## ----echo=TRUE----------------------------------------------------------------
# plot data
fig_data_raw <-
  ggplot(data) +
  geom_path(aes(x, y),
    col = "grey", alpha = 1, size = 0.2
  ) +
  geom_point(aes(x, y),
    col = "grey", alpha = 0.2, size = 0.2
  ) +
  ggthemes::theme_few() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  coord_sf(crs = 32631)

# save figure
ggsave(fig_data_raw,
  filename = "figures/fig_calibration_raw.png",
  width = 185 / 25
)


## -----------------------------------------------------------------------------
# make a copy using the data.table copy function
data_unproc <- copy(data)


## -----------------------------------------------------------------------------
# remove inside must be set to falses
data <- atl_filter_bounds(
  data = data,
  x = "x", y = "y",
  x_range = c(645000, max(data$x)),
  remove_inside = FALSE
)


## ----echo=FALSE---------------------------------------------------------------
# plot data
fig_data_bbox <-
  ggplot() +
  geom_path(
    data = data_raw,
    aes(x, y),
    col = "grey", alpha = 0.5, size = 0.3
  ) +
  geom_point(
    data = data_raw,
    aes(x, y,
      col = x > 645000
    ),
    alpha = ifelse(data_raw$x > 645000, 1, 1),
    size = ifelse(data_raw$x > 645000, 0.3, 4),
    shape = ifelse(data_raw$x > 645000, 16, 4),
    show.legend = F
  ) +
  scale_colour_manual(values = c(pal[1], pal[3])) +
  geom_vline(
    xintercept = 645000,
    col = "grey", lty = 2
  ) +
  ggspatial::annotation_scale(location = "br") +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = 32631)

# save result
ggsave(fig_data_bbox,
  filename = "figures/fig_calib_bbox.png",
  width = 185 / 25
)


## -----------------------------------------------------------------------------
# divide by 1000, convert to integer, then convert to POSIXct
data[, time := as.integer(
  as.numeric(TIME) / 1000
)]


## -----------------------------------------------------------------------------
# add incoming and outgoing speed
data[, `:=`(
  speed_in = atl_get_speed(data,
    x = "x",
    y = "y",
    time = "time"
  ),
  speed_out = atl_get_speed(data, type = "out")
)]

# add turning angle
data[, angle := atl_turning_angle(data = data)]


## -----------------------------------------------------------------------------
# use sapply
speed_angle_thresholds <-
  sapply(data[, list(speed_in, speed_out, angle)],
    quantile,
    probs = 0.9, na.rm = T
  )


## ----echo=FALSE---------------------------------------------------------------
# plot filtered data
fig_speed_outliers <-
  ggplot() +
  geom_point(
    data = data,
    aes(x, y,
      col = speed_in < 25.6
    ),
    size = 0.3, alpha = 0.5,
    show.legend = F
  ) +
  annotate(
    geom = "rect",
    xmin = 650785, xmax = 653250,
    ymin = 5904450, ymax = 5906133,
    fill = NA, col = "grey20"
  ) +
  annotate(
    geom = "rect",
    ymin = 5901650, ymax = 5903100,
    xmin = 650000, xmax = 650600,
    fill = NA, col = "grey20"
  ) +
  scale_color_manual(values = c("grey", pal[3])) +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  ggspatial::annotation_scale(location = "tl") +
  coord_sf(crs = 32631)
# save
ggsave(fig_speed_outliers,
  filename = "figures/fig_speed_outlier.png",
  width = 170 / 25, height = 170 / 25
)


## -----------------------------------------------------------------------------
# make a copy
data_unproc <- copy(data)

# remove speed outliers
data <- atl_filter_covariates(
  data = data,
  filters = c("(speed_in < 15 & speed_out < 15)")
)

# recalculate speed and angle
data[, `:=`(
  speed_in = atl_get_speed(data,
    x = "x",
    y = "y",
    time = "time"
  ),
  speed_out = atl_get_speed(data, type = "out")
)]

# add turning angle
data[, angle := atl_turning_angle(data = data)]


## -----------------------------------------------------------------------------
# apply a 5 point median smooth, first make a copy
data_unproc <- copy(data)

# now apply the smooth
atl_median_smooth(
  data = data,
  x = "x", y = "y", time = "time",
  moving_window = 5
)


## ---- echo=FALSE--------------------------------------------------------------
# make zoomed in figures
fig_smooth <-
  ggplot() +
  geom_path(
    data = data_raw[!data_unproc, on = c("x", "y")],
    aes(x, y),
    col = "grey90",
    size = 0.1
  ) +
  geom_point(
    data = data_raw[!data_unproc, on = c("x", "y")],
    aes(x, y),
    col = "grey",
    shape = 4,
    size = 0.4
  ) +
  geom_point(
    data = data,
    aes(x, y),
    col = pal[3],
    shape = 1,
    stroke = 1,
    alpha = 0.5
  ) +
  coord_cartesian(
    xlim = c(650785, 653250),
    ylim = c(5904450, 5906133),
    expand = F
  ) +
  ggthemes::theme_few() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  ggspatial::annotation_scale(location = "br")

ggsave(fig_smooth,
  filename = "figures/fig_calib_median_smooth.png",
  width = 90 / 25, height = 90 / 25
)


## -----------------------------------------------------------------------------
# save a copy
data_unproc <- copy(data)

# remove columns we don't need
data <- data[, setdiff(
  colnames(data),
  c("tID", "Timestamp", "id", "TIME", "UTCtime")
),
with = FALSE
]

# thin to a 30s interval
data_thin <- atl_thin_data(
  data = data,
  interval = 30,
  method = "aggregate",
  id_columns = "TAG"
)


## ----echo=FALSE---------------------------------------------------------------
# make zoomed in figures
fig_smooth_thin <-
  ggplot() +
  geom_path(
    data = data_raw[!data_unproc, on = c("x", "y")],
    aes(x, y),
    col = "grey90",
    size = 0.1
  ) +
  geom_point(
    data = data_raw[!data_unproc, on = c("x", "y")],
    aes(x, y),
    col = "grey",
    shape = 4,
    size = 0.4
  ) +
  geom_point(
    data = data_thin,
    aes(x, y, size = count),
    col = pal[4],
    alpha = 0.8,
    shape = 0, show.legend = F
  ) +
  geom_point(
    data = data_unproc,
    aes(x, y),
    col = pal[3],
    shape = 16
  ) +
  scale_size(range = c(2, 6)) +
  coord_cartesian(
    xlim = c(650785, 653250),
    ylim = c(5904450, 5906133),
    expand = F
  ) +
  ggthemes::theme_few() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  ggspatial::annotation_scale(location = "br")

ggsave(fig_smooth_thin,
  filename = "figures/fig_calib_smooth_thin.png",
  width = 90 / 25, height = 90 / 25, dpi = 300
)


## ----echo=FALSE---------------------------------------------------------------
# make combined walkthrough figure
figure_walkthrough <-
  wrap_plots(
    list(
      fig_speed_outliers, fig_smooth_thin
    ),
    design = "AB"
  )

# # save combined figure
ggsave(figure_walkthrough,
  filename = "figures/fig_walkthrough.png",
  height = 90, width = 170, units = "mm"
)


## -----------------------------------------------------------------------------
library(stringi)
data_res <- data_unproc[stri_detect(tID, regex = "(WP)")]


## -----------------------------------------------------------------------------
# get centroid
data_res_summary <- data_res[, list(
  x_median = median(x),
  y_median = median(y),
  t_median = median(time)
),
by = "tID"
]

# now get times 10 mins before and after
data_res_summary[, `:=`(
  t_min = t_median - (10 * 60),
  t_max = t_median + (10 * 60)
)]

# make a list of positions 10min before and after
wp_data <- mapply(function(l, u, mx, my) {
  tmp_data <- data_unproc[inrange(time, l, u)]
  tmp_data[, distance := sqrt((mx - x)^2 + (my - y)^2)]

  # keep within 50
  tmp_data <- tmp_data[distance <= 50, ]

  # get duration
  return(diff(range(tmp_data$time)))
}, data_res_summary$t_min, data_res_summary$t_max,
data_res_summary$x_median, data_res_summary$y_median,
SIMPLIFY = TRUE
)


## -----------------------------------------------------------------------------
# get 4 column data
data_for_patch <- data_thin[, list(x, y, time, TAG)]

# get recurse data for a 10m radius
recurse_stats <- getRecursions(data_for_patch,
  radius = 50, timeunits = "mins"
)

# assign to recurse data
data_for_patch[, res_time := recurse_stats$residenceTime]

# save recurse data
fwrite(data_for_patch, file = "data/data_calib_for_patch.csv")


## -----------------------------------------------------------------------------
# assign id as tag
data_for_patch[, id := as.character(TAG)]

# on known residence points
patch_res_known <- atl_res_patch(data_for_patch[res_time >= 5, ],
  buffer_radius = 5,
  lim_spat_indep = 50,
  lim_time_indep = 5,
  min_fixes = 3
)


## -----------------------------------------------------------------------------
# for the known and unkniwn patches
patch_sf_data <- atl_patch_summary(patch_res_known,
  which_data = "spatial",
  buffer_radius = 20
)

# assign crs
sf::st_crs(patch_sf_data) <- 32631

# get summary data
patch_summary_data <- atl_patch_summary(patch_res_known,
  which_data = "summary"
)


## -----------------------------------------------------------------------------
# read griend and hut
griend <- sf::st_read("data/griend_polygon/griend_polygon.shp")
hut <- sf::st_read("data/griend_hut.gpkg")


## ----echo=FALSE---------------------------------------------------------------
# patch with residence points and all patches
fig_basic_residence <-
  ggplot() +
  geom_sf(data = griend, fill = "antiquewhite") +
  geom_point(
    data = data_res_summary,
    aes(x_median, y_median),
    size = 10,
    shape = 2,
    stroke = 1,
    col = pal[4],
    alpha = 1
  ) +
  geom_sf(
    data = hut,
    size = 10, shape = 2,
    stroke = 1,
    col = pal[1]
  ) +
  geom_path(
    data = data_for_patch,
    aes(x, y),
    show.legend = F,
    size = 0.3,
    col = "grey"
  ) +
  geom_point(
    data = data_for_patch,
    aes(x, y),
    show.legend = F,
    shape = 1, col = "grey50"
  ) +
  geom_sf(
    data = patch_sf_data[patch_sf_data$duration >= 5 * 60, ],
    fill = pal[3],
    col = pal[3],
    alpha = 0.3
  ) +
  scale_colour_manual(values = c("grey50", pal[3])) +
  scale_shape_manual(values = c(4, 19)) +
  ggspatial::annotation_scale(location = "tl") +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "aliceblue")
  ) +
  coord_sf(
    crs = 32631,
    expand = F,
    ylim = c(5901650, 5903100),
    xlim = c(650000, 650600)
  )


## ----echo=FALSE---------------------------------------------------------------
ggsave(fig_basic_residence,
  filename = "figures/fig_calib_residence.png",
  height = 170, width = 80, units = "mm"
)


## -----------------------------------------------------------------------------
# get known patch summary
data_res <- data_unproc[stringi::stri_detect(tID, regex = "(WP)"), ]

# get waypoint summary
patch_summary_real <- data_res[, list(
  nfixes_real = .N,
  x_median = round(median(x), digits = -2),
  y_median = round(median(y), digits = -2)
),
by = "tID"
]

# add real duration
patch_summary_real[, duration_real := wp_data]

# round median coordinate for inferred patches
patch_summary_inferred <-
  patch_summary_data[
    ,
    c(
      "x_median", "y_median",
      "nfixes", "duration", "patch"
    )
  ][, `:=`(
    x_median = round(x_median, digits = -2),
    y_median = round(y_median, digits = -2)
  )]

# join with respatch summary
patch_summary_compare <-
  merge(patch_summary_real,
    patch_summary_inferred,
    on = c("x_median", "y_median"),
    all.x = TRUE, all.y = TRUE
  )

# drop nas
patch_summary_compare <- na.omit(patch_summary_compare)

# drop patch around WP080
patch_summary_compare <- patch_summary_compare[tID != "WP080", ]


## -----------------------------------------------------------------------------
# get linear model
model_duration <- lm(duration_real ~ duration,
  data = patch_summary_compare
)

# get R2
summary(model_duration)

# write to file
writeLines(
  text = capture.output(
    summary(model_duration)
  ),
  con = "data/model_output_residence_patch.txt"
)


## ----echo=FALSE---------------------------------------------------------------
# make figure comparing different methods
figure_lm_duration <-
  ggplot() +
  geom_point(
    data = patch_summary_compare,
    aes(
      duration,
      duration_real
    ),
    show.legend = F,
    size = 2, col = "grey"
  ) +
  geom_smooth(
    data = patch_summary_compare,
    aes(
      duration,
      duration_real
    ),
    se = F,
    col = pal[1],
    lwd = 0.5,
    method = "lm",
    show.legend = F
  ) +
  annotate(
    geom = "text",
    x = 600, y = 1020,
    size = 5,
    label = "R^2 == 0.908",
    parse = T, family = "Arial"
  ) +
  scale_x_continuous(
    breaks = 60 * seq(3, 18, 2),
    labels = seq(3, 18, 2)
  ) +
  scale_y_continuous(
    breaks = 60 * seq(5, 20, 2),
    labels = seq(5, 20, 2)
  ) +
  coord_fixed(
    expand = T,
    ratio = 1.115
    #             # xlim = c(400, 1050),
    #             ylim = c(250, 1050)
  ) +
  geom_abline(
    slope = 1, lty = 2,
    col = "black"
  ) +
  ggthemes::theme_few() +
  labs(
    x = "inferred duration (min)",
    y = "real duration (min)"
  )

ggsave(figure_lm_duration,
  filename = "figures/fig_calib_lm_duration.png",
  width = 80 / 25, height = 80 / 25
)


## ---- eval=TRUE---------------------------------------------------------------
cat(
  readLines(
    con = "data/model_output_residence_patch.txt",
    encoding = "UTF-8"
  ),
  sep = "\n"
)


## ----echo=FALSE---------------------------------------------------------------
# wrap together
figure_res_patch <-
  wrap_plots(list(
    fig_speed_outliers,
    fig_smooth_thin,
    fig_basic_residence
  ),
  design = "AC\nAC\nBC\nBC"
  ) +
    plot_annotation(
      tag_levels = "a",
      tag_prefix = "(",
      tag_suffix = ")"
    ) &
    theme(plot.tag = element_text(face = "bold"))

# save figure
ggsave(figure_res_patch,
  filename = "figures/fig_06_calib_residence_patch.png",
  height = 170, width = 170, units = "mm"
)

