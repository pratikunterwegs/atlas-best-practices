## -----------------------------------------------------------------------------
# to handle movement data
library(data.table)
library(atlastools)

# to plot
library(ggplot2)
library(patchwork)


## -----------------------------------------------------------------------------
# source helper functions
source("R/helper_functions.R")


## ----read_sim_data------------------------------------------------------------
# read in the data
data <- fread("data/data_sim.csv")[5000:10000, ]


## ----add_outlier, eval=FALSE--------------------------------------------------
## # make a copy
## # data_copy <- copy(data)
## #
## # # add a prolonged spike or reflection to 300 positions
## # data_copy[500:800, `:=`(
## #   x = x + 0.25,
## #   y = y + 0.25
## # )]
## #
## # # add normal error
## # data_copy[, `:=`(
## #   x = do_add_error(x, std_dev = 0.01),
## #   y = do_add_error(y, std_dev = 0.005)
## # )]
## #
## # # add 100 outliers
## # data_copy <- do_add_outliers(data_copy, p_data = 0.005, std_dev = 0.1)


## ----eval=FALSE---------------------------------------------------------------
## # fwrite(data_copy, file = "data/data_errors.csv")


## -----------------------------------------------------------------------------
data_copy <- fread("data/data_errors.csv")


## -----------------------------------------------------------------------------
# define a four colour palette
pal <- RColorBrewer::brewer.pal(5, "Set1")
pal[3] <- "seagreen"


## ----echo=FALSE---------------------------------------------------------------
# make figure of canonical data with added errors
# figure_raw <-
#   ggplot()+
#   geom_point(data = data_copy,
#             aes(x, y),
#             col = "grey",
#             alpha = 1,
#             shape = 4,
#             size = 0.2)+
#   geom_path(data = data,
#             aes(x, y),
#             col = "grey20",
#             alpha = 1)+
#   ggthemes::theme_few()+
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         plot.title = element_text(
#           face = "bold",
#           margin = margin(t = 30, b = -30),
#           hjust = 0.1
#         ))+
#   coord_equal()+
#   labs(colour = NULL)+
#   labs(
#     title = "(a)"
#   )


## ----remove_outside_bbox------------------------------------------------------
# remove positions outside a bounding box
# NB: set remove_inside to FALSE
data_inside_bbox <- atl_filter_bounds(
  data = data_copy,
  y_range = c(0.5, 1),
  remove_inside = FALSE
)


## ----echo=FALSE---------------------------------------------------------------
# plot data inside and outside bbox
fig_filter_bounds <-
  ggplot() +
  geom_point(
    data = data_inside_bbox,
    aes(x, y),
    col = pal[3],
    alpha = 1, size = 0.2
  ) +
  geom_point(
    data = data_copy[!data_inside_bbox,
      on = c("x", "y")
    ],
    aes(x, y),
    col = "grey",
    shape = 4,
    size = 0.2
  ) +
  geom_path(
    data = data,
    aes(x, y),
    col = "grey20",
    alpha = 1
  ) +
  geom_hline(
    yintercept = c(0.5, 1),
    col = "grey",
    lty = 2
  ) +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  theme(plot.background = element_rect(fill = NA)) +
  coord_equal(expand = T)


## ----example_remove_outliers--------------------------------------------------
# get speed and turning angle
data_copy[, `:=`(
  in_speed = atl_get_speed(data_copy,
    type = "in"
  ),
  out_speed = atl_get_speed(data_copy,
    type = "out"
  ),
  angle = atl_turning_angle(data_copy)
)]


## -----------------------------------------------------------------------------
# get 90 and 95 percentile of speed and turning angle
sapply(data_copy[, c("in_speed", "angle")], function(z) {
  quantile(z, probs = c(0.1, 0.9, 0.95), na.rm = TRUE)
})


## -----------------------------------------------------------------------------
# filter the copy by the 95th percentile
data_filtered <- atl_filter_covariates(data_copy,
  filters = c("(in_speed < 0.025 & out_speed < 0.025) | angle < 35")
)


## ----echo=FALSE---------------------------------------------------------------
# data plot
fig_outlier_remove <-
  ggplot() +
  geom_path(
    data = data_copy,
    aes(x, y),
    col = "grey",
    lwd = 0.2
  ) +
  geom_point(
    data = data_copy[500:800, ],
    aes(x, y),
    # size = 0.5,
    alpha = 0.6,
    shape = 4, col = "black"
  ) +
  geom_point(
    data = data_copy[!data_copy[500:800, ],
      on = c("x", "y")
    ],
    aes(x, y,
      col = (in_speed >= 0.03 & out_speed >= 0.03),
      shape = (in_speed >= 0.03 & out_speed >= 0.03),
      size = (in_speed >= 0.03 & out_speed >= 0.03)
    ),
    show.legend = F,
    alpha = 1
  ) +
  geom_path(
    data = data,
    aes(x, y),
    col = "grey20",
    alpha = 1
  ) +
  scale_color_manual(values = c(pal[3], "grey")) +
  scale_shape_manual(values = c(1, 4)) +
  scale_size_manual(
    values = c(0.2, 2)
  ) +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_equal() +
  theme(plot.background = element_rect(fill = NA))


## -----------------------------------------------------------------------------
# attempt to remove reflections
data_no_reflection <- atl_remove_reflections(data_filtered,
  point_angle_cutoff = 10,
  reflection_speed_cutoff = 0.025
)
# get reflections
reflection <- data_filtered[!data_no_reflection,
  on = c("x", "y")
]
reflection <- na.omit(reflection)


## ----echo=FALSE---------------------------------------------------------------
# wrap plot
figure_01 <-
  wrap_plots(
    fig_filter_bounds,
    fig_outlier_remove
  ) +
    plot_annotation(
      tag_levels = "a",
      tag_prefix = "(",
      tag_suffix = ")"
    ) &
    theme(plot.tag = element_text(face = "bold"))

# save figure
ggsave(figure_01,
  filename = "figures/fig_02_filtering_data.png",
  width = 170, height = 150, units = "mm"
)


## -----------------------------------------------------------------------------
fwrite(data_no_reflection, file = "data/data_no_reflection.csv")

