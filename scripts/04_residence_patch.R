## ----prep_libs_02_01----------------------------------------------------------
library(data.table)
library(atlastools)
library(ggplot2)
library(patchwork)

# for residence time
library(recurse)

# prepare a palette
pal <- RColorBrewer::brewer.pal(4, "Set1")


## -----------------------------------------------------------------------------
# read patch data
data <- fread("data/data_lt_plot.csv")
# filter for illustrative plots
data <- data[id == 3 & growth == 0.01 & type == "exploit", ]

data[, stationary := (
  c(0, diff(x)) == 0 & c(0, diff(y)) == 0
), by = "interval"]

# get cell r from agent
data_r <- copy(data[interval == 1, ])
data_r <- data_r[, c("x_wrap", "y_wrap") := list(
  round(x_wrap),
  round(y_wrap)
)]
data_r <- data_r[, list(
  cell_r = mean(cell_r)
), by = c("x_wrap", "y_wrap")]


## plot initial figure
fig_a <-
  ggplot(
    data[interval == 1, ],
    aes(
      x_wrap, y_wrap
    )
  ) +
  geom_tile(
    data = data_r,
    aes(
      x_wrap, y_wrap,
      fill = cell_r
    ),
    alpha = 0.5
  ) +
  geom_path(
    aes(
      group = interval
    ),
    col = "grey",
    size = 0.3
  ) +
  geom_point(
    aes(
      shape = stationary
    ),
    alpha = 0.8
  ) +
  scale_fill_distiller(
    palette = "YlGn",
    direction = 1,
    breaks = c(0.1, 0.5, 0.99),
    labels = c("Low", "Med.", "High"),
    name = "Productivity"
  ) +
  scale_shape_manual(
    values = c(3, 16),
    guide = "none"
  ) +
  # facet_wrap(~interval)+
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "white", colour = NA
    ),
    legend.position = c(0.2, 0.8),
    legend.key.width = unit(1, "mm"),
    legend.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.text = element_blank(),
    strip.background = element_blank()
  ) +
  coord_equal() +
  labs(
    colour = "Productivity"
  )

# flip lengthwise
# setnames(data, old = c("x", "y"), new = c("y", "x"))

# # do recurse
# data_recurse <- getRecursions(data[, list(x_wrap, y_wrap, t, id)],
#   radius = 1
# )

# assign residence time
# data[, residence_time := data_recurse$residenceTime]


## ----echo=FALSE---------------------------------------------------------------
# restime by position
# fig_res_a <-
#   ggplot()+
#   geom_path(data = data,
#             aes(x, y),
#             lwd = 0.2,
#             col = "grey")+
#   geom_point(data = data,
#              aes(x, y,
#                  group = NA
#                  ),
#              size = 0.3,
#              show.legend = F,
#              alpha = 0.2,
#              col = "grey20")+
#   coord_equal()+
#   scale_colour_manual(values = c("grey", pal[3]))+
#   ggthemes::theme_few()+
#   theme(axis.text = element_blank(),
#         axis.title = element_blank())

## prepare for residence patch methods
# save an original copy
data_org <- copy(data)

# an id is required
data[, id := as.character(id)]
data[, id := "test"]

# set name of time
setnames(data, "t", "time")
# remove unwrapped x, y and replace with wrapped coordinates
data[, c("x", "y") := NULL]
setnames(data, c("x_wrap", "y_wrap"), c("x", "y"))

## split the data by interval
data <- split(data, by = "interval")

## -----------------------------------------------------------------------------
# make residence patch
patches <- Map(
  data,
  f = atl_res_patch,
  buffer_radius = 0.5,
  lim_spat_indep = 5,
  lim_time_indep = 10,
  min_fixes = 5,
  summary_variables = c("cell_r"),
  summary_functions = c("mean", "sd")
)

# get spatial representation
patches_sf <- Map(
  patches,
  f = atl_patch_summary,
  which_data = "spatial",
  buffer_radius = 0.5
)

# get summary data
patches_summary <- Map(
  patches,
  f = atl_patch_summary,
  which_data = "summary"
)

## collect patch data
patches_summary <- Map(
  patches_summary, names(patches_summary),
  f = function(df, i) {
    df$interval <- i
    df
  }
)
patches_summary <- rbindlist(patches_summary)
setnames(patches_summary, ".", "mean_r")

patches_sf <- Map(
  patches_sf, names(patches_sf),
  f = function(df, i) {
    df$interval <- i
    df
  }
)
patches_sf <- rbindlist(patches_sf)
patches_sf <- sf::st_as_sf(patches_sf, sf_column_name = "polygons")

## ----echo=FALSE---------------------------------------------------------------
# plot_patches <-
fig_res_patches <-
  ggplot() +
  geom_path(
    data = data_org[interval == 1],
    aes(x_wrap, y_wrap),
    lwd = 0.3,
    col = "grey"
  ) +
  geom_point(
    data = data_org[interval == 1],
    aes(
      x_wrap, y_wrap,
      shape = stationary,
      col = stationary
    ),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  geom_sf(
    data = patches_sf[patches_sf$interval == 1, ],
    aes(fill = factor(patch)),
    alpha = 0.4,
    lwd = 0.2,
    colour = "grey",
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("black", "black")
  ) +
  scale_fill_viridis_d(
    option = "D"
  ) +
  scale_shape_manual(
    values = c(3, 16)
  ) +
  coord_sf(
    # xlim = c(-3, NA)
  ) +
  facet_wrap(~interval) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "white", colour = NA
    ),
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.text = element_blank(),
    strip.background = element_blank()
  )

# # res time by time
# fig_res_inset <-
#   ggplot() +
#   geom_rect(
#     data = patch_summary,
#     aes(
#       xmin = time_start,
#       xmax = time_end,
#       ymin = 0.0, ymax = 0.2,
#       fill = (patch)
#     ),
#     col = "grey",
#     lwd = 0.1,
#     alpha = 0.5,
#     show.legend = F
#   ) +
#   geom_path(
#     data = data,
#     aes(time, residence_time,
#       group = NA
#     ),
#     lwd = 0.4,
#     col = "grey20",
#     show.legend = F
#   ) +
#   geom_hline(
#     yintercept = 0.04,
#     col = "grey20",
#     lwd = 0.5,
#     lty = 2
#   ) +
#   scale_fill_distiller(palette = "Paired", direction = -1) +
#   scale_colour_manual(values = c("grey20", "darkgreen")) +
#   ggthemes::theme_few() +
#   theme(
#     axis.text = element_blank(),
#     plot.title = element_text(
#       face = "bold"
#     )
#   ) +
#   coord_cartesian(expand = F) +
#   labs(
#     x = "time",
#     y = "res. time",
#     title = "(b)"
#   )


## ----echo=FALSE---------------------------------------------------------------
# fig_residence_patch <-
# fig_res_patches +
# annotation_custom(
#   grob = ggplotGrob(
#     fig_res_inset
#   ),
#   xmin = -3.5,
#   xmax = 4,
#   ymin = 6,
#   ymax = 12.25
# )

fig_residence_patch <-
  wrap_plots(
    fig_a,
    fig_res_patches
  ) +
    plot_annotation(
      tag_levels = "a",
      tag_suffix = ")",
      tag_prefix = "("
    ) &
    theme(
      plot.tag.position = c(0.1, 0.95),
      plot.tag = element_text(face = "bold")
    )

# save the figure
ggsave(fig_residence_patch,
  filename = "figures/fig_06.png",
  height = 170, width = 150, units = "mm"
)
