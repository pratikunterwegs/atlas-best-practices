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
data <- fread("data/data_for_res_patch.csv")

# an id is required
data[, id := "test"]

# flip lengthwise
setnames(data, old = c("x", "y"), new = c("y", "x"))

# do recurse
data_recurse <- getRecursions(data[, list(x, y, time, id)],
  radius = 1
)

# assign residence time
data[, residence_time := data_recurse$residenceTime]


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


## -----------------------------------------------------------------------------
# make residence patch
patch <- atl_res_patch(data,
  buffer_radius = 0.1,
  lim_spat_indep = 1,
  lim_time_indep = 5
)



# get spatial representation
patch_sf <- atl_patch_summary(
  patch_data = patch,
  which_data = "spatial",
  buffer_radius = 0.15
)

# get summary data
patch_summary <- atl_patch_summary(
  patch_data = patch,
  which_data = "summary"
)


## ----echo=FALSE---------------------------------------------------------------
# plot_patches <-
fig_res_patches <-
  ggplot() +
  geom_sf(
    data = patch_sf,
    aes(fill = patch),
    colour = "grey20",
    alpha = 0.5,
    lwd = 0.2,
    show.legend = FALSE
  ) +
  geom_path(
    data = data,
    aes(x, y),
    lwd = 0.2,
    col = "grey20"
  ) +
  geom_point(
    data = data,
    aes(x, y),
    size = 0.4,
    col = "grey20",
    alpha = 0.2
  ) +
  scale_colour_manual(values = c("grey", pal[3])) +
  geom_path(
    data = patch_summary,
    aes(x_median + 2, y_median),
    col = "grey0",
    size = 0.5
  ) +
  geom_point(
    data = patch_summary,
    aes(x_median + 2,
      y_median,
      fill = patch
    ),
    col = "grey20",
    size = 4,
    stroke = 1,
    shape = 21,
    show.legend = F
  ) +
  scale_fill_distiller(palette = "Paired") +
  coord_sf(
    xlim = c(-3, NA)
  ) +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      margin = margin(t = 30, b = -30),
      hjust = 0.925
    )
  ) +
  labs(
    title = "(a)"
  )

# res time by time
fig_res_inset <-
  ggplot() +
  geom_rect(
    data = patch_summary,
    aes(
      xmin = time_start,
      xmax = time_end,
      ymin = 0.0, ymax = 0.2,
      fill = (patch)
    ),
    col = "grey",
    lwd = 0.1,
    alpha = 0.5,
    show.legend = F
  ) +
  geom_path(
    data = data,
    aes(time, residence_time,
      group = NA
    ),
    lwd = 0.4,
    col = "grey20",
    show.legend = F
  ) +
  geom_hline(
    yintercept = 0.04,
    col = "grey20",
    lwd = 0.5,
    lty = 2
  ) +
  scale_fill_distiller(palette = "Paired", direction = -1) +
  scale_colour_manual(values = c("grey20", "darkgreen")) +
  ggthemes::theme_few() +
  theme(
    axis.text = element_blank(),
    plot.title = element_text(
      face = "bold"
    )
  ) +
  coord_cartesian(expand = F) +
  labs(
    x = "time",
    y = "res. time",
    title = "(b)"
  )


## ----echo=FALSE---------------------------------------------------------------
fig_residence_patch <-
  fig_res_patches +
  annotation_custom(
    grob = ggplotGrob(
      fig_res_inset
    ),
    xmin = -3.5,
    xmax = 4,
    ymin = 6,
    ymax = 12.25
  )

# save the figure
ggsave(fig_residence_patch,
  filename = "figures/fig_06.png",
  height = 170, width = 150, units = "mm"
)
