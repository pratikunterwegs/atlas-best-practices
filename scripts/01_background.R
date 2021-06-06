#' ---
#' output: html_document
#' editor_options:
#'   chunk_output_type: console
#' ---
#'
#' # Code to Prepare Example Data
#' 
#' 
#' 
## ----install_package----------------------------------------------------------
# use either devtools or remotes to install
install.packages("devtools")

# installation using devtools
devtools::install_github("pratikunterwegs/atlastools")


## ----prep_libs_01_02----------------------------------------------------------
# load smoove and datatable
library(smoove)
library(data.table)
library(scales)

# source helper functions
source("R/helper_functions.R")


## ----save_data----------------------------------------------------------------
data <- do_smoove_data()
# save simulated data
fwrite(data, "data/data_sim.csv")


## -----------------------------------------------------------------------------
# set a seed
set.seed(0)

# do smoove data using a RACVM for three different rotational speed
data_thermals <- Map(function(rot_speed, patch) {
  dt <- smoove::simulateRACVM(dt = 0.1, Tmax = 50, omega = rot_speed, v0 = 0.1)
  dt <- as.data.table(dt$XY)
  dt[, patch := patch]
  dt[, time := seq(.N)]
}, c(7, 7, 7), sprintf("patch_%i", seq(3)))

# rescale x and y
data_thermals <- Map(
  function(df, lim1, lim2, offset) {
    df[, x := x + lim1]
    df[, y := y + lim2]
    df[, time := time + offset]
  }, data_thermals, c(1, 5, 9), c(0, 2.5, 5),
  c(100, 800, 1500)
)

# get thermals data
data_thermals <- rbindlist(data_thermals)


## -----------------------------------------------------------------------------
# get limits
starts <- data_thermals[, lapply(.SD, first), by = "patch"]
ends <- data_thermals[, lapply(.SD, last), by = "patch"]

# bind
gaps <- rbind(starts, ends)
setorder(gaps, "time")
gaps[, event := rep(c("start", "end"), 3)]

# cast
gaps <- dcast(gaps, patch ~ event, value.var = c("x", "y", "time"))

# make a trajectory section
transit <- data.table(
  x1 = shift(gaps$x_end),
  x2 = gaps$x_start,
  y1 = shift(gaps$y_end),
  y2 = gaps$y_start,
  t1 = shift(gaps$time_end),
  t2 = gaps$time_start,
  transit = sprintf("transit_%i", seq(nrow(gaps))),
  pseudo_time = seq(nrow(gaps))
)

# nafill
transit[, c("x1", "y1", "t1") :=
  lapply(list(x1, y1, t1), nafill, fill = 0)]

# interpolate
transit[, d := Map(function(x1, x2, y1, y2) {
  as.data.table(approx(
    x = c(x1, x2),
    y = c(y1, y2),
    n = 20
  ))
}, x1, x2, y1, y2)]

# add errors
transit[, d := Map(function(df) {
  df[, lapply(.SD, do_add_error, std_dev = 0.05)]
}, d)]

# get transit data
transit_data <- Map(function(df, t1, t2) {
  df <- copy(df)
  df[, time := seq(t1, t2, length.out = nrow(df))]
}, transit$d, transit$t1, transit$t2)

# merge with thermals
transit_data <- rbindlist(transit_data)


## -----------------------------------------------------------------------------
# bind transits to thermals
data_move <- merge(data_thermals, transit_data,
  on = "time", all = T
)
setorder(data_move, time)

# save data
fwrite(data_move, "data/data_for_res_patch.csv")
