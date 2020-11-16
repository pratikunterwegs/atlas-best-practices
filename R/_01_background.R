## ----install_package--------------------------------------------------
# use either devtools or remotes to install
install.packages("devtools")

# installation using devtools
devtools::install_github("pratikunterwegs/atlastools")


## ----prep_libs_01_02--------------------------------------------------
# load smoove and datatable
library(smoove)
library(data.table)
library(scales)

# source helper functions
source("R/helper_functions.R")


## ----save_data--------------------------------------------------------
data <- do_smoove_data()
# save simulated data
fwrite(data, "data/data_sim.csv")


## ---------------------------------------------------------------------
# do smoove data using a RACVM for three different rotational speed
data_thermals <- Map(function(rot_speed, patch) {
 dt <- smoove::simulateRACVM(dt = 0.1, Tmax = 50, omega = rot_speed, v0 = 0.1) 
 dt <- as.data.table(dt$XY)
 dt[, patch := patch]
}, c(7, 7, 7), sprintf('patch_%i', seq(3)))

# rescale x and y
data_thermals <- Map(function(df, lim1, lim2, t1, t2) {
  df[, x := x + lim1]
  df[, y := y + lim2]
  df[, time := seq(t1, t2)]
}, data_thermals, c(1, 5, 9), c(0, 2.5, 5),
   c(100, 800, 1500), c(600, 1300, 2000))

# get thermals data
data_thermals <- rbindlist(data_thermals)


## ---------------------------------------------------------------------
# get limits
starts <- data_thermals[, lapply(.SD, first), by = "patch"]
ends <- data_thermals[, lapply(.SD, last), by = "patch"]

# bind
gaps <- rbind(starts, ends)
setorder(gaps, "time")

# get transition phases
# THIS IS HARDCODED BECAUSE THERE DOESN'T APPEAR TO BE A BETTER WAY
# FOR THE SAME TIME
transit_1 <- gaps[c(2, 3), ]
transit_2 <- gaps[c(4, 5), ]

# simulate movement
transits <- Map(function(df){
  time_lim <- as.vector(df$time)
  
  track <- as.data.table(approx(df$x, df$y, n = 10))
  
  # add some error
  track[, `:=`(y = do_add_error(y, 0.01))]
  
  track[, `:=`(time = seq(time_lim[1], time_lim[2], length.out = 10),
               patch = "transit")]
  
  return(track)
  
}, list(transit_1, transit_2))

# bind
transits <- rbindlist(transits)


## ---------------------------------------------------------------------
# bind transits to thermals
data_move <- merge(data_thermals, transits, on = "time", all = T)
setorder(data_move, time)

# save data
fwrite(data_move, "data/data_for_res_patch.csv")

