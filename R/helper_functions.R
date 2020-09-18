
do_smoove_data <- function(t_max = 100, dt = dt) {
  # define parameters
  nu <- 2
  tau <- 1
  dt <- .01 
  ucvm1 <- smoove::simulateUCVM(nu = nu, tau = tau, T.max = 100, dt = dt)
  
  # get the data in data.table format
  tmp_data <- data.table::as.data.table(ucvm1$XY)
  # rescale coordinates
  tmp_data[, c("x", "y") := (lapply(.SD, 
                                scales::rescale)),
       .SDcols = c("x", "y")]
  
  # add a time column
  tmp_data[, time := seq_len(nrow(tmp_data))]

  return(tmp_data)
}

# simple function to introduce normally distributed error
do_add_error <- function(v, std_dev = 0.5) {
  pos_error <- stats::rnorm(length(v), sd = std_dev)
  return(v + pos_error)
}

# function for point outliers
do_add_outliers <- function(data, p_data = 0.1, std_dev = 0.05) {
  n_rows <- floor(p_data * nrow(data))
  tmp_data <- data.table::copy(data)
  
  tmp_data[sample(nrow(data), n_rows),
            c("x", "y") := lapply(.SD,
                                  function(z) {
                                    z + stats::rnorm(n_rows, sd = std_dev)
                                  }),
            .SDcols = c("x", "y")]
  return(tmp_data)
}

# function for extracting and tallying landscape values
do_count_landcover <- function(land, track) {
  land_values <- raster::extract(land, track[, c("x", "y")])
  land_values <- data.table::as.data.table(table(land_values))
  land_values[, p_land := N / sum(N)]
  return(land_values)
}

# a function to scale the landscape
do_scale_landscape <- function(land_, scale_factor) {
  bounds <- c(0, 1, 0, 1)
  bounds <- bounds * scale_factor
  raster::extent(land_) <- bounds
  return(land_)
}
