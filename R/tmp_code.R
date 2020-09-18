### Simulate movement replicates

```{r}
sim_data <- mapply(function(p_outlier, ext_outlier, replicate) {
  
  # make simulated data
  ref_data <- do_smoove_data()
  
  # extract 'true' movement metrics
  speed <- atl_get_speed(ref_data, type = "in")
  
  # now add error
  error_data <- do_add_outliers(ref_data, 
                                p_data = p_outlier,
                                std_dev = ext_outlier)
  
  # get movement metrics with error
  error_speed <- atl_get_speed(error_data, type = "in")
  
  # now clean data
  clean_data <- copy(error_data)
  clean_data[, `:=`(in_speed = atl_get_speed(clean_data, type = "in"),
                    out_speed = atl_get_speed(clean_data, type = "out"),
                    angle = atl_turning_angle(clean_data))]
  upper_speed_cutoff <- stats::quantile(clean_data$in_speed, 
                                        probs = 0.9,
                                        na.rm = TRUE)
  angle_cutoff <- 40
  clean_data <- atl_filter_covariates(clean_data,
                                      filters = c(glue('{c("in_speed", 
                                                           "out_speed")} < 
                                                         {upper_speed_cutoff}'),
                                                  glue('angle < {angle_cutoff}')
                                                  )
                                      )
  
  # get speed and land proportion
  clean_speed <- atl_get_speed(clean_data, type = "in")
  
  # make speed results
  speed_res <- data.table(type = c("reference", "outliers", "cleaned"),
                          speed_mean = unlist(lapply(list(speed, 
                                                          error_speed, 
                                                          clean_speed),
                                                     mean, na.rm = TRUE)),
                          speed_sd = unlist(lapply(list(speed, 
                                                          error_speed, 
                                                          clean_speed),
                                                     sd, na.rm = TRUE)),
                          p_outlier = p_outlier,
                          ext_outlier = ext_outlier,
                          replicate = replicate
                          )
  results <- list(speed_res = speed_res)
  
  return(results)
}, 
param_c$p_outlier, 
param_c$ext_outlier,
param_c$replicate,
SIMPLIFY = FALSE)
```


```{r outlier_removal_speed}
# get speed data
speed_data <- rbindlist(lapply(sim_data, function(l) { l$speed_res }))

# cast wide
speed_data <- speed_data[, !"speed_sd"]

# find proportion of speed data
speed_data <- dcast(speed_data, 
      p_outlier + ext_outlier + replicate ~ type, 
      value.var = "speed_mean")

# get proportion of speeds
speed_data[, `:=`(d_clean = cleaned * 100 / reference,
                  d_outlier = cleaned * 100 / reference)]

# melt
speed_data <- melt(speed_data[, !c("replicate", "cleaned", 
                                   "reference", "outliers")], 
                   id.vars = c("p_outlier", "ext_outlier"))

# summarise
speed_data <- speed_data[, list(mean = mean(value),
                                sd = sd(value)),
                         by = list(p_outlier, ext_outlier, variable)]

ggplot(speed_data)+
  geom_pointrange(aes(factor(variable), mean - 100,
                      ymin = mean - sd - 100,
                      ymax = mean + sd - 100,
                      col = ext_outlier),
                  position = position_dodge(width = 1))+
  theme_test()+
  theme(strip.background = element_rect(fill = "grey90",
                                        colour = NA))
```