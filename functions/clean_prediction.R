clean_prediction <- function(x, obs_days, t_pred) {
  
  as_tibble(x) %>%
    mutate(
      section = rep(1:14, each = obs_days + t_pred),
      date    = rep(as.Date(min_date:(min_date + (obs_days + t_pred - 1)), origin = "1970-01-01"), 14),
      pred    = rep(c(rep(0, obs_days), rep(1, t_pred)), 14)
    ) %>%
    dplyr::select(section, date, pred, everything())
    # group_by(section) %>%
    # group_split()
  
}

# clean_prediction(prediction, obs_days = obs_days, t_pred = t_pred)
