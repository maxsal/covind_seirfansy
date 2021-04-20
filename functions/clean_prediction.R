clean_prediction <- function(x, state, obs_days, t_pred) {
  
  vals <- c(
    "susceptible",
    "exposed",
    "infectious_unreported",
    "positive_reported",
    "false_negative",
    "recovered_unreported",
    "recovered_reported",
    "death_unreported",
    "death_reported",
    "positive_daily_reported",
    "recovered_daily_reported",
    "death_daily_reported",
    "false_negative_daily",
    "unreported_daily"
  )
  
  as_tibble(x) %>%
    mutate(
      state   = state,
      section = rep(vals, each = obs_days + t_pred),
      date    = rep(as.Date(min_date:(min_date + (obs_days + t_pred - 1)), origin = "1970-01-01"), 14),
      pred    = rep(c(rep(0, obs_days), rep(1, t_pred)), 14)
    ) %>%
    dplyr::select(state, section, date, pred, everything())
    # group_by(section) %>%
    # group_split()
  
}

# clean_prediction(prediction, obs_days = obs_days, t_pred = t_pred)
