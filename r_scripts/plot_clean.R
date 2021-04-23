library(here)

source("libraries.R")

f <- list.files(here("functions"))
for (i in seq_along(f)) { source(here("functions", f[i])) }

nrow(pop) 

n_obs <- 1000

n_date <- 61

data_case <- matrix(nrow = nrow(pop), ncol = n_date)

# loop over states
state_codes <- c("TN")

for (i in seq_along(state_codes)) {
  
  tmp_prediction <- read_csv(here("output", paste0("prediction_", tolower(state_codes[i]), ".csv")),
                         col_types = cols())
  
  tmp_p_pred <- tmp_prediction %>%
    average_pred(sec = "positive_reported", n_date = n_date)
  tmp_r_pred <- tmp_prediction %>%
    average_pred(sec = "recovered_reported", n_date = n_date)
  tmp_d_pred <- tmp_prediction %>%
    average_pred(sec = "death_reported", n_date = n_date)
  tmp_total_rep_case <- tmp_p_pred$value + tmp_r_pred$value + tmp_d_pred$value
  tmp_total_rep_death <- tmp_d_pred$value
  
  tmp_daily_rep_case <- my_diff(tmp_total_rep_case)
  tmp_daily_rep_death <- my_diff(tmp_total_rep_death)
  
  if (i == 1) {
    
    case_dat <- tibble(
      state = tmp_p_pred$state,
      date  = tmp_p_pred$date,
      value = tmp_daily_rep_case
    )
    
    death_dat <- tibble(
      state = tmp_p_pred$state,
      date  = tmp_p_pred$date,
      value = tmp_daily_rep_death
    )
    
  } else {
    
    case_dat <- bind_rows(
      case_dat,
      tibble(
        state = tmp_p_pred$state,
        date  = tmp_p_pred$date,
        value = tmp_daily_rep_case
      )
    )
    
    death_dat <- bind_rows(
      death_dat,
      tibble(
        state = tmp_p_pred$state,
        date  = tmp_p_pred$date,
        value = tmp_daily_rep_death
      )
    )
    
  }
  
}

super_special_line_plot <- function(dat, title) {
  
  dat %>%
    ggplot(aes(x = date, y = value, color = state, group = state)) +
    geom_line() +
    labs(
      title = title,
      x     = "Date",
      y     = "Predicted counts"
    ) +
    scale_y_continuous(labels = addUnits) +
    final_theme +
    guides(size = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 2)))
  
}

case_dat %>% super_special_line_plot(title = "Projected daily new cases")
