library(here)
source("libraries.R")

f <- list.files(here("functions"))
for (i in seq_along(f)) { source(here("functions", f[i])) }

# specs -----------
state    <- "TN" # <---
max_date <- as.Date(Sys.Date() - 1) # <---
min_date <- as.Date("2020-04-01")
obs_days <- length(as.Date(min_date):as.Date(max_date))
t_pred   <- 150 # number of predicted days
N        <- get_pop(state) %>% unique()
n_iter   <- 1e5 #default 1e5
burn_in  <- 1e5 #default 1e5
opt_num  <- 200   #default 200
plt      <- FALSE
save_plt <- FALSE

# load and prepare ----------
data <- readr::read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                        col_types = cols()) %>%
  janitor::clean_names() %>%
  dplyr::select("date" = "date_ymd", "status", "val" = tolower(state)) %>%
  arrange(date) %>%
  tidyr::pivot_wider(
    names_from  = "status",
    values_from = "val",
    id_cols = "date"
  ) %>%
  dplyr::filter(date <= max_date)

data_initial <- get_init(data)

data <- data %>% dplyr::filter(date >= min_date)

mCFR <- tail(cumsum(data$Deceased) / cumsum(data$Deceased + data$Recovered), 1)

phases <- get_phase(start_date = "2020-04-01")

# predict -----------
result    <- SEIRfansy.predict(
  data            = abs(data %>% dplyr::select(-date)),
  init_pars       = NULL,
  data_init       = data_initial,
  T_predict       = t_pred,
  niter           = n_iter,
  BurnIn          = burn_in,
  model           = "Multinomial",
  N               = N,
  lambda          = 1/(69.416 * 365),
  mu              = 1/(69.416 * 365),
  period_start    = phases,
  opt_num         = opt_num,
  auto.initialize = T,
  alpha_u         = 0.5,
  f               = 0.15,
  plot            = plt,
  save_plots      = save_plt
)

write_rds(result$prediction, here("output", paste0("prediction_", state, ".rds")),
         compress = "gz")
write_rds(result$mcmc_pars, here("output", paste0("prediction_pars_", state, ".rds")),
          compress = "gz")

prediction <- result$prediction
dim(prediction)

# prepare and important metrics ----------
pred_clean <- clean_prediction(prediction,
                               state = pop %>% filter(abbrev == tolower(state)) %>% pull(full) %>% unique(),
                               obs_days = obs_days,
                               t_pred = t_pred)
write_csv(pred_clean, here("output", paste0("prediction_", state, ".csv")))

p_pred <- pred_clean %>%
  filter(section == "positive_reported") %>%
  dplyr::select(-(1:4)) %>%
  rowMeans()

r_pred <- pred_clean %>%
  filter(section == "recovered_reported") %>%
  dplyr::select(-(1:4)) %>%
  rowMeans()

d_pred <- pred_clean %>%
  filter(section == "death_reported") %>%
  dplyr::select(-(1:4)) %>%
  rowMeans()

t_d <- r_pred + d_pred + p_pred
total_pred <- rowSums(matrix(rowMeans(prediction), nrow = obs_days + t_pred)[, 3:9])
UF_p <- total_pred / t_d

d_u <- pred_clean %>%
  filter(section == "death_unreported") %>%
  dplyr::select(-(1:4)) %>%
  rowMeans()
total_death <- d_u + d_pred
UF_d <- total_death / d_pred
ifr <- total_death / total_pred

impo <- tibble(
  "underreporting_cases"  = UF_p[obs_days + 1],
  "underreporting_deaths" = UF_d[obs_days + 1],
  "ifr"                   = ifr[obs_days + 1]
)

write_csv(impo, here("output", paste0("important_", state, ".csv")))
