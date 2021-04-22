library(here)
source("libraries.R")

f <- list.files(here("functions"))
for (i in seq_along(f)) { source(here("functions", f[i])) }

# specs -----------
state    <- "DL"
max_date <- as.Date(Sys.Date() - 1)
min_date <- as.Date("2020-04-01")
obs_days <- length(as.Date(min_date):as.Date(max_date))
t_pred   <- 150 # number of predicted days
N        <- pop %>% filter(abbrev == tolower(state)) %>% pull(population)
n_iter   <- 1e3 #default 1e5
burn_in  <- 1e2 #default 1e5
opt_num  <- 1   #default 200
plt      <- FALSE
save_plt <- FALSE

data <- readr::read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                        col_types = cols()) %>%
  janitor::clean_names() %>%
  dplyr::select("date" = "date_ymd", "status", "val" = tolower(state)) %>%
  arrange(date) %>%
  tidyr::pivot_wider(
    names_from  = "status",
    values_from = "val",
    id_cols = "date"
  )

pre_data <- data %>%
  filter(date < min_date) %>%
  dplyr::select(-date) %>%
  summarize(
    Confirmed = sum(Confirmed),
    Recovered = sum(Recovered),
    Deceased  = sum(Deceased)
  ) %>%
  as.numeric(as.vector(.))

data %<>% filter(date >= min_date & date <= max_date)

data_initial <- c(pre_data,
                  data %>%
                    filter(date == min_date) %>%
                    dplyr::select(-date) %>%
                    as.numeric(as.vector(.))
                  )
if (data_initial[1] == 0) {data_initial[1] <- 1}
if (data_initial[4] == 0) {data_initial[4] <- 1} # check with Ritwik/Ritoban if this is necessary

mCFR <- tail(cumsum(data$Deceased) / cumsum(data$Deceased + data$Recovered), 1)

phases <- c(1, 15, 34, 48, 62, 92, 123, 154, 184, 215, 245, 276, 307, 335)

result <- model_predictR(data            = abs(data %>% dplyr::select(-date)),
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

pred_clean <- clean_prediction(prediction,
                               state = pop %>% filter(abbrev == tolower(state)) %>% pull(full),
                               obs_days = obs_days,
                               t_pred = t_pred)

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

write_rds(impo, here("output", paste0("important_", state, ".rds")),
          compress = "gz")
