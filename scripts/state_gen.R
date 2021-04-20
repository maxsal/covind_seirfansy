library(here)
source("libraries.R")

f <- list.files(here("functions"))
for (i in seq_along(f)) { source(here("functions", f[i])) }

# specs -----------
state    <- "AN"
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

#############
data <- readr::read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv") %>%
  janitor::clean_names() %>%
  dplyr::select("date" = "date_ymd", "status", "val" = tolower(state)) %>%
  arrange(date) %>%
  tidyr::pivot_wider(
    names_from  = "status",
    values_from = "val",
    id_cols = "date"
  ) %>%
  dplyr::filter(date >= min_date & date <= max_date)

data_initial <- c(10, 0, 0, 1, 0, 0)

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


P_pred <- prediction[515*3+1:515, ] %>% rowMeans()
R_pred <- prediction[515*6+1:515, ] %>% rowMeans()
D_pred <- prediction[515*8+1:515, ] %>% rowMeans()
T_d <- R_pred + D_pred + P_pred
total_pred  <- rowSums(matrix(rowMeans(prediction), nrow = 515)[, 3:9])
UF_p <- total_pred / T_d

D_U <- prediction[515*7 + 1:515, ] %>% rowMeans()
total_death <- D_U+D_pred
UF_d <- total_death / D_pred
ifr  <- total_death / total_pred

impo <- tibble(
  "UnderReporting_Cases" = UF_p[366],
  "UnderReporting_Deaths" = UF_d[366],
  "ifr" = ifr[366]
  )

write_rds(impo, here("output", paste0("important_", state, ".rds")),
          compress = "gz")
