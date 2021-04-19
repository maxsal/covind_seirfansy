library(here)
source("libraries.R")

f <- list.files(here("functions"))
for (i in seq_along(f)) { source(here("functions", f[i])) }

# specs -----------
state    <- "AN"
max_date <- Sys.Date() - 1
min_date <- "2020-04-01"
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

res <- SEIRfansy(data = abs(data %>% dplyr::select(-date)),
                 init_pars = NULL, 
                 data_init = data_initial,
                 niter = n_iter,
                 BurnIn = burn_in, 
                 model = "Multinomial",
                 N = N,
                 lambda = 1/(69.416 * 365), 
                 mu = 1/(69.416 * 365),
                 period_start = phases,
                 opt_num = opt_num, 
                 auto.initialize = TRUE,
                 f = 0.15)