library(dplyr)
library(arm)

source("par_initializeR.R")
source("model_estimateR.R")
source("model_initializeR.R")
source("mcmc_performR.R")
source("model_deterministic_simulateR.R")
source("R0_calculateR.R")
source("model_predictR.R")
source("model_stochastic_simulateR.R")
source("model_plotR.R")


#############
data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))
colnames(data_state)[3:ncol(data_state)]
state = "TG"
data = data_state[, c("Date", "Status", state)]
daily_confirmed = filter(data, Status %in% "Confirmed")[,3]
daily_recovered = filter(data, Status %in% "Recovered")[,3]
daily_deceased = filter(data, Status %in% "Deceased")[,3]
date = as.character(filter(data, Status %in% "Confirmed")[,1])
data = data.frame(date = date, "Daily.Confirmed" = daily_confirmed, 
                  "Daily.Recovered" = daily_recovered, "Daily.Deceased" = daily_deceased)

date_1_April= which(data$date == "01-Apr-20")
date_1_dec = which(data$date == "01-Dec-20")
data_initial = data[date_1_April, ]
data_train = data[date_1_April:date_1_dec, ]
obsP_tr <- data_train[,"Daily.Confirmed"] ## Daily Positive
obsR_tr <- data_train[,"Daily.Recovered"] ## Daily Recovered
obsD_tr <- data_train[,"Daily.Deceased"] ##  Daily Deaths

## Hyperparamters
N = 35.2e6

data_initial = c(154,17,9,27, 3, 0)
data = data_state[, c("Date_YMD", "Status", state)]
daily_confirmed = filter(data, Status %in% "Confirmed")[,3]
daily_recovered = filter(data, Status %in% "Recovered")[,3]
daily_deceased = filter(data, Status %in% "Deceased")[,3]
date = as.character(filter(data, Status %in% "Confirmed")[,1])
data = data.frame(date = date, "Daily.Confirmed" = daily_confirmed, 
                  "Daily.Recovered" = daily_recovered, "Daily.Deceased" = daily_deceased)

date_1_April= which(data$date == "2020-04-01")
date_31_mar = which(data$date == "2021-03-31")
data_train = data[date_1_April:date_31_mar, ]
obsP_tr <- data_train[,"Daily.Confirmed"] ## Daily Positive
obsR_tr <- data_train[,"Daily.Recovered"] ## Daily Recovered
obsD_tr <- data_train[,"Daily.Deceased"] ##  Daily Deaths


mCFR = tail(cumsum(obsD_tr) / cumsum(obsD_tr+obsR_tr),1)
## Run predictR

data_multinomial = abs(data.frame("Confirmed" = obsP_tr, "Recovered" = obsR_tr, "Deceased" = obsD_tr))

#pars_start <- c(c(1,0.8,0.6,0.4,0.2), c(0.2,0.2,0.2,0.25,0.2))

phases = c(1,15,34,48,62,92,123, 154, 184, 215, 245, 276, 307, 335)

Result = model_predictR(data = data_multinomial,init_pars=NULL,data_init = data_initial, T_predict =150,
                        niter = 1e5, BurnIn = 1e5, model = "Multinomial", N = N, lambda = 1/(69.416 * 365),
                        mu = 1/(69.416 * 365), period_start = phases, opt_num = 200,auto.initialize=T,alpha_u=0.5,f=0.15,
                        plot = FALSE, save_plots = FALSE)

saveRDS(Result$prediction,paste0("Prediction_", state, ".rds"))
saveRDS(Result$mcmc_pars,paste0("Prediction_pars_", state, ".rds"))

prediction=Result$prediction
dim(prediction)

P_pred=prediction[515*3+1:515,]
P_pred=rowMeans(P_pred)
R_pred=prediction[515*6+1:515,]
R_pred=rowMeans(R_pred)
D_pred=prediction[515*8+1:515,]
D_pred=rowMeans(D_pred)
T_d=R_pred+D_pred+P_pred
total_pred = rowSums(matrix(rowMeans(prediction), nrow = 515)[, 3:9])
UF_p =total_pred/T_d
UF_p[366]
total_pred[366]
D_U=prediction[515*7+1:515,]
D_U=rowMeans(D_U)
total_death=D_U+D_pred
UF_d=total_death/D_pred
UF_d[366]
ifr=total_death/total_pred
ifr[366]

impo=data.frame("UnderReporting_Cases" = UF_p[366], "UnderReporting_Deaths" = UF_d[366], "ifr" = ifr[366])
saveRDS(impo,paste0("Important_", state, ".rds"))

