library(dplyr)
data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))

daily_confirmed = filter(data_state, Status %in% "Confirmed")

data_state= daily_confirmed[,4:ncol(daily_confirmed)]

data_p=data_state[,-which(colnames(data_state) %in% c("TT","DD","LD","UN"))]

data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))
daily_deceased = filter(data_state, Status %in% "Deceased")
data_state=daily_deceased[,4:ncol(daily_deceased)]

data_d=data_state[,-which(colnames(data_state) %in% c("TT","DD","LD","UN"))]
data_p=data_p[-c(1:18),]
data_d=data_d[-c(1:18),]
state_codes = colnames(data_state)[3:(ncol(data_state)-1)]
state_codes = c("AN", "AP", "AR", "AS", "BR", "CH", "CT", "DN", "DL", "GA", "GJ", "HR", "HP" ,"JK", "JH", "KA", "KL", "LA", 
                "MP", "MH", "MN", "ML","MZ", "NL", "OR", "PY", "PB", "RJ" ,"SK" ,"TN" ,"TG", "TR", "UP", "UT" ,"WB")
state_names = c("Andaman and Nicobar", "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chandigarh", "Chhattisgarh",
                "Dadra and Nagar Haveli", "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu and Kashmir", 
                "Jharkhand", "Karnataka", "Kerala","Ladakh", "Madhya Pradesh","Maharashtra","Manipur","Meghalaya","Mizoram",
                "Nagaland","Odisha","Puducherry","Punjab","Rajasthan","Sikkim","Tamil Nadu","Telangana","Tripura","Uttar Pradesh",
                "Uttarakhand","West Bengal")

mat_1=matrix(0, nrow = 36, ncol = 13)
for(i in 1:length(state_codes)){
  state_code = state_codes[i]
  state_name = state_names[i]  
  prediction = readRDS(paste0("Prediction_",state_code, ".rds"))
  dime=nrow(prediction)/14
  daily_p=prediction[dime*9+1:dime,]
  daily_p_pred=round(rowMeans(daily_p))
  daily_d=prediction[dime*11+1:dime,]
  daily_d_pred=round(rowMeans(daily_d))
  mat_1[i,]=c(state_name,data_p[366,state_code],daily_p_pred[366],data_d[366,state_code],daily_d_pred[366],daily_p_pred[380],daily_d_pred[380],
            daily_p_pred[395],daily_d_pred[395],daily_p_pred[396],daily_d_pred[396],daily_p_pred[410],daily_d_pred[410])
  #data[i,]=c(state_name,as.numeric(data_15_p[i]),(round(T_d[168])),round(total_pred[168]),round(UF_p,1),as.numeric(data_15_d[i]),
  #round(D_pred[168]),round(total_death[168]),round(UF_d,1),round(as.numeric(c(ifr1,ifr2,cfr)),4))
}




data = read.csv(url("https://api.covid19india.org/csv/latest/case_time_series.csv"))
data = data %>%
  mutate(Current.Confirmed = Total.Confirmed - Total.Recovered - Total.Deceased)
# View(data)
date_initial= which(data$Date_YMD == "2020-04-01")
data_initial = data[date_initial, ]
data_train = data[-c(1:date_initial-1), ]
daily_confirmed <- data_train[,"Daily.Confirmed"] ## Daily Positive
daily_recovered <- data_train[,"Daily.Recovered"] ## Daily Recovered
daily_deaths <- data_train[,"Daily.Deceased"] ##  Daily Deaths
prediction=readRDS("Prediction_India.rds")
dime=nrow(prediction)/14
daily_p=prediction[dime*9+1:dime,]
daily_p_pred=round(rowMeans(daily_p))
daily_d=prediction[dime*11+1:dime,]
daily_d_pred=round(rowMeans(daily_d))
mat_1[36,]=c("India",daily_p[366],daily_p_pred[366],daily_d[366],daily_d_pred[366],daily_p_pred[380],daily_d_pred[380],
           daily_p_pred[395],daily_d_pred[395],daily_p_pred[396],daily_d_pred[396],daily_p_pred[410],daily_d_pred[410])
dim(prediction)
colnames(mat_1)=c("Place","Obs_P_Apr_1","Pred_P_Apr_1","Obs_D_Apr_1","Pred_D_Apr_1","Pred_P_Apr_15","Pred_D_Apr_15","Pred_P_Apr_30",
                "Pred_D_Apr_30","Pred_P_May_1","Pred_D_May_1","Pred_P_May_15","Pred_D_May_15")




View(mat)
write.csv(mat,"Mostimp.csv")



##########
library(dplyr)
data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))

daily_confirmed = filter(data_state, Status %in% "Confirmed")

data_state= daily_confirmed[,4:ncol(daily_confirmed)]

data_p=data_state[,-which(colnames(data_state) %in% c("TT","DD","LD","UN"))]

data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))
daily_deceased = filter(data_state, Status %in% "Deceased")
data_state=daily_deceased[,4:ncol(daily_deceased)]

data_d=data_state[,-which(colnames(data_state) %in% c("TT","DD","LD","UN"))]
data_p=data_p[-c(1:18),]
data_d=data_d[-c(1:18),]
state_codes = colnames(data_state)[3:(ncol(data_state)-1)]
state_codes = c("AN", "AP", "AR", "AS", "BR", "CH", "CT", "DN", "DL", "GA", "GJ", "HR", "HP" ,"JK", "JH", "KA", "KL", "LA", 
                "MP", "MH", "MN", "ML","MZ", "NL", "OR", "PY", "PB", "RJ" ,"SK" ,"TN" ,"TG", "TR", "UP", "UT" ,"WB")
state_names = c("Andaman and Nicobar", "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chandigarh", "Chhattisgarh",
                "Dadra and Nagar Haveli", "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu and Kashmir", 
                "Jharkhand", "Karnataka", "Kerala","Ladakh", "Madhya Pradesh","Maharashtra","Manipur","Meghalaya","Mizoram",
                "Nagaland","Odisha","Puducherry","Punjab","Rajasthan","Sikkim","Tamil Nadu","Telangana","Tripura","Uttar Pradesh",
                "Uttarakhand","West Bengal")
data=matrix(0, nrow = 35, ncol = 19)
for(i in 1:length(state_codes)){
  i=3
  state_code = state_codes[i]
  state_name = state_names[i]  
  prediction = readRDS(paste0("Prediction_",state_code, ".rds"))
  P_pred=prediction[dime*3+1:dime,]
  R_pred=prediction[dime*6+1:dime,]
  D_pred=prediction[dime*8+1:dime,]
  T_d=R_pred+D_pred+P_pred
  total_pred = colSums(prediction[c(2:8)*dime+366,])
  UF_p =total_pred/as.numeric(data_p[366,state_code])
  UF_p_low=quantile(UF_p, 0.025)
  UF_p_mean=mean(UF_p)
  UF_p_upper= quantile(UF_p, 0.975)
  
  D_U=prediction[dime*7+1:dime,]
  total_death=D_U+D_pred
  UF_d=total_death[366,]/as.numeric(data_d[366,state_code])
  UF_d_low=quantile(UF_d, 0.025)
  UF_d_mean=mean(UF_d)
  UF_d_upper= quantile(UF_d, 0.975)
  
  ifr_1=as.numeric(data_1_d[i])/as.numeric(total_pred)
  ifr_1_low=quantile(ifr_1, 0.025)
  ifr_1_mean=mean(ifr_1)
  ifr_1_upper= quantile(ifr_1, 0.975)
  
  hist(ifr_1,nclass=1000000,xlim=c(0,10e-06))
  
  ifr_2=total_death[366,]/total_pred
  ifr_2_low=quantile(ifr_2, 0.025)
  ifr_2_mean=mean(ifr_2)
  ifr_2_upper= quantile(ifr_2, 0.975)
  
  
  
  
  data[i,]=c(state_name,as.numeric(data_p[366,state_code]),(round(T_d[366])),round(total_pred[366]),round(UF_p,1),as.numeric(data_d[366,state_code]),
             round(D_pred[366]),round(total_death[366]),round(c(UF_p_low,UF_p_mean,UF_p_upper),2),
             round(c(UF_d_low,UF_d_mean,UF_d_upper),2),round(c(ifr_1_low,ifr_1_mean,ifr_1_upper,ifr_2_low,ifr_2_mean,ifr_2_upper)*100,2))
}



colnames(data)=c("Place","Observed Cases","Predicted Reported Cases","Predicted Total Cases",
                 "Observed Deaths","Predicted Reported Deaths","Predicted Total Deaths","UFcaseslow","UFcasesmean","UFcaseshigh","UFdeathslow",
                 "UFdeathsmean","UFdeathshigh","ifr1low","ifr1mean","ifr1high","ifr2low","ifr2mean","ifr2high")
write.csv(data[1:35,],"Mostimp.csv")

