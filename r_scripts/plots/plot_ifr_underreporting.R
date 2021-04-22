library(ggplot2)
underreporting_theme <-   theme_minimal() +
  theme(
    plot.title         = element_text(size = 16, face = "bold"),
    plot.subtitle      = element_text(size = 16, color = "black"),
    plot.caption       = element_text(hjust = 0, size = 14, lineheight = 1.1),
    axis.text.y        = element_text(size = 14, color = "black"),
    #axis.text.x        = element_blank(),
    axis.text.x        = element_text(angle = 90, size = 11, hjust = 0.95, color = "black"), 
    axis.title.y       = element_text(size = 14, face = "italic"),
    axis.title.x       = element_blank() ,  
    legend.title       = element_blank() ,
    legend.text        = element_text(size = 14) ,
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    axis.ticks.x = element_line(),
    axis.line = element_line(color = "black")
  )

# which(seq(as.Date("2020-04-15"), as.Date("2020-04-15")+ 1000, by = 1) == as.Date("2021-04-01"))

## states omitted = mizoram, lakhshadweep, daman diu

data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))
data_state_cases = data_state[data_state["Status"] == "Confirmed", ]
data_state_cases = data_state_cases[1:which(data_state_cases["Date"] == "01-Apr-21"),]
data_state_deaths = data_state[data_state["Status"] == "Deceased", ]
data_state_deaths = data_state_deaths[1:which(data_state_deaths["Date"] == "01-Apr-21"),]
state_codes = c("AN", "AP", "AR", "AS", "BR", "CH", "CT", "DN", "DL", "GA", "GJ", "HR", "HP" ,"JK", "JH", "KA", "KL", "LA", 
                "MP", "MH", "MN", "ML", "MZ", "NL", "OR", "PY", "PB", "RJ" ,"SK" ,"TN" ,"TG", "TR", "UP", "UT", "WB", "India")
colnames(data_state_cases)
observed_cum_cases = colSums(data_state_cases[, state_codes[-length(state_codes)]])
observed_cum_cases = c(observed_cum_cases, "IN" = 12391138)
observed_cum_deaths = colSums(data_state_deaths[, state_codes[-length(state_codes)]])
observed_cum_deaths = c(observed_cum_deaths, "IN" = 163428)
state_names = c("Andaman and Nicobar", "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chandigarh", "Chhattisgarh",
                "Dadra and Nagar Haveli", "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu and Kashmir", 
                "Jharkhand", "Karnataka", "Kerala","Ladakh", "Madhya Pradesh","Maharashtra","Manipur","Meghalaya","Mizoram",
                "Nagaland","Odisha","Puducherry","Punjab","Rajasthan","Sikkim","Tamil Nadu","Telangana","Tripura","Uttar Pradesh",
                "Uttarakhand","West Bengal","India")

n_var = ifelse(length(state_codes) == length(state_names), length(state_codes), stop("length not equal"))
n_obs = 1000
underreporting_case = matrix(0, nrow = n_obs, ncol = n_var)
underreporting_death = matrix(0, nrow = n_obs, ncol = n_var)
ifr_1 = matrix(0, nrow = n_obs, ncol = n_var)
ifr_2 = matrix(0, nrow = n_obs, ncol = n_var)

## Set directory to "data" folder
setwd("result files")

for(i in 1:length(state_codes)){
  state_code = state_codes[i]
  state_name = state_names[i]  
  prediction = readRDS(paste0("Prediction_",state_code, ".rds"))
  P_pred=prediction[515*3+352,]
  R_pred=prediction[515*6+352,]
  D_pred=prediction[515*8+352,]
  total_det_case = R_pred+D_pred+P_pred
  total_det_death = D_pred
  total_pred_case = colSums(prediction[352 + 515*(2:8), ])
  total_pred_death = colSums(prediction[352 + 515*(7:8), ])
  UF_case = total_pred_case / observed_cum_cases[i]
  UF_death = total_pred_death / observed_cum_deaths[i]
  IFR_1 = observed_cum_deaths[i] / total_pred_case
  IFR_2 = total_pred_death / total_pred_case
  underreporting_case[,i] = UF_case
  underreporting_death[,i] = UF_death
  ifr_1[,i] = IFR_1
  ifr_2[,i] = IFR_2
}

i=28
for(i in 1:ncol(underreporting_death)){
  temp = underreporting_death[,i]
  if(sum(is.na(temp)) == 0){
    if(!sum(temp == Inf))
      next
  }
  if(sum(is.na(temp))){
    na_indices = which(is.na(temp))
    temp_not_na = temp[-na_indices]
  }
  else temp_not_na = temp
  infty_indices = which(temp_not_na == Inf)
  temp_good = temp_not_na[-infty_indices]
  mean_temp = mean(temp_good)
  print(c(i, length(temp_good), mean_temp))
  temp = c(temp_good, rep(mean_temp, n_obs-length(temp_good)))
  underreporting_death[,i] = temp
}
data_underreporting = matrix(0, nrow = n_obs*n_var*2, ncol = 3)
colnames(data_underreporting) = c("Label", "variable", "value")
data_underreporting[1:(n_obs*n_var),3] = matrix(underreporting_case,ncol = 1, byrow = FALSE)
data_underreporting[(n_obs*n_var) + 1:(n_obs*n_var),3] = matrix(underreporting_death,ncol = 1, byrow = FALSE)
data_underreporting[,1] = rep(c("case", "death"), each = n_obs*n_var)
data_underreporting[,2] = rep(rep(state_names, each = n_obs), times = 2)
data_underreporting = data.frame(data_underreporting)
data_underreporting$value = as.numeric(data_underreporting$value)
data_underreporting$variable = factor(data_underreporting$variable, levels = state_names[order(colMeans(underreporting_case))])
data_underreporting$Label = factor(data_underreporting$Label, levels = c("case", "death")[2:1])
library(ggplot2)
require(scales)
library(DescTools)
gg_colors = c("#F8766D","#00BFC4", "#00BA38" )

color_array_1 = ifelse(state_names[order(colMeans(underreporting_case))] == "India", "red", "black")
p1 = ggplot(data = data.frame(data_underreporting), aes(x=variable, y=value)) +
  underreporting_theme+
  geom_boxplot(aes(fill=Label), position=position_dodge(width = 0), outlier.alpha = 0, width=1.5)+
  scale_fill_manual(values = gg_colors[1:2], labels = c("URF death", "URF case")) +
  scale_color_manual(values =  MixColor(gg_colors[1:2],"black" ), labels = c("URF-death", "URF-case"))+
  scale_y_log10(breaks = c(0,1, 10, 100)) + 
  ggtitle("Estimated vs. Reported Cases and Deaths") +
  ylab("Ratio of Estimated to Reported (Log Scale)") + 
  theme(axis.text.x = element_text(colour = color_array_1))
p1

## change directory back to root folder
setwd("..")

ggsave("temp1.png", p1, width = 6, height = 6)

###########################################################################################################
# IFR Plot
###########################################################################################################
#cfr_temp = readRDS("cfr.rds")

observed_CFR_values = c(0.0129,0.0081,0.0033,0.0046,0.0054,0.0159,0.0121,0.0006,0.0161,0.0143,0.0189,
                        0.0104,0.0159,0.0154,0.0089,0.0133,0.0037,0.014,0.0158,0.0258,0.0114,0.0095,
                        0.00156, 0.0052,0.0056,0.0165,0.0316,0.0086,0.0218,0.015,0.0054,0.0112,0.0143,0.0164,0.0174,0.0145)
sd_calculator <- function(x, n){ # x = cfr, n = observed cases
  return(sqrt(x*(1-x)/n))
}

sd_cfr = c()
for(i in 1:length(observed_CFR_values)){
  x = observed_CFR_values[i]
  n = observed_cum_cases[i]
  sd_cfr[i] = sd_calculator(x, n)
}

sd_cfr
low_CI_cfr = observed_CFR_values - sd_cfr*1.96
upper_CI_cfr = observed_CFR_values + sd_cfr*1.96
saveRDS(data.frame(mean_cfr = observed_CFR_values, lower_CI_cfr = low_CI_cfr, upper_CI_cfr = upper_CI_cfr),"CFR_CI.rds")


cfr = matrix(0, nrow = n_obs, ncol = n_var)
for(i in 1:length(observed_CFR_values)){
  cfr[,i] = rnorm(n_obs, observed_CFR_values[i], sd_cfr[i])
}


# mean(data_IFR[1:(n_obs*n_var),3])
# mean(data_IFR[n_obs*n_var + 1:(n_obs*n_var),3])

data_IFR = matrix(0, nrow = n_obs*n_var*3, ncol = 3)
colnames(data_IFR) = c("type", "variable", "value")
data_IFR[1:(n_obs*n_var),3] = matrix(ifr_1,ncol = 1, byrow = FALSE)
data_IFR[n_obs*n_var + 1:(n_obs*n_var),3] = matrix(ifr_2,ncol = 1, byrow = FALSE)
data_IFR[2 * n_obs*n_var + 1:(n_obs*n_var),3] = matrix(cfr,ncol = 1, byrow = FALSE)
data_IFR[,2] = rep(rep(state_names, each = n_obs), times = 3)
data_IFR[,1] = rep(c("IFR_1", "IFR_2", "CFR"), each = n_obs * n_var)
data_IFR = data.frame(data_IFR)
data_IFR$value = as.numeric(data_IFR$value)
data_IFR$variable = factor(data_IFR$variable, levels = state_names[order(colMeans(ifr_2))])
data_IFR$type = factor(data_IFR$type, levels = c("IFR_1", "IFR_2", "CFR"))

color_array_2 = ifelse(state_names[order(colMeans(ifr_2))] == "India", "red", "black")

library(DescTools)
gg_colors = c("#F8766D","#00BFC4", "#00BA38" )
library(plyr)
p2 = ggplot(data = data_IFR, aes(x=variable, y=value)) +
  underreporting_theme+
  geom_boxplot(data = function(x){x[x$type %in% c("IFR_1", "IFR_2", "CFR"),]},aes(fill=type, color = type), 
               width = 2, position=position_dodge(width = 0),outlier.alpha = 0)+
  scale_fill_manual(values = gg_colors, labels = c("IFR 1", "IFR 2", "CFR")) +
  scale_color_manual(values =  MixColor(gg_colors,"black" ), labels = c("IFR 1", "IFR 2", "CFR") )+
  scale_y_log10(labels = scales::percent_format(accuracy = 0.01),breaks = c(0,0.000001,0.00001,0.0001,0.001, 0.01, 0.05))+
  ggtitle("Estimated Infection Fatality Rates") +
  ylab("Infection Fatality Rates (Log Scale %)") + 
  theme(axis.text.x = element_text(colour = color_array_2))
p2

ggsave("temp2.png", p2, width = 6, height = 6)



library(ggpubr)
## Flipped coordinate
panel = ggarrange(p1 + coord_flip()+ theme(axis.title.x = element_text(size = 14, face = "italic"),axis.title.y= element_blank(), axis.text.x = element_text(angle= 0, hjust = 0.5),axis.text.y = element_text(colour = color_array_1)),
                  p2 + coord_flip() + theme(axis.title.x = element_text(size = 14, face = "italic"),axis.title.y= element_blank(), axis.text.x = element_text(angle= 0, hjust = 0.5),axis.text.y = element_text(colour = color_array_2)),
                  labels = c("A","B"),font.label = list(size = 20))
ggsave("underreporting_factors.png", panel, width = 15, height = 7)
ggsave("underreporting_factors.pdf", panel, width = 15, height = 7)

