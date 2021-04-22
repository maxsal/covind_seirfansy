library(ggplot2)
state_codes = c("AN", "AP", "AS", "BR", "CH", "CT", "DN", "DL", "GA", "GJ", "HR", "HP" ,"JK", "JH", "KA", "KL", "LA", 
                "MP", "MH", "MN", "ML", "MZ", "NL", "OR", "PY", "PB", "RJ" ,"SK" ,"TN" ,"TG", "TR", "UP", "UT", "WB", "India")
state_names = c("Andaman and Nicobar", "Andhra Pradesh", "Assam", "Bihar", "Chandigarh", "Chhattisgarh",
                "Dadra and Nagar Haveli", "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu and Kashmir", 
                "Jharkhand", "Karnataka", "Kerala","Ladakh", "Madhya Pradesh","Maharashtra","Manipur","Meghalaya","Mizoram",
                "Nagaland","Odisha","Puducherry","Punjab","Rajasthan","Sikkim","Tamil Nadu","Telangana","Tripura","Uttar Pradesh",
                "Uttarakhand","West Bengal","India")
n_var = ifelse(length(state_codes) == length(state_names), length(state_codes), stop("length not equal"))
n_obs = 1000

setwd("result files")
my_diff <- function(x){c(2*diff(x)[1]-diff(x)[2], diff(x))}

n_date = 426-366+1

data_case = matrix(nrow = n_var, ncol = n_date)
data_death = matrix(nrow = n_var, ncol = n_date)

for(i in 1:length(state_codes)){
  state_code = state_codes[i]
  state_name = state_names[i]  
  prediction = readRDS(paste0("Prediction_",state_code, ".rds"))
  P_pred = rowMeans(prediction[515*3+366:426,])
  R_pred = rowMeans(prediction[515*6+366:426,])
  D_pred = rowMeans(prediction[515*8+366:426,])
  total_rep_case = R_pred+D_pred+P_pred
  total_rep_death = D_pred
  daily_rep_case = my_diff(total_rep_case)
  daily_rep_death = my_diff(total_rep_death)
  data_case[i,] = daily_rep_case  
  data_death[i,] = daily_rep_death
}

final_theme <-   theme_minimal() +
  theme(
    plot.title         = element_text(size = 20, face = "bold"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = element_text(hjust = 0, size = 10, lineheight = 1.1),
    axis.text          = element_text(size = 18, color = "#36454f"),
    axis.title         = element_text(size = 20, face = "italic"),
    legend.title       = element_blank() ,
    legend.text        = element_text(size = 16) ,
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    axis.ticks = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.line = element_line()
  )

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

## Cases

cc <- scales::seq_gradient_pal("red", "blue", "Lab")(seq(0,1,length.out=n_var-1))
cc <- c(cc[1:12], "#000000", cc[13:n_var])
  
dates = seq(as.Date("2021-04-01"), as.Date("2021-05-31"), by = 1)
data_case_new = data.frame("value" = c(t(data_case)), 
                           "state" = rep(as.factor(state_names), each = n_date), #, labels = state_names
                           "dates" = rep(dates, times = n_var),
                           line_width = rep(c(rep("1", n_var - 1), "2"), each = n_date))

p1 <- ggplot(data = data_case_new, aes(x = dates, y = value)) + 
  geom_line(aes(col = state, size = line_width)) + 
  scale_size_manual(values = c(1.2, 2)) + 
  final_theme +
  scale_color_manual(values = cc) + 
  labs(
    title = "Projected daily new cases",
    x     = "dates",
    y     = "Cases"
  ) +
  scale_y_continuous(labels = addUnits) +
  guides(size = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 2)))
  
p1

# ggsave(filename = here("case_all.png"), plot = p1, units = "in", width = 12, height = 12, dpi = "retina")

## Deaths 

dates = seq(as.Date("2021-04-01"), as.Date("2021-05-31"), by = 1)
data_death_new = data.frame("value" = c(t(data_death)), 
                           "state" = rep(as.factor(state_names), each = n_date), #, labels = state_names
                           "dates" = rep(dates, times = n_var),
                           line_width = rep(c(rep("1", n_var - 1), "2"), each = n_date))

p2 <- ggplot(data = data_death_new, aes(x = dates, y = value)) + 
  geom_line(aes(col = state, size = line_width)) + 
  scale_size_manual(values = c(1.2, 2)) + 
  final_theme +
  scale_color_manual(values = cc) + 
  labs(
    title = "Projected daily new deaths",
    x     = "dates",
    y     = "Deaths"
  ) +
  scale_y_continuous(labels = addUnits) +
  guides(size = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 2)))

p2

# ggsave(filename = here("death_all.png"), plot = p2, units = "in", width = 12, height = 12, dpi = "retina")

library(ggpubr)
library(gridExtra)
png(here("all_panel.png"), width = 24, height = 12, units = "in", res = 320)
grid_plot = ggarrange(p1, p2, nrow = 1, common.legend = TRUE, labels = c("A", "B"), legend = "bottom", font.label = list(size = 20))
grid_plot
dev.off()


#########################################################################################################################
## Without India
#########################################################################################################################

## Cases

cc <- scales::seq_gradient_pal("red", "blue", "Lab")(seq(0,1,length.out=n_var-1))
colfunc<-colorRampPalette(c("blue", "red", "green"))

dates = seq(as.Date("2021-04-01"), as.Date("2021-05-31"), by = 1)
data_case_new = data.frame("value" = c(t(data_case))[ -c((n_var-1)*(n_date) + 1:(n_date))], 
                           "state" = rep(as.factor(state_names[-n_var]), each = n_date), #, labels = state_names
                           "dates" = rep(dates, times = n_var-1),
                           line_width = rep(c(rep("1", n_var - 1)), each = n_date))

p3 <- ggplot(data = data_case_new, aes(x = dates, y = value)) + 
  geom_line(aes(col = state, size = line_width)) + 
  scale_size_manual(values = c(1.2, 2)) + 
  final_theme +
  scale_color_manual(values = colfunc(n_var-1)) + 
  labs(
    title = "Projected daily new cases",
    x     = "dates",
    y     = "Cases"
  ) +
  scale_y_continuous(labels = addUnits) +
  guides(size = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 2)))+
  scale_color_viridis_d(option = "plasma")

p3

# ggsave(filename = here("case_state.png"), plot = p3, units = "in", width = 12, height = 12, dpi = "retina")

## Deaths 

dates = seq(as.Date("2021-04-01"), as.Date("2021-05-31"), by = 1)
data_death_new = data.frame("value" = c(t(data_death))[ -c((n_var-1)*(n_date) + 1:(n_date))], 
                            "state" = rep(as.factor(state_names[-n_var]), each = n_date), #, labels = state_names
                            "dates" = rep(dates, times = n_var-1),
                            line_width = rep(c(rep("1", n_var - 1)), each = n_date))

p4 <- ggplot(data = data_death_new, aes(x = dates, y = value)) + 
  geom_line(aes(col = state, size = line_width)) + 
  scale_size_manual(values = c(1.2, 2)) + 
  final_theme +
  scale_color_manual(values = colfunc(n_var-1)) + 
  labs(
    title = "Projected daily new deaths",
    x     = "dates",
    y     = "Deaths"
  ) +
  scale_y_continuous(labels = addUnits) +
  guides(size = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 2)))+
  scale_color_viridis_d(option = "plasma")

p4

# ggsave(filename = here("death_state.png"), plot = p4, units = "in", width = 12, height = 12, dpi = "retina")

library(ggpubr)
library(gridExtra)
png(here("state_panel.png"), width = 24, height = 12, units = "in", res = 320)
grid_plot = ggarrange(p3, p4, nrow = 1, common.legend = TRUE, labels = c("A", "B"), legend = "bottom", font.label = list(size = 20))
grid_plot
dev.off()

# ggsave(here("effect_of_misclassification_delhi_panel.pdf"), grid_plot, width = 24, height = 12, units = "in", dpi = 300)

#########################################################################################################################################
## Panel case plot
#########################################################################################################################################
big_panel_theme <-   theme_minimal() +
  theme(
    plot.title         = element_text(size = 20, face = "bold", color = "#2FA100"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = element_text(hjust = 0, size = 10, lineheight = 1.1),
    axis.text          = element_text(size = 18, color = "#36454f"),
    axis.title         = element_text(size = 20, face = "italic"),
    legend.title       = element_blank() ,
    legend.text        = element_text(size = 16) ,
    # panel.grid.major.x = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    axis.ticks = element_line(),
    axis.ticks.length = unit(5, "pt"),
    # axis.line = element_line()
  )

p1 <- as.list(c(1:n_var))
color_matrix = diag(1, n_var)

dates = seq(as.Date("2021-04-01"), as.Date("2021-05-31"), by = 1)
data_case_big_panel = data.frame("value" = c(t(data_case)), 
                           "state" = rep(as.factor(state_names), each = n_date), #, labels = state_names
                           "dates" = rep(dates, times = n_var),
                           color = as.factor(rep(1:n_var, each = n_date)))

for(i in 1:n_var){
  p1[[i]] <- ggplot(data = data_case_big_panel, aes(x = dates, y = value)) + 
    geom_line(aes(col = color, size = color)) + 
    geom_line(data = data_case_big_panel[(n_date*(i-1)+1):(n_date*i), ], color = 'black', size = 2)+
    geom_point(data = data_case_big_panel[(n_date*(i-1)+1):(n_date*i), ], size = 3, color = "black", alpha = c(rep(0, n_date-1), 1))+
    scale_size_manual(values = c(1.2, 2)[1+color_matrix[,i]]) + 
    big_panel_theme +
    scale_color_manual(values = c("grey", "black")[1+color_matrix[,i]]) + 
    labs(
      title = paste0(state_names[i]),
      x     = " ",
      y     = " "
    ) +
    scale_x_date(breaks = as.Date(c("2021-04-01", "2021-05-01","2021-05-31")), date_labels = "%B %e") +
    scale_y_continuous(labels = addUnits) +
    guides(size = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 2)))
    
}
library(here)
library(ggpubr)
library(gridExtra)
png(here("big_panel_case.png"), width = 28, height = 14, units = "in", res = 320)
grid_plot = ggarrange(
  p1[[1]], p1[[2]], p1[[3]], p1[[4]], p1[[5]], p1[[6]], p1[[7]],
  p1[[7+1]], p1[[7+2]], p1[[7+3]], p1[[7+4]], p1[[7+5]], p1[[7+6]], p1[[7+7]],
  p1[[14+1]], p1[[14+2]], p1[[14+3]], p1[[14+4]], p1[[14+5]], p1[[14+6]], p1[[14+7]],
  p1[[21+1]], p1[[21+2]], p1[[21+3]], p1[[21+4]], p1[[21+5]], p1[[21+6]], p1[[21+7]],
  p1[[28+1]], p1[[28+2]], p1[[28+3]], p1[[28+4]], p1[[28+5]], p1[[28+6]], p1[[28+7]],
  nrow = 5, ncol = 7, legend = "none", font.label = list(size = 20))
grid_plot
dev.off()

## Deaths
p2 <- as.list(c(1:n_var))
color_matrix = diag(1, n_var)


dates = seq(as.Date("2021-04-01"), as.Date("2021-05-31"), by = 1)
data_death_big_panel = data.frame("value" = c(t(data_death)), 
                                 "state" = rep(as.factor(state_names), each = n_date), #, labels = state_names
                                 "dates" = rep(dates, times = n_var),
                                 color = as.factor(rep(1:n_var, each = n_date)))

for(i in 1:n_var){
  p2[[i]] <- ggplot(data = data_death_big_panel, aes(x = dates, y = value)) + 
    geom_line(aes(col = color, size = color)) + 
    geom_line(data = data_death_big_panel[(n_date*(i-1)+1):(n_date*i), ], color = 'black', size = 2)+
    geom_point(data = data_death_big_panel[(n_date*(i-1)+1):(n_date*i), ], size = 3, color = "black", alpha = c(rep(0, n_date-1), 1))+
    scale_size_manual(values = c(1.2, 2)[1+color_matrix[,i]]) + 
    big_panel_theme +
    scale_color_manual(values = c("grey", "black")[1+color_matrix[,i]]) + 
    labs(
      title = paste0(state_names[i]),
      x     = " ",
      y     = " "
    ) +
    scale_x_date(breaks = as.Date(c("2021-04-01", "2021-05-01","2021-05-31")), date_labels = "%B %e") +
    scale_y_continuous(labels = addUnits) +
    guides(size = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 2)))
  
}
library(here)
library(ggpubr)
library(gridExtra)
png(here("big_panel_death.png"), width = 28, height = 14, units = "in", res = 320)
grid_plot = ggarrange(
  p2[[1]], p2[[2]], p2[[3]], p2[[4]], p2[[5]], p2[[6]], p2[[7]],
  p2[[7+1]], p2[[7+2]], p2[[7+3]], p2[[7+4]], p2[[7+5]], p2[[7+6]], p2[[7+7]],
  p2[[14+1]], p2[[14+2]], p2[[14+3]], p2[[14+4]], p2[[14+5]], p2[[14+6]], p2[[14+7]],
  p2[[21+1]], p2[[21+2]], p2[[21+3]], p2[[21+4]], p2[[21+5]], p2[[21+6]], p2[[21+7]],
  p2[[28+1]], p2[[28+2]], p2[[28+3]], p2[[28+4]], p2[[28+5]], p2[[28+6]], p2[[28+7]],
  nrow = 5, ncol = 7, legend = "none", font.label = list(size = 20))
grid_plot
dev.off()

#####################################
## Only states
#####################################

## case
p3 <- as.list(c(1:n_var))
color_matrix = diag(1, (n_var-1))

dates = seq(as.Date("2021-04-01"), as.Date("2021-05-31"), by = 1)
data_case_big_panel = data.frame("value" = c(t(data_case))[ -c((n_var-1)*(n_date) + 1:(n_date))], 
                           "state" = rep(as.factor(state_names[-n_var]), each = n_date), #, labels = state_names
                           "dates" = rep(dates, times = n_var-1),
                           "color" = as.factor(rep(1:(n_var-1), each = n_date)))

for(i in 1:(n_var-1)){
  p3[[i]] <- ggplot(data = data_case_big_panel, aes(x = dates, y = value)) + 
    geom_line(aes(col = color, size = color)) + 
    geom_line(data = data_case_big_panel[(n_date*(i-1)+1):(n_date*i), ], color = 'black', size = 2)+
    geom_point(data = data_case_big_panel[(n_date*(i-1)+1):(n_date*i), ], size = 3, color = "black", alpha = c(rep(0, n_date-1), 1))+
    scale_size_manual(values = c(1.2, 2)[1+color_matrix[,i]]) + 
    big_panel_theme +
    scale_color_manual(values = c("grey", "black")[1+color_matrix[,i]]) + 
    labs(
      title = paste0(state_names[i]),
      x     = " ",
      y     = " "
    ) +
    scale_x_date(breaks = as.Date(c("2021-04-01", "2021-05-01","2021-05-31")), date_labels = "%B %e") +
    scale_y_continuous(labels = addUnits) +
    guides(size = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 2)))
  
}
library(here)
library(ggpubr)
library(gridExtra)
png(here("big_panel_case_state.png"), width = 28, height = 14, units = "in", res = 320)
grid_plot = ggarrange(
  p3[[1]], p3[[2]], p3[[3]], p3[[4]], p3[[5]], p3[[6]], p3[[7]],
  p3[[7+1]], p3[[7+2]], p3[[7+3]], p3[[7+4]], p3[[7+5]], p3[[7+6]], p3[[7+7]],
  p3[[14+1]], p3[[14+2]], p3[[14+3]], p3[[14+4]], p3[[14+5]], p3[[14+6]], p3[[14+7]],
  p3[[21+1]], p3[[21+2]], p3[[21+3]], p3[[21+4]], p3[[21+5]], p3[[21+6]], p3[[21+7]],
  p3[[28+1]], p3[[28+2]], p3[[28+3]], p3[[28+4]], p3[[28+5]], p3[[28+6]],
  nrow = 5, ncol = 7, legend = "none", font.label = list(size = 20))
grid_plot
dev.off()

## death
p4 <- as.list(c(1:n_var))
color_matrix = diag(1, (n_var-1))

dates = seq(as.Date("2021-04-01"), as.Date("2021-05-31"), by = 1)
data_death_big_panel = data.frame("value" = c(t(data_death))[ -c((n_var-1)*(n_date) + 1:(n_date))], 
                                 "state" = rep(as.factor(state_names[-n_var]), each = n_date), #, labels = state_names
                                 "dates" = rep(dates, times = n_var-1),
                                 "color" = as.factor(rep(1:(n_var-1), each = n_date)))

for(i in 1:(n_var-1)){
  p4[[i]] <- ggplot(data = data_death_big_panel, aes(x = dates, y = value)) + 
    geom_line(aes(col = color, size = color)) + 
    geom_line(data = data_death_big_panel[(n_date*(i-1)+1):(n_date*i), ], color = 'black', size = 2)+
    geom_point(data = data_death_big_panel[(n_date*(i-1)+1):(n_date*i), ], size = 3, color = "black", alpha = c(rep(0, n_date-1), 1))+
    scale_size_manual(values = c(1.2, 2)[1+color_matrix[,i]]) + 
    big_panel_theme +
    scale_color_manual(values = c("grey", "black")[1+color_matrix[,i]]) + 
    labs(
      title = paste0(state_names[i]),
      x     = " ",
      y     = " "
    ) +
    scale_x_date(breaks = as.Date(c("2021-04-01", "2021-05-01","2021-05-31")), date_labels = "%B %e") +
    scale_y_continuous(labels = addUnits) +
    guides(size = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 2)))
  
}
library(here)
library(ggpubr)
library(gridExtra)
png(here("big_panel_death_state.png"), width = 28, height = 14, units = "in", res = 320)
grid_plot = ggarrange(
  p4[[1]], p4[[2]], p4[[3]], p4[[4]], p4[[5]], p4[[6]], p4[[7]],
  p4[[7+1]], p4[[7+2]], p4[[7+3]], p4[[7+4]], p4[[7+5]], p4[[7+6]], p4[[7+7]],
  p4[[14+1]], p4[[14+2]], p4[[14+3]], p4[[14+4]], p4[[14+5]], p4[[14+6]], p4[[14+7]],
  p4[[21+1]], p4[[21+2]], p4[[21+3]], p4[[21+4]], p4[[21+5]], p4[[21+6]], p4[[21+7]],
  p4[[28+1]], p4[[28+2]], p4[[28+3]], p4[[28+4]], p4[[28+5]], p4[[28+6]],
  nrow = 5, ncol = 7, legend = "none", font.label = list(size = 20))
grid_plot
dev.off()
