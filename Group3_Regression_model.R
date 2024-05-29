library(haven)
library(data.table)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(tidyr)
library(ggridges)
library(hrbrthemes)
library(ggthemes)
library(plyr)
library(viridis)
library(ggrepel)
library("Hmisc")
library("corrplot")
library(MASS)
library(lmtest)
library(car)


data <- read_dta("E:/Business Forecasting/Assignment 1/Viet-Nam-2023.dta")
financetab_filtered <- data[, c(2:5,171:176, 179:191, 194:215,224)]
financetab_filtered[financetab_filtered < 0] <- NA
# view(financetab_filtered)



#----------------------------------------------------------------

# Fully credit constrained
FCC_1_index <- which(financetab_filtered$k162 == 4 & financetab_filtered$k17 %in% c(2, 3, 4, 5, 6, 7))
FCC_2_index <- which(financetab_filtered$k162 %in% c(1, 2, 3) & financetab_filtered$k20a1 %in% c(3, 4))
financetab_filtered$fully <- 0
financetab_filtered$fully[FCC_1_index] <- 1
financetab_filtered$fully[FCC_2_index] <- 1
# Partially credit constrained
PCC_1_index <- which(financetab_filtered$k162 %in% c(1, 2, 3) & financetab_filtered$k20a1 == 2)
PCC_2_index <- which(financetab_filtered$k162 %in% c(1, 2, 3) & financetab_filtered$k20a1 %in% c(3, 4) & 
                       (financetab_filtered$k5bc + financetab_filtered$k5e + financetab_filtered$k5f + financetab_filtered$k5hdj + 
                          financetab_filtered$k3bc + financetab_filtered$k3f + financetab_filtered$k3e + financetab_filtered$k3hd) > 0)
PCC_3_index <- which(financetab_filtered$k162 == 4 & 
                       financetab_filtered$k17 %in% c(2, 3, 4, 5, 6, 7) & 
                       (financetab_filtered$k5bc + financetab_filtered$k5e + financetab_filtered$k5i + financetab_filtered$k5f + 
                          financetab_filtered$k5hdj + financetab_filtered$k3bc + financetab_filtered$k3f + financetab_filtered$k3e + 
                          financetab_filtered$k3hd) > 0)
financetab_filtered$partially <- 0
financetab_filtered$partially[PCC_1_index] <- 1
financetab_filtered$partially[PCC_2_index] <- 1
financetab_filtered$partially[PCC_3_index] <- 1


financetab_filtered$size_2 <- ifelse(financetab_filtered$a2 == 2, 1, 0)
financetab_filtered$size_3 <- ifelse(financetab_filtered$a2 == 3, 1, 0)
financetab_filtered$sector_1 <- ifelse(financetab_filtered$a4a == 1, 1, 0)
financetab_filtered$sector_2 <- ifelse(financetab_filtered$a4a == 2, 1, 0)
financetab_filtered$sector_3 <- ifelse(financetab_filtered$a4a == 3, 1, 0)
financetab_filtered$sector_4 <- ifelse(financetab_filtered$a4a == 4, 1, 0)
financetab_filtered$sector_5 <- ifelse(financetab_filtered$a4a == 5, 1, 0)
financetab_filtered$sector_6 <- ifelse(financetab_filtered$a4a == 6, 1, 0)
financetab_filtered$region_1 <- ifelse(financetab_filtered$a2 == 1, 1, 0)
financetab_filtered$region_2 <- ifelse(financetab_filtered$a2 == 2, 1, 0)
financetab_filtered$region_3 <- ifelse(financetab_filtered$a2 == 3, 1, 0)
financetab_filtered$region_4 <- ifelse(financetab_filtered$a2 == 4, 1, 0)
financetab_filtered$fin15_1 <- ifelse(financetab_filtered$k6 == 1, 1, 0)
financetab_filtered$fin16_1 <- ifelse(financetab_filtered$k30 == 0, 1, 0)
financetab_filtered$fin16_2 <- ifelse(financetab_filtered$k30 == 1, 1, 0)
financetab_filtered$fin16_3 <- ifelse(financetab_filtered$k30 == 2, 1, 0)
financetab_filtered$fin16_4 <- ifelse(financetab_filtered$k30 == 3, 1, 0)
financetab_filtered$t2_1 <- ifelse(financetab_filtered$k21 == 1, 1, 0)


FCC_1 <- financetab_filtered %>%
  filter(k162 == 4 & k17 %in% c(2, 3, 4, 5, 6, 7))
FCC_2 <- financetab_filtered %>%
  filter(k162 %in% c(1, 2, 3) & k20a1 %in% c(3, 4))
PCC_1 <- financetab_filtered %>%
  filter(k162 %in% c(1, 2, 3) & k20a1 == 2)
PCC_2 <- financetab_filtered %>%
  filter(k162 %in% c(1, 2, 3) & k20a1 %in% c(3, 4) & 
           (k5bc + k5e + k5f + k5hdj + k3bc + k3f + k3e + k3hd) > 0)
PCC_3 <- financetab_filtered %>%
  filter(k162 == 4 & 
           k17 %in% c(2, 3, 4, 5, 6, 7) & 
           (k5bc + k5e + k5i + k5f + k5hdj + k3bc + k3f + k3e + k3hd) > 0)
U_1 <- financetab_filtered %>%
  filter(k162 %in% c(1, 2, 3) & k20a1 == 1)
U_2 <- financetab_filtered %>%
  filter(k162 == 4 & k17 == 1)

new_df <- bind_rows(
  # Fully credit constrained
  FCC_1 %>% mutate(constraint_type = "Fully credit constrained"),
  FCC_2 %>% mutate(constraint_type = "Fully credit constrained"),
  # Partially credit constrained
  PCC_1 %>% mutate(constraint_type = "Partially credit constrained"),
  PCC_2 %>% mutate(constraint_type = "Partially credit constrained"),
  PCC_3 %>% mutate(constraint_type = "Partially credit constrained"),
  # Unconstrained
  U_1 %>% mutate(constraint_type = "Unconstrained"),
  U_2 %>% mutate(constraint_type = "Unconstrained")
)

new_df$fin10 <- with(new_df, ifelse(!is.na(k11) & k11 != 0, (k15a / k11)*100, NA))

colnames(new_df)[colnames(new_df) == "k5a"] <- "fin1"
colnames(new_df)[colnames(new_df) == "k5bc"] <- "fin2"
colnames(new_df)[colnames(new_df) == "k5f"] <- "fin3"
colnames(new_df)[colnames(new_df) == "k5i"] <- "fin4"
colnames(new_df)[colnames(new_df) == "k5e"] <- "fin5_1"
colnames(new_df)[colnames(new_df) == "k5hdj"] <- "fin5_2"
colnames(new_df)[colnames(new_df) == "k3a"] <- "fin6"
colnames(new_df)[colnames(new_df) == "k3bc"] <- "fin7"
colnames(new_df)[colnames(new_df) == "k3f"] <- "fin8"
colnames(new_df)[colnames(new_df) == "k3e"] <- "fin9"
colnames(new_df)[colnames(new_df) == "k11"] <- "loan_value"
colnames(new_df)[colnames(new_df) == "k8"] <- "fin14"

new_df$loan_value<-log(new_df$loan_value)

selected <- c(paste0("size_", 2:3),paste0("sector_", 1:6),paste0("region_", 1:4),
             "fin5_1", "fin5_2",paste0("fin", 1:4),
             "fully", "partially",paste0("fin", 6:9),"loan_value","fin10","fin14","fin15_1",paste0("fin16_", 1:4), "t2_1")
data_rename <- selected[selected %in% colnames(new_df)]
data <- new_df[, data_rename]
View(data)


#----------------------------------------------------------------
ggplot(data, aes(x=loan_value)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="lightblue") 


#Run regression - model 1
model1 <- lm(loan_value ~ . , data = data)
summary(model1)
par(mfrow = c(2,2))
plot(model1)

#Residuals distribution
res <- resid(model1)
plot(model1,2)
hist(res)
shapiro.test(res)

#t-test for mean value of residual
t.test(res, mu=0)

bptest(model1)
durbinWatsonTest(model1)

alias(model1)
model1_alias <- lm(loan_value ~ . - size_2 - size_3 - fin4, data = data)
vif(model1_alias)



#Run regression - model 2
model2 <- lm(loan_value ~ sector_1 + fin1 + fin2 + fin7 + fin9 + fin10 + fin15_1 + t2_1, 
             data = data)
summary(model2)
par(mfrow = c(2,2))
plot(model2)

#Residuals distribution
res2 <- resid(model2)
plot(model2,2)
hist(res2)
shapiro.test(res2)

#t-test for mean value of residual
t.test(res2, mu=0)

bptest(model2)
durbinWatsonTest(model2)

vif(model2)



