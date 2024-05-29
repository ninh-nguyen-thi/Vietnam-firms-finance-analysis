library(haven)
library(data.table)
library(naniar)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(treemap)
library(treemapify)


data <- read_dta("E:/Business Forecasting/Assignment 1/Viet-Nam-2023-full-data.dta")
# View(Viet_Nam_2023_full_data)
financetab <- Viet_Nam_2023_full_data[, c(2:5,171:176, 179:191, 194:215,224)]
# View(financetab)



#--------------------------------------------------
#Data processing
financetab[financetab < 0] <- NA

mv_column <- colSums(is.na(financetab)) # Check for missing values in each column
print("Missing values per column:")
print(mv_column)

ms_check<-financetab[, -c(1:4)]
vis_miss(ms_check)

columns_to_keep <- mv_column <= 1000 #Remove columns having >=1000NA
financetab_filtered <- financetab[, columns_to_keep, with = FALSE]
str(financetab_filtered)
# View(financetab_filtered)

ms_check2<-financetab_filtered[, -c(1:4)]
vis_miss(ms_check2)



#--------------------------------------------------

#Bar chart for fixed assets
financetab_filtered$a6a <- factor(financetab_filtered$a6a)

f1 <- ggplot(data = financetab_filtered, mapping = aes(x = k5a)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% fixed assets funds by: Internal funds", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")
f2 <- ggplot(data = financetab_filtered, mapping = aes(x = k5i)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% fixed assets funds by: Owners’ contribution", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")
f3 <- ggplot(data = financetab_filtered, mapping = aes(x = k5bc)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% fixed assets funds by: Borrowed from banks", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")
f4 <- ggplot(data = financetab_filtered, mapping = aes(x = k5e)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% fixed assets funds by: Borrowed from non-banks", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")
f5 <- ggplot(data = financetab_filtered, mapping = aes(x = k5f)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% fixed assets funds by: Supplier credit & customer advances", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")
f6 <- ggplot(data = financetab_filtered, mapping = aes(x = k5hdj)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% fixed assets funds by: Others", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")

grid.arrange(f1, f2, f3, f4, f5, f6, ncol = 2)




#Bar chart for working capital
w1 <- ggplot(data = financetab_filtered, mapping = aes(x = k3a)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% working capital financed from Internal funds", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")
w2 <- ggplot(data = financetab_filtered, mapping = aes(x = k3bc)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% working capital borrowed from banks", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")
w3 <- ggplot(data = financetab_filtered, mapping = aes(x = k3e)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% working capital borrowed from non-banks", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")
w4 <- ggplot(data = financetab_filtered, mapping = aes(x = k3f)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% working capital purchased on credit/advances from customers", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")
w5 <- ggplot(data = financetab_filtered, mapping = aes(x = k3hd)) +    
  geom_bar(mapping = aes(fill = a6a), color = "yellow", width = 5) +     
  labs(title = "% working capital financed by Others", x = "Percentage", y = "Number of firm") +
  scale_y_continuous(limits = c(0, 400)) +  
  scale_fill_discrete(name = "Sampling size")

grid.arrange(w1, w2, w3, w4, w5, ncol = 2)



#Access to finance as a major constraint (%)
filtered_data <- financetab_filtered %>%
  filter(!is.na(k30)) %>%
  group_by(k30) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(filtered_data, aes(x="", y=n, fill=factor(k30))) +  
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  labs(title = "Access to finance as a major constraint (%)", fill = "Access to finance") +
  scale_fill_manual(values = c("0" = "#DD5746", 
                               "1" = "#F7C566", 
                               "2" = "#00215E", 
                               "3" = "#9AC8CD", 
                               "4" = "#86469C"),
                    labels = c("0" = "No obstacle",
                               "1" = "Minor obstacle",
                               "2" = "Moderate obstacle",
                               "3" = "Major obstacle",
                               "4" = "Very Severe Obstacle")) +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))


#Fully credit constrained
FCC_condition_1 <- financetab_filtered %>%
  filter(k162==4 & k17%in% c(2,3,4,5,6,7))
FCC_condition_2 <- financetab_filtered %>%
  filter(k162%in% c(1,2,3) & k20a1 %in% c(3,4))

count_FCC_condition_1 <- nrow(FCC_condition_1)
count_FCC_condition_2 <- nrow(FCC_condition_2)
print(count_FCC_condition_1)
print(count_FCC_condition_2)


#Partially credit constrained
PCC_condition_1 <- financetab_filtered %>%
  filter(k162 %in% c(1,2,3) & k20a1 == 2)
PCC_condition_2 <- financetab_filtered %>%
  filter(k162 %in% c(1,2,3) & k20a1 %in% c(3,4) & (k5bc + k5e + k5f + k5hdj + k3bc + k3f + k3e + k3hd) > 0)
PCC_condition_3 <- financetab_filtered %>%
  filter(k162 == 4 & k17%in% c(2,3,4,5,6,7) & (k5bc + k5e + k5i + k5f + k5hdj + k3bc + k3f + k3e + k3hd) > 0)

count_PCC_condition_1 <- nrow(PCC_condition_1)
count_PCC_condition_2 <- nrow(PCC_condition_2)
count_PCC_condition_3 <- nrow(PCC_condition_3)
print(count_PCC_condition_1)
print(count_PCC_condition_2)
print(count_PCC_condition_3)


#Unconstrained
U_condition_1 <- financetab_filtered %>%
  filter(k162 %in% c(1,2,3)& k20a1 == 1)
U_condition_2 <- financetab_filtered %>%
  filter(k162 == 4 & k17 == 1)

count_U_condition_1 <- nrow(U_condition_1)
count_U_condition_2 <- nrow(U_condition_2)
print(count_U_condition_1)
print(count_U_condition_2)


piechart_lc <- c(sum_FCC, sum_PCC, sum_U)
labels <- c("Fully credit constrained", "Partially credit constrained", "Unconstrained credit")
percentages <- round(100 * values / sum(values), 1)
labels_percentage <- paste(labels, percentages, "%")
par(mar = c(0.5, 0.5, 0.5, 0.5))
par(cex = 0.8) 
pie(values, labels = labels_percentage,col = c("#DD5746", "#9AC8CD", "#00215E"), main = "Credit constrained level for firms")


#Application's income
R_condition_1 <- financetab_filtered %>%
  filter(k162 %in% c(1,2,3))
R_condition_2 <- financetab_filtered %>% #Reason
  filter(k17 %in% c(2,3,4,5,6,7))

R_condition_1 <- R_condition_1 %>%
  filter(!is.na(k20a1))
R_distribution <- R_condition_1 %>%
  group_by(k20a1) %>%
  summarise(count = n()) %>%
  as.data.frame()


financetab_filtered <- financetab_filtered %>%
  mutate(k20a1 = factor(as.numeric(k20a1), levels = 1:4))
filtered_data <- financetab_filtered %>%
  filter(!is.na(k20a1))
counts <- filtered_data %>%
  group_by(k20a1) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(k20a1, fill = list(count = 0))
total_count <- sum(counts$count)
counts <- counts %>%
  mutate(percentage = (count / total_count) * 100)
ggplot(counts, aes(x = "", y = count, fill = k20a1)) +  
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Application's income", fill = "Type") +
  scale_fill_manual(values = c(
    "1" = "#DD5746", 
    "2" = "#00215E", 
    "3" = "#9AC8CD", 
    "4" = "#86469C"),
    labels = c(
      "1" = "Application was approved in full",
      "2" = "Application was approved in part",
      "3" = "Application was rejected",
      "4" = "Application was withdrawn")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))


#Why did not they apply for any line of credit or loan?
k17_distribution <- R_condition_2 %>%
  group_by(k17) %>%
  summarise(count = n()) %>%
  as.data.frame()

ggplot(k17_distribution, aes(x = factor(k17), y = count, fill = factor(k17))) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Why did not they apply for any line of credit or loan?",
       x = "Type",
       y = "Count",
       fill = "Type") +
  scale_fill_manual(values = c(
    "2" = "#DD5746", 
    "3" = "#00215E", 
    "4" = "#9AC8CD", 
    "5" = "#86469C",
    "6" = "#F7C566",
    "7" = "#135D66"),
    labels = c(
      "2" = "Application procedures were complex", 
      "3" = "Interest rates were not favorable", 
      "4" = "Collateral requirements were too high", 
      "5" = "Size of loan and maturity were insufficient",
      "6" = "Did not think it would be approved",
      "7" = "Other"))




#Approaches to financial resource

k6_con <- financetab_filtered %>%
  filter(k6==1)
k7_con <- financetab_filtered %>%
  filter(k7 == 1)
k82_con <- financetab_filtered %>%
  filter(k82 %in% c(1,2,3))

count_k6_con <- nrow(k6_con)
count_k7_con <- nrow(k7_con)
count_k82_con <- nrow(k82_con)

print(count_k6_con)
print(count_k7_con)
print(count_k82_con)

data_k67 <- data.frame(
  Approach = c("Checking or saving account", "Overdraft facilities"),
  Count = c(count_k6_con, count_k7_con)
)

ggplot(data_k67, aes(x = Approach, y = Count, fill = Approach)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +  # Thêm số vào cột
  labs(title = "", x = "", y = "Number of firms") +
  scale_fill_manual(values = c("#DD5746", "#F7C566")) +
  theme_minimal()




#Firm having checking and/ or saving account

financetab_filtered$a6a <- as.factor(financetab_filtered$a6a)
financetab_filtered$a4a <- as.factor(financetab_filtered$a4a)
financetab_new <- na.omit(financetab_filtered[, c("a6a", "a4a", "k6","k7")])


financetab_new <- na.omit(financetab_filtered[, c("a6a", "a4a", "k6")])
treemap(financetab_filtered, index=c("a6a", "a4a"), vSize="k6", title="Firm having checking and/or saving account", palette="Pastel1")

financetab_new <- na.omit(financetab_filtered[, c("a6a", "a4a", "k7")])
treemap(financetab_filtered, index=c("a6a", "a4a"), vSize="k7", title="Firms having Overdraft Facility", palette="Pastel1")



#Firms having a line of credit or a loan from a financial institution
filtered_data <- subset(financetab_filtered, !is.na(k9) & k82 %in% c(1, 2, 3))
summary_data <- filtered_data %>%
  group_by(k9) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count))
gg <- ggplot(filtered_data, aes(x = "", fill = factor(k9))) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Firms having a line of credit or a loan from a financial institution",
       fill = "Types of financial institution") +
  scale_fill_manual(values = c(
    "1" = "#00215E",
    "2" = "#DD5746",
    "3" = "#F7C566",
    "4" = "#9AC8CD"),
    labels = c(
      "1" = "Private commercial banks ",
      "2" = "State-owned banks or government agency ",
      "3" = "Non-bank financial institutions",
      "4" = "Other"))
gg <- gg +
  geom_text(data = summary_data, aes(label = paste0(round(percentage * 100), "%"), y = count),
            position = position_stack(vjust = 0.5), color = "white", size = 4)
gg


  
#Value of collateral needed for a loan (% of the loan amount)
value_cn <- financetab_filtered[financetab_filtered$k11 <= 1e+12,]
value_cn <- financetab_filtered[financetab_filtered$k15a <= 1e+12,]

v1<-ggplot(value_cn, aes(x = k10, y = k11)) +
  geom_point() +
  labs(title = "Value of loan approved", x = "Index", y = "Values") +
  theme_minimal()  
v2<-ggplot(value_cn, aes(x = k10, y = k15a)) +
  geom_point() +
  labs(title = "Value of collateral required", x = "Index", y = "Values") +
  theme_minimal()  

grid.arrange(v1,v2, ncol = 1)
  

#Proportion of loans requiring collateral
filtered_data <- financetab_filtered[!is.na(financetab_filtered$k13), ]
value_counts <- table(filtered_data$k13)
value_counts_df <- as.data.frame(value_counts)
colnames(value_counts_df) <- c('k13', 'count')
ggplot(value_counts_df, aes(x = "", y = count, fill = k13)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(scales::percent(count/sum(count)))), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Financial institution requiring collateral", fill = "Collateral Required") +
  scale_fill_manual(values = c("1" = "#00215E", "2" = "#9AC8CD")) 



#Distribution of collateral required
collateral_counts <- financetab_filtered %>%
  summarise(
    "Accounts receivable and inventories" = sum(k14a == 1, na.rm = TRUE),
    "Land, buildings under ownership of the establishment" = sum(k14b == 1, na.rm = TRUE),
    "Machinery and equipment including movable" = sum(k14c == 1, na.rm = TRUE),
    "Other forms of collateral not included in the categories above" = sum(k14d == 1, na.rm = TRUE),
    "Personal assets of owner (house, etc)" = sum(k14e == 1, na.rm = TRUE)
  )
collateral_df <- pivot_longer(collateral_counts, cols = everything(),
                              names_to = "Types_of_collateral", 
                              values_to = "Count")
ggplot(collateral_df, aes(x = Types_of_collateral, y = Count, fill = Types_of_collateral)) +
  geom_bar(stat = "identity") +
  labs(title = "Types of collateral required",
       x = "Types of Collateral",
       y = "Count") +
  scale_fill_manual(values = c("Accounts receivable and inventories" = "#DD5746",
                               "Land, buildings under ownership of the establishment" = "#F7C566",
                               "Machinery and equipment including movable" = "#00215E",
                               "Other forms of collateral not included in the categories above" = "#9AC8CD",
                               "Personal assets of owner (house, etc)" = "#86469C")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()


