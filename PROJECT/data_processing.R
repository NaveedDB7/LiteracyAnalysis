install.packages("ggcorrplot")
install.packages("rpart.plot")
library(ggcorrplot)
library(rpart.plot)



data <- read_csv("C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\project.csv")

data <-read_csv("C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\data.csv")

colnames(data)



head(data)
data <- data %>% arrange(desc(literacyTotal))

data <- data %>%
  mutate(literacy_group = 5-ntile(literacyTotal, 4))  # Divides into 4 groups

data$Literacy_gap <- data$literacyM - data$literacyF

data1 <- data %>% filter(literacy_group == 1)  # Highest literacy group
data2 <- data %>% filter(literacy_group == 2)
data3 <- data %>% filter(literacy_group == 3)
data4 <- data %>% filter(literacy_group == 4)  # Lowest literacy group

data <- data[, c(1,2,3,4,6,7,8,9,10,11,5)]
head(data)

correlation_matrix <- cor(data[, c("literacyTotal", "Literacy_gap", "BPL", 
                                      "Turnout", "CRW", "ChildMarr")], 
                          use = "complete.obs")

print(correlation_matrix)


ggcorrplot(correlation_matrix, lab = TRUE, colors = c("blue", "white", "red"))


# Define groups based on literacyTotal quantiles
high_lit <- subset(data, literacyTotal >= quantile(data$literacyTotal, 2/3))
medium_lit <- subset(data, literacyTotal >= quantile(data$literacyTotal, 1/3) & 
                       literacyTotal < quantile(data$literacyTotal, 2/3))
low_lit <- subset(data, literacyTotal < quantile(data$literacyTotal, 1/3))

#  correlation matrices for each group
cor_high <- cor(high_lit[, c("literacyTotal", "Literacy_gap", "BPL", "Turnout", "CRW", "ChildMarr")], use = "complete.obs")
cor_medium <- cor(medium_lit[, c("literacyTotal", "Literacy_gap", "BPL", "Turnout", "CRW", "ChildMarr")], use = "complete.obs")
cor_low <- cor(low_lit[, c("literacyTotal", "Literacy_gap", "BPL", "Turnout", "CRW", "ChildMarr")], use = "complete.obs")


print("High Literacy Group Correlation:")
print(cor_high)

print("Medium Literacy Group Correlation:")
print(cor_medium)

print("Low Literacy Group Correlation:")
print(cor_low)




#t tests

t_test_lit_gap <- t.test(data$Literacy_gap, data$BPL)
print(t_test_lit_gap)

t_test_lit_gap <- t.test(data$Literacy_gap, data$CRW)
print(t_test_lit_gap)

t_test_lit_gap <- t.test(data$Literacy_gap, data$ChildMarr)
print(t_test_lit_gap)

t_test_lit_gap <- t.test(data$Literacy_gap, data$Turnout)
print(t_test_lit_gap)

#multiple regression , as there are multiple variables and lots of redundant computing, 
#only one variable is computed here and the rest was computed with the help of AI tools.

lm_model <- lm(BPL ~ literacyTotal + Literacy_gap, data = data)
summary(lm_model)


plot(lm_model, which=1)



#decision tree

library(rpart)
dt_model <- rpart(BPL ~ literacyTotal + Literacy_gap, data = data, method = "anova")
print(dt_model)
rpart.plot(dt_model)







write_csv(data , "C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\data.csv")
