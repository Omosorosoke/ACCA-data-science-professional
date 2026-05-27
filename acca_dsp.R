install.packages("Tidyverse")
install.packages("gt")

library(tidyverse)
library(gt)
acca <- read_csv("SDS Data Science Professional specimen exam - question 1, appendix 2.csv")

names(acca)

glimpse(acca)


# Check for missing values and address them 

# Data Analysis strategy

# 1 select relevant column
# 
# 2 clean these columns.
# actual_fraud to integer
# label the values of actual_fraud
# Covert the transaction_date to date format type

# 3 And answer your questions

# Appropriate data type
acca <- acca |> mutate(
  transaction_date = mdy(transaction_date),
  actual_fraud = factor(actual_fraud, levels = c(0, 1), labels = c("non fraud", "fraud")))


# What percentage of transactions are fraudulent?  (actual fraud)
# What is the total financial exposure to fraud?  (amount)
fraud_data <- acca |> group_by(actual_fraud) |> 
  summarise(
    num = n(),
    fraud_prop = round(num/nrow(acca) * 100, digits = 1),
    avrg_fraud_value = round(mean(amount, na.rm = T), digits = 1),
    total_amount = format(sum(amount), big.mark = ",")
  ) |> gt()

fraud_data

# Fraud trend over the period
 acca |> mutate(
  day = day(transaction_date)
) |> filter(actual_fraud == "fraud") |> group_by(day) |> 
  summarise(
    total_fraud = sum(amount),
    incidence = n()
  ) |> 
   ggplot() +
   geom_line(aes(x = day, y = incidence))

# Fraud trend over the period
# Are fraudulent transactions increasing over time (data and time)

 acca |> mutate(
  day = day(transaction_date) |> filter(actual_fraud == "fraud")) |> group_by(day) |> 
  summarise(
    total_fraud = sum(amount),
    incidence = n()
  ) |> 
   ggplot() +
   geom_line(aes(x = day, y = total_fraud))

