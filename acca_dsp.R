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
  actual_fraud = as.logical(actual_fraud))

View(acca |> filter(actual_fraud == T))

# What percentage of transactions are fraudulent?  (actual fraud)
# What is the total financial exposure to fraud?  (amount)
fraud_data <- acca |> group_by(actual_fraud) |> 
  summarise(
    num = n(),
    proportion = round(num/nrow(acca) * 100, digits = 1),
    value = round(mean(amount, na.rm = T), digits = 1),
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
   geom_point(aes(x = day, y = incidence)) # The incidence of fraud incidence has been stable overtime; static at one or two per day

# Fraud trend over the period
# Are fraudulent transactions increasing over time (data and time)
 acca |> mutate(
  day = day(transaction_date)
) |> filter(actual_fraud == T) |> group_by(day) |> 
  summarise(
    total_fraud = sum(amount),
    incidence = n()
  ) |> 
   ggplot() +
   geom_point(aes(x = day, y = total_fraud)) # The amount involved in fraudulent transaction is betweeen 50 to 100 units of the currency amount


glimpse(acca)

 
?n()

# Customer risk analysis
# Which customer age groups experience the most fraud?
# Are VIP customers more or less likely to experience fraud?
# Does customer tenure reduce fraud risk?
# Which customers generate the highest fraud losses?

# Data analysis strategy
# Variables (customer_tenure_days, is_vip, customer_id, customer_age    ) 
# data type - is_vip to factor, confirm the distinct number 
data_customer <- acca |> select(customer_tenure_days, is_vip, customer_id, customer_age, amount, actual_fraud)

glimpse(data_customer)


# Which customer age groups experience the most fraud?
data_customer <- data_customer |> mutate(
  is_vip = as.logical(is_vip)

data_customer |> filter(actual_fraud == T) |> 
  count(customer_age, sort = T) |> gt() # Customer age 26 and 42 experience the most fraud with two incidences per age group

data_customer |> filter(actual_fraud == T) |> 
  count(customer_age, wt = amount, name = "sum_loss", sort = T) |> gt() # Customer age 24 and 59 list the most to fraud with 217 and 194 units of the currency value involved

?count()

data_customer |> filter(actual_fraud == T) |>
  group_by(customer_age) |> 
  summarise(
    fraud_count = n(),
    value_lost = round(sum(amount))
  ) |> arrange(desc(value_lost)) |> gt() # Customer age 24 and 59 list the most to fraud with 217 and 194 units of the currency value involved


## Are VIP customers more or less likely to experience fraud?
## Variables (vip, non - vip, actual fraud)
data_vip_customer <- acca |> select(is_vip, actual_fraud, amount)

data_vip_customer |> filter(actual_fraud == T & is_vip == 1) # There are no fraud incidence for VIP personality

data_fraud_vip <- data_vip_customer |> filter(is_vip == 1)

data_fraud_non_vip <- data_vip_customer |> filter(is_vip == 0) 

data_fraud_vip |> count(is_vip, sort = T)

data_fraud_non_vip|> count(is_vip, sort = T)

data_fraud_non_vip |> count(actual_fraud, sort = T)
data_fraud_non_vip |> group_by(actual_fraud) |> 
  summarise(
    fraud_count = n(),
    fraud_prop = round(fraud_count/nrow(data_fraud_non_vip), digits = 2) 
  ) |> gt() # 98% of of non-vip customer reported fraud

data_fraud_vip |> count(actual_fraud, sort = T) # No VIP customer was involved in Fraud

# Does customer tenure reduce fraud risk?
# variables (actual fraud, customer tenure in days) two sample T test or Mann - Whitney U test

