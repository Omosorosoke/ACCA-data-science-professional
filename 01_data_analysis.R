library(tidyverse)
library(gt)
install.packages("gtExtras")
library(gtExtras)
library("ggthemes")
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
) |> filter(actual_fraud == "fraud") |> group_by(day) |> 
  summarise(
    total_fraud = sum(amount),
    incidence = n()
  ) |> 
   ggplot() +
   geom_point(aes(x= total_fraud)) # The amount involved in fraudulent transaction is betweeen 50 to 100 units of the currency amount


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
  is_vip = factor(is_vip, levels = c(0, 1), labels = c("non vip", "vip")))

data_customer |> filter(actual_fraud == "fraud") |> 
  count(customer_age, sort = T) # Customer age 26 and 42 experience the most fraud with two incidences per age group

data_customer |> filter(actual_fraud == "fraud") |> 
  count(customer_age, wt = amount, sort = T) # Customer age 24 and 59 list the most to fraud with 217 and 194 units of the currency value involved

# Which customers generate the highest fraud losses?
data_customer_losses <- acca |> select(amount, customer_id, actual_fraud, currency)
data_customer_losses |> filter(actual_fraud == "fraud") |> count(customer_id, currency, sort = T, wt = amount, name = "Total loss") |> head(9) |> 
  gt() |> gt_highlight_rows(
    rows = c(1:6), 
    fill = "lightgrey", 
    alpha = 0.3, 
    font_weight = "normal", 
    font_color = "navyblue") |> 
  tab_header(title = "Fraud losses by customers", subtitle = "Top six victims") |> 
  tab_style(
    style = cell_borders(
      sides = c("top"), 
      weight = px(1.5), 
      style = "solid" 
    ),
    locations = list(
      cells_body(rows = 1)
    )  
) 

?tab_style()
?gt()

?cell_borders()
# Merchant Risk Analysis
# Which merchant categories have the highest fraud rates?
data_merchant <- acca |> select(merchant_id, merchant_category, merchant_riskscore, actual_fraud, amount) 
data_merchant |> filter(actual_fraud == "fraud") |> count(merchant_category, actual_fraud, wt = amount, sort = T, name = "fraud_amount") |> ggplot(aes(x = fct_reorder(merchant_category, fraud_amount, .desc = T), y = fraud_amount)) +
  geom_col() +
  scale_colour_viridis_c() +
  coord_flip()

# Does merchant risk score correlate with actual fraud?
acca |> select(merchant_riskscore, actual_fraud, amount) |> ggplot(aes( x = actual_fraud, y = merchant_riskscore, color = actual_fraud)) +
  geom_boxplot(outlier.shape = 1, outlier.color = "magenta") +
  theme_minimal() +
  scale_color_colorblind()  +
  labs(title = "Customer transaction fraud", subtitle = "Incidence is more pronounced highly risky merchant",
  )



?gt_highlight_rows()
