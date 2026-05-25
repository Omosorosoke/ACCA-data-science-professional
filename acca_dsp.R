install.packages("Tidyverse")
install.packages("gt")

library(tidyverse)
library(gt)
acca <- read_csv("SDS Data Science Professional specimen exam - question 1, appendix 2.csv")
names(acca)

glimpse(acca)

# Data analyses strategy
# Prompt AI  for possible 
# Transaction date to date , customer age and is_VIP
# S3 vector, Atomic vector, list vector, NULL. Vector attributes
typeof(acca$transaction_date)

# Check for missing values and address them 

# Answer your questions
is.character(acca$billing_country)


# Clean these columns
# actual_fraud to integer
# label the value of actual_fraud
# transaction_date to date and time
# And answer your questions

acca |> mutate(
  transaction_date = mdy(transaction_date),
  actual_fraud = factor(actual_fraud, levels = c(0, 1), labels = c("non fraud", "fraud"))) |> 
  relocate(.before = transaction_id)





View(acca)
# What percentage of transactions are fraudulent?  (actual fraud)
# What is the total financial exposure to fraud?  (amount)
# What is the average fraud transaction amount?  (amount)
# Are fraudulent transactions increasing over time (data and time)

acca |> count(actual_fraud, sort = T)

acca |> group_by(actual_fraud) |> 
  summarise(
    num = n(),
    fraud_prop = num/nrow(acca) * 100,
    total_amount = sum(amount)
  ) |> gt()

?gt()

?sum()
?nrow()