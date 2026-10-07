# Load packages ----------------------------------------------------------

library(tidyverse)
library(gt)
library(gtExtras)
library(ggthemes)
library(janitor)
library(hrbrthemes)
library(scales)
library(marquee)
library(ggrepel)
library(flextable)
library(gtExtras)
library(officer) # Work with flexatable


# Import data and keep a raw copy------------------------------------------------------------

acca_data <- read_csv(
  "SDS Data Science Professional specimen exam - question 1, appendix 2.csv"
)

acca_data_raw <- acca_data # keep a copy of the raw data

# Explore data -----------------------------------------------------------

glimpse(acca_data)

# Check columns for proper names, appropriate data types  and recode values
acca_data_rds <- acca_data |>
  mutate(
    transaction_date = mdy(transaction_date),
    actual_fraud = factor(
      actual_fraud,
      levels = c(0, 1),
      labels = c("non fraud", "fraudulent") # convert to factors and label values
    )
  )

# Count the number of unique values for certain columns of interest. This gives idea of how much variation there is.
acca_data_rds |>
  summarise(across(
    c(
      email_domain,
      currency,
      is_cross_border,
      product_type,
      payment_method,
      channel,
      merchant_category,
      customer_id,
      billing_country,
      shipping_country
    ),
    .fns = ~ n_distinct(.x), #This counts the number of unique values
    .names = "{.col}"
  )) |>
  pivot_longer(
    everything(),
    names_to = "items",
    values_to = "unique_values",
  ) # This would enable you direct the focus of your analysis while others have not missing values.
# It may also not be valuable to analyse customer_id because of its size; 1092 unique customers id

# Check for missing values and address them

### Questions and analysis -------------------------------------------------

# What proportion of transactions are fraudulent.
# Proportion of fraudulent transactions.
acca_data_rds |>
  count(actual_fraud) |>
  mutate(fraud_incidence_prop = n * 100 / nrow(acca_data)) # The overall fraud rate for the month is about 2% of the total transaction.

# What percentage of transactions are fraudulent?  (actual fraud)
# What is the total financial exposure to fraud?  (amount)

# Fraud incidence by currency, total fraud losses and fraud rate (Flextable)
# What is fraud incidence by currency. Are there differences in  total fraud rate and losses reported by currency?
fraud_data <- acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  group_by(currency) |>
  summarise(
    fraud_count = n(),
    total_fraud_loss = format(sum(amount), big.mark = ","),
    average_fraud_loss = round(mean(amount, na.rm = T), digits = 1),
    fraud_rate = round(fraud_count / nrow(acca_data_rds) * 100, digits = 1)
  ) |>
  flextable() |>
  set_header_labels(
    currency = "Currency",
    fraud_count = "Fraud count",
    total_fraud_loss = "Total fraud loss",
    average_fraud_loss = "Average fraud loss",
    fraud_rate = " Fraud rate"
  ) |>
  align(j = (2:5), align = "center") |>
  autofit() |>
  add_header_lines("Fraud incidence by currency", top = TRUE) |> # Header title
  align(part = "header", align = 'center') |> # Align header
  bg(bg = "#d3d3d337", i = 2, part = "header") |> # Color backgroiund for style
  bold(part = "header") |>
  bg(bg = "#d3d3d337", part = "body", i = 2) |>
  color(i = 1, part = "body", j = c(1, 2, 3, 5), color = "#088F8F") |> # Use color to emphasize finding
  bold(i = 1, part = "body", j = c(1, 2, 3, 5)) # Majority of the fraudulent transaction were conducted in EUR with an average value of EUR 60.
# The total value of frudulent trabsaction is decomposed as follows EUR 1200, GBP 429 and USD $73.
# The fraud rate differs by currency. The EUR having a high rate of 1.5% higher than the GBP and USD

fraud_data

# Fraud incidence and  trend over the month
# what is the fraud rate per day
plot_title_fraud_incidence_1 <- "one"
plot_title_fraud_incidence_2 <- "two"
plot_title_fraud_incidence <- marquee_glue(
  "On days when fraud occurs,  {.#088F8F **{plot_title_fraud_incidence_1}** } or {.#088F8F **{plot_title_fraud_incidence_2 }** } cases are typical."
)

marquee_glue(
  "{.#088F8F **{plot_title_fraud_incidence_1}** } or {.#088F8F **{plot_title_fraud_incidence_2 }** } cases of fraud incidence is typical, with three cases on a single day being an outlier."
)

acca_data_rds |>
  mutate(
    day_of_the_month = day(transaction_date)
  ) |>
  filter(actual_fraud == "fraudulent") |>
  group_by(day_of_the_month) |>
  summarise(
    total_fraud = sum(amount),
    incidence = n()
  ) |>
  mutate(
    fraud_incidence_per_day = if_else(
      incidence %in% c(1, 2),
      "typical",
      "outlier"
    )
  ) |>
  ggplot(aes(
    x = day_of_the_month,
    y = incidence,
    fill = fraud_incidence_per_day
  )) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c("#D3D3D3", '#088F8F')) +
  theme_minimal() +
  labs(
    title = plot_title_fraud_incidence,
    subtitle = "In a given month, fraud does not occur every day."
  ) +
  xlab("Day of the month") +
  ylab("Fraud count per day") +
  annotate(
    geom = "text",
    x = 29,
    y = 2,
    hjust = 1,
    color = 'black',
    family = "serif",
    label = "Exceptional case with 3 counts",
    size = 4.5,
    fontface = 'bold',
    angle = 90
  ) +
  theme(
    legend.position = 'none',
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 0.3
    ),
    axis.title.y = element_text(
      size = 13,
      vjust = 1.8,
      hjust = 0.5,
    ),
    axis.text.x = element_text(
      face = "bold",
      size = 11,
      margin = NULL
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      margin = NULL
    ),
    plot.title = element_marquee(
      width = 1,
      size = 19,
      vjust = 0,
      margin = NULL,
      lineheight = 1,
    ),
    plot.subtitle = element_marquee(
      width = 1,
      size = 14,
      vjust = 0,
      margin = NULL,
      lineheight = 1,
    )
  ) # One or two cases of fraud incidence per day are typical, with three cases on a single day being an outlier."
# Fraud incidence in a given month ranges between one and two per day with three being an outlier.

## Customer risk analysis
# Which customer age groups experience the most fraud?
# Are VIP customers more or less likely to experience fraud?
# Does customer tenure reduce fraud risk?
# Which customers generate the highest fraud losses?

# Which customer age groups experience the most fraud?
acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  count(customer_age, sort = TRUE) |>
  arrange(desc(customer_age)) # It appears fraud incident does not differ by customers' ages. Customers of all agaes  are eqaully likely to experience transaction fraud.


# Are VIP customers more or less likely to experience fraud?
acca_data_rds |>
  filter(
    actual_fraud == "fraudulent"
  ) |>
  group_by(is_vip) |>
  summarise(
    fraud_count = n(),
    total_fraud_loss = sum(amount),
  ) #While  No VIP customers transation was involved involved in fraud, all fraud losses were related to non-vip.


# Does customer tenure reduce fraud risk?
acca_data_rds |>
  mutate(
    diff_in_tenure = customer_tenure_days > 1826.647
  ) |>
  filter(actual_fraud == "fraudulent") |>
  count(diff_in_tenure) # Fraud incidence do appear to be associated with customer tenure. Customers with tenure lower than the average
# are more likely to experience fraud. Specifically, 6 out of 10 customers with shorter tenure were victims relative to 40% for longer-tenure customers.

# Which customers generated the highest fraud losses?
acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  slice_max(order_by = amount, n = 5) |>
  select(
    customer_id,
    customer_age,
    amount,
    currency,
    channel,
    payment_method,
    customer_tenure_days
  ) |>
  flextable() |>
  add_header_lines(
    values = 'Top fraud losses by customers profile',
    top = TRUE
  ) |>
  align(
    part = 'header',
    i = 1,
    align = 'center'
  ) |>
  autofit() |>
  align(
    part = "body",
    align = "center"
  ) |>
  width(j = 7, unit = 'mm', width = 0.1) |>
  set_header_labels(
    customer_id = 'Customer ID',
    customer_age = 'Customer age',
    amount = 'Amount',
    currency = 'Currency',
    channel = 'Channel',
    payment_method = 'Payment',
    customer_tenure_days = 'Tenure'
  ) |>
  theme_zebra(
    even_body = "#d3d3d337",
    odd_body = "transparent",
    odd_header = "transparent",
    even_header = "#d3d3d337"
  ) |>
  color(part = 'body', i = 1, color = '#088F8F') |>
  hline(part = "body", i = 5, border = fp_border(width = 0.5)) |>
  hline_top(part = "header", border = fp_border(width = 1)) |>
  hline_top(part = "body", border = fp_border(width = 0.5))
# Top three fraud by value involved card payment. Does that mean that card payments are more succestible?

## Digital properties and fraud incidence.
# Which payment methods are the most vulnerable?
plot_title_variable_payment_method <- "Card"
plot_title_payment_method <- marquee_glue(
  "{.#088F8F **{plot_title_variable_payment_method}** } payment transactions recorded the highest fraud."
)

plot_title_payment_method

acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  count(payment_method, sort = TRUE) |>
  mutate(
    payment_method_risk = if_else(
      n >= 18,
      "High risk",
      "Risky"
    )
  ) |>
  ggplot(aes(
    y = fct_reorder(payment_method, n, .desc = F),
    x = n,
    fill = payment_method_risk,
    label = n
  )) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c('#088F8F', "#D3D3D3")
  ) +
  xlab('Fraud count') +
  ylab('Payment method') +
  geom_text_repel(
    hjust = 2,
    color = "White",
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_y_discrete(
    labels = c(
      "card" = "Card",
      "wallet" = "Wallet",
      "bank_transfer" = "Bank transfer"
    )
  ) +
  labs(
    title = plot_title_payment_method,
    subtitle = "Making it the riskiest transaction payment method"
  ) +
  theme(
    plot.title = element_marquee(
      width = 1,
      size = 19,
      vjust = 0,
      margin = NULL,
      lineheight = 0.5
    ),
    plot.subtitle = element_text(
      size = 15,
      vjust = 1,
      lineheight = 0.5
    ),
    plot.title.position = "plot",
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.3
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      vjust = 1,
      hjust = 1,
      margin = NULL
    ),
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 0.5,
      hjust = 0.5
    ),
    legend.position = 'none',
    axis.text.x.bottom = element_blank(),
    axis.title.x = element_blank()
  )

# Card payment transactions recorded the highest fraud.

# Which channels have the highest fraud rates?
plot_title_variable_channel <- "Web"
plot_title_channel <- marquee_glue(
  "{.#088F8F **{plot_title_variable_channel}** } channel transactions recorded the highest fraud."
)

acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  count(channel, sort = TRUE) |>
  mutate(
    channel_risk = if_else(
      n > 15,
      "High risk channel",
      "Risky"
    )
  ) |>
  ggplot(aes(
    y = fct_reorder(channel, n, .desc = F),
    x = n,
    fill = channel_risk,
    labels = n
  )) +
  geom_col(width = 0.6) +
  ylab(
    "Transaction channels"
  ) +
  scale_fill_manual(
    values = c('#088F8F', "#D3D3D3")
  ) +
  scale_y_discrete(
    labels = c(
      "web" = "Web",
      "mobile_app" = "Mobile app",
      "pos" = "POS"
    )
  ) +
  geom_text_repel(
    hjust = 2,
    color = "White",
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal() +
  labs(
    title = plot_title_channel,
    subtitle = "Making it the most vulnerable transaction channel."
  ) +
  theme(
    plot.title = element_marquee(
      width = 1,
      size = 19,
      vjust = 0,
      margin = NULL,
      lineheight = 1
    ),
    plot.subtitle = element_text(
      size = 15,
      vjust = 1,
      lineheight = 1.5
    ),
    plot.title.position = "plot",
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.3
    ),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      face = "bold",
      size = 12,
      vjust = 1,
      hjust = 1,
      margin = NULL
    ),
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 2,
      hjust = 0.5
    ),
  ) # Web channel transations reported the highest fraud.


## Geographic Fraud Patterns of fraud incidence
# Are cross-border transactions riskier?
# I defined a cross-border transaction as one where the ip_country, billing_country, and shipping_country are differ

acca_data_rds_with_cross_border <- acca_data_rds |>
  # create cross_border variable based on the above definition criteria
  mutate(
    is_cross_border = if_else(
      (ip_country != billing_country) &
        (billing_country != shipping_country),
      "cross border",
      "within border",
      "check"
    )
  )

# Determine the proportion of fraudulent transactions that are cross border.
acca_data_rds_with_cross_border |>
  filter(actual_fraud == "fraudulent") |>
  count(actual_fraud, is_cross_border) |>
  mutate(prop = percent(n / sum(n))) |>
  flextable() |>
  set_header_labels(
    values = c(
      actual_fraud = "Status",
      is_cross_border = "Nature",
      n = "Fraud count",
      prop = "Prop."
    )
  ) |>
  autofit() |>
  color(i = 1, part = "body", color = '#088F8F') |>
  add_header_lines(
    values = "Majority of fradulent transactions are cross border"
  )
# Majority of fraudulent transactions (88%) are cross border transactions.
#Transactions where the IP country is different from the billing as well as where billing and shipping countries differ.

# variables for the plot title
billing_country_high_risk_1 <- "Great Britain (GB)"
billing_country_high_risk_2 <- "Ireland (IE)"

# The plot title
plot_title <- marquee_glue(
  "{.#088F8F **{billing_country_high_risk_1}**} and {.#088F8F **{billing_country_high_risk_2}**} are the top billing destinations
     for fraudulent transactions."
)

plot_title

acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  summarise(
    fraud_incidence = n(),
    .by = billing_country
  ) |>
  mutate(
    prop = fraud_incidence / sum(fraud_incidence),
  ) |>
  mutate(
    billing_country_risk = if_else(
      billing_country %in% c("GB", "IE"),
      "High risk",
      "Risky"
    )
  ) |>
  mutate(
    billing_country = fct_reorder(billing_country, prop, .desc = FALSE)
  ) |>
  ggplot(
    aes(
      y = billing_country,
      x = prop,
      fill = billing_country_risk,
      label = percent(prop)
    ) # This turns the prop doubles into percentage values
  ) +
  geom_col() +
  scale_fill_manual(
    values = c('#088F8F', "#D3D3D3")
  ) +
  geom_text_repel(
    hjust = 1.4,
    color = "White",
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal() +
  labs(
    title = plot_title,
    subtitle = "They represent for more than 50% of the total fraud losses.",
  ) +
  ylab("Billing country") +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.3
    ),
    axis.text.x = element_blank(), #This is made invisble
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      vjust = 1,
      hjust = 1,
      margin = NULL
    ),
    axis.title.x = element_blank(),
    legend.position = 'none',
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 2,
      hjust = 0.5
    ),
    plot.title = element_marquee(
      width = 1,
      size = 19,
      vjust = 0,
      margin = NULL,
      lineheight = 1
    ),
    plot.subtitle = element_text(
      size = 15,
      vjust = 1,
      lineheight = 1.5
    ),
    plot.title.position = "plot"
  ) +
  scale_x_continuous(
    labels = percent_format() # This format the x-axis text to % but I decided to leave the axis not visible.
  )

# Which billing country recorded the highest fraud incidence?
acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  count(billing_country, sort = TRUE) |>
  head(10) # Great Britain and Ireland

# Merchant Risk Analysis
# Does merchant risk score correlate with actual fraud?

plot_title_boxplot_variabe <- "Fraudulent transactions"
plot_title_box_plot <- marquee_glue(
  "{.#6cabdd **{plot_title_boxplot_variabe}** }  are more prevalent with high-risk merchants."
)

acca_data_rds |>
  ggplot(aes(x = actual_fraud, y = merchant_riskscore, color = actual_fraud)) +
  geom_boxplot(
    outlier.shape = 1,
    outlier.color = "orange",
    box.linewidth = 1,
    outlier.fill = "orange",
    whisker.linewidth = 1
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c('#088F8F', "#6cabdd")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.3
    ),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    axis.text.x = element_text(
      face = "bold",
      size = 11,
      vjust = 1,
      margin = NULL
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      vjust = 1,

      margin = NULL
    ),
    axis.title.x = element_blank(),
    legend.position = 'none',
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 3,
      hjust = 0.5
    ),
    plot.title = element_marquee(
      width = 1,
      size = 18,
      hjust = 0,
      vjust = 1,
      margin = NULL,
      lineheight = 1
    ),
    plot.subtitle = element_text(
      size = 14,
      vjust = 1,
      lineheight = 7
    ),
    plot.title.position = "plot"
  ) +
  ylab("Merchants' risk scores") +
  labs(
    title = plot_title_box_plot,
    subtitle = "Low-risk merchants are less vulnerable."
  ) # Fraud transactions are common with high-risk scores relative to non-fraudulent ones


# Which merchant categories have the highest fraud rates?
plot_title_merchant_category_marketplaces <- 'Marketplaces'
plot_title_merchant_category_fashion <- 'fashion'
plot_title_merchant_category <- marquee_glue(
  " {.#088F8F **{plot_title_merchant_category_marketplaces}**} and {.#088F8F **{plot_title_merchant_category_fashion}** } merchants are the most susceptible to fraudulent transactions."
)

acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  count(
    merchant_category,
    actual_fraud,
    sort = T,
    name = "fraud_count"
  ) |>
  mutate(
    merchant_category_risk = if_else(
      merchant_category %in% c("marketplace", "fashion"),
      "High-risk merchant",
      "Risky merchant"
    )
  ) |>
  ggplot(aes(
    y = fct_reorder(merchant_category, fraud_count, .desc = F),
    x = fraud_count,
    fill = merchant_category_risk,
    label = fraud_count
  )) +
  geom_col() +
  theme_minimal() +
  geom_text_repel(
    hjust = 2,
    color = "White",
    size = 6,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c('#088F8F', "#D3D3D3")
  ) +
  labs(
    title = plot_title_merchant_category
  ) +
  ylab("Merchant category channels") +
  scale_y_discrete(
    labels = c(
      "marketplace" = "Marketplace",
      "fashion" = "Fashion",
      "digital_goods" = "Digital goods",
      "gaming" = "Gaming",
      "electronics" = "Electronics",
      "utilities" = "Utilities",
      "telecoms" = "Telecoms",
      "restaurants" = "Restaurants"
    )
  ) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.3
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      face = "bold",
      size = 12,
      vjust = 1,
      hjust = 1,
      margin = NULL
    ),
    axis.title.x = element_blank(),
    legend.position = 'none',
    axis.title.y = element_text(
      face = "plain",
      size = 13,
      vjust = 1.8,
      hjust = 0.5
    ),
    plot.title = element_marquee(
      width = 1,
      size = 19,
      hjust = 0,
      vjust = 1,
      margin = NULL,
      lineheight = 1
    ),
    plot.subtitle = element_text(
      width = 1,
      size = 14,
      vjust = 1,
      lineheight = 7
    ),
    plot.title.position = "plot"
  ) # Fraudulent transations are more prevalent in marketplaces than in other channels


acca_data_rds_with_cross_border |> view()
filter(actual_fraud == "fraudulent") |>
  group_by(is_cross_border) |>
  summarise(
    fraud_incidence = n(),
    sum_loss = round(sum(amount), digits = 0)
  ) |>
  gt() |>
  gt_highlight_rows(
    row = 1,
    fill = "lightgrey",
    font_color = "navyblue",
    alpha = 0.3,
    font_weight = "normal"
  ) # Crossborder transactions are riskier.


mean(acca_data_rds$customer_tenure_days)

acca_data_rds |>
  filter(actual_fraud == "fraudulent")

glimpse(acca_data_rds)


# Data analysis strategy
# Variables (customer_tenure_days, is_vip, customer_id, customer_age    )
# data type - is_vip to factor, confirm the distinct number

# Which customer age groups experience the most fraud?
data_customer <- data_customer |>
  mutate(
    is_vip = factor(is_vip, levels = c(0, 1), labels = c("non vip", "vip"))
  )


data_customer |>
  filter(actual_fraud == "fraud") |>
  count(customer_age, wt = amount, sort = T) # Customer age 24 and 59 list the most to fraud with 217 and 194 units of the currency value involved

# Which customers generate the highest fraud losses?
data_customer_losses <- acca |>
  select(amount, customer_id, actual_fraud, currency)
data_customer_losses |>
  filter(actual_fraud == "fraud") |>
  count(customer_id, currency, sort = T, wt = amount, name = "Total loss") |>
  head(9) |>
  gt() |>
  gt_highlight_rows(
    rows = c(1:6),
    fill = "lightgrey",
    alpha = 0.3,
    font_weight = "normal",
    font_color = "navyblue"
  ) |>
  tab_header(
    title = "Fraud losses by customers",
    subtitle = "Top six victims"
  ) |>
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


+scale_colour_viridis_c() +
  coord_flip()


## Geographic Fraud Patterns
# Which countries generate the most fraud?
# Are cross-border transactions riskier?
# Are mismatches between billing, shipping, and IP country associated with fraud?

data_country <- acca |>
  select(ip_country, billing_country, shipping_country, actual_fraud, amount)

# fraud incidence by their billing countring
acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  count(billing_country, sort = TRUE) |>
  head(10)


# fraud incidence by their billing countring
acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  count(shipping_country, sort = TRUE) |>
  head(10)

acca_data_rds |>
  filter(
    (actual_fraud == "fraudulent") & (shipping_country != billing_country)
  ) |>
  filter(shipping_country != billing_country) |>
  count(actual_fraud, sort = TRUE) |>
  head(10) #


data_country |>
  mutate(
    cross_border = if_else(
      (ip_country != billing_country) &
        (billing_country != shipping_country) &
        (ip_country != shipping_country),
      "cross border",
      "within border",
      "check"
    )
  )


acca_data_rds_with_cross_border |>
  filter(actual_fraud == "fraudulent") |>
  group_by(is_cross_border) |>
  summarise(
    fraud_incidence = n(),
    sum_loss = round(sum(amount), digits = 0)
  ) |>
  gt() |>
  gt_highlight_rows(
    row = 1,
    fill = "lightgrey",
    font_color = "navyblue",
    alpha = 0.3,
    font_weight = "normal"
  ) # Crossborder transactions are riskier. Transactions where the ip country is different from the billing from those of billing and shipping report more than those which is not the case

data_digital <- acca |>
  select(
    payment_method,
    channel,
    amount,
    card_present,
    device_fingerprint,
    actual_fraud
  )


#

game_films <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv'
)
View(game_films)
?(quade.test())


fraud_data <- acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  group_by(currency) |>
  summarise(
    fraud_count = n(),
    fraud_total_loss = format(sum(amount), big.mark = ","),
    fraud_average_loss = round(mean(amount, na.rm = T), digits = 1),
    fraud_proportion = round(
      fraud_count / nrow(acca_data_rds) * 100,
      digits = 1
    )
  )
gt() # Majority of the fraudulent transaction were conducted in EUR with an average value of EUR 60.
# The total value of frudulent trabsaction is decomposed as follows EUR 1200, GBP 429 and USD $73

#
fraud_data <- acca_data_rds |>
  filter(actual_fraud == "fraudulent") |>
  group_by(currency) |>
  summarise(
    fraud_count = n(),
    total_fraud_loss = format(sum(amount), big.mark = ","),
    average_fraud_loss = round(mean(amount, na.rm = T), digits = 1),
    fraud_rate = round(fraud_count / nrow(acca_data_rds) * 100, digits = 1)
  ) |>
  gt() |>
  cols_label(
    currency = "Currency",
    fraud_count = "Fraud count",
    total_fraud_loss = "Total fraud loss",
    average_fraud_loss = "Average fraud loss",
    fraud_rate = " Fraud rate"
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = html("Fraud incidence by currency")
  )


# Fraud trend over the period
# Are fraudulent transactions increasing over time (data and time)
acca_data_rds |>
  mutate(
    day = day(transaction_date)
  ) |>
  filter(actual_fraud == "fraud") |>
  group_by(day) |>
  summarise(
    total_fraud_amount = sum(amount),
    fraud_incidence_count = n()
  ) |>
  ggplot() +
  geom_point(aes(
    x = day,
    y = fraud_incidence_count
  )) # The amount involved in fraudulent transaction is betweeen 50 to 100 units of the currency amount

# Do email domain defer by their fraud incidence/
#

# Which countries generate the most fraud?
# Are cross-border transactions riskier?
# Are mismatches between billing, shipping, and IP country associated with fraud?
