# ============================================
# JC FARMS - PRIORITIZED METRICS SPC DASHBOARD
# ============================================

#packages
install.packages("qcc")

# Load required libraries
library(tidyverse)
library(lubridate)
library(qcc)
library(plotly)
library(ggplot2)
library(scales)
library(knitr)
library(DT)
library(gridExtra)
library(ggthemes)

# Load data
transactions <- read.csv("data/jc_farms_transaction_data_20251228.csv")

# Convert dates and create time variables
transactions$transaction_date <- as.Date(transactions$transaction_date)
transactions$week <- week(transactions$transaction_date)
transactions$month <- month(transactions$transaction_date, label = TRUE)
transactions$year_month <- format(transactions$transaction_date, "%Y-%m")

# ============================================
# METRIC 1: PROFIT MARGIN PER BATCH (HIGHEST PRIORITY)
# ============================================

# Calculate batch-level profit margin
batch_profit_margin <- transactions %>%
  group_by(Batch_Number) %>%
  summarise(
    total_revenue = sum(Amount_UGX[account_type == "revenues"], na.rm = TRUE),
    total_cost = sum(Amount_UGX[account_type %in% c("cogs", "opex")], na.rm = TRUE),
    birds_sold = sum(quantity[account_type == "revenues"], na.rm = TRUE),
    batch_start_date = min(transaction_date[!is.na(transaction_date)]),
    batch_end_date = max(transaction_date[!is.na(transaction_date)]),
    .groups = 'drop'
  ) %>%
  mutate(
    profit = total_revenue - total_cost,
    profit_margin = ifelse(total_revenue > 0, (profit / total_revenue) * 100, NA),
    batch_duration = as.numeric(difftime(batch_end_date, batch_start_date, units = "days"))
  ) %>%
  filter(!is.na(profit_margin) & Batch_Number != "")

# SPC Chart for Profit Margin
create_profit_margin_spc <- function(data) {
  # Calculate control limits
  mean_margin <- mean(data$profit_margin, na.rm = TRUE)
  sd_margin <- sd(data$profit_margin, na.rm = TRUE)
  
  # Create SPC chart
  p <- ggplot(data, aes(x = as.factor(Batch_Number), y = profit_margin, group = 1)) +
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    geom_point(color = "#2E86AB", size = 4, shape = 19) +
    # Center line (mean)
    geom_hline(yintercept = mean_margin, color = "#F24236", 
               linetype = "solid", linewidth = 1.2, alpha = 0.8) +
    # Upper control limit (UCL = mean + 3σ)
    geom_hline(yintercept = mean_margin + (3 * sd_margin), 
               color = "#F24236", linetype = "dashed", linewidth = 1) +
    # Lower control limit (LCL = mean - 3σ)
    geom_hline(yintercept = mean_margin - (3 * sd_margin), 
               color = "#F24236", linetype = "dashed", linewidth = 1) +
    # Warning limits (mean ± 2σ)
    geom_hline(yintercept = mean_margin + (2 * sd_margin), 
               color = "#F2BB05", linetype = "dotted", linewidth = 0.8) +
    geom_hline(yintercept = mean_margin - (2 * sd_margin), 
               color = "#F2BB05", linetype = "dotted", linewidth = 0.8) +
    # Labels
    annotate("text", x = 1, y = mean_margin, 
             label = paste0("CL: ", round(mean_margin, 1), "%"), 
             vjust = -1, hjust = 0, color = "#F24236", fontface = "bold") +
    annotate("text", x = 1, y = mean_margin + (3 * sd_margin), 
             label = paste0("UCL: ", round(mean_margin + (3 * sd_margin), 1), "%"), 
             vjust = -1, hjust = 0, color = "#F24236") +
    annotate("text", x = 1, y = mean_margin - (3 * sd_margin), 
             label = paste0("LCL: ", round(mean_margin - (3 * sd_margin), 1), "%"), 
             vjust = 2, hjust = 0, color = "#F24236") +
    # Highlight negative margins
    geom_point(data = data %>% filter(profit_margin < 0), 
               aes(x = as.factor(Batch_Number), y = profit_margin),
               color = "#F24236", size = 6, shape = 21, stroke = 2) +
    labs(
      title = "PRIORITY 1: Profit Margin per Batch (%) - SPC Chart",
      subtitle = "Control Limits: Mean ± 3σ (Red), Warning: Mean ± 2σ (Yellow)",
      x = "Batch Number",
      y = "Profit Margin (%)",
      caption = "Red circles indicate loss-making batches"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#2E86AB"),
      plot.subtitle = element_text(size = 12, color = "#666666"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_line(color = "#F0F0F0"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 10, color = "#999999")
    ) +
    scale_y_continuous(limits = c(
      min(data$profit_margin, mean_margin - (3 * sd_margin)) * 1.1,
      max(data$profit_margin, mean_margin + (3 * sd_margin)) * 1.1
    ))
  
  return(p)
}

profit_spc_chart <- create_profit_margin_spc(batch_profit_margin)

# ============================================
# METRIC 2: COST PER BIRD (CPB) - SECOND PRIORITY
# ============================================

# Calculate Cost Per Bird
batch_cpb <- transactions %>%
  group_by(Batch_Number) %>%
  summarise(
    total_cost = sum(Amount_UGX[account_type %in% c("cogs", "opex")], na.rm = TRUE),
    birds_sold = sum(quantity[account_type == "revenues"], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    cost_per_bird = ifelse(birds_sold > 0, total_cost / birds_sold, NA)
  ) %>%
  filter(!is.na(cost_per_bird) & birds_sold > 0)

# SPC Chart for Cost Per Bird
create_cpb_spc <- function(data) {
  # Calculate control limits
  mean_cpb <- mean(data$cost_per_bird, na.rm = TRUE)
  sd_cpb <- sd(data$cost_per_bird, na.rm = TRUE)
  
  p <- ggplot(data, aes(x = as.factor(Batch_Number), y = cost_per_bird, group = 1)) +
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    geom_point(color = "#2E86AB", size = 4, shape = 19) +
    # Control limits
    geom_hline(yintercept = mean_cpb, color = "#F24236", 
               linetype = "solid", linewidth = 1.2, alpha = 0.8) +
    geom_hline(yintercept = mean_cpb + (3 * sd_cpb), 
               color = "#F24236", linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = mean_cpb - (3 * sd_cpb), 
               color = "#F24236", linetype = "dashed", linewidth = 1) +
    # Warning limits
    geom_hline(yintercept = mean_cpb + (2 * sd_cpb), 
               color = "#F2BB05", linetype = "dotted", linewidth = 0.8) +
    geom_hline(yintercept = mean_cpb - (2 * sd_cpb), 
               color = "#F2BB05", linetype = "dotted", linewidth = 0.8) +
    # Labels
    annotate("text", x = 1, y = mean_cpb, 
             label = paste0("CL: ", format(round(mean_cpb), big.mark = ","), " UGX"), 
             vjust = -1, hjust = 0, color = "#F24236", fontface = "bold") +
    annotate("text", x = 1, y = mean_cpb + (3 * sd_cpb), 
             label = paste0("UCL: ", format(round(mean_cpb + (3 * sd_cpb)), big.mark = ","), " UGX"), 
             vjust = -1, hjust = 0, color = "#F24236") +
    annotate("text", x = 1, y = mean_cpb - (3 * sd_cpb), 
             label = paste0("LCL: ", format(round(mean_cpb - (3 * sd_cpb)), big.mark = ","), " UGX"), 
             vjust = 2, hjust = 0, color = "#F24236") +
    # Highlight high-cost batches (above mean + 1σ)
    geom_point(data = data %>% filter(cost_per_bird > mean_cpb + sd_cpb), 
               aes(x = as.factor(Batch_Number), y = cost_per_bird),
               color = "#F24236", size = 6, shape = 21, stroke = 2) +
    labs(
      title = "PRIORITY 2: Cost Per Bird (UGX) - SPC Chart",
      subtitle = "Control Limits: Mean ± 3σ (Red), Warning: Mean ± 2σ (Yellow)",
      x = "Batch Number",
      y = "Cost Per Bird (UGX)",
      caption = "Red circles indicate batches with cost > (Mean + 1σ)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#2E86AB"),
      plot.subtitle = element_text(size = 12, color = "#666666"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      panel.grid.major = element_line(color = "#F0F0F0"),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(labels = comma)
  
  return(p)
}

cpb_spc_chart <- create_cpb_spc(batch_cpb)

# ============================================
# METRIC 3: FEED CONVERSION RATIO (FCR) - THIRD PRIORITY
# ============================================

# Calculate Feed Conversion Ratio (Cost Basis)
batch_fcr <- transactions %>%
  group_by(Batch_Number) %>%
  summarise(
    feed_cost = sum(Amount_UGX[category_cogs == "cogs_feeds"], na.rm = TRUE),
    revenue = sum(Amount_UGX[account_type == "revenues"], na.rm = TRUE),
    birds_sold = sum(quantity[account_type == "revenues"], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # FCR as feed cost per revenue generated (lower is better)
    fcr = ifelse(revenue > 0, feed_cost / revenue, NA),
    # Alternative: Feed cost per bird
    feed_cost_per_bird = ifelse(birds_sold > 0, feed_cost / birds_sold, NA)
  ) %>%
  filter(!is.na(fcr) & revenue > 0)

# SPC Chart for FCR
create_fcr_spc <- function(data) {
  # Calculate control limits
  mean_fcr <- mean(data$fcr, na.rm = TRUE)
  sd_fcr <- sd(data$fcr, na.rm = TRUE)
  
  p <- ggplot(data, aes(x = as.factor(Batch_Number), y = fcr, group = 1)) +
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    geom_point(color = "#2E86AB", size = 4, shape = 19) +
    # Control limits
    geom_hline(yintercept = mean_fcr, color = "#F24236", 
               linetype = "solid", linewidth = 1.2, alpha = 0.8) +
    geom_hline(yintercept = mean_fcr + (3 * sd_fcr), 
               color = "#F24236", linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = mean_fcr - (3 * sd_fcr), 
               color = "#F24236", linetype = "dashed", linewidth = 1) +
    # Target line (industry benchmark - adjust as needed)
    geom_hline(yintercept = 0.4, color = "#4CAF50", 
               linetype = "dashed", linewidth = 1, alpha = 0.7) +
    # Labels
    annotate("text", x = 1, y = mean_fcr, 
             label = paste0("CL: ", round(mean_fcr, 2)), 
             vjust = -1, hjust = 0, color = "#F24236", fontface = "bold") +
    annotate("text", x = 1, y = 0.4, 
             label = "Target: 0.40", 
             vjust = 2, hjust = 0, color = "#4CAF50") +
    # Highlight inefficient batches (FCR > mean + 1σ)
    geom_point(data = data %>% filter(fcr > mean_fcr + sd_fcr), 
               aes(x = as.factor(Batch_Number), y = fcr),
               color = "#F24236", size = 6, shape = 21, stroke = 2) +
    labs(
      title = "PRIORITY 3: Feed Conversion Ratio (FCR) - SPC Chart",
      subtitle = "FCR = Feed Cost / Revenue | Lower is better | Green line = Target (0.40)",
      x = "Batch Number",
      y = "Feed Conversion Ratio",
      caption = "Red circles indicate inefficient batches (FCR > Mean + 1σ)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#2E86AB"),
      plot.subtitle = element_text(size = 12, color = "#666666"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
    ) +
    scale_y_continuous(limits = c(0, max(data$fcr, mean_fcr + (3 * sd_fcr)) * 1.2))
  
  return(p)
}

fcr_spc_chart <- create_fcr_spc(batch_fcr)

# ============================================
# METRIC 4: AVERAGE SALE PRICE TREND - FOURTH PRIORITY
# ============================================

# Calculate average sale price per batch
sale_price_trend <- transactions %>%
  filter(account_type == "revenues") %>%
  group_by(Batch_Number, transaction_date) %>%
  summarise(
    daily_revenue = sum(Amount_UGX, na.rm = TRUE),
    daily_sales = sum(quantity, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Batch_Number) %>%
  summarise(
    avg_sale_price = ifelse(sum(daily_sales) > 0, 
                            sum(daily_revenue) / sum(daily_sales), 
                            NA),
    min_sale_price = min(ifelse(daily_sales > 0, daily_revenue / daily_sales, NA), na.rm = TRUE),
    max_sale_price = max(ifelse(daily_sales > 0, daily_revenue / daily_sales, NA), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(avg_sale_price))

# SPC Chart for Average Sale Price
create_price_spc <- function(data) {
  # Calculate control limits
  mean_price <- mean(data$avg_sale_price, na.rm = TRUE)
  sd_price <- sd(data$avg_sale_price, na.rm = TRUE)
  
  p <- ggplot(data, aes(x = as.factor(Batch_Number), y = avg_sale_price, group = 1)) +
    # Ribbon for price range
    geom_ribbon(aes(ymin = min_sale_price, ymax = max_sale_price), 
                fill = "#2E86AB", alpha = 0.2) +
    # Average price line
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    geom_point(color = "#2E86AB", size = 4, shape = 19) +
    # Control limits
    geom_hline(yintercept = mean_price, color = "#F24236", 
               linetype = "solid", linewidth = 1.2, alpha = 0.8) +
    geom_hline(yintercept = mean_price + (3 * sd_price), 
               color = "#F24236", linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = mean_price - (3 * sd_price), 
               color = "#F24236", linetype = "dashed", linewidth = 1) +
    # Target price line (set your target here)
    geom_hline(yintercept = 14000, color = "#4CAF50", 
               linetype = "dashed", linewidth = 1, alpha = 0.7) +
    # Labels
    annotate("text", x = 1, y = mean_price, 
             label = paste0("CL: ", format(round(mean_price), big.mark = ","), " UGX"), 
             vjust = -1, hjust = 0, color = "#F24236", fontface = "bold") +
    annotate("text", x = 1, y = 14000, 
             label = "Target: 14,000 UGX", 
             vjust = 2, hjust = 0, color = "#4CAF50") +
    # Highlight low-price batches
    geom_point(data = data %>% filter(avg_sale_price < mean_price - sd_price), 
               aes(x = as.factor(Batch_Number), y = avg_sale_price),
               color = "#F24236", size = 6, shape = 21, stroke = 2) +
    labs(
      title = "PRIORITY 4: Average Sale Price per Batch (UGX) - SPC Chart",
      subtitle = "Ribbon shows price range | Green line = Target Price",
      x = "Batch Number",
      y = "Average Sale Price (UGX)",
      caption = "Red circles indicate batches with price < (Mean - 1σ)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#2E86AB"),
      plot.subtitle = element_text(size = 12, color = "#666666"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
    ) +
    scale_y_continuous(labels = comma)
  
  return(p)
}

price_spc_chart <- create_price_spc(sale_price_trend)

# ============================================
# DASHBOARD SUMMARY METRICS
# ============================================

# Create summary statistics table
summary_stats <- data.frame(
  Metric = c(
    "Average Profit Margin",
    "Profit Margin Std Dev",
    "Best Profit Margin (Batch)",
    "Worst Profit Margin (Batch)",
    "Average Cost Per Bird",
    "CPB Std Dev",
    "Average FCR",
    "FCR Std Dev",
    "Average Sale Price",
    "Sale Price Std Dev"
  ),
  Value = c(
    paste0(round(mean(batch_profit_margin$profit_margin, na.rm = TRUE), 1), "%"),
    paste0(round(sd(batch_profit_margin$profit_margin, na.rm = TRUE), 1), "%"),
    paste0("Batch ", batch_profit_margin$Batch_Number[which.max(batch_profit_margin$profit_margin)], 
           ": ", round(max(batch_profit_margin$profit_margin, na.rm = TRUE), 1), "%"),
    paste0("Batch ", batch_profit_margin$Batch_Number[which.min(batch_profit_margin$profit_margin)], 
           ": ", round(min(batch_profit_margin$profit_margin, na.rm = TRUE), 1), "%"),
    paste0(format(round(mean(batch_cpb$cost_per_bird, na.rm = TRUE)), big.mark = ","), " UGX"),
    paste0(format(round(sd(batch_cpb$cost_per_bird, na.rm = TRUE)), big.mark = ","), " UGX"),
    round(mean(batch_fcr$fcr, na.rm = TRUE), 3),
    round(sd(batch_fcr$fcr, na.rm = TRUE), 3),
    paste0(format(round(mean(sale_price_trend$avg_sale_price, na.rm = TRUE)), big.mark = ","), " UGX"),
    paste0(format(round(sd(sale_price_trend$avg_sale_price, na.rm = TRUE)), big.mark = ","), " UGX")
  ),
  Status = c(
    ifelse(mean(batch_profit_margin$profit_margin, na.rm = TRUE) > 10, "Good", ifelse(mean(batch_profit_margin$profit_margin, na.rm = TRUE) > 0, "Fair", "Poor")),
    ifelse(sd(batch_profit_margin$profit_margin, na.rm = TRUE) < 15, "Stable", "Variable"),
    "Best",
    "Concern",
    ifelse(mean(batch_cpb$cost_per_bird, na.rm = TRUE) < 10000, "Good", ifelse(mean(batch_cpb$cost_per_bird, na.rm = TRUE) < 12000, "Fair", "High")),
    ifelse(sd(batch_cpb$cost_per_bird, na.rm = TRUE) < 2000, "Stable", "Variable"),
    ifelse(mean(batch_fcr$fcr, na.rm = TRUE) < 0.45, "Efficient", ifelse(mean(batch_fcr$fcr, na.rm = TRUE) < 0.55, "Fair", "Inefficient")),
    ifelse(sd(batch_fcr$fcr, na.rm = TRUE) < 0.1, "Stable", "Variable"),
    ifelse(mean(sale_price_trend$avg_sale_price, na.rm = TRUE) > 13000, "Good", ifelse(mean(sale_price_trend$avg_sale_price, na.rm = TRUE) > 12000, "Fair", "Low")),
    ifelse(sd(sale_price_trend$avg_sale_price, na.rm = TRUE) < 1000, "Stable", "Variable")
  )
)

# Color function for status
color_status <- function(status) {
  case_when(
    status %in% c("Good", "Best", "Efficient", "Stable") ~ "green",
    status %in% c("Fair", "Variable") ~ "orange",
    status %in% c("Poor", "Concern", "High", "Inefficient", "Low") ~ "red",
    TRUE ~ "black"
  )
}

# ============================================
# SPC VIOLATION DETECTION
# ============================================

# Function to detect SPC violations
detect_spc_violations <- function(metric_values, metric_name) {
  mean_val <- mean(metric_values, na.rm = TRUE)
  sd_val <- sd(metric_values, na.rm = TRUE)
  ucl <- mean_val + (3 * sd_val)
  lcl <- mean_val - (3 * sd_val)
  
  violations <- which(metric_values > ucl | metric_values < lcl)
  
  if (length(violations) > 0) {
    return(data.frame(
      Metric = metric_name,
      Violation_Type = ifelse(metric_values[violations] > ucl, "Above UCL", "Below LCL"),
      Value = metric_values[violations],
      Batch = violations
    ))
  } else {
    return(NULL)
  }
}

# Check for violations
violations <- bind_rows(
  detect_spc_violations(batch_profit_margin$profit_margin, "Profit Margin"),
  detect_spc_violations(batch_cpb$cost_per_bird, "Cost Per Bird"),
  detect_spc_violations(batch_fcr$fcr, "FCR"),
  detect_spc_violations(sale_price_trend$avg_sale_price, "Sale Price")
)

# ============================================
# QUARTO DASHBOARD OUTPUT
# ============================================

cat("# JC FARMS - PRIORITIZED METRICS SPC DASHBOARD\n\n")

cat("## 📊 Executive Summary\n")
cat(paste("**Period:**", min(transactions$transaction_date), "to", max(transactions$transaction_date), "\n"))
cat(paste("**Batches Analyzed:**", nrow(batch_profit_margin), "\n"))
cat(paste("**Total Birds Sold:**", sum(batch_cpb$birds_sold, na.rm = TRUE), "\n\n"))

cat("## 🎯 Metric Priority Summary\n")
cat("1. **Profit Margin** - Business viability indicator\n")
cat("2. **Cost Per Bird** - Primary cost control metric\n")  
cat("3. **Feed Conversion Ratio** - Operational efficiency\n")
cat("4. **Average Sale Price** - Revenue management\n\n")

cat("## 📈 SPC Control Charts\n\n")

cat("### PRIORITY 1: Profit Margin per Batch\n")
print(profit_spc_chart)
cat("\n\n")

cat("### PRIORITY 2: Cost Per Bird (CPB)\n")
print(cpb_spc_chart)
cat("\n\n")

cat("### PRIORITY 3: Feed Conversion Ratio (FCR)\n")
print(fcr_spc_chart)
cat("\n\n")

cat("### PRIORITY 4: Average Sale Price Trend\n")
print(price_spc_chart)
cat("\n\n")

cat("## 📋 Performance Summary Table\n")
# Create styled table
summary_table_formatted <- summary_stats %>%
  mutate(
    Value = cell_spec(Value, 
                      color = color_status(Status),
                      bold = ifelse(Status %in% c("Good", "Best", "Efficient"), TRUE, FALSE))
  ) %>%
  select(Metric, Value, Status)

print(kable(summary_table_formatted, 
            format = "html", 
            escape = FALSE,
            caption = "Performance Metrics Summary") %>%
        kable_styling(bootstrap_options = c("striped", "hover")))

cat("\n\n## ⚠️ SPC Violations Detected\n")
if (!is.null(violations) && nrow(violations) > 0) {
  print(kable(violations, format = "simple", caption = "SPC Rule Violations"))
} else {
  cat("**No SPC violations detected.** All metrics within control limits (±3σ).\n")
}

cat("\n\n## 📊 Correlation Analysis\n")

# Create correlation matrix
correlation_data <- batch_profit_margin %>%
  left_join(batch_cpb, by = "Batch_Number") %>%
  left_join(batch_fcr, by = "Batch_Number") %>%
  left_join(sale_price_trend, by = "Batch_Number") %>%
  select(profit_margin, cost_per_bird, fcr, avg_sale_price)

if (nrow(correlation_data) >= 3) {
  cor_matrix <- cor(correlation_data, use = "complete.obs")
  
  cat("Correlation between key metrics:\n")
  print(kable(round(cor_matrix, 2), format = "simple"))
  
  # Key insights
  cat("\n**Key Insights:**\n")
  if (cor_matrix["profit_margin", "cost_per_bird"] < -0.5) {
    cat("- Strong negative correlation between Profit Margin and Cost Per Bird (as expected)\n")
  }
  if (cor_matrix["profit_margin", "avg_sale_price"] > 0.5) {
    cat("- Strong positive correlation between Profit Margin and Sale Price\n")
  }
}

cat("\n\n## 🎯 Recommended Actions\n\n")

# Generate actionable insights
cat("### Based on SPC Analysis:\n\n")

# Profit Margin insights
if (mean(batch_profit_margin$profit_margin, na.rm = TRUE) < 0) {
  cat("1. **CRITICAL**: Average profit margin is negative. Immediate cost reduction or price increase needed.\n")
} else if (mean(batch_profit_margin$profit_margin, na.rm = TRUE) < 10) {
  cat("1. **ACTION**: Profit margin below 10%. Review highest cost components (likely feed or chicks).\n")
}

# CPB insights
cpb_vs_price <- mean(batch_cpb$cost_per_bird, na.rm = TRUE) / mean(sale_price_trend$avg_sale_price, na.rm = TRUE)
if (cpb_vs_price > 0.8) {
  cat("2. **WARNING**: Cost Per Bird is >80% of sale price. Margin compression risk.\n")
}

# FCR insights
if (mean(batch_fcr$fcr, na.rm = TRUE) > 0.5) {
  cat("3. **INEFFICIENCY**: FCR above 0.5 indicates poor feed efficiency. Review feed quality/quantity.\n")
}

# Price insights
price_cv <- sd(sale_price_trend$avg_sale_price, na.rm = TRUE) / mean(sale_price_trend$avg_sale_price, na.rm = TRUE)
if (price_cv > 0.15) {
  cat("4. **PRICING**: High price variability (>15% CV). Consider standardizing pricing strategy.\n")
}

cat("\n### SPC Monitoring Recommendations:\n")
cat("1. Review charts weekly when new batch data is available\n")
cat("2. Investigate any point outside ±2σ warning limits\n")
cat("3. Take corrective action for any point outside ±3σ control limits\n")
cat("4. Track improvement in Profit Margin as primary success metric\n")

# Save data for future comparison
write.csv(batch_profit_margin, "jc_farms_profit_margin_tracking.csv", row.names = FALSE)
write.csv(batch_cpb, "jc_farms_cpb_tracking.csv", row.names = FALSE)

cat("\n\n---\n")
cat("*Dashboard generated on:* ", Sys.Date(), "\n")
cat("*SPC Methodology:* Shewhart control charts with ±3σ control limits\n")
