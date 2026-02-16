# ------------------------------------------------------------------
# JC Farms Dashboard — Helper Functions
# ------------------------------------------------------------------

# Connect to KoboToolbox API and return raw data
jc_connect_kobo <- function() {
  token <- Sys.getenv("KOBO_TOKEN")
  if (token == "") stop("KOBO_TOKEN environment variable not set. Set it in .Renviron or as a system env var.")

  kobo_setup(url = "https://kf.kobotoolbox.org/", token = token)
  projects <- kobo_asset_list()
  uid <- projects$uid[1]
  asset <- kobo_asset(uid)
  kobo_data(asset)
}

# Clean and rename raw KoboToolbox data
jc_clean_data <- function(df_raw) {
  df_raw |>
    # Strip haven labels so downstream code gets plain R types
    mutate(across(where(haven::is.labelled), ~ as.character(.))) |>
    rename(
      Amount_UGX = amount_manual,
      Batch_Number = batch
    ) |>
    mutate(Amount_UGX = as.numeric(Amount_UGX)) |>
    mutate(quantity = as.numeric(quantity)) |>
    filter(account_type != "capex") |>
    filter(Batch_Number != "3") |>
    select(!("_attachments"))
}

# Compute KPIs for a single batch (bird count derived from data)
jc_dashboard_kpis <- function(data, batch_num) {
  batch_data <- data |> filter(Batch_Number == batch_num)

  revenue <- sum(batch_data$Amount_UGX[batch_data$account_type == "revenues"], na.rm = TRUE)
  cost <- sum(batch_data$Amount_UGX[batch_data$account_type %in% c("cogs", "opex")], na.rm = TRUE)
  net_profit <- revenue - cost

  birds_purchased <- sum(batch_data$quantity[!is.na(batch_data$category_cogs) & batch_data$category_cogs == "cogs_chicks_purchased"], na.rm = TRUE)
  birds_sold <- sum(batch_data$quantity[batch_data$account_type == "revenues"], na.rm = TRUE)
  num_birds <- if (birds_purchased > 0) birds_purchased else birds_sold

  cost_per_bird <- if (num_birds > 0) round(cost / num_birds) else NA
  profit_per_bird <- if (num_birds > 0) round(net_profit / num_birds) else NA
  mortality_rate <- if (birds_purchased > 0) round(100 * (birds_purchased - birds_sold) / birds_purchased, 1) else NA
  profit_margin <- if (revenue > 0) round(100 * net_profit / revenue, 1) else NA

  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)

  list(
    revenue = revenue,
    cost = cost,
    net_profit = net_profit,
    birds_purchased = birds_purchased,
    birds_sold = birds_sold,
    num_birds = num_birds,
    cost_per_bird = cost_per_bird,
    profit_per_bird = profit_per_bird,
    mortality_rate = mortality_rate,
    profit_margin = profit_margin,
    revenue_fmt = fmt(revenue),
    cost_fmt = fmt(cost),
    net_profit_fmt = fmt(net_profit),
    cost_per_bird_fmt = fmt(cost_per_bird),
    profit_per_bird_fmt = fmt(profit_per_bird)
  )
}

# Compute batch-level summary across all batches (for SPC and trends)
jc_batch_summary <- function(data) {
  data |>
    mutate(Batch = as.integer(as.character(Batch_Number))) |>
    group_by(Batch) |>
    summarise(
      birds_purchased = sum(quantity[!is.na(category_cogs) & category_cogs == "cogs_chicks_purchased"], na.rm = TRUE),
      birds_sold = sum(quantity[account_type == "revenues"], na.rm = TRUE),
      revenue = sum(Amount_UGX[account_type == "revenues"], na.rm = TRUE),
      cost = sum(Amount_UGX[account_type %in% c("cogs", "opex")], na.rm = TRUE),
      feed_cost = sum(Amount_UGX[!is.na(category_cogs) & category_cogs == "cogs_feeds"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      birds_mortality = birds_purchased - birds_sold,
      mortality_rate = if_else(birds_purchased > 0, 100 * birds_mortality / birds_purchased, NA_real_),
      profit = revenue - cost,
      profit_margin = if_else(revenue > 0, 100 * profit / revenue, NA_real_),
      cost_per_bird = if_else(birds_sold > 0, cost / birds_sold, NA_real_),
      profit_per_bird = if_else(birds_sold > 0, profit / birds_sold, NA_real_),
      fcr = if_else(revenue > 0, feed_cost / revenue, NA_real_),
      avg_sale_price = if_else(birds_sold > 0, revenue / birds_sold, NA_real_)
    ) |>
    arrange(Batch)
}

# Interactive SPC chart using plotly
jc_spc_plotly <- function(df, metric, ylab, title) {
  vals <- na.omit(df[[metric]])
  mu <- mean(vals)
  sig <- sd(vals)
  ucl <- mu + 3 * sig
  lcl <- mu - 3 * sig

  df <- df |> mutate(
    .batch_label = factor(Batch),
    .value = .data[[metric]],
    .out_of_control = .data[[metric]] > ucl | .data[[metric]] < lcl
  )

  plot_ly(df, x = ~.batch_label, y = ~.value, type = "scatter", mode = "lines+markers",
          line = list(color = "#2E86AB", width = 2),
          marker = list(
            size = 8,
            color = ifelse(df$.out_of_control, "#F24236", "#2E86AB"),
            line = list(color = "#ffffff", width = 1)
          ),
          text = ~paste0("Batch ", Batch, "<br>", ylab, ": ", round(.value, 1)),
          hoverinfo = "text",
          name = ylab) |>
    layout(
      title = list(text = title, font = list(size = 14)),
      xaxis = list(title = "Batch"),
      yaxis = list(title = ylab),
      shapes = list(
        # Mean line
        list(type = "line", x0 = 0, x1 = 1, xref = "paper",
             y0 = mu, y1 = mu, line = list(color = "#F24236", width = 1.5)),
        # UCL
        list(type = "line", x0 = 0, x1 = 1, xref = "paper",
             y0 = ucl, y1 = ucl, line = list(color = "#F24236", width = 1, dash = "dash")),
        # LCL
        list(type = "line", x0 = 0, x1 = 1, xref = "paper",
             y0 = lcl, y1 = lcl, line = list(color = "#F24236", width = 1, dash = "dash"))
      ),
      annotations = list(
        list(x = 1.02, y = mu, xref = "paper", yref = "y", text = "Mean",
             showarrow = FALSE, font = list(color = "#F24236", size = 10)),
        list(x = 1.02, y = ucl, xref = "paper", yref = "y", text = "UCL",
             showarrow = FALSE, font = list(color = "#F24236", size = 10)),
        list(x = 1.02, y = lcl, xref = "paper", yref = "y", text = "LCL",
             showarrow = FALSE, font = list(color = "#F24236", size = 10))
      ),
      margin = list(r = 60),
      showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
}

# Return bslib theme name based on value vs thresholds
jc_kpi_theme <- function(value, threshold_good, threshold_warn, higher_is_better = TRUE) {
  if (is.na(value)) return("secondary")
  if (higher_is_better) {
    if (value >= threshold_good) return("success")
    if (value >= threshold_warn) return("warning")
    return("danger")
  } else {
    if (value <= threshold_good) return("success")
    if (value <= threshold_warn) return("warning")
    return("danger")
  }
}

# Build full P&L for one batch (from raw/pre-rename data)
jc_pnl_by_batch <- function(data, batch_num) {

  # Strip haven labels from raw data so comparisons work
  transactions <- data %>%
    mutate(across(where(haven::is.labelled), ~ as.character(.)))

  batch <- transactions %>%
    filter(batch == batch_num) %>%
    mutate(
      Category = case_when(
        account_type == "revenues" ~ category_revenues,
        account_type == "cogs" ~ category_cogs,
        account_type == "opex" ~ category_opex,
        TRUE ~ NA_character_
      ),
      Amount = as.numeric(gsub(",", "", amount_manual))
    ) %>%
    filter(!is.na(Category)) %>%
    group_by(Category) %>%
    summarise(Batch_X = sum(Amount, na.rm = TRUE))

  category_mapping <- c(
    "cogs_chicks_purchased" = "Chicks Purchased",
    "cogs_feeds" = "Feed Costs",
    "cogs_vet" = "Veterinary Supplies",
    "opex_other" = "Other Operating Expenses",
    "opex_salaries_wages" = "Salaries and Wages",
    "opex_transport" = "Transportation Costs",
    "opex_utilities" = "Utilities (Electricity, Water, etc.)",
    "revenues_sales" = "Sales Revenue"
  )

  batch$Category <- category_mapping[batch$Category]

  categories <- data.frame(
    Category = c(
      "Revenues", "Sales Revenue", "Cost of Goods Sold (COGS)", "Feed Costs",
      "Veterinary Supplies", "Chicks Purchased", "Other Direct Costs",
      "Operating Expenses (OPEX)", "Salaries and Wages", "Utilities (Electricity, Water, etc.)",
      "Transportation Costs", "Marketing Expenses", "Other Operating Expenses",
      "Net Profit/Loss"
    )
  )

  batch_full <- categories %>%
    left_join(batch, by = "Category") %>%
    mutate(Batch_X = coalesce(Batch_X, 0)) %>%
    mutate(Batch_X = case_when(
      Category == "Revenues" ~ sum(Batch_X[Category == "Sales Revenue"]),
      Category == "Cost of Goods Sold (COGS)" ~ sum(Batch_X[Category %in% c("Feed Costs", "Veterinary Supplies", "Chicks Purchased", "Other Direct Costs")]),
      Category == "Operating Expenses (OPEX)" ~ sum(Batch_X[Category %in% c("Salaries and Wages", "Utilities (Electricity, Water, etc.)", "Transportation Costs", "Marketing Expenses", "Other Operating Expenses")]),
      TRUE ~ Batch_X
    ))

  batch_full <- batch_full |>
    mutate(
      Batch_X = ifelse(
        Category == "Net Profit/Loss",
        Batch_X[Category == "Revenues"] -
          (Batch_X[Category == "Cost of Goods Sold (COGS)"] +
             Batch_X[Category == "Operating Expenses (OPEX)"]),
        Batch_X
      )
    )

  colnames(batch_full)[2] <- paste("Batch", batch_num)

  batch_full
}

# Create a tiny sparkline-style plotly chart for value box showcases
jc_sparkline <- function(values, color = "#2E86AB") {
  plot_ly(
    x = seq_along(values), y = values,
    type = "scatter", mode = "lines",
    line = list(color = color, width = 2),
    fill = "tozeroy",
    fillcolor = paste0(color, "33")
  ) |>
    layout(
      xaxis = list(visible = FALSE, fixedrange = TRUE),
      yaxis = list(visible = FALSE, fixedrange = TRUE),
      margin = list(l = 0, r = 0, t = 0, b = 0),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
}
