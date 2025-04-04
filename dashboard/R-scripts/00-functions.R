jc_dashboard_kpis <- function(data, batch_num, num_birds) {
  
  # KPI-1 - Total Revenues
  
  # Filter data for selected batch only
  
  filtered_data <- data |> 
    filter(Batch_Number == batch_num) |> 
    
    # Filter for Revenue only
    filter(account_type == "revenues")
  
  # Revenues by Batch Number
  revenues_by_batch <- filtered_data %>%
    group_by(Batch_Number) %>%
    summarise(Total_Revenue = sum(Amount_UGX, na.rm = TRUE))
  
  batch_rev <- revenues_by_batch %>% 
    select(Total_Revenue) %>% 
    pull()
  
  batch_rev_final <- format(batch_rev, big.mark = ",", scientific = FALSE)
  
  
  # KPI-2 - Total Cost (of Production)
  
  # Filter data for selected batch only
  
  filtered_data <- data |> 
    filter(Batch_Number == batch_num) |> 
    
    # Filter for COGS and OPEX
    filter(account_type == "cogs" | account_type == "opex" )
  
  # Total Cost by Batch Number
  cost_by_batch <- filtered_data %>%
    group_by(Batch_Number) %>%
    summarise(Total_Cost = sum(Amount_UGX, na.rm = TRUE))
  
  batch_cost <- cost_by_batch %>% 
    select(Total_Cost) %>% 
    pull()
  
  batch_cost_final <- format(batch_cost, big.mark = ",", scientific = FALSE)
  
  
  # KPI-3 - Net Profit
  
  
  net_profit <- batch_rev - batch_cost  
  net_profit_final <- format(net_profit, big.mark = ",", scientific = FALSE)
  
  
  # KPI-4 Cost Per Bird
  
  total_birds <- num_birds
  
  cost_per_bird <- round(batch_cost/total_birds,digits = 0)
  
  cost_per_bird_final <- format(cost_per_bird, big.mark = ",", scientific = FALSE)
  
  
  # KPI-5 Profit per Bird
  
  profit_per_bird <- round(net_profit/total_birds,digits = 0)
  
  profit_per_bird_final <- format(profit_per_bird, big.mark = ",", scientific = FALSE)
  
  # Create a list of KPIs
  
  kpi_list <- c(batch_rev_final,batch_cost_final,net_profit_final,cost_per_bird_final,profit_per_bird_final)
  
  kpi_list
  
}

jc_pnl_by_batch <- function(data, batch_num) {
  
  # Read CSV data
  transactions <- data
  
  # Filter and aggregate Batch_4 data
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
  
  # Apply the mapping to the entire column at once
  batch$Category <- category_mapping[batch$Category]
  
  
  # Create full category structure
  categories <- data.frame(
    Category = c(
      "Revenues", "Sales Revenue", "Cost of Goods Sold (COGS)", "Feed Costs",
      "Veterinary Supplies", "Chicks Purchased", "Other Direct Costs",
      "Operating Expenses (OPEX)", "Salaries and Wages", "Utilities (Electricity, Water, etc.)",
      "Transportation Costs", "Marketing Expenses", "Other Operating Expenses",
      "Net Profit/Loss"
    )
  )
  
  # Merge with categories and fill missing values
  
  batch_full <- categories %>%
    left_join(batch, by = "Category") %>%
    mutate(Batch_X = coalesce(Batch_X, 0)) %>%
    # Calculate totals
    mutate(Batch_X = case_when(
      Category == "Revenues" ~ sum(Batch_X[Category == "Sales Revenue"]),
      Category == "Cost of Goods Sold (COGS)" ~ sum(Batch_X[Category %in% c("Feed Costs", "Veterinary Supplies", "Chicks Purchased", "Other Direct Costs")]),
      Category == "Operating Expenses (OPEX)" ~ sum(Batch_X[Category %in% c("Salaries and Wages", "Utilities (Electricity, Water, etc.)", "Transportation Costs", "Marketing Expenses", "Other Operating Expenses")]),
      TRUE ~ Batch_X
    ))
  
  # Calculate Net Profit/Loss
  
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
  
  colnames(batch_full)[2] <- paste("Batch",batch_num)
  
  batch_full
  
  
}
