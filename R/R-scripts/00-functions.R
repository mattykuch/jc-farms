jc_dashboard_kpis <- function(data, batch_num, num_birds) {
  
  # KPI-1 - Total Revenues
  
  # Filter data for selected batch only
  
  filtered_data <- data |> 
    filter(Batch_Number == batch_num) |> 
    
    # Filter for Revenue only
    filter(Select.Account.Type == "Revenues")
  
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
    filter(Select.Account.Type == "Cost of Goods Sold (COGS)" | Select.Account.Type == "Operating Expenditure (OPEX)" )
  
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
