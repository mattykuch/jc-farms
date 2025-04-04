
#Libraries

library(tidyverse)
library(lubridate)
library(highcharter)
library(DT)
library(bslib)
library(bsicons)
library(shiny)
library(gt)
library(glue)
library(gtExtras)
library(kableExtra)
library(readxl)
library(scales)
library(formattable)

# Get Data from Batch 1 to 3

pnl_data <- read_excel("data/JC Poultry Farm - Business Operating Model_v1_250323.xlsx", 
                       sheet = "PnL",
                       skip = 3)  # Skip the first 3 rows to get to the data

# Clean and transform the data
pnl_statement <- pnl_data %>%
  # Remove any NA rows
  filter(!is.na(`Category`)) %>%
  # Rename columns for clarity
  rename(
    Category = `Category`,
    description = `Description`,
    Batch_1 = `Amount (UGX)...3`,
    Batch_2 = `Amount (UGX)...4`,
    Batch_3 = `Amount (UGX)...5`
  ) %>%
  # Select only the relevant columns
  select(Category, Batch_1, Batch_2, Batch_3)

# Get Batch 4 data

# Read CSV data
transactions <- read.csv("data/jc_farms_transaction_data_20250403.csv")

# Try the same for df_raw via the remote database

transact_data <- df_raw

# Filter and aggregate Batch_4 data
batch4 <- transactions %>%
  filter(Select.Batch.Number == "Batch 4") %>%
  mutate(
    Category = case_when(
      Select.Account.Type == "Revenues" ~ Select.Category.3,
      Select.Account.Type == "Cost of Goods Sold (COGS)" ~ Select.Category.2,
      Select.Account.Type == "Operating Expenditure (OPEX)" ~ Select.Category.1,
      TRUE ~ NA_character_
    ),
    Amount = as.numeric(gsub(",", "", Amount..UGX.))
  ) %>%
  filter(!is.na(Category)) %>%
  group_by(Category) %>%
  summarise(Batch_4 = sum(Amount, na.rm = TRUE))

# ALT approach - filter transact_data
batch4_2 <- transact_data |> 
  filter(batch == "4") |> 
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
  summarise(Batch_4 = sum(Amount, na.rm = TRUE)
    
  )

# ALT approach - batch 5

batch5 <- transact_data |> 
  filter(batch == "5") |> 
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
  summarise(Batch_5 = sum(Amount, na.rm = FALSE)
            
  )


#Sub-setting the 1 cell I want to change
batch4$Category[5] <- "Sales Revenue"

# Using a named vector with the match function
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
batch4_2$Category <- category_mapping[batch4_2$Category]

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

batch4_full <- categories %>%
  left_join(batch4, by = "Category") %>%
  mutate(Batch_4 = coalesce(Batch_4, 0)) %>%
  # Calculate totals
  mutate(Batch_4 = case_when(
    Category == "Revenues" ~ sum(Batch_4[Category == "Sales Revenue"]),
    Category == "Cost of Goods Sold (COGS)" ~ sum(Batch_4[Category %in% c("Feed Costs", "Veterinary Supplies", "Chicks Purchased", "Other Direct Costs")]),
    Category == "Operating Expenses (OPEX)" ~ sum(Batch_4[Category %in% c("Salaries and Wages", "Utilities (Electricity, Water, etc.)", "Transportation Costs", "Marketing Expenses", "Other Operating Expenses")]),
    TRUE ~ Batch_4
  ))

# Merge categories to batch4_2 as well
batch4_2_full <- categories %>%
  left_join(batch4_2, by = "Category") %>%
  mutate(Batch_4 = coalesce(Batch_4, 0)) %>%
  # Calculate totals
  mutate(Batch_4 = case_when(
    Category == "Revenues" ~ sum(Batch_4[Category == "Sales Revenue"]),
    Category == "Cost of Goods Sold (COGS)" ~ sum(Batch_4[Category %in% c("Feed Costs", "Veterinary Supplies", "Chicks Purchased", "Other Direct Costs")]),
    Category == "Operating Expenses (OPEX)" ~ sum(Batch_4[Category %in% c("Salaries and Wages", "Utilities (Electricity, Water, etc.)", "Transportation Costs", "Marketing Expenses", "Other Operating Expenses")]),
    TRUE ~ Batch_4
  ))



# Calculate Net Profit/Loss

batch4_full <- batch4_full |> 
  mutate(
    Batch_4 = ifelse(
      Category == "Net Profit/Loss",
      Batch_4[Category == "Revenues"] - 
        (Batch_4[Category == "Cost of Goods Sold (COGS)"] + 
           Batch_4[Category == "Operating Expenses (OPEX)"]),
      Batch_4
    )
  )


# Merge with original PnL
final_pnl <- pnl_statement %>%
  left_join(batch4_full, by = "Category")

# Format numbers with commas
final_pnl <- final_pnl %>%
  mutate(across(starts_with("Batch"), ~format(., big.mark = ",", scientific = FALSE)))

# Display results
print(final_pnl)