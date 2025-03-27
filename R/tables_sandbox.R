
# Libraries 

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

# Part 1: Using kableExtra

## Data from mtcars

dt <- mtcars[1:5, 1:6]

# An html table output with no CSS

kbl(dt)

# Adding css via boostrap theme

dt %>%
  kbl() %>%
  kable_styling()

# Adding other themes

dt %>%
  kbl() %>%
  kable_paper("hover", full_width = F)

# Another theme

dt %>%
  kbl(caption = "Recreating booktabs style table") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Highlighting/Bolding Rows and Columns

dt |> 
  kbl(caption = "Recreating booktabs style table") |> 
  kable_paper("striped", full_width = F) %>%
  column_spec(5:7, bold = T) %>%
  row_spec(3:5, bold = T, color = "white", background = "#D7261E")

# Grouped columns and rows

kbl(dt) %>%
  kable_classic() %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))

# Group rows via labelling

kbl(mtcars[1:10, 1:6], caption = "Group Rows") %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Group 1", 4, 7) %>%
  pack_rows("Group 2", 8, 10)

# Add indention
kbl(dt) %>%
  kable_paper("striped", full_width = F) %>%
  add_indent(c(1, 3, 5))
