# Load required libraries
library(tidyverse)
library(BasketballAnalyzeR)
library(nbastatR)
library(ggimage)
library(cropcircles)
library(ggtext)
library(glue)
library(janitor)
library(htmltools)
library(sysfonts)
library(plotly)

# Call chart creating functions
# Source functions
source(file="./R/00-functions.R")

# Set a grey color for consistency in the visualization
grey <- '#818990'

# Use the Chivo font (you must install this locally)
font <- sysfonts::font_add("Chivo", "./fonts/Chivo/static/Chivo-Regular.ttf")

#Import shots data

all_shots <- read.csv("./data/allshots-2003-2024.csv") 
df_dict_nba_teams <- read.csv("./data/df_dict_nba_teams.csv")

lebron_shots <- all_shots |> 
  filter(namePlayer == "LeBron James")



# Filter for specific seasons and scale the x and y coordinates
lebron_shots_season <- lebron_shots |>
  filter(yearSeason %in% c("2004", "2024")) |>
  # Scale x and y coordinates (court dimensions adjusted manually)
  mutate(x = (locationX / 10) - 0,
         y = (locationY / 10) - 41.75) |>
  # Remove shots from beyond half-court range
  filter(y < 0) |> 
  mutate(
    # Categorize shots based on distance from the basket
    court_zone = case_when(
      distanceShot <= 8 ~ "Paint",               # Shots close to the basket
      distanceShot > 8 & distanceShot < 23 ~ "Mid-Range",  # Shots outside the paint but inside the 3PT line
      distanceShot >= 23 ~ "3PT",                # Shots beyond the 3-point line
      TRUE ~ "Other"                            # Catch-all for undefined cases
    )
  )

#Calculate the player statistics 

# Group data by dateofGame and courtzone, then calculate stats
lebron_stats <- lebron_shots_season |>
  filter(dateGame %in% c("20031029", "20231029")) |>
  group_by(nameTeam,dateGame,court_zone) |>
  summarise(total_shots = n(), .groups = 'drop') |>
  pivot_wider(names_from = court_zone, values_from = total_shots, values_fill = 0) |>
  janitor::clean_names() |>
  mutate(total = x3pt + mid_range + paint,
         x3pt_pct = round(100 * x3pt / total,1),
          mid_range_pct = round(100 * mid_range / total,1),
          paint_pct = round(100 * paint / total, 1)) %>% 
  ungroup()

# Creating the visualization
# Step 1: Create data frame with player images and labels
images <- data.frame(
  nameTeam = c( "Cleveland Cavaliers", "Los Angeles Lakers"),
  date = c("October 29, 2003", "October 29, 2023"),
  label = c("Rookie LeBron", "20-Year Vet LeBron"),
  image = c("./assets/lebron_2003.png",
            "./assets/lebron_2023.png")
) |>
  left_join(lebron_stats, by = c("nameTeam" = "name_team")) |>
  mutate(text_label_1 = glue("<span style='font-size:14px;'>**{toupper(label)}**</span><br>
                            <span style='font-size:14px;'>**{toupper(date)}**</span><br>
                           <span style='font-size:12.5px;color:grey40;'>{nameTeam}</span><br>"),
         
         text_label_2 = glue("<span style='font-size:12.5px;color:grey40;'>**{total} shot attempts**</span><br>
                           <span style='color:#FF9999;font-size:12.5px;'>{round(x3pt_pct,0)}% were 3PT-ers</span><br>
                           <span style='color:#50C878;font-size:12.5px;'>{round(mid_range_pct,0)}% were Mid-Range</span><br>
                           <span style='color:#99CCFF;font-size:12.5px;'>{round(paint_pct,0)}% were in the Paint</span>")
  )

# Step 2: Circle crop player images
images$cropped <- cropcircles::circle_crop(
  images = images$image, border_size = 1, border_colour = "whitesmoke"
)

# Create 2 separate tables for the images
first_game_images <- images |> 
  filter(date_game == "20031029")

year_twenty_game_images <- images |> 
  filter(date_game == "20231029")

# Step 3: Creating the short chart visualization

# first game shot chart
base_chart_first_game <- create_shotchart(all_shots,"LeBron James",20031029)

draft_chart_first_game <- base_chart_first_game + 
  
  # backdrop image with fill to create border
  geom_image(data=first_game_images, mapping=aes(x=-20, y=6, image=cropped), color="#818990", size=0.16, asp=1/1.2) +
  
  # player image
  geom_image(data = first_game_images, aes(x = -20, y = 6, image = cropped), size = 0.15, asp = 1 / 1.2) +
  
  # text_label_1 with name and team
  
  ggtext::geom_textbox(data = first_game_images, aes(x = 3.8, y = 6, label = text_label_1), 
                       fill = NA, box.size = NA) +
  
  # text_label_2 with stats
  
  ggtext::geom_textbox(data = first_game_images, aes(x = -15, y = -7, label = text_label_2), 
                       fill = NA, box.size = NA) +
  
  # Create your own discrete scale
  scale_fill_manual(values=rev(c("#99CCFF", "#50C878", "#FF9999"))) +
  
  # For position scales, a vector of range expansion constants used to add some padding around the data to ensure that they are placed some distance away from the axes
  
  scale_y_continuous(expand=c(0.1,0.2)) +
  
  # Cartesian coordinates with fixed "aspect ratio"
  coord_equal() +
  
  # Set guides for each scale
  guides(fill = guide_legend(override.aes=list(size=5)))

draft_chart_first_game

# Set the output directory
output_dir <- "./assets/"

# Save the plot in PNG format
ggsave(paste0(output_dir, "rookie_shot_chart.png"), plot = draft_chart_first_game, width = 7.5, height = 7.5)

# year twenty shot chart
base_chart_yr_twenty_game <- create_shotchart(all_shots,"LeBron James",20231029)

draft_chart_yr_twenty_game <- base_chart_yr_twenty_game + 
  
  # backdrop image with fill to create border
  geom_image(data=year_twenty_game_images, mapping=aes(x=-20, y=6, image=cropped), color="#818990", size=0.16, asp=1/1.2) +
  
  # player image
  geom_image(data = year_twenty_game_images, aes(x = -20, y = 6, image = cropped), size = 0.15, asp = 1 / 1.2) +
  
  # text_label_1 with name and team
  
  ggtext::geom_textbox(data = year_twenty_game_images, aes(x = 3.8, y = 6, label = text_label_1), 
                       fill = NA, box.size = NA) +
  
  # text_label_2 with stats
  
  ggtext::geom_textbox(data = year_twenty_game_images, aes(x = -15, y = -7, label = text_label_2), 
                       fill = NA, box.size = NA) +
  
  # Create your own discrete scale
  scale_fill_manual(values=rev(c("#99CCFF", "#50C878", "#FF9999"))) +
  
  # For position scales, a vector of range expansion constants used to add some padding around the data to ensure that they are placed some distance away from the axes
  
  scale_y_continuous(expand=c(0.1,0.2)) +
  
  # Cartesian coordinates with fixed "aspect ratio"
  coord_equal() +
  
  # Set guides for each scale
  guides(fill = guide_legend(override.aes=list(size=5)))

draft_chart_yr_twenty_game

# Save the plot in PNG format
ggsave(paste0(output_dir, "veteran_shot_chart.png"), plot = draft_chart_yr_twenty_game, width = 7.5, height = 7.5)

# Final Visualizations - All Styled Up

# Rookie Lebron Final Viz

final_chart_first_game <- draft_chart_first_game +
  theme(legend.position = "top", # move the legend to the top
        legend.title = element_text(face="bold", size=12), # make the title bold and increase the size
        plot.margin = margin(t=20), #increases the Top margin of the plot by 20
        legend.text = element_text(size=12), #inceases size of text 
        legend.margin = margin(rep(0,4)),# Adjusting legend margin
        legend.key = element_blank(), # Remove default legend key
        axis.text = element_blank(), # Remove default axit text
        legend.justification = "center", # Move legend to the center
        panel.background = element_blank(),# Remove panel background
        panel.grid.minor=element_blank(),# Remove panel grid minor
        axis.title=element_blank(), # Remove axis titles
        axis.ticks = element_blank())# Remove axis ticks

# Save the plot in PNG format
ggsave(paste0(output_dir, "final_rookie_shot_chart.png"), plot = final_chart_first_game, width = 7.5, height = 7.5)

# Veteran LeBron Final Viz

final_chart_yr_twenty_game <- draft_chart_yr_twenty_game +
  theme(legend.position = "top", # move the legend to the top
        legend.title = element_text(face="bold", size=12), # make the title bold and increase the size
        plot.margin = margin(t=20), #increases the Top margin of the plot by 20
        legend.text = element_text(size=12), #inceases size of text 
        legend.margin = margin(rep(0,4)),# Adjusting legend margin
        legend.key = element_blank(), # Remove default legend key
        axis.text = element_blank(), # Remove default axit text
        legend.justification = "center", # Move legend to the center
        panel.background = element_blank(),# Remove panel background
        panel.grid.minor=element_blank(),# Remove panel grid minor
        axis.title=element_blank(), # Remove axis titles
        axis.ticks = element_blank())# Remove axis ticks

# Save the plot in PNG format
ggsave(paste0(output_dir, "final_veteran_shot_chart.png"), plot = final_chart_yr_twenty_game, width = 7.5, height = 7.5)
