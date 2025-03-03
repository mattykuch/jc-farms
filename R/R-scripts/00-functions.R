# Functions we'll source to qmd

# plot functions

create_shotchart <- function(data, selected_player, selected_date) {
  # Filter the data for the specified player and date
  filtered_data <- data |>
    # Player
    filter(namePlayer == selected_player) |> 
    # Game data
    filter(dateGame == selected_date) |>
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
  
  # Create the shot chart plot
  plot <- # draws the NBA half court
    BasketballAnalyzeR::drawNBAcourt(
      ggplot(data = filtered_data), size = 0.5, col = "grey20") +
    
    # shot data
    geom_point(aes(x = x, y = y, fill = court_zone), shape = 21, color = "white", size = 2.5, alpha = 0.8)
  
  return(plot)
  
  
}

shots_game_by_game_slope_chart <- function(data,selected_player,game_1,game_2) {
  
    # Filter the data for the specified player and date
    filtered_data <- data |>
    # Player
    filter(namePlayer == selected_player) |> 
    # Game 1  and 2 shots data
    filter(dateGame %in% c(game_1, game_2)) |>
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
      )) |> 
    group_by(dateGame, court_zone) |>
    summarize(total_shots_two_games = n(), .groups = "drop") |>
    group_by(dateGame) |>
    mutate(
      all_shots_two_games = sum(total_shots_two_games),
      percentage = (total_shots_two_games / all_shots_two_games) * 100
    ) |>
    ungroup()
  
  # Add label positions: midpoint between start and end points
  label_positions <- filtered_data |>
    group_by(court_zone) |>
    summarize(
      x_pos = mean(as.numeric(as.factor(dateGame))),  # Midpoint of x positions
      y_pos = mean(percentage),                      # Average percentage for the label
      .groups = "drop"
    )
  
  # Create a slope chart. FYI-> "Mid-Range" = "#50C878"
  plot <- ggplot(filtered_data, aes(x = as.factor(dateGame), y = percentage, group = court_zone, color = court_zone)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_text(data = label_positions, aes(x = x_pos, y = y_pos, label = court_zone, hjust= 0.1, vjust=1 ), size = 4, show.legend = FALSE) +
    scale_color_manual(
      values = c("Paint" = "#99CCFF", "Mid-Range" = "#50C878", "3PT" = "#FF9999")
    )
    
  return(plot)   
  
}

create_linechart <- function(data,selected_player,start_season,end_season) {
  # Filter the data for the specified player and date
  filtered_data <- data |>
    # Player
    filter(namePlayer == selected_player) |>
    # Add season as factor column, so we can order in later
    # mutate(season = as.factor(yearSeason)) |>
    # create a line chart for a specific range
    filter(yearSeason >= start_season & yearSeason <= end_season) |>
    # Scale x and y coordinates (court dimensions adjusted manually)
    mutate(x = (locationX / 10) - 0,
           y = (locationY / 10) - 41.75) |>
    mutate(
      # Categorize shots based on distance from the basket
      court_zone = case_when(
        distanceShot <= 8 ~ "Paint",               # Shots close to the basket
        distanceShot > 8 & distanceShot < 23 ~ "Mid-Range",  # Shots outside the paint but inside the 3PT line
        distanceShot >= 23 ~ "3PT",                # Shots beyond the 3-point line
        TRUE ~ "Other"                            # Catch-all for undefined cases
      )
    )
  
  # Group by season and court zone, then summarize frequencies
  
  filtered_summary <- filtered_data |>
    group_by(yearSeason, court_zone) |>
    summarize(total_shots = n(), .groups = "drop") |>
    group_by(yearSeason) |>
    mutate(
      overall_shots = sum(total_shots),  # Calculate overall shots per season
      percentage = (total_shots / overall_shots) * 100  # Calculate percentage by court zone
    ) |>
    ungroup()
  
  # Linechart to show shot preferences over time
  plot <- ggplot(filtered_summary, aes(x = yearSeason, y = percentage, color = court_zone, group = court_zone)) +
    geom_line(linewidth=1) +
    geom_point(size = 2)
  
  return(plot)
  
}
