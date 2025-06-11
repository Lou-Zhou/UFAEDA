library(tidyverse)
library(ggplot2)
library(shiny)
library(hexbin)
library(RColorBrewer)

ufa_throws <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/ufa_throws.csv")
field_width = 53 + 1/3
field_height = 80
ufa_throws <- ufa_throws |>
  mutate(receiver_x_flipped = -receiver_x) 
thrower_data <- ufa_throws |> dplyr::group_by(thrower) |>
  dplyr::summarise(total = n()) |> dplyr::filter(total > 50)
allThrowers = head(unique(thrower_data$thrower), 500)
segment_data = data.frame(
  x = c( -field_width / 2, field_width / 2, -field_width / 2, -field_width / 2,
         -field_width / 2, -field_width / 2),
  xend = c( -field_width / 2, field_width / 2, field_width / 2, field_width / 2,
            field_width / 2, field_width / 2 ), 
  y = c(-20, -20, -20, 100, 120, 0),
  yend = c(120, 120, -20, 100, 120, 0)
)
brick_points = data.frame(
  x = c(0, 0),
  y = c(20, 50)
)
drawEndLocs <- function(successful = "Both", player = "Keep All", distanceBounds = c(0, 100), showContour = TRUE, showPoints = FALSE){
plot_ufa_throws <- ufa_throws
title = "End Locations of Passes in UFA League"
if(successful == "True"){
  title = paste(title, "| Successful Passes")
}
if(successful == "False"){
  title = paste(title, "| Failed Passes")
}
if(player != "Keep All"){
  title = paste(title, "| Player", player)
}
plot_ufa_throws <- ufa_throws |> 
  dplyr::filter(
    (successful != "True" | turnover == 0),
    (successful != "False" | turnover == 1),
    (player == "Keep All" | thrower == player),
    (thrower_y < (100 - distanceBounds[1]) & thrower_y > (100 -distanceBounds[2]))
  )
plot <- ggplot() + theme_classic() + 
  coord_fixed(ratio = field_width / (field_height + 40)) +
  labs(title = title,
       caption = "Data Courtesy Ultimate Frisbee Association(UFA)"
       ) +
  xlab("X Position") + ylab("Y Position") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend))
if (showContour & nrow(plot_ufa_throws) > 0) {
  #if want to see contour
  plot <- plot +
  stat_density_2d(data = plot_ufa_throws, 
                  mapping = aes(x = receiver_x_flipped, y = receiver_y, fill = after_stat(level)), 
                  geom = "polygon", 
                  contour = TRUE, 
                  colour = "black") +
  scale_fill_distiller(name = "Density", palette = "Blues", direction = 1)
}
if (showPoints) {
  #if want to see end locations
  plot <- plot + geom_point(
    data = plot_ufa_throws,
    aes(x = receiver_x_flipped, y = receiver_y, shape = factor(turnover), color = factor(turnover)),
    size = 2, alpha = .6
  ) + scale_shape_manual(
    values = c("0" = 1, "1" = 4),
    labels = c("0" = "Complete", "1" = "Incomplete"),
    name = "Throw Result"
  ) + scale_color_manual(
      values = c("0" = "darkgreen", "1" = "darkred"),
      labels = c("0" = "Complete", "1" = "Incomplete"),
      name = "Throw Result"
    )  + guides(color = guide_legend(title = "Throw Result"), 
               shape = guide_legend(title = "Throw Result"))
} 
plot
}

getThrwerData <- function(successful, player,distanceBounds = c(0, 100) ) {
  player_data <- ufa_throws
  player_data <- player_data |> 
    dplyr::filter(
      (successful != "True" | turnover == 0),
      (successful != "False" | turnover == 1),
      (player == "Keep All" | thrower == player),
      (thrower_y < (100 - distanceBounds[1]) & thrower_y > (100 -distanceBounds[2]))
    )
  total = nrow(player_data)
  completion = 1 - mean(player_data$turnover)
  paste0("<b>Completion Percentage:</b> ", round(completion, 5), "<br> <b>Number of Throws:</b> ", total)
}

shinyApp(
  ui = fluidPage(
    titlePanel("UFA Throw End Locations"),
    sidebarLayout(
      sidebarPanel(
        selectInput("successPasses", "Plot Successful Passes",
                    c("Keep Both", "True", "False")),
        selectInput("thrower", "Thrower",
                    c(allThrowers,"Keep All")),
        checkboxInput("showContour", "Show Contours", value = TRUE),
        checkboxInput("showPoints", "Show End Locations", value = FALSE),
        sliderInput("distBounds", "Start Distance from End Zone", value = c(0, 100), min = 0, max = 100)
      ),
      mainPanel(
        fluidRow(
          column(12, plotOutput("plot")),
          column(6, htmlOutput("text")),
          
      )
    )
  )),
  server = function(input, output) {
    output$plot <- renderPlot(drawEndLocs(input$successPasses, input$thrower, input$distBounds, input$showContour, input$showPoints))
    output$text <- renderText(getThrwerData(input$successPasses, input$thrower, input$distBounds))
  })
  
#80, 53 1/3
ufa_throws %>% filter(score_diff%in% -4:4) %>% filter(game_quarter%in% 1:4) %>% ggplot(aes(x=game_quarter,y=score_diff, fill = throw_distance))+
  geom_tile() + scale_fill_gradientn(colors = brewer.pal(9, "Blues"), name= "Throw Distance") + theme_classic() +
  labs(title = "Comparing Throw Distance depending on Game State") + 
  xlab("Quarter") + ylab("Score Difference")

ufa_throws |> filter(goal == 1) |> group_by(gameID, possession_num) |>
  summarise(poss_length = max(possession_throw)) |> filter(poss_length == 10)
#gameID = 2021-06-04-MIN-MAD              
#possession_num = 4
#

ufa_throws |> dplyr::filter(gameID == "2021-06-04-MIN-MAD", possession_num == 4) |> dplyr::slice_tail(n = 10) |>
  ggplot() + theme_classic() + 
  coord_fixed(ratio = field_width / (field_height + 40)) +
  labs(title = "Game 2021-06-04-MIN-MAD, Possession 4",
       caption = "Data Courtesy Ultimate Frisbee Association(UFA)"
  ) + xlab("X Position") + ylab("Y Position") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_segment(aes(x = thrower_x, y = thrower_y, xend = receiver_x, yend = receiver_y),  arrow = arrow(type="closed", length=unit(0.5,"mm"))
  ) + geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend))
