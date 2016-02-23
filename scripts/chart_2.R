# For testing only:
#library(plotly)
#library(dplyr)
#setwd("/Users/kylewistrand/Documents/Homework/INFO_201/a7-collaborative-coding/")
#data <- read.csv("data/intro_survey_data.csv")

chart_2 <- function(data) {
  # Initially change the column names so they're easier to work with and more clear
  colnames(data) <- c("class", "major", "os", "command", "git", "markdown",
                      "r", "programming", "countries", "animals", "seahawks")
  
  # Make a new column with class standing as a number
  data <- mutate(data, "class_num" = NA)
  data$class_num[data$class == "Freshman"] <- 1
  data$class_num[data$class == "Sophomore"] <- 2
  data$class_num[data$class == "Junior"] <- 3
  data$class_num[data$class == "Senior"] <- 4
  
  # Sort data by class, so plot is in order
  data <- arrange(data, class_num)
  
  # Make and return a box and whisker plot
  plot_ly(data,
    x = class,
    y = countries,
    type = "box"
  ) %>%
  layout(boxmode = "group",
    title = "Number of Countries Visited by Class Standing",
    xaxis = list(title = "Class"),
    yaxis = list(title = "Countries Visited")
  ) %>% 
    return()
}

#chart_2(data)
