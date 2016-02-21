library(plotly)
library(dplyr)

setwd("/Users/kylewistrand/Documents/Homework/INFO_201/a7-collaborative-coding/")

data <- read.csv("data/intro_survey_data.csv")

colnames(data) <- c("class", "major", "os", "command", "git", "markdown", "r", "programming", "countries", "animals", "seahawks")

data <- mutate(data, "class_num" = NA)
data$class_num[data$class == "Freshman"] <- 1
data$class_num[data$class == "Sophomore"] <- 2
data$class_num[data$class == "Junior"] <- 3
data$class_num[data$class == "Senior"] <- 4

data <- arrange(data, class_num)

chart_2 <- function(data) {
  plot_ly(data,
    x = class,
    y = countries,
    type = "box"
  ) %>%
  layout(boxmode = "group",
    title = "Number of Countries Visited by Class Standing",
    xaxis = list(title = "Class"),
    yaxis = list(title = "Countries Visited")
  )
}

chart_2(data)
