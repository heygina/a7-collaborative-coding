# Jared Praino
# A7-Collaborative Coding
# Chart 1


library(dplyr)
library(plotly)

# A function that returns a stacked bar chart representing which operating system
# each student uses separate by class standing.
chart_1 <- function(data) {
  
  # Selecting relevant data
  chart_data <- data %>% select(What.is.your.current.class.standing., 
                                What.operating.system.do.you.typically.use.)
  
  # Creating proper names for relevant data
  chart_data <- chart_data %>% rename("Class Standing"              = What.is.your.current.class.standing., 
                                      "Operating System"            = What.operating.system.do.you.typically.use.)
  
  levels(chart_data$`Operating System`)[levels(chart_data$`Operating System`)
                                        =="Other: Windows at home, Linux (Ubuntu) at work"] <- "Other"
  
  # Formatting data
  df <- chart_data %>% group_by(`Class Standing`) %>% 
    summarise(
      windows = length(which(`Operating System`=="Windows")),
      mac = length(which(`Operating System`=="Mac")),
      linux = length(which(`Operating System`=="Linux")),
      other = length(which(`Operating System`=="Other"))
    ) %>% 
    mutate(order = c(1,3,4,2)) %>% 
    arrange(order)
  
  # Creating and displaying a stacked bar graph
   plot_ly(
    x = df$`Class Standing`,
    y = df$windows,
    name = "Windows",
    type = "bar"
    
  ) %>% add_trace(
    x = df$`Class Standing`,
    y = df$mac,
    name = "Max"
    
  ) %>% add_trace(
    x = df$`Class Standing`,
    y = df$linux,
    name = "Linux"
    
  ) %>% add_trace(
    x = df$`Class Standing`,
    y = df$other,
    name = "Other"
    
  ) %>% layout(barmode = "stack",
               xaxis = list(title = "Class Standing"),
               yaxis = list(title = "Number of Students"),
               title = "What Operating System Does Each Class Use?")
} 






