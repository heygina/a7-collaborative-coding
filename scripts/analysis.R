#data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv")
#library(dplyr)
info_function <- function(data) {
  data <- data %>% rename(Class_Standing = What.is.your.current.class.standing., 
                          Operating_System = What.operating.system.do.you.typically.use.,
                          Applying_to_Info = Are.you.interested.in.applying.to.the.Informatics.major.,
                          R_Experience = What.is.your.familiarity.with..Using.the.R.programming.language,
                          Git_VC_Experience = What.is.your.familiarity.with..using.git.for.version.control,
                          Animal = Do.you.consider.yourself.
                          )
  ret <- list()

  #counts the number of columns
  ret$NumCols <- length(data)
  
  # counts the number of students for each class standing (Freshman, Sophomore, Junior, Senior)
  ret$Freshman <-nrow(data %>% 
                        filter(Class_Standing == "Freshman"))
  ret$Sophomore <- nrow(data %>% 
                          filter(Class_Standing == "Sophomore"))
  ret$Junior <- nrow(data %>%
                       filter(Class_Standing == "Junior"))
  ret$Senior <- nrow(data %>% 
                       filter(Class_Standing == "Senior"))
  
  # counts number of people per operating system (Windows, Mac, and Others(anything not Windows or Mac))
  ret$Mac <- nrow(data %>% 
                      filter(Operating_System == "Mac"))
  ret$Windows <- nrow(data %>% 
                        filter(Operating_System == "Windows"))
  ret$Other <- nrow(data %>% 
                      filter(Operating_System != "Windows")
                    %>% filter(Operating_System != "Mac"))
  
  # different levels of programming experience
  ret$RBeginner <- nrow(data %>% 
                          filter(R_Experience == "Never used it"))
  ret$RFewTimes <- nrow(data %>% 
                          filter(R_Experience == "Have used it a few times"))
  ret$RIntermediate <- nrow(data %>%
                          filter(R_Experience == "Intermediate"))
  
  # Number of people applying to Informatics
  ret$YesApplying <- nrow(data %>% 
                            filter(Applying_to_Info == "Yes"))
  ret$MaybeApplying <- nrow(data %>% 
                              filter(Applying_to_Info == "Maybe"))
  ret$NoApplying <- nrow(data %>% 
                           filter(Applying_to_Info == "No"))
  
  # Git Version Control Experience
  ret$GitExpNone <- nrow(data %>% 
                           filter(Git_VC_Experience == "Never used it"))
  ret$GitExpSome <- nrow(data %>% 
                           filter(Git_VC_Experience == "Have used it a few times"))
  ret$GitExpIntermediate <- nrow(data %>% 
                                   filter(Git_VC_Experience == "Intermediate user"))
  
  # types of people: cat, dog, both, none
  ret$Cat <- nrow(data %>% 
                    filter(Animal == "A cat person...."))
  ret$Dog <- nrow(data %>% 
                    filter(Animal == "A dog person..."))
  ret$Both <- nrow(data %>% 
                     filter(Animal == "Both!"))
  ret$Neither <- nrow(data %>% 
                     filter(Animal == "Neither"))
  
  return (ret)
}