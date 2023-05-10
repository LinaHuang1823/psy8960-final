#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

#Data Import and Cleaning
dataset <- read_delim("../data/dataset.csv", delim = "+", col_names = T) %>%
  mutate(employee_ID = as.numeric(1:1470))

satisfaction_reviews <- read_delim("../data/satisfaction_reviews.csv", 
                                   delim = ".", 
                                   col_names = c("good", "bad", "employee_ID"))%>%
  arrange(employee_ID)

combined_dataset <- merge(dataset, satisfaction_reviews, by = "employee_ID")


# Save the dataset as a CSV file
write.csv(combined_dataset, "../data/combined_dataset.csv", row.names = FALSE)





