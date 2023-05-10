#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)


# Read the dataset from the CSV file
sta_dataset<- read.csv("../data/combined_dataset.csv")

#Analyses
# Test of H1: Correlation and significance test
cor_test1 <- cor.test(sta_dataset$MonthlyIncome, sta_dataset$PerformanceRating)
cor_test1_table<- data.frame(r = cor_test1$estimate, p = cor_test1$p.value)
cor_test1_table
# Save H1 results
write.csv(cor_test1_table,"../out/H1.csv", row.names = FALSE)

# Test of H2: ANOVA and significance tests with funtions in rstatix package
anova_rstatix<- sta_dataset %>%
  anova_test(MonthlyIncome ~ Department)
# Save H2 results
write.csv(anova_rstatix, "../out/H2.csv",row.names = FALSE)
#Test of H2: ANOVA and significance tests, traditional ANOVA ?????
anova_result <- aov(MonthlyIncome ~ Department, data = predict_dataset)
summary(anova_result)
#save traditional?????

# Test of H3: Regression and significance tests
mod_h3 <- lm(YearsAtCompany ~ RelationshipSatisfaction * Gender, data = sta_dataset)
# Get the model summary as a tidy data frame
tidy_mod_h3 <- tidy(mod_h3)
# Create a data frame for marginal effects (predicted values)
marginal_effects <- sta_dataset %>%
  mutate(PredictedYearsAtCompany = predict(mod_h3, newdata = .))
# Save H3 coefficients table
write.csv(select(tidy_mod_h3, term, estimate, statistic, p.value), "../out/H3.csv", row.names = FALSE)

#Visualization
# Visualization of H1: Scatterplot and fit line
h1_plot <- ggplot(predict_dataset, aes(x = MonthlyIncome, y = PerformanceRating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Monthly Income vs Performance Rating",
       x = "Monthly Income",
       y = "Performance Rating")
ggsave("../fig/H1.png", plot = h1_plot, units = "px", width = 1920, height = 1080)

# Visualization of H2: Boxplot split by department
ggplot(sta_dataset, aes(x = Department, y = MonthlyIncome)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Monthly Income by Department",
       x = "Department",
       y = "Monthly Income")
ggsave(filename="../fig/H2.png", units="px", width=1920, height=1080)

#Visualization of H3: Scatterplot and fit lines
h3_plot <- ggplot(marginal_effects, aes(x = RelationshipSatisfaction, y = PredictedYearsAtCompany, color = Gender)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Predicted Tenure & Relationship Satisfaction",
       x = "Relationship Satisfaction",
       y = "Predicted Tenure (Years)")
ggsave("../fig/H3.png", plot = h3_plot, units="px", width=1920, height=1080)


#Publication
# Publication Results for H1 (need to edit forno leading zero)
p_value <- round(cor_test$p.value, 2) # Format p values
correlation <- round(cor_test$estimate, 2) #correlation
df <- cor_test$parameter  # extract df
significant <- ifelse(p_value < 0.05, "was", "was not") # Check for significance
# Print the result
print(sprintf("The correlation between Monthly Income and Performance Rating is r(%d) = %.2f, p = %.2f. This test %s statistically significant.", df, correlation, p_value, significant))

# Publication Results for H2


# Publication Results for H3


