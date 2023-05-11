#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)
library(stringr)

# Data Import and Cleaning
sta_dataset<- read.csv("../data/combined_dataset.csv")

# Analysis
# Test of H1: Correlation and significance test
cor_H1 <- cor.test(sta_dataset$MonthlyIncome, sta_dataset$PerformanceRating)

# Test of H2: ANOVA and significance tests with functions in rstatix package
anova_H2 <- aov(MonthlyIncome ~ Department, data = sta_dataset)
summary(anova_H2)
#Create a publication-ready table (component name, SS, df, MS, F, p)
anova_summary <- summary(anova_H2) # Extract the summary statistics from the ANOVA object
# Create a data frame with the necessary information
anova_summary_df <- data.frame(
  Df = c(anova_summary[[1]]$Df[1], anova_summary[[1]]$Df[2]),
  SumSq = c(anova_summary[[1]]$Sum[1], anova_summary[[1]]$Sum[2]),
  MeanSq = c(anova_summary[[1]]$Mean[1], anova_summary[[1]]$Mean[2]),
  FValue = c(anova_summary[[1]]$F[1], NA),
  Pr = c(anova_summary[[1]]$"Pr(>F)"[1], NA)
)
# Set the row names for the data frame
rownames(anova_summary_df) <- c("Department", "Residuals")
# Set the output file path
output_file_path <- file.path("../out", "H2.csv")
# Save the table as a CSV file
write.csv(anova_summary_df, file = output_file_path, row.names = TRUE)

# Test of H3: Regression and significance tests
mod_H3 <- lm(YearsAtCompany ~ RelationshipSatisfaction * Gender, data = sta_dataset)
# Get the model summary as a tidy data frame
tidy_mod_h3 <- tidy(mod_H3)
# create table for coefficients, t-tests, and p-values
# Create a data frame with coefficients, t-tests, and p-values
results_table <- data.frame(
  Term = tidy_mod_h3$term,
  Coefficient = sprintf("%.2f", round(tidy_mod_h3$estimate, 2)),
  Statistic = sprintf("%.2f", round(tidy_mod_h3$statistic, 2)),
  P_Value = sprintf("%.2f", round(tidy_mod_h3$p.value, 2))
)
# Remove leading zeros from Coefficient, Statistic, and P_Value
results_table$Coefficient <- str_remove(results_table$Coefficient, "^0")
results_table$Statistic <- str_remove(results_table$Statistic, "^0")
results_table$P_Value <- str_remove(results_table$P_Value, "^0")
# Set the output file path
output_file_path <- file.path("../out", "H3.csv")
# Save the data frame as a CSV file
write.csv(results_table, output_file_path, row.names = FALSE)

#Visualization
# Visualization of H1: Scatterplot and fit line
h1_plot <- ggplot(sta_dataset, aes(x = MonthlyIncome, y = PerformanceRating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Monthly Income vs Performance Rating",
       x = "Monthly Income",
       y = "Performance Rating")
h1_plot
ggsave("../fig/H1.png", plot = h1_plot, units = "px", width = 1920, height = 1080)

# Visualization of H2: Boxplot split by department
h2_plot<-ggplot(sta_dataset, aes(x = Department, y = MonthlyIncome)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Monthly Income by Department",
       x = "Department",
       y = "Monthly Income")
h2_plot
ggsave("../fig/H2.png", plot = h2_plot, units = "px", width = 1920, height = 1080)

#Visualization of H3: Scatterplot and fit lines
# Create a data frame for marginal effects (predicted values)
marginal_effects <- sta_dataset %>%
  mutate(PredictedYearsAtCompany = predict(mod_H3, newdata = .))
#ggplot:scatterplot and fit lines
h3_plot <- ggplot(marginal_effects, aes(x = RelationshipSatisfaction, y = PredictedYearsAtCompany, color = Gender)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Predict Tenure from Relationship Satisfaction",
       x = "Relationship Satisfaction",
       y = "Predicted Tenure (Years)")
h3_plot
ggsave("../fig/H3.png", plot = h3_plot, units="px", width=1920, height=1080)

#Publication
# Publication Results for H1
#create publication-ready sentence for H1
# Extract test statistics and p-value
cor_value <- cor_H1$estimate
df <- cor_H1$parameter
p_value <- cor_H1$p.value
# Format the correlation value and p-value to remove leading zero
formatted_cor_value <- gsub("^0", "", sprintf("%.2f", cor_value))
formatted_p_value <- gsub("^0", "", sprintf("%.2f", p_value))
# Create a publication-ready sentence
sentence <- paste0(
  "The correlation between Monthly Income and Performance Rating was r(",
  df,
  ") = ",
  formatted_cor_value,
  ", p = ",
  formatted_p_value,
  ". This test was ",
  ifelse(p_value > .05, "not", ""),
  " statistically significant."
)
# Print the sentence
print(sentence)

# Publication Results for H2
#create publication-ready sentence for H2
# Extract F-value and p-value
f_value <- round(anova_summary_df$FValue[1], 2)
p_value <- round(anova_summary_df$Pr[1], 2)
# Format the p-value to remove leading zero
formatted_p_value <- gsub("^0\\.", ".", as.character(p_value))
# Create a publication-ready sentence
sentence <- sprintf(
  "The analysis of variance (ANOVA) revealed that there is a difference in monthly pay between departments (F(%d, %d) = %0.2f, p = %s).",
  anova_summary_df$Df[1],
  anova_summary_df$Df[2],
  f_value,
  formatted_p_value
)
# Print the sentence
print(sentence)

# Publication Results for H3
#create publication-ready sentence for H3
intercept <- round(tidy_mod_h3$estimate[1], 2)
rel_sat_coef <- round(tidy_mod_h3$estimate[2], 2)
gender_coef <- round(tidy_mod_h3$estimate[3], 2)
interaction_coef <- round(tidy_mod_h3$estimate[4], 2)
rel_sat_p_value <- round(tidy_mod_h3$p.value[2], 4)
gender_p_value <- round(tidy_mod_h3$p.value[3], 4)
interaction_p_value <- round(tidy_mod_h3$p.value[4], 4)
sentence <- sprintf("In the multiple regression analysis, 
                    the relationship between tenure and all predictors are not significant. 
                    The coefficients and p values are below:
                    relationship satisfaction(b = %0.2f, p = %.4f), 
                    gender(b = %0.2f, p = %.4f). 
                    The interaction effect between relationship satisfaction 
                    and gender was also not significant (b = %0.2f, p = %.4f).",
                    rel_sat_coef,
                    rel_sat_p_value,
                    gender_coef,
                    interaction_coef,
                    interaction_p_value)
print(sentence)
                    




