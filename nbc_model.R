library(tidyverse)
library(broom)

#checking correlation between NBC and numeric variables
cor_matrix <- cor(data %>% select_if(is.numeric) %>%
                    abbreviate_long_colnames(),
                  use = "complete.obs")

cor_nbc <- cor_matrix["NBC", , drop = FALSE]
ggcorrplot(cor_nbc, method = "square", lab = T) +
  labs(title = "Correlations between NBC and numeric variables") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

#checking relationship between NBC and categorical variables
anova <- aov(NBC ~ Smoker + `Joint?` + Channel + Product
                     + past_ccj, data = data)
summary(anova)

# Plot F-statistics
ggplot(tidy(anova), aes(x = term, y = statistic)) +
  geom_col(fill = "coral") +
  geom_text(aes(label = round(statistic, 2)), vjust = -0.5) +
  labs(title = "ANOVA F-statistics per Variable") +
  theme_minimal()

data_nbc <- data %>%
  select(-QuoteRef) %>%
  filter(!is.na(`NOB (Property group)`))

#model with past_ccj
ccj_model <- model <- glm(NBC ~ TotalPremium*`NOB (Property group)` + past_ccj, 
             data = data_nbc)
summary(ccj_model)

#model without past_ccj
model_exc_ccj <- glm(NBC ~ TotalPremium*`NOB (Property group)`, 
             data = data_nbc)
summary(model_exc_ccj)

#plotting interaction
data_nbc_summary <- data_nbc %>%
  group_by(`NOB (Property group)`) %>%
  summarise(mean_totalpremium = mean(TotalPremium, na.rm = TRUE))

ggplot(data_nbc_summary, aes(x = factor(`NOB (Property group)`), 
                             y = mean_totalpremium, group = 1)) +
  geom_line() +
  geom_point(size = 3, color = "blue") +  
  labs(title = "Total Premium by Property Group",
       x = "Property Group",
       y = "Mean Total Premium") +
  theme_minimal()

ggplot(data_nbc, aes(x = TotalPremium, y = NBC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ `NOB (Property group)`) +
  labs(title = "Effect of TotalPremium on NBC by NOB (Property group)",
       x = "Total Premium",
       y = "NBC") +
  theme_minimal()
