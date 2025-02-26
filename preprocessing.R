library(tidyverse)
library(readxl)

#Function to reduce long column names
abbreviate_long_colnames <- function(data) {
  
  data %>%
    rename_with(~ sapply(strsplit(.x, "\\("), `[`, 1) %>% 
                  trimws())
}

raw <- read_xlsx("MockDataSet1.xlsx") %>%
  dplyr::select(-1)

#check inputs to non-numeric cols
print(raw %>%
        dplyr::select(where(is.character)) %>%
        lapply(unique))

factor_cols <- c(1, 2, 3, 4, 5, 7, 10)
numeric_cols <- c(6, 8, 9, 11, 12, 13, 14,
             15, 16, 17, 18)

#cleaning, changing coltypes
data <- raw %>%
  mutate(Smoker = case_when(Smoker == "No" ~ "N",
                                      TRUE ~ Smoker),
         `Joint?` = case_when(`Joint?` == "Yes" ~ "Y",
                              `Joint?` == "No" ~ "N",
                              TRUE ~ `Joint?`),
         across(all_of(factor_cols), as.factor),
         across(all_of(numeric_cols), as.numeric),
         across(everything(), ~ replace(., . == -999997, NA)),
         across(everything(), ~ replace(., . == "NULL", NA)),
         `LSB (Regional banded house price band)` = case_when(
           `LSB (Regional banded house price band)` == 99 ~ NA,
           TRUE ~ `LSB (Regional banded house price band)`),
         past_ccj = as.factor(case_when(`BB (Number of CCJs)` > 0 ~ 1,
                                        is.na(`BB (Number of CCJs)`) ~ 0,
                                        TRUE ~ 0)),) %>%
  unique()

#missing values
missmap(data %>% abbreviate_long_colnames(),
        main = "Missing values vs observed")

#data exploration
product_sale_rates <- data %>%
  group_by(Product) %>%
  count(Sold) %>%
  mutate(prop = n / sum(n))

ggplot(data) +
  geom_histogram(aes(x = TotalPremium, fill = Sold),
                 binwidth = 5, position = "stack") +
  scale_x_continuous(limits = c(0, 150)) +
  labs(title = "Sales proportions at different premium buckets") +
  theme_minimal()

ggplot(data) +
  geom_point(aes(x = NBC, y = TotalPremium)) +
  scale_x_continuous(limits = c(0, 5000)) +
  facet_wrap(vars(Term))

ggplot(data) +
  geom_point(aes(x = NBC, y = `URB (Income group)`)) +
  scale_x_continuous(limits = c(0, 5000)) 

ggplot(data) +
  geom_point(aes(x = NBC, y = `Person1 Age`)) +
  scale_x_continuous(limits = c(0, 2000))  +
  geom_smooth(aes(x = NBC, y = `Person1 Age`),
              method = "lm", se = FALSE)

ggplot(data) +
  geom_point(aes(x = TotalPremium, y = Smoker)) +
  facet_wrap(vars(Term))

product_profit <- data %>%
  group_by(Product) %>%
  summarise(p = mean(NBC))

sale_at_income_bands <- data %>%
  mutate(Sold = case_when(Sold == "Y" ~ 1,
                          TRUE ~ 0)) %>%
  filter(!is.na(`URB (Income group)`)) %>%
  group_by(`URB (Income group)`) %>%
  summarise(tp = mean(Sold, na.rm = T)) %>%
  ggplot(aes(x = `URB (Income group)`, y = tp)) +
  geom_line() +
  geom_point(colour = "coral", size = 5) +
  labs(title = "Sale rates at each income group band",
       y = "% quotes leading to sale") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.5))
