# modeling goal is to estimate whether some characteristics of the people
# trafficked by enslavers changed over the last several decades of the 
# trans-atlantic slave trade.

library(tidyverse)

african_names <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv")

african_names %>%
  count(port_disembark, sort = TRUE)

african_names %>%
  add_count(port_embark) %>% 
  mutate(port_embark = case_when(
    n < 4000 ~ "Other",
    TRUE ~ port_embark
  )) %>%
  ggplot(aes(port_embark, year_arrival, fill = port_embark)) +
  geom_boxplot(alpha = 0.4, show.legend = FALSE) +
  labs(x = NULL, y = "Year")

# When is this data from?
african_names %>%
  ggplot(aes(year_arrival)) +
  geom_histogram(bins = 20, fill = "midnightblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    y = "Number of liberated individuals",
    x = NULL
  )

# What is the pattern of missing data?
library(naniar)

african_names %>%
  select(gender, age, height, year_arrival) %>%
  gg_miss_upset()

# What is the relationship between gender and year of arrival?

african_names %>%
  ggplot(aes(gender, year_arrival, fill = gender)) +
  geom_boxplot(alpha = 0.4, show.legend = FALSE) +
  labs(
    x = NULL,
    y = "Year"
  )

african_names %>%
  ggplot(aes(gender, age, fill = gender)) +
  geom_boxplot(alpha = 0.4, show.legend = FALSE) +
  labs(x = NULL, y = "Year")

# What is the relationship between age and year of arrival?

african_names %>%
  filter(year_arrival < 1850) %>%
  group_by(year_arrival) %>%
  summarise(age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(year_arrival, age)) +
  geom_line(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Mean Age")

library(ggrepel)

african_names %>%
  group_by(name) %>%
  summarise(
    n = n(),
    age = mean(age, na.rm = TRUE),
    year_arrival = mean(year_arrival, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(-n) %>%
  filter(n > 30) %>%
  ggplot(aes(year_arrival, age)) +
  geom_text_repel(aes(label = name), size = 3, family = "IBMPlexSans") +
  geom_point(aes(size = n), color = "midnightblue", alpha = 0.7) +
  labs(
    x = "Mean year of arrival", y = "Mean Age",
    size = "Number of People",
    title = "Age and year of arrival for most common names of transported captives",
    caption = "African Names Database from slavesvoyages.org"
  )

# Impute missing data

liberated_df <- african_names %>%
  filter(year_arrival < 1850) %>%
  mutate(gender = case_when(
    gender == "Boy" ~ "Man",
    gender == "Girl" ~ "Woman",
    TRUE ~ gender
  )) %>%
  mutate_if(is.character, factor)

liberated_df

# Let's impute missing data using a recipe

library(recipes)

impute_rec <- recipe(year_arrival ~ gender + age + height, data = liberated_df) %>%
  step_meanimpute(height) %>%
  step_knnimpute(all_predictors())

# We tell our recipe what kind of model we are building, with what variables.
# Next, we impute missing values for age and gender using a k nearest neighbors
# model with all three predictors.

imputed <- prep(impute_rec) %>% juice()

summary(liberated_df$age)
summary(imputed$age)

# The point of imputation in this manner is to retain the information we have in
# the dataset without throwing it away, which feels especially important with
# historical data on individuals who experienced enslavement.

fit_lm <- lm(year_arrival ~ gender + age, data = imputed)

summary(fit_lm)

tidy(fit_lm)





