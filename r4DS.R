
# 7 Exploratory Data Analysis ---------------------------------------------
library(tidyverse)

diamonds2 <-
  diamonds %>%
  filter(between(y, 3, 20))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = faithful) +
  geom_point(x = eruptions, y = waiting)

mod1 <- lm(price ~ carat, data = diamonds)
mod2 <- lm(log(price) ~ log(carat), data = diamonds)


# Tibbles -----------------------------------------------------------------
df <-
tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a", 2, 3.6,
  "b", 1, 8.5
)

df %>% .[["x"]]
