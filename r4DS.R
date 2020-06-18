
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


# Chapter 12 --------------------------------------------------------------

stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c(1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.066)
)

stocks %>%
  complete(year, qtr)


# Chapter 13 --------------------------------------------------------------

library(tidyverse)
library(nycflights13)

planes %>%
  count(tailnum) %>%
  filter(n > 1)

weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>%
  left_join(airlines, by = 'carrier')

# Old base R method.
flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

flights2 %>%
  left_join(weather)

flights2 %>%
  left_join(airports, c("origin" = "faa"))

# Lat/Lon plotting! Very neat.
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()


# 14 Strings --------------------------------------------------------------

str_c(c("x", "y", "z"), collapse = ", ")

x <- c("Apple", "Banana", "Pear")
str_sub(x,1,3)

str_sub(x, -3, -1)

# Regex Tutorial
x <- c("apple", "banana", "pear")
str_view(x, "an")
str(view(x, ".a."))

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) {" and HAPPY BIRTHDAY"}
)


str_c("prefix-", c("a", "b","c"), "-suffix")

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")

str_view(fruit, "(..)\\1", match = TRUE)


# Factors -----------------------------------------------------------------

x1 <- c("Dec", "Apr", "Jan", "Mar")


month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1


z <- c("Pass", "Did Not Pass")
status_levels <- c("Pass", "Did Not Pass")

r <- factor(z, levels = status_levels)

gss_cat %>%
  count(race)

library(magrittr)
gss_cat %>%
  count(race)


# Functions ---------------------------------------------------------------

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))

rescale01(c(-10,0,10))

george <- function(x) {
  
  if (x == 1) {
    next
  } else {
    print("Hoorah SOldier!")
  }
}


