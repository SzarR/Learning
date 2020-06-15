## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Robert Szarek
##
## Date Modified: 15-JUN-2020
##
## Email: szarekr@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


# Load Global Package Requirements ----------------------------------------

library(tidyverse)


# 9 Functionals -----------------------------------------------------------


# * 9.3 Purrr Style -------------------------------------------------------

by_cyl <- split(mtcars, mtcars$cyl) # returns list broken down by cat var.

by_cyl %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map(coef) %>%
  map_dbl(2)

# Base R substitute
by_cyl %>% 
  lapply(function(data) lm(mpg ~ wt, data = data))


# * 9.4 Map Variants ------------------------------------------------------

df <- data.frame(
  x = 1:3,
  y = 6:4
)

map(df, ~ .x * 2) # Returns a list.

modify(df, ~ .x * 2) # Returns the df back.


# ** 9.4.2 Two inputs map2() ----------------------------------------------

xs <- map(1:8, ~ runif(10))

xs[[1]][[1]] <- NA

ws <- map(1:8, ~ rpois(10,5) + 1)

map_dbl(xs, mean)

map_dbl(xs, mean, w = ws)

map_dbl(xs, weighted.mean, w = ws) # error

map2_dbl(.x = xs,.y = ws,.f = weighted.mean)


# ** 9.4.3 walk(( ---------------------------------------------------------

welcome <- function(x) {
  cat("Welcome ", x, "!\n", sep = "")
}

names <- c("Hadley", "Jenny")

map(names, welcome)
walk(names, welcome)

temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

dir(temp)


# ** 9.4.4 Iterating over values and indices ------------------------------

#There are three basic ways to loop over a vector with a for loop:
#Loop over the elements: for (x in xs)
#Loop over the numeric indices: for (i in seq_along(xs))
#Loop over the names: for (nm in names(xs))

# imap is often useful for constructing labels
imap_chr(iris, ~ paste0("THe first value of ", .y, " is ", .x[[1]]))

pmap_dbl(list(xs, ws), weighted.mean)

params <- tibble::tribble(
  ~ n, ~ min, ~ max,
  1L, 0, 1,
  2L, 10, 100,
  3L, 100, 1000
)


trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) factor(x, labels = c("auto", "manual"))
)

nm <- names(trans)
mtcars[nm] <- map2(trans, mtcars[nm], function(f, var) f(var))


# ** 9.5.1 Basics of reduce() ---------------------------------------------

reduce(1:4, f)

l <- map(1:4, ~ sample(1:10, 15, replace=T))
str(l)

out <- l[[1]]
out <- intersect(out, l[[2]])
out <- intersect(out, l[[3]])
out <- intersect(out, l[[4]])


# ** 9.6.1 Basics of predicates -------------------------------------------

r <- c(1,2,3,4)
x <- c(4,5,6)

every(.x = r,.p = x) #TRUE
some(.x = r,.p = x)

keep(.x = r,.p = x)

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
detect(df, is.factor)
detect_index(df, is.factor)


