# Homework 11
library(dplyr)
library(nycflights13)

# 12.5.4
# Question 1
x <- 0:20
even <- if_else(x %% 2 == 0, "even", "odd")
names(even) <- x
even

# Question 2
weekends <- c("Saturday", "Sunday")
x <- c("Monday", "Saturday", "Wednesday")
types <- if_else(x %in% weekends, "Weekend", "Weekday")
names(types) <- x
types

# Question 3
x <- c(-1, 3, 0, 4, -7, 9, -3456)
abs <- if_else(x < 0, x*-1, x)
abs

# Question 4
holiday_dates <- c("1-1", "7-4", "11-27", "12-25")
flights |> mutate(
  date = paste(month, day, sep="-"),
  is_holiday = case_when(
    date %in% holiday_dates ~ TRUE,
    .default = FALSE
  ),
  holiday = case_when(
    date == holiday_dates[1] ~ "New Year's Day",
    date == holiday_dates[2] ~ "4th of July",
    date == holiday_dates[3] ~ "Thanksgiving",
    date == holiday_dates[4] ~ "Christmas",
    .default = NA
  )
)

