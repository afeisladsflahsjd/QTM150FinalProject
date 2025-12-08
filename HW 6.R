library(tidyverse)
library(ggplot2)
lemursB <- read.csv("lemursB.csv")

# 1.2.5 Exercise 8
ggplot(
  data = lemursB,
  mapping = aes(x = age_at_wt_mo, y = weight_g, color = weight_g_last)
) +
  geom_point() +
  geom_smooth(se = FALSE)

# 1.4.3 Exercise 2
penguins <- penguins
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")
# color changes the border color of the bars
ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")
# fill changes the color of the bars. The fill aesthetic is much more useful 
# for changing bar color.


install.packages("nycflights13")
library(nycflights13)
library(dplyr)

# 3.2.5 Exercise 2
flights |> arrange(desc(dep_delay))
View(flights |> filter(dep_time == 1))
# This dataframe gives all 25 flights that departed at 12:01 AM, denoted
# by a dep_time of 1.

# 3.2.5 Exercise 3
flights |> arrange(distance/air_time)
# printing this tibble gives the top ten fastest flights

# 3.3.5 Exercise 7
flights |> 
  select(tailnum) |> 
  arrange(arr_delay)
# This line errors because the select command chooses only the tailnum column.
# Then, the arrange command tries to arrange based on arr_delay, a column that
# was not selected and thus is out of scope. The following code would work as
# intended, giving the tailnum column in ascending order of arr_delay.

flights |> arrange(arr_delay) |> select(tailnum)






