rm(list = ls())

library(tidyverse)
library(moments)

load("lab1_data.Rdata")

#change the code values within sex to labels "male" and "female"

data$sex[data$sex == 1] <- "male"
data$sex[data$sex == 2] <- "female"

#create male and female datasets
sum.func <- function(dat, x) {
  dat %>%
    filter(sex == x & age == 15) %>%
    select(!sex) %>%
    pivot_longer(1:ncol(.),
                 names_to = "vars",
                 values_to = "values") %>%
    group_by(vars) %>%
    summarise(
      mean = mean(values, na.rm = TRUE),
      SD = sd(values, na.rm = TRUE),
      SE = sd(values, na.rm = TRUE)/length(na.omit(values)),
      n = length(na.omit(values)),
      coeffskew = skewness(values, na.rm = TRUE)
    ) %>%
    mutate_if(is.numeric, round, 3)
}

sum15.func <- function(dat, x) {
  dat %>%
    filter(sex == x & age == 15) %>%
    select(!sex) %>%
    pivot_longer(1:ncol(.),
                 names_to = "vars",
                 values_to = "values") %>%
    group_by(vars) %>%
    summarise(
      mean = mean(values, na.rm = TRUE),
      SD = sd(values, na.rm = TRUE)
    ) %>%
    mutate_if(is.numeric, round, 3)
}

male.summary <- sum.func(data, "male")
female.summary <- sum.func(data, "female")

male15.summary <- sum15.func(data, "male")
female15.summary <- sum15.func(data, "female")

#create a function that splits the data by age and sex and calculates the 5, 25, 50, 75 and 95 quantiles for ht wt and vo2max

perc.func <- function(sx, dat) {
  x <- dat %>%
    select(age, sex, ht, wt, vo2max) %>%
    filter(sex == sx) %>%
    select(!sex) %>%
    pivot_longer(2:ncol(.),
                 names_to = "vars",
                 values_to = "val") %>%
    group_by(age, vars) %>%
    summarise(q5 = quantile(val, 
                            prob = 0.05, 
                            na.rm = TRUE),
              q25 = quantile(val, 
                             prob = 0.25, 
                             na.rm = TRUE),
              q50 = quantile(val, 
                             prob = 0.50, 
                             na.rm = TRUE),
              q75 = quantile(val, 
                             prob = 0.75, 
                             na.rm = TRUE),
              q95 = quantile(val, 
                             prob = 0.95, 
                             na.rm = TRUE)
    )
  
  return(x)
}

male_percentiles <- perc.func("male", data)
female_percentiles <- perc.func("female", data)

#split file by sex and age and calculate the z-scores for vo2max, grpmaxc, pushups, sitreach, situps

