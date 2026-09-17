library(tidyverse)
library(data.table)

#set working directory - adjust to your own filepath, additionally you can set the working path to the file location in "Session" and "Set Working Directory"
setwd("XXXXX")

#read in the data
input <- fread("buli_input.csv", sep=";")

#let's conduct the tests we did in Excel, first we need to create some tables on win and loose-numbers
win_table <- table(input$win, input$already_meister)
windraw_table <- table(input$win_draw, input$already_meister)
lose_table <- table(input$lose, input$already_meister)

test_1 <- prop.test(win_table, correct = FALSE, alternative="greater")
sqrt(test_1$statistic)

test_2 <- prop.test(lose_table, correct=FALSE, alternative = "greater")
sqrt(test_2$statistic)

test_3 <- prop.test(windraw_table, correct=FALSE, alternative = "greater")
sqrt(test_3$statistic)

#now for the spread in task e)
test <- input %>% select(saison, Spieltag, win)
test <- spread(test, Spieltag, win)

?spread
