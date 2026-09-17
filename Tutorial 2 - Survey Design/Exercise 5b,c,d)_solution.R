install.packages("dplyr")
install.packages("tidyverse")


library(dplyr)
library(tidyverse)

#Let's set our working directory (ADJUST FOR YOUR OWN DIRECTORY)
setwd("C:/XXXXXXX/")

#Now let's read in the file
survey <- read.csv("survey_data.csv")

#let's create different data frames for each construct
construct_1 <- survey %>% select(1:4)
construct_2 <- survey %>% select(5:8)
construct_3 <- survey %>% select(9:12)
construct_4 <- survey %>% select(13:15)
construct_5 <- survey %>% select(16:18)
construct_6 <- survey %>% select(19:23)
construct_7 <- survey %>% select(24:26)
construct_8 <- survey %>% select(27:31)

#Part b)
#Now let's calculate Cronbach's alpha with just one line of code
install.packages("ltm")
library(ltm)

?cronbach.alpha   
cronbach.alpha(construct_1)
cronbach.alpha(construct_2)
cronbach.alpha(construct_3)
cronbach.alpha(construct_4)
cronbach.alpha(construct_5)
cronbach.alpha(construct_6)
cronbach.alpha(construct_7)
cronbach.alpha(construct_8)

#FYI: there are more parameters to use
cronbach.alpha(construct_1, CI=TRUE, probs=c(0.025, 0.975), B=313, na.rm=TRUE)    

#Part c)
#Let's build the average in a separate dataframe
averages <- as.data.frame(rowMeans(construct_1)) %>% rename("construct_1" = "rowMeans(construct_1)")
averages$construct_2 <- rowMeans(construct_2)
averages$construct_3 <- rowMeans(construct_3)
averages$construct_4 <- rowMeans(construct_4)
averages$construct_5 <- rowMeans(construct_5)
averages$construct_6 <- rowMeans(construct_6)
averages$construct_7 <- rowMeans(construct_7)
averages$construct_8 <- rowMeans(construct_8)

#Part d)
#Now for some plotting 
install.packages("corrplot")
install.packages("ggpubr")

library(ggpubr)
library(corrplot)
cor(averages)

cor_matrix <- cor(averages)
cor_matrix

corrplot(cor_matrix, method="circle")
corrplot(cor_matrix, method="color")
corrplot(cor_matrix, method="number")

#Let's enrich the "circle" plot with confidence intervals and cross out correlations that are below our p-value (sig-argument, very small in this case)
averages_sig <- cor.mtest(as.matrix(averages))
averages_sig <- averages_sig$p

corrplot(cor_matrix, method="pie", p.mat = averages_sig, sig=0.000000000001)



