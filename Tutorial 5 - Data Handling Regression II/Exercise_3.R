library("dplyr")
library("ggplot2")
library("viridis")

## load data

data = read.csv("splines_tut.csv", stringsAsFactors=F) %>% select(-1)


## plot data
ggplot(data, aes(x=age, y=income))+geom_point()

## baseline model
reg = lm(income ~ age, data=data) #create regression model for age and income
summary(reg) # plot model summary
data$predicted_income = predict(reg, data) # use the model to predict the data 
ggplot(data, aes(x=age, y=income))+ #plot both the actual and predicted data
  geom_point()+
  geom_line(aes(x=age, y=predicted_income),size=1, color="red")

## improve model with quadratic effect --> this one fits actually already almost perfectly
data <- data %>% mutate(age2 = age*age) #square the values to create the quadratic model
reg = lm(income ~ age+age2, data=data)
summary(reg)
data$predicted_income = predict(reg, data) # again, plot the data
ggplot(data, aes(x=age, y=income))+
  geom_point()+
  geom_line(aes(x=age, y=predicted_income),size=1, color="red")


## now, assume that the inflexion point is known
tau = 65

## compute new spline variable
data = data %>% mutate(spline_tau = ifelse(age>=tau, age-tau, 0)) 

## fit spline model
reg = lm(income ~ age + spline_tau, data=data)
summary(reg)
data$predicted_income = predict(reg, data)
ggplot(data, aes(x=age, y=income))+
  geom_point()+
  geom_line(aes(x=age, y=predicted_income),size=1, color="red")

### introduce offset at tau
data = data %>% mutate(offset_tau = ifelse(age>=tau, 1, 0))

## fit model with spline and offset
reg = lm(income ~ age + spline_tau + offset_tau, data=data)
summary(reg)
data$predicted_income = predict(reg, data)
ggplot(data, aes(x=age, y=income))+
  geom_point()+
  geom_line(data=data%>%filter(age<tau), aes(x=age, y=predicted_income),size=1, color="red")+
  geom_line(data=data%>%filter(age>=tau), aes(x=age, y=predicted_income),size=1, color="red")
  

## just for fun, only offset
reg = lm(income ~ age + offset_tau, data=data) ### only offset
summary(reg)
data$predicted_income = predict(reg, data)
ggplot(data, aes(x=age, y=income))+
  geom_point()+
  geom_line(data=data%>%filter(age<tau), aes(x=age, y=predicted_income),size=1, color="red")+
  geom_line(data=data%>%filter(age>=tau), aes(x=age, y=predicted_income),size=1, color="red")


## now, find inflexion point from the data endogenously

rm(outputDF)

for(tau_spline in seq(18,85,1)) {
  
  data = data %>% mutate(spline = ifelse(age>=tau_spline,age-tau_spline,0))
  reg = lm(income ~ age + spline, data=data)
  output = summary(reg)
  #print(output$r.squared)
  
  if(exists("outputDF")) outputDF = rbind(outputDF, data.frame(ts=tau_spline, r2=output$r.squared))
  else outputDF = data.frame(ts=tau_spline, r2=output$r.squared)
  
}

ggplot(outputDF, aes(x=ts, y=r2))+
  geom_line()+
  geom_point()+
  coord_cartesian(ylim=c(0.5, 1))+
  theme_bw()


## okay, inflexion point really seems to be at x=60 ;)

