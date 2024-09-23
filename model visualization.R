### MODEL VISUALIZATION

getwd()


##install.packages("tidyverse")
##install.packages("ISLR")
library(tidyverse)
library(ISLR)


glimpse(Wage)


set.seed(123)    ### for reproducability

d<-Wage %>% 
  sample_n(1000) %>% 
  rename(salary=wage)


glimpse(d)



### simple linear regression model  with categorical predictor

m<-lm(salary~jobclass,d)


library(effects)

plot(allEffects(m))



### simple linear model with numeric predictor


m<-lm(salary~age,d)

plot(allEffects(m),grid=T)



### multiple linear model with categorical predictor

m<-lm(salary~jobclass+education,d)

plot(allEffects(m))

plot(predictorEffect(predictor = "education",mod = m))
plot(predictorEffect(predictor = "jobclass",mod = m))





### multiple linear model with numeric predictor

m<-lm(salary~age+year,d)
plot(allEffects(m))

plot(allEffects(m),confint=list(style="bars"))


library(sjPlot)
plot_model(m)




### multiple linear model with categorical and numeric predictor


m<-lm(salary~age+education,d)


plot(allEffects(m))

plot(allEffects(m),rows=2,cols = 1)

plot_model(m, show.values = T)



library(performance)

check_model(m)



### multiple linear model with interaction


m<-lm(salary~education*jobclass,d)
plot(allEffects(m))

plot(allEffects(m),lines=list(multiline=T))

plot(
  allEffects(m),
  lines=list(multiline=T),
  confint=list(style="auto"))


library(emmeans)   #easy post-hoc

emmeans(m,pairwise~education|jobclass,adjust="fdr")$contrasts

emmeans(m,pairwise~jobclass|education,adjust="fdr")$contrasts



### multiple linear model with interaction of a numeric predictor

m<-lm(salary~age*health,d)
plot(allEffects(m))

plot(
  allEffects(m),
  lines=list(multiline=T),
  confint=list(style="auto"))

plot_model(m, type = "int")+
  theme_classic()+
  theme(legend.location = "top")+
  xlab("something different")



ggplot(d,aes(age,salary))+
  geom_point()+
  geom_smooth()+
  facet_grid(jobclass~health)
