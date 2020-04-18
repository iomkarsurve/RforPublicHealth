#import cancer data

getwd()
g<- read.csv(file="cancer data for MOOC 1.csv",header = TRUE,sep=",")

head(g) 

g[4,]

g[,5]

g[1:5,]

dim(g)


g[,'gender']
gender <- g[,'gender']

gender <- as.factor(g[,'gender'])

table(gender)

bmi<-g[,'bmi']

summary(bmi)


fruit <- g[,'fruit']
veg<- g[,'veg']
fruitveg<-fruit+veg
table(fruitveg)

hist(fruitveg)

g$gender
g$veg
g$fruit
fruitveg<-g$veg+g$fruit
fruitveg
table(fruitveg)


View(g)


summary(g$age)
hist(g$age)

table(g$smoking,exclude = NULL)


hist(g$smoking)

hist(g$cancer)

agebmi<-g$bmi+g$age
summary(agebmi)
hist(agebmi)
dist(agebmi)


bmimale<-g$gender




#who eats at least 5 portions
five_a_day <- ifelse(fruitveg >= 5, 1, 0)

table(five_a_day)

summary(five_a_day)

hist(five_a_day)
hist(five_a_day,xlab = "Portions of fruit and veg",
     main="Daily consumption of fruit and veg combined")

hist(five_a_day,xlab = "Portions of fruit and veg",
     main="Daily consumption of fruit and veg combined",axes = F)
axis(side=1,at=seq(0,11,1))
axis(side=2,at=seq(0,16,2))

#ggplot2
library(ggplot2)

ggplot() + geom_histogram(data = g, aes(x = fruitveg),bins = 10, fill = "darkgreen",col = "black")+
labs(x = "Portions of fruit and vegetables", y = "Frequency")+
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 0.5))+ theme_bw()


hist(g$fruit, xlab = "Portions of fruit",
     
     main = "Daily consumption of fruit", axes = F)

axis(side = 1, at = seq(0, 4, 1))

axis(side = 2, at = seq(0, 24, 4))


hist(g$veg, xlab = "Portions of veg",
     
     main = "Daily consumption of veg", axes = F)

axis(side = 1, at = seq(0, 9, 1))

axis(side = 2, at = seq(0, 18, 2))

#healthy bmi

healthy_bmi<- ifelse(g$bmi > 18.5 & g$bmi < 25,1,0)
table(healthy_bmi)

hist(healthy_bmi)


chisq.test(x=five_a_day,y=g$cancer) 

t.test(g$bmi~g$cancer,var.equal=T)

t.test(g$bmi~g$cancer)

overw<- ifelse(g$bmi>25,1,0)

t.test(overw~g$cancer)

t.test(g$bmi,mu=25)


cancer <- g$cancer

overweight <- ifelse(g$bmi >= 25, 1, 0)

table(overweight)

chisq.test(x=overweight,y=cancer)

