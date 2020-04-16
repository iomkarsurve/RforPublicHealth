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



