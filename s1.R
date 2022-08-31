library(readr)
s1 <- read_csv("C:/Users/Onur/OneDrive/Masaüstü/s1 (1).csv")
View(s1)

summary(s1)

p99 <- quantile(s1$age, probs=c(0.98))
p1 <- quantile(s1$age, probs=c(0.005))

s1$age <- pmin(s1$age, p99)
s1$age <- pmax(s1$age, 18)

summary(s1$age)

s1$yrs_school <- pmax(s1$yrs_school, 0.00001)
s1$income <- pmax(s1$income, 0.00001)


#model

r1 <- lm(income~yrs_school+age+gender_f, data=s1)
summary(r1)

#g�rsel

res <- residuals(r1)
s1$res2 <- res^2

plot(res2~yrs_school, data=s1)
plot(res2~age, data=s1)
plot(res2~income, data=s1)

#test
library(lmtest)
bptest(r1, ~age+I(age^2)+yrs_school+I(yrs_school^2)+
         gender_f+I(age*yrs_school), data=s1)

gqtest(r1, order.by=~yrs_school, data=s1, fraction=10)

#log al
l_income <- log(s1$income)
l_age <- log(s1$age)
l_ysc <- log(s1$yrs_school)

r2 <- lm(l_income~l_age+l_ysc+s1$gender_f)
summary(r2)

bptest(r2, ~l_age+I(l_age^2)+l_ysc+I(l_ysc^2)+
         s1$gender_f+I(l_age*l_ysc), data=s1)

#robust

library(sandwich)
coeftest(r2, vcov=vcovHC(r2, "HC1"))


