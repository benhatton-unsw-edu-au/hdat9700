library(mice)
library(VIM)

summary(diabetes)

a1 <- VIM::aggr(diabetes)

sum(is.na(diabetes$Pregnancies))

summary(diabetes)

sum(cci(diabetes))
sum(ici(diabetes))

nrow(diabetes)


sum(cci(diabetes))
sum(ici(diabetes))

diabetesCC <- diabetes %>% filter(cci(diabetes)==1)

histMiss(diabetes[, c('BMI', 'Insulin')])

imp1 <- mice(diabetes)

stripplot(imp1)
plot(imp1)

imp2 <- mice(diabetes, m=20)
plot(imp2)

nrow(imp1$data)

df0 <- complete(imp1, 0)
df1 <- complete(imp1, 1)
df5 <- complete(imp1, 5)


m0 <- glm(Outcome ~ ., family=binomial(link='logit'), data= diabetesCC)
m1 <- with(imp1, glm(Outcome ~ Pregnancies + Glucose + BloodPressure + 
                                SkinThickness + Insulin + BMI + 
                                DiabetesPedigreeFunction + Age, family=binomial(link='logit')))

summary(m0)
summary(m1)

m1Pooled <- mice::pool(m1)
