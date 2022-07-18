# Imported information
library(readr)
nutrition <- read_csv("~/Masters of Applied Data Science/DATA401/Assignment/nutrition.csv")
View(nutrition)
str(nutrition)
attach(nutrition)

# Install packages 
install.packages("magrittr") 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("agricolae")
library(agricolae)
library(magrittr) 
library(dplyr)
library(ggplot2)
attach(nutrition)

# Summary
summary(nutrition)
head(nutrition, n=5)
str(nutrition)

# Expected values for gender and age 
median(nutrition$BetaPlasma, na.rm=T)
median(nutrition$RetinolPlasma, na.rm=T)
mean(nutrition$BetaPlasma, na.rm=T)
mean(nutrition$RetinolPlasma, na.rm=T)

nutrition_gender <- split(nutrition, nutrition$Gender)
female_beta <- nutrition_gender$Female$BetaPlasma
male_beta <- nutrition_gender$Male$BetaPlasma
female_retinol <- nutrition_gender$Female$RetinolPlasma
male_retinol <- nutrition_gender$Male$RetinolPlasma

median(female_beta)
median(male_beta)
median(female_retinol)
median(male_retinol)

mean(subset(female_beta, Age > 19 & Age < 40), na.rm=T)
mean(subset(female_beta, Age > 39 & Age < 60), na.rm=T)
mean(subset(female_beta, Age > 59), na.rm=T)
mean(subset(male_beta, Age > 19 & Age < 40), na.rm=T)
mean(subset(male_beta, Age > 39 & Age < 60), na.rm=T)
mean(subset(male_beta, Age > 59), na.rm=T)

mean(subset(female_retinol, Age > 19 & Age < 40), na.rm=T)
mean(subset(female_retinol, Age > 39 & Age < 60), na.rm=T)
mean(subset(female_retinol, Age > 59), na.rm=T)
mean(subset(male_retinol, Age > 19 & Age < 40), na.rm=T)
mean(subset(male_retinol, Age > 39 & Age < 60), na.rm=T)
mean(subset(male_retinol, Age > 59), na.rm=T)

# RetinalPlasma and BetaPlasma density functions 
hist_beta <- hist(nutrition$BetaPlasma, breaks=100, main="Histogram and normal distribution of beta-plasma", xlab="Beta-plasma (?g/dL)")
normal.freq(hist_beta, frequency=1, lwd=2,col="red")
hist_retinol <- hist(nutrition$RetinolPlasma, breaks=100, main="Histogram and normal distribution of retinal plasma", xlab="Retinol plasma (?g/dL)")
normal.freq(hist_retinol, frequency=1, lwd=2,col="red")


# Correlation female 
cor(nutrition_gender$Female$Quetelet, female_retinol, method='pearson')
cor(nutrition_gender$Female$Vitamin, female_retinol, method='pearson')
cor(nutrition_gender$Female$Calories, female_retinol, method='pearson')
cor(nutrition_gender$Female$Fat, female_retinol, method='pearson')
cor(nutrition_gender$Female$Fiber, female_retinol, method='pearson')
cor(nutrition_gender$Female$Cholesterol, female_retinol, method='pearson')
cor(nutrition_gender$Female$BetaDiet, female_retinol, method='pearson')
cor(nutrition_gender$Female$RetinolDiet, female_retinol, method='pearson')
cor(nutrition_gender$Female$Age, female_retinol, method='pearson')
cor(nutrition_gender$Female$Alcohol, female_retinol, method='pearson')

cor(nutrition_gender$Male$Quetelet, male_retinol, method='pearson')
cor(nutrition_gender$Male$Vitamin, male_retinol, method='pearson')
cor(nutrition_gender$Male$Calories, male_retinol, method='pearson')
cor(nutrition_gender$Male$Fat, male_retinol, method='pearson')
cor(nutrition_gender$Male$Fiber, male_retinol, method='pearson')
cor(nutrition_gender$Male$Cholesterol, male_retinol, method='pearson')
cor(nutrition_gender$Male$BetaDiet, male_retinol, method='pearson')
cor(nutrition_gender$Male$RetinolDiet, male_retinol, method='pearson')
cor(nutrition_gender$Male$Age, male_retinol, method='pearson')
cor(nutrition_gender$Male$Alcohol, male_retinol, method='pearson')

cor(nutrition_gender$Female$Quetelet, female_beta, method='pearson')
cor(nutrition_gender$Female$Vitamin, female_beta, method='pearson')
cor(nutrition_gender$Female$Calories, female_beta, method='pearson')
cor(nutrition_gender$Female$Fat, female_beta, method='pearson')
cor(nutrition_gender$Female$Fiber, female_beta, method='pearson')
cor(nutrition_gender$Female$Cholesterol, female_beta, method='pearson')
cor(nutrition_gender$Female$BetaDiet, female_beta, method='pearson')
cor(nutrition_gender$Female$RetinolDiet, female_beta, method='pearson')
cor(nutrition_gender$Female$Age, female_beta, method='pearson')
cor(nutrition_gender$Female$Alcohol, female_beta, method='pearson')

cor(nutrition_gender$Male$Quetelet, male_beta, method='pearson')
cor(nutrition_gender$Male$Vitamin, male_beta, method='pearson')
cor(nutrition_gender$Male$Calories, male_beta, method='pearson')
cor(nutrition_gender$Male$Fat, male_beta, method='pearson')
cor(nutrition_gender$Male$Fiber, male_beta, method='pearson')
cor(nutrition_gender$Male$Cholesterol, male_beta, method='pearson')
cor(nutrition_gender$Male$BetaDiet, male_beta, method='pearson')
cor(nutrition_gender$Male$RetinolDiet, male_beta, method='pearson')
cor(nutrition_gender$Male$Age, male_beta, method='pearson')
cor(nutrition_gender$Male$Alcohol, male_beta, method='pearson')

# Plots of all to see if they have non-linear patterns 
ggplot(nutrition, aes(Age, BetaPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Calories, BetaPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Fat, BetaPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Alcohol, BetaPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Cholesterol, BetaPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(RetinolDiet, BetaPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Quetelet, BetaPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Vitamin, BetaPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(BetaDiet, BetaPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)

ggplot(nutrition, aes(Quetelet, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Vitamin, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Calories, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Fat, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Fiber, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Cholesterol, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(BetaDiet, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(RetinolDiet, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Age, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)
ggplot(nutrition, aes(Alcohol, RetinolPlasma)) +  geom_point() + geom_smooth(method = "loess", se = FALSE)

# Plots Retinol_Plasma stronger correlation 

plot(nutrition_gender$Female$Age, female_retinol, main="Female age vs Retinol", xlab="Age", ylab="Retinol (?g/dL)")
abline(lm(female_retinol ~ nutrition_gender$Female$Age))

plot(nutrition_gender$Male$Quetelet , male_retinol, main="Male BMI vs Retinol", xlab="BMI", ylab="Retinol (?g/dL)")
abline(lm(male_retinol ~ nutrition_gender$Male$Quetelet))
plot(nutrition_gender$Male$Alcohol , male_retinol, main="Male alcohol vs Retinol", xlab="Alcohol", ylab="Retinol (?g/dL)")
abline(lm(male_retinol ~ nutrition_gender$Male$Alcohol))
plot(nutrition_gender$Male$Cholesterol , male_retinol, main="Male cholesterol vs Retinol", xlab="Cholesterol", ylab="Retinol (?g/dL)")
abline(lm(male_retinol ~ nutrition_gender$Male$Cholesterol))

ggplot(nutrition_gender$Female, aes(Fiber, female_beta)) +  geom_point() + geom_smooth(method = "loess", se = FALSE) + theme_classic() +  ggtitle("    Female fiber vs Beta-carotene") + xlab("Fiber") + ylab("Beta-carotene (?g/dL)")

# Plots BetaPlasma stronger correlation
plot(nutrition_gender$Female$Fiber, female_beta, main="Female fiber vs Beta-carotene", xlab="Fiber", ylab="Beta-carotene (?g/dL)")
abline(lm(female_beta ~ nutrition_gender$Female$Fiber))
plot(nutrition_gender$Female$BetaDiet, female_beta, main="Female beta-diet vs Beta-carotene", xlab="Beta-diet", ylab="Beta-carotene (?g/dL)")
abline(lm(female_beta ~ nutrition_gender$Female$BetaDiet))
plot(nutrition_gender$Female$Quetelet, female_beta, main="Female BMI vs Beta-carotene", xlab="BMI", ylab="Beta-carotene (?g/dL)")
abline(lm(female_beta ~ nutrition_gender$Female$Quetelet))
plot(nutrition_gender$Female$Vitamin, female_beta, main="Female vitamin vs Beta-carotene", xlab="Vitamin", ylab="Beta-carotene (?g/dL)")
abline(lm(female_beta ~ nutrition_gender$Female$Vitamin))

plot(nutrition_gender$Male$Quetelet, male_beta, main="Male BMI vs Beta-carotene", xlab="BMI", ylab="Beta-carotene (?g/dL)")
abline(lm(male_beta ~ nutrition_gender$Male$Quetelet))
plot(nutrition_gender$Male$Vitamin, male_beta, main="Male vitamin vs Beta-carotene", xlab="Vitamin", ylab="Beta-carotene (?g/dL)")
abline(lm(male_beta ~ nutrition_gender$Male$Vitamin))
plot(nutrition_gender$Male$Cholesterol, male_beta, main="Male cholesterol vs Beta-carotene", xlab="Cholesterol", ylab="Beta-carotene (?g/dL)")
abline(lm(male_beta ~ nutrition_gender$Male$Cholesterol))

# Smoking not controlled 
nutrition_split_smoke <- split(nutrition, nutrition$Smoke)
smokers <- nutrition_split_smoke$Yes
nonsmokers <- nutrition_split_smoke$No
summary(smokers)
summary(nonsmokers)
median(smokers$BetaPlasma)
median(nonsmokers$BetaPlasma)
median(smokers$RetinolPlasma)
median(nonsmokers$RetinolPlasma)

# Smoking controlled for gender
smokers_gender <- split(smokers, smokers$Gender)
nonsmokers_gender <- split(nonsmokers, nonsmokers$Gender)

smokers_female <- smokers_gender$Female
nonsmokers_female <- nonsmokers_gender$Female
median(smokers_female$BetaPlasma)
median(nonsmokers_female$BetaPlasma)
median(smokers_female$RetinolPlasma)
median(nonsmokers_female$RetinolPlasma)

smokers_male <- smokers_gender$Male
nonsmokers_male <- nonsmokers_gender$Male
median(smokers_male$BetaPlasma)
median(nonsmokers_male$BetaPlasma)
median(smokers_male$RetinolPlasma)
median(nonsmokers_male$RetinolPlasma)   

# Smoking controlled for gender and age
smokers_fem_age1 <- subset(smokers_female, Age > 19 & Age < 40)
smokers_fem_age2 <- subset(smokers_female, Age > 39 & Age < 60)
smokers_fem_age3 <- subset(smokers_female, Age > 59)
nonsmokers_fem_age1 <- subset(nonsmokers_female, Age > 19 & Age < 40)
nonsmokers_fem_age2 <- subset(nonsmokers_female, Age > 39 & Age < 60)
nonsmokers_fem_age3 <- subset(nonsmokers_female, Age > 59)

median(smokers_fem_age1$BetaPlasma)
median(smokers_fem_age2$BetaPlasma)
median(smokers_fem_age3$BetaPlasma)
median(nonsmokers_fem_age1$BetaPlasma)
median(nonsmokers_fem_age2$BetaPlasma)
median(nonsmokers_fem_age3$BetaPlasma)

median(smokers_fem_age1$RetinolPlasma)
median(smokers_fem_age2$RetinolPlasma)
median(smokers_fem_age3$RetinolPlasma)
median(nonsmokers_fem_age1$RetinolPlasma)
median(nonsmokers_fem_age2$RetinolPlasma)
median(nonsmokers_fem_age3$RetinolPlasma)



