## Analysis of Wine Quality
## Christine Lee
## December 17, 2017


## Read in Data
library(tidyverse)
wine <- read_csv("Wine 2017 Training Complete.csv")
attach(wine)

## Correlation Plots

## GGPlot
library(ggplot2)
pairs(wine[, 3:14])

## GGally
library(GGally)
ggpairs(wine[, 2:14])

## corrplot
library(corrplot)
wine_cor <- round(cor(wine[, 3:14]), 3)
corrplot.mixed(wine_cor, lower = "number", upper = "color", tl.cex = 0.5)


## From the correlation plots we can see that free.sulfur.dioxide and 
## total.sulfur.dioxide are strongly correlated. Density and sulfates 
## are also strongly correlated. It is also observed that Quality and 
## alcohol are strongly associated.



## Initial Model

## Full Model
model_full <- lm(Quality ~ Wine.Color + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = wine)


## Diagnostics

par(mfrow = c(2, 2))
plot(model_full)

summary(model_full)
aov(model_full)
anova(model_full)
library(car)
vif(model_full)

std_res_full <-rstandard(model_full)
plot(std_res_full)
abline(h=2, col = "red")


## From VIF, it can be observed that residual.sugar, density, wine.color, 
## and alcohol have values greater than 5. To fix the model, interaction terms will be used.


## Transformation Test

## Inverse Response Plot
inverseResponsePlot(model_full)
summary(powerTransform(cbind(alcohol,fixed.acidity,volatile.acidity,residual.sugar,free.sulfur.dioxide,total.sulfur.dioxide,pH,sulphates,density)~1,data=wine))

## Boxcox
library(MASS)
boxcox(lm(Quality ~ Wine.Color + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol))

## From the lambdas, the predictors can be transformed accordingly.


## Transformations

alcohol <- alcohol^(-0.5)
fixed.acidity <- fixed.acidity^(-0.5)
volatile.acidity <- log(volatile.acidity)
residual.sugar <- residual.sugar^(0.5)
free.sulfur.dioxide <- free.sulfur.dioxide^(0.5)
sulphates <- sulphates^(-0.5)
pH <- pH^(0.5)
density <- density^(-50)


## New Models

model_new <- lm(Quality ~ Wine.Color + Wine.Color:I(residual.sugar^(0.5)) + Wine.Color:alcohol + Wine.Color:I((fixed.acidity)^(-0.5)) + I(log(volatile.acidity)) + I((fixed.acidity)^(-0.5)) + I((free.sulfur.dioxide)^(0.5)) + total.sulfur.dioxide + I((pH)^(0.5)) + I((pH)^(0.5)):total.sulfur.dioxide + I((fixed.acidity)^(-0.5)):citric.acid + I((free.sulfur.dioxide)^(0.5)):total.sulfur.dioxide + I(density^(-50)):I(sulphates^(-0.5)) + I(density^(-50)):I((pH)^(0.5)))

summary(model_new)


## AIC & BIC

## Backward AIC
step(model_new, direction = "backward", data = wine)

## Backward BIC
n = length(wine$Case)
step(model_new, direction = "backward", data = wine, k = log(n))

model_new2 <- lm(Quality ~ Wine.Color + I(log(volatile.acidity)) + I((fixed.acidity)^(-0.5)) + 
                   I((free.sulfur.dioxide)^(0.5)) + total.sulfur.dioxide + I((pH)^(0.5)) + 
                   Wine.Color:I(residual.sugar^(0.5)) + Wine.Color:alcohol + 
                   Wine.Color:I((fixed.acidity)^(-0.5)) + total.sulfur.dioxide:I((pH)^(0.5)) + 
                   I((free.sulfur.dioxide)^(0.5)):total.sulfur.dioxide + I(density^(-50)):I(sulphates^(-0.5)) + 
                   I((pH)^(0.5)):I(density^(-50)))

summary(model_new2)
vif(model_new2)


## The AIC and BIC tests resulted in a model with a lower R-squared value, 
## so the previous model is used.


wine_testing <- read_csv("Wine 2017 Testing.csv")

quality_predict <- predict(model_new2, newdata = wine_testing[2:13])

write.csv(quality_predict, "Wine Prediction.csv")
```


