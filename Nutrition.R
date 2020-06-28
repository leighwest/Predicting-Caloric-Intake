setwd("C:/Users/Leigh West/OneDrive/Documents/STAT210/Assessment 3")
# Import libraries
library(readr)
library(car)
# Import custom R functions
source("Rfunctions.R")

# Read data from data file (NutritionStudy.txt) and store in R in a data frame (nutrition)
nutrition <- read_tsv("NutritionStudy.txt")
# View dataframe
View(nutrition)

# Create a pairs plot of variables (reordered so predictor variable in bottom right corner, ID dropped)
pairs(nutrition[c(2,4:5,3)], lower.panel=panel.smooth, upper.panel=panel.cor, main="Pairs Plot of Variables in Nutrition Study", cex=0.9)
# Output pairwise correlations
cor.prob(nutrition)

# Model with main effects only
mod1 <- lm(Calories ~ Age + Fat + Fiber, data = nutrition)
# Print summary
print(summary(mod1))
# Print ANOVA
print(anova(mod1))
# Produce variance inflation for predictor variables in mod1
vif(mod1)

# Stepwise regression

# Minimal model
formL <- formula(~1)
# Maximum model
formU <- formula(Calories ~ Age*Fat*Fiber, data=nutrition)
start.model <- lm(Calories ~1, data=nutrition)
stepf.model <- step(start.model, direction = "forward", scope = list(lower = formL, upper = formU))
# Print summary of final stepwise model
summary(stepf.model)
# Print anova of final stepwise model
anova(stepf.model)
# Print confidence intervals of final stepwise model
betaCI(stepf.model)

