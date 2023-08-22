library(tidyverse)
library(leaps)

# Loading in dataset (note that Sleep_efficiency.csv must be in same folder as this file)
sleepData <- read.csv(file = "Sleep_Efficiency.csv", header = TRUE)
head(sleepData)

# Selecting the columns we will be using in our analysis
sleepData <- sleepData |>
  select(Sleep.efficiency,
         Alcohol.consumption, 
         Exercise.frequency, 
         Caffeine.consumption, 
         Smoking.status, 
         Gender)

# Converting columns into factors
sleepData$Smoking.status <- as.factor(sleepData$Smoking.status)
sleepData$Gender <- as.factor(sleepData$Gender)

# Removing all rows with ANY NA in any of the columns we will be using for predictions
sleepData <- sleepData |>
  filter(!is.na(Sleep.efficiency) &
         !is.na(Alcohol.consumption) &
         !is.na(Exercise.frequency) &
         !is.na(Caffeine.consumption) & 
         !is.na(Smoking.status) &
         !is.na(Gender))
head(sleepData)

# Plots of sleep efficiency against predictors
plot(y = sleepData$Sleep.efficiency, 
     x = as.factor(sleepData$Alcohol.consumption), 
     main = "Sleep Efficiency vs Alcohol Consumption", 
     xlab = "Alcohol Consumption", 
     ylab = "Sleep Efficiency")
plot(y = sleepData$Sleep.efficiency, 
     x = (sleepData$Alcohol.consumption), 
     main = "Sleep Efficiency vs Alcohol Consumption", 
     xlab = "Alcohol Consumption", 
     ylab = "Sleep Efficiency")
plot(y = sleepData$Sleep.efficiency, 
     x = as.factor(sleepData$Exercise.frequency),
     main = "Sleep Efficiency vs Exercise Frequency", 
     xlab = "Exercise Frequency", 
     ylab = "Sleep Efficiency")
plot(y = sleepData$Sleep.efficiency, 
     x = (sleepData$Exercise.frequency),
     main = "Sleep Efficiency vs Exercise Frequency", 
     xlab = "Exercise Frequency", 
     ylab = "Sleep Efficiency")
plot(y = sleepData$Sleep.efficiency, 
     x = as.factor(sleepData$Caffeine.consumption),
     main = "Sleep Efficiency vs Caffeine Consumption", 
     xlab = "Caffeine Consumption", 
     ylab = "Sleep Efficiency")
plot(y = sleepData$Sleep.efficiency, 
     x = (sleepData$Caffeine.consumption),
     main = "Sleep Efficiency vs Caffeine Consumption", 
     xlab = "Caffeine Consumption", 
     ylab = "Sleep Efficiency")
plot(y = sleepData$Sleep.efficiency, 
     x = sleepData$Smoking.status,
     main = "Sleep Efficiency vs Smoking Status", 
     xlab = "Smoking Status", 
     ylab = "Sleep Efficiency")
plot(y = sleepData$Sleep.efficiency, 
     x = sleepData$Gender,
     main = "Sleep Efficiency vs Sex", 
     xlab = "Sex", 
     ylab = "Sleep Efficiency")


# MODEL ANALYSIS

# Best subset selection algorithm using exhaustive method
s <- regsubsets(Sleep.efficiency ~
                  Alcohol.consumption + 
                  Exercise.frequency + 
                  Caffeine.consumption + 
                  Smoking.status + 
                  Gender, 
                data = sleepData, method = "exhaustive")
ss <- summary(s)
ss$which
ss$adjr2
ss$cp
ss$rsq

#Best subset selection algorithm using forward selection method
s2 <- regsubsets(Sleep.efficiency ~
                  Alcohol.consumption + 
                  Exercise.frequency + 
                  Caffeine.consumption + 
                  Smoking.status + 
                  Gender, 
                data = sleepData, method = "forward")
ss2 <- summary(s2)
ss2$which
ss2$adjr2
ss2$cp
ss2$rsq


#Fit linear regression no interactions
reg <- lm(formula = Sleep.efficiency ~ 
            Alcohol.consumption + 
            Exercise.frequency + 
            Caffeine.consumption + 
            Smoking.status + 
            Gender, 
          data = sleepData)
summary(reg)

# Linear regression with interactions
regInteractions <- lm(formula = Sleep.efficiency ~ 
                        Alcohol.consumption + 
                        Exercise.frequency + 
                        Caffeine.consumption + 
                        Smoking.status + 
                        Gender +
                        Alcohol.consumption:Smoking.status +
                        Exercise.frequency:Smoking.status +
                        Gender:Smoking.status +
                        Caffeine.consumption:Smoking.status, 
                      data = sleepData)
summary(regInteractions)

# Interaction of alcohol consumption, caffeine, exercise, and gender with smoker status
regSomeInteractions <- lm(formula = Sleep.efficiency ~ 
                            Alcohol.consumption *
                            Exercise.frequency * 
                            Caffeine.consumption *
                            Smoking.status *
                            Gender,
                          data = sleepData,)
summary(regSomeInteractions)

# Model with only significant terms and interactions with smoking
modelSignificant <- lm(formula = Sleep.efficiency ~ 
                         Alcohol.consumption + 
                         Exercise.frequency + 
                         Smoking.status +
                         Alcohol.consumption:Smoking.status + 
                         Exercise.frequency:Smoking.status, 
                       data = sleepData)
summary(modelSignificant)

# Final model
finalModel <- modelSignificant
summary(finalModel)

# Residual plot
plot(x = finalModel$fitted.values, 
     y = finalModel$residuals,
     main = "Residual plot",
     xlab = "Fitted Values",
     ylab = "Residuals") +
  abline(h = 0) 

# QQ-plot
qqnorm(finalModel$residuals) +
  qqline(finalModel$residuals)

