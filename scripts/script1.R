
# load packages -----------------------------------------------------------

library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm03/master/data/weight.csv")
weight_df_male <- filter(weight_df, gender == 'male')

M_1 <- lm(weight ~ height, data = weight_df_male)
M_1a <- lm(weight ~ height + age + race, data = weight_df_male)

# get the estimates of the coefficients of M_1
coef(M_1)
coefficients(M_1)
sigma(M_1)

M_2 <- lm(weight ~ height + age, data = weight_df_male)
coef(M_2)
sigma(M_2)
predict(M_2)[1:10]


M_3 <- lm(weight ~ height + age + gender, data = weight_df)

M_4 <- lm(weight ~ height + race, data = weight_df)


# log odds ----------------------------------------------------------------

theta <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# odds
odds <- theta/(1 - theta)

# log odds
log(odds)

# Binary logistic regression ----------------------------------------------

affairs_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm03/master/data/affairs.csv")

affairs_df <- mutate(affairs_df, cheater = affairs > 0)

M_5 <- glm(cheater ~ yearsmarried, 
           data = affairs_df,
           family = binomial(link = 'logit'))

summary(M_5)

# log odds of having an affair is a linear function of yearsmarried
# the coefficients of the linear function are 
estimates <- coef(M_5)

# what if the yearsmarried = 10? What is the probability of having an affair?
# what is the log odds that they will have an affair?
# a + b x
# a + m x
# beta_0 + beta_1 x 
log_odds <- estimates[1] + estimates[2] * 10

ilogit <- function(x) {1/(1 + exp(-x))}
# plogis will give you ilogit

# this is the probability of having an affair if yearsmarried = 10.
ilogit(log_odds)

# What is yearsmarried is 1, 5, 10, 15, 20, 25 ... ?
log_odds <- estimates[1] + estimates[2] * c(1, 5, 10, 15, 20, 25)

ilogit(log_odds)
plogis(log_odds)

affairs_df_1 <- tibble(yearsmarried = c(1, 5, 10, 15, 20, 25))

# predicted log odds using `predict`
predict(M_5, newdata = affairs_df_1)

# predicted probabilities using `predict`
predict(M_5, newdata = affairs_df_1, type = 'response')

library(modelr)

add_predictions(affairs_df_1, M_5, var = 'probability_of_having_an_affair')

add_predictions(affairs_df_1, M_5, type = 'response')

affairs_df_2 <- tibble(yearsmarried = seq(1, 50, by = 0.1))
predictions_df <- add_predictions(affairs_df_2, M_5, type = 'response')

ggplot(predictions_df, aes(x = yearsmarried, y = pred)) + geom_line()


# coefficients ------------------------------------------------------------

summary(M_5)$coefficients

# e^0.0588 ~= 1.06 is the odds ratio corresponding to yearsmarried

confint.default(M_5)
confint.default(M_5, parm = 'yearsmarried') %>% exp()


# log likelihood and deviance ---------------------------------------------

logLik(M_5)
summary(M_5)

M_6 <- glm(cheater ~ 1, 
           data = affairs_df,
           family = binomial(link = 'logit'))
logLik(M_6) * -2 

anova(M_6, M_5, test = 'Chisq')

# ordinal logistic regresion ----------------------------------------------
library(MASS)
library(pscl) # to load `admit`

head(admit)

M_7 <- polr(score ~ gre.quant, data = admit)
summary(M_7)

admit_df_2 <- tibble(gre.quant = seq(300, 800, by = 100))

add_predictions(admit_df_2, M_7, type = 'prob')

mu <- coef(M_7) * 600

# The predicted probability that the outcome is 1
plogis(M_7$zeta['1|2'], location = mu)
# The predicted probability that the outcome is 1 or 2
plogis(M_7$zeta['2|3'], location = mu)
# The predicted probability that the outcome is 2 
plogis(M_7$zeta['2|3'], location = mu) - plogis(M_7$zeta['1|2'], location = mu)


# Categorical logistic regression ------------------------------------------

library(nnet)
library(pscl)

# see first few rows of data frame
head(admit)

M_8 <- multinom(score ~ gre.quant, data = admit)
summary(M_8)

library(tidyverse)
library(modelr)

admit_df_2 <- tibble(gre.quant = seq(300, 800, by = 100))

add_predictions(admit_df_2, M_8, type = 'prob')

# calculate linear function of gre.quant = 600, 
# using the 4 sets of coefficient
z <- coef(M_8) %*% c(1, 600)
z <- c(0, z)
exp(z)/sum(exp(z))
