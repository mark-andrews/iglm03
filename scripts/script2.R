library(tidyverse)
library(modelr)

# Get some data -----------------------------------------------------------

doctor_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm03/master/data/DoctorAUS.csv")
table(doctor_df$doctorco)

M_9 <- glm(doctorco ~ sex, 
           data = doctor_df,
           family = poisson(link = 'log')
)

estimates <- coef(M_9)
log_rate_men <- estimates[1] + estimates[2] * 0 # log rate for males
log_rate_women <- estimates[1] + estimates[2] * 1 # log rate for females

rate_men <- exp(log_rate_men)
rate_women <- exp(log_rate_women)

tibble(sex = c(0, 1)) %>% 
  add_predictions(M_9, type ='response', var = 'rate')

summary(M_9)
confint.default(M_9)

# e to the power of the coefficient for sex
exp(estimates['sex'])

# this is the factor by which the rate changes for a unit change in the predictor `sex`

M_10 <- glm(doctorco ~ sex + age + insurance, 
           data = doctor_df,
           family = poisson(link = 'log')
)
summary(M_10)

# calculate p-value for null hypothesis test 
# comparing M_9 and M_10
pchisq(deviance(M_9) - deviance(M_10), df = 4, lower.tail = F)

anova(M_9, M_10, test = 'Chisq')


# exposure and offsets ----------------------------------------------------

insurance_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm03/master/data/Insurance.csv")

M_11 <- glm(Claims ~ Age + offset(log(Holders)),
            data = insurance_df,
            family = poisson(link = 'log'))

tibble(Age = unique(insurance_df$Age), Holders = 100) %>% 
  add_predictions(M_11, type = 'response', var = 'rate')
