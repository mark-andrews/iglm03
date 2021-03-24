
# load packages -----------------------------------------------------------

library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm03/master/data/weight.csv")
weight_df_male <- filter(weight_df, gender == 'male')

M_1 <- lm(weight ~ height, data = weight_df_male)

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
