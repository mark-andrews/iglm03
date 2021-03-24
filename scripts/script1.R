
# load packages -----------------------------------------------------------

library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm03/master/data/weight.csv")
weight_df_male <- filter(weight_df, gender == 'male')

M_1 <- lm(weight ~ height, data = weight_df_male)

# get the estimates of the coefficients of M_1
coef(M_1)
coefficients(M_1)
sigma(M_1)
