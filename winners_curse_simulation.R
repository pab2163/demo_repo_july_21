library(tidyverse)

n = 150
x = rnorm(n = n)


y_variables = 500

output_df = data.frame(index = 1:y_variables) 
output_df$coef = NA
output_df$pval = NA

for (i in 1:y_variables){
  y = rnorm(n = n, mean = .1*x, sd = 1)
  df = data.frame(x = x, y = y)
  model = lm(data = df, y ~ x)
  output_df$coef[i] = model$coefficients[2]
  a = summary(model)
  output_df$pval[i] = a$coefficients[2,4]
}


output_df %>% mutate(sig = ifelse(pval < .05, 'significant', 'not significant')) %>%
  group_by(sig) %>%
  summarise(avg_coef = mean(coef))

