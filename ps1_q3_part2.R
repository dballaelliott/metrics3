### Metrics ###

library(tidyverse)
library(stargazer)
set.seed(1234)
n = 10000

###############################
### Nonparametric Bootstrap ###
###############################

df <- data.frame(u1 = rnorm(n,0,1), u2 = rnorm(n,0,1))
df <- df %>% 
  mutate(y1 = 5 + u1,
         y0 = 2 + u2,
         r = runif(n, min=0, max=1),
         treat = ifelse(r < 0.5, 1, 0),
         y = ifelse(treat==1, y1, y0))

# Part a: find e(y_1 - y_0) using OLS
ols1 <- lm(y ~ treat, df)
stargazer(ols1, title="OLS", covariate.labels=c("$\\beta_1$", "$\\beta_0$"), no.space=T,
          header=F, omit.stat="all", dep.var.caption="", float=F,
          dep.var.labels.include=F, table.layout = "=t=",
          out="ps1_q3_c.tex")

# Part b: bootstrap or whatever 
fn_bootstrap <- function(s){
  temp_df <- sample_n(df, 10000, replace=T)
  
  coef <- lm(y ~ treat, temp_df)$coefficients
  
  return(data.frame(num_bs = s, beta0 = coef[1], beta1 = coef[2]))
}

bs <- lapply(1:10000, fn_bootstrap)
bs_df <- data.table::rbindlist(bs)
bs_df <- bs_df %>% 
  mutate(beta02 = beta0^2,
         beta12 = beta1^2)

se0 <- sqrt(1/n * sum(bs_df$beta02) - (1/n * sum(bs_df$beta0))^2)
se1 <- sqrt(1/n * sum(bs_df$beta12) - (1/n * sum(bs_df$beta1))^2)

# table with bootstrapped SE (just changed the list of SEs as calculated above)
stargazer(ols1, covariate.labels=c("$\\beta_1$", "$\\beta_0$"), no.space=T,
          header=F, omit.stat="all", dep.var.caption="", float=F, se = list(NULL, se1, se0),
          dep.var.labels.include=F, table.layout = "=t=",
          out="ps1_q3_d.tex")

# histogram of betas
p <- ggplot(bs_df) + 
  geom_histogram(aes(x=beta1), fill=MetBrewer::met.brewer("Hiroshige")[8], color=MetBrewer::met.brewer("Hiroshige")[9]) + 
  geom_vline(xintercept = ols1$coefficients[2], color=MetBrewer::met.brewer("Hiroshige")[1]) +
  theme_minimal() + 
  labs(y = "Frequency", x = expression("Monte Carlo Distribution of"~beta))
ggsave(p, filename="ps1_q3_d.pdf")
