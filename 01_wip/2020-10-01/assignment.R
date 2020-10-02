# SOK-3020, Assignment 5, due Oct. 2nd, 2020
# Øystein Solvang

### Import data
setwd('/Users/oysteinsolvang/Dropbox/Akademia/01-Studier/09-H20/SOK-3020/02_DATA/poe5rdata/')
load("tvdata.rdata")
load("malawi_small.rdata")
setwd('/Users/oysteinsolvang/Dropbox/Akademia/01-Studier/09-H20/SOK-3020/01_WIP/2020-10-01/')
### Libraries
library(pacman)
library(mosaic)
library(tidyverse)
library(fBasics)



### Exercise 4.17
df <- tvdata

### A
ggplot(df,aes(x=spend_uk,y=rate_uk)) +
    geom_point() +
    coord_flip() +
    labs(title="Figure 1: RATE_UK plotted against SPEND_UK")
ggsave("fig1.png")

### B
fit <- lm(rate_uk~log(spend_uk),data=df)
summary(fit)
df <- df %>% mutate(fitted_values = predict(fit))
ggplot(df,aes(x=spend_uk,y=rate_uk)) +
    geom_point(col="red") +
    geom_point(aes(spend_uk,fitted_values),col="blue") +
    coord_flip() +
    labs(title="Figure 2: RATE_UK plotted against SPEND_UK and OLS estimates of lin-log model")
ggsave("fig2.png")
summary(df$rate_uk)

### D
fit <- lm(rate_uk~log(spend_uk-280),data=df)
summary(fit)
df <- df %>% mutate(fitted_values = predict(fit))
ggplot(df,aes(x=spend_uk,y=rate_uk)) +
    geom_point(col="red") +
    geom_point(aes(spend_uk,fitted_values),col="blue") +
    coord_flip() +
    labs(title="Figure 3: SPEND_UK plotted against OLS estimates of lin-log model")
ggsave("fig3.png")

### E
fit <- lm(log(rate_uk)~I(1/spend_uk),data=df)
summary(fit)
df <- df %>% mutate(fitted_values = predict(fit))
ggplot(df,aes(x=spend_uk,y=log(rate_uk))) +
    geom_point(col="red") +
    geom_point(aes(spend_uk,fitted_values),col="blue") +
    coord_flip() +
    labs(title="Figure 4: SPEND_UK plotted against OLS estimates of log-reciprocal model")
ggsave("fig4.png")

### G
fit <- lm(log(rate_uk)~I(1/(spend_uk-280)),data=df)
summary(fit)
df <- df %>% mutate(fitted_values = predict(fit))
ggplot(df,aes(x=spend_uk,y=log(rate_uk))) +
    geom_point(col="red") +
    geom_point(aes(spend_uk,fitted_values),col="blue") +
    coord_flip() +
    labs(title="Figure 5: SPEND_UK plotted against OLS estimates of log-reciprocal model")
ggsave("fig5.png")


### H
df <- na.omit(df)
# sample data
ggplot(df,aes(x=spend_ir,y=rate_ir)) +
    geom_point() +
    coord_flip() +
    labs(title="Figure 6: RATE_IR plotted against SPEND_IR")
ggsave("fig6.png")

# lin-log without correction
fit <- lm(rate_ir~log(spend_ir),data=df)
summary(fit)
df <- df %>% mutate(fitted_values = predict(fit))
ggplot(df,aes(x=spend_ir,y=rate_ir)) +
    geom_point(col="red") +
    geom_point(aes(spend_ir,fitted_values),col="blue") +
    coord_flip() +
    labs(title="Figure 7: RATE_IR plotted against SPEND_IR and OLS estimates of lin-log model")
ggsave("fig7.png")
summary(df$rate_ir)

# lin-log with correction
fit <- lm(rate_ir~log(spend_ir-240),data=df)
summary(fit)
df <- df %>% mutate(fitted_values = predict(fit))
ggplot(df,aes(x=spend_ir,y=rate_ir)) +
    geom_point(col="red") +
    geom_point(aes(spend_ir,fitted_values),col="blue") +
    coord_flip() +
    labs(title="Figure 8: SPEND_IR plotted against OLS estimates of lin-log model")
ggsave("fig8.png")

# LOG-REC without correction
fit <- lm(log(rate_ir)~I(1/spend_ir),data=df)
summary(fit)
df <- df %>% mutate(fitted_values = predict(fit))
ggplot(df,aes(x=spend_ir,y=log(rate_ir))) +
    geom_point(col="red") +
    geom_point(aes(spend_ir,fitted_values),col="blue") +
    coord_flip() +
    labs(title="Figure 9: SPEND_IR plotted against OLS estimates of log-reciprocal model")
ggsave("fig9.png")

# LOG-REC with correction
fit <- lm(log(rate_ir)~I(1/(spend_ir-240)),data=df)
summary(fit)
df <- df %>% mutate(fitted_values = predict(fit))
ggplot(df,aes(x=spend_ir,y=log(rate_ir))) +
    geom_point(col="red") +
    geom_point(aes(spend_ir,fitted_values),col="blue") +
    coord_flip() +
    labs(title="Figure 10: SPEND_IR plotted against OLS estimates of log-reciprocal model")
ggsave("fig10.png")


### Exercise 4.21
df <- malawi_small
### B
fit <- lm(pfood~log(totexp),data=df)
summary(fit)
confint(fit)
df <- df %>% mutate(fitted_values = predict(fit))
ggplot(df,aes(x=totexp,y=pfood)) +
    geom_point(col="red") +
    geom_point(aes(totexp,fitted_values),col="blue") +
    coord_flip() +
    labs(title="Figure 11: Log-linear model")
ggsave("fig11.png")

### C
totexp_05 <- quantile(df$totexp,c(.05))
totexp_75 <- quantile(df$totexp,c(.75))
(fit$coeff[1] + fit$coeff[2]*(log(totexp_05)+1))/(fit$coeff[1] + fit$coeff[2]*(log(totexp_05)))
(fit$coeff[1] + fit$coeff[2]*(log(totexp_75)+1))/(fit$coeff[1] + fit$coeff[2]*(log(totexp_75)))

### D
ggplot(df,aes(x=resid(fit))) +
    geom_histogram(binwidth=0.05) +
    labs(title="Figure 12: Histogram of residuals of log-linear model")
ggsave("fig12.png")
ggplot(df,aes(resid(fit),log(totexp))) +
    geom_point() +
    labs(title="Figure 13: Residuals plotted against values of LOG(TOTEXP)")
ggsave("fig13.png")
skewness(resid(fit))
kurtosis(resid(fit))
jarque.bera.test(resid(fit))

### E
df <- df %>% mutate(food = pfood*totexp)
fit <- lm(log(food)~log(totexp),data=df)
summary(fit)

### F
ggplot(df,aes(x=resid(fit))) +
    geom_histogram(binwidth=0.05) +
    labs(title="Figure 14: Histogram of residuals of log-log model")
ggsave("fig14.png")
ggplot(df,aes(resid(fit),log(totexp))) +
    geom_point() +
    labs(title="Figure 15: Residuals plotted against values of LOG(TOTEXP)")
ggsave("fig15.png")
skewness(resid(fit))
kurtosis(resid(fit))
jarque.bera.test(resid(fit))

### G
fit <- lm(food~log(totexp),data=df)
summary(fit)
food_50 <- quantile(df$food,.50)
food_75 <- quantile(df$food,.75)
fit$coeff[2]*(1/food_50)
fit$coeff[2]*(1/food_75)

### H
ggplot(df,aes(x=resid(fit))) +
    geom_histogram(binwidth=0.05) +
    labs(title="Figure 16: Histogram of residuals of linear-log model")
ggsave("fig16.png")
ggplot(df,aes(resid(fit),log(totexp))) +
    geom_point() +
    labs(title="Figure 17: Residuals plotted against values of LOG(TOTEXP)")
ggsave("fig17.png")
skewness(resid(fit))
kurtosis(resid(fit))
jarque.bera.test(resid(fit))


### I
df <- malawi_small
df <- df %>% mutate(food = pfood*totexp)
fit1 <- lm(pfood~log(totexp),data=df)
fit2 <- lm(log(food)~log(totexp),data=df)
fit3 <- lm(food~log(totexp),data=df)
df <- df %>% mutate(fit1 = predict(fit1)*totexp) %>%
    mutate(fit2 = exp(predict(fit2))) %>%
    mutate(fit3 = predict(fit3))
df2 <- df[,c(11:14)]
cor(df2)



