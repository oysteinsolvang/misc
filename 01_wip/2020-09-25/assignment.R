# SOK-3020 -- Assignment 4 (Øystein Solvang)
# Due Sep. 25th

### Data
setwd("/Users/oysteinsolvang/Dropbox/Akademia/01-Studier/09-H20/SOK-3020/02_DATA/poe5rdata/")
load("collegetown.rdata")
load("fair5.rdata")
load("ashcan_small.rdata")
load("cps5.rdata")
load("cps5_small.rdata")
load("motel.rdata")

### Preamble
library(tidyverse)
library(mosaic)
library(pacman)
library(ggeffects)
p_load(rockchalk, car, multcomp, FSA)
setwd("/Users/oysteinsolvang/Dropbox/Akademia/01-Studier/09-H20/SOK-3020/01_WIP/2020-09-25")

### Exercise 3.23
### A

### B

### C
d <- collegetown
fit <- lm(price~I(sqft^2),data=d)
f <- makeFun(fit)
f(20,interval="confidence",level=.95)

### D
d1 <- d %>% filter(sqft==20)
mean(d1$price)


### Exercise 3.24
### A
fit <- lm(vote~growth,fair5)
confint(fit,level=.95)

### B
f <- makeFun(fit)
stderror <- summary(fit)$coef[4]
# point estimate
f(4)
# confidence intervals
f(4)-(stderror*qt(0.975,24)) # low bound, 95%
f(4)+(stderror*qt(0.975,24)) # high bound, 95%
f(4)-(stderror*qt(0.995,24)) # low bound, 99%
f(4)+(stderror*qt(0.995,24)) # high bound, 99%


### C
qt(0.99,24) # Critical value
summary(glht(fit, linfct = c("growth <= 0")))


### D
fit <- lm(vote~inflat,data=fair5)


### 3.25
df <- ashcan_small

### A
d <- df %>% filter(sold==1) # Using data on works with SOLD==1
fit <- lm(I(log(rhammer))~years_old,data=d)
b2 <- summary(fit)$coef[2] # slope
stderror <- summary(fit)$coef[4]
summary(fit) # Gives df
b2*100 # Point estimate
100*(b2+(stderror*qt(0.975,420))) # upper bound
100*(b2-(stderror*qt(0.975,420))) # lower bound

### B
qt(0.975,420)
summary(glht(fit, linfct = c("years_old = 0.02")))

### C
fit <- lm(I(log(rhammer))~drec,data=d)
b2 <- summary(fit)$coef[2] # slope
stderror <- summary(fit)$coef[4]
summary(fit) # Gives df
b2*100 # Point estimate
100*(b2+(stderror*qt(0.975,420))) # upper bound
100*(b2-(stderror*qt(0.975,420))) # lower bound

### D
qt(0.05,420)
summary(glht(fit, linfct = c("drec <= 0.02")))


### 3.26
df <- cps5_small
### A
fit <- lm(wage~exper,data=df)
summary(fit)
### B
summary(glht(fit,linfct = c("exper <= 0")))
###
d <- df %>% filter(metro==1)
fit2 <- lm(wage~exper,data=d)
qt(0.99,984)
summary(glht(fit2,linfct = c("exper <= 0")))
### D
d <- df %>% filter(metro==0)
fit3 <- lm(wage~exper,data=d)
qt(0.99,212)
summary(glht(fit3,linfct = c("exper <= 0")))


### 3.28
df <- motel
### A
df$relprice2 <- df$relprice*100
ggplot(df,aes(x=time,y=relprice2)) +
    geom_point()
ggsave("plot1.png")
summary(df$relprice2)
### B
fit <- lm(motel_pct~relprice2,data=df)
summary(fit)
confint(fit)
ggplot(df,aes(x=relprice2,y=motel_pct)) +
    geom_point() +
    geom_smooth(method="lm") +
    coord_flip()
ggsave("plot2.png")
### C
f <- makeFun(fit)
stderror <- summary(fit)$coef[4]
rp80 <- f(80)
rp80 # Point estimate
rp80+(stderror*qt(0.95,23)) # CI upper bound
rp80-(stderror*qt(0.95,23)) # CI lower bound
### D
qt(0.05,23)
summary(glht(fit,linfct = c("relprice2 >= 0")))
### E
qt(0.975,23)
summary(glht(fit,linfct = c("relprice2 = -1")))


