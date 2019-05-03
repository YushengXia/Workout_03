## ----setup, include=FALSE, message=FALSE---------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(binomial)

## ------------------------------------------------------------------------
#Possible combinations in 4 trials with 3 successes.
bin_choose(4, 3)

## ------------------------------------------------------------------------
#Proability of getting 4 successes in 5 trials with a probability 0.2 of success
bin_probability(4, 5, 0.2)

## ------------------------------------------------------------------------
#Dataframe of possible successes in 5 trials and 0.3 success per trial.
bin_distribution(5, 0.3)

## ------------------------------------------------------------------------
#Bar graph of possible successes and probability of possible successes in 5 trials with 0.3 success per trial.
dis1 <- bin_distribution(5, 0.3)
plot.bindis(dis1)

## ------------------------------------------------------------------------
#Dataframe with cumulative probability added.
bin_cumulative(5, 0.3)

## ------------------------------------------------------------------------
#Cumulative line graph of possible successes and cumulative probability of successes in 5 trials with 0.3 success per trial.
dis2 <- bin_cumulative(5, 0.5)
plot.bincum(dis2)

## ------------------------------------------------------------------------
#Using bin_variable()
bin1 <- bin_variable(10, 0.3)
bin1

## ------------------------------------------------------------------------
#Summary statistics for n = 10 and p = 0.3
bin1 <- bin_variable(10, 0.3)
summary(bin1)

