***Workout 03: Creating new package***
-----

This folder contains all the necessary material for Workout 03 assignment on a binomial package. 
The binomial package contains five main functions that relate to the famous binomial distribution. 
This workout was completed by Yusheng Xia and is assigned in Statistics 133.

## Binomial Package

The package '"Binomial"' is a package that contains many functions that relate to the famous binomial probability distribution. The package contains five main functions:
  
  - bin_choose(n, k) to calculate the number of combinations in which *k* successes can occur in *n* trials.
  - bin_probability(k, n, p) to calculate the probability of getting *k* successes in *n* trials with a *p* success probability.
  - bin_distribution(n, p) to return a dataframe of class '"bindis"' that has number of successes (up to n) in first column and probability of success number in second.
  - bin_cumulative(n, p) to return a dataframe of class '"bincum"' that is the same as bin_distribution() but also contains a column of cumulative probability.
  - bin_var(n, p) to return a named list of class '"binvar"' that contains the number of trials *n* and probability of success *p*.
  
In addition, the package also contains various methods that allow for easy plotting and summary functions to be displayed to users.
