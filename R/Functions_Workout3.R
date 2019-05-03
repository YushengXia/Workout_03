#Private Functions:


#Check Probability Function
check_prob <- function(p) {
  #makes sure p is a numeric value
  if (class(p) != "numeric") {
    stop("invalid p value")
  }
  #prevents negative
  if (p<0){
    stop("invalid p value")
  }
  #prevents p from being greater than 1
  if (p>1) {
    stop("invalid p value")
  }
  else {
    return(TRUE)
  }
}

#Check Trials Function
check_trials <- function(n) {
  #make sure n is integer
  if (n != round(n)) {
    stop("invalid n value")
  }
  #make sure n is numeric
  if (class(n) != "numeric") {
    stop("invalid n value")
  }
  #make sure n is positive or 0
  if (n < 0) {
    stop("invalid n value")
  }
  else {
    return(TRUE)
  }
}

#Check Success Function
check_success <- function(k, n) {
  if (sum(k > n) > 0) {
    stop("success cannot be greater than trials")
  }
  if (sum(k) != sum(round(k))) {
    stop("invalid k value")
  }
  #make sure n is numeric
  if (class(k) != "numeric" & class(k) != "integer") {
    stop("invalid k value")
  }
  else {
    return(TRUE)
  }
}

#Function to get mean
aux_mean <- function(n, p) {
  mean <- n*p
  return(mean)
}

#Function to get variance
aux_variance <- function(n, p) {
  variance <- n * p * (1-p)
  return(variance)
}

#Function to get mode
aux_mode <- function(n, p) {
  mode <- round(n * p)
  return(mode)
}

#Function to get skewness
aux_skewness <- function(n, p) {
  skewness <- (1 - 2*p)/(n * p * (1-p))^(1/2)
  return(skewness)
}

#Function to get kurtosis
aux_kurtosis <- function(n, p) {
  kurtosis <- (1 - 6 * p * (1 - p))/(n * p * (1-p))
  return(kurtosis)
}


#Main Functions


#Function bin_choose()
#' @title bin_choose
#' @desciption Calculates the number of combinations in which k successes can occur in n trials
#' @param k numeric value for number of successes
#' @param n numeric value for number of trials
#' @return combinations of successes
#' @export
#' @examples
#' #number of combinations in which 2 successes occur in 5 trials
#' bin_choose(n = 5, k = 2)
#'
#' #number of combinations in which 1:3 successes occur in 5 trials
#' bin_choose(5, 1:3)

bin_choose <- function(n, k) {
  if (max(k) > n) {
    stop("k cannot be greater than n")
  } else
    combinations <- factorial(n)/(factorial(k) * factorial(n - k))
  return(combinations)
}

#Function bin_probability()
#' @title bin_probability
#' @desciption Calculates the probability of getting k successes in n trials
#' @param k numeric value for number of successes
#' @param n numeric value for number of trials
#' @param p numeric value for probability of one success
#' @return probability of k successes
#' @export
#' @examples
#' #probability of 2 successes in 5 trials with success rate of 0.5
#' bin_probability(2, 5, 0.5)
#'
#' #probability of 0:2 successes in 5 trials with success rate of 0.5
#' bin_probability(0:2, 5, 0.5)
#'
#' #probability of 55 successes in 100 trials with success rate of 0.45
#' bin_probability(55, 100, .45)

bin_probability <- function(k, n, p) {
  if (check_trials(n) != TRUE) {
    stop("invalid trial number")
  } else if (check_prob(p) != TRUE) {
    stop("invalid probability")
  } else if (check_success(k, n) != TRUE) {
    stop("invalid success")
  } else {
    probability_success <- bin_choose(n, k) * p^k * (1-p)^(n - k)
    return(probability_success)
  }
}

#bin_distribution() function
#' @title bin_distribution
#' @desciption Creates a data.frame that shows probability of getting certain numer of successes
#' @param n number of trials
#' @param p probability of one success
#' @return table of success rates
#' @export
#' @example
#' #table of successes and probability for obtaining each number of successes
#' bin_distribution(5, 0.5)

bin_distribution <- function(n, p) {
  success <- c(0:n)
  probability <- rep(0, n+1)
  for (i in 0:n) {
    probability[i + 1] <-  bin_probability(i, n, p)
  }
  distribution <- data.frame(success, probability)
  class(distribution) <- c("bindis", "data.frame")
  return(distribution)
}

#Function plot.bindis
#' @export

plot.bindis <- function(x) {
  barplot(x$probability, xlab = "successes", ylab = "probability", names.arg = x$success, las = 1)
}

#bin_cumulative() function
#' @title bin_cumulative
#' @desciption Creates a data.frame that shows probability of getting certain numer of successes, and cumulative success
#' @param n number of trials
#' @param p probability of one success
#' @return table of success rates and cumulative success
#' @export
#' @example
#' #returns table with number of success (up to five), probability of number of successes, and cumulative probability.
#' bin_cumulative(5, 0.5)

bin_cumulative <- function(n, p){
  success <- c(0:n)
  probability <- rep(0, n+1)
  cumulative <- rep(0, n+1)
  for (i in 0:n) {
    probability[i + 1] <-  bin_probability(i, n, p)
    cumulative[i + 1] <- sum(probability)
  }
  cumulative_dist <- data.frame(success, probability, cumulative)
  class(cumulative_dist) <- c("bincum", "data.frame")
  return(cumulative_dist)
}

#plot.bincum
#' @export

plot.bincum <- function(x) {
  plot(x$cumulative, xaxt = "n", xlab = "successes", ylab = "probability", las = 1)
  lines(x$cumulative)
  axis(1, at = 1:length(x$success), labels = x$success)
}

#bin_variable function
#' @title bin_variable
#' @desciption returns object with class "binvar"
#' @param n number of trials
#' @param p probability of one success
#' @return object with class "binvar"
#' @export

bin_variable <- function(n, p) {
  if (check_trials(n) != TRUE) {
    stop("invalid trial number")
  } else if (check_prob(p) != TRUE) {
    stop("invalid probability")
  } else {
    named_elements <- list(
      trials = n,
      probability = p
    )
    class(named_elements) <- "binvar"
  }
  return(named_elements)
}

#Method print.binvar
#' @export

print.binvar <- function(x) {
  cat('"Binomial Variable"')
  cat("\n\nParameters")
  cat("\n- number of trials: ")
  cat(x$trials)
  cat("\n- prob of success : ")
  cat(x$probability)
  invisible(x)
}

#Function summary.binvar
#' @export

summary.binvar <- function(x) {
  summary_elements <- list(
    trials = as.numeric(x[1]),
    prob = as.numeric(x[2]),
    mean = aux_mean(as.numeric(x[1]), as.numeric(x[2])),
    variance = aux_variance(as.numeric(x[1]), as.numeric(x[2])),
    mode = aux_mode(as.numeric(x[1]), as.numeric(x[2])),
    skewness = aux_skewness(as.numeric(x[1]), as.numeric(x[2])),
    kurtosis = aux_kurtosis(as.numeric(x[1]), as.numeric(x[2]))
  )
  class(summary_elements) <- "summary.binvar"
  return(summary_elements)
}

#' @export

print.summary.binvar <- function(x) {
  cat('"Summary Binomial"')
  cat("\n\nParameters")
  cat("\n- number of trials: ")
  cat(x$trials)
  cat("\n- prob of success : ")
  cat(x$prob)
  cat("\n\nMeasures")
  cat("\n- mean    : ")
  cat(x$mean)
  cat("\n- variance: ")
  cat(x$variance)
  cat("\n- mode    : ")
  cat(x$mode)
  cat("\n- skewness: ")
  cat(x$skewness)
  cat("\n- kurtosis: ")
  cat(x$kurtosis)
  invisible(x)
}

#Functions of Measure
#' @title bin_mean
#' @desciption finds mean number of successes
#' @param n number of trials
#' @param p probability of one success
#' @return mean of successes
#' @export
#' @example
#' #mean success of 10 trials and 0.5 success per trial
#' bin_mean(10, 0.3)

bin_mean <- function(n, p) {
  if (check_trials(n) != TRUE){
    stop("invalid n")
  } else if (check_prob(p) != TRUE){
    stop("invalid p")
  } else {
    return(aux_mean(n, p))
  }
}

#' @title bin_variance
#' @desciption finds variance of number of successes
#' @param n number of trials
#' @param p probability of one success
#' @return variance of number of successes
#' @export
#' @example
#' #variance success of 10 trials and 0.5 success per trial
#' bin_variance(10, 0.3)

bin_variance <- function(n, p) {
  if (check_trials(n) != TRUE){
    stop("invalid n")
  } else if (check_prob(p) != TRUE){
    stop("invalid p")
  } else {
    return(aux_variance(n, p))
  }
}

#' @title bin_mode
#' @desciption finds mode of number of successes
#' @param n number of trials
#' @param p probability of one success
#' @return mode of number of successes
#' @export
#' @example
#' #mode success of 10 trials and 0.5 success per trial
#' bin_mode(10, 0.3)

bin_mode <- function(n, p) {
  if (check_trials(n) != TRUE){
    stop("invalid n")
  } else if (check_prob(p) != TRUE){
    stop("invalid p")
  } else {
    return(aux_mode(n, p))
  }
}

#' @title bin_skewness
#' @desciption shows measure of the asymmetry of probability distribution
#' @param n number of trials
#' @param p probability of one success
#' @return asymmetry of probability distribution
#' @export
#' @example
#' #skewness of distribution in success of 10 trials and 0.5 success per trial
#' bin_skewness(10, 0.3)

bin_skewness <- function(n, p) {
  if (check_trials(n) != TRUE){
    stop("invalid n")
  } else if (check_prob(p) != TRUE){
    stop("invalid p")
  } else {
    return(aux_skewness(n, p))
  }
}

#' @title bin_kurtosis
#' @desciption shows measure of the "tailedness" of the probility disribution
#' @param n number of trials
#' @param p probability of one success
#' @return "tailedness" of the probility disribution
#' @export
#' @example
#' #kurtosis of success in 10 trials and 0.5 success per trial
#' bin_kurtosis(10, 0.3)

bin_kurtosis <- function(n, p) {
  if (check_trials(n) != TRUE){
    stop("invalid n")
  } else if (check_prob(p) != TRUE){
    stop("invalid p")
  } else {
    return(aux_kurtosis(n, p))
  }
}
