
# Load packages

library(ggplot2)

# Set seed
set.seed(2020)

# WISC-V Test-Retest Correlations
# This uses the "Corrected R" from Table 4.7 of the WISC-V A&NZ Technical Manual

stability <- c("Verbal Comprehension" = .91, # test-retest reliability on the WISC-V for the VCI 
               "Visual Spatial" = .84, # test-retest reliability on the WISC-V for the VSI
               "Fluid Reasoning" = .75, # test-retest reliability on the WISC-V for the FRI
               "Working Memory" = .82, # test-retest reliability on the WISC-V for the WMI
               "Processing Speed" = .83, # test-retest reliability on the WISC-V for the PSI
               "Full Scale IQ" = .92) # test-retest reliability on the WISC-V for the FSIQ

# WISC-V SEM
# This uses the "Overall Average SEM" from Table 4.4 of the WISC-V A&NZ Technical Manual

wisc.sem <- c("Verbal Comprehension" = 4.22, # average sem for VCI
         "Visual Spatial" = 4.36, # average sem for VSI
         "Fluid Reasoning" = 3.89, # average sem for FRI
         "Working Memory" = 4.26, # average sem for WMI
         "Processing Speed" = 5.24, # average sem for PSI
         "Full Scale IQ" = 2.90) # average sem for FSIQ

wisc.sem <- 1 - (wisc.sem/15) # calculate the expected 

# WISC-V and WAIS-IV correlations
# This uses the "Corrected R" from Table 5.8 of the WISC-V A&NZ Technical Manual

correlations <- c("Verbal Comprehension" = .83, # correlation between the WISC-V VCI and WAIS-IV VCI
                  "Visual Spatial - Perceptual Reasoning" = .83, #  correlation between the WISC-V VSI and WAIS-IV PRI
                  "Fluid Reasoning - Perceptual Reasoning" = .62, # correlation between the WISC-V FRI and WAIS-IV PRI
                  "Working Memory" = .76, # correlation between the WISC-V WMI and WAIS-IV WMI
                  "Processing Speed" = .83, # correlation between the WISC-V PSI and WAIS-IV PSI
                  "Full Scale IQ" = .89) # correlation between the WISC-V FSIQ and WAIS-IV FSIQ

# WAIS-IV SEM
# This uses the "Overall Average SEM" from Table 4.3 of the WAIS-IV Technical Manual

wais.sem <- c("Verbal Comprehension" = 2.85, # average sem for WAIS-IV VCI
              "Visual Spatial - Perceptual Reasoning" = 3.48, # average sem for WAIS-IV PRI
              "Fluid Reasoning - Perceptual Reasoning" = 3.48, # average sem for WAIS-IV PRI
              "Working Memory" = 3.67, # average sem for WAIS-IV WMI
              "Processing Speed" = 4.78, # average sem for WAIS-IV PSI
              "Full Scale IQ" = 2.16) # average sem for WAIS-IV FSIQ

wais.sem <- 1 - (wais.sem/15) # calculate the expected 

# Test Names
first.test <- "WISC-V"
second.test <- "WAIS-IV"

# Functions

simulation <- function(n = 1000, 
                       mean = 100, 
                       sd = 15, 
                       iv,
                       orig.test,
                       r) {
  #' @param n The number of people to simulate, default is 1000
  #' @param mean The mean of the simulated distributions, default is 100
  #' @param sd The standard deviation of the simulated distributions, default is 15
  #' @param iv An independent standard normal variable, default is a normal curve, default uses n, sd and mean
  #' @param orig.test First test distribution, default uses n, sd and mean
  #' @param r The correlation between predicted.test and orig.test
  #' @return A list with the simulated distributions, including predicted.test, a third variable with correlation r with orig.test
  if(missing(iv)) {
    iv <- rnorm(n = n, mean = mean, sd = sd)
  }
  if(missing(orig.test)) {
    orig.test <- rnorm(n = n, mean = mean, sd = sd)
  }
  if(missing(r)) {
    stop("Please provide a value for r")
  }
  predicted.test <- scale(orig.test) * r  +  scale(residuals(lm(iv ~ orig.test))) * sqrt(1 - r * r)
  predicted.test <- mean + (predicted.test - 0) * (sd/1) # Convert to mean and sd of original test
  list <- list("iv" = iv, "orig.test" = orig.test, "predicted.test" = predicted.test)
  return(list)
}

cor.plot <- function(orig.test.scores, predicted.test.scores, orig.test.name, predicted.test.name, title, subtitle) {
  #' @param orig.test.scores The simulated scores for the first test
  #' @param predicted.test.scores The simulated scores for the second test
  #' @param orig.test.name The name of the first test
  #' @param predicted.test.name The name of the second test
  #' @param title The title of the plot
  #' @param subtitle The subtitle of the plot
  cor.plot <- ggplot(mapping = aes(x = orig.test.scores, y = predicted.test.scores)) +
    geom_point() +
    scale_x_continuous(name = orig.test.name, breaks = seq(40, 160, 10), limits = c(40, 160)) +
    scale_y_continuous(name = predicted.test.name, breaks = seq(40, 160, 10), limits = c(40, 160)) +
    labs(title = title, subtitle = subtitle)
}

bias.plot <- function(bias, title, subtitle) {
  #' @param bias The amount of bias between test 1 and test 2
  #' @param title The title of the plot
  #' @param subtitle The subtitle of the plot
  ggplot(mapping = aes(x = bias)) +
    geom_histogram(stat = "bin", binwidth = 10) +
    stat_bin(aes(y = ..count.., label=..count..), geom = "text", vjust = -0.5, binwidth = 10) +
    scale_x_continuous(name = "Bias", breaks = seq(-50, 50, 10), limits = c(-50, 50)) +
    scale_y_continuous(name = "Count", breaks = seq(0, 600, 50), limits = c(0,600)) +
    labs(title = title, subtitle = subtitle)
}
  
# Simulation Code

for (i in 1:6) {
  
  # Simulate test "temporal stability"
  wisc.retest <- simulation(r = stability[i])
  
  # Print the scatter plot of first simulation
  print(cor.plot(orig.test.scores = wisc.retest$orig.test, 
                 predicted.test.scores = wisc.retest$predicted.test,
                 orig.test.name = first.test,
                 predicted.test.name = second.test,
                 title = names(correlations[i]), 
                 subtitle = "Stability"))
  
  # Print the amount of bias in the first simulation
  print(bias.plot(bias = wisc.retest$predicted.test - wisc.retest$orig.test, 
                  title = names(correlations[i]), 
                  subtitle = "Stability Bias"))
  
  # Simulate test "error measurement"
  wisc.error <- simulation(orig.test = wisc.retest$predicted.test, # use the simulated wisc retest scores
                           r = wisc.sem[i])
  
  # Print the scatter plot of first simulation
  print(cor.plot(orig.test.scores = wisc.retest$orig.test, 
                 predicted.test.scores = wisc.error$predicted.test,
                 orig.test.name = first.test,
                 predicted.test.name = second.test,
                 title = names(correlations[i]), 
                 subtitle = "Stability + SEM"))
  
  # Print the amount of bias in the first simulation
  print(bias.plot(bias = wisc.error$predicted.test - wisc.retest$orig.test, 
                  title = names(correlations[i]), 
                  subtitle = "Stability + WISC SEM Bias"))
  
  # Simulate the WISC-V and WAIS-IV intercorrelations
  wais <- simulation(orig.test = wisc.error$predicted.test, # use the simulated wisc retest scores
                     r = correlations[i])

  # Print the scatter plot of second simulation
  print(cor.plot(orig.test.scores = wisc.retest$orig.test, 
                 predicted.test.scores = wais$predicted.test, 
                 orig.test.name = first.test,
                 predicted.test.name = second.test,
                 title = names(correlations[i]), 
                 subtitle = "Stability + WISC SEM + Intercorrelations"))
  
  # Print the amount of bias in the second simulation
  print(bias.plot(bias = wais$predicted.test - wisc.retest$orig.test, # compare WAIS prediction to original WISC score
                  title = names(correlations[i]), 
                  subtitle = "Stability + WISC SEM + Intercorrelations Bias"))
  
  # Simulate the WAIS-IV SEM
  wais.error <- simulation(orig.test = wais$predicted.test, # use the simulated wisc retest scores
                     r = correlations[i])
  
  # Print the scatter plot of second simulation
  print(cor.plot(orig.test.scores = wisc.retest$orig.test, 
                 predicted.test.scores = wais.error$predicted.test, 
                 orig.test.name = first.test,
                 predicted.test.name = second.test,
                 title = names(correlations[i]), 
                 subtitle = "Stability + WISC SEM + Intercorrelations + WAIS SEM"))
  
  # Print the amount of bias in the second simulation
  print(bias.plot(bias = wais.error$predicted.test - wisc.retest$orig.test, # compare WAIS SEM prediction to original WISC score
                  title = names(correlations[i]), 
                  subtitle = "Stability + WISC SEM + Intercorrelations + WAIS SEM Bias"))
}