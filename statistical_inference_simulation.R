## Statistical Inference - Problem 1 - Simulation Exercises

library(ggplot2)

# Simulate rexp distribution - 1000 iterations of sample size = 40
set.seed(10)
lambda <- 0.2
sampleSize <- 40
simulations <- 1000
row_means <- rowMeans(matrix(data = rexp(simulations*sampleSize, rate=lambda), 
                  nrow = simulations, ncol = sampleSize))

# Comparison of center of graph with theoretical center
paste("Graph is centered at: ", round(mean(row_means), 3))
paste("Graph should have been theoretically centered at: ", round(1/lambda, 3))

# Comparison of variance of graph with theoretical variance
paste("Variance of Graph: ", round(var(row_means), 3))
paste("Theoretical variance: ", round(1/(lambda*lambda*sampleSize), 3))

# Plot the histogram of row means
hist(row_means, breaks=50, prob=TRUE, main="Distribution of sample averages
     drawn from exponential distribution with lambda=0.2", xlab="")

# Plot the density curve for the means
lines(density(row_means))

# Add 'theoretical center of distribution' for comparison
abline(v=1/lambda, col="red")

# Add 'theoretical density for sample means' for comparison
xfit <- seq(min(row_means), max(row_means), length=100)
yfit <- dnorm(xfit, mean=1/lambda, sd=(1/lambda/sqrt(sampleSize)))
lines(xfit, yfit, pch=22, col="red", lty=2)

# Add legend to the chart
legend('topright', c("Simulated", "Theoretical"), lty=c(1,2), col=c("black", "red"))

# Use Q-Q plot to test for Normality and Shapiro Test to indicate that the curve just 
# approximates normal distribution
qqnorm(row_means)
qqline(row_means)

# Use Shapiro Test to indicate that the curve just an approximation of normal distribution
shapiro.test(row_means)

# Calculation of coverage of the confidence interval for 1/lambda
lambda_vals <- seq(4, 6, by=0.05)
coverage <- sapply(lambda_vals, function(lamb) {
    means <- rowMeans(matrix(data = rexp(sampleSize*simulations, rate=lambda),
                               nrow = simulations, ncol = sampleSize))
    ll <- means - qnorm(0.975) * sqrt(1/lambda**2/sampleSize)
    ul <- means + qnorm(0.975) * sqrt(1/lambda**2/sampleSize)
    mean(ll < lamb & ul > lamb)
})

qplot(lambda_vals, coverage, xlab = "Values of 1/Lambda"
      , ylab = "Coverage", geom = "line") + geom_hline(yintercept=0.95)