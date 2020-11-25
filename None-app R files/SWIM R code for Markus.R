# install.packages("SWIM", dependencies = TRUE)
library(SWIM)
data("credit_data")
head(credit_data)


# stress VaR of aggregate loss "L"
# increase VaR(90%) by 20%

#this is portfolio VaR of the data
quantile(credit_data[, "L"], probs = 0.9, type = 1)

#create a SWIM object with 1.2 stress on the VaR
stress.VaR <- stress(type = "VaR",
                      x = credit_data,
                      k = "L",
                      alpha = 0.9,
                      q_ratio = 1.2)

# other possibilities:
# type = VaR, VaR ES, mean, mean sd, moment, prob, user


# cdf plot of stressed distribution against base
plot_cdf(object = stress.VaR, xCol = "L", base = TRUE)
plot_cdf(object = stress.VaR, xCol = "L1", base = TRUE)
plot_cdf(object = stress.VaR, xCol = "L2", base = TRUE)
plot_cdf(object = stress.VaR, xCol = "L3", base = TRUE)


# histogram plot of stressed distribution against base
plot_hist(object = stress.VaR, xCol = "L", base = TRUE)
plot_hist(object = stress.VaR, xCol = "L1", base = TRUE)
plot_hist(object = stress.VaR, xCol = "L2", base = TRUE)
plot_hist(object = stress.VaR, xCol = "L3", base = TRUE)


# compare basic stats stressed model against baseline
summary(stress.VaR, xCol = c("L", "L1", "L2", "L3"), base = TRUE)


# extract weights
w.credit <- get_weights(stress.VaR)
# only plot a subset of the scenario weights
grid <- seq(1, 100000, by = 100)
plot(credit_data[grid, 1], w.credit[grid, 1], pch = 19, xlab = "L", ylab = "scenario weights")
abline(h = 1, lty = 2)

#this is the automated way of doing the same plot (can be slow if you plot many points) 
plot_weights(stress.VaR,xCol = "L",n=1000)


# this outputs a sensitivity measure equaling
# normalized change in mean under the stressed model
sensitivity(object = stress.VaR, xCol = c("L1", "L2", "L3"), type = "Gamma")
# You can see that L2 and L3 are the most sensitive ones.

#let's now add some more stresses on to the same SWIM object, including dome downward "capital saving stresses"
stress.VaR <- stress(type = "VaR",
                     x = stress.VaR,
                     k = "L",
                     alpha = 0.9,
                     q_ratio = c(1.1, 0.9, 0.8))
#check impact on cdf of L
plot_cdf(object = stress.VaR, xCol = "L", base = TRUE)
#can also plot the VaRs
plot_quantile(object = stress.VaR, xCol = "L", base = TRUE)

#let's see how the weights differ for an upward and a downward stress
plot_weights(stress.VaR,xCol = "L",wCol = c(1,4), n=1000)
#weights are decreasing in L for a downward stress (Stress 4)

#finally check sensitivity measure for different stresses
sensitivity(object = stress.VaR, xCol = c("L1", "L2", "L3"), type = "Gamma")
#negative sensitivity values for scenarios 3, 4 mean that increases in the risk factors L1-L3 are associated with reductions int eh risk of L
#also: in stresses 1 and 2 where we increase VaR L2 is more important than L3
#however, when we try to reduce VaR in stresses 3-4, the two variables appear equally important

#do a different kind of stress now
# increase sd of latent factor H2, 6th column (random default prob of subportfolio 2), while preserving the mean
stress.latent <- stress(x = credit_data,
                          type = "mean sd",
                          k = 6,
                          new_means = mean(credit_data[, "H2"]),
                          new_sd = sd(credit_data[, "H2"]) * 1.3,
                          control = list(maxit = 1000))


# plot cdfs and hist of "L" after stressing latent variable
plot_cdf(object = stress.latent, xCol = "L", base =  TRUE)
#cdfs cross, becase of same mean, different volatility
plot_hist(object = stress.latent, xCol = "L", base = TRUE)


# We can also calculate Expected Shortfall (TVaR) of stressed portfolio loss
ES_stressed(object = stress.latent, xCol = "L", base = TRUE, alpha = 0.9)

#check what weights look like for this type of stress
plot_weights(stress.latent,xCol = "H2",n=5000)
#the weights are now a smooth function of H2, penalizing extremes exponentially


