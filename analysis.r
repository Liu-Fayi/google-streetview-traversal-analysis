normal_time <- c(193, 181, 184, 184, 228, 181, 209, 192, 216, 220, 364, 301, 206)

normal_error <- c(5, 8, 4, 8, 8, 1, 4, 4, 4, 4, 5, 6, 3)

distracted_time <- c(290, 360, 182, 212, 222, 244, 230, 202, 208, 282, 331, 225, 263)

distracted_error <- c(8, 5, 4, 6, 5, 4, 10, 5, 2, 3, 7, 6, 7)

normal_accuracy <- 1 - normal_error / 20
distracted_accuracy <- 1 - distracted_error / 20

mean_normal_time <- mean(normal_time)
mean_normal_error <- mean(normal_error)
mean_distracted_time <- mean(distracted_time)
mean_distracted_error <- mean(distracted_error)
mean_normal_accuracy <- mean(normal_accuracy)
mean_distracted_accuracy <- mean(distracted_accuracy)

sd_normal_time <- sd(normal_time)
sd_normal_error <- sd(normal_error)
sd_distracted_time <- sd(distracted_time)
sd_distracted_error <- sd(distracted_error)
sd_normal_accuracy <- sd(normal_accuracy)
sd_distracted_accuracy <- sd(distracted_accuracy)

hist(normal_time, breaks = 7)
hist(distracted_time, breaks = 7)
hist(normal_error, breaks = 7)
hist(distracted_error, breaks = 7)
hist(normal_accuracy, breaks = 7)
hist(distracted_accuracy, breaks = 7)
curve(dnorm(x, mean = mean_normal_time, sd = sd_normal_time), from = 0, to = 500)
curve(dnorm(x, mean = mean_distracted_time, sd = sd_distracted_time), from = 0, to = 500)
curve(dnorm(x, mean = mean_normal_error, sd = sd_normal_error), from = 0, to = 20)
curve(dnorm(x, mean = mean_distracted_error, sd = sd_distracted_error), from = 0, to = 20)
curve(dnorm(x, mean = mean_normal_accuracy, sd = sd_normal_accuracy))
curve(dnorm(x, mean = mean_distracted_accuracy, sd = sd_distracted_accuracy))

# t-test for time
t_test_time <- t.test(normal_time, distracted_time, mu = 0, var.equal = FALSE, alternative = "less", conf.level = 0.95)
t_test_time_p_value <- t_test_time$p.value
t_test_accuracy <- t.test(normal_accuracy, distracted_accuracy, mu = 0, var.equal = FALSE, alternative = "greater", conf.level = 0.95)
t_test_accuracy_p_value <- t_test_accuracy$p.value


# X-axis grid
x2 <- seq(min(normal_time), max(normal_time), length = 40)

# Normal curve
fun <- dnorm(x2, mean = mean_normal_time, sd = sd_normal_time)
fun2 <- dnorm(x2, mean = mean_distracted_time, sd = sd_distracted_time)

# Histogram
hist(normal_time, prob = TRUE, col = "white",
     ylim = c(0, max(fun)+0.005),
     main = "No Distraction Time Distrubution")
lines(x2, fun, col = 2, lwd = 2) 
 
x2 <- seq(min(distracted_time), max(distracted_time), length = 40)

fun <- dnorm(x2, mean = mean_normal_time, sd = sd_normal_time)
fun2 <- dnorm(x2, mean = mean_distracted_time, sd = sd_distracted_time)

# Histogram
hist(distracted_time, prob = TRUE, col = "white",
     ylim = c(0, max(fun)+0.01),
     main = "Distraction Time Distrubution")
lines(x2, fun, col = 2, lwd = 2)


x2 <- seq(min(normal_accuracy), max(normal_accuracy), length = 40)

fun <- dnorm(x2, mean = mean_normal_accuracy, sd = sd_normal_accuracy)
fun2 <- dnorm(x2, mean = mean_distracted_accuracy, sd = sd_distracted_accuracy)

# Histogram
hist(normal_accuracy, prob = TRUE, col = "white",
     ylim = c(0, max(fun)+5),
     main = "No Distraction Accuracy Distrubution")
lines(x2, fun, col = 2, lwd = 2)


x2 <- seq(min(distracted_accuracy), max(distracted_accuracy), length = 40)

fun <- dnorm(x2, mean = mean_normal_accuracy, sd = sd_normal_accuracy)
fun2 <- dnorm(x2, mean = mean_distracted_accuracy, sd = sd_distracted_accuracy)

# Histogram
hist(distracted_accuracy, prob = TRUE, col = "white",
     ylim = c(0, max(fun)+1),
     main = "Distraction Accuracy Distrubution")
lines(x2, fun, col = 2, lwd = 2)
