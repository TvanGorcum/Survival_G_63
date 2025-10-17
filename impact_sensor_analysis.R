library(survival)

df <- read.csv("DirtSlurper3100_preprocessed.csv")

impact_data <- df
impact_data$avg.usage.time <- impact_data$Total.usage.time / impact_data$Survival.time
surv_obj_impact <- Surv(time = impact_data$Survival.time, event = impact_data$Impact.censoring)

# Fit a Cox model with avg.usage.time
cox_model_impact <- coxph(surv_obj_impact ~ avg.usage.time + Pets + Carpet.score, data = impact_data)
print(summary(cox_model_impact))

# Check if the Proportional hazards assumption holds
ph_test <- cox.zph(cox_model_impact)
print("Proportional Hazards Test Results:")
print(ph_test)

# beta changes for avg usage time throughout the survival time
png("plots/impact_ph_test_total_usage.png", width = 800, height = 600)
plot(ph_test, var = "avg.usage.time")
dev.off()

#Exp fit
lambda <- exp(-exp_fit$coefficients[1])
mttf_exp <- 1 / lambda
L10_life_exp <- -log(0.9) / lambda

print(sprintf("Rate parameter (λ): %0.6f failures per day", lambda))
print(sprintf("Mean Time To Failure (MTTF): %d days", round(mttf_exp)))
print(sprintf("L10 Life: %d days", round(L10_life_exp)))


# Plot exponential fit
png("plots/impact_survival_fit_exponential.png", width = 800, height = 600)
km_fit_exp <- survfit(surv_obj_impact ~ 1)
plot(km_fit_exp, conf.int = FALSE,
     xlab = "Days since registration",
     ylab = "Survival Probability",
     main = "Impact Sensor: Kaplan-Meier Curve vs. Fitted Exponential Model")

time_points_exp <- seq(0, max(impact_data$Survival.time), length.out = 200)
exp_surv <- exp(-lambda * time_points_exp)
lines(time_points_exp, exp_surv, col = "blue", lwd = 2)
legend("bottomright",
       legend = c("Kaplan-Meier (Empirical)", "Fitted Exponential (Parametric)"),
       col = c("black", "blue"), lty = 1, lwd = c(1, 2))
dev.off()

# QQ plot for exponential
png("plots/impact_qq_plot_exponential.png", width = 800, height = 600)
valid_idx_exp <- km_fit_exp$surv < 1
y_qq_exp <- km_fit_exp$time[valid_idx_exp]
x_qq_exp <- -log(km_fit_exp$surv[valid_idx_exp])

plot(x_qq_exp, y_qq_exp,
     xlab = "Theoretical Quantiles: -log(S(t))",
     ylab = "Empirical Quantiles: t",
     main = "Exponential Probability Plot for Impact Sensor")
grid()
abline(a = 0, b = 1/lambda, col = "blue", lwd = 2)
legend("topleft", legend = "Fitted Exponential Line", col = "blue", lty = 1, lwd = 2)
dev.off()

# Hazard plot for exponential (constant hazard)
png("plots/impact_hazard_plot_exponential.png", width = 800, height = 600)
plot(c(0, max(impact_data$Survival.time)), c(lambda, lambda),
     type = "l",
     xlab = "Days since registration",
     ylab = "Hazard Rate (Instantaneous Failure Rate)",
     main = "Impact Sensor Hazard Function (Fitted Exponential)",
     col = "blue", lwd = 2,
     ylim = c(0, lambda * 1.2))
grid()
dev.off()

print(sprintf("Exponential AIC: %.2f", AIC(exp_fit)))
print(sprintf("Weibull AIC: %.2f", AIC(weibull_fit)))

#Weibull params
beta <- 1 / weibull_fit$scale
eta <- exp(weibull_fit$coefficients[1])
mttf <- eta * gamma(1 + 1 / beta)
L10_life <- eta * (-log(0.9))^(1 / beta)

print(sprintf("Failure Profile: Weibull with β = %0.4f", beta))
print(sprintf("Mean Time To Failure (MTTF): %d days", round(mttf)))
print(sprintf("B10 Life: %d days", round(L10_life)))

png("plots/impact_survival_fit.png", width = 800, height = 600)
km_fit <- survfit(surv_obj_impact ~ 1)
plot(km_fit, conf.int = FALSE,
     xlab = "Days since registration",
     ylab = "Survival Probability",
     main = "Impact Sensor: Kaplan-Meier Curve vs. Fitted Weibull Model")

time_points <- seq(0, max(impact_data$Survival.time), length.out = 200)
weibull_surv <- 1 - pweibull(time_points, shape = beta, scale = eta)
lines(time_points, weibull_surv, col = "red", lwd = 2)
legend("bottomright",
       legend = c("Kaplan-Meier (Empirical)", "Fitted Weibull (Parametric)"),
       col = c("black", "red"), lty = 1, lwd = c(1, 2))
dev.off()

png("plots/impact_qq_plot.png", width = 800, height = 600)

# Filter out zero times only for log operations to avoid log(0) errors
valid_idx <- km_fit$surv < 1 & km_fit$time > 0
y_qq <- log(km_fit$time[valid_idx])
x_qq <- log(-log(km_fit$surv[valid_idx]))

plot(x_qq, y_qq,
     xlab = "Theoretical Quantiles: log(-log(S(t)))",
     ylab = "Empirical Quantiles: log(t)",
     main = "Weibull Probability Plot for Impact Sensor")
grid()
abline(a = log(eta), b = 1/beta, col = "red", lwd = 2)
legend("topleft", legend = "Fitted Weibull Line", col = "red", lty = 1, lwd = 2)
dev.off()

png("plots/impact_hazard_plot.png", width = 800, height = 600)
weibull_hazard <- function(t, shape, scale) {
  (shape / scale) * (t / scale)^(shape - 1)
}
curve(weibull_hazard(x, shape = beta, scale = eta),
      from = 0, to = max(impact_data$Survival.time),
      xlab = "Days since registration",
      ylab = "Hazard Rate (Instantaneous Failure Rate)",
      main = "Impact Sensor Hazard Function (Fitted Weibull)",
      col = "blue", lwd = 2)
grid()
dev.off()
