library(asaur)
library(survival)
library(survminer)
library(flexsurv)
library(muhaz)

# Load the data
data <- read.csv("data/DirtSlurper3100_preprocessed.csv")

# Remove items with survivaltime 0
data <- data[data$Survival.time != 0, ]

# Calculate average number of usage hours per day
data$avg.usage.time <- data$Total.usage.time / data$Survival.time

#
#
#
# IR model
#
#
#

# Create a survival object and fit a Cox model

surv_obj_ir <- Surv(time = data$Survival.time, event = data$IR.censoring)
cox_model_ir <- coxph(surv_obj_ir ~ avg.usage.time + Pets + Carpet.score, data = data)
summary(cox_model_ir)
cox_model_ir$score

# Check if the Proportional hazards assumption holds

ph_test <- cox.zph(cox_model_ir)
ph_test

# See how beta changes for total usage time troughout the survival time
# plot(ph_test, var = "Total.usage.time")

# Fit Kaplan-Meier
km_fit_ir <- survfit(surv_obj_ir ~ 1, data = data)

# Plot Kaplan Meier
png("images/IR_Survival.png", width = 700, height = 450)

plot(km_fit_ir,
     xlab = "Days since registration",
     ylab = "Survival Probability",
     main = "Kaplan–Meier Curve of IR Sensor Survival Probability",
     lwd = 1,
     yaxt = "n",          # suppress default y-axis
     xaxt = "n")          # suppress default x-axis

# Add horizontal grid lines
abline(h = seq(0, 1, by = 0.1), col = "gray80", lty = "dotted")

# Custom y-axis labels
axis(side = 2, at = seq(0, 1, by = 0.1), las = 1, cex.axis = 1.2)

# Add vertical grid lines and ticks at every 200 days
abline(v = seq(0, max(km_fit_ir$time), by = 200), col = "gray90", lty = "dotted")
axis(side = 1, at = seq(0, max(km_fit_ir$time), by = 200), cex.axis = 1.2)

dev.off()


# Evaluate if the manufacturers specification holds

max(data$Survival.time)
summary(km_fit_ir, times = 1825, extend = TRUE)
summary(km_fit_ir, times = 1900, extend = TRUE)

# Impossible to do with non-parametric model,
# therefore, we need to fit a parametric model.


# Kaplan-Meier
km_fit <- survfit(Surv(Survival.time, IR.censoring) ~ 1, data = data)

# Get cumulative hazard from the Kaplan-Meier (Which is the Nelson-Aalen)
H_km <- -log(km_fit$surv)

H_km

# Plot the Nelson-Aalen cumulative hazard
png("images/IR_nelsonaalen.png", width = 700, height = 450)
plot(km_fit$time, H_km, type="s", lwd=2, col="steelblue",
     xlab="Days since registration",
     ylab="Cumulative Hazard",
     main="Cumulative Hazard (Nelson–Aalen)",
     #yaxt = "n",         
     xaxt = "n",
     cex.axis=1.2)

# Also plot a straight line trough origin and the cumulative hazard at 
# the longest known days since registration.

slope_ref <- max(H_km) / max(km_fit$time)
abline(a = 0, b = slope_ref, col="black", lwd=2, lty=2)
axis(side = 1, at = seq(0, max(km_fit_ir$time), by = 200), cex.axis = 1.2)
dev.off()


# The Nelson-Aalen cumulative hazard closely follows straight line,
# Therefore, Fit exponential model
exp_fit <- flexsurvreg(Surv(Survival.time, IR.censoring) ~ 1,
                       data = data,
                       dist = "exp")

summary(exp_fit)

# Get survival estimate at t = 2000
exp_summary <- summary(exp_fit, t = 2000, type = "survival", ci = TRUE)

# Get point estimate and CI
S_2000 <- exp_summary[[1]]$est
S_2000_lcl <- exp_summary[[1]]$lcl
S_2000_ucl <- exp_summary[[1]]$ucl

S_2000
S_2000_lcl
S_2000_ucl

# Get estimate of L10 and CI
q <- summary(exp_fit, type = "quantile", quantiles = 0.1, ci = TRUE)

L10_est <- q[[1]]$est
L10_lcl <- q[[1]]$lcl
L10_ucl <- q[[1]]$ucl

L10_est; L10_lcl; L10_ucl
