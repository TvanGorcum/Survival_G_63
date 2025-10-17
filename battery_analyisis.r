library(survival)
library(ggplot2)
library(muhaz)
data <- read.csv("DirtSlurper3100_preprocessed.csv")
data <- data[data$Survival.time != 0, ]
data$avg.usage.time <- data$Total.usage.time / data$Survival.time


length(data)

# Create a Surv object for battery analysis
battery <- Surv(data$Survival.time, data$Battery.censoring)
battery

# Fit and plot Kaplan-Meier survival curve
fit_battery <- survfit(battery ~ 1, type="kaplan-meier")
head(summary(fit_battery))
plot(fit_battery, xlab = "Days", ylab = "Survival Probability", main = "Battery Survival Curve")

quantile(fit_battery, probs=0.1)

# It becomes clear that L_10 is 928 days, with a lower confidence bound of 901  days and and upper confidence bound of 984
# Therefore, L_10 is less than 1000 days with 95% confidence, which negates the manfacturer's claim of 1000 days
# We'll now investigate whether some batches were faulty by examining the survival curves for different years of registration
fit_battery_year <- survfit(battery ~ data$Registration.year, type="kaplan-meier", conf.int=0.95)

plot(fit_battery_year, xlab = "Days", ylab = "Survival Probability", main = "Battery Survival Curve by Year of Registration", col=1:5)
legend("topright", legend=levels(as.factor(data$Registration.year)), col=1:5, lty=1)


# We want to see if usage intensity affects battery life, so we'll create a new Surv object based on total usage time in hours
battery_by_hours <- Surv(data$Total.usage.time, data$Battery.censoring)
fit_battery_hours <- survfit(battery_by_hours ~ 1, type="kaplan-meier")
plot(fit_battery_hours, xlab = "Hours", ylab = "Survival Probability", main = "Battery Survival Curve by Total Usage Time")



# We will now perform a Cox regression to see if other factors affect the battery life
# Please note that this analysis is only valid under the proportional hazards assumption, which will need to be tested

# First, we'll need to test whether we can treat carpet score as a linear variable, since it is ordinal data
# If it can be treated as linear, we can include it in the Cox regression as a continuous variable
# Otherwise, we'll have to treat it as a categorical variable (i.e. one-hot encode it)
cox_battery <- coxph(battery ~ log(avg.usage.time) + Pets + Carpet.score, data=data)
coxzph <- cox.zph(cox_battery)
coxzph
residuals <- residuals(cox_battery, type="martingale")
data$martingale_residuals <- residuals

png("martingale_residuals_vs_carpet_score.png", width=800, height=600)
ggplot(data = data, mapping = aes(x = Carpet.score, y = martingale_residuals)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(title = "Martingale Residuals vs. Carpet Score",
         x = "Carpet Score",
         y = "Martingale Residuals") +
    theme_bw()
dev.off()

# We treat carpet score as a linear variable
# We need to test the proportional hazards assumption for the Cox regression
ph_test <- cox.zph(cox_battery)
print(ph_test)

# Proportional hazards assumption does not hold for total usage time and pet ownership covariates
# Pet ownership is a dummy variable (0/1), so log-transforming does not make sense. 
# We'll move on to creating an accelerated lifetime model
# First plot the empirical hazard function to investigate what model would be a good pick for the baseline survival function
muhaz_battery <- muhaz(data$Survival.time, data$Battery.censoring)
png("muhaz_battery.png", width=800, height=600)
plot(muhaz_battery, xlab="Days", ylab="Hazard Rate",)
title(main="Empirical Hazard Function for Battery Survival Time")
dev.off()

# Empirical hazard function shows a linear plot, indicating the Weibull distribution is a good choice for S_0


alt_weib <- survreg(battery ~ avg.usage.time + Pets + Carpet.score, data = data, dist = "weibull")

summ_alt_weib <- summary(alt_weib)
print(summ_alt_weib)

