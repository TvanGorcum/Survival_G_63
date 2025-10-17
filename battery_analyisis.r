library(survival)
data <- read.csv("DirtSlurper3100_preprocessed.csv")
head(data)

# Create a Surv object for battery analysis
battery <- Surv(data$Survival.time, data$Battery.censoring)
battery

# Fit and plot Kaplan-Meier survival curve
fit_battery <- survfit(battery ~ 1, type="kaplan-meier")
head(summary(fit_battery))
plot(fit_battery, xlab = "Days", ylab = "Survival Probability", main = "Battery Survival Curve")

quantile(fit_battery, probs=0.1)

# It becomes clear that L_10 is 928 days, with a lower confidence bound of 928 days and and upper confidence bound of 984
# Therefore, L_10 is less than 1000 days with 95% confidence, which negates the manfacturer's claim of 1000 days
# We'll now investigate whether some batches were faulty by examining the survival curves for different years of registration
fit_battery_year <- survfit(battery ~ data$Registration.year, type="kaplan-meier", conf.int=0.95)

plot(fit_battery_year, xlab = "Days", ylab = "Survival Probability", main = "Battery Survival Curve by Year of Registration", col=1:5)
legend("topright", legend=levels(as.factor(data$Registration.year)), col=1:5, lty=1)


# We want to see if usage intensity affects battery life, so we'll create a new Surv object based on total usage time in hours
battery_by_hours <- Surv(data$Total.usage.time, data$Battery.censoring)
fit_battery_hours <- survfit(battery_by_hours ~ 1, type="kaplan-meier")
plot(fit_battery_hours, xlab = "Hours", ylab = "Survival Probability", main = "Battery Survival Curve by Total Usage Time")
