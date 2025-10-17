df <- read.csv("DirtSlurper3100_preprocessed.csv", check.names = FALSE)

#Summary stats
n_devices  <- nrow(df)
n_repaired <- sum(df$Sent.for.repair, na.rm = TRUE)
n_censored <- n_devices - n_repaired
pct_repaired <- 100 * n_repaired / n_devices
pct_censored <- 100 - pct_repaired

#Count how many times each of the components fail (or how many times it is sent back for repair with none of the 3 components failing)
n_battery_fail <- sum(df$Battery.censoring == 1, na.rm = TRUE)
n_impact_fail  <- sum(df$Impact.censoring  == 1, na.rm = TRUE)
n_ir_fail      <- sum(df$IR.censoring     == 1, na.rm = TRUE)
other_repairs  <- n_repaired - (n_battery_fail + n_impact_fail + n_ir_fail)

#factors which may influence the product
n_pets        <- sum(df$Pets, na.rm = TRUE)
pct_pets      <- 100 * n_pets / n_devices
median_carpet <- median(df$Carpet.score, na.rm = TRUE)
mean_usage   <- mean(df$Total.usage.time, na.rm = TRUE)
median_usage <- median(df$Total.usage.time, na.rm = TRUE)

#Generate table used in the report
summary_table <- data.frame(
  Metric = c(
    "Total devices",
    "Devices sent for repair",
    "Devices not sent for repair",
    "Battery failures",
    "Impact sensor failures",
    "IR sensor failures",
    "Devices repaired with none of the 3 failures",
    "Households with pets",
    "Median carpet score",
    "Mean total usage time (hours)",
    "Median total usage time (hours)"
  ),
  Value = c(
    n_devices,
    sprintf("%d (%.1f%%)", n_repaired,  pct_repaired),
    sprintf("%d (%.1f%%)", n_censored, pct_censored),
    n_battery_fail,
    n_impact_fail,
    n_ir_fail,
    other_repairs,
    sprintf("%d (%.1f%%)", n_pets, pct_pets),
    sprintf("%.2f", mean_carpet),
    sprintf("%.0f", median_carpet),
    sprintf("%.1f", mean_usage),
    sprintf("%.1f", median_usage)
  ),
)

summary_table

top_usage <- df[order(-df$Total.usage.time), ]

# Show the first 100 rows
head(top_usage, 100)

head(sort(df$Total.usage.time, decreasing = TRUE), 100)
