# install.packages("lubridate")
library(lubridate)

data <- read.csv("DirtSlurper3100.csv", skip = 12)

# Change the format of the columns so working with it is easier
data$Registration.date
data$Pets <- ifelse(data$Pets == "YES", 1, 0)
data$Sent.for.repair <- ifelse(data$Sent.for.repair == "YES", 1, 0)
data$Failure.date[data$Failure.date == "---"] <- NA
data$Battery.status[data$Battery.status == "---"] <- NA
data$Impact.status[data$Impact.status == "---"] <- NA
data$IR.status[data$IR.status == "---"] <- NA


# Create a censoring column for each of the 3 parts
data$Battery.censoring <- ifelse(!is.na(data$Battery.status) & data$Battery.status == "Damage", 1, 0)
data$Impact.censoring <- ifelse(!is.na(data$Impact.status) & data$Impact.status == "Damage", 1, 0)
data$IR.censoring <- ifelse(!is.na(data$IR.status) & data$IR.status == "Damage", 1, 0)

# Turn the registration and failure date into a proper date column
data$Registration.date <- as.character(data$Registration.date)
data$Registration.date <- trimws(data$Registration.date)
data$Registration.date <- gsub("[^[:alnum:]]", "", data$Registration.date)
data$Registration.date <- dmy(data$Registration.date)

data$Failure.date <- as.character(data$Failure.date)
data$Failure.date <- trimws(data$Failure.date)
data$Failure.date[data$Failure.date == "---"] <- NA
data$Failure.date <- dmy(data$Failure.date)

# Replace NAs in Failure.date with 31 Dec 2019
data$Failure.date[is.na(data$Failure.date)] <- as.Date("2019-12-31")

# Create a new column with the survival time of each unit, year of registration date, and year and month of registration date
data$Survival.time <- as.numeric(data$Failure.date - data$Registration.date)
data$Registration.year <- format(as.Date(data$Registration.date), "%Y")
data$Registration.year.month <- format(as.Date(data$Registration.date), "%Y-%m")

#remove products with survival time 0, these all have registration day on the last day of the trial so they dont matter
data <- data[data$Survival.time != 0, ]


head(data)

write.csv(data, "DirtSlurper3100_preprocessed.csv", row.names = FALSE)
