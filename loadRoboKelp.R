#############################################################################
## This script does:
# 1. Reads in ALL raw cyborg data;
# 2. Add POSIXct time stamps;
# 3. Match up data by site/ kelp and time;
# 4. Check for duplicates and smooth them out;
# 5. Save files as "~/Dropbox/Robokelp/R_scripts/temperature.RData" and "~/Dropbox/Robokelp/R_scripts/accelerometer.RData"
# 6. Manually clean up any data that appear erroneous
# 7. Visualize data and save in "~/Dropbox/Robokelp/R_scripts/"
# 8. Create arrays, combine into one list and save as "~/Dropbox/Robokelp/R_scripts/roboData.RData"
#############################################################################

#############################################################################
## DEPENDS ON:
require(reshape2); require(lubridate); require(plyr); require(ggplot2); require(zoo); library(ggthemes); library(gridExtra)
#############################################################################

#############################################################################
## SET THE PATH:
setwd("~/Dropbox/Robokelp")
#############################################################################

#############################################################################
## USED BY:
# These data do not need a "prep" script as the raw data come from the same source
# So the workflow jumps from here to a "proc" script
#############################################################################

#############################################################################
## CREATES:
#"~/Dropbox/Robokelp/R_scripts/temperature.RData"
#############################################################################

#############################################################################
## To Do:
# This code will work for n sites but it could be improved
# Also, it would be nice to keep the x,y,z accel data together in one matrix
  # I need to figure out how to do this, current attempts have not succeeded
# The time stamps are converted back to chr when turned into an array
  # This needs to be corrected or prevented
# The "proc" script will need to somehow decipher these data
  # Need to figure out how to extract data from arrays effectively
#############################################################################

# dir <- getwd() # However long your pathway is, the last directory must be "/tempssa_v3.0"

################################################################################
## Load in ALL raw cyborg data
# Select folder with raw data
# Do NOT need separate folders for different data types
# Must set your working directory so that it is immediately above "~/Dropbox/"
directory <- paste(getwd(), "/Robokelp_logger_rawdata/Robokelp1_270415",
                   sep = "")
temp_files <- dir(directory, full.names = FALSE, pattern = "*.*Temp*.*.csv")
tilt_files <- dir(directory, full.names = FALSE, pattern = "*.*Tilt*.*.csv")

# Load temp/ lux data
temperature <- data.frame()
for (i in 1:length(temp_files)) {
  x <- read.csv(paste(directory, "/", temp_files[i], sep = ""), header = TRUE, skip = 1)
  x <- x[,c(2:4)] # Select only relevent columns
  colnames(x) <- c("date", "temp", "lux") # Rename columns if necessary
  fileBase1 <- strsplit(temp_files[i], "[_]") # Split character vectors wherever there is a "_"
  fileBase1.1 <- sapply(fileBase1, "[[", 4) # Selects the fourth split, site name
  fileBase1.2 <- sapply(fileBase1, "[[", 2) # Selects the second split, kelp name and depth
  fileBase2 <- strsplit(fileBase1.2, "") # Split character vector into individual letters
  fileBase2.1 <- sapply(fileBase2, "[[", 1) # Selects the first split, kelp individual
  fileBase2.2 <- sapply(fileBase2, "[[", 2) # Selects the second split, depth position
  x$site <- fileBase1.1
  x$ind <- fileBase2.1
  x$ver <- fileBase2.2
  x$date <- parse_date_time(x$date, "mdy IMS p", tz = "Australia/Perth")
  x <- x[, c(4:6, 1:3)]
  temperature <- rbind(temperature, x)
}
rm(x); rm(i); rm(fileBase1); rm(fileBase1.1); rm(fileBase1.2); rm(fileBase2); rm(fileBase2.1); rm(fileBase2.2)

# This smoothes out duplicates but is very slow...
temperature <- ddply(temperature, .(site, ind, ver, date), summarize,
             temp = round(mean(temp, na.rm = TRUE), 3),
             lux = round(mean(lux, na.rm = TRUE), 1))
save(temperature, file = paste(getwd(), "/R_scripts/temperature.RData",
     sep = ""))

# Load accelerometer data
accelerometer <- data.frame()
for (i in 1:length(tilt_files)) {
  x <- read.csv(paste(directory, "/", tilt_files[i], sep = ""),
                header = TRUE, skip = 1)
  x <- x[,c(2:5)] # Select only relevent columns
  colnames(x) <- c("date", "x", "y", "z") # Rename columns if necessary
  fileBase1 <- strsplit(tilt_files[i], "[_]") # Split character vectors wherever there is a "_"
  fileBase1.1 <- sapply(fileBase1, "[[", 4) # Selects the fourth split, site name
  fileBase1.2 <- sapply(fileBase1, "[[", 2) # Selects the second split, kelp name and depth
  fileBase2 <- strsplit(fileBase1.2, "") # Split character vector into individual letters
  fileBase2.1 <- sapply(fileBase2, "[[", 1) # Selects the first split, kelp individual
  fileBase2.2 <- sapply(fileBase2, "[[", 2) # Selects the second split, depth position
  x$site <- fileBase1.1
  x$ind <- fileBase2.1
  x$ver <- fileBase2.2
  x$date  <- parse_date_time(x$date, "mdy IMS p", tz = "Australia/Perth")
  x <- x[, c(5:7, 1:4)]
  accelerometer <- rbind(accelerometer, x)
}
rm(x); rm(i); rm(fileBase1); rm(fileBase1.1); rm(fileBase1.2); rm(fileBase2); rm(fileBase2.1); rm(fileBase2.2)

# This smoothes out duplicates but is very slow...
accelerometer <- ddply(accelerometer, .(site, ind, ver, date), summarize,
                     x = round(mean(x, na.rm = TRUE), 2),
                     y = round(mean(y, na.rm = TRUE), 2),
                     z = round(mean(z, na.rm = TRUE), 2))
save(accelerometer, file = paste(getwd(), "/R_scripts/accelerometer.RData",
     sep = ""))

################################################################################
## Manually clean up data
load("~/Dropbox/Robokelp/R_scripts/temperature.RData")
load("~/Dropbox/Robokelp/R_scripts/accelerometer.RData")

temperature$date[temperature$date >= parse_date_time("4/26/15 07:12:10 PM", "mdy IMS p", tz = "Australia/Perth")] <- NA
temperature <- temperature[complete.cases(temperature), ]
#temperature$index <- paste(temperature$ind, temperature$ver, sep = "") # This can be used to remove one entire data file

################################################################################
## Visualize data and save
# Load data
temperature <- droplevels(subset(temperature, !(ver %in% "E"))) # Manually remove REF files before plotting
accelerometer <- droplevels(subset(accelerometer, !(ver %in% "E"))) # Manually remove REF files before plotting

source("/Users/ajsmit/Dropbox/R/ggplot2_themes/themes.R")

# Plot of temperature for all sites
temp.plot <- ggplot(temperature, aes(x = date, y = temp, group = ver)) +
        geom_line(aes(colour = as.factor(ver), alpha = 0.5), size = 0.2) +
        facet_grid(ind ~ .) +
        guides(colour = guide_legend(title = "Vertical Position"), alpha = "none") + # Insert guide and label it
        xlab(expression(paste("Date/Time"))) +
        ylab(expression(paste("Temperature"))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))
temp.plot
ggsave("~/Dropbox/Robokelp/R_scripts/temp.plot.pdf")

# Plot of lux for all sites
lux.plot <- ggplot(temperature, aes(x = date, y = lux, group = ver)) +
        geom_line(aes(colour = as.factor(ver), alpha = 0.5), size = 0.2) +
        facet_grid(ind ~ .) +
        guides(colour = guide_legend(title = "Vertical Position"), alpha = "none") + # Insert guide and label it
        xlab(expression(paste("Date/Time"))) +
        ylab(expression(paste("Temperature"))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))
lux.plot
ggsave("~/Dropbox/Robokelp/R_scripts/lux.plot.pdf")

# Plot of X accel for all sites
x.plot <- ggplot(accelerometer, aes(x = date, y = x, group = ver)) +
        geom_line(aes(colour = as.factor(ver), alpha = 0.5), size = 0.2) +
        facet_grid(ind ~ .) +
        guides(colour = guide_legend(title = "Vertical Position"), alpha = "none") + # Insert guide and label it
        xlab(expression(paste("Date/Time"))) +
        ylab(expression(paste("X Accel"))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))
x.plot
ggsave("~/Dropbox/Robokelp/R_scripts/x.plot.pdf") # B1 appears to be malfunctioning. Stuck at 3.15

# Plot of Y accel for all sites
y.plot <- ggplot(accelerometer, aes(x = date, y = y, group = ver)) +
        geom_line(aes(colour = as.factor(ver), alpha = 0.5), size = 0.2) +
        facet_grid(ind ~ .) +
        guides(colour = guide_legend(title = "Vertical Position"), alpha = "none") + # Insert guide and label it
        xlab(expression(paste("Date/Time"))) +
        ylab(expression(paste("Y Accel"))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))
y.plot
ggsave("~/Dropbox/Robokelp/R_scripts/y.plot.pdf")

# Plot of Z accel for all sites
z.plot <- ggplot(accelerometer, aes(x = date, y = z, group = ver)) +
        geom_line(aes(colour = as.factor(ver), alpha = 0.5), size = 0.2) +
        facet_grid(ind ~ .) +
        guides(colour = guide_legend(title = "Vertical Position"), alpha = "none") + # Insert guide and label it
        xlab(expression(paste("Date/Time"))) +
        ylab(expression(paste("Z Accel"))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))
z.plot
ggsave("~/Dropbox/Robokelp/R_scripts/z.plot.pdf")

################################################################################
## Create arrays for different data types

# Temperature array
temp.array <- acast(temperature, ver ~ ind ~ date, value.var = "temp", mean)
#temp.array[,,1] # Test it is working

# Luminosity array
lux.array <- acast(temperature, ver ~ ind ~ date, value.var = "lux", mean)
#lux.array[,,1] # Test it is working

# Accelerometer arrays
accel.x.array <- acast(accelerometer, ver ~ ind ~ date, value.var = "x")
accel.y.array <- acast(accelerometer, ver ~ ind ~ date, value.var = "y")
accel.z.array <- acast(accelerometer, ver ~ ind ~ date, value.var = "z")
#accel.z.array[,,1] # Test it is working

## Combine arrays into one list and save
roboData <- list(temp.array, lux.array, accel.x.array, accel.y.array, accel.z.array)
#str(roboData)
save(roboData, file = "~/Dropbox/Robokelp/R_scripts/roboData.RData") # Too complex to be saved as .csv
#load("~/Dropbox/Robokelp/R_scripts/roboData.RData") # test

################################################################################
# Clean Up...
rm(accelerometer); rm(temperature); rm(accel.x.array); rm(accel.y.array); rm(accel.z.array)
rm(lux.array); rm(temp.array); rm(roboData); rm(tilt_files); rm(temp_files); rm(directory)
rm(lux.plot); rm(temp.plot); rm(x.plot); rm(y.plot); rm(z.plot)
