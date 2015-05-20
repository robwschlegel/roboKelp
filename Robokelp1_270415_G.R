### Robokelp 27 April 2015

##############################################################################
## Temperature and light
#############################################################################
library(lubridate); library(ggplot2)

LT_headers <- c("no", "date_time", "x", "y", "z", "null")
A1_G <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Tilt_A1_G2395084_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)
A2_G <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Tilt_A2_G2370935_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)
A3_G <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Tilt_A3_G9903339_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)

B1_G <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Tilt_B1_G2427311_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)
B2_G <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Tilt_B2_G10091230_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)
B3_G <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Tilt_B3_G2370931_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)

AB_G <- rbind(A1_G, A2_G, A3_G, B1_G, B2_G, B3_G)

AB_G$date_time <- parse_date_time(AB_G$date_time, "mdy IMS p", tz = "Australia/Perth") # parse the time so that SA time is given...

AB_G$ind <- c(rep("A", sum(length(A1_G$no), length(A2_G$no),
               		 length(A3_G$no))),
               rep("B",sum(length(B1_G$no), length(B2_G$no),
                   length(B3_G$no))))

AB_G$ver <- c(rep(1, length(A1_G$no)),
               rep(2, length(A2_G$no)),
               rep(3, length(A3_G$no)),
               rep(1, length(B1_G$no)),
               rep(2, length(B2_G$no)),
               rep(3, length(B3_G$no)))

remove(A1_G, A2_G, A3_G, B1_G, B2_G, B3_G)

AB_G.plot <- ggplot(AB_G, aes(x=date_time, y=z, group = ver)) +
geom_line(aes(colour = as.factor(ver)), size = 0.2, alpha = 0.4) +
facet_grid(ind ~ .) +
	xlab(expression(paste("Date/Time"))) +
	ylab(expression(paste("z")))
AB_G.plot

AB_G.plot <- ggplot(AB_G, aes(x=date_time, y=x, group = ver)) +
geom_line(aes(colour = as.factor(ver)), size = 0.2, alpha = 0.4) +
facet_grid(ind ~ .) +
  xlab(expression(paste("Date/Time"))) +
  ylab(expression(paste("x")))
AB_G.plot

AB_G.plot <- ggplot(AB_G, aes(x=date_time, y=y, group = ver)) +
geom_line(aes(colour = as.factor(ver)), size = 0.2, alpha = 0.4) +
facet_grid(ind ~ .) +
  xlab(expression(paste("Date/Time"))) +
  ylab(expression(paste("y")))
AB_G.plot