### Robokelp 27 April 2015

##############################################################################
## Temperature and light
#############################################################################
library(lubridate); library(ggplot2)

LT_headers <- c("no", "date_time", "temp", "light", "null")
A1_LT <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Temp_A1_L9786169_RSA_Robokelp_Apr15_0.csv", header = FALSE, skip = 2, col.names = LT_headers)
A2_LT <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Temp_A2_L1071221_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)
# A3_LT_temp <- read.csv("~/ajsmit/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Temp_A3_L9910315_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2) # the formatting of this file is odd...
# A3_LT <- A3_LT_temp[1:length(A1_LT$no), 1:5]
# colnames(A3_LT) <- LT_headers
A4_LT <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Temp_A4_L9775645_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)

B1_LT <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Temp_B1_L1079716_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)
B2_LT <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Temp_B2_L2437071_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)
B3_LT <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Temp_B3_L9775642_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)
B4_LT <- read.csv("~/Dropbox/Robokelp/Robokelp_logger_rawdata/Robokelp1_270415/Temp_B4_L9910332_RSA_Robokelp_Apr15.csv", header = FALSE, skip = 2, col.names = LT_headers)

AB_LT <- rbind(A1_LT, A2_LT, A4_LT, B1_LT, B2_LT, B3_LT, B4_LT)

AB_LT$date_time <- parse_date_time(AB_LT$date_time, "mdy IMS p", tz = "Australia/Perth") # parse the time so that SA time is given...

AB_LT$ind <- c(rep("A", sum(length(A1_LT$no), length(A2_LT$no),
               		 length(A4_LT$no))),
               rep("B",sum(length(B1_LT$no), length(B2_LT$no),
                   length(B3_LT$no), length(B4_LT$no))))

AB_LT$ver <- c(rep(1, length(A1_LT$no)),
               rep(2, length(A2_LT$no)),
               rep(4, length(A4_LT$no)),
               rep(1, length(B1_LT$no)),
               rep(2, length(B2_LT$no)),
               rep(3, length(B3_LT$no)),
               rep(4, length(B4_LT$no)))

remove(A1_LT, A2_LT, A4_LT, B1_LT, B2_LT, B3_LT, B4_LT)

AB_LT.plot <- ggplot(AB_LT, aes(x=date_time, y=temp, group = ver)) +
geom_line(aes(colour = as.factor(ver)), size = 0.2) +
facet_grid(ind ~ .) +
	xlab(expression(paste("Date/Time"))) +
	ylab(expression(paste("Temperature")))
AB_LT.plot
