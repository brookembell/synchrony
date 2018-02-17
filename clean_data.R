# This code imports the raw bite files and cleans the data
# Updated: 2-16-18
# Author: Brooke Bell

# IMPORT RAW DATA -----

rm(list=ls())
library(plyr)
library(dplyr)
library(gmodels)

# set working directory
setwd("~/Dropbox (Metzlab - VS)/Metz Lab/CURRENT PROJECTS/M2FED/Bite Annotations/Cleaned data")

# import data from csv files
for(i in c("01", "07", "09", "13a", "13b", "15", "17")) {
  fam <- paste("fam", i, sep = "")
  assign(fam, read.csv(paste("fam", i, "_bites.csv", sep = ""), na = "<NA>"))
}

# REFORMAT -----

# function for changing time format from hms to seconds
hms_to_sec <- function(x){
  strtoi(as.difftime(as.character(x), format = "%H:%M:%S", units = "secs"))
}

# reformat time to seconds

famList <- as.list(paste("fam", c("01", "07", "09", "13a", "13b", "15", "17"), sep = ""))

fam01$bite_sec <- hms_to_sec(fam01$BiteTime_hms)
fam01$bite_sec_cent <- fam01$bite_sec - min(fam01$bite_sec)

fam07$bite_sec <- hms_to_sec(fam07$BiteTime_hms)
fam07$bite_sec_cent <- fam07$bite_sec - min(fam07$bite_sec)

fam09$bite_sec <- hms_to_sec(fam09$BiteTime_hms)
fam09$bite_sec_cent <- fam09$bite_sec - min(fam09$bite_sec)

fam13a$bite_sec <- hms_to_sec(fam13a$BiteTime_hms)
fam13a$bite_sec_cent <- fam13a$bite_sec - min(fam13a$bite_sec)

fam13b$bite_sec <- hms_to_sec(fam13b$BiteTime_hms)
fam13b$bite_sec_cent <- fam13b$bite_sec - min(fam13b$bite_sec)

fam15$bite_sec <- hms_to_sec(fam15$BiteTime_hms)
fam15$bite_sec_cent <- fam15$bite_sec - min(fam15$bite_sec)

fam17$bite_sec <- hms_to_sec(fam17$BiteTime_hms)
fam17$bite_sec_cent <- fam17$bite_sec - min(fam17$bite_sec)

# LOOK AT BITES ONLY AT TABLE -----
# create dataframe to store start/end times
my_times <- data.frame(rbind(c("fam01", "0:18:34", "0:31:46"),
                             c("fam07", "0:29:46", "0:46:43"),
                             c("fam09", "0:23:58", "1:02:04"),
                             c("fam13a", "0:25:40", "0:37:34"),
                             c("fam13b", "0:13:22", "0:17:12"),
                             c("fam15", "0:27:44", "0:55:13"),
                             c("fam17", "0:17:13", "0:36:56")))
colnames(my_times) <- c("famid", "starttime", "endtime")

# transform times to seconds format
my_times$starttime_sec <- lapply(my_times$starttime, hms_to_sec)
my_times$endtime_sec <- lapply(my_times$endtime, hms_to_sec)

# subset for bites only take at the table

fam01_at_table <- subset(fam01, bite_sec >= as.integer(my_times$starttime_sec[1]) & bite_sec <= as.integer(my_times$endtime_sec[1]))
fam07_at_table <- subset(fam07, bite_sec >= as.integer(my_times$starttime_sec[2]) & bite_sec <= as.integer(my_times$endtime_sec[2]))
fam09_at_table <- subset(fam09, bite_sec >= as.integer(my_times$starttime_sec[3]) & bite_sec <= as.integer(my_times$endtime_sec[3]))
fam13a_at_table <- subset(fam13a, bite_sec >= as.integer(my_times$starttime_sec[4]) & bite_sec <= as.integer(my_times$endtime_sec[4]))
fam13b_at_table <- subset(fam13b, bite_sec >= as.integer(my_times$starttime_sec[5]) & bite_sec <= as.integer(my_times$endtime_sec[5]))
fam15_at_table <- subset(fam15, bite_sec >= as.integer(my_times$starttime_sec[6]) & bite_sec <= as.integer(my_times$endtime_sec[6]))
fam17_at_table <- subset(fam17, bite_sec >= as.integer(my_times$starttime_sec[7]) & bite_sec <= as.integer(my_times$endtime_sec[7]))

# CREATE DYADS -----

setwd("~/Dropbox (Metzlab - VS)/Metz Lab/CURRENT PROJECTS/M2FED/Bite Annotations/Cleaned data/At table")

# FAMILY 01
unique(fam01_at_table$SubjectID)
combn(unique(fam01_at_table$SubjectID), 2)

dyad1 <- subset(fam01_at_table, SubjectID == "102" | SubjectID == "105")
write.csv(dyad1, file = "dyad1.csv", row.names = FALSE)
dyad2 <- subset(fam01_at_table, SubjectID == "105" | SubjectID == "103")
write.csv(dyad2, file = "dyad2.csv", row.names = FALSE)
dyad3 <- subset(fam01_at_table, SubjectID == "102" | SubjectID == "103")
write.csv(dyad3, file = "dyad3.csv", row.names = FALSE)

# FAMILY 07
unique(fam07_at_table$SubjectID)
combn(unique(fam07_at_table$SubjectID), 2)

dyad4 <- subset(fam07_at_table, SubjectID == "701" | SubjectID == "702")
write.csv(dyad4, file = "dyad4.csv", row.names = FALSE)
dyad5 <- subset(fam07_at_table, SubjectID == "701" | SubjectID == "703")
write.csv(dyad5, file = "dyad5.csv", row.names = FALSE)
dyad6 <- subset(fam07_at_table, SubjectID == "702" | SubjectID == "703")
write.csv(dyad6, file = "dyad6.csv", row.names = FALSE)

# FAMILY 09
unique(fam09_at_table$SubjectID)
combn(unique(fam09_at_table$SubjectID), 2)

dyad7 <- subset(fam09_at_table, SubjectID == "901" | SubjectID == "902")
write.csv(dyad7, file = "dyad7.csv", row.names = FALSE)
dyad8 <- subset(fam09_at_table, SubjectID == "901" | SubjectID == "903")
write.csv(dyad8, file = "dyad8.csv", row.names = FALSE)
dyad9 <- subset(fam09_at_table, SubjectID == "902" | SubjectID == "903")
write.csv(dyad9, file = "dyad9.csv", row.names = FALSE)

# FAMILY 13a
unique(fam13a_at_table$SubjectID)
combn(unique(fam13a_at_table$SubjectID), 2)

dyad10 <- subset(fam13a_at_table, SubjectID == "1306" | SubjectID == "1301")
write.csv(dyad10, file = "dyad10.csv", row.names = FALSE)
dyad11 <- subset(fam13a_at_table, SubjectID == "1306" | SubjectID == "1302")
write.csv(dyad11, file = "dyad11.csv", row.names = FALSE)
dyad12 <- subset(fam13a_at_table, SubjectID == "1301" | SubjectID == "1302")
write.csv(dyad12, file = "dyad12.csv", row.names = FALSE)

# FAMILY 13b
unique(fam13b_at_table$SubjectID)
combn(unique(fam13b_at_table$SubjectID), 2)

dyad13 <- subset(fam13b_at_table, SubjectID == "1305" | SubjectID == "1303")
write.csv(dyad13, file = "dyad13.csv", row.names = FALSE)
dyad14 <- subset(fam13b_at_table, SubjectID == "1305" | SubjectID == "1304")
write.csv(dyad14, file = "dyad14.csv", row.names = FALSE)
dyad15 <- subset(fam13b_at_table, SubjectID == "1303" | SubjectID == "1304")
write.csv(dyad15, file = "dyad15.csv", row.names = FALSE)

# FAMILY 15
unique(fam15_at_table$SubjectID)
combn(unique(fam15_at_table$SubjectID), 2)

dyad16 <- subset(fam15_at_table, SubjectID == "1501" | SubjectID == "1503")
write.csv(dyad16, file = "dyad16.csv", row.names = FALSE)
dyad17 <- subset(fam15_at_table, SubjectID == "1501" | SubjectID == "1502")
write.csv(dyad17, file = "dyad17.csv", row.names = FALSE)
dyad18 <- subset(fam15_at_table, SubjectID == "1503" | SubjectID == "1502")
write.csv(dyad18, file = "dyad18.csv", row.names = FALSE)

# FAMILY 17
unique(fam17_at_table$SubjectID)
combn(unique(fam17_at_table$SubjectID), 2)

dyad19 <- subset(fam17_at_table, SubjectID == "1701" | SubjectID == "1703")
write.csv(dyad19, file = "dyad19.csv", row.names = FALSE)
dyad20 <- subset(fam17_at_table, SubjectID == "1701" | SubjectID == "1704")
write.csv(dyad20, file = "dyad20.csv", row.names = FALSE)
dyad21 <- subset(fam17_at_table, SubjectID == "1701" | SubjectID == "1702")
write.csv(dyad21, file = "dyad21.csv", row.names = FALSE)
dyad22 <- subset(fam17_at_table, SubjectID == "1703" | SubjectID == "1704")
write.csv(dyad22, file = "dyad22.csv", row.names = FALSE)
dyad23 <- subset(fam17_at_table, SubjectID == "1703" | SubjectID == "1702")
write.csv(dyad23, file = "dyad23.csv", row.names = FALSE)
dyad24 <- subset(fam17_at_table, SubjectID == "1704" | SubjectID == "1702")
write.csv(dyad24, file = "dyad24.csv", row.names = FALSE)
