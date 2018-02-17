# Analysis for synchrony paper
# Updated: 2/1/18
# Author: Brooke Bell

# IMPORT DATA -----

rm(list=ls())
library(plyr)
library(dplyr)
library(gmodels)

# set working directory
setwd("~/Dropbox (Metzlab - VS)/Metz Lab/CURRENT PROJECTS/M2FED/Bite Annotations/Cleaned data")

# how many dyads do we have?
num_dyads <- (choose(3,2) * 6) + (choose(4,2) * 1)

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


# IMPORT DYAD DATASETS ----

# import dyad data from csv files
for(i in 1:24) {
  dyad <- paste("dyad", i, sep = "")
  assign(dyad, read.csv(paste("dyad", i, ".csv", sep = ""), na = "<NA>"))
}

# COMPUTE MIMICKED BITES (work on later) ----- 

  # create mimic variable if bite came within 5 secs of previous bite taken by other person
  
  dyad1$mimic <- NA
  for(i in 2:n1){
    dyad1$mimic[i] <- ifelse(dyad1$bite_sec_cent[i]  <= (dyad1$bite_sec_cent[i - 1] + 5) & dyad1$SubjectID[i] != dyad1$SubjectID[i-1], 1, 0)
  }
  
  sum(dyad1$mimic, na.rm = TRUE)



# Dyad 1
n1 <- nrow(dyad1)

# create mimic variable if bite came within 5 secs of previous bite taken by other person

dyad1$mimic_10 <- NA
for(i in 2:n1){
  dyad1$mimic_10[i] <- ifelse(dyad1$bite_sec_cent[i]  <= (dyad1$bite_sec_cent[i - 1] + 10) & dyad1$SubjectID[i] != dyad1$SubjectID[i-1], 1, 0)
}

sum(dyad1$mimic_10, na.rm = TRUE)

# Dyad 2
n2 <- nrow(dyad2)

# create mimic variable if bite came within 5 secs of previous bite taken by other person
dyad2$mimic_10 <- NA

for(i in 2:n2){
  dyad2$mimic_10[i] <- ifelse(dyad2$bite_sec_cent[i]  <= (dyad2$bite_sec_cent[i - 1] + 5) & dyad2$SubjectID[i] != dyad2$SubjectID[i-1], 1, 0)
}

sum(dyad2$mimic_10, na.rm = TRUE)

# Dyad 3
n3 <- nrow(dyad3)

# create mimic variable if bite came within 5 secs of previous bite taken by other person
dyad3$mimic_10 <- NA

for(i in 2:n3){
  dyad3$mimic_10[i] <- ifelse(dyad3$bite_sec_cent[i]  <= (dyad3$bite_sec_cent[i - 1] + 5) & dyad3$SubjectID[i] != dyad3$SubjectID[i-1], 1, 0)
}

sum(dyad3$mimic_10, na.rm = TRUE)

# Dyad 4
n4 <- nrow(dyad4)

# create mimic variable if bite came within 5 secs of previous bite taken by other person
dyad4$mimic <- NA

for(i in 2:n4){
  dyad4$mimic[i] <- ifelse(dyad4$bite_sec_cent[i]  <= (dyad4$bite_sec_cent[i - 1] + 5) & dyad4$SubjectID[i] != dyad4$SubjectID[i-1], 1, 0)
}

sum(dyad4$mimic, na.rm = TRUE)

# Dyad 5
n5 <- nrow(dyad5)

# create mimic variable if bite came within 5 secs of previous bite taken by other person
dyad5$mimic <- NA

for(i in 2:n5){
  dyad5$mimic[i] <- ifelse(dyad5$bite_sec_cent[i]  <= (dyad5$bite_sec_cent[i - 1] + 5) & dyad5$SubjectID[i] != dyad5$SubjectID[i-1], 1, 0)
}

sum(dyad5$mimic, na.rm = TRUE)

# COMPUTE BITES PER MIN (RATE) -----

# first compute meal duration for each dyad
for(i in 1:24) {
  assign(paste0('dyad', i), `[[<-`(get(paste0('dyad', i)), 'meal_duration', 
  value = max(get(paste0('dyad', i))$bite_sec_cent) - min(get(paste0('dyad', i))$bite_sec_cent)), envir=.GlobalEnv)
}

# then compute number of bites per subject
dyads <- ls(pattern = "dyad[0-9]+")
DAT <- NULL

for(i in 1:24){
  bite_count <- with(get(paste0('dyad', i)), by(bite_sec_cent, SubjectID, function(x) length(x))) #number bites
  duration_min <- with(get(paste0('dyad', i)), by(meal_duration, SubjectID, function(x) head(x,1)/60))
  DAT <- rbind(
    DAT, 
    data.frame(dyadid = print(paste0('dyad', i)), SubjectID = names(bite_count), bites = unclass(bite_count), duration_min = unclass(duration_min)))
}

# calculate bpm
DAT$bmp <- DAT$bites / DAT$duration_min

# RUN CORRELATION -----
# reshape data to wide format
DAT <- cbind(DAT, as.vector(rep(c(1,2), 24)))
colnames(DAT)[6] <- "person"
DAT_wide <- reshape(DAT, idvar = "dyadid", timevar = "person", direction = "wide")

# compare bpm
cor.test(DAT_wide$bmp.1, DAT_wide$bmp.2) #r=0.3346903

#compare total bites
cor.test(DAT_wide$bites.1, DAT_wide$bites.2) #r = 0.6452947, p = 0.0006617
cov(DAT_wide$bites.1, DAT_wide$bites.2) #830.6413
var.test(DAT_wide$bites.1, DAT_wide$bites.2) #p = 0.4061

plot(DAT_wide$bites.1, DAT_wide$bites.2, 
     main = "Scatterplot of total bites", 
     xlab = "Total Bites for Subject 1", 
     ylab = "Total Bites for Subject 2", 
     pch = 19, asp = 0.2)

# RUN TTEST -----

# between dyad types (need to work on)
DAT_wide$bpm_diff <- abs(DAT_wide$bmp.1 - DAT_wide$bmp.2)
child_child <- subset(DAT_wide, DAT_wide$dyad_type == "Child-Child")
child_parent <- subset(DAT_wide, DAT_wide$dyad_type == "Child-Parent")
t.test(child_child$bpm_diff, child_parent$bpm_diff)
# not significant p=0.5629

# SUMMARY STATS -----

# join demographic and bite data
demographics <- read.csv("/Users/antunez/Dropbox (Metzlab - VS)/Metz Lab/CURRENT PROJECTS/M2FED/SBM Conference '18/Participant masterlist.csv")
DAT$SubjectID <- as.vector(as.character(DAT$SubjectID))
demographics$SubjectID <- as.vector(as.character(demographics$SubjectID))
DAT_full <- left_join(DAT, demographics, by = "SubjectID")

DAT_wide1 <- reshape(DAT_full, idvar = "dyadid", timevar = "person", direction = "wide")

# ugh this is so messy - come back and fix
summary_stats <- readxl::read_xlsx("/Users/antunez/Dropbox (Metzlab - VS)/Metz Lab/CURRENT PROJECTS/M2FED/SBM Conference '18/summary stats.xlsx")
DAT_wide1$dyad_type <- summary_stats$`Type of Dyad` 

# demographics of those who participated in eating session
sum(demographics$Child) #17
sum(demographics$Parent) #9

mean(demographics$Age) #24.03846
mean(subset(demographics$Age, demographics1$Child == 1))
sd(subset(demographics$Age, demographics1$Child == 1))
mean(subset(demographics$Age, demographics1$Child == 0))
sd(subset(demographics$Age, demographics1$Child == 0))

#avg bpm for older children
median(subset(DAT_full$bmp, demographics$Age > median(subset(demographics$Age, demographics$Child == 1))))

#med bpm for younger children
median(subset(DAT_full$bmp, demographics$Age < median(subset(demographics$Age, demographics$Child == 1))))

#med bpm for older children
median(subset(DAT_full$bmp, demographics$Age > median(subset(demographics$Age, demographics$Child == 1))))

# ttest
older_children <- subset(DAT_full, demographics$Age > median(subset(demographics$Age, demographics$Child == 1)))
younger_children <- subset(DAT_full, demographics$Age < median(subset(demographics$Age, demographics$Child == 1)))

#med bites male vs female
median(subset(DAT_full$bites, DAT_full$Gender == "F")) #37
median(subset(DAT_full$bites, DAT_full$Gender == "M")) #61
mean(subset(DAT_full$bites, DAT_full$Gender == "F")) #44
mean(subset(DAT_full$bites, DAT_full$Gender == "M")) #61

# ttest

males <- subset(DAT_full, DAT_full$Gender == "M")
females <- subset(DAT_full, DAT_full$Gender == "F")

# code below doesn't work...
mean(subset(demographics$Age, demographics$Child == 0))
count(DAT_full$Race)
count(DAT_full$Gender)
count(demographics, 'Child')
count(demographics, 'Parent')
count(DAT_wide1, 'dyad_type')

mean(DAT_full$bmp)
mean(subset(DAT_full$bmp, DAT_full$Child == 1))
mean(subset(DAT_full$bmp, DAT_full$Child == 0))
mean(DAT_full$bites)
mean(subset(DAT_full$bites, DAT_full$Child == 1))
mean(subset(DAT_full$bites, DAT_full$Child == 0))
mean(DAT_full$duration_min)

total_bites <- nrow(fam01) + nrow(fam07) + nrow(fam09) + nrow(fam13a) + nrow(fam13b) + nrow(fam15) + nrow(fam17)
total_bites


# NON-PARAMETRIC TESTING ----




