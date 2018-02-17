# This code analyzes the cleaned dyad bite data
# Updated: 2-16-   18
# Author: Brooke Bell

rm(list=ls())
library(plyr)
library(dplyr)
library(gmodels)
library(boot)
library(stringr)
library(tidyr)
library(knitr)

# IMPORT AND CLEAN DYAD DATASETS ----

# let n = the number of dyads we have
n <- 24

setwd("/Users/antunez/Dropbox (Metzlab - VS)/Metz Lab/CURRENT PROJECTS/M2FED/Bite Annotations/Cleaned data/At table")

my_files <- paste0("dyad", 1:n, ".csv")
my_data <- lapply(my_files, read.csv)
names(my_data) <- stringr::str_replace(my_files, pattern = ".csv", replacement = "")

# Reshape data to wide format

dyad_reshape <- function(x){
  
  num_subjects <- length(unique(x$SubjectID))
  
  leader <- subset(x, x$SubjectID == unique(x$SubjectID)[1]) # Leader bites
  follower <- subset(x, x$SubjectID == unique(x$SubjectID)[2]) # Follower bites
  
  my_time <- min(x$bite_sec):max(x$bite_sec)
  a_bite_list <- leader[[3]] # list of times when person1 took bites
  b_bite_list <- follower[[3]] # list of times when person2 took bites
  
  mat <- as.data.frame(my_time) # turn time matrix into data frame
  
  for (i in 1:nrow(mat)){
    mat$a[i] <- ifelse(is.element(mat$my_time[i], a_bite_list), 1, 0)
    mat$b[i] <- ifelse(is.element(mat$my_time[i], b_bite_list), 1, 0)
  }
  
  print(mat)
}

# reshape all dyad datasets
my_data_wide <- lapply(my_data, dyad_reshape)

# COMPUTE SENSITIVE PERIODS ----

compute_sensitivity <- function(x){
  
  x$sensitive <- 0
  
  for (i in 2:nrow(x)){
    x$sensitive[i] <- ifelse(x$a[i-1] == 1 
                               || x$a[i-2] == 1
                               || x$a[i-3] == 1
                               || x$a[i-4] == 1
                               || x$a[i-5] == 1, 
                               1, 0)
  }
  
  print(x)
  
}

# apply sensitive function to list
my_data_wide1 <- lapply(my_data_wide, compute_sensitivity)

compare_sensitivity <- function(x){
  
  # Step 1: Divide into sensitive and nonsensitive subsets
  sens <- subset(x, sensitive == 1)
  nonsens <- subset(x, sensitive == 0)
  
  # Step 2: Compute ratio of mimicked bites for Person A (Lead)
  a_bpm_sens <- (sum(sens$a) / nrow(sens)) * 60
  
  # Step 2: Compute ratio of mimicked bites for Person B (Follower)
  b_bpm_sens <- (sum(sens$b) / nrow(sens)) * 60
  
  # Step 3: Compute ratio of non-mimicked bites for Person A (Lead)
  a_bpm_nonsens <- (sum(nonsens$a) / nrow(nonsens)) * 60
  
  # Step 3: Compute ratio of non-mimicked bites for Person B (Follower)
  b_bpm_nonsens <- (sum(nonsens$b) / nrow(nonsens)) * 60
  
  
  kable(cbind(a_bpm_sens, a_bpm_nonsens, b_bpm_sens, b_bpm_nonsens), 
        col.names = c("A bpm Sensitive", "A bpm Non-Sensitive",
                      "B bpm Sensitive", "B bpm Non-Sensitive"))
  
}

# apply compare_sensitive function to list
sens_stats <- lapply(my_data_wide1, compare_sensitivity)

# Print list of ratios for each dyad
sens_stats

# Step 5: Conduct paired sample t-tests comparing ratios of mimicked bites with ratios of nonmimicked bites

# create list of mimicked bites ratios

# create list of nonmimicked bites ratios

# RUN PERMUTATIONS ----

# George's permtest function

permtest <- function(nsim=1e3, statistic, x, ...) {
  
  # Creating space in the memory
  ans <- vector("double", nsim)
  
  # Random sorting of the data (we will use this with the `order` function)
  ORD <- matrix(runif(nsim*length(x)), ncol=nsim)
  for (b in 1:nsim)
    # For each `i`, we compute the statistic changing the order of `x` as a
    # function of `order(ORD[,i])` (this returns indexes)
    ans[b] <- statistic(x[order(ORD[,b])], ...)
  
  # We return the resulting statistic
  ans
  
}

# Step one: defining test statistic (come back to this)

statistic <- function(x){
  sum(x$mimic_5, na.rm = TRUE) / sum(x$a, x$b, na.rm = TRUE)
}

statistic(my_data_wide_mimic$dyad14)

X <- my_data_wide_mimic$dyad14$a
Y <- my_data_wide_mimic$dyad14$b

S <- permtest(1e4, statistic, X, y=Y)

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




