# Calculating sensitive and non-sensitive time periods
# Using Dyad 16 as an example
# Need to ask George how to loop this through all dyad files

dyad16 <- read.csv("~/Dropbox (Metzlab - VS)/Metz Lab/CURRENT PROJECTS/M2FED/Bite Annotations/Cleaned data/At table/dyad16.csv", na = "<NA>")

q <- subset(dyad16, dyad16$SubjectID == 1501) # Lead
s <- subset(dyad16, dyad16$SubjectID == 1503) # Follower

# create complete time matrix
my_time <- min(dyad16$bite_sec):max(dyad16$bite_sec)
p1_bite_list <- q[[3]] # list of times when person1 took bites
p2_bite_list <- s[[3]] # list of times when person2 took bites

mat <- as.data.frame(my_time) # turn time matrix into data frame

# add p1 and p2 bite list columns to data frame
for (i in 1:nrow(mat)){
  mat$p1_bite[i] <- ifelse(is.element(mat$my_time[i], p1_bite_list), 1, 0)
  mat$p2_bite[i] <- ifelse(is.element(mat$my_time[i], p2_bite_list), 1, 0)
}

# calculate sensitive time periods in data frame (5 secs after p1 bite)
# this is literally the worst way to do this...but it works so yay
# I will come back and make this more efficient
mat$sensitive <- 0
for (i in 2:nrow(mat)){
  mat$sensitive[i] <- ifelse(mat$p1_bite[i-1] == 1 
                             || mat$p1_bite[i-2] == 1
                             || mat$p1_bite[i-3] == 1
                             || mat$p1_bite[i-4] == 1
                             || mat$p1_bite[i-5] == 1, 
                             1, 0)
}

# export mat dataset to directory
write.csv(mat, file = "dyad16_sensitive_example.csv", row.names = FALSE)

# divide into sensitive and nonsensitive subsets
dyad16_sens <- subset(mat, sensitive == 1)
dyad16_nonsens <- subset(mat, sensitive == 0)

# calculate consumption rates by # bites / # seconds for each group

# Person 1 (Lead)
(p1_bps_sens <- sum(dyad16_sens$p1_bite) / nrow(dyad16_sens)) #0.097
(p1_bps_nonsens <- sum(dyad16_nonsens$p1_bite) / nrow(dyad16_nonsens)) #0.047

# Person 2 (Follower)
(p2_bps_sens <- sum(dyad16_sens$p2_bite) / nrow(dyad16_sens)) #0.044
(p2_bps_nonsens <- sum(dyad16_nonsens$p2_bite) / nrow(dyad16_nonsens)) #0.081

# Sharps paper showed that consumption ratios of follower were higher in sensitive
# periods compared to nonsensitive periods (for food specific mimicry).
# The consumption ratios for each of the three sensitive time periods were 
# not significantly higher than the consumption ratios observed during the 
# equivalent non-sensitive periods (for non-specific food mimicry).

