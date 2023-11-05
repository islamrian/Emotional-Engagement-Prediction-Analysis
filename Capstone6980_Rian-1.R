
getwd()
setwd("~/ALY_6980_Capstone")
library(readr)
Capstone <- read_csv("/Users/rian/Downloads/NE_PathosAI_PredictiveAnalytics_Capstone_DataFile - Data.csv",
        col_types = c("icffcffdccfffffffffffffffff"))
# now they are mostly factors, good for grouping
# convert data to date type
library(lubridate)
Capstone$Date <- mdy_hm(Capstone$Date)
######

summary(Capstone)

# 3 NAs in ID
Capstone <- Capstone[!is.na(Capstone$Id),]
colSums(is.na(Capstone))
# no NAs for emotion and driver or EE, positive terms has only 736 NAs and Neg terms has
# 5993, Dimensions has only 8750. The rest are all mostly NAs. Satisfied, Recommend,taste, 
# switched have over 1000 non-NAs.

table(Capstone$other_buy_not_buy)
#not that much info there

#check out freq of various dimensions
dd <- as.data.frame(table(Capstone$dimensions_detected))

summary(Capstone)

library(tidyverse)
# assign each emotion a number. Negative is -5:-1 positive is 1-4
Capstone <- Capstone %>% 
  mutate(Emotion_Value = if_else(Emotion == "disgust", -5,
                                 if_else(Emotion %in% "angry", -4,
                                 if_else(Emotion %in% "fear", -2, 
                                         if_else(Emotion %in% "sad", -1,
                                                 if_else(Emotion == "frustration", -3,
                                                         if_else(Emotion %in% "happy", 2,
          if_else(Emotion %in% "excited", 4,
          if_else(Emotion %in% "surprised", 3,0)))))))))
table(Capstone$Emotion_Value)

plot(Capstone$Date, Capstone$Emotion_Value)
sum(!is.na(Capstone$`recommend/not recommend`))
#1278 non-na's

table(Capstone$`recommend/not recommend`)

#create field for positive and neg. recommendations
Capstone$NotRecommended <-  str_detect(Capstone$`recommend/not recommend`,
                                    "avoid|cannot|not|never|wouldnt")
table(Capstone$NotRecommended)
Capstone$Recommended <- str_detect(Capstone$`recommend/not recommend`,
                                   "recommend|appreciated|positive|preferred")
table(Capstone$Recommended)

Capstone <- Capstone %>% 
  mutate(Recommend = if_else(NotRecommended == TRUE, -1,
                               if_else(Recommended == TRUE, 1,0)))
Capstone$Recommend[is.na(Capstone$Recommend)] <- 0

#remove extra columns
Capstone$NotRecommended <- NULL
Capstone$Recommended <- NULL
table(Capstone$Recommend)
qplot(Capstone$Recommend)

#create field for positive and neg. Satisfied
Capstone$Satisfied <- str_detect(Capstone$other_satisfied_not_satisfied,
                                 "best|stars|great|satis")
Capstone$Satisfied <- if_else(Capstone$Satisfied == TRUE,1,-1)
Capstone$Satisfied[is.na(Capstone$Satisfied)] <- 0
table(Capstone$Satisfied)