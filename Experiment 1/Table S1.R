####
#### script prepared by Joshua Becker
#### contact info at 
#### www.joshua-becker.com
####

rm(list=ls());gc()

library(dplyr)

### SET YOUR WORKING DIRECTORY TO SOURCE FILE LOCATION
d=read.csv("../Becker Centola Porter - Wisdom of Partisan Crowds - Supplementary Dataset.csv", stringsAsFactors=F) %>%
  subset(experiment==1) %>%
  ### REMOVE RESPONSES AN ORDER OF MAGNITUDE ABOVE TRUE VALUE
  ### (see Appendix)
  ### THIS ALSO REMOVES NA RESPONSES
  subset(
    response_1 <= 10*truth
    & response_2 <= 10*truth
    & response_3 <= 10*truth
  )


d %>%
  group_by(q) %>%
  summarize(
      mean_repub = mean(response_1[party=="Repub"])
    , mean_dem = mean(response_1[party=="Dem"])
    , med_repub = median(response_1[party=="Repub"])
    , med_dem = median(response_1[party=="Dem"])
  )

