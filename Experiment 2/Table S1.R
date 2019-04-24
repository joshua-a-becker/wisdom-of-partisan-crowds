####
#### script prepared by Joshua Becker
#### contact info at 
#### www.joshua-becker.com
####

rm(list=ls());gc()

library(dplyr)

### SET YOUR WORKING DIRECTORY TO SOURCE FILE LOCATION
d=read.csv("../Becker Centola Porter - Wisdom of Partisan Crowds - Supplementary Dataset.csv", stringsAsFactors=F) %>%
  subset(experiment==2) %>% 
  ### REMOVE EXTREME RESPONSES---SEE APPENDIX FOR DETAILS
  subset(remove_extreme=="") %>%
  ### REMOVE NA
  subset(is.finite(response_1) 
         #& is.finite(response_2) 
         #& is.finite(response_3)
  )


d %>%
  group_by(q) %>%
  summarize(
      mean_repub = mean(response_1[party=="Repub"])
    , mean_dem = mean(response_1[party=="Dem"])
    , med_repub = median(response_1[party=="Repub"])
    , med_dem = median(response_1[party=="Dem"])
  )
