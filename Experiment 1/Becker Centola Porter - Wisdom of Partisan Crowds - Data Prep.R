####
#### script prepared by Joshua Becker
#### contact info at 
#### www.joshua-becker.com
####

rm(list=ls());gc()

library(dplyr)

### SET YOUR WORKING DIRECTORY TO SOURCE FILE LOCATION
d=read.csv("../Becker Centola Porter - Wisdom of Partisan Crowds - Supplementary Dataset.csv", stringsAsFactors=F) %>%
  subset(experiment==1)


### LOAD BECKER THEME & HELPFUL FUNCTIONS
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")


### LOG ALL ANSWERS, SOCIAL INFO, AND TRUTH VALUES
d = mutate(d
           , response_1 = log(response_1)
           , response_2 = log(response_2)
           , response_3 = log(response_3)
           
           #, mean.neighbor.1 = log(mean.neighbor.1)
           #, mean.neighbor.2 = log(mean.neighbor.2)
           #, mean.neighbor.3 = log(mean.neighbor.3)
           
           , truth=log(truth)
           
           , err1 = abs(response_1 - truth)
           , err2 = abs(response_2 - truth)
           , err3 = abs(response_3 - truth)
           
           )


### REMOVE INVALID ANSWERS
### Ie, where people answered only at time 1 OR time 3
### is.finite removes answers that were NA (no response)
###    or 0 (can't be logged, but also conceptually invalid answers)
myd = subset(d, is.finite(response_1) 
             & is.finite(response_2) 
             & is.finite(response_3))



###  TRUTH-CENTER THE MEAN FOR EASIER INTERPRETATION
aggreg = myd %>% 
  group_by(network, pair_id, q, party, set, experiment) %>%
  summarize(
       truth=unique(truth)
     , mean_1 = (mean(response_1)-truth)
     , mean_2 = (mean(response_2) - truth)
     , mean_3 = (mean(response_3)- truth)
     
     , med_1 = (median(response_1)-truth)
     , med_2 = (median(response_2) - truth)
     , med_3 = (median(response_3)- truth)
     
     , err_mean_1 = abs(mean_1)
     , err_mean_2 = abs(mean_2)
     , err_mean_3 = abs(mean_3)
     
     
     , err_med_1 = abs(med_1)
     , err_med_2 = abs(med_2)
     , err_med_3 = abs(med_3)
     
     , err_ind_1 = mean(err1)
     , err_ind_2 = mean(err2)
     , err_ind_3 = mean(err3)
     
     
     , change_err_mean = err_mean_3 - err_mean_1
     , change_err_med = err_med_3 - err_med_1
  
     , change_err_ind = err_ind_3 - err_ind_1
     
     , sd_1 = sd(response_1)
     , sd_2 = sd(response_2)
     , sd_3 = sd(response_3)
     
     , change_sd = sd_3 - sd_1
     )
  

### CREATE OTHER USEFUL INDICES FOR GROUPING DATA
aggreg = mutate(aggreg
                , item = paste(q, set, pair_id, network,sep="_")
                , sub_trial = paste(set, pair_id,sep="_")
)

### GIVE THESE INDICES TO THE INDIVIDUAL LEVEL DATA AS WELL
myd$index = paste0(myd$pair_id, myd$q, myd$set, myd$network, myd$party)
aggreg$index = paste0(aggreg$pair_id, aggreg$q, aggreg$set, aggreg$network, aggreg$party)
myd = merge(myd, aggreg[,c("item","sub_trial","index")], 
            by="index")




### AGGREGATE AT THE TRIAL LEVEL

trial = aggreg %>%
  group_by(pair_id, set, party, network) %>%
  summarize(
      change_err_mean = mean(change_err_mean)
      
    , change_err_med = mean(change_err_med)
    
    , change_err_ind = mean(change_err_ind)
    
    , err_mean_1 = mean(err_mean_1)
    , err_mean_3 = mean(err_mean_3)
    , err_mean_2 = mean(err_mean_2)
    
    , err_ind_1 = mean(err_ind_1)
    , err_ind_2 = mean(err_ind_2)
    , err_ind_3 = mean(err_ind_3)
    , sd_1 = mean(sd_1)
    , sd_2 = mean(sd_2)
    , sd_3 = mean(sd_3)
    
    , change_sd = mean(change_sd)
  )


### CREATE SUBSETS BY PARTY FOR CONVENIENCE
repub = subset(aggreg, party=="Repub")
dem = subset(aggreg, party=="Dem")


### NOW CREATE A DATAFRAME WHERE EACH ROW IS A SINGLE PAIR
### CONTAINING OUTCOMES FOR BOTH PARTIES.

avg_pairwise = function(dems, repubs) {
  mean(abs(expand.grid(dems, repubs)[,1]-expand.grid(dems, repubs)[,2]), na.rm=T)
}

paired = myd %>% 
  group_by(sub_trial, q, pair_id, network) %>% 
  summarize(
     pairwise_1 = avg_pairwise(response_1[party=="Repub"],response_1[party=="Dem"])
   , pairwise_2 = avg_pairwise(response_2[party=="Repub"],response_2[party=="Dem"])
   , pairwise_3 = avg_pairwise(response_3[party=="Repub"],response_3[party=="Dem"])
   
   , diff_mean_1 = abs(mean(response_1[party=="Repub"])-mean(response_1[party=="Dem"]))
   , diff_mean_2 = abs(mean(response_2[party=="Repub"])-mean(response_2[party=="Dem"]))
   , diff_mean_3 = abs(mean(response_3[party=="Repub"])-mean(response_3[party=="Dem"]))
   , diff_pairwise = pairwise_3 - pairwise_1
   , diff_mean_diff = diff_mean_3 - diff_mean_1 
   , set=unique(set)
   )


### AND CALCULATE DIFFERENCES BETWEEN THE TWO GROUPS
paired = paired %>% mutate(
    diff_mean_change = diff_mean_3 - diff_mean_1
  , pairwise_change = pairwise_3 - pairwise_1
)

### NOW CREATE A DATAFRAME FOR PAIRED OUTCOMES
### AVERAGED OVER A TRIAL--IE, ALL FOUR QUESTIONS
trial_paired = paired %>%
  group_by(sub_trial, network) %>%
  summarize(
     diff_mean_1 = mean(diff_mean_1)
   , diff_mean_2 = mean(diff_mean_2)
   , diff_mean_3 = mean(diff_mean_3)
   
   , pairwise_1 = mean(pairwise_1)
   , pairwise_2 = mean(pairwise_2)
   , pairwise_3 = mean(pairwise_3)
   
   , diff_mean_change = mean(diff_mean_change)
   , pairwise_change = mean(pairwise_change)
)



save.image("BeckerCentolaPorter_WisdomOfPartisanCrowds.Rdata")
