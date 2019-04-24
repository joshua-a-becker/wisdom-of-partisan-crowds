rm(list=ls());gc()
library(httr)
library(dplyr)
library(foreign)



if(!file.exists("bullock.zip")){
  GET("http://dx.doi.org/10.1561/100.00014074_supp", write_disk(tf <- "bullock.zip"))  
}

if(!file.exists("qjps_14074_supp/MTurk12/MTURK2012PublicReplicationDataset.dta")){
  unzip("bullock.zip")  
}


exp2_data=read.dta("qjps_14074_supp/MTurk12/MTURK2012PublicReplicationDataset.dta", 
         convert.dates = TRUE, convert.factors = T,
         missing.type = FALSE,
         convert.underscore = FALSE, warn.missing.labels = TRUE) %>%
  mutate(
    questionid = as.character(questionid)
    ### calculate adjustment constant to transform slider responses
    ### into real number responses
    , adj_constant = correctanswer / correctanswer_sliderscale
    , actual_response = sliderresponse*adj_constant
  )




### GET SPLITS FOR EXPERIMENT 2
exp2_by_question = exp2_data %>% 
  subset(!is.na(partisanship)) %>%
  group_by(questionid, partisanship) %>%
  summarize(
    correct_slider = unique(correctanswer_sliderscale)
    , correct_actual = unique(correctanswer)
    , mu_slider = mean(sliderresponse, na.rm=T)
    , mu_actual = mean(actual_response, na.rm=T)
    , rel_slider = mu_slider - correct_slider
    , rel_actual = mu_actual - correct_actual
  )


### just for thoroughness and sanity
### calculate splits & answers for both the actual slider response
### and the adjusted-to-real-number response

exp2_sum_actual = reshape2::dcast(exp2_by_question, 
                        questionid + correct_actual ~ partisanship, value.var="mu_actual") %>%
  mutate(
    Dem_Rel = Democrat - correct_actual
    , Rep_Rel = Republican - correct_actual
    , same = (Dem_Rel<0) == (Rep_Rel<0)
  )

exp2_sum_slider = reshape2::dcast(exp2_by_question, 
                        questionid + correct_slider ~ partisanship, value.var="mu_slider") %>%
  mutate(
    Dem_Rel = Democrat - correct_slider
    , Rep_Rel = Republican - correct_slider
    , same = (Dem_Rel<0) == (Rep_Rel<0)
  )


### only one question is nominally split around truth:
### however, this effect is TINY is unlikely to be replicable
View(exp2_sum_actual %>% arrange(same) )

View(exp2_sum_slider %>% arrange(same) )
