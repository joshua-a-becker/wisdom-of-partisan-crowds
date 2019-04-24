library(dplyr)
rm(list=ls());gc()
d <- read.csv("Becker Porter Centola - Pilot Questions.csv", stringsAsFactors=F)


d_clean = d  %>% 
  subset(response<1000 | 
           ! question %in% c("china","longtaxchange","longeducationchange"
                             , "gssconservatives", "gssliberals"
                             , "taxes2009", "taxes2015"
                             , "clintonunemploy","X1985blacks","X2004kerry","X2004bush"
                             , "military.research", "murder") )


d_sum = d_clean %>% 
  ## remove independents
  subset(party !="I") %>%
  group_by(question) %>%
  mutate(
      sd_pool = sd(response, na.rm=T)
    , z = abs(response/sd_pool)
  ) %>% 
  group_by(question, party) %>%
  summarize(
      mu = round(mean(response),1)
    , truth=unique(truth)
  ) %>%
  reshape2::dcast(question+truth~party, value.var="mu") %>%
  mutate(
      D_bias = D - truth
    , R_bias = R - truth
    , split = (D_bias<0) != (R_bias<0)
  )

names(d_sum)

### very few questions show split bias.
### those that do have small effects
### and are not particulalry controversial questions
### --i.e., probably just coincidence
View(d_sum %>% arrange(split))
