####
#### script prepared by Joshua Becker
#### contact info at 
#### www.joshua-becker.com
####


### SET YOUR WORKING DIRECTORY!!
rm(list=ls());gc()

### LOAD/PREP DATA
if(file.exists("BeckerCentolaPorter_WisdomOfPartisanCrowds_Experiment2.Rdata")) {
  load("BeckerCentolaPorter_WisdomOfPartisanCrowds_Experiment2.Rdata")  
} else {
  source('Becker Centola Porter - Wisdom of Partisan Crowds - Data Prep.R')
}


library(ggplot2)


long_trial = reshape2::melt(trial_paired, id.vars=c("sub_trial","network"))

long_trial$var = factor(long_trial$variable)
levels(long_trial$var)=c("Round 1","Round 2","Round 3",
                            "Round 1","Round 2","Round 3",
                            "Diff Mean Diff","Diff Pairwise Diff")

long_trial$round = factor(long_trial$variable)

long_trial$measurement = ifelse(grepl("pairwise",long_trial$variable), "Average Pairwise Difference","Difference Between Means")


ggplot(subset(long_trial, variable %in% c("diff_mean_1","diff_mean_2","diff_mean_3")),
       aes(x=var, y=value, linetype=network, group=network)) +
  stat_summary(fun.y="mean", geom="point") +
  stat_summary(fun.y="mean", geom="line") +
  scale_linetype_manual(values=c("dashed","solid"))+
  guides(linetype=F) + 
  labs(x="", y="Distance Between Means\n")+
  beckertheme
ggsave("Figure 3 - Left.png", width=3.05, height=2.8, dpi=600)

ggplot(subset(long_trial, variable %in% c("pairwise_1","pairwise_2","pairwise_3")),
       aes(x=var, y=value, linetype=network, group=network)) +
  stat_summary(fun.y="mean", geom="point") +
  stat_summary(fun.y="mean", geom="line") +
  scale_linetype_manual(values=c("dashed","solid"))+
  labs(x="", y="Average Pairwise Distance\n")+
  guides(linetype=F) + 
  beckertheme
ggsave("Figure 3 - Right.png", width=3, height=2.8, dpi=600)

