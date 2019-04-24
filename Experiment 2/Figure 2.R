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

long = reshape2::melt(trial, id.vars=c("pair_id","set","party","network"))

long$round = factor(as.numeric(sub(".*\\D(\\d+).*", "\\1", paste(" ", long$variable))),
                    levels=c(1,2,3),
                    labels=c("Round 1","Round 2","Round 3")
)

long$network = factor(long$network, levels=c("Social","Control"))

ggplot(subset(long,grepl("err_mean_",variable)), 
       aes(x=round, y=value, linetype=network, group=network)) +
  stat_summary(fun.y="mean", geom="point")+
  stat_summary(fun.y="mean", geom="line") +
  guides(linetype=F) +
  beckertheme + 
  labs(y="Normalized Error",x="")
ggsave("Figure 2 Left.png", width=3, height=2.6, dpi=600)





dsum=summarySE(subset(trial)
               , measurevar="change_err_mean"
               , groupvars=c("network")
)


ggplot(dsum, aes(x=network, y=change_err_mean)) + 
  geom_bar(stat="identity", fill="#888888", width=0.7) +
  geom_errorbar(aes(ymin=change_err_mean-ci, ymax=change_err_mean+ci), width=0) +
  beckertheme +
  geom_hline(yintercept=0, linetype="dashed") +
#  ylim(c(-0.17,0.17))+
  labs(x="", y="Cumulative Change inError\nRound 1 to Round 3\n")
ggsave("Figure 2 Right.png", width=2.5, height=2.6, dpi=600)
