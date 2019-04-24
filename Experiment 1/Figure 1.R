####
#### script prepared by Joshua Becker
#### contact info at 
#### www.joshua-becker.com
####


### SET YOUR WORKING DIRECTORY!!
rm(list=ls());gc()

### LOAD/PREP DATA
if(file.exists("BeckerCentolaPorter_WisdomOfPartisanCrowds.Rdata")) {
  load("BeckerCentolaPorter_WisdomOfPartisanCrowds.Rdata")  
} else {
  source('Becker Centola Porter - Wisdom of Partisan Crowds - Data Prep.R')
}


library(ggplot2)
library(dplyr)


### AGGREGATE AT THE QUESTION LEVEL
question = aggreg %>% 
  subset(experiment==1) %>%
  group_by(q, party, network) %>%
  summarize( 
                  mean_1 = mean(mean_1)
                 , mean_2 = mean(mean_2)
                 , mean_3 = mean(mean_3)
                 
                 , err_mean_1 = mean(err_mean_1)
                 , err_mean_2 = mean(err_mean_2)
                 , err_mean_3 = mean(err_mean_3)
                 
                 , sd_1 = mean(sd_1)
                 , sd_2 = mean(sd_2)
                 , sd_3 = mean(sd_3)
)



long = reshape2::melt(question)

long$variable = factor(long$variable)
long$var = long$variable
levels(long$variable)=c("Round 1","Round 2", "Round 3",
                        "Round 1","Round 2", "Round 3",
                        "Round 1","Round 2", "Round 3")


count=0

ranges = list(
    c(-0.59, 0.2)
  , c(-0.59, 0.2)
  , c(-0.2, 0.59)
  , c(-0.2, 0.59)
)

plot_list=lapply(unique(long$q), FUN=function(x){
  
  if(count==4){count<<-0}
  count<<-count+1
  outplot=ggplot(subset(long, var %in% c("mean_1","mean_2","mean_3") & q==x), 
                 aes(x=variable, y=value, color=party
                     , alpha=network
                     , linetype=network, group=paste0(q,party, network))) +
    geom_hline(yintercept=0, color="grey", linetype="dashed")+
    scale_linetype_manual(values=c("dashed","solid"))+
    scale_alpha_discrete(range=c(0.35,1))+
    geom_point() + geom_line() + 
    facet_grid(.~q) + 
    scale_y_continuous(lim=ranges[[count]], breaks=seq(-1,1,by=0.2))+
    labs(x="", y="Truth-Centered Average") +
    guides(color=FALSE)+
    scale_color_manual(values=c("Blue","Red")) +
    beckertheme + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
  if(count>1) {
    outplot = outplot+labs(y="")
  }
  if(count<4) {
    outplot = outplot+theme(strip.text.y=element_text(color="white"))
  }
  outplot+guides(linetype=F, alpha=F) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
})

png("Figure 1 Experiment 1.png",
    width=9.1, height=3, units="in", res=600)

cowplot::plot_grid(plotlist=plot_list
                   , rel_widths = c(2,2,2,2)
                   , ncol=4, align="v")

dev.off()



png("Figure 1_legend.png",
    width=9, height=3.2, units="in", res=300)

plot_list[[1]]+guides(linetype="legend")+labs(linetype="")

dev.off()
