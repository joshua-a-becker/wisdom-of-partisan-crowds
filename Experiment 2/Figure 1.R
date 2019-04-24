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
  source('Becker Centola Porter - Wisdom of Partisan Crowds - Exp 2 - Data Prep.R')
}


library(reshape2)
library(ggplot2)
library(dplyr)


### AGGREGATE AT THE QUESTION LEVEL
question = aggreg %>% 
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

plot_list=lapply(unique(long$q), FUN=function(x){
  this_data = subset(long, var %in% c("mean_1","mean_2","mean_3") & q==x)
  max_val =max(abs(range(this_data$value)))
  
  if(count==4){count<<-0}
  count<<-count+1
  outplot=ggplot(this_data, 
                 aes(x=variable, y=value, color=party
                     , alpha=network
                     , linetype=network, group=paste0(q,party, network))) +
    geom_hline(yintercept=0, color="grey", linetype="dashed")+
    scale_linetype_manual(values=c("dashed","solid"))+
    scale_alpha_discrete(range=c(0.35,1))+
    geom_point() + geom_line() + 
    facet_grid(.~q) + 
    #scale_y_continuous(lim=ranges[[count]], breaks=seq(-1,1,by=0.2))+
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
  
  if(unique(subset(d, q==x)$raw.truth)<0) {
    outplot = outplot + scale_y_reverse(lim=sort(c(-1*max_val, max_val), decreasing=T)/c(7,1))
  } else {
    outplot = outplot + scale_y_continuous(lim=sort(c(max_val, -1*max_val))/c(9,1))
  }
  
  
  outplot+guides(linetype=F, alpha=F) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
})

png("Figure 1 Experiment 2.png",
    width=9.1, height=3, units="in", res=600)

cowplot::plot_grid(plotlist=plot_list
                   , rel_widths = c(2,2,2,2)
                   , ncol=4, align="v")

dev.off()


png("Figure 1_legend.png",
    width=9, height=3.2, units="in", res=300)

plot_list[[1]]+guides(linetype="legend")+labs(linetype="")

dev.off()
