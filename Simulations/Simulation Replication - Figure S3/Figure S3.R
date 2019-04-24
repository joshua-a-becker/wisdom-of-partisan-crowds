library(ggplot2)
library(dplyr)
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

filename="TheoreticalFig.csv"
results = read.csv(filename)


beckertheme =   theme(panel.background=element_rect(fill="white", color="black", size=1.1), 
                      axis.text=element_text(size=rel(1), color="black"), 
                      strip.text=element_text(size=rel(1.1)), 
                      legend.text=element_text(size=rel(1.1)), strip.background=element_blank(),
                      title=element_text(size=rel(1.1)),
                      panel.grid=element_blank(),
                      plot.title=element_text(hjust=0.5))


results$corr_bias_round = round(results$corr_bias, 1)

results$prop = (results$prop+1)/2

results$bias_start = results$mean_start

d_sum=subset(results,noise<=5&noise>0) %>% 
  group_by(prop, noise) %>%
  summarize(
    sd = sd(change_err_mu)
  , change_err_mu=mean(change_err_mu)
  , change_mu=mean(mean_end-mean_start)
  , N=length(prop)
  , change_ind_err=mean(change_ind_err)
)

xticks=seq(0,1,by=0.25)
xlabs = paste0(xticks*100, "%")
d_sum$noise_level = factor(d_sum$noise)
levels(d_sum$noise_level)=c("Low Noise","High Noise")


### Multiple change in error by -1 so we get increase in error
### (negative change in error is decrase)
ggplot(d_sum, aes(x=prop, y=change_err_mu)) + 
  geom_ribbon(aes(ymin=change_err_mu-sd, ymax=change_err_mu+sd), fill="#CCCCCC")+
  geom_point()+geom_line() +
  #geom_point(aes(y=change_ind_err))+
  scale_x_continuous(expand=c(0.005,0.005), breaks=xticks, labels=xlabs) +
  facet_grid(.~noise_level) + beckertheme +
  theme(panel.spacing = unit(2, "lines")) +
  labs(x="Proportion of Type A", y="Increase in Error\n(Standard Deviations)")


ggsave("Figure S3.png", width=4.5, height=2.5)
