library(ggplot2)
library(dplyr)
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

filename="TheoreticalFig.csv"
results = read.csv(filename, stringsAsFactors=F)
head(results)

d_sum = results %>% group_by(party, r, question) %>%
  summarize(
      N = length(change_err_mu)
    , change_err_mu = mean(change_err_mu)  
  )

substr(d_sum$question, 1, 1) <- toupper(substr(d_sum$question, 1, 1))

plot_list=lapply(unique(d_sum$question), function(q) {
  ggplot(d_sum%>%subset(question==q), aes(x=r, y=change_err_mu, color=party)) + 
    geom_point() + geom_line() +
    geom_hline(yintercept=0, linetype="dashed", color="grey")+
    scale_color_manual(values=c("grey","blue","red")) +
    labs(title=q, y="Change in Error of Mean", x="Degree/Belief Correlation") +
    guides(color=F) + beckertheme
})

png("Figure S4.png", width=10, height=5.5, units="in", res=500)
Rmisc::multiplot(plotlist=plot_list, cols=4)
dev.off()
