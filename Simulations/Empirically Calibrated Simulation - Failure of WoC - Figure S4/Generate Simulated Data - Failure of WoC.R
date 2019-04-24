rm(list=ls());gc()
require(igraph)
library(dplyr)
library(RCurl)
library(ggplot2)
source("../DeGrootModel.R")


complement <- function(y, rho, x) {
  ### FUNCTION BY whuber TO GENERATE CORRELATED VECTORS
  ### SOURCE: https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables
  if (missing(x)) x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}




#################################################################
###                                                           ###
###  TEST THE EFFECT OF SOCIAL INFLUENCE                      ###
###  IN A NETWORK WITH HETEROGENEOUS DEGREE DISTRIBUTION      ###
###  WHERE PEOPLE WITH MORE EXTREME BELIEFS                   ###
###  ALSO HAVE MORE CENTRALIZED POSITIONS                     ###
###                                                           ###
#################################################################


### START WITH EMPIRICAL DATA
### BECAUSE PEOPLE ON MTURK SOMETIMES GIVE CRAZY ANSWERS
### [not unlike people on twitter!]
### KEEP ONLY THE INNER 99% FOR EACH QUESTION
### TO ELIMINATE ANOMALOUS BEHAVIOR FROM THE MODEL
### ALSO, FIGURE OUT THE DIRECTION OF THE R & D BIASES
### THIS WILL INFORM THE MODEL
empirical_data1 = read.csv("C:/Users/joshua/Dropbox/beckerporter/RR/Replication Materials/Becker Porter Centola - The Wisdom of Partisan Crowds - Data and Replication Code/Experiment 1/Becker Centola Porter - Wisdom of Partisan Crowds - Supplementary Dataset.csv", stringsAsFactors=FALSE) %>%
  select(c("q","response_1","party","truth"))

empirical_data2 = read.csv("C:/Users/joshua/Dropbox/beckerporter/RR/Replication Materials/Becker Porter Centola - The Wisdom of Partisan Crowds - Data and Replication Code/Experiment 2/BeckerPorterCentola_SI_Dataset_Experiment2.csv", stringsAsFactors=F) %>%
  select(c("q","response_1","party","truth"))

empirical_data = rbind(empirical_data1, empirical_data2) %>%
  mutate(
    response=response_1
  ) %>%
  group_by(q) %>% do((function(x){
       lower_95 = as.numeric(quantile(x$response, probs=c(0.005,0.995), na.rm=T))[1]
       upper_95 = as.numeric(quantile(x$response, probs=c(0.005,0.995), na.rm=T))[2]
       x %>% 
         subset(response>lower_95 & response<upper_95 & !is.na(response)) %>%
         mutate(
           dem_lower = (mean(response[party=="Dem"])-mean(response[party=="Repub"]))<1
         )
      })(.)
  ) %>% 
  ungroup


### GENERATE NETWORK WHERE SUBJECTS WITH MOST EXTREME BELIEFS
### ---in the relative direction of their parties overall bias--
### ARE CORRELATED WITH CENTRALITY
generatePopulation = function(question, N=100, r=0.5
                         , network_generator = function(N){barabasi.game(n=N, pow=2, m=2, directed=F)}
                         ) {
  d = subset(empirical_data, q==question)  
  g = network_generator(N)
  
  dem_data = subset(d, party=="Dem") %>% 
    sample_n(N, replace=T) %>%
    select(c("response","dem_lower")) %>%
    mutate(
      extremity_rank = ifelse(
        dem_lower, rank(response, ties.method="random"), N-rank(response, ties.method="random")
      )
      , centrality_rank = rank(complement(extremity_rank, r))
    ) %>% 
    arrange(desc(centrality_rank)) %>%
    arrange(order(degree(g))) %>%
    mutate(
      degree=degree(g)
    )
  
  rep_data  = subset(d, party=="Repub") %>% 
    sample_n(N, replace=T) %>%
    select(c("response","dem_lower")) %>%
    mutate(
      extremity_rank = ifelse(
        dem_lower, N-rank(response, ties.method="random"), rank(response, ties.method="random")
      )
      , centrality_rank = rank(complement(extremity_rank, r))
    ) %>% 
    arrange(desc(centrality_rank)) %>%
    arrange(order(degree(g)))

  
  dems = g
  reps = g
  
  V(dems)$guess = dem_data$response
  V(dems)$alpha = runif(N,0,1)
  
  V(reps)$guess = rep_data$response
  V(reps)$alpha = runif(N,0,1)
  
  list(reps=reps, dems=dems)
}


### TEST THE EFFECT OF CORRELATION BTW EXTREMITY & CENTRALITY


results = data.frame(  
    party=character()
  , change_err_mu = numeric()
  , change_ind_err = numeric()
  , corr_centrality_rank=numeric()
  , question=character()
  , r=numeric()
  , trial=numeric()
  , centralization=numeric()
  , stringsAsFactors=F
)

filename="TheoreticalFig.csv"
#write.csv(results,filename, row.names=F)

N=100
for(i in 1:10000) {
  print(i)
  for(question in unique(empirical_data$q)) {
    for(r in seq(0,1,by=0.1)) {
      population = generatePopulation(question, N, r)
      
      TRUTH=unique(empirical_data$truth[empirical_data$q==question])
      
      dems = simDegroot(population$dems, 5, 0, TRUTH=TRUTH) %>% mutate(
        err_mu = abs(truth-mean)
        , party="dem"
        , corr_centrality_rank=cor(rank(V(population$reps)$guess),rank(degree(population$reps)))
      )
      
      
      reps = simDegroot(population$reps, 5, 0, TRUTH=TRUTH) %>% mutate(
        err_mu = abs(truth-mean)
        , party="rep"
        , corr_centrality_rank=cor(rank(V(population$dems)$guess),rank(degree(population$dems)))
      )
      
      new_result = rbind(reps, dems) %>% group_by(party) %>%
        summarize(
          change_err_mu = err_mu[round==5]-err_mu[round==0]
          , change_ind_err = mean_ind_err[round==5]-mean_ind_err[round==0]
          , corr_centrality_rank = unique(corr_centrality_rank)
        ) %>% rbind(rbind(reps, dems) %>% summarize(
          party="all"
          , change_err_mu = mean(err_mu[round==5])-mean(err_mu[round==0])
          , change_ind_err = mean(mean_ind_err[round==5])-mean(mean_ind_err[round==0])
          , corr_centrality_rank = mean(corr_centrality_rank)
        )) %>% mutate(
            question=question
          , r = r
          , trial=i
          , centralization=centralization.degree(population$dems)$centralization
        )
      
      write.table(new_result, filename, append=T, sep=",", row.names = F, col.names=F)
    }
  }
}

