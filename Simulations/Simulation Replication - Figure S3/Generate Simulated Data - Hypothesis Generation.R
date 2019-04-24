rm(list=ls());gc()
require(igraph)
library(dplyr)
library(RCurl)
source("../DeGrootModel.R")


sigmoid = function (x, c=1, w=1) {
  1/(1 + c*exp(-1*w*x))
}

generatePopulation = function(g, noise=0, bias=1){
  ### DETERMINE BELIEFS AND ALPHA ACCORDING TO DETERMINATE MODEL WITH NOISE
  b = rnorm(vcount(g), 0, 1)
  alpha = 1-sigmoid(b*V(g)$bias+rnorm(vcount(g), 0, noise))
  
  corr_bias = cor(
    c(b[V(g)$bias==-1], b[V(g)$bias==1])
    , c(1-alpha[V(g)$bias==-1], alpha[V(g)$bias==1])
  )
  
  
  V(g)$guess=b
  V(g)$alpha = alpha
  
  
  g$corr_truth = cor(V(g)$guess, V(g)$alpha)
  g$corr_bias = corr_bias
  g
}

N=100
trial = 0
results = data.frame(  
                        mean_start = numeric()
                      , mean_end = numeric()
                      , change_err_mu = numeric()
                      , change_ind_err = numeric()
                      , corr_truth=numeric()
                      , corr_bias=numeric()
                      , noise=numeric()
                      , trial=numeric()
                      , truth=numeric()
                      , centralization=numeric()
                      , prop=numeric()
                      , stringsAsFactors=F
)

filename="TheoreticalFig.csv"
write.csv(results,filename, row.names=F)

### TEST THE EFFECT OF CORRELATION BTW BELIEF & ALPHA
### IN A HOMOGENEOUS DECENTRALIZED POPULATION
replications=10000

for(i in 1:replications) {
 
  gen_bias = function(N, p){
    sample(c(rep(1, round(N*p)),rep(-1, round(N*(1-p)))))
  }
   
  for(noise in c(0,1,5)) {
    for(p in seq(0, 1, by=0.1)) {
        print(i)
        trial=trial+1
        
        
        g=degree.sequence.game(rep(4,N))
        V(g)$bias = gen_bias(N, p)
        pop=generatePopulation(g, noise)
        
        mean(V(g)$bias)
        mean_start = mean(V(pop)$guess)
        output = simDegroot(pop, 5, 0)
        mean_end = mean(output$mean)
        
        output = output %>% mutate(
          err_mu = abs(truth-mean)
        )
        change_err_mu = output$err_mu[output$round==5]-output$err_mu[output$round==0]
        change_ind_err = output$mean_ind_err[output$round==5]-output$mean_ind_err[output$round==0]
        new_results=data.frame(
            mean_start = mean_start
          , mean_end = mean_end
          , change_err_mu = change_err_mu
          , change_ind_err = change_ind_err
          , corr_truth = pop$corr_truth
          , corr_bias = pop$corr_bias
          , noise=noise
          , trial=trial
          , truth = unique(output$truth)
          , centralization=unique(output$deg.cent)
          , prop=mean(V(g)$bias)
          , stringsAsFactors = F
        )
        write.table(new_results, filename, append=T, sep=",", row.names = F, col.names=F)
    }
  }
}

