
simDegroot = function(g, rounds, TRUTH=0, return.distribution=F) {
  g$deg.cent = centralization.degree(g, loops=F)$centralization
  GAME_LENGTH=rounds
  output = data.frame()
  V(g)$new_guess=NA
  for(i in 0:GAME_LENGTH) {
    output_row = data.frame(
      N=vcount(g),
      deg.cent=g$deg.cent,
      truth=TRUTH,
      mean = mean(V(g)$guess),
      median = median((V(g)$guess)),
      mean_ind_err = mean(abs(V(g)$guess-TRUTH)),
      core = V(g)[1]$guess,
      sd.pool=sd(V(g)$guess),
      round = i,
      stringsAsFactors=F
    )
    output = rbind(output, output_row)
    this_guess = V(g)$guess
    V(g)$guess = V(g)$guess*V(g)$alpha + (1-V(g)$alpha)*(V(g)$guess %*% as_adj(g, sparse=F) / rep(1, vcount(g)) %*% as_adj(g, sparse=F))
  }
  output
  if(return.distribution) {
    return(list(
      distribution=this_guess,
      output=output
    ))
  } else { return(output) }
}