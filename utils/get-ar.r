### This code was written by Patrick Walker and Charlie for the LMIC global report
### Edited by Ettie for USA States

### functions needed to calculate final sizes###
### outputs the 'error' relative to true final size 
output_set<-function(inf_pops,rates,mixing,demog,inits){
  return(sapply(seq_along(inf_pops),function(x){
    ((demog[x]-inits[x])/inf_pops[x]*(1-exp(-sum(rates[x]*mixing[x,]*(inf_pops-inits)/demog)))-1)
  }))
}

### homogeneous solution
single_set<-function(inf_pop, rate, pop, init){
  diff=(pop-init)/inf_pop*(1-exp(-rate*(inf_pop-init)/pop))-1
  return(diff*diff)
}

### outputs the sum of squares
sum_output_set<-function(inf_pops,rates,mixing,demog,inits){
  if(min(inf_pops<=0)){return(rep(1000,length(inf_pops)))}
  outset=output_set(inf_pops,rates,mixing,demog,inits)
  return(sum(outset*outset))
}

### get the final size for a set contact matrix and R0
get_AR <- function(R, rmod, c_mat, demog, init, guess_hom, guess_modifiers, 
                   iterates, reiterates, restarts, tol){
  ## modifier for next generation matrix to get required R0,1,0.6,5,100000,10,0.00000001
  tot_pop <- sum(demog)
  single_init = init
  ## distribute initial infection across demographics
  init_demog = single_init*demog/tot_pop
  ## get homogenous solution
  fit_err = 1
  guess_inf = guess_hom*tot_pop
  ## store final homogenous attack rata
  fit_single <- optim(guess_inf, single_set, rate=R, pop=tot_pop, init=single_init,
                      method="Brent", lower=0, upper=tot_pop, 
                      control=list(abstol=tol, maxit=iterates))
  while(fit_err>0.0000001){
    ###use homogeouns solution as first  guess  
    fit<-nmkb(fit_single$par*demog/sum(demog)*guess_modifiers, sum_output_set, lower=0, 
              upper=demog, rates=rmod, mixing=c_mat, demog=demog, inits=init_demog,
              control=list(maxfeval=iterates,tol=tol,restarts.max=restarts))
    for(f in 1:reiterates){
      ## reiterate to make sure it finds the solution
      fit<-nmkb(fit$par, sum_output_set, lower=0, upper=demog, rates=rmod,
                mixing=c_mat, demog=demog, inits=init_demog, 
                control=list(maxfeval=iterates,tol=tol,restarts.max=restarts))
    }
    fit_err=fit$value
    guess_modifiers <- guess_modifiers+0.05
  }
  return(fit)
}

