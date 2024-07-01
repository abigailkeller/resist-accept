##############################
# create transition matrices #
##############################


# load packages
library(tidyverse)
library(parallel)

# define state space (species density)
K <- 100000 # carrying capacity
states <- seq(0,1.4*K,by=1000)

# define action space (units of effort, ranging from 0 to 1)
actions <- seq(0, 1, 0.01)

# function for density dependent growth
growth <- function(Sa,f,Sl,ratio,N,K,alpha,removal){
  R0 <- ratio*f*Sl
  Nprime <- (1-removal)*Sa*N
  E <- Nprime + alpha*K
  M <- (R0-1) / K
  out <- R0/(1+E*M) * E
  return(out)
}

# function for removal rate
removal_rate <- function(a,b,c){
  c*(1-exp(-b*a))
}

create_transition <- function(states,actions,K,shape,scale,removal_params){

  ##############################
  # removal rate stochasticity #
  ##############################

  # create vector of available removal rates
  removal_seq <- seq(0,0.99,by=0.03)
  
  removal_rate_noise <- 0.05
  
  # removal rate stochasticity
  # create empty matrix
  removal_outcome_stoch <- matrix(NA,nrow=length(actions),ncol=length(removal_seq))
  # Loop through all actions
  removal_outcome_stoch[1,] <-  c(1,rep(0,length(removal_seq)-1))
  for (j in seq_along(actions)[2:length(actions)]) {
    removal_rate_stoch <- dnorm(x=removal_seq,
                                mean=removal_rate(a=actions[j],
                                                  b=removal_params$b,
                                                  c=removal_params$c),
                                sd=removal_rate_noise)
    removal_outcome_stoch[j,] <- removal_rate_stoch/sum(removal_rate_stoch)
  }

  #####################
  # growth parameters #
  #####################

  Sa_mean <- 0.82 # adult survival
  Sa_sd <- 0.02
  Sa <- seq(0.75,0.9,0.01)
  Sa_prob <- dnorm(Sa,Sa_mean,Sa_sd)/sum(dnorm(Sa,Sa_mean,Sa_sd))
  
  f_mean <- 192000 # fecundity
  f_sd <- 7000
  f <- seq(170000,215000,5000)
  f_prob <- dnorm(f,f_mean,f_sd)/sum(dnorm(f,f_mean,f_sd))
  
  Sl_mean <- 0.000161 # larval survival
  Sl_sd <- 0.0000171
  Sl <- seq(0.0001,0.00022,0.00001)
  Sl_prob <- dnorm(Sl,Sl_mean,Sl_sd)/sum(dnorm(Sl,Sl_mean,Sl_sd))
  
  ratio <- 0.1 # % of population that reproduces


  ############################
  # create transition matrix #
  ############################


  # create empty transition matrix
  transition <- array(0, dim = c(length(states), length(states), length(actions)))

  ##############################
  # colonization stochasticity #
  ##############################

  # create vector of available alpha
  alpha_seq <- seq(0.01,2,by=0.01)
  # calculate probabilities based on above params
  alpha <- dgamma(alpha_seq,shape=shape,scale=scale)
  sub <- which(alpha > 0.0005)
  if(length(sub)>30){
    step <- floor(length(sub)/30)
    sub2 <- seq(sub[1],last(sub),step)
  } else {
    sub2 <- sub
  }
  alpha <- alpha[sub2]
  alpha_seq <- alpha_seq[sub2]
  alpha_prob <- alpha/sum(alpha)

  # Fill in the transition and utility matrix
  # Loop on all states
  out <- mclapply(1:length(states), function(a) {
    calc_state(a,states,actions,removal_seq,removal_outcome_stoch,
               Sa,Sa_prob,f,f_prob,Sl,Sl_prob,alpha_seq,alpha_prob,
               ratio,K)
  }, mc.cores = 8)
  
  # add results to transition matrix
  for (a in seq_along(states)){
    transition[a,,] <- out[[a]]
  }
  return(transition)
}


calc_state <- function(a,states,actions,removal_seq,removal_outcome_stoch,
                       Sa,Sa_prob,f,f_prob,Sl,Sl_prob,alpha_seq,alpha_prob,
                       ratio,K){
  
  mat <- matrix(NA,nrow=length(states),ncol=length(actions))
  
  for(b in seq_along(actions)){
    
    removal_seq_sub <- removal_seq[which(removal_outcome_stoch[b,] > 0.0005)]
    removal_prob <- removal_outcome_stoch[b,][which(removal_outcome_stoch[b,] > 0.0005)]
    removal_prob <- removal_prob/sum(removal_prob)
    
    # time1 <- Sys.time()
    # create vector to store probabilities
    sprime <- rep(0,length=length(states))
    
    for(c in seq_along(Sa)){
      for(d in seq_along(f)){
        for(e in seq_along(Sl)){
          for(g in seq_along(alpha_seq)){
            for(h in seq_along(removal_seq_sub)){
              
              out <- growth(Sa[c],f[d],Sl[e],ratio,states[a],K,
                            alpha_seq[g],removal_seq_sub[h])
              index <- which.min(abs(states-out))
              sprime[index] <- (
                sprime[index] +
                  Sa_prob[c]*f_prob[d]*Sl_prob[e]*alpha_prob[g]*
                  removal_prob[h]
              )
              
            }
          }
        }
      }
    }
    # print(Sys.time()-time1)
    # add to transition matrix
    mat[,b] <- sprime/sum(sprime)
  }
  return(mat)
}


############
############

# get colonization params
k_vec <- c(0.2,0.5,2,4,15,30,50)
theta_vec <- c(0.025,0.1,0.1,0.125,0.05,1/30,0.025)

result <- create_transition(states,actions,K,shape=k_vec[1],scale=theta_vec[1],
                            removal_params=list(b=4,c=0.8))
#save transition matrix
saveRDS(result,file='data/transition_matrices/20240524_TM_0.2_0.025.rds')

result <- create_transition(states,actions,K,shape=k_vec[2],scale=theta_vec[2],
                            removal_params=list(b=4,c=0.8))
#save transition matrix
saveRDS(result,file='data/transition_matrices/20240524_TM_0.5_0.1.rds')

result <- create_transition(states,actions,K,shape=k_vec[3],scale=theta_vec[3],
                            removal_params=list(b=4,c=0.8))
#save transition matrix
saveRDS(result,file='data/transition_matrices/20240524_TM_2_0.1.rds')

result <- create_transition(states,actions,K,shape=k_vec[4],scale=theta_vec[4],
                            removal_params=list(b=4,c=0.8))
#save transition matrix
saveRDS(result,file='data/transition_matrices/20240524_TM_4_0.125.rds')

result <- create_transition(states,actions,K,shape=k_vec[5],scale=theta_vec[5],
                            removal_params=list(b=4,c=0.8))
#save transition matrix
saveRDS(result,file='data/transition_matrices/20240524_TM_15_0.05.rds')

# result <- create_transition(states,actions,K,shape=k_vec[6],scale=theta_vec[6],
#                             removal_params=list(b=4,c=0.8))
# #save transition matrix
# saveRDS(result,file='data/transition_matrices/20240524_TM_30_0.03.rds')
# 
# result <- create_transition(states,actions,K,shape=k_vec[7],scale=theta_vec[7],
#                             removal_params=list(b=4,c=0.8))
# #save transition matrix
# saveRDS(result,file='data/transition_matrices/20240524_TM_50_0.025.rds')

