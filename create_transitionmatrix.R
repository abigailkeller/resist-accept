##############################
# create transition matrices #
##############################


# load packages
library(tidyverse)

# define state space (species density)
K <- 1 # carrying capacity
states <- seq(0,K,by=0.01)

# define action space (units of effort, ranging from 0 to 1)
actions <- seq(0, 1, 0.01)

# function for density dependent growth
growth_fun <- function(s,r,K,imm){
  growth <- s + (imm+s*r)*(1-s/K)
  growth <- case_when(growth<0~0,
                      growth>K~K,
                      TRUE~growth)
}

# function for removal rate
removal_rate <- function(a,b,c){
  c*(1-exp(-b*a))
}

create_transition <- function(r,K,shape,scale,removal_params){

  ##############################
  # removal rate stochasticity #
  ##############################

  # create vector of available removal rates
  removal_seq <- seq(0,0.99,by=0.01)
  
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

  #############################
  # growth rate stochasticity #
  #############################

  # set values for growth rate stochasticity -- environmental
  CV_env <- 0.1

  # set values for growth rate stochasticity -- demographic
  CV_dem <- 0.05


  ############################
  # create transition matrix #
  ############################


  # create empty transition matrix
  transition <- array(0, dim = c(length(states), length(states), length(actions)))

  ##############################
  # colonization stochasticity #
  ##############################

  # create vector of available colonization rates
  imm_seq <- seq(0.01,2.5,by=0.01)
  # calculate probabilities based on above params
  imm <- dgamma(imm_seq,shape=shape,scale=scale)
  # normalize
  imm_stoch <- imm/sum(imm)

  # Fill in the transition and utility matrix
  
  # if state = 0
  # create vector to store probabilities
  sprime <- rep(0,length=length(states))

  #get mean s'
  mean_out <- imm_seq

  #calculate stochasticity
  out_stoch <- imm_seq * imm_stoch

  #add to sprime
  sprime[1:100] <- out_stoch[1:100]
  sprime[101] <- sum(out_stoch[101:length(out_stoch)])

  #add to transition matrix
  transition[1,,j] <- sprime/sum(sprime)

  # Loop on all states, s > 0
  for (k in 2:length(states)) {

    # create vector to store probabilities
    sprime <- rep(0,length=length(states))

    # create empty matrix for mean s'
    mean_out <- matrix(NA,nrow=length(removal_seq),ncol=length(imm_seq))

    for(m in seq_along(removal_seq)){
      mean_out[m,] <- growth_fun(s=(1-removal_seq[m])*states[k],
                                 r=r,
                                 K=K,
                                 imm=imm_seq)
    }

    if(r == 0){

      for (j in seq_along(actions)) {

        for(b in seq_along(removal_seq)){
          for(a in seq_along(imm_seq)){

            out <- dnorm(x=states,mean=mean_out[b,a],sd=CV_env*states[k])
            out_stoch <- out/sum(out) * removal_outcome_stoch[j,b] * imm_stoch[a]
            sprime <- sprime + out_stoch
          }
        }
        # add to transition matrix
        transition[k,,j] <- sprime/sum(sprime)
      }
    }

    else{

      # Loop on all actions
      for (j in seq_along(actions)) {

        for(b in seq_along(removal_seq)){
          for(a in seq_along(imm_seq)){

            out <- dnorm(x=states,mean=mean_out[b,a],sd=CV_dem+CV_env*states[k])
            out_stoch <- out/sum(out) * removal_outcome_stoch[j,b] * imm_stoch[a]
            sprime <- sprime + out_stoch
          }
        }
        # add to transition matrix
        transition[k,,j] <- sprime/sum(sprime)

      }
    }
  } # end of state loop
  return(transition)
}

############
############

# get colonization params
k_vec <- c(0.343, 0.45, 1.2, 2, 4, 6, 14, 30, 52, 80, 17.4, 94)
theta_vec <- c(0.0875, 1/3, 0.25, 0.2, 0.125, 0.1, 0.05, 1/30, 0.025, 0.02, 0.04, 0.013)

out <- create_transition(r=0,K=1,shape=k_vec[1],scale=theta_vec[1],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r0_imm0.3.0.09_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[1],scale=theta_vec[1],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm0.3.0.09_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[2],scale=theta_vec[2],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm0.45.0.33_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[3],scale=theta_vec[3],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm1.2.0.25_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[4],scale=theta_vec[4],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm2.0.2_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[5],scale=theta_vec[5],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm4.0.125_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[6],scale=theta_vec[6],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm6.0.1_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[7],scale=theta_vec[7],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm14.0.05_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[8],scale=theta_vec[8],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm30.0.03_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[9],scale=theta_vec[9],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm52.0.025_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[10],scale=theta_vec[10],
                        removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm80.0.02_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[11],scale=theta_vec[11],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm17.4.0.04_gamma.rds')

out <- create_transition(r=1,K=1,shape=k_vec[12],scale=theta_vec[12],
                         removal_params=list(b=6,c=0.7))
#save transition matrix
saveRDS(out,file='transition_matrices/20240104_TM_r1_imm94.0.013_gamma.rds')