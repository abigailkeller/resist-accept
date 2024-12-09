###########################
# create utility matrices #
###########################

# load packages
library(tidyverse)
library(patchwork)

# define state space (species density)
K <- 100000 # carrying capacity
states <- seq(0,1.4*K,by=1000)
states_scale <- states/K

# action space (units of effort, ranging from 0 to 1)
actions <- seq(0, 1, 0.01)

# loss params -- sigmoidal ecological change
sig_loss_params <- list(
  loss_a=-0.2,
  loss_b=15,
  loss_c=0.4
)
# loss params -- exponential ecological change
exp_loss_params <- list(
  loss_a=0.204,
  loss_b=4,
  loss_c=1
)

# exponent for nonlinear cost function
nonlinear_exp <- 4

# function for ecological change -- sigmoidal
reward_sig <- function(s,loss_params,K){
  reward <- loss_params$loss_a/(1+exp(-loss_params$loss_b*(s-loss_params$loss_c)))
  out <- reward/(max(reward)-min(reward))
  return(out)
}
# function for ecological change -- exponential
reward_exp <- function(s,loss_params,K){
  reward <- loss_params$loss_a*(exp(-loss_params$loss_b*s)-loss_params$loss_c)
  out <- reward/(max(reward)-min(reward))
  return(out)
}

# multi-objective utility function -- linear removal cost
utility_linear <- function(s,fun,loss_params,
                           weights,action){
  out <- weights[1]*fun(s,loss_params)-weights[2]*action
  return(out)
}

# multi-objective utility function -- nonlinear removal cost
utility_nonlinear <- function(s,fun,loss_params,
                              weights,action,nonlinear_exp){
  out <- weights[1]*fun(s,loss_params)-weights[2]*action^nonlinear_exp
  return(out)
}




###########################
# create utility matrices #
###########################

# function that creates utility matrix
create_utility <- function(states,actions,transition,
                           reward_fun1,loss_params,weights,nonlinear_exp){
  
  # Utility matrix -- high action penalization
  utility_penal_high <- array(0, dim = c(length(states), length(actions)))
  
  # Utility matrix -- linear action penalization
  utility_penal_linear <- array(0, dim = c(length(states), length(actions)))
  
  # Fill in the transition and utility matrix
  # Loop on all states
  for (k in seq_along(states)) {
    # Loop on all actions
    for (j in seq_along(actions)) {
      
      # Compute utility based on entire sprime probability distribution -- nonlinear action penalization
      utility_penal_high[k,j] <- sum(transition[k,,j]*utility_nonlinear(states,reward_fun1,loss_params,
                                                                        weights,actions[j],nonlinear_exp))
      
      # Compute utility based on entire sprime probability distribution -- linear action penalization
      utility_penal_linear[k,j] <- sum(transition[k,,j]*utility_linear(states,reward_fun1,loss_params,
                                                                       weights,actions[j]))
      
    } # end of action loop
  } # end of state loop
  return(list(utility_penal_high,utility_penal_linear))
}

# function to get utility filename
get_utility_filename <- function(filename,penaltype,losstype){
  #split up filename
  sub1 <- str_split(str_split(filename,'/')[[1]][3],'_')
  sub2 <- paste0(str_split(sub1[[1]][4],'[.]')[[1]][1],'.',
                 str_split(sub1[[1]][4],'[.]')[[1]][2])
  
  out <- paste0('data/utility_matrices/',sub1[[1]][1],"_UM_",sub1[[1]][3],
                  "_",sub2,"_",penaltype,"_",losstype,".rds")
  return(out)
}

# removal cost types
type <- c('hpenal','lpenal')

# function for saving output
create_utility_output <- function(filename){
  
  #split up filename
  sub1 <- str_split(str_split(filename,'/')[[1]][3],'_')
  sub2 <- str_split(sub1[[1]][4],'[.]')
  
  #sigmoidal
  ##create utility matrix
  out_sig <- create_utility(states_scale,actions,transition=readRDS(filename),
                            reward_fun1=reward_sig,loss_params=sig_loss_params,
                            weights=c(0.98,0.02),nonlinear_exp)
  #save utility matrix
  for(i in 1:length(type)){
    saveRDS(out_sig[[i]],
            file=get_utility_filename(filename,type[i],'sig'))
  }
  
  #exponential
  ##create utility matrix
  out_exp <- create_utility(states_scale,actions,transition=readRDS(filename),
                            reward_fun1=reward_exp,loss_params=exp_loss_params,
                            weights=c(0.98,0.02),nonlinear_exp)
  #save utility matrix
  for(i in 1:length(type)){
    saveRDS(out_exp[[i]],
            file=get_utility_filename(filename,type[i],'exp'))
  }
}

# create utility output
file_list <- c("data/transition_matrices/20240524_TM_0.2_0.025.rds","data/transition_matrices/20240524_TM_0.5_0.1.rds",
               "data/transition_matrices/20240524_TM_2_0.1.rds","data/transition_matrices/20240524_TM_4_0.125.rds",
               "data/transition_matrices/20240524_TM_15_0.05.rds","data/transition_matrices/20240524_TM_30_0.03.rds",
               "data/transition_matrices/20240524_TM_50_0.025.rds","data/transition_matrices/20240524_TM_75_0.02.rds",
               "data/transition_matrices/20240524_TM_125_0.016.rds","data/transition_matrices/20240524_TM_500_0.01.rds"
)


for(i in 1:length(file_list)){
  create_utility_output(file_list[i])
}
