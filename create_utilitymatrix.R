###########################
# create utility matrices #
###########################

# load packages
library(tidyverse)
library(patchwork)

# define state space (species density)
K <- 1 # carrying capacity
states <- seq(0,K,by=0.01)

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
  loss_a=0.21,
  loss_b=3,
  loss_c=1
)

# parameters that scale the relative contribution of removal cost/ecological change in reward function
linear_scale <- 0.001
action_intersect <- 0.75

# function for ecological change -- sigmoidal
reward_sig <- function(s,loss_params){
  loss_params$loss_a/(1+exp(-loss_params$loss_b*(s-loss_params$loss_c)))
}
# function for ecological change -- exponential
reward_exp <- function(s,loss_params){
  loss_params$loss_a*(exp(-loss_params$loss_b*s)-loss_params$loss_c)
}

# function for removal cost -- linear
reward_penalize_linear <- function(s,fun,loss_params,linear_scale,action){
  fun(s,loss_params)-linear_scale*action
}

# function to get nonlinear scale parameter, given linear scale parameter and action intersection
get_nonlinear_scale <- function(linear_scale,action_intersect){
  nonlinear_scale <- linear_scale*action_intersect/(action_intersect^10)
  return(nonlinear_scale)
}

# function for removal cost -- nonlinear
reward_penalize_high <- function(s,fun,loss_params,linear_scale,action_intersect,action){
  fun(s,loss_params)-get_nonlinear_scale(linear_scale,action_intersect)*action^10
}




###########################
# create utility matrices #
###########################

# function that creates utility matrix
create_utility <- function(states,actions,transition,reward_fun1,loss_params){
  
  # Utility matrix -- high action penalization
  utility_penal_high <- array(0, dim = c(length(states), length(actions)))
  
  # Utility matrix -- linear action penalization
  utility_penal_linear <- array(0, dim = c(length(states), length(actions)))
  
  # Fill in the transition and utility matrix
  # Loop on all states
  for (k in seq_along(states)) {
    # Loop on all actions
    for (j in seq_along(actions)) {
      
      # Compute utility based on entire sprime probability distribution -- action penalization
      utility_penal_high[k,j] <- sum(transition[k,,j]*reward_penalize_high(states,reward_fun1,loss_params,
                                                                           linear_scale,action_intersect,actions[j]))
      
      # Compute utility based on entire sprime probability distribution -- action penalization
      utility_penal_linear[k,j] <- sum(transition[k,,j]*reward_penalize_linear(states,reward_fun1,loss_params, 
                                                                               linear_scale, actions[j]))
      
    } # end of action loop
  } # end of state loop
  return(list(utility_penal_high,utility_penal_linear))
}

# function to get utility filename
get_utility_filename <- function(filename,penaltype,losstype){
  #split up filename
  sub1 <- str_split(str_split(filename,'/')[[1]][3],'_')
  #sub2 <- str_split(sub1[[1]][4],'[.]')
  
  if(length(sub1[[1]])==4){
    out <- paste0('data/utility_matrices/',sub1[[1]][1],"_UM_",sub1[[1]][3],
                  "_",sub1[[1]][4],"_",penaltype,"_",losstype,".rds")
  } else {
    sub3 <- str_split(sub1[[1]][5],'[.]')
    out <- paste0('data/utility_matrices/',sub1[[1]][1],"_UM_",sub1[[1]][3],
                  "_",sub1[[1]][4],"_",sub3[[1]][1],"_",penaltype,"_",losstype,".rds")
  }
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
  out_sig <- create_utility(states,actions,transition=readRDS(filename),
                            reward_fun1=reward_sig,loss_params=sig_loss_params)
  #save utility matrix
  for(i in 1:length(type)){
    saveRDS(out_sig[[i]],
            file=get_utility_filename(filename,type[i],'sig'))
  }
  
  #exponential
  ##create utility matrix
  out_exp <- create_utility(states,actions,transition=readRDS(filename),
                            reward_fun1=reward_exp,loss_params=exp_loss_params)
  #save utility matrix
  for(i in 1:length(type)){
    saveRDS(out_exp[[i]],
            file=get_utility_filename(filename,type[i],'exp'))
  }
}

# create utility output
file_list <- c("data/transition_matrices/20240104_TM_r0_imm0.3.0.09_gamma.rds","data/transition_matrices/20240104_TM_r1_imm0.3.0.09_gamma.rds",
               "data/transition_matrices/20240104_TM_r1_imm0.45.0.33_gamma.rds","data/transition_matrices/20240104_TM_r1_imm1.2.0.25_gamma.rds",
               "data/transition_matrices/20240104_TM_r1_imm2.0.2_gamma.rds","data/transition_matrices/20240104_TM_r1_imm4.0.125_gamma.rds",
               "data/transition_matrices/20240104_TM_r1_imm6.0.1_gamma.rds","data/transition_matrices/20240104_TM_r1_imm14.0.05_gamma.rds",
               "data/transition_matrices/20240104_TM_r1_imm30.0.03_gamma.rds","data/transition_matrices/20240104_TM_r1_imm52.0.025_gamma.rds",
               "data/transition_matrices/20240104_TM_r1_imm80.0.02_gamma.rds"
)

for(i in 1:length(file_list)){
  create_utility_output(file_list[i])
}
