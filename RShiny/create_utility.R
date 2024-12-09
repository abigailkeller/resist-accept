#function that creates utility matrix -- sigmoidal ecological reward
create_utility_sig <- function(states,actions,transition,
                               loss_params,penaltype,weights,
                               nonlinear_exp){
  
  K <- 100000 # carrying capacity
  states_scale <- states/K
  
  #Utility matrix
  utility <- array(0, dim = c(length(states_scale), length(actions)))
  
  # Fill in the transition and utility matrix
  # Loop on all states
  for (k in seq_along(states)) {
    # Loop on all actions
    for (j in seq_along(actions)) {
      
      if(penaltype == 'Linear'){
        
        utility[k,j] <- sum(transition[k,,j]*utility_linear(states_scale,reward_sig,loss_params,
                                                            weights,actions[j]))
      } else if(penaltype == 'Nonlinear'){
        
        utility[k,j] <- sum(transition[k,,j]*utility_nonlinear(states_scale,reward_sig,loss_params,
                                                               weights,actions[j],nonlinear_exp))
      }
      
    } # end of action loop
  } # end of state loop
  return(utility)
}


#function that creates utility matrix -- exponential ecological reward
create_utility_exp <- function(states,actions,transition,
                               loss_params,penaltype,weights,
                               nonlinear_exp){
  
  K <- 100000 # carrying capacity
  states_scale <- states/K
  
  #Utility matrix
  utility <- array(0, dim = c(length(states_scale), length(actions)))
  
  # Fill in the transition and utility matrix
  # Loop on all states
  for (k in seq_along(states)) {
    # Loop on all actions
    for (j in seq_along(actions)) {
      
      if(penaltype == 'Linear'){
        
        utility[k,j] <- sum(transition[k,,j]*utility_linear(states_scale,reward_exp,loss_params,
                                                            weights,actions[j]))
      } else if(penaltype == 'Nonlinear'){
        
        utility[k,j] <- sum(transition[k,,j]*utility_nonlinear(states_scale,reward_exp,loss_params,
                                                               weights,actions[j],nonlinear_exp))
      }
      
    } # end of action loop
  } # end of state loop
  return(utility)
}

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
