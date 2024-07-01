#function that creates utility matrix -- sigmoidal ecological reward
create_utility_sig <- function(states,actions,transition,loss_params,penaltype,ratio){
  
  action_intersect <- 0.75
  linear_scale <- ratio*0.2/action_intersect
  nonlinear_exp <- 4
  
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
        
        utility[k,j] <- sum(transition[k,,j]*reward_penalize_linear(states_scale,reward_sig,loss_params, 
                                                                    linear_scale, actions[j]))
      } else if(penaltype == 'Nonlinear'){
        
        utility[k,j] <- sum(transition[k,,j]*reward_penalize_high(states_scale,reward_sig,loss_params,
                                                                  linear_scale,action_intersect,actions[j],
                                                                  nonlinear_exp))
      }
      
    } # end of action loop
  } # end of state loop
  return(utility)
}


#function that creates utility matrix -- exponential ecological reward
create_utility_exp <- function(states,actions,transition,loss_params,penaltype,ratio){
  
  action_intersect <- 0.75
  linear_scale <- ratio*0.2
  nonlinear_exp <- 4
  
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
        
        utility[k,j] <- sum(transition[k,,j]*reward_penalize_linear(states_scale,reward_exp,loss_params, 
                                                                    linear_scale, actions[j]))
      } else if(penaltype == 'Nonlinear'){
        
        utility[k,j] <- sum(transition[k,,j]*reward_penalize_high(states_scale,reward_exp,loss_params,
                                                                  linear_scale,action_intersect,actions[j],
                                                                  nonlinear_exp))
      }
      
    } # end of action loop
  } # end of state loop
  return(utility)
}

#ecological reward -- sigmoidal
reward_sig <- function(s,loss_params){
  loss_params$loss_a/(1+exp(-loss_params$loss_b*(s-loss_params$loss_c)))
}

#ecological reward -- exponential
reward_exp <- function(s,loss_params){
  loss_params$loss_a*(exp(-loss_params$loss_b*s)-loss_params$loss_c)
}

#reward -- penalize effort -- linear
reward_penalize_linear <- function(s,fun,loss_params,linear_scale,action){
  fun(s,loss_params)-linear_scale*action
}

#function to get nonlinear scale parameter, given linear scale parameter and action intersection
get_nonlinear_scale <- function(linear_scale,action_intersect,nonlinear_exp){
  nonlinear_scale <- linear_scale*action_intersect/(action_intersect^nonlinear_exp)
  return(nonlinear_scale)
}

#reward -- penalize high effort -- nonlinear
reward_penalize_high <- function(s,fun,loss_params,linear_scale,action_intersect,action,
                                 nonlinear_exp){
  fun(s,loss_params)-get_nonlinear_scale(linear_scale,action_intersect,nonlinear_exp)*action^nonlinear_exp
}
