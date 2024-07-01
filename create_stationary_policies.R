################################
# Creating stationary policies #
################################


# load packages
library(tidyverse)

# define state space (species density)
K <- 100000 # carrying capacity
states <- seq(0,K*1.4,by=1000)

# define action space (units of effort, ranging from 0 to 1)
actions <- seq(0, 1, 0.01)

# Time horizon
Tmax <- 150

# import transition matrices (i.e. different regimes)
transition_filenames <- c("20240524_TM_0.2_0.025.rds","20240524_TM_0.5_0.1.rds",
                          "20240524_TM_2_0.1.rds","20240524_TM_4_0.125.rds",
                          "20240524_TM_15_0.05.rds","20240524_TM_30_0.03.rds",
                          "20240524_TM_50_0.025.rds","20240524_TM_75_0.02.rds",
                          "20240524_TM_125_0.016.rds","20240524_TM_500_0.01.rds"
                          )


# types of ecological change functions (loss types) and removal cost functions (penaltypes)
losstypes <- c('sig','exp')
penaltypes <- c('hpenal','lpenal')

# function to get utility matrix filenames
get_utility_filenames <- function(filename){
  #split up filename
  sub1 <- str_split(filename,'_')[[1]]
  sub2 <- paste0(str_split(sub1[4],'[.]')[[1]][1],'.',
                 str_split(sub1[4],'[.]')[[1]][2])
  
  out <- paste0(sub1[1],"_UM_",sub1[3],
                  "_",sub2)
  
  utility_filenames <- c()
  
  for(i in 1:length(losstypes)){
    for(j in 1:length(penaltypes)){
      utility_filenames <- append(utility_filenames,paste0(out,"_",penaltypes[j],"_",losstypes[i],".rds"))
    }
  }
  
  return(utility_filenames)
}


# create empty data frame to store policies
out_df <- as.data.frame(matrix(NA,nrow=0,ncol=length(states)+4))
colnames(out_df) <- c('alpha_shape','alpha_scale','penal','loss',states)

# set discount factor
beta <- 0.99

# loop through transition matrices and utility matrices to get stationary policies through backward iteration
for(i in 1:length(transition_filenames)){
  
  #load transition matrix
  transition <- readRDS(paste0('data/transition_matrices/',transition_filenames[i]))
  
  #get associated utility filenames
  utility_filenames <- get_utility_filenames(transition_filenames[i])
  
  for(j in 1:length(utility_filenames)){
    
    #load utility matrix
    utility <- readRDS(paste0('data/utility_matrices/',utility_filenames[j]))
    
    ####
    #run MDP -- backward iteration
    ####
    #mdp_output <- mdp_value_iteration(transition,utility,discount=0.99)
    
    # Action value vector at tmax
    Vtmax <- numeric(length(states))
    
    # Action value vector at t and t+1
    Vt <- numeric(length(states))
    Vtplus <- numeric(length(states))
    
    # Optimal policy vector
    D <- numeric(length(states))
    
    # The backward iteration consists in storing action values in the vector Vt which is the maximum of
    # utility plus the future action values for all possible next states. Knowing the final action 
    # values, we can then backwardly reset the next action value Vtplus to the new value Vt. We start 
    # The backward iteration at time T-1 since we already defined the action #value at Tmax.
    for (t in (Tmax-1):1) {
      
      # We define a matrix Q that stores the updated action values for #all states (rows)
      # actions (columns)
      Q <- array(0, dim=c(length(states), length(actions)))
      
      for (a in 1:length(actions)) {
        
        # For each harvest rate we fill for all states values (row) #the ith column (Action) of matrix Q
        # The utility of the ith action recorded for all states is #added to the product of the transition 
        # matrix of the ith #action by the action value of all states 
        Q[,a] <- utility[,a] + beta*(transition[,,a] %*% Vtplus)
        
      } # end of the harvest loop
      
      # Find the optimal action value at time t is the maximum of Q
      Vt <- apply(Q, 1, max)
      
      # After filling vector Vt of the action values at all states, we #update the vector Vt+1 to Vt and we go to the next step standing #for previous time t-1, since we iterate backward
      Vtplus <- Vt
      
    } # end of the time loop
    
    for (k in seq_along(states)) {
      # We look for each state which column of Q corresponds to the #maximum of the last updated value 
      # of Vt (the one at time t+1). If the index vector is longer than 1 #(if there is more than one optimal value we chose the minimum #harvest rate)
      D[k] <- actions[(min(which(round(Q[k,],13) == round(Vt[k],13))))]
    }
    
    #get optimal policies
    #policy <- actions[mdp_output$policy]
    
    #get metadata from filenames
    alpha_shape <- as.numeric(str_split(transition_filenames[i],'_')[[1]][3])
    alpha_scale <- as.numeric(
      paste0(str_split(str_split(transition_filenames[i],'_')[[1]][4],'[.]')[[1]][1],'.',
             str_split(str_split(transition_filenames[i],'_')[[1]][4],'[.]')[[1]][2]))
    out_penal <- ifelse(length(str_split(utility_filenames[j],'_')[[1]])==6,
                        str_split(utility_filenames[j],'_')[[1]][5],
                        str_split(utility_filenames[j],'_')[[1]][6])
    out_loss <- ifelse(length(str_split(utility_filenames[j],'_')[[1]])==6,
                       str_split(str_split(utility_filenames[j],'_')[[1]][6],'[.]')[[1]][1],
                       str_split(str_split(utility_filenames[j],'_')[[1]][7],'[.]')[[1]][1])
    
    #concatenate data and add to df
    out_df[nrow(out_df)+1,] <- c(alpha_shape,alpha_scale,out_penal,out_loss,D)
  }
}

# save output
saveRDS(out_df,'data/stationary_policies.rds')


