#################################
# create nonstationary policies #
#################################


#load packages
library(tidyverse)
library(patchwork)
library(viridis)
library(ggpubr)
library(RColorBrewer)

# define state space (species density)
K <- 100000 # carrying capacity
states <- seq(0,K*1.4,by=1000)

# define action space (units of effort, ranging from 0 to 1)
actions <- seq(0, 1, 0.01)

# load filenames
transition_filenames_all <- c("20240524_TM_0.2_0.025.rds","20240524_TM_0.5_0.1.rds",
                              "20240524_TM_4_0.125.rds",
                              "20240524_TM_30_0.03.rds"
                              )

# set ecological change functions (loss types) and removal cost functions (penaltypes)
losstypes <- c('sig','exp')
penaltypes <- c('hpenal','lpenal')

# function to get utility matrix filename
get_utility_filename_single <- function(filename,losstype,penaltype){
  #split up filename
  sub1 <- str_split(filename,'_')[[1]]
  sub2 <- paste0(str_split(sub1[4],'[.]')[[1]][1],'.',
                 str_split(sub1[4],'[.]')[[1]][2])
  
  out <- paste0(sub1[1],"_UM_",sub1[3],
                "_",sub2,"_",penaltype,"_",losstype,".rds")
  
  return(out)
}


#############################################
# function for getting nonstationary policies

# Time horizon
Tmax <- 40

# burnin time
burnin <- 25

# number of stationary periods
nperiods <- length(transition_filenames_all)

get_policies_transition_nonstationary <- function(transitions,losstype,penaltype,Tmax,burnin,nperiods){
  #period length
  period_length <- round(Tmax/nperiods)
  
  # set discount factor
  beta <- 0.99
  
  time_matrix <- matrix(NA,nrow=nperiods,ncol=2)
  time_matrix[nperiods,1] <- Tmax+burnin-1
  time_matrix[nperiods,2] <- Tmax-(period_length-1)
  for(i in 1:(nperiods-1)){
    time_matrix[i,1] <- i*period_length
    time_matrix[i,2] <- i*period_length - (period_length-1)
  }
  
  ###
  #do by hand
  # Action value vector at tmax
  Vtmax <- numeric(length(states))
  
  # Action value vector at t+1
  Vtplus <- numeric(length(states))
  
  # Optimal policy vector
  D <- matrix(NA,nrow=length(states),ncol=Tmax+burnin-1)
  
  #create matrix to store optimal actions at time, t
  Vt <- matrix(NA,nrow=length(states),ncol=Tmax+burnin-1)
  
  ##
  for (p in nperiods:1){
    for (t in time_matrix[p,1]:time_matrix[p,2]) {
      
      transition <- readRDS(paste0('data/transition_matrices/',transitions[p]))
      
      utility <- readRDS(paste0('data/utility_matrices/',
                                get_utility_filename_single(transitions[p],losstype,penaltype)))
      
      # We define a matrix Q that stores the updated action values for #all states (rows)
      # actions (columns)
      Q <- array(0, dim=c(length(states), length(actions)))
      
      for (i in 1:length(actions)) {
        
        # For each harvest rate we fill for all states values (row) #the ith column (Action) of matrix Q
        # The utility of the ith action recorded for all states is #added to the product of the transition matrix of the ith #action by the action value of all states 
        Q[,i] <- utility[,i] + beta*(transition[,,i] %*% Vtplus)
        
      } # end of the harvest loop
      
      # Find the optimal action value at time t is the maximum of Q
      Vt[,t] <- apply(Q, 1, max)
      
      # After filling vector Vt of the action values at all states, we #update the vector Vt+1 to Vt and we go to the next step standing #for previous time t-1, since we iterate backward
      Vtplus <- Vt[,t]
      
      for (k in seq_along(states)) {
        # We look for each state which column of Q corresponds to the #maximum of the last updated value 
        # of Vt (the one at time t+1). If the index vector is longer than 1 #(if there is more than one optimal value we chose the minimum #harvest rate)
        D[k,t] <- actions[(min(which(round(Q[k,],12) == round(Vt[k,t],12))))]
      }
      
    } # end of the time loop
  }
  return(D)
}


############
# create function for getting stationary policies
get_stationary_policies <- function(transitions,losstype,penaltype,Tmax){
  #create empty data frame
  out_df <- as.data.frame(matrix(NA,nrow=0,ncol=length(states)+2))
  colnames(out_df) <- c('alpha_shape','alpha_scale',states)
  
  for(i in 1:length(transitions)){
    
    #load transition matrix
    transition <- readRDS(paste0('data/transition_matrices/',transitions[i]))
    
    #get associated utility filenames
    utility <- readRDS(paste0('data/utility_matrices/',
                              get_utility_filename_single(transitions[i],losstype,penaltype)))
    
    ####
    #run MDP -- backward iteration
      
    # Action value vector at tmax
    Vtmax <- numeric(length(states))
      
    # Action value vector at t and t+1
    Vt <- numeric(length(states))
    Vtplus <- numeric(length(states))
      
    # Optimal policy vector
    D <- numeric(length(states))
      
    # Backward iteration
    for (t in (Tmax-1):1) {
      # We define a matrix Q that stores the updated action values for #all states (rows)
      # actions (columns)
      Q <- array(0, dim=c(length(states), length(actions)))
        
      for (a in 1:length(actions)) {
          
        # For each harvest rate we fill for all states values (row) #the ith column (Action) of matrix Q
        # The utility of the ith action recorded for all states is #added to the product of the transition matrix of the ith #action by the action value of all states 
        Q[,a] <- utility[,a] + 0.99*(transition[,,a] %*% Vtplus)
          
      } # end of the harvest loop
        
      # Find the optimal action value at time t is the maximum of Q
      Vt <- apply(Q, 1, max)
        
      # After filling vector Vt of the action values at all states, we #update the vector Vt+1 to Vt and we go to the next step standing #for previous time t-1, since we iterate backward
      Vtplus <- Vt
        
    } # end of the time loop
    
    for (k in seq_along(states)) {
      # We look for each state which column of Q corresponds to the #maximum of the last updated value 
      # of Vt (the one at time t+1). If the index vector is longer than 1 #(if there is more than one optimal value we chose the minimum #harvest rate)
      D[k] <- actions[(min(which(round(Q[k,],12) == round(Vt[k],12))))]
    }
      
    #get metadata from filenames
    alpha_shape <- as.numeric(str_split(transitions[i],'_')[[1]][3])
    alpha_scale <- as.numeric(
      paste0(str_split(str_split(transitions[i],'_')[[1]][4],'[.]')[[1]][1],'.',
             str_split(str_split(transitions[i],'_')[[1]][4],'[.]')[[1]][2]))
      
    #concatenate data and add to df
    out_df[nrow(out_df)+1,] <- c(alpha_shape,alpha_scale,D)
  }
  return(out_df)
}

##
## get nonstationary policies
nonstationary_policies <- get_policies_transition_nonstationary(transitions=transition_filenames_all,
                                                            losstype='exp',penaltype='hpenal',Tmax=Tmax,burnin=burnin,
                                                            nperiods=length(transition_filenames_all))



##
## get stationary policies
stationary_policies <- get_stationary_policies(transitions=transition_filenames_all,
                                                      losstype='exp',penaltype='hpenal',Tmax=Tmax+burnin)

# save
saveRDS(nonstationary_policies,'data/nonstationary_nonstatpolicies_exp_hpenal.rds')
saveRDS(stationary_policies,'data/nonstationary_statpolicies_exp_hpenal.rds')
  
