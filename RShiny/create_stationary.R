create_stationary_df <- function(ecotype,penaltype,weights,
                              states,actions,sig_loss_params,
                              exp_loss_params,nonlinear_exp,
                              discount_factor,source_pop){
  
  #create empty data frame
  out_df <- as.data.frame(matrix(NA,nrow=0,ncol=length(states)+2))
  colnames(out_df) <- c('penal','loss',states)
  
  #set discount factor
  beta <- discount_factor
  
  #load transition matrix
  transition <- readRDS(paste0('transition/',get_transition(source_pop)))
  
  # append dataframe
  
  if(penaltype == 'Both' | penaltype == 'Nonlinear'){
    
    if(ecotype == 'Both' | ecotype == 'Sigmoidal'){
      
      D <- iterate_backward(states,actions,transition,
                            create_utility_sig(states,actions,transition,
                                               sig_loss_params,'Nonlinear',weights,
                                               nonlinear_exp),
                            beta)
      out_df[nrow(out_df)+1,] <- c('Nonlinear','Sigmoidal',D)
      
    }
    
    if(ecotype == 'Both' | ecotype == 'Exponential'){
      
      D <- iterate_backward(states,actions,transition,
                            create_utility_exp(states,actions,transition,
                                               exp_loss_params,'Nonlinear',weights,
                                               nonlinear_exp),
                            beta)
      out_df[nrow(out_df)+1,] <- c('Nonlinear','Exponential',D)
      
    }
    
  }
  
  if(penaltype == 'Both' | penaltype == 'Linear'){
    
    if(ecotype == 'Both' | ecotype == 'Sigmoidal'){
      
      D <- iterate_backward(states,actions,transition,
                            create_utility_sig(states,actions,transition,
                                               sig_loss_params,'Linear',weights,
                                               nonlinear_exp),
                            beta)
      out_df[nrow(out_df)+1,] <- c('Linear','Sigmoidal',D)
        
    }
    
    if(ecotype == 'Both' | ecotype == 'Exponential'){
      
      D <- iterate_backward(states,actions,transition,
                            create_utility_exp(states,actions,transition,
                                               exp_loss_params,'Linear',weights,
                                               nonlinear_exp),
                            beta)
      out_df[nrow(out_df)+1,] <- c('Linear','Exponential',D)
      
    }
    
  } 
  
  
  #create long df
  out_df_long <- out_df %>% 
    pivot_longer(cols=!c(penal,loss),
                 values_to = 'policy', names_to = 'state')
  
  
  #convert to numeric
  out_df_long$state <- as.numeric(out_df_long$state)
  out_df_long$policy <- as.numeric(out_df_long$policy)
  
  
  return(out_df_long)
  
}
  
create_stationary_plot <- function(df,penaltype,ecotype){
  
  # plot
  if(penaltype == 'Both'){
    if(ecotype == 'Both'){
      plot <- ggplot()+
        geom_line(data=df,
                  aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.75)+
        scale_color_manual(values=c('mediumorchid','green4'),
                           labels=c('Linear','Nonlinear'))+
        scale_y_continuous(limits=c(0,1))+
        scale_x_continuous(breaks=c(0,100000),labels=c(0,'K'),
                           limits=c(0,110000))+
        labs(x='state (EGC density)',y='optimal removal effort',
             color='Action\npenalty\nfunction',
             linetype='Ecological\nchange\nfunction')+
        theme_minimal()+
        theme(text = element_text(size=16),
              legend.title = element_text(hjust=0.5),
              legend.key.width = unit(1.25,'cm'))
    } else if(ecotype == 'Sigmoidal'){
      plot <- ggplot()+
        geom_line(data=df,
                  aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.75)+
        scale_color_manual(values=c('mediumorchid','green4',),
                           labels=c('Linear','Nonlinear'))+
        scale_linetype_manual(values = c('dotted'),
                              labels=c('Sigmoidal'))+
        scale_x_continuous(breaks=c(0,100000),labels=c(0,'K'),
                           limits=c(0,110000))+
        scale_y_continuous(limits=c(0,1))+
        labs(x='state (EGC density)',y='optimal removal effort',
             color='Action\npenalty\nfunction',
             linetype='Ecological\nchange\nfunction')+
        theme_minimal()+
        theme(text = element_text(size=16),
              legend.title = element_text(hjust=0.5),
              legend.key.width = unit(1.25,'cm'))
    } else if(ecotype == 'Exponential'){
      plot <- ggplot()+
        geom_line(data=df,
                  aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.75)+
        scale_color_manual(values=c('mediumorchid','green4',),
                           labels=c('Linear','Nonlinear'))+
        scale_linetype_manual(values = c('solid'),
                              labels=c('Exponential'))+
        scale_x_continuous(breaks=c(0,100000),labels=c(0,'K'),
                           limits=c(0,110000))+
        scale_y_continuous(limits=c(0,1))+
        labs(x='state (EGC density)',y='optimal removal effort',
             color='Action\npenalty\nfunction',
             linetype='Ecological\nchange\nfunction')+
        theme_minimal()+
        theme(text = element_text(size=16),
              legend.title = element_text(hjust=0.5),
              legend.key.width = unit(1.25,'cm'))
    } 
    
    } else if(penaltype == 'Linear'){
    if(ecotype == 'Both'){
      plot <- ggplot()+
        geom_line(data=df,
                  aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.75)+
        scale_color_manual(values=c('mediumorchid'),
                           labels=c('Linear'))+
        scale_x_continuous(breaks=c(0,100000),labels=c(0,'K'),
                           limits=c(0,110000))+
        scale_y_continuous(limits=c(0,1))+
        labs(x='state (EGC density)',y='optimal removal effort',
             color='Action\npenalty\nfunction',
             linetype='Ecological\nchange\nfunction')+
        theme_minimal()+
        theme(text = element_text(size=16),
              legend.title = element_text(hjust=0.5),
              legend.key.width = unit(1.25,'cm'))
    } else if(ecotype == 'Sigmoidal'){
      plot <- ggplot()+
        geom_line(data=df,
                  aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.75)+
        scale_color_manual(values=c('mediumorchid'),
                           labels=c('Linear'))+
        scale_linetype_manual(values = c('dotted'),
                              labels=c('Sigmoidal'))+
        scale_x_continuous(breaks=c(0,100000),labels=c(0,'K'),
                           limits=c(0,110000))+
        scale_y_continuous(limits=c(0,1))+
        labs(x='state (EGC density)',y='optimal removal effort',
             color='Action\npenalty\nfunction',
             linetype='Ecological\nchange\nfunction')+
        theme_minimal()+
        theme(text = element_text(size=16),
              legend.title = element_text(hjust=0.5),
              legend.key.width = unit(1.25,'cm'))
    } else if(ecotype == 'Exponential'){
      plot <- ggplot()+
        geom_line(data=df,
                  aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.75)+
        scale_color_manual(values=c('mediumorchid'),
                           labels=c('Linear'))+
        scale_linetype_manual(values = c('solid'),
                              labels=c('Exponential'))+
        scale_x_continuous(breaks=c(0,100000),labels=c(0,'K'),
                           limits=c(0,110000))+
        scale_y_continuous(limits=c(0,1))+
        labs(x='state (EGC density)',y='optimal removal effort',
             color='Action\npenalty\nfunction',
             linetype='Ecological\nchange\nfunction')+
        theme_minimal()+
        theme(text = element_text(size=16),
              legend.title = element_text(hjust=0.5),
              legend.key.width = unit(1.25,'cm'))
    }
      } else if(penaltype == 'Nonlinear'){
    if(ecotype == 'All'){
      plot <- ggplot()+
        geom_line(data=df,
                  aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.75)+
        scale_color_manual(values=c('green4'),
                           labels=c('Nonlinear'))+
        scale_x_continuous(breaks=c(0,100000),labels=c(0,'K'),
                           limits=c(0,110000))+
        scale_y_continuous(limits=c(0,1))+
        labs(x='state (EGC density)',y='optimal removal effort',
             color='Action\npenalty\nfunction',
             linetype='Ecological\nchange\nfunction')+
        theme_minimal()+
        theme(text = element_text(size=16),
              legend.title = element_text(hjust=0.5),
              legend.key.width = unit(1.25,'cm'))
    } else if(ecotype == 'Sigmoidal'){
      plot <- ggplot()+
        geom_line(data=df,
                  aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.75)+
        scale_color_manual(values=c('green4'),
                           labels=c('Nonlinear'))+
        scale_linetype_manual(values = c('dotted'),
                              labels=c('Sigmoidal'))+
        scale_x_continuous(breaks=c(0,100000),labels=c(0,'K'),
                           limits=c(0,110000))+
        scale_y_continuous(limits=c(0,1))+
        labs(x='state (EGC density)',y='optimal removal effort',
             color='Action\npenalty\nfunction',
             linetype='Ecological\nchange\nfunction')+
        theme_minimal()+
        theme(text = element_text(size=16),
              legend.title = element_text(hjust=0.5),
              legend.key.width = unit(1.25,'cm'))
    } else if(ecotype == 'Exponential'){
      plot <- ggplot()+
        geom_line(data=df,
                  aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.75)+
        scale_color_manual(values=c('green4'),
                           labels=c('Nonlinear'))+
        scale_linetype_manual(values = c('solid'),
                              labels=c('Exponential'))+
        scale_x_continuous(breaks=c(0,100000),labels=c(0,'K'),
                           limits=c(0,110000))+
        scale_y_continuous(limits=c(0,1))+
        labs(x='state (EGC density)',y='optimal removal effort',
             color='Action\npenalty\nfunction',
             linetype='Ecological\nchange\nfunction')+
        theme_minimal()+
        theme(text = element_text(size=16),
              legend.title = element_text(hjust=0.5),
              legend.key.width = unit(1.25,'cm'))
    } 
  }
  return(plot)
}
  
  

get_transition <- function(source_pop){
  
  
  transition_filename <- case_when(source_pop=='0.005K' ~ '20240524_TM_0.2_0.025.rds',
                                   source_pop=='0.05K' ~ '20240524_TM_0.5_0.1.rds',
                                   source_pop=='0.2K' ~ '20240524_TM_2_0.1.rds',
                                   source_pop=='0.5K' ~ '20240524_TM_4_0.125.rds',
                                   source_pop=='0.75K' ~ '20240524_TM_15_0.05.rds',
                                   source_pop=='1.0K' ~ '20240524_TM_30_0.03.rds',
                                   source_pop=='1.25K' ~ '20240524_TM_50_0.025.rds',
                                   source_pop=='1.5K' ~ '20240524_TM_75_0.02.rds',
                                   source_pop=='2.0K' ~ '20240524_TM_125_0.016.rds',
                                   source_pop=='5.0K' ~ '20240524_TM_500_0.01.rds')
  
  return(transition_filename)
}


iterate_backward <- function(states,actions,transition,utility,beta){
  
  # Time horizon
  Tmax <- 150
  
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
      # The utility of the ith action recorded for all states is #added to the product of the transition matrix of the ith #action by the action value of all states 
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
  
  
  return(D)
}
