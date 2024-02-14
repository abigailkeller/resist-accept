# get transition filenames
get_transition_nonstat <- function(nregime,alpha,r1){
  
  r <- rep('1',nregime)
  r[1] <- ifelse(r1=='0','1','1')
  
  transition_filenames <- list()
  
  for(i in 1:nregime){
    transition_filenames <- append(transition_filenames,
                                  case_when(r[i]=='0'&& alpha[i]=='0.00' ~ '20240104_TM_r0_imm0.3.0.09_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='0.00' ~ '20240104_TM_r1_imm0.3.0.09_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='0.02' ~ '20240104_TM_r1_imm0.45.0.33_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='0.04' ~ '20240104_TM_r1_imm1.2.0.25_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='0.06' ~ '20240104_TM_r1_imm2.0.2_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='0.07' ~ '20240104_TM_r1_imm4.0.125_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='0.12' ~ '20240104_TM_r1_imm6.0.1_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='0.14' ~ '20240104_TM_r1_imm14.0.05_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='0.69' ~ '20240104_TM_r1_imm30.0.03_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='0.99' ~ '20240104_TM_r1_imm52.0.025_gamma.rds',
                                            r[i]=='1'&& alpha[i]=='1.00' ~ '20240104_TM_r1_imm80.0.02_gamma.rds')
    )
  }
  
  return(transition_filenames)
}


get_nonstat_nonstationary <- function(alpha,r1,ecotype,nregime,ratio,
                                      discount,states,actions,sig_loss_params,
                                      exp_loss_params,Tmax=125,burnin=25){
  #period length
  period_length <- Tmax/as.numeric(nregime)
  
  # crate matrix of regime transitions
  time_matrix <- case_when(nregime == '5' ~ cbind(c(25,50,75,100,149),c(1,26,51,76,101)),
                           nregime == '4' ~ cbind(c(31,63,94,149,NA),c(1,32,64,95,NA)),
                           nregime == '3' ~ cbind(c(42,83,149,NA,NA),c(1,43,84,NA,NA)),
                           nregime == '2' ~ cbind(c(63,149,NA,NA,NA),c(1,64,NA,NA,NA)))
  
  # get transition filenames
  transition_filenames <- get_transition_nonstat(nregime,alpha,r1)
    
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
  for (p in nregime:1){
    for (t in time_matrix[p,1]:time_matrix[p,2]) {
      
      transition <- readRDS(paste0('transition/',transition_filenames[p]))
      
      if(ecotype == 'Sigmoidal'){
        utility <- create_utility_sig(states,actions,transition,sig_loss_params,'Nonlinear',ratio)
      } else if(ecotype == 'Exponential'){
        utility <- create_utility_exp(states,actions,transition,exp_loss_params,'Nonlinear',ratio)
      }
      
      # We define a matrix Q that stores the updated action values for #all states (rows)
      # actions (columns)
      Q <- array(0, dim=c(length(states), length(actions)))
      
      for (i in 1:length(actions)) {
        
        # For each harvest rate we fill for all states values (row) #the ith column (Action) of matrix Q
        # The utility of the ith action recorded for all states is #added to the product of the transition matrix of the ith #action by the action value of all states 
        Q[,i] <- utility[,i] + discount*(transition[,,i] %*% Vtplus)
        
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
#create function for getting stationary policies
get_nonstat_stationary <- function(alpha,r1,ecotype,nregime,discount,ratio,
                                   states,actions,sig_loss_params,
                                   exp_loss_params,Tmax=125){
  
  r <- rep('1',nregime)
  r[1] <- ifelse(r1=='0','1','1')
  
  # get transition filenames
  transition_filenames <- get_transition_nonstat(nregime,alpha,r1)
  
  #create empty data frame
  out_df <- as.data.frame(matrix(NA,nrow=0,ncol=length(states)+2))
  colnames(out_df) <- c('r','alpha',states)
  
  for(i in 1:length(transition_filenames)){
    
    transition <- readRDS(paste0('transition/',transition_filenames[i]))
    
    if(ecotype == 'Sigmoidal'){
      utility <- create_utility_sig(states,actions,transition,sig_loss_params,'Nonlinear',ratio)
    } else if(ecotype == 'Exponential'){
      utility <- create_utility_exp(states,actions,transition,exp_loss_params,'Nonlinear',ratio)
    }
    
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
        Q[,a] <- utility[,a] + discount*(transition[,,a] %*% Vtplus)
        
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
    
    #concatenate data and add to df
    out_df[nrow(out_df)+1,] <- c(r[i],alpha[i],D)
  }
  return(out_df)
}


#combine nonstat and stat dfs
combine_stat_nonstat_tile <- function(stat_df,nonstat_df,r1,alpha,nregime,states,
                                      Tmax=125,burnin=25,thresh=c(0.3,0.7)){
  
  r <- rep('1',nregime)
  r[1] <- ifelse(r1=='0','1','1')
  
  stat_df_long <- stat_df %>% 
    pivot_longer(cols=!c(r,alpha),
                 values_to = 'policy_stat', names_to = 'state')
  stat_df_long$state <- round(as.numeric(stat_df_long$state),2)
  stat_df_long$r <- as.numeric(stat_df_long$r)
  stat_df_long$policy_stat <- as.numeric(stat_df_long$policy_stat)
  
  #period length
  period_length <- Tmax/as.numeric(nregime)
  
  # crate matrix of regime transitions
  time_matrix <- case_when(nregime == '5' ~ cbind(c(25,50,75,100,149),c(1,26,51,76,101)),
                           nregime == '4' ~ cbind(c(31,63,94,149,NA),c(1,32,64,95,NA)),
                           nregime == '3' ~ cbind(c(42,83,149,NA,NA),c(1,43,84,NA,NA)),
                           nregime == '2' ~ cbind(c(63,149,NA,NA,NA),c(1,64,NA,NA,NA)))
  
  #add r and alpha
  time_df <- cbind(as.data.frame(na.omit(time_matrix)),r,alpha)
  
  #create metadata df
  metadata <- as.data.frame(matrix(NA,nrow=0,ncol=3))
  colnames(metadata) <- c('time','r','alpha')
  for(i in 1:as.numeric(nregime)){
    temp <- data.frame(time=seq(time_df[i,'V2'],time_df[i,'V1']),
                       r=time_df[i,'r'],
                       alpha=time_df[i,'alpha'])
    metadata <- rbind(metadata,temp)
  }
  
  colnames(nonstat_df) <- 1:(Tmax+burnin-1)
  nonstat_df_long <- as.data.frame(nonstat_df) %>% 
    mutate(state=states) %>% 
    pivot_longer(cols=!state,
                 values_to = 'policy_nonstat', names_to = 'time')
  nonstat_df_long$time <- as.numeric(nonstat_df_long$time)
  nonstat_df_long$state <- round(as.numeric(nonstat_df_long$state),2)
  
  #add metadata
  nonstat_df_long <- left_join(nonstat_df_long,metadata,by='time')
  nonstat_df_long$r <- as.numeric(nonstat_df_long$r)
  
  #join with stat_df -- group by threshold
  out1 <- left_join(nonstat_df_long,stat_df_long,by=c('r','alpha','state')) %>% 
    mutate(delta_policy=policy_nonstat-policy_stat,
           threshold=case_when(state<=thresh[1] ~ 1,
                               state<=thresh[2] & state>thresh[1] ~ 2,
                               state>thresh[2] ~ 3)) %>% 
    group_by(time,r,alpha,threshold) %>% 
    summarize(mean_delta_policy=mean(delta_policy))
  
  #join with stat_df -- keep policies
  out2 <- left_join(nonstat_df_long,stat_df_long,by=c('r','alpha','state'))
  
  return(list(out1,out2))
}

create_plot_deltaoptim <- function(out,r1,alpha,nregime,
                                   Tmax=125,burnin=25){
  #period length
  period_length <- round(Tmax/as.numeric(nregime))
  
  # plot limits
  max.c <- max(abs(out$mean_delta_policy))
  min.c <- -max(abs(out$mean_delta_policy))
  
  # make sure max and min are large enough
  max.c <- ifelse(max.c<0.01,0.01,max.c)
  min.c <- -max.c
  
  # base plot
  base_plot <- ggplot(data=out[out$time<=(Tmax-period_length),],
                      aes(x=as.numeric(time),y=as.factor(threshold)))+
    geom_tile(aes(fill=mean_delta_policy))+
    scale_fill_gradientn(colors = c('darkred','red','cornsilk1','cornsilk1','cornsilk1','blue','darkblue'), 
                         values=scales::rescale(c(min.c*5,min.c,-0.01,0,0.01,max.c,max.c*5)),
                         limits=c(min.c,max.c))+
    labs(x='time',y='species density',fill='Δ optimal\nremoval\neffort')+
    scale_y_discrete(labels=c('Low','Medium','High'))+
    theme_minimal()+
    theme(text = element_text(size=16),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          legend.title.align=0.5)
  
  if(nregime == '2'){
    final_plot <- base_plot +
      scale_x_continuous(limits = c(0,75),expand=c(0,0))+
      geom_vline(xintercept=63.5,linetype='dashed')
  } else if(nregime == '3'){
    final_plot <- base_plot +
      geom_vline(xintercept=42.5,linetype='dashed')+
      geom_vline(xintercept=83.5,linetype='dashed')+
      scale_x_continuous(limits = c(0,95),expand=c(0,0))
  } else if(nregime == '4'){
    final_plot <- base_plot +
      geom_vline(xintercept=31.5,linetype='dashed')+
      geom_vline(xintercept=63.5,linetype='dashed')+
      scale_x_continuous(limits = c(0,75),expand=c(0,0))
  } else if(nregime == '5'){
    final_plot <- base_plot +
      geom_vline(xintercept=25.5,linetype='dashed')+
      geom_vline(xintercept=50.5,linetype='dashed')+
      geom_vline(xintercept=75.5,linetype='dashed')+
      geom_vline(xintercept=100.5,linetype='dashed')+
      scale_x_continuous(limits = c(0,110),expand=c(0,0))
  }
  
  return(final_plot)
}

create_hoverplot <- function(out,x){
  subdata <- out[out$time==x,]%>% 
    pivot_longer(cols=!c(state,time,r,alpha),
                 values_to = 'effort', names_to = 'type')
  
  plot <- ggplot() +
    geom_line(data=subdata,
              aes(x=state,y=effort,color=type),linewidth=1.75)+
    labs(x='state (species density)',y='optimal policy',color='')+
    scale_color_manual(values=c('darkorchid','deepskyblue'),
                       labels=c('Non-stationary','Stationary'))+
    theme_minimal()+
    theme(text = element_text(size=16))
  
  return(plot)
}

