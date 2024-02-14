#################################################
# Create nonstationary policies -- supplemental #
#################################################


# load packages
library(tidyverse)
library(MDPtoolbox)
library(patchwork)
library(viridis)
library(ggpubr)
library(RColorBrewer)

# define state space (species density)
K <- 1
states <- seq(0,K,by=0.01)

# define action space (units of effort, ranging from 0 to 1)
actions <- seq(0, 1, 0.01)

# load transition matrix filenames
transition_filenames_all <- c("20240104_TM_r0_imm0.3.0.09_gamma.rds","20240104_TM_r1_imm0.3.0.09_gamma.rds",
                              "20240104_TM_r1_imm2.0.2_gamma.rds","20240104_TM_r1_imm14.0.05_gamma.rds",
                              "20240104_TM_r1_imm52.0.025_gamma.rds"
)

# set ecological change functions (loss types) and removal cost functions (penaltypes)
losstypes <- c('sig','exp')
penaltypes <- c('hpenal')

# function to get utility filename
get_utility_filename_single <- function(filename,losstype,penaltype){
  #split up filename
  sub1 <- str_split(filename,'_')
  sub2 <- str_split(sub1[[1]][4],'[.]')
  
  
  if(length(sub1[[1]])==4){
    out <- paste0(sub1[[1]][1],"_UM_",sub1[[1]][3],
                  "_",sub1[[1]][4],"_",penaltype,"_",losstype,".rds")
  } else {
    sub3 <- str_split(sub1[[1]][5],'[.]')
    out <- paste0(sub1[[1]][1],"_UM_",sub1[[1]][3],
                  "_",sub1[[1]][4],"_",sub3[[1]][1],"_",penaltype,"_",losstype,".rds")
  }
  
  return(out)
}


######################################
# create function for getting nonstationary policies

# Time horizon
Tmax <- 125

# burnin
burnin <- 25

# number of stationary periods
nperiods <- length(transition_filenames_all)

get_policies_transition_nonstationary <- function(transitions,losstype,penaltype,Tmax,burnin,nperiods){
  #period length
  period_length <- round(Tmax/nperiods)
  
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
        Q[,i] <- utility[,i] + 0.995*(transition[,,i] %*% Vtplus)
        
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
  colnames(out_df) <- c('r','imm',states)
  
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
    out_r <- as.numeric(str_split(str_split(transitions[i],'_')[[1]][3],'')[[1]][2])
    full_imm <- str_split(transitions[i],'_')[[1]][4]
      
    #concatenate data and add to df
    out_df[nrow(out_df)+1,] <- c(out_r,full_imm,D)
  }
  return(out_df)
}
  
##
## get nonstationary policies
out_sig_hpenal_all <- get_policies_transition_nonstationary(transitions=transition_filenames_all,
                                                             losstype='sig',penaltype='hpenal',Tmax=125,burnin=25,
                                                            nperiods=length(transition_filenames_all))
out_exp_hpenal_all <- get_policies_transition_nonstationary(transitions=transition_filenames_all,
                                                             losstype='exp',penaltype='hpenal',Tmax=125,burnin=25,
                                                            nperiods=length(transition_filenames_all))



##
#get stationary policies
out_sig_hpenal_station_all <- get_stationary_policies(transitions=transition_filenames_all,
                                                        losstype='sig',penaltype='hpenal',Tmax=125+25)
out_exp_hpenal_station_all <- get_stationary_policies(transitions=transition_filenames_all,
                                                        losstype='exp',penaltype='hpenal',Tmax=125+25)


# function to combine stationary and nonstationary dfs
combine_stat_nonstat_tile <- function(stat_df,nonstat_df,r,imm,Tmax,burnin,nperiods,thresh){
  stat_df_long <- stat_df %>% 
    pivot_longer(cols=!c(r,imm),
                 values_to = 'policy_stat', names_to = 'state')
  stat_df_long$state <- round(as.numeric(stat_df_long$state),2)
  stat_df_long$r <- as.numeric(stat_df_long$r)
  stat_df_long$policy_stat <- as.numeric(stat_df_long$policy_stat)
  
  #period length
  period_length <- round(Tmax/nperiods)
  
  time_matrix <- matrix(NA,nrow=nperiods,ncol=2)
  time_matrix[nperiods,1] <- Tmax+burnin-1
  time_matrix[nperiods,2] <- Tmax-(period_length-1)
  for(i in 1:(nperiods-1)){
    time_matrix[i,1] <- i*period_length
    time_matrix[i,2] <- i*period_length - (period_length-1)
  }
  #add r and imm
  time_df <- cbind(as.data.frame(time_matrix),r,imm)
  
  #create metadata df
  metadata <- as.data.frame(matrix(NA,nrow=0,ncol=3))
  colnames(metadata) <- c('time','r','imm')
  for(i in 1:nperiods){
    temp <- data.frame(time=seq(time_df[i,'V2'],time_df[i,'V1']),
                       r=time_df[i,'r'],
                       imm=time_df[i,'imm'])
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
  
  #join with stat_df
  out <- left_join(nonstat_df_long,stat_df_long,by=c('r','imm','state')) %>% 
    mutate(delta_policy=policy_nonstat-policy_stat,
           threshold=case_when(state<=thresh[1] ~ 1,
                               state<=thresh[2] & state>thresh[1] ~ 2,
                               state>thresh[2] ~ 3)) %>% 
    group_by(time,r,imm,threshold) %>% 
    summarize(mean_delta_policy=mean(delta_policy))
  return(out)
}



# combine stat and nonstat dfs
sig_hpenal_all_tile <- combine_stat_nonstat_tile(out_sig_hpenal_station_all,out_sig_hpenal_all,
                                               r=c(0,1,1,1,1),imm=c("imm0.3.0.09","imm0.3.0.09","imm2.0.2","imm14.0.05","imm52.0.025"),
                                               Tmax=125,burnin=25,nperiods=length(transition_filenames_all),
                                               thresh=c(0.3,0.7))
exp_hpenal_all_tile <- combine_stat_nonstat_tile(out_exp_hpenal_station_all,out_exp_hpenal_all,
                                                 r=c(0,1,1,1,1),imm=c("imm0.3.0.09","imm0.3.0.09","imm2.0.2","imm14.0.05","imm52.0.025"),
                                                 Tmax=125,burnin=25,nperiods=length(transition_filenames_all),
                                               thresh=c(0.3,0.7))



# tile plot of delta action
plot_all <- function(out,title,min.c,max.c,legend=TRUE,lab.x='time',lab.y='state (species density)'){
  
  if(legend==TRUE){
    plot <- ggplot(data=out,
                   aes(x=as.numeric(time),y=as.factor(threshold)))+
      geom_tile(aes(fill=mean_delta_policy))+
      scale_fill_gradientn(colors = c('darkred','red','cornsilk1','cornsilk1','cornsilk1','blue','darkblue'), 
                           values=scales::rescale(c(min.c*5,min.c,-0.05,0,0.05,max.c,max.c*5)),
                           limits=c(min.c,max.c))+
      labs(x=lab.x,y=lab.y,fill='Δ optimal\nremoval\neffort')+
      scale_x_continuous(limits=c(0,109),expand=c(0,0))+
      scale_y_discrete(labels=c('Low','Medium','High'))+
      geom_vline(xintercept=25.5,linetype='dashed')+ 
      geom_vline(xintercept=50.5,linetype='dashed')+
      geom_vline(xintercept=75.5,linetype='dashed')+ 
      geom_vline(xintercept=100.5,linetype='dashed')+ 
      #geom_vline(xintercept=125.5,linetype='dashed')+
      ggtitle(title)+
      theme_minimal()+
      theme(axis.text = element_text(size = 12),
            legend.text=element_text(size=12),
            legend.title = element_text(size=14,hjust = 0.5),
            legend.key.size = unit(0.5,'cm'),
            axis.title.y=element_text(size = 15),
            title = element_text(size = 14),
            plot.margin = margin(0, 0, 0, 0, "cm"))
  } else {
    plot <- ggplot(data=out,
                   aes(x=as.numeric(time),y=as.factor(threshold)))+
      geom_tile(aes(fill=mean_delta_policy))+
      scale_fill_gradientn(colors = c('darkred','red','cornsilk1','cornsilk1','cornsilk1','blue','darkblue'), 
                           values=scales::rescale(c(min.c*5,min.c,-0.05,0,0.05,max.c,max.c*5)),
                           limits=c(min.c,max.c))+
      labs(x=lab.x,y=lab.y,fill='Δ optimal\nremoval\neffort')+
      geom_vline(xintercept=25.5,linetype='dashed')+ 
      geom_vline(xintercept=50.5,linetype='dashed')+
      geom_vline(xintercept=75.5,linetype='dashed')+ 
      geom_vline(xintercept=100.5,linetype='dashed')+ 
      #geom_vline(xintercept=125.5,linetype='dashed')+
      scale_x_continuous(limits=c(0,109),expand=c(0,0))+
      scale_y_discrete(labels=c('Low','Medium','High'))+
      ggtitle(title)+
      theme_minimal()+
      theme(axis.text = element_text(size = 12),
            title = element_text(size = 14),
            legend.position = 'None',
            axis.title.y=element_text(size = 15),
            plot.margin = margin(0, 0, 0, 0, "cm"))
  }
  
  return(plot)
}

##
##plot
# period length
period_length <- round(Tmax/nperiods)

# find max and min
tile_plot_max_hpenal <- max(max(sig_hpenal_all_tile[sig_hpenal_all_tile$time<=(Tmax-period_length),'mean_delta_policy']),
                            max(exp_hpenal_all_tile[exp_hpenal_all_tile$time<=(Tmax-period_length),'mean_delta_policy']))
tile_plot_min_hpenal <- min(min(sig_hpenal_all_tile[sig_hpenal_all_tile$time<=(Tmax-period_length),'mean_delta_policy']),
                            min(exp_hpenal_all_tile[exp_hpenal_all_tile$time<=(Tmax-period_length),'mean_delta_policy']))
max.c <- max(abs(tile_plot_max_hpenal),abs(tile_plot_min_hpenal))
min.c <- -max(abs(tile_plot_max_hpenal),abs(tile_plot_min_hpenal))

# tile plot of delta action
all_sig_hpenal_plot <- plot_all(sig_hpenal_all_tile[sig_hpenal_all_tile$time<=(Tmax-period_length),],
                                title='',
                                min.c=min.c,
                                max.c=max.c, legend=FALSE, lab.x='', lab.y='') +
  geom_point(aes(x=105,y=2),size=5,color='green4',shape=15)
all_exp_hpenal_plot <- plot_all(exp_hpenal_all_tile[exp_hpenal_all_tile$time<=(Tmax-period_length),],
                                title='A.',
                                min=min.c,
                                max=max.c, legend=FALSE, lab.x='', lab.y='') +
  geom_point(aes(x=105,y=2),size=5,color='green4',shape=16)

all_exp_hpenal_plot_wlegend <- plot_all(exp_hpenal_all_tile[exp_hpenal_all_tile$time<=(Tmax-period_length),],
                                title='',
                                min=min.c,
                                max=max.c, lab.x='',lab.y='')

legend <- get_legend(all_exp_hpenal_plot_wlegend)

#make fake plots to get icon legends
icon_data <- data.frame(x=c(1,2,3),
                        y=c(1,2,2),
                        z=c(1,2,2))

shape_plot <- ggplot()+
  geom_point(data=icon_data,
             aes(x=x,y=y,shape=as.factor(y)),size=4)+
  scale_shape_manual(values=c(16,15),
                     labels=c('Exponential','Sigmoidal'))+
  labs(shape='Ecological\nchange\nfunction')+
  theme_minimal()+
  theme(legend.text=element_text(size=12),
        legend.title = element_text(size=14,hjust = 0.5),
        legend.key.size = unit(0.5,'cm'),
        axis.text = element_text(size = 14),
        axis.title=element_text(size = 16),
        title = element_text(size = 18),
        
  )
shape_legend <- get_legend(shape_plot)
color_plot <- ggplot()+
  geom_point(data=icon_data,
             aes(x=x,y=y,color=as.factor(z)),size=4)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  labs(color='Action\npenalty\nfunction')+
  theme_minimal()+
  theme(legend.text=element_text(size=12),
        legend.title = element_text(size=14,hjust = 0.5),
        legend.key.size = unit(0.5,'cm'),
        axis.text = element_text(size = 14),
        axis.title=element_text(size = 16),
        title = element_text(size = 18))
color_legend <- get_legend(color_plot)
        
#plot nonstationary params
mu <- c(0.03,0.03, 0.4, 0.7, 1.0)

ratio <- c(11, 11, 5, 20, 30)

var_vec <- mu/ratio

get_theta <- function(mu,var){
  theta <- var/mu
  return(theta)
}
get_k <- function(mu,theta){
  k <- mu/theta
  return(k)
}

k_vec <- rep(NA,length(mu))
theta_vec <- rep(NA,length(mu))

# calculate k and theta
for(i in 1:length(mu)){
  theta_vec[i] <- get_theta(mu[i],var_vec[i])
  k_vec[i] <- get_k(mu[i],theta_vec[i])
}

# colonization intensity
calc_intensity <- function(k,theta){
  intensity <- 1 - pgamma(q=0.9,shape=k,scale=theta)
  return(intensity)
}

intensities <- rep(NA,length(mu))
for(i in 1:length(mu)){
  intensities[i] <- calc_intensity(k_vec[i],theta_vec[i])
}

r_plot <- ggplot()+
  geom_line(aes(x=1:105,y=c(rep(0,25),rep(1,80))),color='#00AEEF',linewidth=1.5)+
  labs(x='',y='growth\nrate (r)')+
  scale_y_continuous(breaks=c(0,0.5,1))+
  scale_x_continuous(limits=c(0,105),expand=c(0,0))+
  ggtitle('B.')+
  theme_minimal()+
  theme(text = element_text(size = 15),
        axis.title=element_text(size=14))
alpha_plot <- ggplot()+
  geom_line(aes(x=1:105,y=c(rep(intensities[1],25),rep(intensities[2],25),
                            rep(intensities[3],25),rep(intensities[4],25),
                            rep(intensities[5],5))),color='#F7941D',linewidth=1.5)+
  labs(x='time',y='colonization\nintensity (\u03B1)')+
  scale_x_continuous(limits=c(0,105),expand=c(0,0))+
  scale_y_continuous(breaks=c(0,0.3,0.6))+
  ggtitle('C.')+
  theme_minimal()+
  theme(text = element_text(size = 15),
        axis.title=element_text(size=14))


#define plot layout
layout2 <- "
AAAAA#
AAAAAE
BBBBBE
BBBBBF
CCCCCF
DDDDD#
"
final_plot <- all_exp_hpenal_plot+all_sig_hpenal_plot+
  r_plot+alpha_plot+legend+shape_legend+plot_layout(design=layout2)
ggsave('figures/supplemental_nonstationarity_timeseries.tiff',final_plot,dpi=700,width=8,height=8)
#ggsave('figures/nonstationarity_timeseries.svg',final_plot,dpi=700,width=6,height=8)