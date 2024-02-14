###################
# Create Figure 6 #
###################

# read in data
nonstationary_policies <- readRDS('data/nonstationary_nonstatpolicies.rds')
stationary_policies <- readRDS('data/nonstationary_statpolicies.rds')


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
all_tile <- combine_stat_nonstat_tile(stationary_policies,nonstationary_policies,
                                                 r=c(0,1,1,1,1),imm=c("imm0.3.0.09","imm0.3.0.09","imm2.0.2","imm14.0.05","imm52.0.025"),
                                                 Tmax=125,burnin=25,nperiods=length(transition_filenames_all),
                                                 thresh=c(0.3,0.7))

# plot nonstationary params
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
  theme(text = element_text(size = 12),
        title = element_text(size = 11),
        axis.title=element_text(size=12))
alpha_plot <- ggplot()+
  geom_line(aes(x=1:105,y=c(rep(intensities[1],25),rep(intensities[2],25),
                            rep(intensities[3],25),rep(intensities[4],25),
                            rep(intensities[5],5))),color='#F7941D',linewidth=1.5)+
  labs(x='time',y='colonization\nintensity (\u03B1)')+
  scale_x_continuous(limits=c(0,105),expand=c(0,0))+
  ggtitle('C.')+
  theme_minimal()+
  theme(text = element_text(size = 12),
        title = element_text(size = 11),
        axis.title=element_text(size=12))

# period length
period_length <- round(Tmax/nperiods)

# plot limits
max.c <- max(abs(all_tile$mean_delta_policy))
min.c <- -max(abs(all_tile$mean_delta_policy))

tile_plot <- ggplot(data=all_tile[all_tile$time<=(Tmax-period_length),],
                    aes(x=as.numeric(time),y=as.factor(threshold)))+
  geom_tile(aes(fill=mean_delta_policy))+
  scale_fill_gradientn(colors = c('darkred','red','cornsilk1','cornsilk1','cornsilk1','blue','darkblue'), 
                       values=scales::rescale(c(min.c*5,min.c,-0.05,0,0.05,max.c,max.c*5)),
                       limits=c(min.c,max.c))+
  labs(x='',y='species density',fill='Δ optimal\nremoval\neffort')+
  geom_vline(xintercept=25.5,linetype='dashed')+ 
  geom_vline(xintercept=50.5,linetype='dashed')+
  geom_vline(xintercept=75.5,linetype='dashed')+ 
  geom_vline(xintercept=100.5,linetype='dashed')+ 
  scale_x_continuous(limits=c(0,105),expand=c(0,0))+
  scale_y_discrete(labels=c('Low','Medium','High'))+
  ggtitle('A.')+
  theme_minimal()+
  theme(text = element_text(size = 12),
        title = element_text(size = 11),
        #legend.position = 'None',
        axis.title=element_text(size=12),
        legend.title = element_text(size=12),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        legend.title.align=0.5)

# define plot layout
layout <- "
AAAAA
AAAAA
AAAAA
BBBBB
CCCCC
"
final_plot <- tile_plot+r_plot+alpha_plot+plot_layout(design=layout)
#ggsave('figures/nonstationarity_timeseries_gamma.tiff',final_plot,dpi=700,width=6,height=6)
#ggsave('figures/nonstationarity_timeseries_gamma.svg',final_plot,dpi=700,width=6,height=6)