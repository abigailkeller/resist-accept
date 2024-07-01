###################
# Create Figure 6 #
###################

# read in data
nonstationary_policies_exp <- readRDS('data/nonstationary_nonstatpolicies_exp_hpenal.rds')
stationary_policies_exp <- readRDS('data/nonstationary_statpolicies_exp_hpenal.rds')
nonstationary_policies_sig <- readRDS('data/nonstationary_nonstatpolicies_sig_hpenal.rds')
stationary_policies_sig <- readRDS('data/nonstationary_statpolicies_sig_hpenal.rds')

K <- 100000 # carrying capacity
states <- seq(0,K*1.4,by=1000)

Tmax <- 40
burnin <- 25
nperiods <- 4


# function to combine stationary and nonstationary dfs
combine_stat_nonstat_tile <- function(stat_df,nonstat_df,alpha_shape,alpha_scale,
                                      Tmax,burnin,nperiods,thresh,states){
  stat_df_long <- stat_df %>% 
    pivot_longer(cols=!c(alpha_shape,alpha_scale),
                 values_to = 'policy_stat', names_to = 'state')
  stat_df_long$state <- round(as.numeric(stat_df_long$state),2)
  
  #period length
  period_length <- round(Tmax/nperiods)
  
  time_matrix <- matrix(NA,nrow=nperiods,ncol=2)
  time_matrix[nperiods,1] <- Tmax+burnin-1
  time_matrix[nperiods,2] <- Tmax-(period_length-1)
  for(i in 1:(nperiods-1)){
    time_matrix[i,1] <- i*period_length
    time_matrix[i,2] <- i*period_length - (period_length-1)
  }
  #add alpha params
  time_df <- cbind(as.data.frame(time_matrix),alpha_shape,alpha_scale)
  
  #create metadata df
  metadata <- as.data.frame(matrix(NA,nrow=0,ncol=3))
  colnames(metadata) <- c('time','alpha_shape','alpha_scale')
  for(i in 1:nperiods){
    temp <- data.frame(time=seq(time_df[i,'V2'],time_df[i,'V1']),
                       alpha_shape=time_df[i,'alpha_shape'],
                       alpha_scale=time_df[i,'alpha_scale'])
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
  out <- left_join(nonstat_df_long,stat_df_long,by=c('alpha_shape','alpha_scale','state')) %>% 
    mutate(delta_policy=policy_nonstat-policy_stat,
           threshold=case_when(state<=thresh[1] ~ 1,
                               state<=thresh[2] & state>thresh[1] ~ 2,
                               state>thresh[2] ~ 3)) %>% 
    group_by(time,alpha_shape,alpha_scale,threshold) %>% 
    summarize(mean_delta_policy=mean(delta_policy))
  return(out)
}

# combine stat and nonstat dfs
all_tile_exp <- combine_stat_nonstat_tile(stat_df=stationary_policies_exp,
                                      nonstat_df=nonstationary_policies_exp,
                                      alpha_shape =c(0.2,0.5,4,30),
                                      alpha_scale = c(0.025,0.1,0.125,0.03),
                                      Tmax=Tmax,burnin=burnin,nperiods=nperiods,
                                      thresh=c(30000,70000),states=states)
all_tile_sig <- combine_stat_nonstat_tile(stat_df=stationary_policies_sig,
                                          nonstat_df=nonstationary_policies_sig,
                                          alpha_shape =c(0.2,0.5,4,30),
                                          alpha_scale = c(0.025,0.1,0.125,0.03),
                                          Tmax=Tmax,burnin=burnin,nperiods=nperiods,
                                          thresh=c(30000,70000),states=states)

# period length
period_length <- round(Tmax/nperiods)

# plot limits
max.c <- max(c(abs(all_tile_exp$mean_delta_policy),
               abs(all_tile_sig$mean_delta_policy)))
min.c <- -max(c(abs(all_tile_exp$mean_delta_policy),
                abs(all_tile_sig$mean_delta_policy)))

tile_plot_exp <- ggplot()+
  geom_tile(data=all_tile_exp[all_tile_exp$time<=(Tmax-period_length),],
            aes(x=as.numeric(time),y=as.factor(threshold),
                fill=mean_delta_policy))+
  scale_fill_gradientn(colors = c('darkred','red','cornsilk1','cornsilk1','cornsilk1','blue','darkblue'), 
                       values=scales::rescale(c(min.c*5,min.c,
                                                -0.05,0,0.05,
                                                max.c,max.c*5)),
                       limits=c(min.c,max.c))+
  labs(x='',y='local EGC density',fill='Δ optimal\nremoval\neffort')+
  geom_vline(xintercept=10.5,linetype='dashed',linewidth=1.25)+ 
  geom_vline(xintercept=20.5,linetype='dashed',linewidth=1.25)+
  geom_vline(xintercept=30.5,linetype='dashed',linewidth=1.25)+ 
  scale_x_continuous(limits=c(0,32),expand=c(0,0))+
  scale_y_discrete(labels=c('Low','Medium','High'))+
  ggtitle('B. Exponential ecological change')+
  theme_minimal()+
  theme(text = element_text(size = 11),
        title = element_text(size = 10),
        #legend.position = 'None',
        axis.title=element_text(size=11),
        legend.title = element_text(size=11),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        legend.title.align=0.5)
tile_plot_sig <- ggplot()+
  geom_tile(data=all_tile_sig[all_tile_sig$time<=(Tmax-period_length),],
            aes(x=as.numeric(time),y=as.factor(threshold),
                fill=mean_delta_policy))+
  scale_fill_gradientn(colors = c('darkred','red','cornsilk1','cornsilk1','cornsilk1','blue','darkblue'), 
                       values=scales::rescale(c(min.c*5,min.c,
                                                -0.05,0,0.05,
                                                max.c,max.c*5)),
                       limits=c(min.c,max.c))+
  labs(x='time',y='local EGC density',fill='Δ optimal\nremoval\neffort')+
  geom_vline(xintercept=10.5,linetype='dashed',linewidth=1.25)+ 
  geom_vline(xintercept=20.5,linetype='dashed',linewidth=1.25)+
  geom_vline(xintercept=30.5,linetype='dashed',linewidth=1.25)+ 
  scale_x_continuous(limits=c(0,32),expand=c(0,0))+
  scale_y_discrete(labels=c('Low','Medium','High'))+
  ggtitle('C. Sigmoidal ecological change')+
  theme_minimal()+
  theme(text = element_text(size = 11),
        title = element_text(size = 10),
        legend.position = 'None',
        axis.title=element_text(size=11),
        legend.title = element_text(size=11),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        legend.title.align=0.5)

# plot nonstationary param
alpha_mu <- c(0.005, 0.05, 0.5, 1)

alpha_plot <- ggplot()+
  geom_line(aes(x=1:32,y=c(rep(alpha_mu[1],10),rep(alpha_mu[2],10),
                           rep(alpha_mu[3],10),rep(alpha_mu[4],2))),
            linewidth=1.5)+
  labs(x='',y='mean \u03B1')+
  scale_x_continuous(limits=c(0,32),expand=c(0,0))+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1),
                     labels=c('0','0.25K','0.50K','0.75K','K'))+
  ggtitle('A. Non-stationary dynamics')+
  theme_minimal()+
  theme(text = element_text(size = 11),
        title = element_text(size = 10),
        axis.title=element_text(size=11))

# define plot layout
layout <- "
AAAAA
BBBBB
CCCCC
"
final_plot <- alpha_plot+tile_plot_exp+tile_plot_sig+plot_layout(design=layout)
#ggsave('figures/nonstationarity_timeseries.svg',final_plot,dpi=700,width=5,height=5)
ggsave('figures/Figure5_nonstationarity_timeseries.tiff',final_plot,dpi=700,width=5,height=5)
