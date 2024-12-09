##########################
# Create Figure 4 legend #
##########################

# load packages
library(tidyverse)
library(patchwork)

# get objective weights
weights <- c(0.98, 0.02)

# define state space (species density)
K <- 1 # carrying capacity
states <- seq(0,K*1.3,by=0.01)

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
  loss_a=0.204,
  loss_b=4,
  loss_c=1
)

# exponent for nonlinear cost function
nonlinear_exp <- 4

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

######
# create dfs

cost_df <- data.frame(
  action = c(actions,actions),
  cost = c(-actions,
           -actions^nonlinear_exp),
  type = c(rep('linear',length(actions)),rep('nonlinear',length(actions)))
)

eco_df <- data.frame(
  state = c(states,states),
  reward = c(reward_sig(states,sig_loss_params,K),
             reward_exp(states,exp_loss_params,K)),
  type = c(rep('sigmoidal',length(states)),rep('exponential',length(states)))
)

#######
# plots

cost_plot <- ggplot()+
  geom_line(data=cost_df,
            aes(x=action,y=cost,color=type),linewidth=1)+
  scale_color_manual(values=c('mediumorchid','green4'))+
  scale_x_continuous(breaks=c(0,1),labels=c(0,1))+
  scale_y_continuous(breaks=c(0,-1),labels=c(0,-weights[2]))+
  labs(x='removal effort (a)',
       y = bquote("weighted value (" * w[c] ~ V[c] * ")"),color='')+
  ggtitle('Removal cost\nfunction')+
  theme_minimal()+
  theme(text = element_text(size=8),
        plot.title = element_text(size=9,
                             hjust = 0.5))

ecochange_plot <- ggplot()+
  geom_line(data=eco_df,
            aes(x=state,y=reward,linetype=type),linewidth=1)+
  scale_x_continuous(breaks=c(0,K),labels=c('0','K'),
                     limits=c(0,K*1.1))+
  scale_y_continuous(breaks=c(0,-1),labels=c(0,-weights[1]))+
  labs(x='local EGC density (s)', 
       y = bquote("weighted value (" * w[d] ~ V[d] * ")"), linetype ='')+
  ggtitle('Ecological change\nfunction')+
  theme_minimal()+
  theme(text = element_text(size=8),
        plot.title = element_text(size=9,
                             hjust = 0.5))


ggsave('figures/figure4_legend_cost.svg',cost_plot,dpi=600,width=2.5,height=1.5)
ggsave('figures/figure4_legend_eco.svg',ecochange_plot,dpi=600,width=2.5,height=1.5)
