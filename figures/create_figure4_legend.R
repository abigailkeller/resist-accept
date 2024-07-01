##########################
# Create Figure 4 legend #
##########################

# load packages
library(tidyverse)
library(patchwork)

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

# parameters that scale the relative contribution of removal cost/ecological change in reward function
linear_scale <- 0.005
action_intersect <- 0.75
nonlinear_exp <- 4

# function for ecological change -- sigmoidal
reward_sig <- function(s,loss_params,K){
  out <- loss_params$loss_a/(1+exp(-loss_params$loss_b*(s-loss_params$loss_c)))
  return(out)
}
# function for ecological change -- exponential
reward_exp <- function(s,loss_params,K){
  out <- loss_params$loss_a*(exp(-loss_params$loss_b*s)-loss_params$loss_c)
  return(out)
}

# function for removal cost -- linear
reward_penalize_linear <- function(s,K,fun,loss_params,linear_scale,action){
  fun(s,loss_params,K)-linear_scale*action
}

# function to get nonlinear scale parameter, given linear scale parameter and action intersection
get_nonlinear_scale <- function(linear_scale,action_intersect,nonlinear_exp){
  nonlinear_scale <- linear_scale*action_intersect/(action_intersect^nonlinear_exp)
  return(nonlinear_scale)
}

# function for removal cost -- nonlinear
reward_penalize_high <- function(s,K,fun,loss_params,linear_scale,action_intersect,action,nonlinear_exp){
  fun(s,loss_params,K)-get_nonlinear_scale(linear_scale,action_intersect,nonlinear_exp)*action^nonlinear_exp
}

######
# create dfs

cost_df <- data.frame(
  action = c(actions,actions),
  cost = c(-linear_scale*actions,
           -get_nonlinear_scale(linear_scale,action_intersect,nonlinear_exp)*actions^nonlinear_exp),
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
  scale_y_continuous(breaks=c(-0.01,-0.005,0),labels=c(-0.01,-0.005,0))+
  labs(x='removal effort',y='reward',color='')+
  ggtitle('Removal cost\nfunction')+
  theme_minimal()+
  theme(text = element_text(size=10),
        plot.title = element_text(size=9,
                             hjust = 0.5))

ecochange_plot <- ggplot()+
  geom_line(data=eco_df,
            aes(x=state,y=reward,linetype=type),linewidth=1)+
  scale_x_continuous(breaks=c(0,K),labels=c('0','K'),
                     limits=c(0,K*1.1))+
  scale_y_continuous(breaks=c(-0.2,-0.1,0),labels=c(-0.2,-0.1,0))+
  labs(x='local EGC density',y='reward',linetype='')+
  ggtitle('Ecological change\nfunction')+
  theme_minimal()+
  theme(text = element_text(size=10),
        plot.title = element_text(size=9,
                             hjust = 0.5))


ggsave('figures/figure4_legend_cost.svg',cost_plot,dpi=600,width=2.5,height=1.5)
ggsave('figures/figure4_legend_eco.svg',ecochange_plot,dpi=600,width=2.5,height=1.5)
