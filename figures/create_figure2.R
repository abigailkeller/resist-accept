###################
# Create Figure 2 #
###################

# load packages
library(tidyverse)
library(patchwork)

# define state space (species density)
K <- 1 # carrying capacity
states <- seq(0,K,by=0.01)

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
  loss_a=0.21,
  loss_b=3,
  loss_c=1
)

# parameters that scale the relative contribution of removal cost/ecological change in reward function
linear_scale <- 0.001
action_intersect <- 0.75

# function for ecological change -- sigmoidal
reward_sig <- function(s,loss_params){
  loss_params$loss_a/(1+exp(-loss_params$loss_b*(s-loss_params$loss_c)))
}
# function for ecological change -- exponential
reward_exp <- function(s,loss_params){
  loss_params$loss_a*(exp(-loss_params$loss_b*s)-loss_params$loss_c)
}

# function for removal cost -- linear
reward_penalize_linear <- function(s,fun,loss_params,linear_scale,action){
  fun(s,loss_params)-linear_scale*action
}

# function to get nonlinear scale parameter, given linear scale parameter and action intersection
get_nonlinear_scale <- function(linear_scale,action_intersect){
  nonlinear_scale <- linear_scale*action_intersect/(action_intersect^10)
  return(nonlinear_scale)
}

# function for removal cost -- nonlinear
reward_penalize_high <- function(s,fun,loss_params,linear_scale,action_intersect,action){
  fun(s,loss_params)-get_nonlinear_scale(linear_scale,action_intersect)*action^10
}

#######
# plots

cost_plot <- ggplot()+
  geom_line(aes(x=actions,y=-linear_scale*actions),color='mediumorchid',linewidth=1.5)+
  geom_line(aes(x=actions,y=-get_nonlinear_scale(linear_scale,action_intersect)*actions^10),color='green4',linewidth=1.5)+
  ggtitle('B. Removal cost')+
  labs(x='action',y='reward')+
  theme_minimal()+
  theme(text = element_text(size=15),
        title = element_text(size=12))

ecochange_plot <- ggplot()+
  geom_line(aes(x=states,y=reward_sig(states,sig_loss_params)),color='black',linetype='dotted',linewidth=1.5)+
  geom_line(aes(x=states,y=reward_exp(states,exp_loss_params)),color='black',linewidth=1.5)+
  labs(x='state (species density)',y='reward')+
  ggtitle('A. Ecological change')+
  theme_minimal()+
  theme(text = element_text(size=15),
        title = element_text(size=12))

#make fake plots to get line legends
line_data <- data.frame(x=c(1,2,3),
                        y=c(1,2,2),
                        z=c(1,2,2))
eco_plot <- ggplot()+
  geom_line(data=line_data,
            aes(x=x,y=y,linetype=as.factor(y)),linewidth=1.5)+
  scale_linetype_manual(values = c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  labs(linetype='Ecological\nchange\nfunction')+
  theme_minimal()+
  theme(legend.text=element_text(size=10),
        legend.title = element_text(size=12,hjust = 0.5),
        legend.key.width = unit(1.25,'cm'))
eco_legend <- get_legend(eco_plot)
action_plot <- ggplot()+
  geom_line(data=line_data,
            aes(x=x,y=y,color=as.factor(z)),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  labs(color='Removal\ncost\nfunction')+
  theme_minimal()+
  theme(legend.text=element_text(size=10),
        legend.title = element_text(size=12,hjust = 0.5),
        legend.key.width = unit(1.25,'cm'))
action_legend <- get_legend(action_plot)

layout <- '
AAABBBC
AAABBBC
AAABBBD
AAABBBD
'
utility_plot <- ecochange_plot+cost_plot+eco_legend+action_legend+
  plot_layout(design=layout)
ggsave('figures/Figure2_utility_plot.tiff',utility_plot,dpi=600,width=10,height=4)
