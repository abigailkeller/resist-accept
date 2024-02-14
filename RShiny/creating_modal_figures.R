#load packages
library(tidyverse)
library(patchwork)
library(cowplot)


#################
# reward function
#################

#define states
#states (relative density) 
K <- 1
#states <- 0:K/K
states <- seq(0,K,by=0.01)

#vector of actions: units of effort, ranging from 0 to 1
actions <- seq(0, 1, 0.01)

#loss params -- sigmoidal
sig_loss_params <- list(
  loss_a=-0.2,
  loss_b=15,
  loss_c=0.4
)
#loss params -- exponential
exp_loss_params <- list(
  loss_a=0.21,
  loss_b=3,
  loss_c=1
)


linear_scale <- 0.001
action_intersect <- 0.75

# ecological change function -- sigmoidal
reward_sig <- function(s,loss_params){
  loss_params$loss_a/(1+exp(-loss_params$loss_b*(s-loss_params$loss_c)))
}
# ecological change function -- exponential
reward_exp <- function(s,loss_params){
  loss_params$loss_a*(exp(-loss_params$loss_b*s)-loss_params$loss_c)
}

#reward -- penalize effort -- linear
reward_penalize_linear <- function(s,fun,loss_params,linear_scale,action){
  fun(s,loss_params)-linear_scale*action
}

#function to get nonlinear scale parameter, given linear scale parameter and action intersection
get_nonlinear_scale <- function(linear_scale,action_intersect){
  nonlinear_scale <- linear_scale*action_intersect/(action_intersect^10)
  return(nonlinear_scale)
}

#reward -- penalize high effort -- nonlinear
reward_penalize_high <- function(s,fun,loss_params,linear_scale,action_intersect,action){
  fun(s,loss_params)-get_nonlinear_scale(linear_scale,action_intersect)*action^10
}
penalty_plot <- ggplot()+
  geom_line(aes(x=actions,y=-linear_scale*actions),color='mediumorchid',linewidth=1.5)+
  geom_line(aes(x=actions,y=-get_nonlinear_scale(linear_scale,action_intersect)*actions^10),color='green4',linewidth=1.5)+
  geom_hline(aes(yintercept=-0.00075),color='red',linewidth=1, linetype = 'dashed')+
  scale_y_continuous(breaks=c(-0.00075),
                     labels=c(expression(scale[R])))+
  ggtitle('Removal cost')+
  labs(x='action',y='reward')+
  theme_minimal()+
  theme(text = element_text(size=15),
        title = element_text(size=12),
        axis.text.y = element_text(colour="red"))
#ggsave('figures/penalty_plot_PICES.tiff',penalty_plot,dpi=200,width=4,height=4)

reward_plot <- ggplot()+
  geom_line(aes(x=states,y=reward_sig(states,sig_loss_params)),color='black',linetype='dotted',linewidth=1.5)+
  geom_line(aes(x=states,y=reward_exp(states,exp_loss_params)),color='black',linewidth=1.5)+
  geom_hline(aes(yintercept=-0.2),color='red',linewidth=1, linetype = 'dashed')+
  scale_y_continuous(breaks=c(-0.2),
                     labels=c(expression(scale[E])))+
  labs(x='state (species density)',y='reward')+
  ggtitle('Ecological change function')+
  theme_minimal()+
  theme(text = element_text(size=15),
        title = element_text(size=12),
        axis.text.y = element_text(colour="red"))

#make fake plots to get line legends
line_data <- data.frame(x=c(1,2,3),
                        y=c(1,2,2),
                        z=c(1,2,2))
eco_plot <- ggplot()+
  geom_line(data=line_data,
             aes(x=x,y=y,linetype=as.factor(y)),linewidth=1.5)+
  scale_linetype_manual(values = c('solid','dashed'),
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
utility_plot <- reward_plot+penalty_plot+eco_legend+action_legend+
  plot_layout(design=layout)
ggsave('www/ratio_plot.jpg',utility_plot,dpi=600,width=10,height=4)


###################
#colonization rates
###################

#create vector of available immigration rates
imm_seq <- seq(0.01,2.2,by=0.01) 

# gamma
mu <- c(#0.03,
  0.15, 0.3, 0.4, 0.5, 0.6, 0.7, 1.0, 1.3, 1.6)

ratio <- c(3, 4, 5, 8, 10, 20, 30, 40, 50)

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

rates <- rbind(
  data.frame(
    imm = imm_seq,
    prob = dgamma(imm_seq,shape=k_vec[1], scale = theta_vec[1])/
      sum(dgamma(imm_seq,shape=k_vec[1], scale = theta_vec[1])),
    params = 1
  ),
  data.frame(
    imm = imm_seq,
    prob = dgamma(imm_seq,shape=k_vec[2], scale = theta_vec[2])/
      sum(dgamma(imm_seq,shape=k_vec[2], scale = theta_vec[2])),
    params = 2
  ),
  data.frame(
    imm = imm_seq,
    prob = dgamma(imm_seq,shape=k_vec[3], scale = theta_vec[3])/
      sum(dgamma(imm_seq,shape=k_vec[3], scale = theta_vec[3])),
    params = 3
  ),
  data.frame(
    imm = imm_seq,
    prob = dgamma(imm_seq,shape=k_vec[4], scale = theta_vec[4])/
      sum(dgamma(imm_seq,shape=k_vec[4], scale = theta_vec[4])),
    params = 4
  ),
  data.frame(
    imm = imm_seq,
    prob = dgamma(imm_seq,shape=k_vec[5], scale = theta_vec[5])/
      sum(dgamma(imm_seq,shape=k_vec[5], scale = theta_vec[5])),
    params = 5
  ),
  data.frame(
    imm = imm_seq,
    prob = dgamma(imm_seq,shape=k_vec[6], scale = theta_vec[6])/
      sum(dgamma(imm_seq,shape=k_vec[6], scale = theta_vec[6])),
    params = 6
  ),
  data.frame(
    imm = imm_seq,
    prob = dgamma(imm_seq,shape=k_vec[7], scale = theta_vec[7])/
      sum(dgamma(imm_seq,shape=k_vec[7], scale = theta_vec[7])),
    params = 7
  ),
  data.frame(
    imm = imm_seq,
    prob = dgamma(imm_seq,shape=k_vec[8], scale = theta_vec[8])/
      sum(dgamma(imm_seq,shape=k_vec[8], scale = theta_vec[8])),
    params = 8
  ),
  data.frame(
    imm = imm_seq,
    prob = dgamma(imm_seq,shape=k_vec[9], scale = theta_vec[9])/
      sum(dgamma(imm_seq,shape=k_vec[9], scale = theta_vec[9])),
    params = 9
  )
)


imm_stoch_plot_gamma <- ggplot()+
  geom_line(data=rates,
            aes(x=imm,
                y=prob,
                color=as.factor(params)), linewidth=1)+
  scale_color_manual(values=c('cornflowerblue','firebrick','darkorchid',
                              'aquamarine3','orange3','green4','dodgerblue4',
                              'lightcoral','goldenrod1'),
                     labels=c('\u03B1 = 0.02, E[\u03B3] = 0.15','\u03B1 = 0.04, E[\u03B3] = 0.3',
                              '\u03B1 = 0.06, E[\u03B3] = 0.4','\u03B1 = 0.07, E[\u03B3] = 0.5',
                              '\u03B1 = 0.12, E[\u03B3] = 0.6','\u03B1 = 0.14, E[\u03B3] = 0.7',
                              '\u03B1 = 0.69, E[\u03B3] = 1.0','\u03B1 = 0.99, E[\u03B3] = 1.3',
                              '\u03B1 = 1.00, E[\u03B3] = 1.6'))+
  geom_vline(aes(xintercept=0.9),linetype='dashed')+
  labs(x="Colonization rate (\u03B3)",y='Probability', color='')+
  theme_minimal()+
  theme(legend.position=c(0.75,0.75),
        legend.text = element_text(size=6))+
  guides(color=guide_legend(ncol=2))
ggsave('www/colonizationrates.jpg',imm_stoch_plot_gamma,bg='white',dpi=400,width=5,height=2.5)

