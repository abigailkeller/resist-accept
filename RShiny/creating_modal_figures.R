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
states <- seq(0,K*1.1,by=0.01)

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
  loss_a=0.204,
  loss_b=4,
  loss_c=1
)

nonlinear_exp1 <- 4
nonlinear_exp2 <- 0.5


# ecological change function -- sigmoidal
reward_sig <- function(s,loss_params){
  loss_params$loss_a/(1+exp(-loss_params$loss_b*(s-loss_params$loss_c)))
}
# ecological change function -- exponential
reward_exp <- function(s,loss_params){
  loss_params$loss_a*(exp(-loss_params$loss_b*s)-loss_params$loss_c)
}

#reward -- penalize high effort -- nonlinear
reward_penalize_high <- function(s,fun,loss_params,
                                 action,nonlinear_exp){
  fun(s,loss_params)-action^nonlinear_exp
}

penalty_plot1 <- ggplot()+
  geom_line(aes(x=actions,y=-actions^nonlinear_exp1),color='green4',linewidth=1.5)+
  ggtitle('Nonlinear exponent = 4')+
  labs(x='action',y='reward')+
  theme_minimal()+
  theme(text = element_text(size=15),
        title = element_text(size=12))
penalty_plot2 <- ggplot()+
  geom_line(aes(x=actions,y=-actions^nonlinear_exp2),color='green4',linewidth=1.5)+
  ggtitle('Nonlinear exponent = 0.5')+
  labs(x='action',y='reward')+
  theme_minimal()+
  theme(text = element_text(size=15),
        title = element_text(size=12))


final_plot <- penalty_plot1+penalty_plot2+plot_layout(nrow=1)
ggsave('www/penalty_plot.jpg',final_plot,dpi=600,width=8,height=4)


###################
#colonization rates
###################

library(tidyverse)
library(viridis)
library(patchwork)

k_vec <- c(0.2,0.5,2,4,15,30,50,75,125,500)
theta_vec <- c(0.025,0.1,0.1,0.125,0.05,1/30,0.025,0.02,0.016,0.01)

# create vector of available alpha
alpha_seq <- seq(0.01,6,by=0.01)

out <- as.data.frame(matrix(NA,nrow=0,ncol=5))
colnames(out) <- c('alpha','prob','shape','scale','mean')
for(i in 1:length(k_vec)){
  df <- data.frame(
    alpha = alpha_seq,
    prob = dgamma(alpha_seq,shape=k_vec[i],scale=theta_vec[i])/
      sum(dgamma(alpha_seq,shape=k_vec[i],scale=theta_vec[i])),
    shape = rep(k_vec[i],length(alpha_seq)),
    scale = rep(theta_vec[i],length(alpha_seq)),
    mean = rep(k_vec[i]*theta_vec[i],length(alpha_seq))
  )
  out <- rbind(out,df)
}

out$mean <- round(out$mean,3)

my_palette <- viridis(10)


plot1 <- ggplot(data=out[out$mean==0.005,])+
  geom_line(aes(x=alpha,y=prob,color=as.factor(mean)),
            linewidth=1)+
  scale_color_manual(values='#440154FF',labels='0.005')+
  labs(x='',y='probability',color='mean \u03B1 / K')+
  scale_x_continuous(breaks=c(0,0.1,0.2),labels=c('0','0.1','0.2'),
                     limits=c(0,0.25))+
  theme_minimal()+
  theme(text = element_text(size = 12))
plot2 <- ggplot(data=out %>% filter(mean!=0.005 & mean!=5))+
  geom_line(aes(x=alpha,y=prob,color=as.factor(mean)),
            linewidth=1)+
  scale_color_manual(values=c("#482878FF","#3E4A89FF","#31688EFF","#26828EFF",
                              "#1F9E89FF","#35B779FF","#6DCD59FF","#B4DE2CFF"),
                     labels=c('0.05','0.2','0.5','0.75',
                              '1','1.25','1.5','2'))+
  labs(x='',y='probability',color='mean \u03B1 / K')+
  scale_x_continuous(breaks=c(0,1,2),labels=c('0','1.0','2.0'),
                     limits=c(0,2.5))+
  theme_minimal()+
  theme(text = element_text(size = 12))
plot3 <- ggplot(data=out[out$mean==5,])+
  geom_line(aes(x=alpha,y=prob,color=as.factor(mean)),
            linewidth=1)+
  scale_color_manual(values='#FDE725FF',labels='5.0')+
  labs(x='scaled source\npopulation size\n(\u03B1 / K)',y='probability',color='mean \u03B1 / K')+
  scale_x_continuous(breaks=c(4,5,6),labels=c('4.0','5.0','6.0'),
                     limits=c(4,6))+
  theme_minimal()+
  theme(text = element_text(size = 12))

#define plot layout
layout <- "
AA
BB
CC
"

final_plot <- plot1+plot2+plot3+plot_layout(design=layout)
ggsave('www/sourcepop.jpg',final_plot,
       dpi=500,width=4,height=7)

