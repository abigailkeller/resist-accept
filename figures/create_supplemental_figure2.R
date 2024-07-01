##################################
# Creating Supplemental Figure 2 #
##################################

# load packages
library(tidyverse)
library(patchwork)
library(viridis)
library(ggpubr)
library(RColorBrewer)

# read in data
out_df <- readRDS('data/stationary_policies.rds')

# create long df
out_df_long <- out_df %>% 
  pivot_longer(cols=!c(penal,loss,alpha_shape,alpha_scale),
               values_to = 'policy', names_to = 'state')

# convert to numeric
out_df_long$state <- as.numeric(out_df_long$state)
out_df_long$policy <- as.numeric(out_df_long$policy)

K <- 100000

# alpha = 0.005
alpha0.005 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='0.2', alpha_scale == '0.025'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='',y='optimal removal effort',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('A. \u03B1 = 0.005K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))

# alpha = 0.05
alpha0.05 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='0.5', alpha_scale == '0.1'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('B. \u03B1 = 0.05K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))

# alpha = 0.2
alpha0.2 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape == '2', alpha_scale == '0.1'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('C. \u03B1 = 0.2K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))

# alpha = 0.5
alpha0.5 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='4', alpha_scale == '0.125'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('D. \u03B1 = 0.5K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))

# alpha = 0.75
alpha0.75 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='15', alpha_scale == '0.05'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='',y='optimal removal effort',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('E. \u03B1 = 0.75K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))

# alpha = 1.0
alpha1.0 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='30', alpha_scale == '0.03'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('F. \u03B1 = 1.0K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))

# alpha = 1.25
alpha1.25 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='50', alpha_scale == '0.025'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('G. \u03B1 = 1.25K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))

# alpha = 1.5
alpha1.5 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='75', alpha_scale == '0.02'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='EGC density',y='optimal removal effort',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('H. \u03B1 = 1.5K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))

# alpha = 2
alpha2.0 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='125', alpha_scale == '0.016'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('I. \u03B1 = 2.0K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))

# alpha = 5
alpha5.0 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='500', alpha_scale == '0.01'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('J. \u03B1 = 5.0K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        plot.title = element_text(size=13))



#define plot layout
layout <- "
AABBCCDD
AABBCCDD
EEFFGG##
EEFFGG##
HHIIJJ##
HHIIJJ##
"

final_plot <- alpha0.005+alpha0.05+alpha0.2+alpha0.5+
  alpha0.75+alpha1.0+alpha1.25+alpha1.5+alpha2.0+
  alpha5.0+plot_layout(design=layout)
ggsave('figures/supplemental_resist-accept.svg',final_plot,
       dpi=700,width=9,height=8)

