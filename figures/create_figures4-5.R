########################
# Creating Figures 4-5 #
########################

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
  pivot_longer(cols=!c(penal,loss,r,imm),
               values_to = 'policy', names_to = 'state')

# convert to numeric
out_df_long$state <- as.numeric(out_df_long$state)
out_df_long$policy <- as.numeric(out_df_long$policy)

############
# Figure 4 #
############

#r = 1, mu = 0.7 -- linear
linear_mu0.7 <- ggplot()+
  geom_line(data=out_df_long[c(out_df_long$imm=='imm14.0.05'&
                                 out_df_long$loss=='exp'&
                                 out_df_long$penal=='lpenal'),],
            aes(x=state,y=policy),linewidth=1.5,color='mediumorchid')+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='optimal removal effort')+
  ggtitle(paste0('A. \u03B1 = 0.14'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 0.7 -- nonlinear
nonlinear_mu0.7 <- ggplot()+
  geom_line(data=out_df_long[c(out_df_long$imm=='imm14.0.05'&
                                 out_df_long$loss=='exp'&
                                 out_df_long$penal=='hpenal'),],
            aes(x=state,y=policy),linewidth=1.5,color='green4')+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='optimal removal effort')+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1 -- linear
linear_mu1 <- ggplot()+
  geom_line(data=out_df_long[c(out_df_long$imm=='imm30.0.03'&
                                 out_df_long$loss=='exp'&
                                 out_df_long$penal=='lpenal'),],
            aes(x=state,y=policy),linewidth=1.5,color='mediumorchid')+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='')+
  ggtitle(paste0('B. \u03B1 = 0.69'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_blank(),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1 -- nonlinear
nonlinear_mu1 <- ggplot()+
  geom_line(data=out_df_long[c(out_df_long$imm=='imm30.0.03'&
                                 out_df_long$loss=='exp'&
                                 out_df_long$penal=='hpenal'),],
            aes(x=state,y=policy),linewidth=1.5,color='green4')+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='')+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1.3 -- linear
linear_mu1.3 <- ggplot()+
  geom_line(data=out_df_long[c(out_df_long$imm=='imm52.0.025'&
                                 out_df_long$loss=='exp'&
                                 out_df_long$penal=='lpenal'),],
            aes(x=state,y=policy),linewidth=1.5,color='mediumorchid')+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='')+
  ggtitle(paste0('C. \u03B1 = 0.99'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_blank(),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1.3 -- nonlinear
nonlinear_mu1.3 <- ggplot()+
  geom_line(data=out_df_long[c(out_df_long$imm=='imm52.0.025'&
                                 out_df_long$loss=='exp'&
                                 out_df_long$penal=='hpenal'),],
            aes(x=state,y=policy),linewidth=1.5,color='green4')+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='')+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text.y=element_blank(),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#make fake plots to get line legends
line_data <- data.frame(x=c(1,2,3),
                        y=c(1,2,3),
                        z=c(1,2,2))

# create legend
action_plot <- ggplot()+
  geom_line(data=line_data,
            aes(x=x,y=y,color=as.factor(z)),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  labs(color='Removal\ncost\nfunction')+
  theme_minimal()+
  theme(legend.text=element_text(size=10),
        legend.title = element_text(size=12,hjust = 0.5))
action_legend <- get_legend(action_plot)

layout_RA <- '
AABBCC#
AABBCCG
DDEEFFG
DDEEFF#
'

figure4 <- linear_mu0.7 + linear_mu1 + linear_mu1.3 +
  nonlinear_mu0.7 + nonlinear_mu1 + nonlinear_mu1.3 + 
  action_legend + plot_layout(design = layout_RA)
ggsave('figures/figure4_resist-accept_binary.svg',figure4,dpi=700,bg='transparent',width=8,height=5)

############
# Figure 5 #
############

#r = 1, mu = 0.6
mu0.6 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm6.0.1',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='optimal removal effort',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('A. \u03B1 = 0.12'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 0.7
mu0.7 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm14.0.05',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('B. \u03B1 = 0.14'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1
mu1 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm30.0.03',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('C. \u03B1 = 0.69'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1.3
mu1.3 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm52.0.025',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='optimal removal effort',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('D. \u03B1 = 0.99'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1.6
mu1.6 <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm80.0.02',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('E. \u03B1 = 1.00'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

#r = 1, mu = 1.3, with legend
mu1.3_wlegend <- ggplot()+
  geom_line(data=out_df_long[out_df_long$imm=='imm52.0.025',],
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  labs(x='state (species density)',y='',
       color='Removal cost function',
       linetype='Ecological change function')+
  ggtitle(paste0('mu = 1.3, phi = 30'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.text=element_text(size=9),
        legend.title=element_text(size=11),
        legend.key.width = unit(1,'cm'),
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))
legend <- get_legend(mu1.3_wlegend)

#define plot layout
layout <- "
AABBCC
AABBCC
DDEEFF
DDEEFF
"

figure5 <- mu0.6+mu0.7+mu1+mu1.3+mu1.6+legend+plot_layout(design=layout)
ggsave('figures/Figure5_resist-accept_nuance.tiff',figure5,dpi=700,width=7.5,height=5)

