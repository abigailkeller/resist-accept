##################################
# Supplemental: Tmax sensitivity #
##################################

# load packages
library(tidyverse)
library(patchwork)
library(viridis)
library(ggpubr)
library(RColorBrewer)

# read in data
out_df <- readRDS('data/tmax_sensitivity_policies.rds')

# create long df
out_df_long <- out_df %>% 
  pivot_longer(cols=!c(penal,loss,Tmax),
               values_to = 'policy', names_to = 'state')

# convert to numeric
out_df_long$state <- as.numeric(out_df_long$state)
out_df_long$policy <- as.numeric(out_df_long$policy)

K <- 100000

# Tmax = 25
Tmax_25 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(Tmax =='25'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='optimal removal effort')+
  ggtitle(paste0('A. Tmax = 25'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

# Tmax = 50
Tmax_50 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(Tmax =='50'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='optimal removal effort')+
  ggtitle(paste0('B. Tmax = 50'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

# Tmax = 100
Tmax_100 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(Tmax =='100'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='optimal removal effort')+
  ggtitle(paste0('C. Tmax = 100'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

# Tmax = 150
Tmax_150 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(Tmax =='150'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='optimal removal effort')+
  ggtitle(paste0('D. Tmax = 150'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

# Tmax = 200
Tmax_200 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(Tmax =='200'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='optimal removal effort')+
  ggtitle(paste0('E. Tmax = 200'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

# legend plot
legend_plot <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(Tmax =='200'),
            aes(x=state,y=policy,color=penal,linetype=loss),linewidth=1.5)+
  scale_color_manual(values=c('green4','mediumorchid'),
                     labels=c('Nonlinear','Linear'))+
  scale_linetype_manual(values=c('solid','dotted'),
                        labels=c('Exponential','Sigmoidal'))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='',
       color='Removal cost function',
       linetype='Ecological change function')+
  ggtitle(paste0('D. Tmax = 200'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))
legend <- get_legend(legend_plot)



#define plot layout
layout <- "
AAABBBCCC
AAABBBCCC
DDDEEEFFF
DDDEEEFFF
"

finalfigure <- Tmax_25+Tmax_50+Tmax_100+Tmax_150+Tmax_200+legend+plot_layout(design=layout)
ggsave('figures/supplemental_tmax_sensitivity.tiff',finalfigure,
       dpi=700,width=6.5,height=4)

