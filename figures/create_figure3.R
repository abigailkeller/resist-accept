#####################
# Creating Figure 3 #
#####################

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

############
# Figure 3 #
############


# alpha = 0.05
alpha0.05 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='0.5', alpha_scale == '0.1',
                     loss=='sig',penal=='lpenal'),
            aes(x=state,y=policy),linewidth=1.5)+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='optimal removal effort')+
  ggtitle(paste0('A. \u03B1 = 0.05K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

# alpha = 0.5
alpha0.5 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='4', alpha_scale == '0.125',
                     loss=='sig',penal=='lpenal'),
            aes(x=state,y=policy),linewidth=1.5)+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('B. \u03B1 = 0.5K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))

# alpha = 0.75
alpha0.75 <- ggplot()+
  geom_line(data=out_df_long %>%
              filter(alpha_shape =='15', alpha_scale == '0.05',
                     loss=='sig',penal=='lpenal'),
            aes(x=state,y=policy),linewidth=2)+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(breaks=c(0,0.5*K,K),labels=c(0,'0.5K','K'),
                     limits=c(0,1.1*K))+
  labs(x='local EGC density',y='',
       color='Action\npenalty\nfunction',
       linetype='Ecological\nchange\nfunction')+
  ggtitle(paste0('C. \u03B1 = 0.75K'))+
  theme_minimal()+
  theme(legend.title.align=0.5,
        legend.position='None',
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title = element_text(size=12))





#define plot layout
layout <- "
AABBCC
AABBCC
"

figure3 <- alpha0.05+alpha0.5+alpha0.75+plot_layout(design=layout)
ggsave('figures/Figure3_resist-accept.svg',figure3,dpi=700,width=6,height=2.5)

